//===- ExprCXX.cpp - (C++) Expression AST Node Implementation -------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the subclesses of Expr class declared in ExprCXX.h
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ExprCXX.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/AST/ComputeDependence.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclAccessPair.h"
#include "clang/AST/DeclBase.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/DeclarationName.h"
#include "clang/AST/DependenceFlags.h"
#include "clang/AST/Expr.h"
#include "clang/AST/LambdaCapture.h"
#include "clang/AST/NestedNameSpecifier.h"
#include "clang/AST/TemplateBase.h"
#include "clang/AST/Type.h"
#include "clang/AST/TypeLoc.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Basic/LLVM.h"
#include "clang/Basic/OperatorKinds.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/Specifiers.h"
#include "clang/Basic/TargetInfo.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include <cassert>
#include <cstddef>
#include <cstring>
#include <memory>

using namespace clang;

//===----------------------------------------------------------------------===//
//  Child Iterators for iterating over subexpressions/substatements
//===----------------------------------------------------------------------===//

bool CXXOperatorCallExpr::isInfixBinaryOp() const {
  // An infix binary operator is any operator with two arguments other than
  // operator() and operator[]. Note that none of these operators can have
  // default arguments, so it suffices to check the number of argument
  // expressions.
  if (getNumArgs() != 2)
    return false;

  switch (getOperator()) {
  case OO_Call: case OO_Subscript:
    return false;
  default:
    return true;
  }
}

CXXRewrittenBinaryOperator::DecomposedForm
CXXRewrittenBinaryOperator::getDecomposedForm() const {
  DecomposedForm Result = {};
  const Expr *E = getSemanticForm()->IgnoreImplicit();

  // Remove an outer '!' if it exists (only happens for a '!=' rewrite).
  bool SkippedNot = false;
  if (auto *NotEq = dyn_cast<UnaryOperator>(E)) {
    assert(NotEq->getOpcode() == UO_LNot);
    E = NotEq->getSubExpr()->IgnoreImplicit();
    SkippedNot = true;
  }

  // Decompose the outer binary operator.
  if (auto *BO = dyn_cast<BinaryOperator>(E)) {
    assert(!SkippedNot || BO->getOpcode() == BO_EQ);
    Result.Opcode = SkippedNot ? BO_NE : BO->getOpcode();
    Result.LHS = BO->getLHS();
    Result.RHS = BO->getRHS();
    Result.InnerBinOp = BO;
  } else if (auto *BO = dyn_cast<CXXOperatorCallExpr>(E)) {
    assert(!SkippedNot || BO->getOperator() == OO_EqualEqual);
    assert(BO->isInfixBinaryOp());
    switch (BO->getOperator()) {
    case OO_Less: Result.Opcode = BO_LT; break;
    case OO_LessEqual: Result.Opcode = BO_LE; break;
    case OO_Greater: Result.Opcode = BO_GT; break;
    case OO_GreaterEqual: Result.Opcode = BO_GE; break;
    case OO_Spaceship: Result.Opcode = BO_Cmp; break;
    case OO_EqualEqual: Result.Opcode = SkippedNot ? BO_NE : BO_EQ; break;
    default: llvm_unreachable("unexpected binop in rewritten operator expr");
    }
    Result.LHS = BO->getArg(0);
    Result.RHS = BO->getArg(1);
    Result.InnerBinOp = BO;
  } else {
    llvm_unreachable("unexpected rewritten operator form");
  }

  // Put the operands in the right order for == and !=, and canonicalize the
  // <=> subexpression onto the LHS for all other forms.
  if (isReversed())
    std::swap(Result.LHS, Result.RHS);

  // If this isn't a spaceship rewrite, we're done.
  if (Result.Opcode == BO_EQ || Result.Opcode == BO_NE)
    return Result;

  // Otherwise, we expect a <=> to now be on the LHS.
  E = Result.LHS->IgnoreImplicitAsWritten();
  if (auto *BO = dyn_cast<BinaryOperator>(E)) {
    assert(BO->getOpcode() == BO_Cmp);
    Result.LHS = BO->getLHS();
    Result.RHS = BO->getRHS();
    Result.InnerBinOp = BO;
  } else if (auto *BO = dyn_cast<CXXOperatorCallExpr>(E)) {
    assert(BO->getOperator() == OO_Spaceship);
    Result.LHS = BO->getArg(0);
    Result.RHS = BO->getArg(1);
    Result.InnerBinOp = BO;
  } else {
    llvm_unreachable("unexpected rewritten operator form");
  }

  // Put the comparison operands in the right order.
  if (isReversed())
    std::swap(Result.LHS, Result.RHS);
  return Result;
}

bool CXXTypeidExpr::isPotentiallyEvaluated() const {
  if (isTypeOperand())
    return false;

  // C++11 [expr.typeid]p3:
  //   When typeid is applied to an expression other than a glvalue of
  //   polymorphic class type, [...] the expression is an unevaluated operand.
  const Expr *E = getExprOperand();
  if (const CXXRecordDecl *RD = E->getType()->getAsCXXRecordDecl())
    if (RD->isPolymorphic() && E->isGLValue())
      return true;

  return false;
}

bool CXXTypeidExpr::isMostDerived(ASTContext &Context) const {
  assert(!isTypeOperand() && "Cannot call isMostDerived for typeid(type)");
  const Expr *E = getExprOperand()->IgnoreParenNoopCasts(Context);
  if (const auto *DRE = dyn_cast<DeclRefExpr>(E)) {
    QualType Ty = DRE->getDecl()->getType();
    if (!Ty->isPointerType() && !Ty->isReferenceType())
      return true;
  }

  return false;
}

QualType CXXTypeidExpr::getTypeOperand(ASTContext &Context) const {
  assert(isTypeOperand() && "Cannot call getTypeOperand for typeid(expr)");
  Qualifiers Quals;
  return Context.getUnqualifiedArrayType(
      Operand.get<TypeSourceInfo *>()->getType().getNonReferenceType(), Quals);
}

QualType CXXUuidofExpr::getTypeOperand(ASTContext &Context) const {
  assert(isTypeOperand() && "Cannot call getTypeOperand for __uuidof(expr)");
  Qualifiers Quals;
  return Context.getUnqualifiedArrayType(
      Operand.get<TypeSourceInfo *>()->getType().getNonReferenceType(), Quals);
}

// CXXScalarValueInitExpr
SourceLocation CXXScalarValueInitExpr::getBeginLoc() const {
  return TypeInfo ? TypeInfo->getTypeLoc().getBeginLoc() : getRParenLoc();
}

// CXXNewExpr
CXXNewExpr::CXXNewExpr(bool IsGlobalNew, FunctionDecl *OperatorNew,
                       FunctionDecl *OperatorDelete, bool ShouldPassAlignment,
                       bool UsualArrayDeleteWantsSize,
                       ArrayRef<Expr *> PlacementArgs, SourceRange TypeIdParens,
                       Optional<Expr *> ArraySize,
                       InitializationStyle InitializationStyle,
                       Expr *Initializer, QualType Ty,
                       TypeSourceInfo *AllocatedTypeInfo, SourceRange Range,
                       SourceRange DirectInitRange)
    : Expr(CXXNewExprClass, Ty, VK_PRValue, OK_Ordinary),
      OperatorNew(OperatorNew), OperatorDelete(OperatorDelete),
      AllocatedTypeInfo(AllocatedTypeInfo), Range(Range),
      DirectInitRange(DirectInitRange) {

  assert((Initializer != nullptr || InitializationStyle == NoInit) &&
         "Only NoInit can have no initializer!");

  CXXNewExprBits.IsGlobalNew = IsGlobalNew;
  CXXNewExprBits.IsArray = ArraySize.hasValue();
  CXXNewExprBits.ShouldPassAlignment = ShouldPassAlignment;
  CXXNewExprBits.UsualArrayDeleteWantsSize = UsualArrayDeleteWantsSize;
  CXXNewExprBits.StoredInitializationStyle =
      Initializer ? InitializationStyle + 1 : 0;
  bool IsParenTypeId = TypeIdParens.isValid();
  CXXNewExprBits.IsParenTypeId = IsParenTypeId;
  CXXNewExprBits.NumPlacementArgs = PlacementArgs.size();

  if (ArraySize)
    getTrailingObjects<Stmt *>()[arraySizeOffset()] = *ArraySize;
  if (Initializer)
    getTrailingObjects<Stmt *>()[initExprOffset()] = Initializer;
  for (unsigned I = 0; I != PlacementArgs.size(); ++I)
    getTrailingObjects<Stmt *>()[placementNewArgsOffset() + I] =
        PlacementArgs[I];
  if (IsParenTypeId)
    getTrailingObjects<SourceRange>()[0] = TypeIdParens;

  switch (getInitializationStyle()) {
  case CallInit:
    this->Range.setEnd(DirectInitRange.getEnd());
    break;
  case ListInit:
    this->Range.setEnd(getInitializer()->getSourceRange().getEnd());
    break;
  default:
    if (IsParenTypeId)
      this->Range.setEnd(TypeIdParens.getEnd());
    break;
  }

  setDependence(computeDependence(this));
}

CXXNewExpr::CXXNewExpr(EmptyShell Empty, bool IsArray,
                       unsigned NumPlacementArgs, bool IsParenTypeId)
    : Expr(CXXNewExprClass, Empty) {
  CXXNewExprBits.IsArray = IsArray;
  CXXNewExprBits.NumPlacementArgs = NumPlacementArgs;
  CXXNewExprBits.IsParenTypeId = IsParenTypeId;
}

CXXNewExpr *
CXXNewExpr::Create(const ASTContext &Ctx, bool IsGlobalNew,
                   FunctionDecl *OperatorNew, FunctionDecl *OperatorDelete,
                   bool ShouldPassAlignment, bool UsualArrayDeleteWantsSize,
                   ArrayRef<Expr *> PlacementArgs, SourceRange TypeIdParens,
                   Optional<Expr *> ArraySize,
                   InitializationStyle InitializationStyle, Expr *Initializer,
                   QualType Ty, TypeSourceInfo *AllocatedTypeInfo,
                   SourceRange Range, SourceRange DirectInitRange) {
  bool IsArray = ArraySize.hasValue();
  bool HasInit = Initializer != nullptr;
  unsigned NumPlacementArgs = PlacementArgs.size();
  bool IsParenTypeId = TypeIdParens.isValid();
  void *Mem =
      Ctx.Allocate(totalSizeToAlloc<Stmt *, SourceRange>(
                       IsArray + HasInit + NumPlacementArgs, IsParenTypeId),
                   alignof(CXXNewExpr));
  return new (Mem)
      CXXNewExpr(IsGlobalNew, OperatorNew, OperatorDelete, ShouldPassAlignment,
                 UsualArrayDeleteWantsSize, PlacementArgs, TypeIdParens,
                 ArraySize, InitializationStyle, Initializer, Ty,
                 AllocatedTypeInfo, Range, DirectInitRange);
}

CXXNewExpr *CXXNewExpr::CreateEmpty(const ASTContext &Ctx, bool IsArray,
                                    bool HasInit, unsigned NumPlacementArgs,
                                    bool IsParenTypeId) {
  void *Mem =
      Ctx.Allocate(totalSizeToAlloc<Stmt *, SourceRange>(
                       IsArray + HasInit + NumPlacementArgs, IsParenTypeId),
                   alignof(CXXNewExpr));
  return new (Mem)
      CXXNewExpr(EmptyShell(), IsArray, NumPlacementArgs, IsParenTypeId);
}

bool CXXNewExpr::shouldNullCheckAllocation() const {
  return !getOperatorNew()->hasAttr<ReturnsNonNullAttr>() &&
         getOperatorNew()
             ->getType()
             ->castAs<FunctionProtoType>()
             ->isNothrow() &&
         !getOperatorNew()->isReservedGlobalPlacementOperator();
}

// CXXDeleteExpr
QualType CXXDeleteExpr::getDestroyedType() const {
  const Expr *Arg = getArgument();

  // For a destroying operator delete, we may have implicitly converted the
  // pointer type to the type of the parameter of the 'operator delete'
  // function.
  while (const auto *ICE = dyn_cast<ImplicitCastExpr>(Arg)) {
    if (ICE->getCastKind() == CK_DerivedToBase ||
        ICE->getCastKind() == CK_UncheckedDerivedToBase ||
        ICE->getCastKind() == CK_NoOp) {
      assert((ICE->getCastKind() == CK_NoOp ||
              getOperatorDelete()->isDestroyingOperatorDelete()) &&
             "only a destroying operator delete can have a converted arg");
      Arg = ICE->getSubExpr();
    } else
      break;
  }

  // The type-to-delete may not be a pointer if it's a dependent type.
  const QualType ArgType = Arg->getType();

  if (ArgType->isDependentType() && !ArgType->isPointerType())
    return QualType();

  return ArgType->castAs<PointerType>()->getPointeeType();
}

// CXXPseudoDestructorExpr
PseudoDestructorTypeStorage::PseudoDestructorTypeStorage(TypeSourceInfo *Info)
    : Type(Info) {
  Location = Info->getTypeLoc().getLocalSourceRange().getBegin();
}

CXXPseudoDestructorExpr::CXXPseudoDestructorExpr(
    const ASTContext &Context, Expr *Base, bool isArrow,
    SourceLocation OperatorLoc, NestedNameSpecifierLoc QualifierLoc,
    TypeSourceInfo *ScopeType, SourceLocation ColonColonLoc,
    SourceLocation TildeLoc, PseudoDestructorTypeStorage DestroyedType)
    : Expr(CXXPseudoDestructorExprClass, Context.BoundMemberTy, VK_PRValue,
           OK_Ordinary),
      Base(static_cast<Stmt *>(Base)), IsArrow(isArrow),
      OperatorLoc(OperatorLoc), QualifierLoc(QualifierLoc),
      ScopeType(ScopeType), ColonColonLoc(ColonColonLoc), TildeLoc(TildeLoc),
      DestroyedType(DestroyedType) {
  setDependence(computeDependence(this));
}

QualType CXXPseudoDestructorExpr::getDestroyedType() const {
  if (TypeSourceInfo *TInfo = DestroyedType.getTypeSourceInfo())
    return TInfo->getType();

  return QualType();
}

SourceLocation CXXPseudoDestructorExpr::getEndLoc() const {
  SourceLocation End = DestroyedType.getLocation();
  if (TypeSourceInfo *TInfo = DestroyedType.getTypeSourceInfo())
    End = TInfo->getTypeLoc().getLocalSourceRange().getEnd();
  return End;
}

// UnresolvedLookupExpr
UnresolvedLookupExpr::UnresolvedLookupExpr(
    const ASTContext &Context, CXXRecordDecl *NamingClass,
    NestedNameSpecifierLoc QualifierLoc, SourceLocation TemplateKWLoc,
    const DeclarationNameInfo &NameInfo, bool RequiresADL, bool Overloaded,
    const TemplateArgumentListInfo *TemplateArgs, UnresolvedSetIterator Begin,
    UnresolvedSetIterator End)
    : OverloadExpr(UnresolvedLookupExprClass, Context, QualifierLoc,
                   TemplateKWLoc, NameInfo, TemplateArgs, Begin, End, false,
                   false, false),
      NamingClass(NamingClass) {
  UnresolvedLookupExprBits.RequiresADL = RequiresADL;
  UnresolvedLookupExprBits.Overloaded = Overloaded;
}

UnresolvedLookupExpr::UnresolvedLookupExpr(EmptyShell Empty,
                                           unsigned NumResults,
                                           bool HasTemplateKWAndArgsInfo)
    : OverloadExpr(UnresolvedLookupExprClass, Empty, NumResults,
                   HasTemplateKWAndArgsInfo) {}

UnresolvedLookupExpr *UnresolvedLookupExpr::Create(
    const ASTContext &Context, CXXRecordDecl *NamingClass,
    NestedNameSpecifierLoc QualifierLoc, const DeclarationNameInfo &NameInfo,
    bool RequiresADL, bool Overloaded, UnresolvedSetIterator Begin,
    UnresolvedSetIterator End) {
  unsigned NumResults = End - Begin;
  unsigned Size = totalSizeToAlloc<DeclAccessPair, ASTTemplateKWAndArgsInfo,
                                   TemplateArgumentLoc>(NumResults, 0, 0);
  void *Mem = Context.Allocate(Size, alignof(UnresolvedLookupExpr));
  return new (Mem) UnresolvedLookupExpr(Context, NamingClass, QualifierLoc,
                                        SourceLocation(), NameInfo, RequiresADL,
                                        Overloaded, nullptr, Begin, End);
}

UnresolvedLookupExpr *UnresolvedLookupExpr::Create(
    const ASTContext &Context, CXXRecordDecl *NamingClass,
    NestedNameSpecifierLoc QualifierLoc, SourceLocation TemplateKWLoc,
    const DeclarationNameInfo &NameInfo, bool RequiresADL,
    const TemplateArgumentListInfo *Args, UnresolvedSetIterator Begin,
    UnresolvedSetIterator End) {
  assert(Args || TemplateKWLoc.isValid());
  unsigned NumResults = End - Begin;
  unsigned NumTemplateArgs = Args ? Args->size() : 0;
  unsigned Size =
      totalSizeToAlloc<DeclAccessPair, ASTTemplateKWAndArgsInfo,
                       TemplateArgumentLoc>(NumResults, 1, NumTemplateArgs);
  void *Mem = Context.Allocate(Size, alignof(UnresolvedLookupExpr));
  return new (Mem) UnresolvedLookupExpr(Context, NamingClass, QualifierLoc,
                                        TemplateKWLoc, NameInfo, RequiresADL,
                                        /*Overloaded*/ true, Args, Begin, End);
}

UnresolvedLookupExpr *UnresolvedLookupExpr::CreateEmpty(
    const ASTContext &Context, unsigned NumResults,
    bool HasTemplateKWAndArgsInfo, unsigned NumTemplateArgs) {
  assert(NumTemplateArgs == 0 || HasTemplateKWAndArgsInfo);
  unsigned Size = totalSizeToAlloc<DeclAccessPair, ASTTemplateKWAndArgsInfo,
                                   TemplateArgumentLoc>(
      NumResults, HasTemplateKWAndArgsInfo, NumTemplateArgs);
  void *Mem = Context.Allocate(Size, alignof(UnresolvedLookupExpr));
  return new (Mem)
      UnresolvedLookupExpr(EmptyShell(), NumResults, HasTemplateKWAndArgsInfo);
}

OverloadExpr::OverloadExpr(StmtClass SC, const ASTContext &Context,
                           NestedNameSpecifierLoc QualifierLoc,
                           SourceLocation TemplateKWLoc,
                           const DeclarationNameInfo &NameInfo,
                           const TemplateArgumentListInfo *TemplateArgs,
                           UnresolvedSetIterator Begin,
                           UnresolvedSetIterator End, bool KnownDependent,
                           bool KnownInstantiationDependent,
                           bool KnownContainsUnexpandedParameterPack)
    : Expr(SC, Context.OverloadTy, VK_LValue, OK_Ordinary), NameInfo(NameInfo),
      QualifierLoc(QualifierLoc) {
  unsigned NumResults = End - Begin;
  OverloadExprBits.NumResults = NumResults;
  OverloadExprBits.HasTemplateKWAndArgsInfo =
      (TemplateArgs != nullptr ) || TemplateKWLoc.isValid();

  if (NumResults) {
    // Copy the results to the trailing array past UnresolvedLookupExpr
    // or UnresolvedMemberExpr.
    DeclAccessPair *Results = getTrailingResults();
    memcpy(Results, Begin.I, NumResults * sizeof(DeclAccessPair));
  }

  if (TemplateArgs) {
    auto Deps = TemplateArgumentDependence::None;
    getTrailingASTTemplateKWAndArgsInfo()->initializeFrom(
        TemplateKWLoc, *TemplateArgs, getTrailingTemplateArgumentLoc(), Deps);
  } else if (TemplateKWLoc.isValid()) {
    getTrailingASTTemplateKWAndArgsInfo()->initializeFrom(TemplateKWLoc);
  }

  setDependence(computeDependence(this, KnownDependent,
                                  KnownInstantiationDependent,
                                  KnownContainsUnexpandedParameterPack));
  if (isTypeDependent())
    setType(Context.DependentTy);
}

OverloadExpr::OverloadExpr(StmtClass SC, EmptyShell Empty, unsigned NumResults,
                           bool HasTemplateKWAndArgsInfo)
    : Expr(SC, Empty) {
  OverloadExprBits.NumResults = NumResults;
  OverloadExprBits.HasTemplateKWAndArgsInfo = HasTemplateKWAndArgsInfo;
}

// DependentScopeDeclRefExpr
DependentScopeDeclRefExpr::DependentScopeDeclRefExpr(
    QualType Ty, NestedNameSpecifierLoc QualifierLoc,
    SourceLocation TemplateKWLoc, const DeclarationNameInfo &NameInfo,
    const TemplateArgumentListInfo *Args)
    : Expr(DependentScopeDeclRefExprClass, Ty, VK_LValue, OK_Ordinary),
      QualifierLoc(QualifierLoc), NameInfo(NameInfo) {
  DependentScopeDeclRefExprBits.HasTemplateKWAndArgsInfo =
      (Args != nullptr) || TemplateKWLoc.isValid();
  if (Args) {
    auto Deps = TemplateArgumentDependence::None;
    getTrailingObjects<ASTTemplateKWAndArgsInfo>()->initializeFrom(
        TemplateKWLoc, *Args, getTrailingObjects<TemplateArgumentLoc>(), Deps);
  } else if (TemplateKWLoc.isValid()) {
    getTrailingObjects<ASTTemplateKWAndArgsInfo>()->initializeFrom(
        TemplateKWLoc);
  }
  setDependence(computeDependence(this));
}

DependentScopeDeclRefExpr *DependentScopeDeclRefExpr::Create(
    const ASTContext &Context, NestedNameSpecifierLoc QualifierLoc,
    SourceLocation TemplateKWLoc, const DeclarationNameInfo &NameInfo,
    const TemplateArgumentListInfo *Args) {
  assert(QualifierLoc && "should be created for dependent qualifiers");
  bool HasTemplateKWAndArgsInfo = Args || TemplateKWLoc.isValid();
  std::size_t Size =
      totalSizeToAlloc<ASTTemplateKWAndArgsInfo, TemplateArgumentLoc>(
          HasTemplateKWAndArgsInfo, Args ? Args->size() : 0);
  void *Mem = Context.Allocate(Size);
  return new (Mem) DependentScopeDeclRefExpr(Context.DependentTy, QualifierLoc,
                                             TemplateKWLoc, NameInfo, Args);
}

DependentScopeDeclRefExpr *
DependentScopeDeclRefExpr::CreateEmpty(const ASTContext &Context,
                                       bool HasTemplateKWAndArgsInfo,
                                       unsigned NumTemplateArgs) {
  assert(NumTemplateArgs == 0 || HasTemplateKWAndArgsInfo);
  std::size_t Size =
      totalSizeToAlloc<ASTTemplateKWAndArgsInfo, TemplateArgumentLoc>(
          HasTemplateKWAndArgsInfo, NumTemplateArgs);
  void *Mem = Context.Allocate(Size);
  auto *E = new (Mem) DependentScopeDeclRefExpr(
      QualType(), NestedNameSpecifierLoc(), SourceLocation(),
      DeclarationNameInfo(), nullptr);
  E->DependentScopeDeclRefExprBits.HasTemplateKWAndArgsInfo =
      HasTemplateKWAndArgsInfo;
  return E;
}

SourceLocation CXXConstructExpr::getBeginLoc() const {
  if (isa<CXXTemporaryObjectExpr>(this))
    return cast<CXXTemporaryObjectExpr>(this)->getBeginLoc();
  return getLocation();
}

SourceLocation CXXConstructExpr::getEndLoc() const {
  if (isa<CXXTemporaryObjectExpr>(this))
    return cast<CXXTemporaryObjectExpr>(this)->getEndLoc();

  if (ParenOrBraceRange.isValid())
    return ParenOrBraceRange.getEnd();

  SourceLocation End = getLocation();
  for (unsigned I = getNumArgs(); I > 0; --I) {
    const Expr *Arg = getArg(I-1);
    if (!Arg->isDefaultArgument()) {
      SourceLocation NewEnd = Arg->getEndLoc();
      if (NewEnd.isValid()) {
        End = NewEnd;
        break;
      }
    }
  }

  return End;
}

CXXOperatorCallExpr::CXXOperatorCallExpr(OverloadedOperatorKind OpKind,
                                         Expr *Fn, ArrayRef<Expr *> Args,
                                         QualType Ty, ExprValueKind VK,
                                         SourceLocation OperatorLoc,
                                         FPOptionsOverride FPFeatures,
                                         ADLCallKind UsesADL)
    : CallExpr(CXXOperatorCallExprClass, Fn, /*PreArgs=*/{}, Args, Ty, VK,
               OperatorLoc, FPFeatures, /*MinNumArgs=*/0, UsesADL) {
  CXXOperatorCallExprBits.OperatorKind = OpKind;
  assert(
      (CXXOperatorCallExprBits.OperatorKind == static_cast<unsigned>(OpKind)) &&
      "OperatorKind overflow!");
  Range = getSourceRangeImpl();
}

CXXOperatorCallExpr::CXXOperatorCallExpr(unsigned NumArgs, bool HasFPFeatures,
                                         EmptyShell Empty)
    : CallExpr(CXXOperatorCallExprClass, /*NumPreArgs=*/0, NumArgs,
               HasFPFeatures, Empty) {}

CXXOperatorCallExpr *
CXXOperatorCallExpr::Create(const ASTContext &Ctx,
                            OverloadedOperatorKind OpKind, Expr *Fn,
                            ArrayRef<Expr *> Args, QualType Ty,
                            ExprValueKind VK, SourceLocation OperatorLoc,
                            FPOptionsOverride FPFeatures, ADLCallKind UsesADL) {
  // Allocate storage for the trailing objects of CallExpr.
  unsigned NumArgs = Args.size();
  unsigned SizeOfTrailingObjects = CallExpr::sizeOfTrailingObjects(
      /*NumPreArgs=*/0, NumArgs, FPFeatures.requiresTrailingStorage());
  void *Mem = Ctx.Allocate(sizeof(CXXOperatorCallExpr) + SizeOfTrailingObjects,
                           alignof(CXXOperatorCallExpr));
  return new (Mem) CXXOperatorCallExpr(OpKind, Fn, Args, Ty, VK, OperatorLoc,
                                       FPFeatures, UsesADL);
}

CXXOperatorCallExpr *CXXOperatorCallExpr::CreateEmpty(const ASTContext &Ctx,
                                                      unsigned NumArgs,
                                                      bool HasFPFeatures,
                                                      EmptyShell Empty) {
  // Allocate storage for the trailing objects of CallExpr.
  unsigned SizeOfTrailingObjects =
      CallExpr::sizeOfTrailingObjects(/*NumPreArgs=*/0, NumArgs, HasFPFeatures);
  void *Mem = Ctx.Allocate(sizeof(CXXOperatorCallExpr) + SizeOfTrailingObjects,
                           alignof(CXXOperatorCallExpr));
  return new (Mem) CXXOperatorCallExpr(NumArgs, HasFPFeatures, Empty);
}

SourceRange CXXOperatorCallExpr::getSourceRangeImpl() const {
  OverloadedOperatorKind Kind = getOperator();
  if (Kind == OO_PlusPlus || Kind == OO_MinusMinus) {
    if (getNumArgs() == 1)
      // Prefix operator
      return SourceRange(getOperatorLoc(), getArg(0)->getEndLoc());
    else
      // Postfix operator
      return SourceRange(getArg(0)->getBeginLoc(), getOperatorLoc());
  } else if (Kind == OO_Arrow) {
    return SourceRange(getArg(0)->getBeginLoc(), getOperatorLoc());
  } else if (Kind == OO_Call) {
    return SourceRange(getArg(0)->getBeginLoc(), getRParenLoc());
  } else if (Kind == OO_Subscript) {
    return SourceRange(getArg(0)->getBeginLoc(), getRParenLoc());
  } else if (getNumArgs() == 1) {
    return SourceRange(getOperatorLoc(), getArg(0)->getEndLoc());
  } else if (getNumArgs() == 2) {
    return SourceRange(getArg(0)->getBeginLoc(), getArg(1)->getEndLoc());
  } else {
    return getOperatorLoc();
  }
}

CXXMemberCallExpr::CXXMemberCallExpr(Expr *Fn, ArrayRef<Expr *> Args,
                                     QualType Ty, ExprValueKind VK,
                                     SourceLocation RP,
                                     FPOptionsOverride FPOptions,
                                     unsigned MinNumArgs)
    : CallExpr(CXXMemberCallExprClass, Fn, /*PreArgs=*/{}, Args, Ty, VK, RP,
               FPOptions, MinNumArgs, NotADL) {}

CXXMemberCallExpr::CXXMemberCallExpr(unsigned NumArgs, bool HasFPFeatures,
                                     EmptyShell Empty)
    : CallExpr(CXXMemberCallExprClass, /*NumPreArgs=*/0, NumArgs, HasFPFeatures,
               Empty) {}

CXXMemberCallExpr *CXXMemberCallExpr::Create(const ASTContext &Ctx, Expr *Fn,
                                             ArrayRef<Expr *> Args, QualType Ty,
                                             ExprValueKind VK,
                                             SourceLocation RP,
                                             FPOptionsOverride FPFeatures,
                                             unsigned MinNumArgs) {
  // Allocate storage for the trailing objects of CallExpr.
  unsigned NumArgs = std::max<unsigned>(Args.size(), MinNumArgs);
  unsigned SizeOfTrailingObjects = CallExpr::sizeOfTrailingObjects(
      /*NumPreArgs=*/0, NumArgs, FPFeatures.requiresTrailingStorage());
  void *Mem = Ctx.Allocate(sizeof(CXXMemberCallExpr) + SizeOfTrailingObjects,
                           alignof(CXXMemberCallExpr));
  return new (Mem)
      CXXMemberCallExpr(Fn, Args, Ty, VK, RP, FPFeatures, MinNumArgs);
}

CXXMemberCallExpr *CXXMemberCallExpr::CreateEmpty(const ASTContext &Ctx,
                                                  unsigned NumArgs,
                                                  bool HasFPFeatures,
                                                  EmptyShell Empty) {
  // Allocate storage for the trailing objects of CallExpr.
  unsigned SizeOfTrailingObjects =
      CallExpr::sizeOfTrailingObjects(/*NumPreArgs=*/0, NumArgs, HasFPFeatures);
  void *Mem = Ctx.Allocate(sizeof(CXXMemberCallExpr) + SizeOfTrailingObjects,
                           alignof(CXXMemberCallExpr));
  return new (Mem) CXXMemberCallExpr(NumArgs, HasFPFeatures, Empty);
}

Expr *CXXMemberCallExpr::getImplicitObjectArgument() const {
  const Expr *Callee = getCallee()->IgnoreParens();
  if (const auto *MemExpr = dyn_cast<MemberExpr>(Callee))
    return MemExpr->getBase();
  if (const auto *BO = dyn_cast<BinaryOperator>(Callee))
    if (BO->getOpcode() == BO_PtrMemD || BO->getOpcode() == BO_PtrMemI)
      return BO->getLHS();

  // FIXME: Will eventually need to cope with member pointers.
  return nullptr;
}

QualType CXXMemberCallExpr::getObjectType() const {
  QualType Ty = getImplicitObjectArgument()->getType();
  if (Ty->isPointerType())
    Ty = Ty->getPointeeType();
  return Ty;
}

CXXMethodDecl *CXXMemberCallExpr::getMethodDecl() const {
  if (const auto *MemExpr = dyn_cast<MemberExpr>(getCallee()->IgnoreParens()))
    return cast<CXXMethodDecl>(MemExpr->getMemberDecl());

  // FIXME: Will eventually need to cope with member pointers.
  // NOTE: Update makeTailCallIfSwiftAsync on fixing this.
  return nullptr;
}

CXXRecordDecl *CXXMemberCallExpr::getRecordDecl() const {
  Expr* ThisArg = getImplicitObjectArgument();
  if (!ThisArg)
    return nullptr;

  if (ThisArg->getType()->isAnyPointerType())
    return ThisArg->getType()->getPointeeType()->getAsCXXRecordDecl();

  return ThisArg->getType()->getAsCXXRecordDecl();
}

//===----------------------------------------------------------------------===//
//  Named casts
//===----------------------------------------------------------------------===//

/// getCastName - Get the name of the C++ cast being used, e.g.,
/// "static_cast", "dynamic_cast", "reinterpret_cast", or
/// "const_cast". The returned pointer must not be freed.
const char *CXXNamedCastExpr::getCastName() const {
  switch (getStmtClass()) {
  case CXXStaticCastExprClass:      return "static_cast";
  case CXXDynamicCastExprClass:     return "dynamic_cast";
  case CXXReinterpretCastExprClass: return "reinterpret_cast";
  case CXXConstCastExprClass:       return "const_cast";
  case CXXAddrspaceCastExprClass:   return "addrspace_cast";
  default:                          return "<invalid cast>";
  }
}

CXXStaticCastExpr *
CXXStaticCastExpr::Create(const ASTContext &C, QualType T, ExprValueKind VK,
                          CastKind K, Expr *Op, const CXXCastPath *BasePath,
                          TypeSourceInfo *WrittenTy, FPOptionsOverride FPO,
                          SourceLocation L, SourceLocation RParenLoc,
                          SourceRange AngleBrackets) {
  unsigned PathSize = (BasePath ? BasePath->size() : 0);
  void *Buffer =
      C.Allocate(totalSizeToAlloc<CXXBaseSpecifier *, FPOptionsOverride>(
          PathSize, FPO.requiresTrailingStorage()));
  auto *E = new (Buffer) CXXStaticCastExpr(T, VK, K, Op, PathSize, WrittenTy,
                                           FPO, L, RParenLoc, AngleBrackets);
  if (PathSize)
    std::uninitialized_copy_n(BasePath->data(), BasePath->size(),
                              E->getTrailingObjects<CXXBaseSpecifier *>());
  return E;
}

CXXStaticCastExpr *CXXStaticCastExpr::CreateEmpty(const ASTContext &C,
                                                  unsigned PathSize,
                                                  bool HasFPFeatures) {
  void *Buffer =
      C.Allocate(totalSizeToAlloc<CXXBaseSpecifier *, FPOptionsOverride>(
          PathSize, HasFPFeatures));
  return new (Buffer) CXXStaticCastExpr(EmptyShell(), PathSize, HasFPFeatures);
}

CXXDynamicCastExpr *CXXDynamicCastExpr::Create(const ASTContext &C, QualType T,
                                               ExprValueKind VK,
                                               CastKind K, Expr *Op,
                                               const CXXCastPath *BasePath,
                                               TypeSourceInfo *WrittenTy,
                                               SourceLocation L,
                                               SourceLocation RParenLoc,
                                               SourceRange AngleBrackets) {
  unsigned PathSize = (BasePath ? BasePath->size() : 0);
  void *Buffer = C.Allocate(totalSizeToAlloc<CXXBaseSpecifier *>(PathSize));
  auto *E =
      new (Buffer) CXXDynamicCastExpr(T, VK, K, Op, PathSize, WrittenTy, L,
                                      RParenLoc, AngleBrackets);
  if (PathSize)
    std::uninitialized_copy_n(BasePath->data(), BasePath->size(),
                              E->getTrailingObjects<CXXBaseSpecifier *>());
  return E;
}

CXXDynamicCastExpr *CXXDynamicCastExpr::CreateEmpty(const ASTContext &C,
                                                    unsigned PathSize) {
  void *Buffer = C.Allocate(totalSizeToAlloc<CXXBaseSpecifier *>(PathSize));
  return new (Buffer) CXXDynamicCastExpr(EmptyShell(), PathSize);
}

/// isAlwaysNull - Return whether the result of the dynamic_cast is proven
/// to always be null. For example:
///
/// struct A { };
/// struct B final : A { };
/// struct C { };
///
/// C *f(B* b) { return dynamic_cast<C*>(b); }
bool CXXDynamicCastExpr::isAlwaysNull() const
{
  QualType SrcType = getSubExpr()->getType();
  QualType DestType = getType();

  if (const auto *SrcPTy = SrcType->getAs<PointerType>()) {
    SrcType = SrcPTy->getPointeeType();
    DestType = DestType->castAs<PointerType>()->getPointeeType();
  }

  if (DestType->isVoidType())
    return false;

  const auto *SrcRD =
      cast<CXXRecordDecl>(SrcType->castAs<RecordType>()->getDecl());

  if (!SrcRD->hasAttr<FinalAttr>())
    return false;

  const auto *DestRD =
      cast<CXXRecordDecl>(DestType->castAs<RecordType>()->getDecl());

  return !DestRD->isDerivedFrom(SrcRD);
}

CXXReinterpretCastExpr *
CXXReinterpretCastExpr::Create(const ASTContext &C, QualType T,
                               ExprValueKind VK, CastKind K, Expr *Op,
                               const CXXCastPath *BasePath,
                               TypeSourceInfo *WrittenTy, SourceLocation L,
                               SourceLocation RParenLoc,
                               SourceRange AngleBrackets) {
  unsigned PathSize = (BasePath ? BasePath->size() : 0);
  void *Buffer = C.Allocate(totalSizeToAlloc<CXXBaseSpecifier *>(PathSize));
  auto *E =
      new (Buffer) CXXReinterpretCastExpr(T, VK, K, Op, PathSize, WrittenTy, L,
                                          RParenLoc, AngleBrackets);
  if (PathSize)
    std::uninitialized_copy_n(BasePath->data(), BasePath->size(),
                              E->getTrailingObjects<CXXBaseSpecifier *>());
  return E;
}

CXXReinterpretCastExpr *
CXXReinterpretCastExpr::CreateEmpty(const ASTContext &C, unsigned PathSize) {
  void *Buffer = C.Allocate(totalSizeToAlloc<CXXBaseSpecifier *>(PathSize));
  return new (Buffer) CXXReinterpretCastExpr(EmptyShell(), PathSize);
}

CXXConstCastExpr *CXXConstCastExpr::Create(const ASTContext &C, QualType T,
                                           ExprValueKind VK, Expr *Op,
                                           TypeSourceInfo *WrittenTy,
                                           SourceLocation L,
                                           SourceLocation RParenLoc,
                                           SourceRange AngleBrackets) {
  return new (C) CXXConstCastExpr(T, VK, Op, WrittenTy, L, RParenLoc, AngleBrackets);
}

CXXConstCastExpr *CXXConstCastExpr::CreateEmpty(const ASTContext &C) {
  return new (C) CXXConstCastExpr(EmptyShell());
}

CXXAddrspaceCastExpr *
CXXAddrspaceCastExpr::Create(const ASTContext &C, QualType T, ExprValueKind VK,
                             CastKind K, Expr *Op, TypeSourceInfo *WrittenTy,
                             SourceLocation L, SourceLocation RParenLoc,
                             SourceRange AngleBrackets) {
  return new (C) CXXAddrspaceCastExpr(T, VK, K, Op, WrittenTy, L, RParenLoc,
                                      AngleBrackets);
}

CXXAddrspaceCastExpr *CXXAddrspaceCastExpr::CreateEmpty(const ASTContext &C) {
  return new (C) CXXAddrspaceCastExpr(EmptyShell());
}

CXXFunctionalCastExpr *CXXFunctionalCastExpr::Create(
    const ASTContext &C, QualType T, ExprValueKind VK, TypeSourceInfo *Written,
    CastKind K, Expr *Op, const CXXCastPath *BasePath, FPOptionsOverride FPO,
    SourceLocation L, SourceLocation R) {
  unsigned PathSize = (BasePath ? BasePath->size() : 0);
  void *Buffer =
      C.Allocate(totalSizeToAlloc<CXXBaseSpecifier *, FPOptionsOverride>(
          PathSize, FPO.requiresTrailingStorage()));
  auto *E = new (Buffer)
      CXXFunctionalCastExpr(T, VK, Written, K, Op, PathSize, FPO, L, R);
  if (PathSize)
    std::uninitialized_copy_n(BasePath->data(), BasePath->size(),
                              E->getTrailingObjects<CXXBaseSpecifier *>());
  return E;
}

CXXFunctionalCastExpr *CXXFunctionalCastExpr::CreateEmpty(const ASTContext &C,
                                                          unsigned PathSize,
                                                          bool HasFPFeatures) {
  void *Buffer =
      C.Allocate(totalSizeToAlloc<CXXBaseSpecifier *, FPOptionsOverride>(
          PathSize, HasFPFeatures));
  return new (Buffer)
      CXXFunctionalCastExpr(EmptyShell(), PathSize, HasFPFeatures);
}

SourceLocation CXXFunctionalCastExpr::getBeginLoc() const {
  return getTypeInfoAsWritten()->getTypeLoc().getBeginLoc();
}

SourceLocation CXXFunctionalCastExpr::getEndLoc() const {
  return RParenLoc.isValid() ? RParenLoc : getSubExpr()->getEndLoc();
}

UserDefinedLiteral::UserDefinedLiteral(Expr *Fn, ArrayRef<Expr *> Args,
                                       QualType Ty, ExprValueKind VK,
                                       SourceLocation LitEndLoc,
                                       SourceLocation SuffixLoc,
                                       FPOptionsOverride FPFeatures)
    : CallExpr(UserDefinedLiteralClass, Fn, /*PreArgs=*/{}, Args, Ty, VK,
               LitEndLoc, FPFeatures, /*MinNumArgs=*/0, NotADL),
      UDSuffixLoc(SuffixLoc) {}

UserDefinedLiteral::UserDefinedLiteral(unsigned NumArgs, bool HasFPFeatures,
                                       EmptyShell Empty)
    : CallExpr(UserDefinedLiteralClass, /*NumPreArgs=*/0, NumArgs,
               HasFPFeatures, Empty) {}

UserDefinedLiteral *UserDefinedLiteral::Create(const ASTContext &Ctx, Expr *Fn,
                                               ArrayRef<Expr *> Args,
                                               QualType Ty, ExprValueKind VK,
                                               SourceLocation LitEndLoc,
                                               SourceLocation SuffixLoc,
                                               FPOptionsOverride FPFeatures) {
  // Allocate storage for the trailing objects of CallExpr.
  unsigned NumArgs = Args.size();
  unsigned SizeOfTrailingObjects = CallExpr::sizeOfTrailingObjects(
      /*NumPreArgs=*/0, NumArgs, FPFeatures.requiresTrailingStorage());
  void *Mem = Ctx.Allocate(sizeof(UserDefinedLiteral) + SizeOfTrailingObjects,
                           alignof(UserDefinedLiteral));
  return new (Mem)
      UserDefinedLiteral(Fn, Args, Ty, VK, LitEndLoc, SuffixLoc, FPFeatures);
}

UserDefinedLiteral *UserDefinedLiteral::CreateEmpty(const ASTContext &Ctx,
                                                    unsigned NumArgs,
                                                    bool HasFPOptions,
                                                    EmptyShell Empty) {
  // Allocate storage for the trailing objects of CallExpr.
  unsigned SizeOfTrailingObjects =
      CallExpr::sizeOfTrailingObjects(/*NumPreArgs=*/0, NumArgs, HasFPOptions);
  void *Mem = Ctx.Allocate(sizeof(UserDefinedLiteral) + SizeOfTrailingObjects,
                           alignof(UserDefinedLiteral));
  return new (Mem) UserDefinedLiteral(NumArgs, HasFPOptions, Empty);
}

UserDefinedLiteral::LiteralOperatorKind
UserDefinedLiteral::getLiteralOperatorKind() const {
  if (getNumArgs() == 0)
    return LOK_Template;
  if (getNumArgs() == 2)
    return LOK_String;

  assert(getNumArgs() == 1 && "unexpected #args in literal operator call");
  QualType ParamTy =
    cast<FunctionDecl>(getCalleeDecl())->getParamDecl(0)->getType();
  if (ParamTy->isPointerType())
    return LOK_Raw;
  if (ParamTy->isAnyCharacterType())
    return LOK_Character;
  if (ParamTy->isIntegerType())
    return LOK_Integer;
  if (ParamTy->isFloatingType())
    return LOK_Floating;

  llvm_unreachable("unknown kind of literal operator");
}

Expr *UserDefinedLiteral::getCookedLiteral() {
#ifndef NDEBUG
  LiteralOperatorKind LOK = getLiteralOperatorKind();
  assert(LOK != LOK_Template && LOK != LOK_Raw && "not a cooked literal");
#endif
  return getArg(0);
}

const IdentifierInfo *UserDefinedLiteral::getUDSuffix() const {
  return cast<FunctionDecl>(getCalleeDecl())->getLiteralIdentifier();
}

CXXDefaultInitExpr::CXXDefaultInitExpr(const ASTContext &Ctx,
                                       SourceLocation Loc, FieldDecl *Field,
                                       QualType Ty, DeclContext *UsedContext)
    : Expr(CXXDefaultInitExprClass, Ty.getNonLValueExprType(Ctx),
           Ty->isLValueReferenceType()   ? VK_LValue
           : Ty->isRValueReferenceType() ? VK_XValue
                                         : VK_PRValue,
           /*FIXME*/ OK_Ordinary),
      Field(Field), UsedContext(UsedContext) {
  CXXDefaultInitExprBits.Loc = Loc;
  assert(Field->hasInClassInitializer());

  setDependence(computeDependence(this));
}

CXXTemporary *CXXTemporary::Create(const ASTContext &C,
                                   const CXXDestructorDecl *Destructor) {
  return new (C) CXXTemporary(Destructor);
}

CXXBindTemporaryExpr *CXXBindTemporaryExpr::Create(const ASTContext &C,
                                                   CXXTemporary *Temp,
                                                   Expr* SubExpr) {
  assert((SubExpr->getType()->isRecordType() ||
          SubExpr->getType()->isArrayType()) &&
         "Expression bound to a temporary must have record or array type!");

  return new (C) CXXBindTemporaryExpr(Temp, SubExpr);
}

CXXTemporaryObjectExpr::CXXTemporaryObjectExpr(
    CXXConstructorDecl *Cons, QualType Ty, TypeSourceInfo *TSI,
    ArrayRef<Expr *> Args, SourceRange ParenOrBraceRange,
    bool HadMultipleCandidates, bool ListInitialization,
    bool StdInitListInitialization, bool ZeroInitialization)
    : CXXConstructExpr(
          CXXTemporaryObjectExprClass, Ty, TSI->getTypeLoc().getBeginLoc(),
          Cons, /* Elidable=*/false, Args, HadMultipleCandidates,
          ListInitialization, StdInitListInitialization, ZeroInitialization,
          CXXConstructExpr::CK_Complete, ParenOrBraceRange),
      TSI(TSI) {}

CXXTemporaryObjectExpr::CXXTemporaryObjectExpr(EmptyShell Empty,
                                               unsigned NumArgs)
    : CXXConstructExpr(CXXTemporaryObjectExprClass, Empty, NumArgs) {}

CXXTemporaryObjectExpr *CXXTemporaryObjectExpr::Create(
    const ASTContext &Ctx, CXXConstructorDecl *Cons, QualType Ty,
    TypeSourceInfo *TSI, ArrayRef<Expr *> Args, SourceRange ParenOrBraceRange,
    bool HadMultipleCandidates, bool ListInitialization,
    bool StdInitListInitialization, bool ZeroInitialization) {
  unsigned SizeOfTrailingObjects = sizeOfTrailingObjects(Args.size());
  void *Mem =
      Ctx.Allocate(sizeof(CXXTemporaryObjectExpr) + SizeOfTrailingObjects,
                   alignof(CXXTemporaryObjectExpr));
  return new (Mem) CXXTemporaryObjectExpr(
      Cons, Ty, TSI, Args, ParenOrBraceRange, HadMultipleCandidates,
      ListInitialization, StdInitListInitialization, ZeroInitialization);
}

CXXTemporaryObjectExpr *
CXXTemporaryObjectExpr::CreateEmpty(const ASTContext &Ctx, unsigned NumArgs) {
  unsigned SizeOfTrailingObjects = sizeOfTrailingObjects(NumArgs);
  void *Mem =
      Ctx.Allocate(sizeof(CXXTemporaryObjectExpr) + SizeOfTrailingObjects,
                   alignof(CXXTemporaryObjectExpr));
  return new (Mem) CXXTemporaryObjectExpr(EmptyShell(), NumArgs);
}

SourceLocation CXXTemporaryObjectExpr::getBeginLoc() const {
  return getTypeSourceInfo()->getTypeLoc().getBeginLoc();
}

SourceLocation CXXTemporaryObjectExpr::getEndLoc() const {
  SourceLocation Loc = getParenOrBraceRange().getEnd();
  if (Loc.isInvalid() && getNumArgs())
    Loc = getArg(getNumArgs() - 1)->getEndLoc();
  return Loc;
}

CXXConstructExpr *CXXConstructExpr::Create(
    const ASTContext &Ctx, QualType Ty, SourceLocation Loc,
    CXXConstructorDecl *Ctor, bool Elidable, ArrayRef<Expr *> Args,
    bool HadMultipleCandidates, bool ListInitialization,
    bool StdInitListInitialization, bool ZeroInitialization,
    ConstructionKind ConstructKind, SourceRange ParenOrBraceRange) {
  unsigned SizeOfTrailingObjects = sizeOfTrailingObjects(Args.size());
  void *Mem = Ctx.Allocate(sizeof(CXXConstructExpr) + SizeOfTrailingObjects,
                           alignof(CXXConstructExpr));
  return new (Mem) CXXConstructExpr(
      CXXConstructExprClass, Ty, Loc, Ctor, Elidable, Args,
      HadMultipleCandidates, ListInitialization, StdInitListInitialization,
      ZeroInitialization, ConstructKind, ParenOrBraceRange);
}

CXXConstructExpr *CXXConstructExpr::CreateEmpty(const ASTContext &Ctx,
                                                unsigned NumArgs) {
  unsigned SizeOfTrailingObjects = sizeOfTrailingObjects(NumArgs);
  void *Mem = Ctx.Allocate(sizeof(CXXConstructExpr) + SizeOfTrailingObjects,
                           alignof(CXXConstructExpr));
  return new (Mem)
      CXXConstructExpr(CXXConstructExprClass, EmptyShell(), NumArgs);
}

CXXConstructExpr::CXXConstructExpr(
    StmtClass SC, QualType Ty, SourceLocation Loc, CXXConstructorDecl *Ctor,
    bool Elidable, ArrayRef<Expr *> Args, bool HadMultipleCandidates,
    bool ListInitialization, bool StdInitListInitialization,
    bool ZeroInitialization, ConstructionKind ConstructKind,
    SourceRange ParenOrBraceRange)
    : Expr(SC, Ty, VK_PRValue, OK_Ordinary), Constructor(Ctor),
      ParenOrBraceRange(ParenOrBraceRange), NumArgs(Args.size()) {
  CXXConstructExprBits.Elidable = Elidable;
  CXXConstructExprBits.HadMultipleCandidates = HadMultipleCandidates;
  CXXConstructExprBits.ListInitialization = ListInitialization;
  CXXConstructExprBits.StdInitListInitialization = StdInitListInitialization;
  CXXConstructExprBits.ZeroInitialization = ZeroInitialization;
  CXXConstructExprBits.ConstructionKind = ConstructKind;
  CXXConstructExprBits.Loc = Loc;

  Stmt **TrailingArgs = getTrailingArgs();
  for (unsigned I = 0, N = Args.size(); I != N; ++I) {
    assert(Args[I] && "NULL argument in CXXConstructExpr!");
    TrailingArgs[I] = Args[I];
  }

  setDependence(computeDependence(this));
}

CXXConstructExpr::CXXConstructExpr(StmtClass SC, EmptyShell Empty,
                                   unsigned NumArgs)
    : Expr(SC, Empty), NumArgs(NumArgs) {}

LambdaCapture::LambdaCapture(SourceLocation Loc, bool Implicit,
                             LambdaCaptureKind Kind, VarDecl *Var,
                             SourceLocation EllipsisLoc)
    : DeclAndBits(Var, 0), Loc(Loc), EllipsisLoc(EllipsisLoc) {
  unsigned Bits = 0;
  if (Implicit)
    Bits |= Capture_Implicit;

  switch (Kind) {
  case LCK_StarThis:
    Bits |= Capture_ByCopy;
    LLVM_FALLTHROUGH;
  case LCK_This:
    assert(!Var && "'this' capture cannot have a variable!");
    Bits |= Capture_This;
    break;

  case LCK_ByCopy:
    Bits |= Capture_ByCopy;
    LLVM_FALLTHROUGH;
  case LCK_ByRef:
    assert(Var && "capture must have a variable!");
    break;
  case LCK_VLAType:
    assert(!Var && "VLA type capture cannot have a variable!");
    break;
  }
  DeclAndBits.setInt(Bits);
}

LambdaCaptureKind LambdaCapture::getCaptureKind() const {
  if (capturesVLAType())
    return LCK_VLAType;
  bool CapByCopy = DeclAndBits.getInt() & Capture_ByCopy;
  if (capturesThis())
    return CapByCopy ? LCK_StarThis : LCK_This;
  return CapByCopy ? LCK_ByCopy : LCK_ByRef;
}

LambdaExpr::LambdaExpr(QualType T, SourceRange IntroducerRange,
                       LambdaCaptureDefault CaptureDefault,
                       SourceLocation CaptureDefaultLoc, bool ExplicitParams,
                       bool ExplicitResultType, ArrayRef<Expr *> CaptureInits,
                       SourceLocation ClosingBrace,
                       bool ContainsUnexpandedParameterPack)
    : Expr(LambdaExprClass, T, VK_PRValue, OK_Ordinary),
      IntroducerRange(IntroducerRange), CaptureDefaultLoc(CaptureDefaultLoc),
      ClosingBrace(ClosingBrace) {
  LambdaExprBits.NumCaptures = CaptureInits.size();
  LambdaExprBits.CaptureDefault = CaptureDefault;
  LambdaExprBits.ExplicitParams = ExplicitParams;
  LambdaExprBits.ExplicitResultType = ExplicitResultType;

  CXXRecordDecl *Class = getLambdaClass();
  (void)Class;
  assert(capture_size() == Class->capture_size() && "Wrong number of captures");
  assert(getCaptureDefault() == Class->getLambdaCaptureDefault());

  // Copy initialization expressions for the non-static data members.
  Stmt **Stored = getStoredStmts();
  for (unsigned I = 0, N = CaptureInits.size(); I != N; ++I)
    *Stored++ = CaptureInits[I];

  // Copy the body of the lambda.
  *Stored++ = getCallOperator()->getBody();

  setDependence(computeDependence(this, ContainsUnexpandedParameterPack));
}

LambdaExpr::LambdaExpr(EmptyShell Empty, unsigned NumCaptures)
    : Expr(LambdaExprClass, Empty) {
  LambdaExprBits.NumCaptures = NumCaptures;

  // Initially don't initialize the body of the LambdaExpr. The body will
  // be lazily deserialized when needed.
  getStoredStmts()[NumCaptures] = nullptr; // Not one past the end.
}

LambdaExpr *LambdaExpr::Create(const ASTContext &Context, CXXRecordDecl *Class,
                               SourceRange IntroducerRange,
                               LambdaCaptureDefault CaptureDefault,
                               SourceLocation CaptureDefaultLoc,
                               bool ExplicitParams, bool ExplicitResultType,
                               ArrayRef<Expr *> CaptureInits,
                               SourceLocation ClosingBrace,
                               bool ContainsUnexpandedParameterPack) {
  // Determine the type of the expression (i.e., the type of the
  // function object we're creating).
  QualType T = Context.getTypeDeclType(Class);

  unsigned Size = totalSizeToAlloc<Stmt *>(CaptureInits.size() + 1);
  void *Mem = Context.Allocate(Size);
  return new (Mem)
      LambdaExpr(T, IntroducerRange, CaptureDefault, CaptureDefaultLoc,
                 ExplicitParams, ExplicitResultType, CaptureInits, ClosingBrace,
                 ContainsUnexpandedParameterPack);
}

LambdaExpr *LambdaExpr::CreateDeserialized(const ASTContext &C,
                                           unsigned NumCaptures) {
  unsigned Size = totalSizeToAlloc<Stmt *>(NumCaptures + 1);
  void *Mem = C.Allocate(Size);
  return new (Mem) LambdaExpr(EmptyShell(), NumCaptures);
}

void LambdaExpr::initBodyIfNeeded() const {
  if (!getStoredStmts()[capture_size()]) {
    auto *This = const_cast<LambdaExpr *>(this);
    This->getStoredStmts()[capture_size()] = getCallOperator()->getBody();
  }
}

Stmt *LambdaExpr::getBody() const {
  initBodyIfNeeded();
  return getStoredStmts()[capture_size()];
}

const CompoundStmt *LambdaExpr::getCompoundStmtBody() const {
  Stmt *Body = getBody();
  if (const auto *CoroBody = dyn_cast<CoroutineBodyStmt>(Body))
    return cast<CompoundStmt>(CoroBody->getBody());
  return cast<CompoundStmt>(Body);
}

bool LambdaExpr::isInitCapture(const LambdaCapture *C) const {
  return (C->capturesVariable() && C->getCapturedVar()->isInitCapture() &&
          (getCallOperator() == C->getCapturedVar()->getDeclContext()));
}

LambdaExpr::capture_iterator LambdaExpr::capture_begin() const {
  return getLambdaClass()->getLambdaData().Captures;
}

LambdaExpr::capture_iterator LambdaExpr::capture_end() const {
  return capture_begin() + capture_size();
}

LambdaExpr::capture_range LambdaExpr::captures() const {
  return capture_range(capture_begin(), capture_end());
}

LambdaExpr::capture_iterator LambdaExpr::explicit_capture_begin() const {
  return capture_begin();
}

LambdaExpr::capture_iterator LambdaExpr::explicit_capture_end() const {
  struct CXXRecordDecl::LambdaDefinitionData &Data
    = getLambdaClass()->getLambdaData();
  return Data.Captures + Data.NumExplicitCaptures;
}

LambdaExpr::capture_range LambdaExpr::explicit_captures() const {
  return capture_range(explicit_capture_begin(), explicit_capture_end());
}

LambdaExpr::capture_iterator LambdaExpr::implicit_capture_begin() const {
  return explicit_capture_end();
}

LambdaExpr::capture_iterator LambdaExpr::implicit_capture_end() const {
  return capture_end();
}

LambdaExpr::capture_range LambdaExpr::implicit_captures() const {
  return capture_range(implicit_capture_begin(), implicit_capture_end());
}

CXXRecordDecl *LambdaExpr::getLambdaClass() const {
  return getType()->getAsCXXRecordDecl();
}

CXXMethodDecl *LambdaExpr::getCallOperator() const {
  CXXRecordDecl *Record = getLambdaClass();
  return Record->getLambdaCallOperator();
}

FunctionTemplateDecl *LambdaExpr::getDependentCallOperator() const {
  CXXRecordDecl *Record = getLambdaClass();
  return Record->getDependentLambdaCallOperator();
}

TemplateParameterList *LambdaExpr::getTemplateParameterList() const {
  CXXRecordDecl *Record = getLambdaClass();
  return Record->getGenericLambdaTemplateParameterList();
}

ArrayRef<NamedDecl *> LambdaExpr::getExplicitTemplateParameters() const {
  const CXXRecordDecl *Record = getLambdaClass();
  return Record->getLambdaExplicitTemplateParameters();
}

Expr *LambdaExpr::getTrailingRequiresClause() const {
  return getCallOperator()->getTrailingRequiresClause();
}

bool LambdaExpr::isMutable() const { return !getCallOperator()->isConst(); }

LambdaExpr::child_range LambdaExpr::children() {
  initBodyIfNeeded();
  return child_range(getStoredStmts(), getStoredStmts() + capture_size() + 1);
}

LambdaExpr::const_child_range LambdaExpr::children() const {
  initBodyIfNeeded();
  return const_child_range(getStoredStmts(),
                           getStoredStmts() + capture_size() + 1);
}

ExprWithCleanups::ExprWithCleanups(Expr *subexpr,
                                   bool CleanupsHaveSideEffects,
                                   ArrayRef<CleanupObject> objects)
    : FullExpr(ExprWithCleanupsClass, subexpr) {
  ExprWithCleanupsBits.CleanupsHaveSideEffects = CleanupsHaveSideEffects;
  ExprWithCleanupsBits.NumObjects = objects.size();
  for (unsigned i = 0, e = objects.size(); i != e; ++i)
    getTrailingObjects<CleanupObject>()[i] = objects[i];
}

ExprWithCleanups *ExprWithCleanups::Create(const ASTContext &C, Expr *subexpr,
                                           bool CleanupsHaveSideEffects,
                                           ArrayRef<CleanupObject> objects) {
  void *buffer = C.Allocate(totalSizeToAlloc<CleanupObject>(objects.size()),
                            alignof(ExprWithCleanups));
  return new (buffer)
      ExprWithCleanups(subexpr, CleanupsHaveSideEffects, objects);
}

ExprWithCleanups::ExprWithCleanups(EmptyShell empty, unsigned numObjects)
    : FullExpr(ExprWithCleanupsClass, empty) {
  ExprWithCleanupsBits.NumObjects = numObjects;
}

ExprWithCleanups *ExprWithCleanups::Create(const ASTContext &C,
                                           EmptyShell empty,
                                           unsigned numObjects) {
  void *buffer = C.Allocate(totalSizeToAlloc<CleanupObject>(numObjects),
                            alignof(ExprWithCleanups));
  return new (buffer) ExprWithCleanups(empty, numObjects);
}

CXXUnresolvedConstructExpr::CXXUnresolvedConstructExpr(QualType T,
                                                       TypeSourceInfo *TSI,
                                                       SourceLocation LParenLoc,
                                                       ArrayRef<Expr *> Args,
                                                       SourceLocation RParenLoc)
    : Expr(CXXUnresolvedConstructExprClass, T,
           (TSI->getType()->isLValueReferenceType()   ? VK_LValue
            : TSI->getType()->isRValueReferenceType() ? VK_XValue
                                                      : VK_PRValue),
           OK_Ordinary),
      TSI(TSI), LParenLoc(LParenLoc), RParenLoc(RParenLoc) {
  CXXUnresolvedConstructExprBits.NumArgs = Args.size();
  auto **StoredArgs = getTrailingObjects<Expr *>();
  for (unsigned I = 0; I != Args.size(); ++I)
    StoredArgs[I] = Args[I];
  setDependence(computeDependence(this));
}

CXXUnresolvedConstructExpr *CXXUnresolvedConstructExpr::Create(
    const ASTContext &Context, QualType T, TypeSourceInfo *TSI, SourceLocation LParenLoc,
    ArrayRef<Expr *> Args, SourceLocation RParenLoc) {
  void *Mem = Context.Allocate(totalSizeToAlloc<Expr *>(Args.size()));
  return new (Mem)
      CXXUnresolvedConstructExpr(T, TSI, LParenLoc, Args, RParenLoc);
}

CXXUnresolvedConstructExpr *
CXXUnresolvedConstructExpr::CreateEmpty(const ASTContext &Context,
                                        unsigned NumArgs) {
  void *Mem = Context.Allocate(totalSizeToAlloc<Expr *>(NumArgs));
  return new (Mem) CXXUnresolvedConstructExpr(EmptyShell(), NumArgs);
}

SourceLocation CXXUnresolvedConstructExpr::getBeginLoc() const {
  return TSI->getTypeLoc().getBeginLoc();
}

CXXDependentScopeMemberExpr::CXXDependentScopeMemberExpr(
    const ASTContext &Ctx, Expr *Base, QualType BaseType, bool IsArrow,
    SourceLocation OperatorLoc, NestedNameSpecifierLoc QualifierLoc,
    SourceLocation TemplateKWLoc, NamedDecl *FirstQualifierFoundInScope,
    DeclarationNameInfo MemberNameInfo,
    const TemplateArgumentListInfo *TemplateArgs)
    : Expr(CXXDependentScopeMemberExprClass, Ctx.DependentTy, VK_LValue,
           OK_Ordinary),
      Base(Base), BaseType(BaseType), QualifierLoc(QualifierLoc),
      MemberNameInfo(MemberNameInfo) {
  CXXDependentScopeMemberExprBits.IsArrow = IsArrow;
  CXXDependentScopeMemberExprBits.HasTemplateKWAndArgsInfo =
      (TemplateArgs != nullptr) || TemplateKWLoc.isValid();
  CXXDependentScopeMemberExprBits.HasFirstQualifierFoundInScope =
      FirstQualifierFoundInScope != nullptr;
  CXXDependentScopeMemberExprBits.OperatorLoc = OperatorLoc;

  if (TemplateArgs) {
    auto Deps = TemplateArgumentDependence::None;
    getTrailingObjects<ASTTemplateKWAndArgsInfo>()->initializeFrom(
        TemplateKWLoc, *TemplateArgs, getTrailingObjects<TemplateArgumentLoc>(),
        Deps);
  } else if (TemplateKWLoc.isValid()) {
    getTrailingObjects<ASTTemplateKWAndArgsInfo>()->initializeFrom(
        TemplateKWLoc);
  }

  if (hasFirstQualifierFoundInScope())
    *getTrailingObjects<NamedDecl *>() = FirstQualifierFoundInScope;
  setDependence(computeDependence(this));
}

CXXDependentScopeMemberExpr::CXXDependentScopeMemberExpr(
    EmptyShell Empty, bool HasTemplateKWAndArgsInfo,
    bool HasFirstQualifierFoundInScope)
    : Expr(CXXDependentScopeMemberExprClass, Empty) {
  CXXDependentScopeMemberExprBits.HasTemplateKWAndArgsInfo =
      HasTemplateKWAndArgsInfo;
  CXXDependentScopeMemberExprBits.HasFirstQualifierFoundInScope =
      HasFirstQualifierFoundInScope;
}

CXXDependentScopeMemberExpr *CXXDependentScopeMemberExpr::Create(
    const ASTContext &Ctx, Expr *Base, QualType BaseType, bool IsArrow,
    SourceLocation OperatorLoc, NestedNameSpecifierLoc QualifierLoc,
    SourceLocation TemplateKWLoc, NamedDecl *FirstQualifierFoundInScope,
    DeclarationNameInfo MemberNameInfo,
    const TemplateArgumentListInfo *TemplateArgs) {
  bool HasTemplateKWAndArgsInfo =
      (TemplateArgs != nullptr) || TemplateKWLoc.isValid();
  unsigned NumTemplateArgs = TemplateArgs ? TemplateArgs->size() : 0;
  bool HasFirstQualifierFoundInScope = FirstQualifierFoundInScope != nullptr;

  unsigned Size = totalSizeToAlloc<ASTTemplateKWAndArgsInfo,
                                   TemplateArgumentLoc, NamedDecl *>(
      HasTemplateKWAndArgsInfo, NumTemplateArgs, HasFirstQualifierFoundInScope);

  void *Mem = Ctx.Allocate(Size, alignof(CXXDependentScopeMemberExpr));
  return new (Mem) CXXDependentScopeMemberExpr(
      Ctx, Base, BaseType, IsArrow, OperatorLoc, QualifierLoc, TemplateKWLoc,
      FirstQualifierFoundInScope, MemberNameInfo, TemplateArgs);
}

CXXDependentScopeMemberExpr *CXXDependentScopeMemberExpr::CreateEmpty(
    const ASTContext &Ctx, bool HasTemplateKWAndArgsInfo,
    unsigned NumTemplateArgs, bool HasFirstQualifierFoundInScope) {
  assert(NumTemplateArgs == 0 || HasTemplateKWAndArgsInfo);

  unsigned Size = totalSizeToAlloc<ASTTemplateKWAndArgsInfo,
                                   TemplateArgumentLoc, NamedDecl *>(
      HasTemplateKWAndArgsInfo, NumTemplateArgs, HasFirstQualifierFoundInScope);

  void *Mem = Ctx.Allocate(Size, alignof(CXXDependentScopeMemberExpr));
  return new (Mem) CXXDependentScopeMemberExpr(
      EmptyShell(), HasTemplateKWAndArgsInfo, HasFirstQualifierFoundInScope);
}

static bool hasOnlyNonStaticMemberFunctions(UnresolvedSetIterator begin,
                                            UnresolvedSetIterator end) {
  do {
    NamedDecl *decl = *begin;
    if (isa<UnresolvedUsingValueDecl>(decl))
      return false;

    // Unresolved member expressions should only contain methods and
    // method templates.
    if (cast<CXXMethodDecl>(decl->getUnderlyingDecl()->getAsFunction())
            ->isStatic())
      return false;
  } while (++begin != end);

  return true;
}

UnresolvedMemberExpr::UnresolvedMemberExpr(
    const ASTContext &Context, bool HasUnresolvedUsing, Expr *Base,
    QualType BaseType, bool IsArrow, SourceLocation OperatorLoc,
    NestedNameSpecifierLoc QualifierLoc, SourceLocation TemplateKWLoc,
    const DeclarationNameInfo &MemberNameInfo,
    const TemplateArgumentListInfo *TemplateArgs, UnresolvedSetIterator Begin,
    UnresolvedSetIterator End)
    : OverloadExpr(
          UnresolvedMemberExprClass, Context, QualifierLoc, TemplateKWLoc,
          MemberNameInfo, TemplateArgs, Begin, End,
          // Dependent
          ((Base && Base->isTypeDependent()) || BaseType->isDependentType()),
          ((Base && Base->isInstantiationDependent()) ||
           BaseType->isInstantiationDependentType()),
          // Contains unexpanded parameter pack
          ((Base && Base->containsUnexpandedParameterPack()) ||
           BaseType->containsUnexpandedParameterPack())),
      Base(Base), BaseType(BaseType), OperatorLoc(OperatorLoc) {
  UnresolvedMemberExprBits.IsArrow = IsArrow;
  UnresolvedMemberExprBits.HasUnresolvedUsing = HasUnresolvedUsing;

  // Check whether all of the members are non-static member functions,
  // and if so, mark give this bound-member type instead of overload type.
  if (hasOnlyNonStaticMemberFunctions(Begin, End))
    setType(Context.BoundMemberTy);
}

UnresolvedMemberExpr::UnresolvedMemberExpr(EmptyShell Empty,
                                           unsigned NumResults,
                                           bool HasTemplateKWAndArgsInfo)
    : OverloadExpr(UnresolvedMemberExprClass, Empty, NumResults,
                   HasTemplateKWAndArgsInfo) {}

bool UnresolvedMemberExpr::isImplicitAccess() const {
  if (!Base)
    return true;

  return cast<Expr>(Base)->isImplicitCXXThis();
}

UnresolvedMemberExpr *UnresolvedMemberExpr::Create(
    const ASTContext &Context, bool HasUnresolvedUsing, Expr *Base,
    QualType BaseType, bool IsArrow, SourceLocation OperatorLoc,
    NestedNameSpecifierLoc QualifierLoc, SourceLocation TemplateKWLoc,
    const DeclarationNameInfo &MemberNameInfo,
    const TemplateArgumentListInfo *TemplateArgs, UnresolvedSetIterator Begin,
    UnresolvedSetIterator End) {
  unsigned NumResults = End - Begin;
  bool HasTemplateKWAndArgsInfo = TemplateArgs || TemplateKWLoc.isValid();
  unsigned NumTemplateArgs = TemplateArgs ? TemplateArgs->size() : 0;
  unsigned Size = totalSizeToAlloc<DeclAccessPair, ASTTemplateKWAndArgsInfo,
                                   TemplateArgumentLoc>(
      NumResults, HasTemplateKWAndArgsInfo, NumTemplateArgs);
  void *Mem = Context.Allocate(Size, alignof(UnresolvedMemberExpr));
  return new (Mem) UnresolvedMemberExpr(
      Context, HasUnresolvedUsing, Base, BaseType, IsArrow, OperatorLoc,
      QualifierLoc, TemplateKWLoc, MemberNameInfo, TemplateArgs, Begin, End);
}

UnresolvedMemberExpr *UnresolvedMemberExpr::CreateEmpty(
    const ASTContext &Context, unsigned NumResults,
    bool HasTemplateKWAndArgsInfo, unsigned NumTemplateArgs) {
  assert(NumTemplateArgs == 0 || HasTemplateKWAndArgsInfo);
  unsigned Size = totalSizeToAlloc<DeclAccessPair, ASTTemplateKWAndArgsInfo,
                                   TemplateArgumentLoc>(
      NumResults, HasTemplateKWAndArgsInfo, NumTemplateArgs);
  void *Mem = Context.Allocate(Size, alignof(UnresolvedMemberExpr));
  return new (Mem)
      UnresolvedMemberExpr(EmptyShell(), NumResults, HasTemplateKWAndArgsInfo);
}

CXXRecordDecl *UnresolvedMemberExpr::getNamingClass() {
  // Unlike for UnresolvedLookupExpr, it is very easy to re-derive this.

  // If there was a nested name specifier, it names the naming class.
  // It can't be dependent: after all, we were actually able to do the
  // lookup.
  CXXRecordDecl *Record = nullptr;
  auto *NNS = getQualifier();
  if (NNS && NNS->getKind() != NestedNameSpecifier::Super) {
    const Type *T = getQualifier()->getAsType();
    assert(T && "qualifier in member expression does not name type");
    Record = T->getAsCXXRecordDecl();
    assert(Record && "qualifier in member expression does not name record");
  }
  // Otherwise the naming class must have been the base class.
  else {
    QualType BaseType = getBaseType().getNonReferenceType();
    if (isArrow())
      BaseType = BaseType->castAs<PointerType>()->getPointeeType();

    Record = BaseType->getAsCXXRecordDecl();
    assert(Record && "base of member expression does not name record");
  }

  return Record;
}

SizeOfPackExpr *
SizeOfPackExpr::Create(ASTContext &Context, SourceLocation OperatorLoc,
                       NamedDecl *Pack, SourceLocation PackLoc,
                       SourceLocation RParenLoc,
                       Optional<unsigned> Length,
                       ArrayRef<TemplateArgument> PartialArgs) {
  void *Storage =
      Context.Allocate(totalSizeToAlloc<TemplateArgument>(PartialArgs.size()));
  return new (Storage) SizeOfPackExpr(Context.getSizeType(), OperatorLoc, Pack,
                                      PackLoc, RParenLoc, Length, PartialArgs);
}

SizeOfPackExpr *SizeOfPackExpr::CreateDeserialized(ASTContext &Context,
                                                   unsigned NumPartialArgs) {
  void *Storage =
      Context.Allocate(totalSizeToAlloc<TemplateArgument>(NumPartialArgs));
  return new (Storage) SizeOfPackExpr(EmptyShell(), NumPartialArgs);
}

QualType SubstNonTypeTemplateParmExpr::getParameterType(
    const ASTContext &Context) const {
  // Note that, for a class type NTTP, we will have an lvalue of type 'const
  // T', so we can't just compute this from the type and value category.
  if (isReferenceParameter())
    return Context.getLValueReferenceType(getType());
  return getType().getUnqualifiedType();
}

SubstNonTypeTemplateParmPackExpr::SubstNonTypeTemplateParmPackExpr(
    QualType T, ExprValueKind ValueKind, NonTypeTemplateParmDecl *Param,
    SourceLocation NameLoc, const TemplateArgument &ArgPack)
    : Expr(SubstNonTypeTemplateParmPackExprClass, T, ValueKind, OK_Ordinary),
      Param(Param), Arguments(ArgPack.pack_begin()),
      NumArguments(ArgPack.pack_size()), NameLoc(NameLoc) {
  setDependence(ExprDependence::TypeValueInstantiation |
                ExprDependence::UnexpandedPack);
}

TemplateArgument SubstNonTypeTemplateParmPackExpr::getArgumentPack() const {
  return TemplateArgument(llvm::makeArrayRef(Arguments, NumArguments));
}

FunctionParmPackExpr::FunctionParmPackExpr(QualType T, VarDecl *ParamPack,
                                           SourceLocation NameLoc,
                                           unsigned NumParams,
                                           VarDecl *const *Params)
    : Expr(FunctionParmPackExprClass, T, VK_LValue, OK_Ordinary),
      ParamPack(ParamPack), NameLoc(NameLoc), NumParameters(NumParams) {
  if (Params)
    std::uninitialized_copy(Params, Params + NumParams,
                            getTrailingObjects<VarDecl *>());
  setDependence(ExprDependence::TypeValueInstantiation |
                ExprDependence::UnexpandedPack);
}

FunctionParmPackExpr *
FunctionParmPackExpr::Create(const ASTContext &Context, QualType T,
                             VarDecl *ParamPack, SourceLocation NameLoc,
                             ArrayRef<VarDecl *> Params) {
  return new (Context.Allocate(totalSizeToAlloc<VarDecl *>(Params.size())))
      FunctionParmPackExpr(T, ParamPack, NameLoc, Params.size(), Params.data());
}

FunctionParmPackExpr *
FunctionParmPackExpr::CreateEmpty(const ASTContext &Context,
                                  unsigned NumParams) {
  return new (Context.Allocate(totalSizeToAlloc<VarDecl *>(NumParams)))
      FunctionParmPackExpr(QualType(), nullptr, SourceLocation(), 0, nullptr);
}

MaterializeTemporaryExpr::MaterializeTemporaryExpr(
    QualType T, Expr *Temporary, bool BoundToLvalueReference,
    LifetimeExtendedTemporaryDecl *MTD)
    : Expr(MaterializeTemporaryExprClass, T,
           BoundToLvalueReference ? VK_LValue : VK_XValue, OK_Ordinary) {
  if (MTD) {
    State = MTD;
    MTD->ExprWithTemporary = Temporary;
    return;
  }
  State = Temporary;
  setDependence(computeDependence(this));
}

void MaterializeTemporaryExpr::setExtendingDecl(ValueDecl *ExtendedBy,
                                                unsigned ManglingNumber) {
  // We only need extra state if we have to remember more than just the Stmt.
  if (!ExtendedBy)
    return;

  // We may need to allocate extra storage for the mangling number and the
  // extended-by ValueDecl.
  if (!State.is<LifetimeExtendedTemporaryDecl *>())
    State = LifetimeExtendedTemporaryDecl::Create(
        cast<Expr>(State.get<Stmt *>()), ExtendedBy, ManglingNumber);

  auto ES = State.get<LifetimeExtendedTemporaryDecl *>();
  ES->ExtendingDecl = ExtendedBy;
  ES->ManglingNumber = ManglingNumber;
}

bool MaterializeTemporaryExpr::isUsableInConstantExpressions(
    const ASTContext &Context) const {
  // C++20 [expr.const]p4:
  //   An object or reference is usable in constant expressions if it is [...]
  //   a temporary object of non-volatile const-qualified literal type
  //   whose lifetime is extended to that of a variable that is usable
  //   in constant expressions
  auto *VD = dyn_cast_or_null<VarDecl>(getExtendingDecl());
  return VD && getType().isConstant(Context) &&
         !getType().isVolatileQualified() &&
         getType()->isLiteralType(Context) &&
         VD->isUsableInConstantExpressions(Context);
}

TypeTraitExpr::TypeTraitExpr(QualType T, SourceLocation Loc, TypeTrait Kind,
                             ArrayRef<TypeSourceInfo *> Args,
                             SourceLocation RParenLoc, bool Value)
    : Expr(TypeTraitExprClass, T, VK_PRValue, OK_Ordinary), Loc(Loc),
      RParenLoc(RParenLoc) {
  assert(Kind <= TT_Last && "invalid enum value!");
  TypeTraitExprBits.Kind = Kind;
  assert(static_cast<unsigned>(Kind) == TypeTraitExprBits.Kind &&
         "TypeTraitExprBits.Kind overflow!");
  TypeTraitExprBits.Value = Value;
  TypeTraitExprBits.NumArgs = Args.size();
  assert(Args.size() == TypeTraitExprBits.NumArgs &&
         "TypeTraitExprBits.NumArgs overflow!");

  auto **ToArgs = getTrailingObjects<TypeSourceInfo *>();
  for (unsigned I = 0, N = Args.size(); I != N; ++I)
    ToArgs[I] = Args[I];

  setDependence(computeDependence(this));
}

TypeTraitExpr *TypeTraitExpr::Create(const ASTContext &C, QualType T,
                                     SourceLocation Loc,
                                     TypeTrait Kind,
                                     ArrayRef<TypeSourceInfo *> Args,
                                     SourceLocation RParenLoc,
                                     bool Value) {
  void *Mem = C.Allocate(totalSizeToAlloc<TypeSourceInfo *>(Args.size()));
  return new (Mem) TypeTraitExpr(T, Loc, Kind, Args, RParenLoc, Value);
}

TypeTraitExpr *TypeTraitExpr::CreateDeserialized(const ASTContext &C,
                                                 unsigned NumArgs) {
  void *Mem = C.Allocate(totalSizeToAlloc<TypeSourceInfo *>(NumArgs));
  return new (Mem) TypeTraitExpr(EmptyShell());
}

// [reflection-ts]
ReflexprIdExpr::ReflexprIdExpr(QualType resultType,
                               SourceLocation OperatorLoc,
                               SourceLocation RParenLoc)
    : Expr(ReflexprIdExprClass, resultType, VK_PRValue, OK_Ordinary),
      OperatorLoc(OperatorLoc),
      RParenLoc(RParenLoc) {

  setKind(MOK_Nothing);
  setSeqKind(MOSK_None);
  setArgKind(REAK_Nothing);
  Argument.Nothing = nullptr;
  setAccessibility(MOA_AllowPrivate);
  setRemoveSugar(false);
}

ReflexprIdExpr::ReflexprIdExpr(QualType resultType, MetaobjectKind kind,
                               SourceLocation OperatorLoc,
                               SourceLocation RParenLoc)
    : Expr(ReflexprIdExprClass, resultType, VK_PRValue, OK_Ordinary),
      OperatorLoc(OperatorLoc),
      RParenLoc(RParenLoc) {

  setKind(kind);
  setSeqKind(MOSK_None);
  setArgKind(REAK_Nothing);
  Argument.Nothing = nullptr;
  setAccessibility(MOA_AllowPrivate);
  setRemoveSugar(false);
  setDependence(computeDependence(this));
}

ReflexprIdExpr::ReflexprIdExpr(QualType resultType, tok::TokenKind specTok,
                               SourceLocation OperatorLoc,
                               SourceLocation RParenLoc)
    : Expr(ReflexprIdExprClass, resultType, VK_PRValue, OK_Ordinary),
      OperatorLoc(OperatorLoc),
      RParenLoc(RParenLoc) {

  setKind(MOK_Specifier);
  setSeqKind(MOSK_None);
  setArgKind(REAK_Specifier);
  Argument.SpecTok = specTok;
  setAccessibility(MOA_AllowPrivate);
  setRemoveSugar(false);
  setDependence(computeDependence(this));
}

ReflexprIdExpr::ReflexprIdExpr(QualType resultType, const NamedDecl *nDecl,
                               SourceLocation OperatorLoc,
                               SourceLocation RParenLoc)
    : Expr(ReflexprIdExprClass, resultType, VK_PRValue, OK_Ordinary),
      OperatorLoc(OperatorLoc),
      RParenLoc(RParenLoc) {

  if (isa<NamespaceAliasDecl>(nDecl)) {
    setKind(MOK_NamespaceAlias);
  } else if (isa<NamespaceDecl>(nDecl)) {
    setKind(MOK_NamespaceScope);
  } else if (isa<EnumDecl>(nDecl)) {
    if (nDecl->isCXXClassMember()) {
      setKind(MOK_MemberEnum);
    } else {
      setKind(MOK_Enum);
    }
  } else if (const auto *RD = dyn_cast<CXXRecordDecl>(nDecl)) {
    if (nDecl->isCXXClassMember()) {
      if (RD->isLambda()) {
        setKind(MOK_MemberLambda);
      } else if (RD->isUnion()) {
        setKind(MOK_MemberRecord);
      } else {
        setKind(MOK_MemberClass);
      }
    } else {
      if (RD->isLambda()) {
        setKind(MOK_Lambda);
      } else if (RD->isUnion()) {
        setKind(MOK_Record);
      } else {
        setKind(MOK_Class);
      }
    }
  } else if (isa<RecordDecl>(nDecl)) {
    if (nDecl->isCXXClassMember()) {
      setKind(MOK_MemberRecord);
    } else {
      setKind(MOK_Record);
    }
  } else if (const auto *TND = dyn_cast<TypedefNameDecl>(nDecl)) {
    const Type *UT = TND->getUnderlyingType()->getUnqualifiedDesugaredType();
    const TagDecl *TD = nullptr;
    if (const auto *UTT = dyn_cast<TagType>(UT)) {
      TD = UTT->getDecl();
    }
    if (nDecl->isCXXClassMember()) {
      if (TD && TD->isUnion()) {
        setKind(MOK_MemberRecordAlias);
      } else if (TD && (TD->isClass() || TD->isStruct())) {
        setKind(MOK_MemberClassAlias);
      } else if (TD && TD->isEnum()) {
        setKind(MOK_MemberEnumAlias);
      } else {
        if (TD && isa<ClassTemplateSpecializationDecl>(TD)) {
          setKind(MOK_MemberClassAlias);
        } else {
          setKind(MOK_MemberTypeAlias);
        }
      }
    } else {
      if (TD && TD->isUnion()) {
        setKind(MOK_RecordAlias);
      } else if (TD && (TD->isClass() || TD->isStruct())) {
        setKind(MOK_ClassAlias);
      } else if (TD && TD->isEnum()) {
        setKind(MOK_EnumAlias);
      } else {
        if (TD && isa<ClassTemplateSpecializationDecl>(TD)) {
          setKind(MOK_ClassAlias);
        } else {
          setKind(MOK_TypeAlias);
        }
      }
    }
  } else if (const auto *TTPD = dyn_cast<TemplateTypeParmDecl>(nDecl)) {
    if (TTPD->getTypeForDecl()->isEnumeralType()) {
      setKind(MOK_TemplateEnumTypeParameter);
    } else {
      setKind(MOK_TemplateTypeParameter);
    }
  } else if (const auto *FD = dyn_cast<FunctionDecl>(nDecl)) {
    if (const auto *CCD = dyn_cast<CXXConstructorDecl>(FD)) {
      if (CCD->isDefaultConstructor() ||
          CCD->isCopyOrMoveConstructor()) 
        setKind(MOK_SpecialConstructor);
      else
        setKind(MOK_Constructor);
    } else if (isa<CXXDestructorDecl>(FD)) {
      setKind(MOK_Destructor);
    } else if (isa<CXXConversionDecl>(FD)) {
      setKind(MOK_ConversionOperator);
    } else if (const auto *CMF = dyn_cast<CXXMethodDecl>(FD)) {
      if (CMF->isMoveAssignmentOperator() ||
          CMF->isCopyAssignmentOperator() ||
          CMF->isOverloadedOperator()) {
        setKind(MOK_MemberOperator);
      } else
        setKind(MOK_MemberFunction);
    } else {
      if (FD->isOverloadedOperator())
        setKind(MOK_Operator);
      else
        setKind(MOK_Function);
    }
  } else if (isa<TypeDecl>(nDecl)) {
    setKind(MOK_ScopedType);
  } else if (isa<FieldDecl>(nDecl)) {
    setKind(MOK_DataMember);
  } else if (const auto *VD = dyn_cast<VarDecl>(nDecl)) {
    if (isa<ParmVarDecl>(VD)) {
      setKind(MOK_FunctionParameter);
    } else if (VD->isStaticDataMember()) {
      setKind(MOK_DataMember);
    } else {
      setKind(MOK_Variable);
    }
  } else if (isa<EnumConstantDecl>(nDecl)) {
    setKind(MOK_Enumerator);
  } else if (isa<CXXMethodDecl>(nDecl)) {
    setKind(MOK_MemberFunction);
  } else if (isa<FunctionDecl>(nDecl)) {
    setKind(MOK_NamedFunction);
  } else {
    setKind(MOK_Object);
  }
  setSeqKind(MOSK_None);
  setArgKind(REAK_NamedDecl);
  Argument.ReflDecl = nDecl;
  setAccessibility(MOA_AllowPrivate);
  setRemoveSugar(false);
  setDependence(computeDependence(this));
}

ReflexprIdExpr::ReflexprIdExpr(QualType resultType, const TypeSourceInfo *TInfo,
                               bool removeSugar,
                               SourceLocation OperatorLoc,
                               SourceLocation RParenLoc)
    : Expr(ReflexprIdExprClass, resultType, VK_PRValue, OK_Ordinary),
      OperatorLoc(OperatorLoc),
      RParenLoc(RParenLoc) {

  const Type *RT = TInfo->getType().getTypePtr();

  bool isAlias = false;
  if (const auto *STTPT = dyn_cast<SubstTemplateTypeParmType>(RT)) {
    isAlias = true;
    RT = STTPT->getReplacementType().getTypePtr();
  } else if (isa<TypedefType>(RT)) {
    isAlias = true;
  }
  isAlias &= !removeSugar;

  RT = RT->getUnqualifiedDesugaredType();

  if (const auto *TTPT = dyn_cast<TemplateTypeParmType>(RT)) {
    if (TTPT->isEnumeralType()) {
      setKind(MOK_TemplateEnumTypeParameter);
    } else {
      setKind(MOK_TemplateTypeParameter);
    }
  } else if (isa<RecordType>(RT)) {
    setKind(isAlias ? MOK_ClassAlias : MOK_Class);
  } else if (isa<EnumType>(RT)) {
    setKind(isAlias ? MOK_EnumAlias : MOK_Enum);
  } else {
    if (isAlias) {
      setKind(MOK_TypeAlias);
    } else {
      const bool isNotScoped =
        RT->isPointerType() ||
        RT->isReferenceType() ||
        RT->isFunctionType();
      if (isNotScoped)
        setKind(MOK_Type);
      else
        setKind(MOK_ScopedType);
    }
  }
  Argument.TypeInfo = TInfo;
  setSeqKind(MOSK_None);
  setArgKind(REAK_TypeInfo);
  setAccessibility(MOA_AllowPrivate);
  setRemoveSugar(removeSugar);
  setDependence(computeDependence(this));
}

ReflexprIdExpr::ReflexprIdExpr(QualType resultType,
                               const CXXBaseSpecifier *baseSpec,
                               SourceLocation OperatorLoc,
                               SourceLocation RParenLoc)
    : Expr(ReflexprIdExprClass, resultType, VK_PRValue, OK_Ordinary),
      OperatorLoc(OperatorLoc),
      RParenLoc(RParenLoc) {

  setKind(MOK_Base);
  setSeqKind(MOSK_None);
  setArgKind(REAK_BaseSpecifier);
  Argument.BaseSpec = baseSpec;
  setAccessibility(MOA_AllowPrivate);
  setRemoveSugar(false);
  setDependence(computeDependence(this));
}

ReflexprIdExpr::ReflexprIdExpr(QualType resultType,
                               const LambdaCapture *capture,
                               SourceLocation OperatorLoc,
                               SourceLocation RParenLoc)
    : Expr(ReflexprIdExprClass, resultType, VK_PRValue, OK_Ordinary),
      OperatorLoc(OperatorLoc),
      RParenLoc(RParenLoc) {

  setKind(MOK_LambdaCapture);
  setSeqKind(MOSK_None);
  setArgKind(REAK_Capture);
  Argument.Capture = capture;
  setAccessibility(MOA_AllowPrivate);
  setRemoveSugar(false);
  setDependence(computeDependence(this));
}

ReflexprIdExpr::ReflexprIdExpr(const ReflexprIdExpr &that)
    : Expr(ReflexprIdExprClass, that.getType(), VK_PRValue, OK_Ordinary),
      OperatorLoc(that.getOperatorLoc()),
      RParenLoc(that.getRParenLoc()) {

  setKind(that.getKind());
  setSeqKind(that.getSeqKind());
  setArgKind(that.getArgKind());
  switch (getArgKind()) {
  case REAK_Nothing:
    Argument.Nothing = that.Argument.Nothing;
    break;
  case REAK_Specifier:
    Argument.SpecTok = that.Argument.SpecTok;
    break;
  case REAK_NamedDecl:
    Argument.ReflDecl = that.Argument.ReflDecl;
    break;
  case REAK_TypeInfo:
    Argument.TypeInfo = that.Argument.TypeInfo;
    break;
  case REAK_BaseSpecifier:
    Argument.BaseSpec = that.Argument.BaseSpec;
    break;
  case REAK_Capture:
    Argument.Capture = that.Argument.Capture;
    break;
  }
  setAccessibility(that.getAccessibility());
  setRemoveSugar(that.getRemoveSugar());
}

ReflexprIdExpr*
ReflexprIdExpr::getEmptyReflexprIdExpr(ASTContext &Ctx,
                                       SourceLocation opLoc,
                                       SourceLocation endLoc) {
  if (ReflexprIdExpr *E = Ctx.findEmptyReflexpr())
    return E;
  return Ctx.cacheEmptyReflexpr(new (Ctx) ReflexprIdExpr(
        Ctx.MetaobjectIdTy, opLoc, endLoc));
}

ReflexprIdExpr*
ReflexprIdExpr::getGlobalScopeReflexprIdExpr(ASTContext &Ctx,
                                             SourceLocation opLoc,
                                             SourceLocation endLoc) {
  if (ReflexprIdExpr *E = Ctx.findGlobalScopeReflexpr())
    return E;
  return Ctx.cacheGlobalScopeReflexpr(new (Ctx) ReflexprIdExpr(
      Ctx.MetaobjectIdTy, MOK_GlobalScope, opLoc, endLoc));
}

ReflexprIdExpr*
ReflexprIdExpr::getNoSpecifierReflexprIdExpr(ASTContext &Ctx,
                                             SourceLocation opLoc,
                                             SourceLocation endLoc) {
  if (ReflexprIdExpr *E = Ctx.findNoSpecifierReflexpr())
    return E;
  return Ctx.cacheNoSpecifierReflexpr(new (Ctx) ReflexprIdExpr(
      Ctx.MetaobjectIdTy, MOK_Specifier, opLoc, endLoc));
}

ReflexprIdExpr*
ReflexprIdExpr::getSpecifierReflexprIdExpr(ASTContext &Ctx,
                                           tok::TokenKind specTok,
                                           SourceLocation opLoc,
                                           SourceLocation endLoc) {
  if (ReflexprIdExpr *E = Ctx.findSpecifierReflexpr(specTok))
    return E;
  return Ctx.cacheSpecifierReflexpr(
      specTok, new (Ctx) ReflexprIdExpr(Ctx.MetaobjectIdTy,
                                        specTok, opLoc, endLoc));
}

ReflexprIdExpr*
ReflexprIdExpr::getNamedDeclReflexprIdExpr(ASTContext &Ctx,
                                           const NamedDecl *nDecl,
                                           SourceLocation opLoc,
                                           SourceLocation endLoc) {
  if (ReflexprIdExpr *E = Ctx.findNamedDeclReflexpr(nDecl))
    return E;
  return Ctx.cacheNamedDeclReflexpr(
      nDecl, new (Ctx) ReflexprIdExpr(Ctx.MetaobjectIdTy, nDecl,
                                      opLoc, endLoc));
}

ReflexprIdExpr*
ReflexprIdExpr::getTypeReflexprIdExpr(ASTContext &Ctx,
                                      const TypeSourceInfo *TInfo,
                                      bool removeSugar,
                                      SourceLocation opLoc,
                                      SourceLocation endLoc) {
  // [reflection-ts] FIXME cache?
  return new (Ctx) ReflexprIdExpr(Ctx.MetaobjectIdTy, TInfo,
                                  removeSugar, opLoc, endLoc);
}

ReflexprIdExpr*
ReflexprIdExpr::getTypeReflexprIdExpr(ASTContext &Ctx,
                                      QualType Ty, bool removeSugar,
                                      SourceLocation opLoc,
                                      SourceLocation endLoc) {
  // [reflection-ts] FIXME cache?
  return new (Ctx) ReflexprIdExpr(Ctx.MetaobjectIdTy,
                                  Ctx.getTrivialTypeSourceInfo(Ty),
                                  removeSugar, opLoc, endLoc);
}

ReflexprIdExpr*
ReflexprIdExpr::getBaseSpecifierReflexprIdExpr(ASTContext &Ctx,
                                               const CXXBaseSpecifier *baseSpec,
                                               SourceLocation opLoc,
                                               SourceLocation endLoc) {
  // [reflection-ts] FIXME cache?
  return new (Ctx)
      ReflexprIdExpr(Ctx.MetaobjectIdTy, baseSpec, opLoc, endLoc);
}

ReflexprIdExpr*
ReflexprIdExpr::getLambdaCaptureReflexprIdExpr(ASTContext &Ctx,
                                               const LambdaCapture *capture,
                                               SourceLocation opLoc,
                                               SourceLocation endLoc) {
  // [reflection-ts] FIXME cache?
  return new (Ctx)
      ReflexprIdExpr(Ctx.MetaobjectIdTy, capture, opLoc, endLoc);
}

ReflexprIdExpr *ReflexprIdExpr::getSeqReflexprIdExpr(ASTContext &Ctx,
                                                     ReflexprIdExpr *that,
                                                     MetaobjectSequenceKind MoSK) {
  assert(that != nullptr);
  // [reflection-ts] FIXME cache?
  ReflexprIdExpr *Res = new (Ctx) ReflexprIdExpr(*that);
  Res->setKind(MOK_ObjectSequence);
  Res->setSeqKind(MoSK);
  return Res;
}

ReflexprIdExpr*
ReflexprIdExpr::getHideProtectedReflexprIdExpr(ASTContext &Ctx,
                                               ReflexprIdExpr *that) {
  assert(that != nullptr);
  // [reflection-ts] FIXME cache in ASTContext when possible
  ReflexprIdExpr *Res = new (Ctx) ReflexprIdExpr(*that);
  Res->setAccessibility(MOA_OnlyPublic);
  return Res;
}

ReflexprIdExpr*
ReflexprIdExpr::getHidePrivateReflexprIdExpr(ASTContext &Ctx,
                                             ReflexprIdExpr *that) {
  assert(that != nullptr);
  // [reflection-ts] FIXME cache in ASTContext when possible
  ReflexprIdExpr *Res = new (Ctx) ReflexprIdExpr(*that);
  Res->setAccessibility(MOA_AllowProtected);
  return Res;
}

ReflexprIdExpr*
ReflexprIdExpr::fromMetaobjectId(ASTContext &Ctx,
                                 const llvm::APInt& MoId) {
  return Ctx.decodeMetaobject(MoId);
}

llvm::APInt
ReflexprIdExpr::toMetaobjectId(ASTContext &Ctx,
                               const ReflexprIdExpr *that) {
  return Ctx.encodeMetaobject(that);
}

ReflexprIdExpr *ReflexprIdExpr::fromExpr(ASTContext &Ctx, Expr *E,
                                         void *EvlInfo) {
  if (ReflexprIdExpr *REE = dyn_cast<ReflexprIdExpr>(E)) {
    return REE;
  }

  if (MetaobjectIdExpr *MIE = dyn_cast<MetaobjectIdExpr>(E)) {
    return MIE->asReflexprIdExpr(Ctx);
  }

  if (const auto MoId = E->getMetaobjectIdExpr(EvlInfo, Ctx)) {
    return fromMetaobjectId(Ctx, *MoId);
  }

  return nullptr;
}

StringRef ReflexprIdExpr::getMetaobjectKindName(MetaobjectKind MoK) {
  switch (MoK) {
  case MOK_Specifier:
    return "a specifier";
  case MOK_Base:
    return "a base specifier";
  case MOK_GlobalScope:
    return "the global scope";
  case MOK_NamespaceScope:
    return "a namespace";
  case MOK_NamespaceAlias:
    return "a namespace alias";
  case MOK_TemplateParameterScope:
    return "a template parameter scope";
  case MOK_Type:
  case MOK_ScopedType:
    return "a type";
  case MOK_Enum:
    return "an enum";
  case MOK_Record:
    return "a record";
  case MOK_Class:
    return "a class";
  case MOK_Lambda:
    return "a lambda";
  case MOK_Function:
  case MOK_NamedFunction:
    return "a function";
  case MOK_Constructor:
  case MOK_SpecialConstructor:
    return "a constructor";
  case MOK_Destructor:
    return "a destructor";
  case MOK_Operator:
  case MOK_MemberOperator:
    return "an operator";
  case MOK_ConversionOperator:
    return "a conversion operator";
  case MOK_TypeAlias:
    return "a type alias";
  case MOK_EnumAlias:
    return "a enum alias";
  case MOK_RecordAlias:
    return "a record alias";
  case MOK_LambdaAlias:
    return "a lambda alias";
  case MOK_ClassAlias:
    return "a class alias";
  case MOK_TemplateClass:
    return "a template class";
  case MOK_TemplateTypeParameter:
  case MOK_TemplateEnumTypeParameter:
    return "a template type parameter";
  case MOK_Variable:
    return "a variable";
  case MOK_LambdaCapture:
    return "a lambda capture";
  case MOK_FunctionParameter:
    return "a function parameter";
  case MOK_DataMember:
    return "a data member";
  case MOK_MemberType:
    return "a member type";
  case MOK_MemberTypeAlias:
    return "a member type alias";
  case MOK_MemberRecord:
    return "a member record";
  case MOK_MemberRecordAlias:
    return "a member record alias";
  case MOK_MemberLambda:
    return "a member record";
  case MOK_MemberLambdaAlias:
    return "a member lambda alias";
  case MOK_MemberClass:
    return "a member class";
  case MOK_MemberClassAlias:
    return "a member class alias";
  case MOK_MemberEnum:
    return "a member enum";
  case MOK_MemberEnumAlias:
    return "a member enum alias";
  case MOK_MemberFunction:
    return "a member function";
  case MOK_Enumerator:
    return "a enumerator";
  case MOK_ObjectSequence:
    return "a metaobject sequence";
  case MOK_Object:
  case MOK_Nothing:
    break;
  }
  return StringRef();
}

static MetaobjectConcept
translateMetaobjectKindToMetaobjectConcept(MetaobjectKind MoK) {
  // [reflection-ts] FIXME this needs to be synced with the TS
  switch (MoK) {
  case MOK_Specifier:
    return MOC_Specifier;
  case MOK_Base:
    return MOC_Base;
  case MOK_GlobalScope:
    return MOC_GlobalScope;
  case MOK_NamespaceScope:
    return MOC_NamespaceScope;
  case MOK_NamespaceAlias:
    return MOC_NamespaceAlias;
  case MOK_TemplateParameterScope:
    return MOC_TemplateParameterScope;
  case MOK_Type:
    return MOC_Type;
  case MOK_Enum:
    return MOC_Enum;
  case MOK_Record:
    return MOC_Record;
  case MOK_Class:
    return MOC_Class;
  case MOK_Lambda:
    return MOC_Lambda;
  case MOK_Function:
    return MOC_Function;
  case MOK_Constructor:
    return MOC_Constructor;
  case MOK_SpecialConstructor:
    return MOC_SpecialConstructor;
  case MOK_Destructor:
    return MOC_Destructor;
  case MOK_Operator:
    return MOC_Operator;
  case MOK_MemberOperator:
    return MOC_MemberOperator;
  case MOK_ConversionOperator:
    return MOC_ConversionOperator;
  case MOK_TypeAlias:
    return MOC_TypeAlias;
  case MOK_EnumAlias:
    return MOC_EnumAlias;
  case MOK_RecordAlias:
    return MOC_RecordAlias;
  case MOK_LambdaAlias:
    return MOC_LambdaAlias;
  case MOK_ClassAlias:
    return MOC_ClassAlias;
  case MOK_TemplateClass:
    return MOC_TemplateClass;
  case MOK_TemplateTypeParameter:
    return MOC_TemplateTypeParameter;
  case MOK_TemplateEnumTypeParameter:
    return MOC_TemplateEnumTypeParameter;
  case MOK_Variable:
    return MOC_Variable;
  case MOK_LambdaCapture:
    return MOC_LambdaCapture;
  case MOK_FunctionParameter:
    return MOC_FunctionParameter;
  case MOK_NamedFunction:
    return MOC_NamedFunction;
  case MOK_ScopedType:
    return MOC_ScopedType;
  case MOK_DataMember:
    return MOC_DataMember;
  case MOK_MemberType:
    return MOC_MemberType;
  case MOK_MemberTypeAlias:
    return MOC_MemberTypeAlias;
  case MOK_MemberRecord:
    return MOC_MemberRecord;
  case MOK_MemberRecordAlias:
    return MOC_MemberRecordAlias;
  case MOK_MemberLambda:
    return MOC_MemberLambda;
  case MOK_MemberLambdaAlias:
    return MOC_MemberLambdaAlias;
  case MOK_MemberClass:
    return MOC_MemberClass;
  case MOK_MemberClassAlias:
    return MOC_MemberClassAlias;
  case MOK_MemberEnum:
    return MOC_MemberEnum;
  case MOK_MemberEnumAlias:
    return MOC_MemberEnumAlias;
  case MOK_MemberFunction:
    return MOC_MemberFunction;
  case MOK_Enumerator:
    return MOC_Enumerator;
  case MOK_ObjectSequence:
    return MOC_ObjectSequence;
  case MOK_Object:
    return MOC_Object;
  case MOK_Nothing:
    return MOC_Nothing;
  }
  llvm_unreachable("Metaobject kind not implemented!");
}

MetaobjectConcept ReflexprIdExpr::getCategory() const {
  return translateMetaobjectKindToMetaobjectConcept(getKind());
}

const NamedDecl *ReflexprIdExpr::findTypeDecl(QualType Ty) {
  if (const auto *TdT = dyn_cast<TypedefType>(Ty)) {
    return TdT->getDecl();
  } else if (const auto *TgT = dyn_cast<TagType>(Ty)) {
    return TgT->getDecl();
  } else if (const auto *TST = dyn_cast<TemplateSpecializationType>(Ty)) {
    return TST->getTemplateName().getAsTemplateDecl();
  } else if (const auto *STTPT = dyn_cast<SubstTemplateTypeParmType>(Ty)) {
    return STTPT->getReplacedParameter()->getDecl();
  } else if (const auto *TTPT = dyn_cast<TemplateTypeParmType>(Ty)) {
    return TTPT->getDecl();
  }
  return nullptr;
}

QualType ReflexprIdExpr::getBaseArgumentType(ASTContext &,
                                             bool removeSugar) const {

  QualType Res = getArgumentType();
  removeSugar |= isa<DecltypeType>(Res);

  if (removeSugar)
    Res = Res.getCanonicalType();

  if (const auto *ET = dyn_cast<ElaboratedType>(Res)) {
    Res = ET->getNamedType();
  }

  while (true) {
    if (const auto *PT = dyn_cast<PointerType>(Res)) {
      Res = PT->getPointeeType();
    } else if (const auto *RT = dyn_cast<ReferenceType>(Res)) {
      Res = RT->getPointeeType();
    } else if (const auto *AT = dyn_cast<ArrayType>(Res)) {
      Res = AT->getElementType();
    } else {
      break;
    }
  }

  return Res;
}


const NamedDecl *ReflexprIdExpr::findArgumentNamedDecl(ASTContext &Ctx,
                                                       bool removeSugar) const {
  if (isArgumentNamedDecl())
    return getArgumentNamedDecl();
  if (isArgumentLambdaCapture())
    return getArgumentLambdaCapture()->getCapturedVar();
  if (isArgumentType())
    return findTypeDecl(getBaseArgumentType(Ctx, removeSugar));
  return nullptr;
}

const ValueDecl *ReflexprIdExpr::findArgumentValueDecl(ASTContext &Ctx) const {
  return dyn_cast<ValueDecl>(findArgumentNamedDecl(Ctx, false));
}

const LambdaCapture *ReflexprIdExpr::findArgumentLambdaCapture(ASTContext &) const {
  if (isArgumentLambdaCapture())
    return getArgumentLambdaCapture();
  return nullptr;
}

bool ReflexprIdExpr::reflectsType() const {
  if (isArgumentType())
    return true;

  if (isArgumentNamedDecl())
    return isa<TypeDecl>(getArgumentNamedDecl());

  return false;
}

QualType ReflexprIdExpr::getReflectedType() const {
  if (isArgumentType())
    return getArgumentType();

  if (isArgumentNamedDecl()) {
    if (const auto *TDND = dyn_cast<TypedefNameDecl>(getArgumentNamedDecl())) {
      return TDND->getUnderlyingType();
    } else if (const auto *TD = dyn_cast<TypeDecl>(getArgumentNamedDecl())) {
      return QualType(TD->getTypeForDecl(), 0);
    }
  }

  return QualType();
}

bool ReflexprIdExpr::isArgumentDependent() const {
  if (isArgumentNamedDecl()) {
    const NamedDecl *ND = getArgumentNamedDecl();
    return isa<TemplateTypeParmDecl>(ND);
  }
  return false;
}

AccessSpecifier ReflexprIdExpr::getArgumentAccess(ASTContext &Ctx) const {
  if (isArgumentBaseSpecifier()) {
    return getArgumentBaseSpecifier()->getAccessSpecifier();
  }

  if (const NamedDecl *ND = findArgumentNamedDecl(Ctx, true)) {
    return ND->getAccess();
  }

  return AS_none;
}

Stmt::child_range ReflexprIdExpr::children() {
  // [reflection-ts] FIXME
  return child_range(child_iterator(), child_iterator());
}

// MetaobjectIdExpr
MetaobjectIdExpr::MetaobjectIdExpr(const llvm::APInt& V, QualType Ty, SourceLocation L)
    : Expr(MetaobjectIdExprClass, Ty, VK_PRValue, OK_Ordinary),
      Value(V.getZExtValue()), Loc(L) {
  setDependence(computeDependence(this));
}

llvm::APInt MetaobjectIdExpr::getValue() const {
  return llvm::APInt(sizeof(uintptr_t) * 8, Value);
}
void MetaobjectIdExpr::setValue(const llvm::APInt& V) {
  Value = V.getZExtValue();
}

ReflexprIdExpr *MetaobjectIdExpr::asReflexprIdExpr(ASTContext &Context) const {
  return ReflexprIdExpr::fromMetaobjectId(Context, getValue());
}

Stmt::child_range MetaobjectIdExpr::children() {
  // [reflection-ts] FIXME
  return child_range(child_iterator(),
                     child_iterator());
}

// MetaobjectOpExprBase
AccessSpecifier MetaobjectOpExprBase::getArgumentAccess(ASTContext &Ctx,
                                                        ReflexprIdExpr *REE) {
  return REE->getArgumentAccess(Ctx);
}

DeclRefExpr *MetaobjectOpExprBase::buildDeclRefExpr(ASTContext &Ctx,
                                                    const ValueDecl* VD,
                                                    ExprValueKind VK,
                                                    SourceLocation Loc) {

  return DeclRefExpr::Create(
      Ctx, NestedNameSpecifierLoc(), Loc, const_cast<ValueDecl *>(VD),
      false /*EnclVarOrCapture?*/, Loc, VD->getType(),
      VK, const_cast<NamedDecl *>(dyn_cast<NamedDecl>(VD)));
}

bool MetaobjectOpExprBase::queryExprUIntValue(ASTContext &Ctx, uint64_t &value,
                                              Expr *E) {
  if (const auto apsi = E->getIntegerConstantExpr(Ctx)) {
    value = apsi->getZExtValue();
    return true;
  }
  return false;
}

ReflexprIdExpr *MetaobjectOpExprBase::getReflexprIdExpr(ASTContext &Ctx,
                                                        Expr *E,
                                                        void *EvlInfo) {
  if (const auto MoId = E->getMetaobjectIdExpr(EvlInfo, Ctx)) {
    return ReflexprIdExpr::fromMetaobjectId(Ctx, *MoId);
  }

  if (MetaobjectIdExpr *MOIE = dyn_cast<MetaobjectIdExpr>(E)) {
    return MOIE->asReflexprIdExpr(Ctx);
  }

  if (ReflexprIdExpr *REE = dyn_cast<ReflexprIdExpr>(E)) {
    return REE;
  }
  return nullptr;
}

llvm::APSInt MetaobjectOpExprBase::makeBoolResult(ASTContext&, QualType,
                                                  bool v) {
  // [reflection-ts] FIXME is there a better way to get true/false APSInt?
  return v ? llvm::APSInt::getMaxValue(1, true)
           : llvm::APSInt::getMinValue(1, true);
}

llvm::APSInt MetaobjectOpExprBase::makeSizeTResult(ASTContext &Ctx, QualType,
                                                   uint64_t v) {
  const unsigned w = Ctx.getTargetInfo().getTypeWidth(
      Ctx.getTargetInfo().getSizeType());
  return llvm::APSInt(llvm::APInt(w, v, false));
}

llvm::APSInt MetaobjectOpExprBase::makeULongResult(ASTContext &Ctx, QualType,
                                                   uint64_t v) {
  const unsigned w = Ctx.getTargetInfo().getLongWidth();
  return llvm::APSInt(llvm::APInt(w, v, false));
}

llvm::APSInt MetaobjectOpExprBase::makeConstantResult(ASTContext& Ctx,
                                                      QualType Ty,
                                                      llvm::APSInt R) {
  R = R.zextOrSelf(Ctx.getIntWidth(Ty));
  return R;
}

llvm::APInt MetaobjectOpExprBase::makeMetaobjectResult(ASTContext &Ctx,
                                                       ReflexprIdExpr *REE) {
  return ReflexprIdExpr::toMetaobjectId(Ctx, REE);
}

llvm::APSInt MetaobjectOpExprBase::opGetConstant(ASTContext &Ctx,
                                                 ReflexprIdExpr *REE) {
  if (REE->isArgumentNamedDecl()) {
    if (const auto *ND = REE->getArgumentNamedDecl()) {
      if (const auto *ECD = dyn_cast<EnumConstantDecl>(ND)) {
        return ECD->getInitVal();
      }
    }
  }
  llvm_unreachable("Unable to get constant value!");
}

// __metaobj_{operation}
QualType UnaryMetaobjectOpExpr::getResultKindType(ASTContext &Ctx,
                                                  UnaryMetaobjectOp Oper,
                                                  MetaobjectOpResult OpRes,
                                                  Expr *argExpr) {
  bool isDependent =
    (argExpr->getDependence() & ExprDependence::TypeValueInstantiation);

  switch (OpRes) {
    case MOOR_Metaobject:
      return Ctx.MetaobjectIdTy;
    case MOOR_SizeT:
      return Ctx.getSizeType();
    case MOOR_ULong:
      return Ctx.UnsignedLongTy;
    case MOOR_Bool:
      return Ctx.BoolTy;
    case MOOR_Constant:
    case MOOR_Pointer: {
      if (ReflexprIdExpr *REE = ReflexprIdExpr::fromExpr(Ctx, argExpr)) {
        if (REE->isArgumentNamedDecl()) {
          if (const auto *ND = REE->getArgumentNamedDecl()) {
            if (const auto *VD = dyn_cast<ValueDecl>(ND)) {
              return UnaryMetaobjectOpExpr::getValueDeclType(Ctx, Oper, VD);
            }
          }
        }
        if (REE->isArgumentLambdaCapture()) {
          if (const auto *LC = REE->getArgumentLambdaCapture()) {
            if (const auto *VD = LC->getCapturedVar()) {
              return UnaryMetaobjectOpExpr::getValueDeclType(Ctx, Oper, VD);
            }
          }
        }
        llvm_unreachable("Unable to find the type of constant-returning operation");
      }

      if (isDependent)
        return Ctx.DependentTy;

      if (OpRes == MOOR_Constant)
        return Ctx.getIntMaxType();

      return Ctx.VoidPtrTy;
    }
    case MOOR_String: {
      if (isDependent) {
        return Ctx.DependentTy;
      }
      QualType CharTyConst = Ctx.CharTy.withConst();
      if (ReflexprIdExpr *REE = ReflexprIdExpr::fromExpr(Ctx, argExpr)) {
        std::string Value;
        if (getStrResult(Ctx, Oper, REE, nullptr, Value)) {
          return Ctx.getStringLiteralArrayType(CharTyConst, Value.size());
        }
        llvm_unreachable("Unable to find the type of string-returning operation");
      }
      return Ctx.getPointerType(CharTyConst);
    }
  }
  return QualType();
}

UnaryMetaobjectOpExpr::UnaryMetaobjectOpExpr(ASTContext &Ctx,
                                             UnaryMetaobjectOp Oper,
                                             MetaobjectOpResult OpRes,
                                             QualType resultType, Expr *argExpr,
                                             ExprValueKind VK,
                                             SourceLocation opLoc,
                                             SourceLocation endLoc)
    : Expr(UnaryMetaobjectOpExprClass, resultType, VK, OK_Ordinary),
      ArgExpr(argExpr), OpLoc(opLoc), EndLoc(endLoc) {

  setKind(Oper);
  setResultKind(OpRes);
  setDependence(computeDependence(this));
}

UnaryMetaobjectOpExpr *
UnaryMetaobjectOpExpr::Create(ASTContext &Ctx, UnaryMetaobjectOp Oper,
                              MetaobjectOpResult OpRes, Expr *argExpr,
                              SourceLocation opLoc, SourceLocation endLoc) {
  QualType resType = getResultKindType(Ctx, Oper, OpRes, argExpr);

  return new (Ctx) UnaryMetaobjectOpExpr(Ctx, Oper, OpRes, resType, argExpr,
                                         VK_PRValue, opLoc, endLoc);
}

StringRef UnaryMetaobjectOpExpr::getOperationSpelling(UnaryMetaobjectOp MoOp) {
  switch (MoOp) {
#define METAOBJECT_OP_1(Spelling, R, Name) \
  case UMOO_##Name: \
    return #Spelling;
#include "clang/Basic/TokenKinds.def"
      break;
  }
  return StringRef();
}

bool UnaryMetaobjectOpExpr::isOperationApplicable(MetaobjectKind MoK,
                                                  UnaryMetaobjectOp MoOp) {

  MetaobjectConcept MoC = translateMetaobjectKindToMetaobjectConcept(MoK);
  switch (MoOp) {
  case UMOO_GetIdValue:
#define METAOBJECT_TRAIT(S, Concept) case UMOO_IsMeta##Concept:
#include "clang/Basic/TokenKinds.def"
  case UMOO_SourceFileNameLen:
  case UMOO_GetSourceFileName:
  case UMOO_GetSourceLine:
  case UMOO_GetSourceColumn:
    return true;
  case UMOO_IsUnnamed:
  case UMOO_NameLen:
  case UMOO_GetName:
  case UMOO_DisplayNameLen:
  case UMOO_GetDisplayName:
    return conceptIsA(MoC, MOC_Named);
  case UMOO_GetUnderlyingType:
  case UMOO_IsScopedEnum:
    return conceptIsA(MoC, MOC_Enum);
  case UMOO_GetScope:
    return conceptIsA(MoC, MOC_ScopeMember);
  case UMOO_GetType:
    return conceptIsA(MoC, MOC_Typed);
  case UMOO_GetAliased:
    return conceptIsA(MoC, MOC_Alias);
  case UMOO_GetTagSpecifier:
  case UMOO_IsEnum:
  case UMOO_UsesClassKey:
  case UMOO_UsesStructKey:
  case UMOO_IsUnion:
    return conceptIsA(MoC, MOC_TagType);
  case UMOO_GetBaseClasses:
  case UMOO_GetPublicBaseClasses:
    return conceptIsA(MoC, MOC_Class);
  case UMOO_GetMemberTypes:
  case UMOO_GetPublicMemberTypes:
  case UMOO_GetDataMembers:
  case UMOO_GetPublicDataMembers:
  case UMOO_GetMemberFunctions:
  case UMOO_GetPublicMemberFunctions:
  case UMOO_GetConstructors:
  case UMOO_GetDestructors:
  case UMOO_GetDestructor:
  case UMOO_GetOperators:
    return conceptIsA(MoC, MOC_Record);
  case UMOO_GetEnumerators:
    return conceptIsA(MoC, MOC_Enum);
  case UMOO_GetParameters:
    return conceptIsA(MoC, MOC_Callable);
  case UMOO_GetCaptures:
  case UMOO_UsesDefaultCopyCapture:
  case UMOO_UsesDefaultReferenceCapture:
  case UMOO_IsCallOperatorConst:
    return conceptIsA(MoC, MOC_Lambda);
  case UMOO_IsExplicitlyCaptured:
    return conceptIsA(MoC, MOC_LambdaCapture);
  case UMOO_HasDefaultArgument:
    return conceptIsA(MoC, MOC_FunctionParameter);
  case UMOO_GetClass:
    return conceptIsA(MoC, MOC_Base);
  case UMOO_GetAccessSpecifier:
  case UMOO_IsPublic:
  case UMOO_IsProtected:
  case UMOO_IsPrivate:
    return conceptIsA(MoC, MOC_RecordMember) ||
           conceptIsA(MoC, MOC_Base);
  case UMOO_IsConstexpr:
    return conceptIsA(MoC, MOC_Variable) ||
           conceptIsA(MoC, MOC_Callable);
  case UMOO_IsNoexcept:
  case UMOO_IsDeleted:
    return conceptIsA(MoC, MOC_Callable);
  case UMOO_IsExplicit:
    return conceptIsA(MoC, MOC_Constructor) ||
           conceptIsA(MoC, MOC_ConversionOperator);
  case UMOO_IsInline:
    return conceptIsA(MoC, MOC_Namespace) ||
           conceptIsA(MoC, MOC_Callable);
  case UMOO_IsThreadLocal:
    return conceptIsA(MoC, MOC_Variable);
  case UMOO_IsStatic:
    return conceptIsA(MoC, MOC_Variable) ||
           conceptIsA(MoC, MOC_MemberFunction);
  case UMOO_IsVirtual:
    return conceptIsA(MoC, MOC_Base) ||
           conceptIsA(MoC, MOC_Destructor) ||
           conceptIsA(MoC, MOC_MemberFunction);
  case UMOO_IsPureVirtual:
    return conceptIsA(MoC, MOC_Destructor) ||
           conceptIsA(MoC, MOC_MemberFunction);
  case UMOO_IsFinal:
    return conceptIsA(MoC, MOC_Class) ||
           conceptIsA(MoC, MOC_MemberFunction);
  case UMOO_IsConst:
  case UMOO_IsVolatile:
  case UMOO_HasLValueRefQualifier:
  case UMOO_HasRValueRefQualifier:
    return conceptIsA(MoC, MOC_MemberFunction);
  case UMOO_IsImplicitlyDeclared:
  case UMOO_IsDefaulted:
    return conceptIsA(MoC, MOC_SpecialMemberFunction);
  case UMOO_IsCopyConstructor:
  case UMOO_IsMoveConstructor:
    return conceptIsA(MoC, MOC_Constructor);
  case UMOO_IsCopyAssignmentOperator:
  case UMOO_IsMoveAssignmentOperator:
    return conceptIsA(MoC, MOC_Operator);
  case UMOO_GetPointer:
    return conceptIsA(MoC, MOC_Variable) ||
           conceptIsA(MoC, MOC_Function);
  case UMOO_GetConstant:
    return conceptIsA(MoC, MOC_Constant);
  case UMOO_HideProtected:
  case UMOO_HidePrivate:
  case UMOO_IsEmpty:
  case UMOO_GetSize:
    return conceptIsA(MoC, MOC_ObjectSequence);
  }
  return false;
}

bool UnaryMetaobjectOpExpr::getTraitValue(UnaryMetaobjectOp MoOp,
                                          MetaobjectConcept Cat) {
  switch (MoOp) {
#define METAOBJECT_TRAIT(S, Concept) \
  case UMOO_IsMeta##Concept: \
    return conceptIsA(Cat, MOC_##Concept);
#include "clang/Basic/TokenKinds.def"
  default:
    llvm_unreachable("Not a metaobject trait operation");
  }
}

uintptr_t UnaryMetaobjectOpExpr::opGetIdValue(ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);

  return REE->getIdValue(Ctx).getZExtValue();
}

uint64_t UnaryMetaobjectOpExpr::opSourceFileNameLen(ASTContext &Ctx,
                                                    ReflexprIdExpr *REE) {
  return opGetSourceFileName(Ctx, REE).size();
}

std::string UnaryMetaobjectOpExpr::opGetSourceFileName(ASTContext &Ctx,
                                                       ReflexprIdExpr *REE) {
  assert(REE);

  StringRef result;
  if (const NamedDecl *ND = REE->findArgumentNamedDecl(Ctx)) {
    SourceLocation L = ND->getLocation();
    result = Ctx.getSourceManager().getFilename(L);
  }
  return result.str();
}

uint64_t UnaryMetaobjectOpExpr::opGetSourceLine(ASTContext &Ctx,
                                                ReflexprIdExpr *REE) {
  assert(REE);

  unsigned result = 0;
  if (const NamedDecl *ND = REE->findArgumentNamedDecl(Ctx)) {
    SourceLocation L = ND->getLocation();
    result = Ctx.getSourceManager().getSpellingLineNumber(L);
  }
  return result;
}

uint64_t UnaryMetaobjectOpExpr::opGetSourceColumn(ASTContext &Ctx,
                                                  ReflexprIdExpr *REE) {
  assert(REE);

  unsigned result = 0;
  if (const NamedDecl *ND = REE->findArgumentNamedDecl(Ctx)) {
    SourceLocation L = ND->getLocation();
    result = Ctx.getSourceManager().getSpellingColumnNumber(L);
  }
  return result;
}

bool UnaryMetaobjectOpExpr::opIsUnnamed(ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);

  if (REE->isArgumentSpecifier()) {
    return false;
  } else if (REE->isArgumentNamedDecl()) {
    return REE->getArgumentNamedDecl()->getName().empty();
  } else if (REE->isArgumentLambdaCapture()) {
    if (const auto *VD = REE->getArgumentLambdaCapture()->getCapturedVar()) {
      return VD->getName().empty();
    }
  } else if (REE->isArgumentType()) {
    QualType RT = REE->getBaseArgumentType(Ctx);

    if (const NamedDecl *ND = ReflexprIdExpr::findTypeDecl(RT)) {
      return ND->getName().empty();
    } else if (isa<BuiltinType>(RT)) {
      return false;
    } else if (RT.getBaseTypeIdentifier()) {
      return RT.getBaseTypeIdentifier()->getName().empty();
    }
  }
  return true;
}

std::string UnaryMetaobjectOpExpr::getOperatorSpelling(
    ASTContext &, OverloadedOperatorKind OOK) {
  switch(OOK) {
#define OVERLOADED_OPERATOR(Name, Spelling, Token, Unary, Binary, MemberOnly)  \
    case OO_##Name: \
      return Spelling;
#include "clang/Basic/OperatorKinds.def"
#undef OVERLOADED_OPERATOR
    case OO_None:
    case NUM_OVERLOADED_OPERATORS:
      break;
  }
  return {};
}

std::string UnaryMetaobjectOpExpr::opGetName(ASTContext &Ctx,
                                             ReflexprIdExpr *REE) {
  assert(REE);

  if (REE->isArgumentGlobalScope()) {
    return {};
  } else if (REE->isArgumentNoSpecifier()) {
    return {};
  } else if (REE->isArgumentSpecifier()) {
    return tok::getKeywordSpelling(
        REE->getArgumentSpecifierKind());
  } else if (REE->isArgumentNamedDecl()) {
    const auto *ND = REE->getArgumentNamedDecl();
    if (const auto *CD = dyn_cast<CXXConstructorDecl>(ND)) {
      return CD->getParent()->getName().str();
    } else if (const auto *DD = dyn_cast<CXXDestructorDecl>(ND)) {
      return "~" + DD->getParent()->getName().str();
    } else if (const auto *FD = dyn_cast<FunctionDecl>(ND)) {
      const std::string spelling =
        getOperatorSpelling(Ctx, FD->getOverloadedOperator());
      if (!spelling.empty()) {
        if (std::ispunct(spelling.front())) {
          return "operator" + spelling;
        }
        return "operator " + spelling;
      }
    }
    return ND->getName().str();
  } else if (REE->isArgumentLambdaCapture()) {
    const auto *LC = REE->getArgumentLambdaCapture();
    if (const auto *VD = LC->getCapturedVar()) {
      return VD->getName().str();
    }
    if (LC->capturesThis()) {
      return "this";
    }
    return {};
  } else if (REE->isArgumentType()) {
    QualType RT = REE->getBaseArgumentType(Ctx);

    if (!RT.isNull()) {
      if (const NamedDecl *ND = ReflexprIdExpr::findTypeDecl(RT)) {
        return ND->getName().str();
      } else if (const auto *BT = dyn_cast<BuiltinType>(RT)) {
        return BT->getName(Ctx.getPrintingPolicy()).str();
      } else if (RT.getBaseTypeIdentifier()) {
        return RT.getBaseTypeIdentifier()->getName().str();
      } else
        return std::string();
    }
  }
  llvm_unreachable("Unable to get metaobject name!");
}

uint64_t UnaryMetaobjectOpExpr::opNameLen(ASTContext &Ctx,
                                          ReflexprIdExpr *REE) {
  return opGetName(Ctx, REE).size();
}

std::string UnaryMetaobjectOpExpr::opGetDisplayName(ASTContext &Ctx,
                                                    ReflexprIdExpr *REE) {
  assert(REE);

  if (REE->isArgumentGlobalScope()) {
    return std::string("::", 2);
  } else if (REE->isArgumentNamedDecl()) {
    return REE->getArgumentNamedDecl()->getQualifiedNameAsString();
  } else if (REE->isArgumentType()) {
    QualType RT = REE->getArgumentType();
    if (const NamedDecl *ND = ReflexprIdExpr::findTypeDecl(RT)) {
      return ND->getQualifiedNameAsString();
    }
    // [reflection-ts] FIXME can we use this ?
    // return TypeName::getFullyQualifiedName(RT, Ctx);
    // otherwise we'd need to copy its functionality here
  }
  return opGetName(Ctx, REE);
}

uint64_t UnaryMetaobjectOpExpr::opDisplayNameLen(ASTContext &Ctx,
                                                 ReflexprIdExpr *REE) {
  return opGetDisplayName(Ctx, REE).size();
}


ReflexprIdExpr *UnaryMetaobjectOpExpr::opGetScope(ASTContext &Ctx,
                                                  ReflexprIdExpr *REE) {
  assert(REE);

  if (const NamedDecl *ND = REE->findArgumentNamedDecl(Ctx)) {
    if (const DeclContext *scopeDC = ND->getDeclContext()) {
      if (const NamedDecl *nDecl = dyn_cast<NamedDecl>(scopeDC)) {
        return ReflexprIdExpr::getNamedDeclReflexprIdExpr(Ctx, nDecl);
      }
    }
  }
  // [reflection-ts] FIXME

  return ReflexprIdExpr::getGlobalScopeReflexprIdExpr(Ctx);
}

ReflexprIdExpr *UnaryMetaobjectOpExpr::opGetType(ASTContext &Ctx,
                                                 ReflexprIdExpr *REE) {
  assert(REE);

  if (const NamedDecl *ND = REE->findArgumentNamedDecl(Ctx)) {
    if (const auto *FD = dyn_cast<FunctionDecl>(ND)) {
      return ReflexprIdExpr::getTypeReflexprIdExpr(Ctx, FD->getReturnType(), true);
    }
    if (const auto *VD = dyn_cast<ValueDecl>(ND)) {
      return ReflexprIdExpr::getTypeReflexprIdExpr(Ctx, VD->getType(), true);
    }
    if (const auto *DD = dyn_cast<DeclaratorDecl>(ND)) {
      if (TypeSourceInfo *TInfo = DD->getTypeSourceInfo()) {
        return ReflexprIdExpr::getTypeReflexprIdExpr(Ctx, TInfo, true);
      }
    }
    if (const DeclContext *scopeDC = ND->getDeclContext()) {
      if (const auto *ED = dyn_cast<EnumDecl>(scopeDC)) {
        return ReflexprIdExpr::getNamedDeclReflexprIdExpr(Ctx, ED);
      }
    }
  }
  // [reflection-ts] FIXME
  llvm_unreachable("Failed to get type!");

  return REE;
}

ReflexprIdExpr *UnaryMetaobjectOpExpr::opGetUnderlyingType(
    ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);

  if (REE->isArgumentNamedDecl()) {
    if (const auto *ED = dyn_cast<EnumDecl>(REE->getArgumentNamedDecl())) {
      if (TypeSourceInfo *TInfo = ED->getIntegerTypeSourceInfo()) {
        return ReflexprIdExpr::getTypeReflexprIdExpr(Ctx, TInfo, true);
      }
      QualType UTy = ED->getIntegerType();
      return ReflexprIdExpr::getTypeReflexprIdExpr(Ctx, UTy, true);
    }
  }

  // [reflection-ts] FIXME
  llvm_unreachable("Failed to get underlying type!");

  return REE;
}

ReflexprIdExpr *UnaryMetaobjectOpExpr::opGetAliased(ASTContext &Ctx,
                                                    ReflexprIdExpr *REE) {
  assert(REE);

  if (REE->isArgumentType()) {
    QualType RT = REE->getArgumentType();
    if (const auto *STTPT = dyn_cast<SubstTemplateTypeParmType>(RT)) {
      QualType Ty = STTPT->getReplacementType();
      return ReflexprIdExpr::getTypeReflexprIdExpr(Ctx, Ty, true);
    } else if (const auto *TDT = dyn_cast<TypedefType>(RT)) {
      QualType Ty = TDT->desugar();
      return ReflexprIdExpr::getTypeReflexprIdExpr(Ctx, Ty, true);
    }
  }

  if (const NamedDecl *ND = REE->findArgumentNamedDecl(Ctx)) {
    if (const auto *TDND = dyn_cast<TypedefNameDecl>(ND)) {
      QualType Ty = TDND->getUnderlyingType();
      return ReflexprIdExpr::getTypeReflexprIdExpr(Ctx, Ty, true);
    } else if (const auto *TD = dyn_cast<TypeDecl>(ND)) {
      QualType Ty(TD->getTypeForDecl(), 0);
      return ReflexprIdExpr::getTypeReflexprIdExpr(Ctx, Ty, true);
    } else if (const auto *NsAD = dyn_cast<NamespaceAliasDecl>(ND)) {
      const NamespaceDecl *NsD = NsAD->getNamespace();
      return ReflexprIdExpr::getNamedDeclReflexprIdExpr(Ctx, NsD);
    }
  }
  // [reflection-ts] FIXME
  //llvm_unreachable("Failed to get aliased declaration or type!");

  return REE;
}

ReflexprIdExpr *UnaryMetaobjectOpExpr::opGetTagSpecifier(ASTContext &C,
                                                         ReflexprIdExpr *REE) {
  assert(REE);

  if (const NamedDecl *ND = REE->findArgumentNamedDecl(C, true)) {
    if (const TagDecl *TD = dyn_cast<TagDecl>(ND)) {
      switch (TD->getTagKind()) {
      case TTK_Enum:
        return ReflexprIdExpr::getSpecifierReflexprIdExpr(C, tok::kw_enum);
      case TTK_Union:
        return ReflexprIdExpr::getSpecifierReflexprIdExpr(C, tok::kw_union);
      case TTK_Class:
        return ReflexprIdExpr::getSpecifierReflexprIdExpr(C, tok::kw_class);
      case TTK_Struct:
        return ReflexprIdExpr::getSpecifierReflexprIdExpr(C, tok::kw_struct);
      case TTK_Interface:
        return ReflexprIdExpr::getSpecifierReflexprIdExpr(C, tok::kw___interface);
      }
    }
  }
  return ReflexprIdExpr::getNoSpecifierReflexprIdExpr(C);
}

bool UnaryMetaobjectOpExpr::opIsEnum(ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);

  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *TD = dyn_cast<TagDecl>(ND))
      return TD->getTagKind() == TTK_Enum;
  }
  return false;
}

bool UnaryMetaobjectOpExpr::opIsUnion(ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);

  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *TD = dyn_cast<TagDecl>(ND))
      return TD->getTagKind() == TTK_Union;
  }
  return false;
}

bool UnaryMetaobjectOpExpr::opUsesClassKey(
    ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);

  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *TD = dyn_cast<TagDecl>(ND))
      return TD->getTagKind() == TTK_Class;
  }
  return false;
}

bool UnaryMetaobjectOpExpr::opUsesStructKey(
    ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);

  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *TD = dyn_cast<TagDecl>(ND))
      return TD->getTagKind() == TTK_Struct;
  }
  return false;
}

bool UnaryMetaobjectOpExpr::opUsesDefaultCopyCapture(
    ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);

  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *RD = dyn_cast<CXXRecordDecl>(ND))
      return RD->getLambdaCaptureDefault() == LCD_ByCopy;
  }
  return false;
}

bool UnaryMetaobjectOpExpr::opUsesDefaultReferenceCapture(
    ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);

  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *RD = dyn_cast<CXXRecordDecl>(ND))
      return RD->getLambdaCaptureDefault() == LCD_ByRef;
  }
  return false;
}

bool UnaryMetaobjectOpExpr::opIsCallOperatorConst(
    ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);

  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *RD = dyn_cast<CXXRecordDecl>(ND))
      if (RD->isLambda()) {
        if (const auto *CMD = RD->getLambdaCallOperator()) {
          return CMD->isConst();
        }
      }
  }
  return false;
}

bool UnaryMetaobjectOpExpr::opIsExplicitlyCaptured(
    ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);

  if (const auto *LC = REE->findArgumentLambdaCapture(Ctx)) {
    return LC->isExplicit();
  }
  return false;
}

bool UnaryMetaobjectOpExpr::opHasDefaultArgument(
    ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);

  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *PVD = dyn_cast<ParmVarDecl>(ND))
      return PVD->hasDefaultArg();
  }
  return false;
}

ReflexprIdExpr *UnaryMetaobjectOpExpr::opGetAccessSpecifier(ASTContext &Ctx,
                                                            ReflexprIdExpr *REE) {
  assert(REE);

  switch (getArgumentAccess(Ctx, REE)) {
  case AS_public:
    return ReflexprIdExpr::getSpecifierReflexprIdExpr(Ctx, tok::kw_public);
  case AS_protected:
    return ReflexprIdExpr::getSpecifierReflexprIdExpr(Ctx, tok::kw_protected);
  case AS_private:
    return ReflexprIdExpr::getSpecifierReflexprIdExpr(Ctx, tok::kw_private);
  case AS_none:
    break;
  }
  return ReflexprIdExpr::getNoSpecifierReflexprIdExpr(Ctx);
}

bool UnaryMetaobjectOpExpr::opIsPublic(ASTContext &Ctx, ReflexprIdExpr *REE) {
  return getArgumentAccess(Ctx, REE) == AS_public;
}
bool UnaryMetaobjectOpExpr::opIsProtected(ASTContext &Ctx, ReflexprIdExpr *REE) {
  return getArgumentAccess(Ctx, REE) == AS_protected;
}
bool UnaryMetaobjectOpExpr::opIsPrivate(ASTContext &Ctx, ReflexprIdExpr *REE) {
  return getArgumentAccess(Ctx, REE) == AS_private;
}

ReflexprIdExpr *UnaryMetaobjectOpExpr::opGetBaseClasses(
    ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);
  REE = ReflexprIdExpr::getSeqReflexprIdExpr(Ctx, REE, MOSK_BaseClasses);
  REE->setAccessibility(MOA_AllowPrivate);
  return REE;
}

ReflexprIdExpr *UnaryMetaobjectOpExpr::opGetPublicBaseClasses(
    ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);
  REE = ReflexprIdExpr::getSeqReflexprIdExpr(Ctx, REE, MOSK_BaseClasses);
  REE->setAccessibility(MOA_OnlyPublic);
  return REE;
}

ReflexprIdExpr *UnaryMetaobjectOpExpr::opGetMemberTypes(
    ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);
  REE = ReflexprIdExpr::getSeqReflexprIdExpr(Ctx, REE, MOSK_MemberTypes);
  REE->setAccessibility(MOA_AllowPrivate);
  return REE;
}

ReflexprIdExpr *UnaryMetaobjectOpExpr::opGetPublicMemberTypes(
    ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);
  REE = ReflexprIdExpr::getSeqReflexprIdExpr(Ctx, REE, MOSK_MemberTypes);
  REE->setAccessibility(MOA_OnlyPublic);
  return REE;
}

ReflexprIdExpr *UnaryMetaobjectOpExpr::opGetDataMembers(
    ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);
  REE = ReflexprIdExpr::getSeqReflexprIdExpr(Ctx, REE, MOSK_DataMembers);
  REE->setAccessibility(MOA_AllowPrivate);
  return REE;
}

ReflexprIdExpr *UnaryMetaobjectOpExpr::opGetPublicDataMembers(
    ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);
  REE = ReflexprIdExpr::getSeqReflexprIdExpr(Ctx, REE, MOSK_DataMembers);
  REE->setAccessibility(MOA_OnlyPublic);
  return REE;
}

ReflexprIdExpr *UnaryMetaobjectOpExpr::opGetMemberFunctions(
    ASTContext &Ctx, ReflexprIdExpr* REE) {
  assert(REE);
  REE = ReflexprIdExpr::getSeqReflexprIdExpr(Ctx, REE, MOSK_MemberFunctions);
  REE->setAccessibility(MOA_AllowPrivate);
  return REE;
}

ReflexprIdExpr *UnaryMetaobjectOpExpr::opGetPublicMemberFunctions(
    ASTContext &Ctx, ReflexprIdExpr* REE) {
  assert(REE);
  REE = ReflexprIdExpr::getSeqReflexprIdExpr(Ctx, REE, MOSK_MemberFunctions);
  REE->setAccessibility(MOA_OnlyPublic);
  return REE;
}

ReflexprIdExpr *UnaryMetaobjectOpExpr::opGetConstructors(
    ASTContext &Ctx, ReflexprIdExpr* REE) {
  assert(REE);
  REE = ReflexprIdExpr::getSeqReflexprIdExpr(Ctx, REE, MOSK_Constructors);
  REE->setAccessibility(MOA_AllowPrivate);
  return REE;
}

ReflexprIdExpr *UnaryMetaobjectOpExpr::opGetDestructors(
    ASTContext &Ctx, ReflexprIdExpr* REE) {
  assert(REE);
  REE = ReflexprIdExpr::getSeqReflexprIdExpr(Ctx, REE, MOSK_Destructors);
  REE->setAccessibility(MOA_AllowPrivate);
  return REE;
}

ReflexprIdExpr *UnaryMetaobjectOpExpr::opGetOperators(
    ASTContext &Ctx, ReflexprIdExpr* REE) {
  assert(REE);
  REE = ReflexprIdExpr::getSeqReflexprIdExpr(Ctx, REE, MOSK_Operators);
  REE->setAccessibility(MOA_AllowPrivate);
  return REE;
}

ReflexprIdExpr *UnaryMetaobjectOpExpr::opGetEnumerators(
    ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);
  REE = ReflexprIdExpr::getSeqReflexprIdExpr(Ctx, REE, MOSK_Enumerators);
  REE->setAccessibility(MOA_AllowPrivate);
  return REE;
}

ReflexprIdExpr *UnaryMetaobjectOpExpr::opGetParameters(
    ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);
  REE = ReflexprIdExpr::getSeqReflexprIdExpr(Ctx, REE, MOSK_Parameters);
  REE->setAccessibility(MOA_AllowPrivate);
  return REE;
}

ReflexprIdExpr *UnaryMetaobjectOpExpr::opGetCaptures(
    ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);
  REE = ReflexprIdExpr::getSeqReflexprIdExpr(Ctx, REE, MOSK_Captures);
  REE->setAccessibility(MOA_AllowPrivate);
  return REE;
}

ReflexprIdExpr *UnaryMetaobjectOpExpr::opGetDestructor(ASTContext &Ctx,
                                                       ReflexprIdExpr *REE) {
  assert(REE);

  if (const NamedDecl *ND = REE->findArgumentNamedDecl(Ctx)) {
    if (const auto *RD = dyn_cast<CXXRecordDecl>(ND)) {
      return ReflexprIdExpr::getNamedDeclReflexprIdExpr(
          Ctx, RD->getDestructor());
    }
  }

  return REE;
}

ReflexprIdExpr *UnaryMetaobjectOpExpr::opGetClass(ASTContext &Ctx,
                                                  ReflexprIdExpr *REE) {
  assert(REE && REE->isArgumentBaseSpecifier());

  const CXXBaseSpecifier *BS = REE->getArgumentBaseSpecifier();
  return ReflexprIdExpr::getTypeReflexprIdExpr(Ctx, BS->getTypeSourceInfo(), true);
}

bool UnaryMetaobjectOpExpr::opIsScopedEnum(ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);

  if (REE->isArgumentNamedDecl()) {
    if (const auto *ED = dyn_cast<EnumDecl>(REE->getArgumentNamedDecl()))
      return ED->isScoped();
  }
  return true;
}

bool UnaryMetaobjectOpExpr::opIsConstexpr(ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);

  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *VD = dyn_cast<VarDecl>(ND))
      return VD->isConstexpr();
    if (const auto *FD = dyn_cast<FunctionDecl>(ND))
      return FD->isConstexpr();
  }
  return false;
}

bool UnaryMetaobjectOpExpr::opIsNoexcept(ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);

  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *FD = dyn_cast<FunctionDecl>(ND)) {
      const auto EST = FD->getExceptionSpecType();
      return EST == EST_BasicNoexcept || EST == EST_NoexceptTrue;
    }
  }
  return false;
}

bool UnaryMetaobjectOpExpr::opIsExplicit(ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);

  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *CTD = dyn_cast<CXXConstructorDecl>(ND))
      return CTD->isExplicit();
    if (const auto *COD = dyn_cast<CXXConversionDecl>(ND))
      return COD->isExplicit();
  }
  return false;
}

bool UnaryMetaobjectOpExpr::opIsInline(ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);

  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *VD = dyn_cast<VarDecl>(ND))
      return VD->isInline() || VD->isInlineSpecified();
    if (const auto *FD = dyn_cast<FunctionDecl>(ND))
      return FD->isInlineSpecified();
    if (const auto *NSD = dyn_cast<NamespaceDecl>(ND))
      return NSD->isInline();
  }
  return false;
}

bool UnaryMetaobjectOpExpr::opIsThreadLocal(ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);

  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *VD = dyn_cast<VarDecl>(ND))
      return VD->getTLSKind() != VarDecl::TLS_None;
  }
  return false;
}

bool UnaryMetaobjectOpExpr::opIsStatic(ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);

  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *VD = dyn_cast<VarDecl>(ND))
      return VD->isStaticDataMember() || VD->isStaticLocal();
    if (const auto *FD = dyn_cast<FunctionDecl>(ND)) {
      if (const auto *CMD = dyn_cast<CXXMethodDecl>(FD)) {
        return CMD->isStatic();
      }
      return FD->isStatic();
    }
  }
  return false;
}

bool UnaryMetaobjectOpExpr::opIsVirtual(ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);

  if (REE->isArgumentBaseSpecifier()) {
    return REE->getArgumentBaseSpecifier()->isVirtual();
  }
  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *FD = dyn_cast<FunctionDecl>(ND)) {
      if (const auto *CMD = dyn_cast<CXXMethodDecl>(FD)) {
        return CMD->isVirtual();
      }
      return FD->isVirtualAsWritten();
    }
  }
  return false;
}

bool UnaryMetaobjectOpExpr::opIsPureVirtual(ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);

  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *FD = dyn_cast<FunctionDecl>(ND)) {
      if (const auto *CMD = dyn_cast<CXXMethodDecl>(FD)) {
        return CMD->isVirtual() && CMD->isPure();
      }
      return FD->isVirtualAsWritten() && FD->isPure();
    }
  }
  return false;
}

bool UnaryMetaobjectOpExpr::opIsFinal(ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);

  // [reflection-ts] FIXME
  return false;
}

bool UnaryMetaobjectOpExpr::opIsConst(
    ASTContext &Ctx, ReflexprIdExpr* REE) {
  assert(REE);

  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *CMD = dyn_cast<CXXMethodDecl>(ND)) {
      return CMD->isConst();
    }
  }
  return false;
}

bool UnaryMetaobjectOpExpr::opIsVolatile(
    ASTContext &Ctx, ReflexprIdExpr* REE) {
  assert(REE);

  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *CMD = dyn_cast<CXXMethodDecl>(ND)) {
      return CMD->isVolatile();
    }
  }
  return false;
}

bool UnaryMetaobjectOpExpr::opHasLValueRefQualifier(
    ASTContext &Ctx, ReflexprIdExpr* REE) {
  assert(REE);

  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *CMD = dyn_cast<CXXMethodDecl>(ND)) {
      return CMD->getRefQualifier() == RQ_LValue;
    }
  }
  return false;
}

bool UnaryMetaobjectOpExpr::opHasRValueRefQualifier(
    ASTContext &Ctx, ReflexprIdExpr* REE) {
  assert(REE);

  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *CMD = dyn_cast<CXXMethodDecl>(ND)) {
      return CMD->getRefQualifier() == RQ_RValue;
    }
  }
  return false;
}

bool UnaryMetaobjectOpExpr::opIsImplicitlyDeclared(
    ASTContext &Ctx, ReflexprIdExpr* REE) {
  assert(REE);

  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *FD = dyn_cast<FunctionDecl>(ND)) {
      return !FD->isUserProvided() && !FD->isExplicitlyDefaulted();
    }
  }
  return false;
}

bool UnaryMetaobjectOpExpr::opIsDefaulted(
    ASTContext &Ctx, ReflexprIdExpr* REE) {
  assert(REE);

  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *FD = dyn_cast<FunctionDecl>(ND)) {
      return FD->isDefaulted();
    }
  }
  return false;
}

bool UnaryMetaobjectOpExpr::opIsDeleted(
    ASTContext &Ctx, ReflexprIdExpr* REE) {
  assert(REE);

  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *FD = dyn_cast<FunctionDecl>(ND)) {
      return FD->isDeleted();
    }
  }
  return false;
}

bool UnaryMetaobjectOpExpr::opIsCopyConstructor(
    ASTContext &Ctx, ReflexprIdExpr* REE) {
  assert(REE);

  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *CCD = dyn_cast<CXXConstructorDecl>(ND)) {
      return CCD->isCopyConstructor();
    }
  }
  return false;
}

bool UnaryMetaobjectOpExpr::opIsMoveConstructor(
    ASTContext &Ctx, ReflexprIdExpr* REE) {
  assert(REE);

  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *CCD = dyn_cast<CXXConstructorDecl>(ND)) {
      return CCD->isMoveConstructor();
    }
  }
  return false;
}

bool UnaryMetaobjectOpExpr::opIsCopyAssignmentOperator(
    ASTContext &Ctx, ReflexprIdExpr* REE) {
  assert(REE);

  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *CMD = dyn_cast<CXXMethodDecl>(ND)) {
      return CMD->isCopyAssignmentOperator();
    }
  }
  return false;
}

bool UnaryMetaobjectOpExpr::opIsMoveAssignmentOperator(
    ASTContext &Ctx, ReflexprIdExpr* REE) {
  assert(REE);

  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *CMD = dyn_cast<CXXMethodDecl>(ND)) {
      return CMD->isMoveAssignmentOperator();
    }
  }
  return false;
}

ReflexprIdExpr *UnaryMetaobjectOpExpr::opHideProtected(ASTContext &Ctx,
                                                       ReflexprIdExpr *REE) {
  assert(REE);

  return ReflexprIdExpr::getHideProtectedReflexprIdExpr(Ctx, REE);
}

ReflexprIdExpr *UnaryMetaobjectOpExpr::opHidePrivate(ASTContext &Ctx,
                                                     ReflexprIdExpr *REE) {
  assert(REE);

  return ReflexprIdExpr::getHidePrivateReflexprIdExpr(Ctx, REE);
}

template <typename Action>
static void applyOnMetaobjSeqElements(ASTContext &Ctx, Action &act,
                                      ReflexprIdExpr *REE) {
  assert(REE && REE->getKind() == MOK_ObjectSequence);

  const MetaobjectAccessibility access = REE->getAccessibility();

  if (const auto *ND = REE->findArgumentNamedDecl(Ctx, true)) {
    if (const auto *DC = dyn_cast<DeclContext>(ND)) {
      if (REE->getSeqKind() == MOSK_MemberTypes) {
        auto matches = [](const Decl *d) -> bool {
          if (isa<CXXRecordDecl>(d) && d->isImplicit()) {
            return false;
          }
          return isa<TypeDecl>(d);
        };
        act(matches, DC->decls_begin(), DC->decls_end(), access);
      } else if (REE->getSeqKind() == MOSK_DataMembers) {
        auto matches = [](const Decl *d) -> bool {
          return isa<FieldDecl>(d) || isa<VarDecl>(d);
        };
        act(matches, DC->decls_begin(), DC->decls_end(), access);
      } else if (REE->getSeqKind() == MOSK_Constructors) {
        auto matches = [](const Decl *d) -> bool {
          return isa<CXXConstructorDecl>(d);
        };
        act(matches, DC->decls_begin(), DC->decls_end(), access);
      } else if (REE->getSeqKind() == MOSK_Destructors) {
        auto matches = [](const Decl *d) -> bool {
          return isa<CXXDestructorDecl>(d);
        };
        act(matches, DC->decls_begin(), DC->decls_end(), access);
      } else if (REE->getSeqKind() == MOSK_Operators) {
        auto matches = [](const Decl *d) -> bool {
          if (const auto *CMF = dyn_cast<CXXMethodDecl>(d)) {
            return CMF->isMoveAssignmentOperator() ||
                   CMF->isCopyAssignmentOperator() ||
                   CMF->isOverloadedOperator();
          }
          return false;
        };
        act(matches, DC->decls_begin(), DC->decls_end(), access);
      } else if (REE->getSeqKind() == MOSK_MemberFunctions) {
        auto matches = [](const Decl *d) -> bool {
          if (const auto *CMF = dyn_cast<CXXMethodDecl>(d)) {
            return !isa<CXXConstructorDecl>(CMF) &&
                   !isa<CXXDestructorDecl>(CMF) &&
                   !CMF->isMoveAssignmentOperator() &&
                   !CMF->isCopyAssignmentOperator() &&
                   !CMF->isOverloadedOperator();
          }
          return false;
        };
        act(matches, DC->decls_begin(), DC->decls_end(), access);
      } else if (REE->getSeqKind() == MOSK_Enumerators) {
        auto matches = [](const Decl *d) -> bool {
          return isa<EnumConstantDecl>(d);
        };
        act(matches, DC->decls_begin(), DC->decls_end(), access);
      } else if (REE->getSeqKind() == MOSK_BaseClasses) {
        if (const auto *RD = dyn_cast<CXXRecordDecl>(ND)) {
          auto matches = [](const CXXBaseSpecifier &) -> bool { return true; };
          act(matches, RD->bases_begin(), RD->bases_end(), access);
        }
      } else if (REE->getSeqKind() == MOSK_Parameters) {
        if (const auto *FD = dyn_cast<FunctionDecl>(ND)) {
          auto matches = [](const Decl *) -> bool { return true; };
          act(matches, FD->param_begin(), FD->param_end(), access);
        }
      } else if (REE->getSeqKind() == MOSK_Captures) {
        if (const auto *FD = dyn_cast<CXXRecordDecl>(ND)) {
          auto matches = [](const LambdaCapture&) -> bool { return true; };
          act(matches, FD->captures_begin(), FD->captures_end(), access);
        }
      }
    }
  }
}

struct matchingMetaobjSeqElementUtils {
  static bool isAccessible(const CXXBaseSpecifier &x, MetaobjectAccessibility access) {
    switch (access) {
      case MOA_AllowPrivate:
        break;
      case MOA_AllowProtected:
        return x.getAccessSpecifier() != AS_private;
      case MOA_OnlyPublic:
      case MOA_ContextDependent: // [reflection-ts] FIXME
        return x.getAccessSpecifier() != AS_private &&
               x.getAccessSpecifier() != AS_protected;
    }
    return true;
  }

  static bool isAccessible(const Decl *x, MetaobjectAccessibility access) {
    switch (access) {
      case MOA_AllowPrivate:
        break;
      case MOA_AllowProtected:
        return x->getAccess() != AS_private;
      case MOA_OnlyPublic:
      case MOA_ContextDependent: // [reflection-ts] FIXME
        return x->getAccess() != AS_private &&
               x->getAccess() != AS_protected;
    }
    return true;
  }

  static bool isAccessible(const LambdaCapture &, MetaobjectAccessibility) {
    return true;
  }
};

struct hasMatchingMetaobjSeqElements : matchingMetaobjSeqElementUtils {
  bool isEmpty{true};

  template <typename Predicate, typename Iter>
  void operator()(Predicate &matches, Iter i, Iter e,
                  MetaobjectAccessibility access) {

    while (i != e) {
      if (isAccessible(*i, access) && matches(*i)) {
          isEmpty = false;
          break;
      }
      ++i;
    }
  }
};

bool UnaryMetaobjectOpExpr::opIsEmpty(ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);

  hasMatchingMetaobjSeqElements action;
  applyOnMetaobjSeqElements(Ctx, action, REE);
  return action.isEmpty;
}

struct countMatchingMetaobjSeqElements : matchingMetaobjSeqElementUtils {
  uint64_t count{0ULL};

  template <typename Predicate, typename Iter>
  void operator()(Predicate &matches, Iter i, Iter e,
                  MetaobjectAccessibility access) {

    while (i != e) {
      if (isAccessible(*i, access) && matches(*i)) {
          ++count;
      }
      ++i;
    }
  }
};

uint64_t UnaryMetaobjectOpExpr::opGetSize(ASTContext &Ctx, ReflexprIdExpr *REE) {
  assert(REE);

  countMatchingMetaobjSeqElements action;
  applyOnMetaobjSeqElements(Ctx, action, REE);
  return action.count;
}

struct findMatchingMetaobjSeqElement : matchingMetaobjSeqElementUtils {
  unsigned index;
  union {
    void *rptr;
    const Decl *decl;
    const CXXBaseSpecifier *base;
    const LambdaCapture *capture;
  } result;

  void storeResult(const Decl *d) { result.decl = d; }
  void storeResult(const CXXBaseSpecifier &b) { result.base = &b; }
  void storeResult(const LambdaCapture &c) { result.capture = &c; }

  findMatchingMetaobjSeqElement(unsigned idx) : index(idx) {
    result.rptr = nullptr;
  }

  template <typename Predicate, typename Iter>
  void operator()(Predicate &matches, Iter i, Iter e,
                  MetaobjectAccessibility access) {
    if (i != e) {
      while (i != e) {
        if (isAccessible(*i, access) && matches(*i)) {
          if (index == 0)
            break;
          --index;
        }
        ++i;
      }
      assert((index == 0) && "Metaobject sequence index out of range");
      storeResult(*i);
    }
  }
};

struct collectMatchingMetaobjSeqElements : matchingMetaobjSeqElementUtils {
  std::vector<const void *> elements;

  void pushElement(const Decl *d) { elements.push_back(d); }
  void pushElement(const CXXBaseSpecifier &b) { elements.push_back(&b); }
  void pushElement(const LambdaCapture &c) { elements.push_back(&c); }

  collectMatchingMetaobjSeqElements(void) { elements.reserve(8); }

  template <typename Predicate, typename Iter>
  void operator()(Predicate matches, Iter i, Iter e,
                  MetaobjectAccessibility access) {

    while (i != e) {
      if (isAccessible(*i, access) && matches(*i)) {
        pushElement(*i);
      }
      ++i;
    }
  }
};

void UnaryMetaobjectOpExpr::unpackSequence(ASTContext &Ctx, ReflexprIdExpr *REE,
                                           std::vector<llvm::APInt> &dest) {
  assert(REE);

  collectMatchingMetaobjSeqElements action;
  applyOnMetaobjSeqElements(Ctx, action, REE);

  dest.reserve(dest.size() + action.elements.size());

  if (REE->getSeqKind() == MOSK_BaseClasses) {
    for (const void *E : action.elements) {
      const CXXBaseSpecifier *B = static_cast<const CXXBaseSpecifier *>(E);

      ReflexprIdExpr *REE =
        ReflexprIdExpr::getBaseSpecifierReflexprIdExpr(Ctx, B);
      dest.push_back(makeMetaobjectResult(Ctx, REE));
    }
  } else if (REE->getSeqKind() == MOSK_Captures) {
    for (const void *E : action.elements) {
      const LambdaCapture *C = static_cast<const LambdaCapture *>(E);

      ReflexprIdExpr *REE =
        ReflexprIdExpr::getLambdaCaptureReflexprIdExpr(Ctx, C);
      dest.push_back(makeMetaobjectResult(Ctx, REE));
    }
  } else {
    for (const void *E : action.elements) {
      const Decl *D = static_cast<const Decl *>(E);
      const NamedDecl *ND = dyn_cast<NamedDecl>(D);
      assert(ND != nullptr);

      ReflexprIdExpr *REE =
        ReflexprIdExpr::getNamedDeclReflexprIdExpr(Ctx, ND);
      dest.push_back(makeMetaobjectResult(Ctx, REE));
    }
  }
}

bool UnaryMetaobjectOpExpr::hasIntResult() const {
  switch (getResultKind()) {
    case MOOR_SizeT:
    case MOOR_ULong:
    case MOOR_Bool:
    case MOOR_Constant:
      return true;
    case MOOR_Pointer:
    case MOOR_String:
    case MOOR_Metaobject:
      break;
  }
  return false;
}

bool UnaryMetaobjectOpExpr::getIntResult(ASTContext &Ctx, void *EvlInfo,
                                         llvm::APSInt &result) const {
  if (ReflexprIdExpr *REE = getArgumentReflexprIdExpr(Ctx, EvlInfo)) {
    const UnaryMetaobjectOp MoOp = getKind();
    switch (MoOp) {
#define METAOBJECT_INT_OP_1(S, OpRes, OpName) \
      case UMOO_##OpName: \
        result = make##OpRes##Result(Ctx, getType(), op##OpName(Ctx, REE)); \
        return true;
#include "clang/Basic/TokenKinds.def"
#undef METAOBJECT_INT_OP_1

#define METAOBJECT_TRAIT(S, Concept) \
      case UMOO_IsMeta##Concept:
#include "clang/Basic/TokenKinds.def"
#undef METAOBJECT_TRAIT
      {
        MetaobjectConcept MoC =
          translateMetaobjectKindToMetaobjectConcept(REE->getKind());
        result = makeBoolResult(Ctx, getType(), getTraitValue(MoOp, MoC));
        return true;
      }
      default: {
        llvm_unreachable("This metaobject operation does not return int value!");
      }
    }
    llvm_unreachable("Metaobject operation not implemented yet!");
  }
  return false;
}

bool UnaryMetaobjectOpExpr::hasObjResult() const {
  return getResultKind() == MOOR_Metaobject;
}

bool UnaryMetaobjectOpExpr::getObjResult(ASTContext &Ctx, void *EvlInfo,
                                         llvm::APInt &result) const {
  if (ReflexprIdExpr *REE = getArgumentReflexprIdExpr(Ctx, EvlInfo)) {
    switch (getKind()) {
#define METAOBJECT_OBJ_OP_1(S, OpRes, OpName) \
      case UMOO_##OpName: \
        result = make##OpRes##Result(Ctx, op##OpName(Ctx, REE)); \
        return true;
#include "clang/Basic/TokenKinds.def"
#undef METAOBJECT_OBJ_OP_1
      default: {
        llvm_unreachable("This metaobject operation does not return metaobject!");
      }
    }
    llvm_unreachable("Metaobject operation not implemented yet!");
  }
  return false;
}

bool UnaryMetaobjectOpExpr::hasStrResult() const {
  return getResultKind() == MOOR_String;
}

bool UnaryMetaobjectOpExpr::getStrResult(ASTContext &Ctx, UnaryMetaobjectOp Oper,
                                         ReflexprIdExpr *REE,
                                         void *EvlInfo, std::string &result) {
  assert(REE);
  switch (Oper) {
#define METAOBJECT_STR_OP_1(S, OpRes, OpName) \
    case UMOO_##OpName: \
      result = op##OpName(Ctx, REE); \
      return true;
#include "clang/Basic/TokenKinds.def"
#undef METAOBJECT_STR_OP_1
    default: {
      llvm_unreachable("This metaobject operation does not return a string!");
    }
  }
  llvm_unreachable("Metaobject operation not implemented yet!");
}

bool UnaryMetaobjectOpExpr::getStrResult(ASTContext &Ctx, void *EvlInfo,
                                         std::string &result) const {
  if (ReflexprIdExpr *REE = getArgumentReflexprIdExpr(Ctx, EvlInfo)) {
    return getStrResult(Ctx, getKind(), REE, EvlInfo, result);
  }
  return false;
}

bool UnaryMetaobjectOpExpr::hasPtrResult() const {
  return getResultKind() == MOOR_Pointer;
}

const ValueDecl *UnaryMetaobjectOpExpr::getResultValueDecl(
  ASTContext &Ctx, UnaryMetaobjectOp MoOp, ReflexprIdExpr *REE) {
  assert(REE);

  switch (MoOp) {
    case UMOO_GetPointer:
    case UMOO_GetConstant: {
      return REE->findArgumentValueDecl(Ctx);
    }
    default:
      break;
  }
  llvm_unreachable("Failed to get value declaration from metaobject!");
}

const ValueDecl *UnaryMetaobjectOpExpr::getResultValueDecl(
    ASTContext &Ctx, void *EvlInfo) const {
  if (ReflexprIdExpr *REE = getArgumentReflexprIdExpr(Ctx, EvlInfo)) {
    return getResultValueDecl(Ctx, getKind(), REE);
  }
  llvm_unreachable("Failed to query Metaobject information!");
}

bool UnaryMetaobjectOpExpr::hasOpResultType() const {
  switch (getKind()) {
  case UMOO_GetSourceLine:
  case UMOO_GetSourceColumn:
  case UMOO_GetPointer:
  case UMOO_GetConstant:
    return true;
  default:
    break;
  }
  return false;
}

QualType UnaryMetaobjectOpExpr::getValueDeclType(ASTContext &Ctx,
                                                 UnaryMetaobjectOp MoOp,
                                                 const ValueDecl *valDecl) {
  assert(valDecl != nullptr);

  QualType result;

  if (MoOp == UMOO_GetPointer) {
    if (const VarDecl *varDecl = dyn_cast<VarDecl>(valDecl)) {
      result = Ctx.getPointerType(varDecl->getType());
    } else if (const FieldDecl *fldDecl = dyn_cast<FieldDecl>(valDecl)) {
      const RecordDecl *RD = fldDecl->getParent();
      QualType RecTy = Ctx.getRecordType(RD);
      result = Ctx.getMemberPointerType(fldDecl->getType(), RecTy.getTypePtr());
    } else if (const CXXMethodDecl *mthdDecl = dyn_cast<CXXMethodDecl>(valDecl)) {
      if (mthdDecl->isStatic()) {
        result = Ctx.getPointerType(mthdDecl->getType());
      } else {
        const RecordDecl *RD = mthdDecl->getParent();
        QualType RecTy = Ctx.getRecordType(RD);
        result = Ctx.getMemberPointerType(mthdDecl->getType(), RecTy.getTypePtr());
      }
    }
  } else if (MoOp == UMOO_GetConstant) {
    result = valDecl->getType();
  }
  return result;
}

Stmt::child_range UnaryMetaobjectOpExpr::children() {
  return child_range(child_iterator(&ArgExpr + 0),
                     child_iterator(&ArgExpr + 1));
}

QualType NaryMetaobjectOpExpr::getResultKindType(ASTContext &Ctx,
                                                 NaryMetaobjectOp Oper,
                                                 MetaobjectOpResult OpRes,
                                                 unsigned arity, Expr **argExpr) {
  switch (OpRes) {
  case MOOR_Metaobject:
    return Ctx.MetaobjectIdTy;
  case MOOR_SizeT:
    return Ctx.getSizeType();
  case MOOR_ULong:
    return Ctx.UnsignedLongTy;
  case MOOR_Bool:
    return Ctx.BoolTy;
  case MOOR_Constant:
  case MOOR_Pointer:
  case MOOR_String:
    break;
  }
  llvm_unreachable("invalid n-ary metaobject operation result");
  return QualType();
}

NaryMetaobjectOpExpr::NaryMetaobjectOpExpr(ASTContext &Ctx,
                                           NaryMetaobjectOp Oper,
                                           MetaobjectOpResult OpRes,
                                           QualType resultType, unsigned arity,
                                           Expr **argExpr, SourceLocation opLoc,
                                           SourceLocation endLoc)
    : Expr(NaryMetaobjectOpExprClass, resultType, VK_PRValue, OK_Ordinary),
      OpLoc(opLoc), EndLoc(endLoc) {

  for (unsigned i = 0; i < arity; ++i) {
    ArgExpr[i] = argExpr[i];
  }
  for (unsigned i = arity; i < MaxArity; ++i) {
    ArgExpr[i] = nullptr;
  }

  setKind(Oper);
  setResultKind(OpRes);
  setDependence(computeDependence(this));
}

NaryMetaobjectOpExpr *
NaryMetaobjectOpExpr::Create(ASTContext &Ctx, NaryMetaobjectOp Oper,
                             MetaobjectOpResult OpRes,
                             unsigned arity, Expr **argExpr,
                             SourceLocation opLoc, SourceLocation endLoc) {
  QualType resType = getResultKindType(Ctx, Oper, OpRes, arity, argExpr);

  return new (Ctx) NaryMetaobjectOpExpr(Ctx, Oper, OpRes, resType,
                                        arity, argExpr, opLoc, endLoc);
}

StringRef NaryMetaobjectOpExpr::getOperationSpelling(NaryMetaobjectOp MoOp) {
  switch (MoOp) {
#define METAOBJECT_OP_2(Spelling, R, Name) \
  case NMOO_##Name: \
    return #Spelling;
#include "clang/Basic/TokenKinds.def"
#undef METAOBJECT_OP_2
  }
  return StringRef();
}

bool NaryMetaobjectOpExpr::isOperationApplicable(MetaobjectKind MoK,
                                                 NaryMetaobjectOp MoOp) {

  MetaobjectConcept MoC = translateMetaobjectKindToMetaobjectConcept(MoK);
  switch (MoOp) {
  case NMOO_ReflectsSame:
    return conceptIsA(MoC, MOC_Object);
  case NMOO_GetElement:
    return conceptIsA(MoC, MOC_ObjectSequence);
  }
  return false;
}

bool NaryMetaobjectOpExpr::opReflectsSame(ASTContext &Ctx,
                                          ReflexprIdExpr* REE1,
                                          ReflexprIdExpr* REE2) {
  if (REE1 == REE2)
    return true;

  if (REE1->isArgumentEmpty() && REE2->isArgumentEmpty())
    return true;

  if (REE1->isArgumentGlobalScope() && REE2->isArgumentGlobalScope())
    return true;

  if (REE1->isArgumentNoSpecifier() && REE2->isArgumentNoSpecifier())
    return true;

  if (REE1->isArgumentSpecifier() && REE2->isArgumentSpecifier()) {
    return REE1->getArgumentSpecifierKind() ==
           REE2->getArgumentSpecifierKind();
  }

  if (REE1->isArgumentNamedDecl() && REE2->isArgumentNamedDecl()) {
    auto ND1 = REE1->getArgumentNamedDecl();
    auto ND2 = REE2->getArgumentNamedDecl();
    if (ND1 == ND2)
      return true;
    if (ND1->getDeclName() == ND2->getDeclName()) {
      if (declaresSameEntity(
        cast<Decl>(ND1->getDeclContext()->getRedeclContext()),
        cast<Decl>(ND2->getDeclContext()->getRedeclContext()))) {
        if (ND1->getKind() == ND2->getKind()) {
          // [reflection-ts] FIXME: this will not work
          // for overloaded functions, etc.
          return ND1->getName() == ND2->getName();
        }
      }
    }
  }

  if (REE1->isArgumentType() && REE2->isArgumentType()) {
    auto Ty1 = REE1->getArgumentType();
    auto Ty2 = REE2->getArgumentType();

    if (Ctx.hasSameType(Ty1, Ty2)) {
      return true;
    }
  }
  // [reflection-ts] FIXME
  return false;
}

ReflexprIdExpr *NaryMetaobjectOpExpr::opGetElement(ASTContext &Ctx,
                                                   ReflexprIdExpr *REE,
                                                   unsigned idx) {

  findMatchingMetaobjSeqElement action(idx);
  applyOnMetaobjSeqElements(Ctx, action, REE);
  assert(action.result.decl || action.result.base || action.result.capture);

  if (REE->getSeqKind() == MOSK_BaseClasses) {
    const CXXBaseSpecifier *BS = action.result.base;
    assert(BS != nullptr);

    return ReflexprIdExpr::getBaseSpecifierReflexprIdExpr(Ctx, BS);
  } else if (REE->getSeqKind() == MOSK_Captures) {
    const LambdaCapture *LC = action.result.capture;
    assert(LC != nullptr);

    return ReflexprIdExpr::getLambdaCaptureReflexprIdExpr(Ctx, LC);
  } else {
    const NamedDecl *ND = dyn_cast<NamedDecl>(action.result.decl);
    assert(ND != nullptr);

    return ReflexprIdExpr::getNamedDeclReflexprIdExpr(Ctx, ND);
  }
}

bool NaryMetaobjectOpExpr::hasIntResult() const {
  switch (getResultKind()) {
    case MOOR_SizeT:
    case MOOR_ULong:
    case MOOR_Bool:
    case MOOR_Constant:
      return true;
    case MOOR_Pointer:
    case MOOR_String:
    case MOOR_Metaobject:
      break;
  }
  return false;
}

bool NaryMetaobjectOpExpr::getIntResult(ASTContext &Ctx, void *EvlInfo,
                                        llvm::APSInt &result) const {
  const auto opKind = getKind();
  if(opKind == NMOO_ReflectsSame) {
    ReflexprIdExpr *REE0 = getArgumentReflexprIdExpr(Ctx, 0, EvlInfo);
    ReflexprIdExpr *REE1 = getArgumentReflexprIdExpr(Ctx, 1, EvlInfo);
    if(REE0 && REE1) {
      result = makeBoolResult(Ctx, getType(), opReflectsSame(Ctx, REE0, REE1));
      return true;
    }
  }
  return false;
}

bool NaryMetaobjectOpExpr::hasObjResult() const {
  return getResultKind() == MOOR_Metaobject;
}

bool NaryMetaobjectOpExpr::getObjResult(ASTContext &Ctx, void *EvlInfo,
                                        llvm::APInt &result) const {

  const auto opKind = getKind();
  if(opKind == NMOO_GetElement) {
    ReflexprIdExpr *REE = getArgumentReflexprIdExpr(Ctx, 0, EvlInfo);
    uint64_t index;
    if (queryArgUIntValue(Ctx, index, 1)) {
      result = makeMetaobjectResult(Ctx, opGetElement(Ctx, REE, index));
      return true;
    }
  }
  return false;
}

Stmt::child_range NaryMetaobjectOpExpr::children() {
  return child_range(child_iterator(ArgExpr + 0),
                     child_iterator(ArgExpr + getArity()));
}
// [reflection-ts]

CUDAKernelCallExpr::CUDAKernelCallExpr(Expr *Fn, CallExpr *Config,
                                       ArrayRef<Expr *> Args, QualType Ty,
                                       ExprValueKind VK, SourceLocation RP,
                                       FPOptionsOverride FPFeatures,
                                       unsigned MinNumArgs)
    : CallExpr(CUDAKernelCallExprClass, Fn, /*PreArgs=*/Config, Args, Ty, VK,
               RP, FPFeatures, MinNumArgs, NotADL) {}

CUDAKernelCallExpr::CUDAKernelCallExpr(unsigned NumArgs, bool HasFPFeatures,
                                       EmptyShell Empty)
    : CallExpr(CUDAKernelCallExprClass, /*NumPreArgs=*/END_PREARG, NumArgs,
               HasFPFeatures, Empty) {}

CUDAKernelCallExpr *
CUDAKernelCallExpr::Create(const ASTContext &Ctx, Expr *Fn, CallExpr *Config,
                           ArrayRef<Expr *> Args, QualType Ty, ExprValueKind VK,
                           SourceLocation RP, FPOptionsOverride FPFeatures,
                           unsigned MinNumArgs) {
  // Allocate storage for the trailing objects of CallExpr.
  unsigned NumArgs = std::max<unsigned>(Args.size(), MinNumArgs);
  unsigned SizeOfTrailingObjects = CallExpr::sizeOfTrailingObjects(
      /*NumPreArgs=*/END_PREARG, NumArgs, FPFeatures.requiresTrailingStorage());
  void *Mem = Ctx.Allocate(sizeof(CUDAKernelCallExpr) + SizeOfTrailingObjects,
                           alignof(CUDAKernelCallExpr));
  return new (Mem)
      CUDAKernelCallExpr(Fn, Config, Args, Ty, VK, RP, FPFeatures, MinNumArgs);
}

CUDAKernelCallExpr *CUDAKernelCallExpr::CreateEmpty(const ASTContext &Ctx,
                                                    unsigned NumArgs,
                                                    bool HasFPFeatures,
                                                    EmptyShell Empty) {
  // Allocate storage for the trailing objects of CallExpr.
  unsigned SizeOfTrailingObjects = CallExpr::sizeOfTrailingObjects(
      /*NumPreArgs=*/END_PREARG, NumArgs, HasFPFeatures);
  void *Mem = Ctx.Allocate(sizeof(CUDAKernelCallExpr) + SizeOfTrailingObjects,
                           alignof(CUDAKernelCallExpr));
  return new (Mem) CUDAKernelCallExpr(NumArgs, HasFPFeatures, Empty);
}
