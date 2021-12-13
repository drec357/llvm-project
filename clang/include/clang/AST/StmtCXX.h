//===--- StmtCXX.h - Classes for representing C++ statements ----*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines the C++ statement AST node classes.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_STMTCXX_H
#define LLVM_CLANG_AST_STMTCXX_H

#include "clang/AST/DeclarationName.h"
#include "clang/AST/Expr.h"
#include "clang/AST/NestedNameSpecifier.h"
#include "clang/AST/Stmt.h"
#include "llvm/Support/Compiler.h"

namespace clang {

class VarDecl;
class SizeOfPackExpr;
class NonTypeTemplateParmDecl;

/// CXXCatchStmt - This represents a C++ catch block.
///
class CXXCatchStmt : public Stmt {
  SourceLocation CatchLoc;
  /// The exception-declaration of the type.
  VarDecl *ExceptionDecl;
  /// The handler block.
  Stmt *HandlerBlock;

public:
  CXXCatchStmt(SourceLocation catchLoc, VarDecl *exDecl, Stmt *handlerBlock)
  : Stmt(CXXCatchStmtClass), CatchLoc(catchLoc), ExceptionDecl(exDecl),
    HandlerBlock(handlerBlock) {}

  CXXCatchStmt(EmptyShell Empty)
  : Stmt(CXXCatchStmtClass), ExceptionDecl(nullptr), HandlerBlock(nullptr) {}

  SourceLocation getBeginLoc() const LLVM_READONLY { return CatchLoc; }
  SourceLocation getEndLoc() const LLVM_READONLY {
    return HandlerBlock->getEndLoc();
  }

  SourceLocation getCatchLoc() const { return CatchLoc; }
  VarDecl *getExceptionDecl() const { return ExceptionDecl; }
  QualType getCaughtType() const;
  Stmt *getHandlerBlock() const { return HandlerBlock; }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == CXXCatchStmtClass;
  }

  child_range children() { return child_range(&HandlerBlock, &HandlerBlock+1); }

  const_child_range children() const {
    return const_child_range(&HandlerBlock, &HandlerBlock + 1);
  }

  friend class ASTStmtReader;
};

/// CXXTryStmt - A C++ try block, including all handlers.
///
class CXXTryStmt final : public Stmt,
                         private llvm::TrailingObjects<CXXTryStmt, Stmt *> {

  friend TrailingObjects;
  friend class ASTStmtReader;

  SourceLocation TryLoc;
  unsigned NumHandlers;
  size_t numTrailingObjects(OverloadToken<Stmt *>) const { return NumHandlers; }

  CXXTryStmt(SourceLocation tryLoc, Stmt *tryBlock, ArrayRef<Stmt*> handlers);
  CXXTryStmt(EmptyShell Empty, unsigned numHandlers)
    : Stmt(CXXTryStmtClass), NumHandlers(numHandlers) { }

  Stmt *const *getStmts() const { return getTrailingObjects<Stmt *>(); }
  Stmt **getStmts() { return getTrailingObjects<Stmt *>(); }

public:
  static CXXTryStmt *Create(const ASTContext &C, SourceLocation tryLoc,
                            Stmt *tryBlock, ArrayRef<Stmt*> handlers);

  static CXXTryStmt *Create(const ASTContext &C, EmptyShell Empty,
                            unsigned numHandlers);

  SourceLocation getBeginLoc() const LLVM_READONLY { return getTryLoc(); }

  SourceLocation getTryLoc() const { return TryLoc; }
  SourceLocation getEndLoc() const {
    return getStmts()[NumHandlers]->getEndLoc();
  }

  CompoundStmt *getTryBlock() {
    return cast<CompoundStmt>(getStmts()[0]);
  }
  const CompoundStmt *getTryBlock() const {
    return cast<CompoundStmt>(getStmts()[0]);
  }

  unsigned getNumHandlers() const { return NumHandlers; }
  CXXCatchStmt *getHandler(unsigned i) {
    return cast<CXXCatchStmt>(getStmts()[i + 1]);
  }
  const CXXCatchStmt *getHandler(unsigned i) const {
    return cast<CXXCatchStmt>(getStmts()[i + 1]);
  }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == CXXTryStmtClass;
  }

  child_range children() {
    return child_range(getStmts(), getStmts() + getNumHandlers() + 1);
  }

  const_child_range children() const {
    return const_child_range(getStmts(), getStmts() + getNumHandlers() + 1);
  }
};

/// CXXForRangeStmt - This represents C++0x [stmt.ranged]'s ranged for
/// statement, represented as 'for (range-declarator : range-expression)'
/// or 'for (init-statement range-declarator : range-expression)'.
///
/// This is stored in a partially-desugared form to allow full semantic
/// analysis of the constituent components. The original syntactic components
/// can be extracted using getLoopVariable and getRangeInit.
class CXXForRangeStmt : public Stmt {
  SourceLocation ForLoc;
  enum { INIT, RANGE, BEGINSTMT, ENDSTMT, COND, INC, LOOPVAR, BODY, END };
  // SubExprs[RANGE] is an expression or declstmt.
  // SubExprs[COND] and SubExprs[INC] are expressions.
  Stmt *SubExprs[END];
  SourceLocation CoawaitLoc;
  SourceLocation ColonLoc;
  SourceLocation RParenLoc;

  friend class ASTStmtReader;
public:
  CXXForRangeStmt(Stmt *InitStmt, DeclStmt *Range, DeclStmt *Begin,
                  DeclStmt *End, Expr *Cond, Expr *Inc, DeclStmt *LoopVar,
                  Stmt *Body, SourceLocation FL, SourceLocation CAL,
                  SourceLocation CL, SourceLocation RPL);
  CXXForRangeStmt(EmptyShell Empty) : Stmt(CXXForRangeStmtClass, Empty) { }

  Stmt *getInit() { return SubExprs[INIT]; }
  VarDecl *getLoopVariable();
  Expr *getRangeInit();

  const Stmt *getInit() const { return SubExprs[INIT]; }
  const VarDecl *getLoopVariable() const;
  const Expr *getRangeInit() const;


  DeclStmt *getRangeStmt() { return cast<DeclStmt>(SubExprs[RANGE]); }
  DeclStmt *getBeginStmt() {
    return cast_or_null<DeclStmt>(SubExprs[BEGINSTMT]);
  }
  DeclStmt *getEndStmt() { return cast_or_null<DeclStmt>(SubExprs[ENDSTMT]); }
  Expr *getCond() { return cast_or_null<Expr>(SubExprs[COND]); }
  Expr *getInc() { return cast_or_null<Expr>(SubExprs[INC]); }
  DeclStmt *getLoopVarStmt() { return cast<DeclStmt>(SubExprs[LOOPVAR]); }
  Stmt *getBody() { return SubExprs[BODY]; }

  const DeclStmt *getRangeStmt() const {
    return cast<DeclStmt>(SubExprs[RANGE]);
  }
  const DeclStmt *getBeginStmt() const {
    return cast_or_null<DeclStmt>(SubExprs[BEGINSTMT]);
  }
  const DeclStmt *getEndStmt() const {
    return cast_or_null<DeclStmt>(SubExprs[ENDSTMT]);
  }
  const Expr *getCond() const {
    return cast_or_null<Expr>(SubExprs[COND]);
  }
  const Expr *getInc() const {
    return cast_or_null<Expr>(SubExprs[INC]);
  }
  const DeclStmt *getLoopVarStmt() const {
    return cast<DeclStmt>(SubExprs[LOOPVAR]);
  }
  const Stmt *getBody() const { return SubExprs[BODY]; }

  void setInit(Stmt *S) { SubExprs[INIT] = S; }
  void setRangeInit(Expr *E) { SubExprs[RANGE] = reinterpret_cast<Stmt*>(E); }
  void setRangeStmt(Stmt *S) { SubExprs[RANGE] = S; }
  void setBeginStmt(Stmt *S) { SubExprs[BEGINSTMT] = S; }
  void setEndStmt(Stmt *S) { SubExprs[ENDSTMT] = S; }
  void setCond(Expr *E) { SubExprs[COND] = reinterpret_cast<Stmt*>(E); }
  void setInc(Expr *E) { SubExprs[INC] = reinterpret_cast<Stmt*>(E); }
  void setLoopVarStmt(Stmt *S) { SubExprs[LOOPVAR] = S; }
  void setBody(Stmt *S) { SubExprs[BODY] = S; }

  SourceLocation getForLoc() const { return ForLoc; }
  SourceLocation getCoawaitLoc() const { return CoawaitLoc; }
  SourceLocation getColonLoc() const { return ColonLoc; }
  SourceLocation getRParenLoc() const { return RParenLoc; }

  SourceLocation getBeginLoc() const LLVM_READONLY { return ForLoc; }
  SourceLocation getEndLoc() const LLVM_READONLY {
    return SubExprs[BODY]->getEndLoc();
  }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == CXXForRangeStmtClass;
  }

  // Iterators
  child_range children() {
    return child_range(&SubExprs[0], &SubExprs[END]);
  }

  const_child_range children() const {
    return const_child_range(&SubExprs[0], &SubExprs[END]);
  }
};

/// The base class of all expansion statements.
///
/// Tuple and pack expansion statements have the following form:
///
/// \verbatim
///   for... (auto x : expandable) statement
/// \endverbatim
///
/// The "expandable" expression is a parameter pack, an array, a tuple, or
/// a constexpr range.
class CXXExpansionStmt : public Stmt {
protected:
  /// The subexpressions of an expression include the loop variable,
  /// the tuple or pack being expanded, and the loop body.
  enum {
    LOOP,  ///< The variable bound to each member of the expansion.
    RANGE, ///< The expression or captured variable being expanded.
    BODY,  ///< The uninstantiated loop body.
    END
  };
  Stmt *SubExprs[END];

  /// The template parameter list which stores the induction variable
  /// used to form the dependent structure of the loop body.
  TemplateParameterList *InductionVarTPL;

  SourceLocation TemplateForLoc;
  SourceLocation ConstexprLoc;
  SourceLocation ColonLoc;
  SourceLocation StructLoc;
  SourceLocation RParenLoc;

  /// The expansion size of the range. When the range is dependent
  /// this value is not meaningful.
  size_t NumInstantiatedStmts;

  /// The statements instantiated from the loop body. These are not
  /// sub-expressions.
  Stmt **InstantiatedStmts;

  friend class ASTStmtReader;

protected:
  CXXExpansionStmt(StmtClass SC, DeclStmt *LoopVar,
                          TemplateParameterList *InductionVarTPL, size_t N,
                          SourceLocation TFL, SourceLocation CEL,
                          SourceLocation CL, SourceLocation SL,
                          SourceLocation RPL)
    : Stmt(SC), TemplateForLoc(TFL), ConstexprLoc(CEL),
      ColonLoc(CL), StructLoc(SL), RParenLoc(RPL), NumInstantiatedStmts(N),
      InstantiatedStmts(nullptr) {
    SubExprs[LOOP] = LoopVar;
    SubExprs[RANGE] = SubExprs[BODY] = nullptr;
  }

  CXXExpansionStmt(StmtClass SC, EmptyShell Empty)
    : Stmt(SC, Empty) {}

public:
  /// Returns the dependent loop variable declaration statement.
  DeclStmt *getLoopVarStmt() const { return cast<DeclStmt>(SubExprs[LOOP]); }
  void setLoopVarStmt(Stmt *V) { SubExprs[LOOP] = V; }

  /// Returns the loop variable.
  const VarDecl *getLoopVariable() const;
  VarDecl *getLoopVariable();

  /// Returns the template parameter list storing the induction variable.
  TemplateParameterList *getInductionVarTPL() { return InductionVarTPL; }

  /// Returns loop induction variable used to form the dependent
  /// structure of the loop body.
  NonTypeTemplateParmDecl *getInductionVariable();

  /// Returns the parsed body of the loop.
  Stmt *getBody() const { return SubExprs[BODY]; }
  void setBody(Stmt *S) { SubExprs[BODY] = S; }

  /// Set the sequence of instantiated statements.
  void setInstantiatedStatements(Stmt **S) {
    assert(!InstantiatedStmts && "instantiated statements already defined");
    InstantiatedStmts = S;
  }

  /// Returns the sequence of instantiated statements.
  ArrayRef<Stmt *> getInstantiatedStatements() const {
    return llvm::makeArrayRef(InstantiatedStmts, NumInstantiatedStmts);
  }

  size_t getNumInstantiatedStatements() const { return NumInstantiatedStmts; }

  /// Returns a pointer to the first instantiated statement.
  Stmt **begin_instantiated_statements() const { return InstantiatedStmts; }
  /// Returns a pointer past the last instantiated statement.
  Stmt **end_instantiated_statements() const {
    return InstantiatedStmts + NumInstantiatedStmts;
  }

  SourceLocation getTemplateForLoc() const { return TemplateForLoc; }
  SourceLocation getConstexprLoc() const { return ConstexprLoc; }
  SourceLocation getColonLoc() const { return ColonLoc; }
  SourceLocation getStructLoc() const { return StructLoc; }
  SourceLocation getRParenLoc() const { return RParenLoc; }

  SourceLocation getBeginLoc() const LLVM_READONLY { return TemplateForLoc; }
  SourceLocation getEndLoc() const LLVM_READONLY {
    if (SubExprs[BODY])
      return SubExprs[BODY]->getEndLoc();
    return RParenLoc;
  }

  child_range children() { return child_range(&SubExprs[0], &SubExprs[END]); }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == CXXCompositeExpansionStmtClass ||
           T->getStmtClass() == CXXPackExpansionStmtClass;
  }
};

/// CXXCompositeExpansionStmt - a compile-time expansion over a single
/// declaration, which may be either a tuple, constant array, range, or,
/// if the `struct` word was prepended to the range expression, an object
/// of any class.
///
/// - - -
///
/// ***Tuples***
///
/// An object of  class T is considered a tuple so long as std::get<I>(T t)
/// and std::tuple_size<T>::value are defined.
///
/// \code
///     template for (auto x : tup) stmt;
/// \endcode
///
/// will expand as:
/// \code
///     auto&& __range = tup;
///     { // expansion-1
///       auto x = std::get<0>(__range);
///       stmt_1;
///     }
///     { // expansion-2
///       auto x = std::get<1>(__range);
///       stmt_2;
///     }
///     ... // Up to std::tuple_size_v<decltype(tup)>;
/// \endcode
///
///
/// And similarly for `template for (constexpr auto x : tup)`.
///
/// - - -
///
/// ***Ranges***
///
/// An object of class T is a "range" if T defines begin() and end()
/// methods return iterators of type U for which std::next(U, size_t) is
/// defined.  (Eventually, only a operator++ should be needed; for now,
/// non-random-access iterators will require quadratic iteration time.)
///
/// \code
///     template for (auto x : range) stmt;
/// \endcode
///
/// will expand as:
///
/// \code
///     constexpr auto&& __range = range;
///     constexpr auto __begin = begin-expr(__range)
///     constexpr auto __end = end-expr(__range)
///     { // expansion-1
///       constexpr auto x = std::next(__begin, 0);
///       stmt_1;
///     }
///     { // expansion-2
///       constexpr auto x = std::next(__begin, 1);
///       stmt_2;
///     }
///     ... // Up to std::size(range) or std:distance(range),
//          // whichever is available.
/// \endcode
///
/// FIXME: Using std::next guarantees quadratic loop performance for
/// non-random-access iterators. Either pre-compute an array (which is weird)
/// or build each expansion within a dedicated loop elsewhere.
///
/// - - -
///
/// ***Arrays***
/// A object is a considered an "array" if it is a constant array
/// type and defines an operator[](size_t).
///
/// \code
///     template for (auto x : arr) stmt;
/// \endcode
///
/// will expand as:
///
/// \code
///     { // expansion-1
///       auto x = arr[0];
///       stmt_1;
///     }
///     { // expansion-2
///       auto x = arr[1];
///       stmt_2;
///     }
///     ... // Up to std::extent_v<decltype(arr)>.
/// \endcode
///
/// And similarly for constexpr loops.
///
/// - - -
///
/// ***Classes***
/// For itertating over a class's accessible fields (as if
/// they were stored in a tuple), prepend the range variable
/// name with the struct keyword.
///
/// \code
///     class MyClass {
///     public:
///       int i;
///     private:
///       char priv;
///     public:
///        float f;
///     } obj;
///     for constexpr (auto x : struct obj) stmt;
/// \endcode
///
/// will expand as:
///
/// \code
///     constexpr auto&& __range = obj;
///     { // expansion-1
///       constexpr auto x = __select(obj, 0); // = obj.i
///       stmt_1;
///     }
///     { // expansion-2
///       constexpr auto x = __select(obj, 1); // = obj.f
///       stmt_2;
///     }
///     ... // Up to the number of accessible members in the class. (= 2)
/// \endcode
///
/// The range-index is a precomputed list of iterators, maintained internally
/// by the compiler. The __select intrinsic (CXXSelectMemberExpr)
/// returns the Ith such iterator in the sequence.
///
class CXXCompositeExpansionStmt final : public CXXExpansionStmt {
  /// The statement declaring  __begin.
  Stmt *BeginStmt = nullptr;

  /// The statement declaring __end.
  Stmt *EndStmt = nullptr;

  CXXCompositeExpansionStmt(DeclStmt *LoopVar, DeclStmt *RangeVar,
                             TemplateParameterList *InductionVarTPL, size_t N,
                             SourceLocation TFL, SourceLocation CEL,
                             SourceLocation CL, SourceLocation SL,
                             SourceLocation RPL)
    : CXXExpansionStmt(CXXCompositeExpansionStmtClass, LoopVar,
                              InductionVarTPL, N, TFL, CEL, CL, SL, RPL) {
    SubExprs[RANGE] = RangeVar;
  }

  CXXCompositeExpansionStmt(EmptyShell Empty)
    : CXXExpansionStmt(CXXCompositeExpansionStmtClass, Empty) {}

public:
  static CXXCompositeExpansionStmt *Create(
        ASTContext &Context, DeclStmt *LoopVar, DeclStmt *RangeVar,
        TemplateParameterList *InductionVarTPL, std::size_t N,
        SourceLocation TFL, SourceLocation CEL, SourceLocation CL,
        SourceLocation SL, SourceLocation RPL);

  static CXXCompositeExpansionStmt *Create(ASTContext &Context,
                                            EmptyShell Empty);

  /// Returns the statement containing the range declaration.
  const DeclStmt *getRangeStmt() const {
    return cast<DeclStmt>(SubExprs[RANGE]);
  }
  DeclStmt *getRangeStmt() { return cast<DeclStmt>(SubExprs[RANGE]); }
  void setRangeStmt(Stmt *V) { SubExprs[RANGE] = V; }

  /// The range variable.
  const VarDecl *getRangeVariable() const;
  VarDecl *getRangeVariable();

  /// The original range expression.
  const Expr *getRangeInit() const { return getRangeVariable()->getInit(); }
  Expr *getRangeInit() { return getRangeVariable()->getInit(); }

  /// Accesses the declaration statements of __begin and __end respectively.
  /// These are only relevant for range expansions.
  const Stmt *getBeginStmt() const { return BeginStmt; }
  Stmt *getBeginStmt() { return BeginStmt; }
  void setBeginStmt(Stmt *B) { BeginStmt = B; }

  const Stmt *getEndStmt() const { return EndStmt; }
  Stmt *getEndStmt() { return EndStmt; }
  void setEndStmt(Stmt *E) { EndStmt = E; }

  SourceLocation getBeginLoc() const LLVM_READONLY { return TemplateForLoc; }
  SourceLocation getEndLoc() const LLVM_READONLY {
    if (SubExprs[BODY])
      return SubExprs[BODY]->getEndLoc();
    return RParenLoc;
  }

  child_range children() { return child_range(&SubExprs[0], &SubExprs[END]); }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == CXXCompositeExpansionStmtClass;
  }
};

/// CXXPackExpansionStmt - a compile-time expansion over an
/// expression pack.
///
/// \verbatim
///   template for (auto x : exprpack) statement
///   template for (constexpr auto x : exprpack) statement
/// \endverbatim
///
class CXXPackExpansionStmt final : public CXXExpansionStmt {

  CXXPackExpansionStmt(DeclStmt *LoopVar, Expr *RangeExpr,
                         TemplateParameterList *Parms, size_t N,
                         SourceLocation TFL, SourceLocation CEL,
                         SourceLocation CL, SourceLocation RPL)
    : CXXExpansionStmt(CXXPackExpansionStmtClass, LoopVar, Parms,
                         N, TFL, CEL, CL, /*StructLoc=*/SourceLocation(), RPL) {
    SubExprs[RANGE] = RangeExpr;
  }

  CXXPackExpansionStmt(EmptyShell Empty)
    : CXXExpansionStmt(CXXPackExpansionStmtClass, Empty) {}

public:
  static CXXPackExpansionStmt *Create(
      ASTContext &Context, DeclStmt *LoopVar, Expr *RangeExpr,
      TemplateParameterList *InductionVarTPL, std::size_t N,
      SourceLocation TFL, SourceLocation CEL,
      SourceLocation CL, SourceLocation RPL);

  static CXXPackExpansionStmt *Create(ASTContext &Context,
                                        EmptyShell Empty);

  // Returns the range expression as a statement.
  Stmt *getRangeStmt() const { return SubExprs[RANGE]; }
  void setRangeExprStmt(Stmt *V) { SubExprs[RANGE] = V; }

  /// Returns the range expression containing the pack.
  Expr *getRangeExpr() const;

  SourceLocation getBeginLoc() const LLVM_READONLY { return TemplateForLoc; }
  SourceLocation getEndLoc() const LLVM_READONLY {
    if (SubExprs[BODY])
      return SubExprs[BODY]->getEndLoc();
    return RParenLoc;
  }

  child_range children() { return child_range(&SubExprs[0], &SubExprs[END]); }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == CXXPackExpansionStmtClass;
  }
};

/// Representation of a Microsoft __if_exists or __if_not_exists
/// statement with a dependent name.
///
/// The __if_exists statement can be used to include a sequence of statements
/// in the program only when a particular dependent name does not exist. For
/// example:
///
/// \code
/// template<typename T>
/// void call_foo(T &t) {
///   __if_exists (T::foo) {
///     t.foo(); // okay: only called when T::foo exists.
///   }
/// }
/// \endcode
///
/// Similarly, the __if_not_exists statement can be used to include the
/// statements when a particular name does not exist.
///
/// Note that this statement only captures __if_exists and __if_not_exists
/// statements whose name is dependent. All non-dependent cases are handled
/// directly in the parser, so that they don't introduce a new scope. Clang
/// introduces scopes in the dependent case to keep names inside the compound
/// statement from leaking out into the surround statements, which would
/// compromise the template instantiation model. This behavior differs from
/// Visual C++ (which never introduces a scope), but is a fairly reasonable
/// approximation of the VC++ behavior.
class MSDependentExistsStmt : public Stmt {
  SourceLocation KeywordLoc;
  bool IsIfExists;
  NestedNameSpecifierLoc QualifierLoc;
  DeclarationNameInfo NameInfo;
  Stmt *SubStmt;

  friend class ASTReader;
  friend class ASTStmtReader;

public:
  MSDependentExistsStmt(SourceLocation KeywordLoc, bool IsIfExists,
                        NestedNameSpecifierLoc QualifierLoc,
                        DeclarationNameInfo NameInfo,
                        CompoundStmt *SubStmt)
  : Stmt(MSDependentExistsStmtClass),
    KeywordLoc(KeywordLoc), IsIfExists(IsIfExists),
    QualifierLoc(QualifierLoc), NameInfo(NameInfo),
    SubStmt(reinterpret_cast<Stmt *>(SubStmt)) { }

  /// Retrieve the location of the __if_exists or __if_not_exists
  /// keyword.
  SourceLocation getKeywordLoc() const { return KeywordLoc; }

  /// Determine whether this is an __if_exists statement.
  bool isIfExists() const { return IsIfExists; }

  /// Determine whether this is an __if_exists statement.
  bool isIfNotExists() const { return !IsIfExists; }

  /// Retrieve the nested-name-specifier that qualifies this name, if
  /// any.
  NestedNameSpecifierLoc getQualifierLoc() const { return QualifierLoc; }

  /// Retrieve the name of the entity we're testing for, along with
  /// location information
  DeclarationNameInfo getNameInfo() const { return NameInfo; }

  /// Retrieve the compound statement that will be included in the
  /// program only if the existence of the symbol matches the initial keyword.
  CompoundStmt *getSubStmt() const {
    return reinterpret_cast<CompoundStmt *>(SubStmt);
  }

  SourceLocation getBeginLoc() const LLVM_READONLY { return KeywordLoc; }
  SourceLocation getEndLoc() const LLVM_READONLY {
    return SubStmt->getEndLoc();
  }

  child_range children() {
    return child_range(&SubStmt, &SubStmt+1);
  }

  const_child_range children() const {
    return const_child_range(&SubStmt, &SubStmt + 1);
  }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == MSDependentExistsStmtClass;
  }
};

/// Represents the body of a coroutine. This wraps the normal function
/// body and holds the additional semantic context required to set up and tear
/// down the coroutine frame.
class CoroutineBodyStmt final
    : public Stmt,
      private llvm::TrailingObjects<CoroutineBodyStmt, Stmt *> {
  enum SubStmt {
    Body,          ///< The body of the coroutine.
    Promise,       ///< The promise statement.
    InitSuspend,   ///< The initial suspend statement, run before the body.
    FinalSuspend,  ///< The final suspend statement, run after the body.
    OnException,   ///< Handler for exceptions thrown in the body.
    OnFallthrough, ///< Handler for control flow falling off the body.
    Allocate,      ///< Coroutine frame memory allocation.
    Deallocate,    ///< Coroutine frame memory deallocation.
    ReturnValue,   ///< Return value for thunk function: p.get_return_object().
    ResultDecl,    ///< Declaration holding the result of get_return_object.
    ReturnStmt,    ///< Return statement for the thunk function.
    ReturnStmtOnAllocFailure, ///< Return statement if allocation failed.
    FirstParamMove ///< First offset for move construction of parameter copies.
  };
  unsigned NumParams;

  friend class ASTStmtReader;
  friend class ASTReader;
  friend TrailingObjects;

  Stmt **getStoredStmts() { return getTrailingObjects<Stmt *>(); }

  Stmt *const *getStoredStmts() const { return getTrailingObjects<Stmt *>(); }

public:

  struct CtorArgs {
    Stmt *Body = nullptr;
    Stmt *Promise = nullptr;
    Expr *InitialSuspend = nullptr;
    Expr *FinalSuspend = nullptr;
    Stmt *OnException = nullptr;
    Stmt *OnFallthrough = nullptr;
    Expr *Allocate = nullptr;
    Expr *Deallocate = nullptr;
    Expr *ReturnValue = nullptr;
    Stmt *ResultDecl = nullptr;
    Stmt *ReturnStmt = nullptr;
    Stmt *ReturnStmtOnAllocFailure = nullptr;
    ArrayRef<Stmt *> ParamMoves;
  };

private:

  CoroutineBodyStmt(CtorArgs const& Args);

public:
  static CoroutineBodyStmt *Create(const ASTContext &C, CtorArgs const &Args);
  static CoroutineBodyStmt *Create(const ASTContext &C, EmptyShell,
                                   unsigned NumParams);

  bool hasDependentPromiseType() const {
    return getPromiseDecl()->getType()->isDependentType();
  }

  /// Retrieve the body of the coroutine as written. This will be either
  /// a CompoundStmt or a TryStmt.
  Stmt *getBody() const {
    return getStoredStmts()[SubStmt::Body];
  }

  Stmt *getPromiseDeclStmt() const {
    return getStoredStmts()[SubStmt::Promise];
  }
  VarDecl *getPromiseDecl() const {
    return cast<VarDecl>(cast<DeclStmt>(getPromiseDeclStmt())->getSingleDecl());
  }

  Stmt *getInitSuspendStmt() const {
    return getStoredStmts()[SubStmt::InitSuspend];
  }
  Stmt *getFinalSuspendStmt() const {
    return getStoredStmts()[SubStmt::FinalSuspend];
  }

  Stmt *getExceptionHandler() const {
    return getStoredStmts()[SubStmt::OnException];
  }
  Stmt *getFallthroughHandler() const {
    return getStoredStmts()[SubStmt::OnFallthrough];
  }

  Expr *getAllocate() const {
    return cast_or_null<Expr>(getStoredStmts()[SubStmt::Allocate]);
  }
  Expr *getDeallocate() const {
    return cast_or_null<Expr>(getStoredStmts()[SubStmt::Deallocate]);
  }
  Expr *getReturnValueInit() const {
    return cast<Expr>(getStoredStmts()[SubStmt::ReturnValue]);
  }
  Stmt *getResultDecl() const { return getStoredStmts()[SubStmt::ResultDecl]; }
  Stmt *getReturnStmt() const { return getStoredStmts()[SubStmt::ReturnStmt]; }
  Stmt *getReturnStmtOnAllocFailure() const {
    return getStoredStmts()[SubStmt::ReturnStmtOnAllocFailure];
  }
  ArrayRef<Stmt const *> getParamMoves() const {
    return {getStoredStmts() + SubStmt::FirstParamMove, NumParams};
  }

  SourceLocation getBeginLoc() const LLVM_READONLY {
    return getBody() ? getBody()->getBeginLoc()
                     : getPromiseDecl()->getBeginLoc();
  }
  SourceLocation getEndLoc() const LLVM_READONLY {
    return getBody() ? getBody()->getEndLoc() : getPromiseDecl()->getEndLoc();
  }

  child_range children() {
    return child_range(getStoredStmts(),
                       getStoredStmts() + SubStmt::FirstParamMove + NumParams);
  }

  const_child_range children() const {
    return const_child_range(getStoredStmts(), getStoredStmts() +
                                                   SubStmt::FirstParamMove +
                                                   NumParams);
  }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == CoroutineBodyStmtClass;
  }
};

/// Represents a 'co_return' statement in the C++ Coroutines TS.
///
/// This statament models the initialization of the coroutine promise
/// (encapsulating the eventual notional return value) from an expression
/// (or braced-init-list), followed by termination of the coroutine.
///
/// This initialization is modeled by the evaluation of the operand
/// followed by a call to one of:
///   <promise>.return_value(<operand>)
///   <promise>.return_void()
/// which we name the "promise call".
class CoreturnStmt : public Stmt {
  SourceLocation CoreturnLoc;

  enum SubStmt { Operand, PromiseCall, Count };
  Stmt *SubStmts[SubStmt::Count];

  bool IsImplicit : 1;

  friend class ASTStmtReader;
public:
  CoreturnStmt(SourceLocation CoreturnLoc, Stmt *Operand, Stmt *PromiseCall,
               bool IsImplicit = false)
      : Stmt(CoreturnStmtClass), CoreturnLoc(CoreturnLoc),
        IsImplicit(IsImplicit) {
    SubStmts[SubStmt::Operand] = Operand;
    SubStmts[SubStmt::PromiseCall] = PromiseCall;
  }

  CoreturnStmt(EmptyShell) : CoreturnStmt({}, {}, {}) {}

  SourceLocation getKeywordLoc() const { return CoreturnLoc; }

  /// Retrieve the operand of the 'co_return' statement. Will be nullptr
  /// if none was specified.
  Expr *getOperand() const { return static_cast<Expr*>(SubStmts[Operand]); }

  /// Retrieve the promise call that results from this 'co_return'
  /// statement. Will be nullptr if either the coroutine has not yet been
  /// finalized or the coroutine has no eventual return type.
  Expr *getPromiseCall() const {
    return static_cast<Expr*>(SubStmts[PromiseCall]);
  }

  bool isImplicit() const { return IsImplicit; }
  void setIsImplicit(bool value = true) { IsImplicit = value; }

  SourceLocation getBeginLoc() const LLVM_READONLY { return CoreturnLoc; }
  SourceLocation getEndLoc() const LLVM_READONLY {
    return getOperand() ? getOperand()->getEndLoc() : getBeginLoc();
  }

  child_range children() {
    if (!getOperand())
      return child_range(SubStmts + SubStmt::PromiseCall,
                         SubStmts + SubStmt::Count);
    return child_range(SubStmts, SubStmts + SubStmt::Count);
  }

  const_child_range children() const {
    if (!getOperand())
      return const_child_range(SubStmts + SubStmt::PromiseCall,
                               SubStmts + SubStmt::Count);
    return const_child_range(SubStmts, SubStmts + SubStmt::Count);
  }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == CoreturnStmtClass;
  }
};

/// Represents an __inj or __injf statement
/// (enabled via LangOpts.StringInjection).
///
/// When evaluated, queues the new source code to be
/// lexed and processed at the end of the enclosing
/// MetaprogramDecl (or the MetaprogramDecl enclosing the
/// CallExpr calling some consteval function which
/// contains the __inj/__injf statement).
///
/// String literal and integer literal arguments are
/// both acceptable for all but the first arg of
/// __injf, which must be a string literal.
///
/// Example:
/// \code
///   consteval f() {
//      __inj("int bar = 4;");
//      __injf("int {} = {};", "baz", 42);
//    }
///   consteval { // (MetaprogramDecl)
///     const char *name = "foo";
///     __inj("int {} = {};", name, "34");
///     //assert(foo==34); //ERROR
///     f();
///   } // queued injections performed here when non-dependent...
///   assert(foo==34);
///   assert(bar==4);
///   assert(baz==42);
/// \endcode
class StringInjectionStmt final :
    public StmtWithKWAndArbitraryParenExprArgs<StringInjectionStmt> {

  using ImplBase = StmtWithKWAndArbitraryParenExprArgs<StringInjectionStmt>;
  template<typename, typename>
  friend class HasKWAndArbitraryParenExprArgs;

  /// Only nonnull if isSpelledWithF.  If so, this is the original 
  /// first argument, which we will have subdivided, removing the
  /// placeholders, into Args.
  StringLiteral *WrittenFirstArg;

  StringInjectionStmt(SourceLocation KeywordLoc, SourceLocation LParenLoc,
                      ArrayRef<Expr *> Args, SourceLocation RParenLoc,
                      StringLiteral *WrittenFirstArg)
    : ImplBase(KeywordLoc, LParenLoc, Args, RParenLoc,
               StringInjectionStmtClass), WrittenFirstArg(WrittenFirstArg) {}

  StringInjectionStmt(EmptyShell Empty, unsigned NumArgs)
    : ImplBase(NumArgs, StringInjectionStmtClass, Empty) {}

public:
  static StringInjectionStmt *Create(ASTContext &C,
                                     SourceLocation KeywordLoc,
                                     SourceLocation LParenLoc,
                                     ArrayRef<Expr *> Args,
                                     SourceLocation RParenLoc,
                                     StringLiteral *WrittenFirstArg) {
    return ImplBase::Create(C, KeywordLoc, LParenLoc,
                            Args, RParenLoc,
                            WrittenFirstArg);
  }

  static StringInjectionStmt *CreateEmpty(const ASTContext &C,
                                          unsigned NumArgs) {
    return ImplBase::CreateEmpty(C, NumArgs);
  }

  /// If true, the keyword used was __injf; if false, __inj.
  /// If false, Args represents the original written arguments.
  /// If true, getWrittenFirstArg() represents the written first
  /// arg, and Args alternates between sub-strings
  /// of WrittenFirstArg, divided at the placeholders,
  /// and the original remaining args in order.
  bool isSpelledWithF() const { return WrittenFirstArg; }

  /// Returns null if the keyword used was __inj; otherwise
  /// returns the string literal first argument of the __injf
  /// statement.
  StringLiteral *getWrittenFirstArg() const {
    return WrittenFirstArg;
  }
  void setWrittenFirstArg(StringLiteral *v) { WrittenFirstArg = v; }

  // Returns the string that will be interpreted as a placeholder
  // for __injf statements.
  static StringRef PlaceholderStr() { return StringRef("{}", 2); };

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == StringInjectionStmtClass;
  }
};

}  // end namespace clang

#endif
