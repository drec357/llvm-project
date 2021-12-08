//===----- SemaMetaprogram.cpp - (C++) consteval {...} Sema implems ------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Implements semantics for metaprograms and for various
// non-reflection metaprogramming expressions/statements
//
//===----------------------------------------------------------------------===//


#include "clang/AST/ASTLambda.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Basic/MetaprogramContext.h"
#include "clang/Parse/Parser.h"
#include "clang/Parse/RAIIObjectsForParser.h"
#include "clang/Sema/SemaInternal.h"
#include "clang/Sema/ScopeInfo.h"

using namespace clang;
using namespace sema;

// FIXME: it would be nice to add some sugar indicating the effect of the
// metaprogram on the AST.  So:
// Declarations introduced via an injection statement should store
// a pointer to that statement (as sugar).  And, injection statements should
// store a pointer to their parent metaprogram.  Then, you should have a
// method that tests whether a declaration was introduced by a given
// metaprogramdecl.
// Oh and MetaprogramDecls should have an instantiatedFrom MetaprogramDecl
// field too, if they do not already.

/*-----------------------------------------------------------------*/
//  Sema MetaprogramDecl implems (ActOn, Evaluate, etc.)

/// Returns true if a metaprogram-declaration in declaration context DC
/// would be represented using a function (vs. a lambda).
static inline bool NeedsFunctionRepresentation(DeclContext *DC) {
  return (DC->isFileContext() || DC->isRecord());
}

/// Create a metaprogram-declaration to hold the content of
/// the metaprogram.
///
/// \p ScopeFlags is set to the value that should be used to create the scope
/// containing the metaprogram-declaration body.
Decl *Sema::ActOnMetaprogramDecl(Scope *S, SourceLocation ConstevalLoc,
                                 unsigned &ScopeFlags,
                                 const MetaprogramContext &MetaCtx,
                                 bool Nested) {
  MetaprogramDecl *MpD;

  // Note: we'll handle pushing function scopes here, since
  // the lambda representation seems to need it here, but we'll
  // defer PushExpressionEvaluationContext until ActOnStartMetaprogramDecl.
  if (NeedsFunctionRepresentation(CurContext)) {
    assert(!MetaCtx.isFunctionContext());
    ScopeFlags = Scope::FnScope | Scope::DeclScope;

    PushFunctionScope();

    // Build the function
    //
    //  consteval void __metaprogdef() compound-statement
    //
    // where compound-statement is the body of the
    // metaprogram-declaration.
    IdentifierInfo *II = &PP.getIdentifierTable().get("__metaprogdef");
    DeclarationName Name(II);
    DeclarationNameInfo NameInfo(Name, ConstevalLoc);

    FunctionProtoType::ExtProtoInfo EPI(
        Context.getDefaultCallingConvention(/*IsVariadic=*/false,
                                            /*IsCXXMethod=*/false));
    QualType FunctionTy = Context.getFunctionType(Context.VoidTy, None, EPI);
    TypeSourceInfo *FunctionTyInfo =
        Context.getTrivialTypeSourceInfo(FunctionTy);

    FunctionDecl *Function =
        FunctionDecl::Create(Context, CurContext, ConstevalLoc, NameInfo,
                             FunctionTy, FunctionTyInfo, SC_None,
                             /*UsesFPIntrin=*/false,
                             /*isInlineSpecified=*/false,
                             /*hasWrittenPrototype=*/true,
                             ConstexprSpecKind::Consteval,
                             /*TrailingRequiresClause=*/nullptr);

    Function->setImplicit();
    Function->setIsMetaprogram();

    // Build the metaprogram declaration around the function.
    MpD = MetaprogramDecl::Create(Context, CurContext, ConstevalLoc,
                                  MetaCtx, Function);

  } else if (CurContext->isFunctionOrMethod() || Nested) {
    assert(MetaCtx.isFunctionContext());
    ScopeFlags = Scope::BlockScope | Scope::FnScope | Scope::DeclScope;
    LambdaScopeInfo *LSI = PushLambdaScope();

    // Build the expression
    //
    //    []() -> void compound-statement
    //
    // where compound-statement is the as-of-yet parsed body of the
    // metaprogram-declaration. Note that the return type is not deduced (it
    // doesn't need to be).
    //
    // TODO: It would be great if we could only capture constexpr declarations,
    // but C++ doesn't have a constexpr default.
    const bool KnownDependent = S->getTemplateParamParent();

    FunctionProtoType::ExtProtoInfo EPI(
        Context.getDefaultCallingConvention(/*IsVariadic=*/false,
                                            /*IsCXXMethod=*/true));
    EPI.HasTrailingReturn = true;
    EPI.TypeQuals.addConst();
    QualType MethodTy = Context.getFunctionType(Context.VoidTy, None, EPI);
    TypeSourceInfo *MethodTyInfo = Context.getTrivialTypeSourceInfo(MethodTy);

    LambdaIntroducer Intro;
    Intro.Range = SourceRange(ConstevalLoc);
    // ByRef is sufficient, as this will be called just after it is defined,
    // so no issue with referenced temporaries being out of scope at call time:
    Intro.Default = LCD_ByRef;

    CXXRecordDecl *Closure = createLambdaClosureType(
        Intro.Range, MethodTyInfo, KnownDependent, Intro.Default);

    CXXMethodDecl *Method =
        startLambdaDefinition(Closure, Intro.Range, MethodTyInfo, ConstevalLoc,
                              None, ConstexprSpecKind::Consteval,
                              /*TrailingRequiresClause=*/nullptr);
    buildLambdaScope(LSI, Method, Intro.Range, Intro.Default, Intro.DefaultLoc,
                     /*ExplicitParams=*/false,
                     /*ExplicitResultType=*/true,
                     /*Mutable=*/false);
    Method->setIsMetaprogram();

    // NOTE: The call operator is not yet attached to the closure type. That
    // happens in ActOnFinishMetaprogramDecl(). The operator is, however,
    // available in the LSI.
    MpD = MetaprogramDecl::Create(Context, CurContext, ConstevalLoc,
                                  MetaCtx, Closure);
  } else {
    Decl::castFromDeclContext(CurContext)->dump();
    llvm_unreachable("metaprogram declaration in unsupported context");
  }

  CurContext->addDecl(MpD);
  return MpD;
}

/// Called just prior to parsing the body of a metaprogram-declaration.
///
/// This ensures that the declaration context is pushed with the appropriate
/// scope.
void Sema::ActOnStartMetaprogramDecl(Scope *S, Decl *D) {
  MetaprogramDecl *MpD = cast<MetaprogramDecl>(D);
  if (MpD->hasFunctionRepresentation()) {
    if (S)
      PushDeclContext(S, MpD->getImplicitFunctionDecl());
    else
      CurContext = MpD->getImplicitFunctionDecl();
  } else {
    LambdaScopeInfo *LSI = cast<LambdaScopeInfo>(FunctionScopes.back());
    if (S)
      PushDeclContext(S, LSI->CallOperator);
    else
      CurContext = LSI->CallOperator;
  }
  // We already handled PushFunctionScope etc.
  // in ActOnMetaprogramDecl.  All that remains to do
  // is PushExpressionEvaluationContext:
  // FIXME should this be EM_ConstantExpression?
  PushExpressionEvaluationContext(
      ExpressionEvaluationContext::PotentiallyEvaluated);
}

/// Called immediately after parsing the body of a metaprogram-declaration.
///
/// The statements within the body are evaluated here.
void Sema::ActOnFinishMetaprogramDecl(Scope *S, Decl *D, Stmt *Body) {
  MetaprogramDecl *MpD = cast<MetaprogramDecl>(D);

  if (MpD->hasFunctionRepresentation()) {
    FunctionDecl *Fn = MpD->getImplicitFunctionDecl();
    ActOnFinishFunctionBody(Fn, Body);
    if (!CurContext->isDependentContext()) {
      assert(!MpD->isDependent());
      ParserBrickWallRAII SavedParserState(getParser());
      EvaluateMetaprogramDecl(MpD, Fn);
      if (cast<Decl>(CurContext)->isInvalidDecl())
        SavedParserState.setInvalid();
      MpD->setAlreadyRun(true);
    } else {
      MpD->setDependent();
      assert(!MpD->alreadyRun());
      // Tell the owning class whether this metaprogram contains code injection
      // statements.
      if (Fn->isCodeInjectingMetafunction())
        cast<CXXRecordDecl>(CurContext)->setHasDependentCodeInjectingMetaprograms();
    }
  } else {
    ExprResult Lambda = ActOnLambdaExpr(MpD->getLocation(), Body, S);
    CXXMethodDecl *Fn = MpD->getImplicitClosureCallOperator();
    if (!CurContext->isDependentContext()) {
      assert(!MpD->isDependent());
      ParserBrickWallRAII SavedParserState(getParser());
      EvaluateMetaprogramDecl(MpD, Lambda.get());
      if (cast<Decl>(CurContext)->isInvalidDecl())
        SavedParserState.setInvalid();
      MpD->setAlreadyRun(true);
    } else {
      MpD->setDependent();
      assert(!MpD->alreadyRun());
      // Tell the owning function whether this metaprogram contains code
      // injection statements.
      if (Fn->isCodeInjectingMetafunction())
        cast<FunctionDecl>(CurContext)->setHasDependentCodeInjectingMetaprograms();
      MpD->setImplicitLambdaExpr(Lambda.get());
    }
  }

  // If we didn't have a scope when building this, we need to restore the
  // current context.
  if (!S)
    CurContext = MpD->getDeclContext();
}

/// Called when an error occurs while parsing the metaprogram-declaration body.
void Sema::ActOnMetaprogramDeclError(Scope *S, Decl *D) {
  MetaprogramDecl *MpD = cast<MetaprogramDecl>(D);
  MpD->setInvalidDecl();
  if (MpD->hasFunctionRepresentation()) {
    ActOnFinishFunctionBody(MpD->getImplicitFunctionDecl(), nullptr);
  } else {
    ActOnLambdaError(MpD->getLocation(), S);
  }
  MpD->setAlreadyRun(true);
}

/// Evaluate a metaprogram-declaration.
///
/// This takes an unnamed consteval void function whose body is that of
/// the metaprogram-declaration, and evaluates a call to that function.
bool Sema::EvaluateMetaprogramDecl(MetaprogramDecl *MpD,
                                   FunctionDecl *D) {
  QualType FunctionTy = D->getType();
  DeclRefExpr *Ref =
      new (Context) DeclRefExpr(Context, D,
                                /*RefersToEnclosingVariableOrCapture=*/false,
                                FunctionTy, VK_LValue, SourceLocation());

  QualType PtrTy = Context.getPointerType(FunctionTy);
  ImplicitCastExpr *Cast =
      ImplicitCastExpr::Create(Context, PtrTy, CK_FunctionToPointerDecay, Ref,
                               /*BasePath=*/nullptr, VK_PRValue,
                               FPOptionsOverride());

  CallExpr *Call =
      CallExpr::Create(Context, Cast, ArrayRef<Expr *>(), Context.VoidTy,
                       VK_PRValue, SourceLocation(), FPOptionsOverride());

  return EvaluateMetaprogramDeclCall(MpD, Call);
}

/// Evaluate a metaprogram-declaration.
///
/// This builds an unnamed consteval lambda whose body is that of
/// the metaprogram-declaration, and evaluates a call to that lambda.
bool Sema::EvaluateMetaprogramDecl(MetaprogramDecl *MpD,
                                   Expr *E) {
  LambdaExpr *Lambda = cast<LambdaExpr>(E);
  CXXMethodDecl *Method = Lambda->getCallOperator();
  QualType MethodTy = Method->getType();
  DeclRefExpr *Ref = new (Context)
      DeclRefExpr(Context, Method,
                  /*RefersToEnclosingVariableOrCapture=*/false,
                  MethodTy, VK_LValue, SourceLocation());
  QualType PtrTy = Context.getPointerType(MethodTy);
  ImplicitCastExpr *Cast =
      ImplicitCastExpr::Create(Context, PtrTy, CK_FunctionToPointerDecay, Ref,
                               /*BasePath=*/nullptr, VK_PRValue,
                               FPOptionsOverride());
  CallExpr *Call = CXXOperatorCallExpr::Create(Context, OO_Call,
                                               Cast, {Lambda},
                                               Context.VoidTy,
                                               VK_PRValue,
                                               SourceLocation(),
                                               FPOptions());
  return EvaluateMetaprogramDeclCall(MpD, Call);
}

/// Evaluate the CallExpr referring to the metaprogram.
///
/// \returns  \c true if the expression \p E can be evaluated, \c false
///           otherwise.
///
bool Sema::EvaluateMetaprogramDeclCall(MetaprogramDecl *MpD,
                                       CallExpr *Call) {
  // Associate the call expression with the declaration.
  MpD->setImplicitCallExpr(Call);

  SmallVector<PartialDiagnosticAt, 8> Notes;

  Expr::EvalResult Result;
  Result.Diag = &Notes;

  SmallVector<const StringLiteral *, 16> StringInjectionChunks;
  Result.StringInjectionChunks = &StringInjectionChunks;

  assert(Call->getType()->isVoidType());
  if (!Call->EvaluateAsVoid(Result, Context)) {

    // Sometimes we may have a fold failure without other errors,
    // due to e.g. the condition on a non-constexpr if encountering
    // an error.  We want an error to stop the program in such cases.
    // TODO if we could check if other errors had already been
    // raised, we should avoid this error in such cases, as it's
    // redundant.
    if (Notes.empty())
      Diag(MpD->getEndLoc(), diag::err_metaprogram_eval_failure);
    else {
      // If we got a compiler error, then just emit that.
      if (Notes[0].second.getDiagID() == diag::err_user_defined_error)
        Diag(MpD->getBeginLoc(), Notes[0].second);
      else {
        Diag(MpD->getEndLoc(), diag::err_metaprogram_eval_failure);
        for (const PartialDiagnosticAt &Note : Notes)
          Diag(Note.first, Note.second);
      }
    }
  }

  SourceLocation POI = MpD->getSourceRange().getEnd();

  // Perform injected string parsing if necessary:
  if (!StringInjectionChunks.empty()) {
    ParserBrickWallRAII SavedParserState(getParser());
    InjectQueuedStrings(POI, StringInjectionChunks, MpD);
    if (Decl::castFromDeclContext(CurContext)->isInvalidDecl())
      SavedParserState.setInvalid(); //disables certain checks on destruction
  }

  // FIXME: Do we really want to remove the metaprogram after evaluation? Or
  // should we just mark it completed?
  MpD->getDeclContext()->removeDecl(MpD);

  return !Notes.empty();
}

StmtResult Sema::ActOnStringInjectionStmt(SourceLocation KeywordLoc,
                                          SourceLocation LParenLoc,
                                          ArrayRef<Expr *> Args,
                                          SourceLocation RParenLoc,
                                          bool AnyDependent,
                                          StringLiteral *WrittenFirstArg) {
  if (!AnyDependent && !PrepareStrIntArgsForEval(Args))
    return StmtError();
  cast<FunctionDecl>(CurContext)->setIsCodeInjectingMetafunction(true);
  return StringInjectionStmt::Create(Context, KeywordLoc, LParenLoc,
                                       Args, RParenLoc, WrittenFirstArg);
}

void Sema::TheParserEnterScope(unsigned int ScopeFlags) {
  TheParser->EnterScope(ScopeFlags);
}

void Sema::TheParserExitScope() {
  TheParser->ExitScope();
}

static DeclSpec::TST getTypeSpecForTagTypeKind(TagTypeKind Kind) {
  switch (Kind) {
    case TTK_Class:     return TST_class;
    case TTK_Enum:      return TST_enum;
    case TTK_Struct:    return TST_struct;
    case TTK_Interface: return TST_interface;
    case TTK_Union:     return TST_union;
  }
}

bool Sema::
InjectQueuedStrings(SourceLocation POI,
                    ArrayRef<const StringLiteral *> StringInjectionChunks,
                    MetaprogramDecl *MpD) {
  assert(!StringInjectionChunks.empty() &&
          "Should have checked if there were any "
          "StringInjectionChunks before calling InjectQueuedStrings");

  assert(TheParser &&
         "Should have called setParser(...) on the Sema object "
         "after constructing the parser; we need it for "
         "processing generated source strings.");

  assert(MpD);
  MetaprogramContext MetaCtx = MpD->getMetaprogramContext();

  bool Ok = true;

  { //scope
    // First, we'll push an r_brace token onto the stack of
    // generated source code strings to process --
    // this will be processed last, and will signal to the
    // parser that it has reached the end:
    static const SmallString<4> finaldummystr("}");
    static const QualType RBraceStrLitTy = Context.getConstantArrayType(
          Context.CharTy.withConst(),
          llvm::APInt(32, finaldummystr.size() + 1),
          nullptr,
          ArrayType::Normal, 0);

    SourceLocation rbraceloc = MpD->getEndLoc();
    assert(rbraceloc.isValid() &&
           "The loc assigned to the rbrace has to be valid, "
           "since we test for invalid locs to identify terminators.");

    // For preprocessor diagnostics (e.g. unterminated metaparses):
    PP.setCurInjectedStrLoc(rbraceloc);

    const StringLiteral *rbracestrlit =  StringLiteral::Create(
          Context, finaldummystr, StringLiteral::Ascii,
          false, RBraceStrLitTy, rbraceloc);
    PP.PushGeneratedSrcStr(rbracestrlit->getString(), rbraceloc);

    for (auto strlitit = StringInjectionChunks.rbegin();
        strlitit != StringInjectionChunks.rend();
        ++strlitit)
    {
      const StringLiteral *strlit = *strlitit;
      assert(strlit);

      StringRef Str = strlit->getString();

      assert(Str.data());
      assert(*Str.end() == '\0' &&
             "Expected each string literal to be null terminated");
      assert(strlit->getBeginLoc().isValid());

      PP.PushGeneratedSrcStr(Str, strlit->getBeginLoc());
    }
  } //end scope

  assert(TheParser);
  assert(TheParser->Tok.getLocation().isInvalid() &&
         "Expected an invalid dummy token before we begin parsing "
         "(to signal the ParserBrickWallRAII has been properly "
         "created)"); //NB this is different from the dummy rbrace

  // Ensure CurContext gets restored at end,
  // since we might have to adjust it while parsing, in case we
  // encounter e.g. new metaprograms decls within the injected strings.
  Sema::ContextRAII(*this, CurContext);
  assert(CurContext);

  // For metaprograms in a non-dependent context, temporarily pop the scope
  // to escape the implicit function declaration scope, to ensure proper lookup
  // and injection.  (We do not have to worry about this for template
  // instantiations.)
  llvm::SaveAndRestore<Scope *> ScopeRAII(CurScope);
  if (!MpD->isInstantiation())
    CurScope = CurScope->getParent();

  if (MetaCtx.isFunctionContext()) {
    assert(CurContext->isFunctionOrMethod());

    // ParseCompoundStatementBody expects the current tok to be
    // an l_brace, so we make it so:
    Token initlbracetok;
    initlbracetok.startToken();
    initlbracetok.setKind(tok::l_brace);
    TheParser->Tok = initlbracetok;

    // ParseCompoundStatementBody should consume all the contents of
    // the queued metaparses, unless it encounters an extra r_brace,
    // in which case we'll get an error on clearAnyDeadLexers.
    // Note that ParseCompoundStatementBody takes a
    // "bool isStmtExpr = false" param, but after looking at
    // the code that seems to only be true when processing a statement
    // in parentheses, which we needn't worry about for the metaprogram,
    // so we can safely leave it to the default false (rather than
    // passing it via MetaprogramContext).
    StmtResult sr(TheParser->ParseCompoundStatementBody());

    if (sr.isInvalid()) {
      Ok = false;
    }
    // This will allow this result to be properly acted on
    // in ActOnDeclStmt:
    MpD->setStmtResult(sr);

  } else { //kind is cls or extdecl:
    assert(!CurContext->isFunctionOrMethod() &&
           "Either MetaprogramContext wasn't constructed correctly, "
           "or this is a nested case and you perhaps need to "
           "temporarily set CurContext to teh parent context here");

    // For external declarations and class member declarations,
    // we'll be handling the loop ourselves, so we consume
    // the initial token to get straight to the declarations
    // (i.e. no need to set up an l_brace tok like above).
    TheParser->ConsumeToken();

    Parser::DeclGroupPtrTy ADecl;
    bool isclass = MetaCtx.isClassContext();
    assert(isclass || MetaCtx.isFileContext());
    assert(isclass == CurContext->isRecord()
           && "MetaprogramContext seems to have been incorrectly constructed");
    CXXRecordDecl *InstantiationOrClass = isclass ?
          cast<CXXRecordDecl>(CurContext) : nullptr;

    while (TheParser->Tok.isNot(tok::r_brace)) {
      if (PP.NoMoreInjectedStrsAvailable() && Ok) {
        TheParser->Diag(MpD->getEndLoc(),
                        diag::err_extraneous_closing_brace);
        PP.setNoMoreInjectedStrsAvailable(false);
        if (InstantiationOrClass)
          InstantiationOrClass->setInvalidDecl();
        return false;
      }
      assert(TheParser->getCurToken().getLocation().isValid());
      if (InstantiationOrClass) {
        DeclSpec::TST TagType = getTypeSpecForTagTypeKind(
                InstantiationOrClass->getTagKind());
        ADecl = TheParser->ParseCXXClassMemberDeclarationWithPragmas(
              MetaCtx.cls.AS, MetaCtx.cls.AccessAttrs, TagType,
              InstantiationOrClass);
      }
      else { //ext decl:
        // NB we don't actually use MetaContextVar's attrs now;
        // we locally construct.  But we've left the original in
        // MetaprogramContext in case those attributes are important --
        // can't remember if I tested it.  TESTME.
        ParsedAttributesWithRange attrs(TheParser->AttrFactory);
        TheParser->MaybeParseCXX11Attributes(attrs);
        ADecl = TheParser->ParseExternalDeclaration(attrs);
      }
      if (ADecl) {
        assert(ADecl.get().isSingleDecl() &&
               "Didn't expect ADecl to not be a SingleDecl");
        Decl *SingleDecl = ADecl.get().getSingleDecl();
        assert(SingleDecl);
        if (SingleDecl->isInvalidDecl()) {
          Ok = false;
          if (InstantiationOrClass)
            InstantiationOrClass->setInvalidDecl();
        }
      }

      // When you have multiple statements/decls
      // in a single argument of an __inj(...),
      // e.g. __inj("int i; float f;"),
      // then you won't have an invalid token after
      // parsing the first statement/decl.
      // But after the last one in the __inj(...)
      // statement you WILL have an invalid token.
      // BUT that's not all -- if you use a call to a constexpr
      // function that contains an __inj(...) statement, that
      // seems to add an ADDITIONAL invalid token -- hence the need
      // for a while loop here (AND in the ParseCompoundStmtBody version).
      while (TheParser->getCurToken().getLocation().isInvalid()
             && !TheParser->getCurToken().is(tok::r_brace)
             //^ DWR HACK: some of the dummy rbraces have invalid locs,
             //  don't skip over them.
             ) {
        TheParser->ConsumeToken();
      }
    } //end injected string parsing loop

    // Consume the dummy r_brace we entered above, and make sure
    // that concludes the metaprogram
    assert(TheParser->Tok.is(tok::r_brace));
    auto RBraceTok = TheParser->Tok;
    TheParser->ConsumeBrace();
    if (!PP.NoMoreInjectedStrsAvailable() && Ok) {
      TheParser->Diag(RBraceTok, diag::err_extraneous_closing_brace);
      Ok = false;
    }
    PP.setNoMoreInjectedStrsAvailable(false);

  } //end else

  return Ok;
}

static bool IsIntOrStr(QualType T) {
  if (T.isCXXStringLiteralType())
    return true;
  if (T->isIntegerType())
    return true;
  return false;
}

static bool CheckIsIntOrStr(Sema &SemaRef, Expr *E) {
  if (IsIntOrStr(E->getType()))
    return true;
  E->dump(); //[DELETEME]
  E->getType()->dump();
  SemaRef.Diag(E->getExprLoc(), diag::err_expected_int_or_str);
  return false;
}

/// Returns false if errors encountered.
/// This will change Args.
/// Only call when all Args are non-dependent.
bool Sema::PrepareStrIntArgsForEval(ArrayRef<Expr *> Args) {
  // Convert operands to rvalues to prepare for evaluation:
  for (auto argit = Args.begin(); argit != Args.end(); ++argit) {
    // Decay arrays first.
    ExprResult R = DefaultFunctionArrayLvalueConversion(*argit);
    if (R.isInvalid())
      return false;
    // Check that the operand type is acceptable.
    if (!CheckIsIntOrStr(*this, *argit))
      return false;

    //Replace the array entry with the converted Expr *:
    const_cast<Expr *&>(*argit) = R.get();
  }
  return true; //No error
}