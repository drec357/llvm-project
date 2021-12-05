//===--- ParseMetaprogram.cpp - (C++) consteval {...} parser implem. ------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the parsing of consteval {...} metaprograms and
// string injection facilities used within it.
// (Note that the parsing of the *content of* string literals passed in
// string injection statements is handled in Sema/SemaMetaprogram.cpp.)
//
//===----------------------------------------------------------------------===//


#include "clang/AST/ASTContext.h"
#include "clang/Basic/MetaprogramContext.h"
#include "clang/Parse/ParseDiagnostic.h"
#include "clang/Parse/Parser.h"
#include "clang/Parse/RAIIObjectsForParser.h"
#include "clang/AST/PrettyDeclStackTrace.h"

using namespace clang;

/// Parse a metaprogram.
///
/// \verbatim
///   metaprogram:
///     'consteval' compound-statement
/// \endverbatim
Parser::DeclGroupPtrTy Parser::ParseMetaprogram(MetaprogramContext &MetaCtx,
                                                bool Nested) {
  SourceLocation KeywordLoc = ConsumeToken();

  if (!Tok.is(tok::l_brace)) {
    Diag(Tok, diag::err_expected) << tok::l_brace;
    return nullptr;
  }
  unsigned ScopeFlags;
  Decl *D = Actions.ActOnMetaprogramDecl(getCurScope(), KeywordLoc, ScopeFlags,
                                         MetaCtx, Nested);

  // Enter a scope for the metaprogram declaration body.
  ParseScope BodyScope(this, ScopeFlags);

  Actions.ActOnStartMetaprogramDecl(getCurScope(), D);

  PrettyDeclStackTraceEntry CrashInfo(Actions.Context, D, KeywordLoc,
                                      "parsing metaprogram declaration body");

  // Parse the body of the metaprogram.
  StmtResult Body(ParseCompoundStatementBody());

  if (!Body.isInvalid())
    Actions.ActOnFinishMetaprogramDecl(getCurScope(), D, Body.get());
  else
    Actions.ActOnMetaprogramDeclError(getCurScope(), D);

  return Actions.ConvertDeclToDeclGroup(D);
}

/// Helper: parses any number of constexpr Expr arguments,
/// param packs allowed.
/// Returns true if an error is encountered.
bool Parser::ParseArbitraryConstexprExprArgs(SmallVectorImpl<Expr *> &Args,
                                             bool &AnyDependent) {
  do {
    ExprResult Expr = ParseConstantExpression(); //ParseConditionalExpression?
    if (Tok.is(tok::ellipsis))
      Expr = Actions.ActOnPackExpansion(Expr.get(), ConsumeToken());
    if (Expr.isInvalid())
      return true;
    if (!AnyDependent &&
        ( Expr.get()->isTypeDependent() ||
          Expr.get()->isValueDependent() ))
      AnyDependent = true;

    Args.push_back(Expr.get());

  } while (TryConsumeToken(tok::comma));
  return false;
}

/// Helper: parses any number of constexpr Expr arguments,
/// param packs allowed, within parentheses, and
/// sets the paren locs.
/// Returns true if an error is encountered.
bool Parser::ParseArbitraryParensConstexprExprArgs(SourceLocation &LParenLoc,
                                                   SmallVectorImpl<Expr *> &Args,
                                                   SourceLocation &RParenLoc,
                                                   bool &AnyDependent) {
  BalancedDelimiterTracker Parens(*this, tok::l_paren);
  if (Parens.expectAndConsume())
    return true;
  LParenLoc = Parens.getOpenLocation();

  if (ParseArbitraryConstexprExprArgs(Args, AnyDependent)) {
    Parens.skipToEnd();
    return true;
  }

  if (Parens.consumeClose())
    return true;

  RParenLoc = Parens.getCloseLocation();
  return false;
}

// Creates a c-string of type const char[N].
static StringLiteral *
MakeString(ASTContext &Ctx, StringRef Str, SourceLocation Loc) {
  QualType StrLitTy = Ctx.getConstantArrayType(
      Ctx.CharTy.withConst(), llvm::APInt(32, Str.size() + 1), nullptr,
      ArrayType::Normal, 0);

  // Create a string literal of type const char [L] where L
  // is the number of characters in the StringRef.
  return StringLiteral::Create(
      Ctx, Str, StringLiteral::Ascii, false, StrLitTy, Loc);
}

/// \brief Parse an __inj(...) or __injf(...) statement.
///
///   string-injection-statement:
///     '__inj'  '(' constant-argument-list ')' ';'
///     '__injf' '(' string-literal, constant-argument-list ')' ';'
///
/// Each argument must be a string literal or integer constant expression,
/// except the first argument of __injf which can only be a string literal.
/// When in a non-dependent metaprogram, the strings will be parsed *with
/// whitespace in between* each argument.
///
/// Note that the statement parser will collect the trailing semicolon.
///
StmtResult Parser::ParseStringInjectionStmt(bool IsSpelledWithF) {
  SourceLocation KeywordLoc = ConsumeToken();
  SourceLocation LParenLoc, RParenLoc;
  SmallVector<Expr *, 4> Args;
  bool AnyDependent = false;

  if (ParseArbitraryParensConstexprExprArgs(LParenLoc, Args,
                                            RParenLoc, AnyDependent))
    return StmtError();

  if (!IsSpelledWithF)
    return Actions.ActOnStringInjectionStmt(KeywordLoc, LParenLoc,
                                          Args, RParenLoc, AnyDependent);

  // We have just parsed an __injf(...) statement, e.g.
  //   __injf("int {} = {};", "foo", 42);
  // We want to interpret that as these sequential arguments, i.e. as if
  // it had been written
  //   __inj("int ", "foo", " = ", 42, ";")

  // Make sure the first arg is a non-dependent string literal.
  StringLiteral *WrittenFirstArg = dyn_cast<StringLiteral>(Args[0]);
  if (!WrittenFirstArg) {
    Diag(LParenLoc.getLocWithOffset(1),
         diag::err___injf_first_arg_not_str);
    return StmtError();
  }

  SmallVector<Expr *, 8> ReorderedArgs;
  static const StringRef PlaceholderStr =
      StringInjectionStmt::PlaceholderStr();
  static const size_t PlaceholderSize = PlaceholderStr.size();
  StringRef FirstArgStr = WrittenFirstArg->getString();
  SourceLocation FirstArgLoc = WrittenFirstArg->getBeginLoc();
  const unsigned NumArgs = Args.size();

  unsigned SubstArgIdx = 0;
  StringRef FirstArgSubStr;
  SourceLocation FirstArgSubLoc;
  size_t CurBegin = 0, CurEnd = FirstArgStr.find(PlaceholderStr, 0);

  while (CurEnd != StringRef::npos) {
    // Make sure we don't have more placeholders than remaining args
    if (SubstArgIdx + 1 >= NumArgs) {
      Diag(RParenLoc, diag::err___injf_wrong_num_args)
          << "at least " + std::to_string(SubstArgIdx + 1)
          << std::to_string(NumArgs-1);
      return StmtError();
    }
    // Create a string literal from a substring of the first argument's
    // string literal, and push it onto ReorderedArgs.
    FirstArgSubStr = FirstArgStr.substr(CurBegin, CurEnd-CurBegin);
    FirstArgSubLoc = FirstArgLoc.getLocWithOffset(CurBegin);
    StringLiteral *FirstArgSubStrLit =
        MakeString(Actions.getASTContext(), FirstArgSubStr, FirstArgSubLoc);
    ReorderedArgs.push_back(FirstArgSubStrLit);

    // Push the next of the remaining args onto ReorderedArgs; in this way
    // it serves as the substitute for the placeholder.
    ReorderedArgs.push_back(Args[++SubstArgIdx]);

    // Iterate
    CurBegin = CurEnd + PlaceholderSize;
    CurEnd = FirstArgStr.find(PlaceholderStr, CurBegin);
  }
  // Make sure we don't have fewer placeholders than remaining args
  if (SubstArgIdx + 1 < Args.size()) {
    Diag(Args[SubstArgIdx]->getExprLoc(), diag::err___injf_wrong_num_args)
        << std::to_string(SubstArgIdx + 1)
        << std::to_string(NumArgs-1);
    return StmtError();
  }

  // Add the final substring
  FirstArgSubStr = FirstArgStr.substr(CurBegin, FirstArgStr.size()-CurBegin);
  FirstArgSubLoc = FirstArgLoc.getLocWithOffset(CurBegin);
  StringLiteral *FirstArgSubStrLit =
      MakeString(Actions.getASTContext(), FirstArgSubStr, FirstArgSubLoc);
  ReorderedArgs.push_back(FirstArgSubStrLit);

  return Actions.ActOnStringInjectionStmt(KeywordLoc, LParenLoc,
                                          ReorderedArgs, RParenLoc,
                                          AnyDependent, WrittenFirstArg);
}