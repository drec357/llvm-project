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

/// \brief Parse an __inject(...) statement.
///
///   string-injection-statement:
///     '__inject' '(' constant-argument-list ')' ';'
///
/// Each argument must be a string literal or integer constant expression.
/// They will be meta-parsed *with whitespace in between* each argument.
/// If concatenation is needed with no whitespace, use __concatenate(...)
/// for the appropriate argument.
///
/// Note that the statement parser will collect the trailing semicolon.
///
StmtResult Parser::ParseStringInjectionStmt() {
  SourceLocation KeywordLoc = ConsumeToken();
  SourceLocation LParenLoc, RParenLoc;
  SmallVector<Expr *, 4> Args;
  bool AnyDependent = false;

  if (ParseArbitraryParensConstexprExprArgs(LParenLoc, Args,
                                            RParenLoc, AnyDependent))
    return StmtError();

  return Actions.ActOnStringInjectionStmt(KeywordLoc, LParenLoc,
                                            Args, RParenLoc, AnyDependent);
}