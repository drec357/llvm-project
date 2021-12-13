//===--- ParserBrickWallRAII.cpp - saves full state of Parser -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements RAII objects needed to save & restore the state of
//  the Parser, Preprocessor, and Sema objects for parsing into
//  template instantiations.
//
//===----------------------------------------------------------------------===//

#include "clang/Parse/RAIIObjectsForParser.h"

using namespace clang;


ParserBrickWallRAII::JustTheParserVariables::JustTheParserVariables(Parser &P)
  : TheParser(&P) {
  // Note The PreprocessorBrickWallRAII will NOT have been constructed yet,
  // so TheParser->PP is still in its old state.

#ifndef NDEBUG
  unsigned DEBUGorignumlexers = P.PP.getTotalNumLexers();
#endif

  // If there is anything to be done to clear out unneeded tokens
  // etc. before we save the state of the Preprocessor -- i.e. anything which
  // does not need to be restored when this RAII is destroyed -- those
  // tasks should be done in Preprocessor::CleanUpCache() (defined
  // in PPCaching.cpp).
  P.PP.CleanUpCache();

  // We unconsume the current token to help us restore the state during
  // destruction via a simple ConsumeToken call.
  // Note that this may push an extra lexer temporarily onto the
  // IncludeMacroStack than we had originally -- but since the
  // PreprocessorBrickWallRAII will be constructed after this, it will register
  // the proper num lexers as the TotalNumLexersAtRAIIConstruction it uses to
  // identify exactly when it is finished processing the current batch of
  // generated source strings.  I.e. this code shouldn't affect the behavior
  // of the Preprocessor.
  Token EmptyToken = {};
  P.UnconsumeToken(EmptyToken);
  assert(P.Tok.getLocation().isInvalid() &&
         "We rely on this dummy token having an invalid location as a signal "
         "that the ParserBrickWallRAII has been created before injected string parsing "
         "(see lib/Sema/SemaMetaprogram.cpp)");

  // Calling UnconsumeToken will have put us in CachingLexMode, such that the
  // next Lex call would fetch the old P.Tok we just cached.  We don't want
  // to do that -- we want to preserve that token for when we are done with
  // injected string parsing.  Exiting caching lex mode will do this.
  P.PP.ExitCachingLexMode();

  assert(P.PP.CachedTokens.size() > 0);
  assert(P.PP.getTotalNumLexers() == DEBUGorignumlexers);
}

ParserBrickWallRAII::JustTheParserVariables::
JustTheParserVariables(JustTheParserVariables &&other)
  : TheParser(other.TheParser),
    Valid(other.Valid),
    ShouldCallDestructor(other.ShouldCallDestructor) {
  // Prevent the destructor from being called in the temporary
  other.ShouldCallDestructor = false;
}

ParserBrickWallRAII::JustTheParserVariables::~JustTheParserVariables() {
  if (ShouldCallDestructor) {
    // Note the PreprocessorBrickWallRAII will have been destroyed BEFORE this,
    // so TheParser->PP should be back to its original state
    // (and all the asserts about the numlexers being the same before
    // and after will have passed.)

    bool WasInCachingLexMode = TheParser->PP.InCachingLexMode();
    // DWR FIXME if this assert never fails, then get rid of the WasInCachingLexMode test;
    // otherwise remove the assert
    assert(!WasInCachingLexMode);

    // Reverse our P.UnconsumeToken call from the ctor above.
    // We need to EnterCachingLexMode since the UnconsumeToken call during
    // construction would have cached the token; we need to enter caching
    // lex mode to fetch from that cache.
    TheParser->PP.EnterCachingLexMode();
    TheParser->ConsumeAnyToken();

    if (!WasInCachingLexMode)
      TheParser->PP.ExitCachingLexMode();
  }
}

void ParserBrickWallRAII::JustTheParserVariables::setInvalid() {
  Valid = false;
}

ParserBrickWallRAII::PreprocessorBrickWallRAII::
PreprocessorBrickWallRAII(Preprocessor &ThePP)
  : PP(ThePP),
    OldTotalNumLexersAtRAIIConstruction(PP.TotalNumLexersAtRAIIConstruction),
    OldParsingFromInjectedStrBool(PP.ParsingFromInjectedStrBool),
    SavedCachedTokens(PP.CachedTokens),
    SavedCachedLexPos(PP.CachedLexPos),
    SavedBacktrackPositions(PP.BacktrackPositions)
{
  PP.TotalNumLexersAtRAIIConstruction = PP.getTotalNumLexers();

  assert(!PP.NoMoreInjectedStrsAvailableBool
         && "Did not expect to construct a PreprocessorBrickWallRAII while "
            "DoneProcessing... was true; was that value perhaps not reset "
            "correctly after the last constexpr decl processing?");
  PP.ParsingFromInjectedStrBool = true;

  assert(PP.isBacktrackEnabled() == !SavedBacktrackPositions.empty()); //sanity

  // Clear the cache
  PP.CachedTokens.clear();
  PP.CachedLexPos = 0;
  PP.BacktrackPositions.clear();
}

ParserBrickWallRAII::PreprocessorBrickWallRAII::
PreprocessorBrickWallRAII(PreprocessorBrickWallRAII &&other)
  : PP(other.PP),
    OldTotalNumLexersAtRAIIConstruction(
          other.OldTotalNumLexersAtRAIIConstruction),
    OldParsingFromInjectedStrBool(other.OldParsingFromInjectedStrBool),
    SavedCachedTokens(std::move(other.SavedCachedTokens)),
    SavedCachedLexPos(other.SavedCachedLexPos),
    SavedBacktrackPositions(std::move(other.SavedBacktrackPositions)),
    ShouldCallDestructor(other.ShouldCallDestructor) {
  other.ShouldCallDestructor = false;
}

ParserBrickWallRAII::PreprocessorBrickWallRAII::~PreprocessorBrickWallRAII() {
  assert (PP.getTotalNumLexers() >= PP.TotalNumLexersAtRAIIConstruction
          && "You ate past the brick wall!");

  if (ShouldCallDestructor) {
    clearAnyDeadLexers();

    PP.TotalNumLexersAtRAIIConstruction = OldTotalNumLexersAtRAIIConstruction;
    PP.ParsingFromInjectedStrBool = OldParsingFromInjectedStrBool;

    assert(PP.CachedTokens.size() == PP.CachedLexPos ||
               PP.ErrorWhileParsingFromInjectedStr() &&
           "Error - finished parsing injected strings, with no errors reported, "
           "and num lexers was same before and after (per previous assert), "
           "but it appears you somehow cached some extra tokens that you didn't "
           "use, indicating you ate into the enclosing source code "
           "after the generated source.  So, somehow the code has a bug AND the "
           "previous assert isn't doing its job! (Well, assuming that the "
           "CachedLexPos < CachedTokens.size(); should never be greater.)");

    PP.setErrorWhileParsingFromInjectedStr(false);

    PP.CachedTokens = SavedCachedTokens;
    PP.CachedLexPos = SavedCachedLexPos;
    PP.BacktrackPositions = SavedBacktrackPositions;
  }
}

/// Used in ~ParserBrickWallRAII() to clear out any dead lexers (should only
/// be TokenLexers I think, left over by late parsing) before proceeding
/// to the ~PreprocessorBrickWallRAII() call, where we will double-check
/// our work by asserting that the numlexers is the same as when it was
/// first constructed.
///
/// Also, if errors are encountered during processing, such that you've
/// called setInvalid on this object, this will clear away the "live lexers"
/// as well.
void ParserBrickWallRAII::PreprocessorBrickWallRAII::clearAnyDeadLexers() {
  assert(ShouldCallDestructor &&
         "Calling this after you've already exited the RAII -- "
         "this risks tripping asserts, because restoring the "
         "old token via ConsumeToken will add a lexer we don't "
         "want to clear.");
  // If we've been lexing from a terminator lexer to kill off an unterminated
  // metaparse expression, we can stop now.  Must do this before any more
  // PP.Lex calls or we'll never make any progress.
  if (PP.CurLexerKind == Preprocessor::CLK_TerminatorPretendLexer) {
    // Since we didn't PushIncludeMacroStack when we first set
    // CLK_TerminatorPretendLexer as the kind, we can just recompute
    // to get back to the original state.
    // First must set the ParsingFromInjectedStrBool val; we'll do that
    // again later but we definitely need to do it now before recomputing,
    // becuase the introduction of the terminator lexer wiped out that
    // info:
    PP.ParsingFromInjectedStrBool = OldParsingFromInjectedStrBool;
    PP.recomputeCurLexerKind();
    assert(PP.CurLexerKind != Preprocessor::CLK_TerminatorPretendLexer);
  }

  // After lexing from injected source strings, the old lexers will still be in
  // place, only with no content left -- the lexer change will only ...
  // FIXME maybe we should just call Preprocessor::HandleEndOfFile
  // directly here
  Token DummyToken;

  while (PP.getTotalNumLexers() > PP.TotalNumLexersAtRAIIConstruction)
    PP.Lex(DummyToken);

  PP.setNoMoreInjectedStrsAvailable(false);
}

void ParserBrickWallRAII::PreprocessorBrickWallRAII::setInvalid() {
  // Hack, possibly - this seems to be left as true sometimes during
  // error recovery, so we'll make absolutely sure it is false here.
  PP.setNoMoreInjectedStrsAvailable(false);
  Valid = false;
}

ParserBrickWallRAII::ParserBrickWallRAII(Parser &P)
  : SavedParseState(P), // Must be constructed BEFORE
                        // SavedPreprocessorState and destroyed AFTER it.
    SavedPreprocessorState(P.PP) {
}
ParserBrickWallRAII::ParserBrickWallRAII(ParserBrickWallRAII &&other)
    // SavedParseState must be constructed BEFORE SavedPreprocessorState
    // and destroyed AFTER it.
  : SavedParseState(std::move(other.SavedParseState)),
    SavedPreprocessorState(std::move(other.SavedPreprocessorState)) {
}

ParserBrickWallRAII::~ParserBrickWallRAII() {
}

/// Called when we encounter an error during string injection, and need to
/// recover, by e.g. clearing out any pending injected string lexers to
/// get back to the original parser state.
void ParserBrickWallRAII::setInvalid() {
  Valid = false;
  SavedPreprocessorState.setInvalid();
  SavedParseState.setInvalid();
}

SemaPIIRAII::SemaPIIRAII(Sema &SemaRef, decltype(SemaRef.PII) NewPII)
    : SemaRef(SemaRef), OldPII(SemaRef.PII) {
  SemaRef.PII = NewPII;
}

SemaPIIRAII::SemaPIIRAII(SemaPIIRAII &&other)
  : SemaRef(other.SemaRef), OldPII(other.OldPII), Exited(other.Exited) {
  other.Exited = true;
}

SemaPIIRAII::~SemaPIIRAII() {
  if (!Exited)
    SemaRef.PII = OldPII;
}

void SemaPIIRAII::Exit() {
  if (!Exited) {
    SemaRef.PII = OldPII;
    Exited = true;
  }
}


TempParseIntoDiffScope::
TempParseIntoDiffScope(Sema &S, Sema::ParsingIntoInstantiation NewPII,
                       unsigned ScopeFlags)
  : ParserBrickWallRAII(S.getParser()),
    TheSema(S),
    OldScope(S.CurScope),
    SavedPIIState(S, NewPII) {
  // Enter the scope of TagOrTemplate, using the TUScope as the parent:
  assert(S.TUScope);
  S.CurScope = S.TUScope;
  S.getParser().EnterScope(ScopeFlags);
  S.CurScope->setEntity(S.CurContext);
}

TempParseIntoDiffScope::
TempParseIntoDiffScope(TempParseIntoDiffScope &&other)
  : ParserBrickWallRAII(std::move(other)),
    TheSema(other.TheSema),
    OldScope(other.OldScope),
    SavedPIIState(std::move(other.SavedPIIState)),
    ShouldCallDestructor(other.ShouldCallDestructor) {
  other.ShouldCallDestructor = false;
}

TempParseIntoDiffScope::~TempParseIntoDiffScope() {
  if (ShouldCallDestructor) {
    TheSema.getParser().ExitScope();
    assert(TheSema.CurScope == TheSema.TUScope &&
           "Expected to be back at TUScope -- was a scope "
           "imbalance somehow introduced?");
    TheSema.CurScope = OldScope;
  }
}

TempParseIntoClassInstantiation::TempParseIntoClassInstantiation(
        Sema &S, bool IsInterface)
  : TempParseIntoDiffScope(S, Sema::PII_class,
                           Scope::ClassScope|Scope::DeclScope),
    Pcd(S.getParser(), Decl::castFromDeclContext(S.CurContext),
        true/*always treat instantiations as non-nested classes*/,
        IsInterface) {}

TempParseIntoClassInstantiation::~TempParseIntoClassInstantiation() {}

TempParseIntoFuncInstantiation::TempParseIntoFuncInstantiation(Sema &S)
  : TempParseIntoDiffScope(S, Sema::PII_func,
                           Scope::FnScope | Scope::DeclScope |
                             Scope::CompoundStmtScope) {}

TempParseIntoFuncInstantiation::~TempParseIntoFuncInstantiation() {}