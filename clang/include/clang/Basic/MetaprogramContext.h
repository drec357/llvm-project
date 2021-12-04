//===--- MetaprogramContext.h -----------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Defines MetaprogramContext, a discriminated union which transmits context
// info from a MetaprogramDecl to its StringInjectionStmts, for proper
// interpretation of their parsed content.
//
//===----------------------------------------------------------------------===//


#ifndef LLVM_CLANG_BASIC_METAPROGRAMCONTEXT_H
#define LLVM_CLANG_BASIC_METAPROGRAMCONTEXT_H

#include "clang/Basic/Specifiers.h" //for AccessSpecifier

namespace clang {

struct ParsedAttributesWithRange;
class ParsingDeclSpec;

/// Carries parser and context info to any metaparse instances
/// encountered while parsing a MetaprogramDecl.
struct MetaprogramContext {
  union {
    struct {
      // FIXME needed?
      ParsedAttributesWithRange &attrs;
      ParsingDeclSpec *DS;
    } extdecl;
    struct {
      ParsedAttributesWithRange &AccessAttrs;
      AccessSpecifier AS;
    } cls;
    struct {
      //nothing for now
    } fcn;
  };
  enum {
    MC_file,
    MC_cls,
    MC_fcn
  } kind;

  AccessSpecifier getAccessUnsafe() const {
    if (kind == MC_cls)
      return cls.AS;
    else
      return AS_none;
  }

  /// Call only when you know \c isClassContext()==true .
  AccessSpecifier getAccess() const {
    assert(kind == MC_cls);
    return AS_none;
  }

  bool isClassContext()    const { return kind == MC_cls; }
  bool isFunctionContext() const { return kind == MC_fcn; }
  bool isFileContext()     const { return kind == MC_file; }

  MetaprogramContext(ParsedAttributesWithRange &attrs,
                     ParsingDeclSpec *DS)
    : extdecl{attrs, DS}, kind(MC_file)
  {}
  MetaprogramContext(ParsedAttributesWithRange &AccessAttrs,
                     AccessSpecifier AS)
    : cls{AccessAttrs, AS}, kind(MC_cls)
  {}
  MetaprogramContext()
    : fcn{}, kind(MC_fcn)
  {}
};

} //namespace clang

#endif //LLVM_CLANG_BASIC_METAPROGRAMCONTEXT_H
