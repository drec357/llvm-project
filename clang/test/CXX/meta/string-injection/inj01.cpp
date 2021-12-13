// RUN: %clang_cc1 -std=c++2a -fstring-injection -verify %s
// expected-no-diagnostics

#define assert(expr) ((expr) ? (void)(0) : __builtin_abort())

static_assert(__has_extension(string_injection),
    "Pass -fstring-injection to compiler");

static constexpr const char *five = "5";
static const char *fiveB = "5";
const char *fiveC = "5";

// String injection in top level context
consteval {
  __inj("");
  __injf("");
  __inj("static const int h = 2;");
  __inj("static const int i = h + ", 1, ";");
  __injf("constexpr int {} = {} + {} + {};", "j", 4, five, "i");
  __inj("//does nothing");

  __injf("//single arg to injf okay");
//  __inj("int k = ", fiveB, ";"); //ERROR (DWR TODO turn these into exp error tests...)
//  __inj("int k = ", fiveC, ";"); //ERROR
//  __inj("void f() {}>"); //ERROR
//  __inj("void f() {}]"); //ERROR
//  __inj("void f() {})"); //ERROR
//  __inj("void f() {}}"); //ERROR

  // If not terminated via subsequent __inj/__injf statements in this same
  // consteval {}, the following should produce errors:
//  __inj("void f("); //ERROR (FIXME: diagnostics suck)
//  __inj("template<int I>>"); //ERROR
//  __inj("template<int i, template<typename, "); //ERROR (FIXME: diagnostics suck)
//  __inj("template<int I>"); //ERROR but no crash //FIXME pointed-to location wrong
//  __inj("\""); //ERROR
//  __inj("/*"); //ERROR

  // So long as each injected entity is terminated by the
  // end of this consteval {} you're okay:
  __inj("void foo(");
  __injf("int {}, int {}", "parm1", "parm2");
  __inj("){");
  __inj("}");
}

static_assert(h==2);
static_assert(i==3);
static_assert(j==12);

// Namespace context
namespace test1 {
consteval {
  __inj("static const int i", " = ", 3, ";");
  __injf("constexpr int {} = {} + {} + {};", "j", 4, five, "i");
}
}
static_assert(test1::i==3);
static_assert(test1::j==12);

// Non-dependent class context
struct test2 {
  consteval {
    __inj("static const int i", " = ", 3, ";");
    __injf("static const int {} = {} + {} + {};", "j", 4, five, "i");
  }
};
static_assert(test2::i==3);
static_assert(test2::j==12);

// Non-dependent function context
void f(int b) {
  if (b) {
    consteval {
      __inj("int v = b + 1;");
      __inj("static const int fi", " = ", 3, ";");
      __injf("static const int {} = {} + {} + {};", "fj", 4, five, "fi");
    }
    v += 0; // Referencing v here is only possible because the metaprogram was
            // non-dependent, and so could be evaluated immediately
    static_assert(fi==3);
    static_assert(fj==12);
  }
}

// Dependent function context
template<int I>
bool g() {
  bool res;
  consteval {
    __inj("");
    __inj("static const int v = 2;");
    __inj("static const int fi = 33 + v;");
    __injf("res = (v == 2 + {});", I);
  }
//  static const int w = v + I; //ERROR
  consteval {
    __injf("static const int w = v + {};", I); //Okay
  }
  return res;
}
template<int I>
struct Wrapper {
  bool g() {
    bool res;
    consteval {
      __inj("");
      __inj("static const int v = 2;");
      __inj("static const int fi = 33 + v;");
      __inj("res = (v==2+", I, ");");
    }
//    static const int w = v + I; //ERROR
    consteval {
      __injf("static const int w = v + {};", I); //Okay
    }
    return res;
  }
};
void dummy() {
  bool res = g<3>();
  res = Wrapper<4>().g();
}

// Dependent class context
template<int I>
struct B {
  static const int init = 0;
  consteval {
    __inj("static const int v = 2 + ", I, ";");
    __inj("static const decltype(init) fi = 33 + v;");
    __inj("static const int fj=",44,"+",five,";");
  }
//  static const int w = v + I; //ERROR
  consteval {
    __injf("static const int w = v + {};", I); //Okay
  }
  struct Inner {
    consteval {
      __injf("static const int x = w + v + {};", I); //Okay
    }
  };
};
static_assert(B<3>::fi==38);
static_assert(B<3>::fj==49);
static_assert(B<3>::w==8);
static_assert(B<3>::Inner::x==16);



// Macros shouldn't be used within injection statements;
// instead, stringize and send as a separate argument.

/// This expands any macros in x and stringizes the result.
#   define PP_EMPTY()
#   define PP_DEFER(id) id PP_EMPTY()
#   define PP_STRINGIZE_SIMPLE(x) #x
#   define PP_EXPAND(x) x
#define STRINGIZE(x) PP_EXPAND(PP_DEFER(PP_STRINGIZE_SIMPLE)(x))

static const int x = 8;
#define MYMACROA ((2+4) * x)
#define MYMACRO_AA(a,b) a-b
#define MYMACRO_AAA MYMACRO_AA(5,MYMACROA) + 6
consteval {
  __inj("static const int y = 3 + ", STRINGIZE(MYMACROA), ";");
//      __inj("static_assert(MYMACROA==2);"); //ERROR (bad diagnostics)
//      __inj("static_assert(MYMACRO_AAA==9);"); //ERROR (bad diagnostics)
//      __inj("static_assert(MYMACRO_AA(5,3)==2;"); //ERROR (bad diags)
//      __inj("#define MYMACRO_BB(a,b) a+b"); //ERROR (good diags)
//      __inj("#define MYMACRO_B 3"); // ERROR (good diags)
//      __inj("int asdf3 = MYMACRO_AAA;"); //ERROR (bad diags)
//      __inj("#undef MY_MACRO_AA"); //this actually works, but shouldn't
}
static_assert(y==51);









// Injection via meta functions
namespace metafuncinj {
  consteval void a42() {
    __inj("int a = 42;");
  }
  consteval void declare_intvar(const char *name, int val) {
    __injf("int {} = {};", name, val);
  }
  template<typename INTORSTR>
  consteval void declare_intorstrvar(const char *name, INTORSTR val) {
    __injf("static constexpr auto {} = {};", name, val);
  }
  constexpr auto lambda_declare_intvar = [](const char *maybeinline, const char *name) -> void {
    __injf("static {} int {} = 0;", maybeinline, name);
  };
  template<typename INTORSTR>
  consteval auto set_var(const char *name, INTORSTR val) {
    __injf("{} = {};", name, val);
  }

  // Namespace context
  consteval {
      a42();
      declare_intvar("b", 43);
      declare_intorstrvar("c", "\"asdf\"");
      lambda_declare_intvar("inline", "d");
      __inj("void d_increment_by_2() {");
      set_var("d", "d+2");
      __inj("}");
  }
  void testA() {
    assert(a == 42);
    assert(b == 43);
    assert(c[0] == 'a' && c[3] == 'f');
    metafuncinj::d = 0;
    assert(d == 0);
    d_increment_by_2();
    assert(d == 2);
  }

  // Non-dependent class context
  struct Foo {
    consteval {
        a42();
        declare_intvar("b", 43);
        declare_intorstrvar("c", "\"asdf\"");
        lambda_declare_intvar("inline", "d");
        __inj("void d_increment_by_2() {");
        set_var("d", "d+2");
        __inj("}");
    }
    void test() {
      assert(a == 42);
      assert(b == 43);
      assert(c[0] == 'a' && c[3] == 'f');
      assert(d == 0);
      this->d_increment_by_2();
      assert(d == 2);
    }
  };

  // Non-dependent function context
  void testB(int param2 = 2) {
    static const int local43 = 43;
    if (param2 == 2) {
      int localVarInScope = 1;
      consteval {
        __inj("localVarInScope += 1;");
        a42();
        declare_intvar("b", local43);
        declare_intorstrvar("c", "\"asdf\"");
        lambda_declare_intvar("", "d");
        __inj("auto d_increment_by_2 = [&](int zero = 0){");
        set_var("d", "d + param2 + zero");
        __inj("};");
      }
      assert(localVarInScope==2);
      assert(a == 42);
      assert(b == 43);
      assert(c[0] == 'a' && c[3] == 'f');
      assert(d == 0);
      d_increment_by_2(0);
      assert(d == 2);
    }
  }

  // Dependent class context
  template<int I>
  struct B {
    static const int memberTwo = 2;
    consteval {
        const char *name = I==3 ? "\"asdf\"" : "\"qwer\"";
        a42();
        declare_intvar("b", 40 + I);
        declare_intorstrvar("c", name);
        lambda_declare_intvar("inline", "d");
        __inj("void d_increment_by_2() {");
        set_var("d", "d + memberTwo");
        __inj("}");
    }
    void test() {
      assert(a == 42);
      assert(b == 40 + I);
      assert(c[0] == 'a' && c[3] == 'f');
      assert(d == 0);
      this->d_increment_by_2();
      assert(d == 2);
    }
  };

  // Explicit instantiation (this is needed to avoid
  // a linker error regarding the static d, since it
  // is not seen until instantiation - FIXME?)
  template class B<3>;
}

int main() {
  metafuncinj::testA();
  metafuncinj::Foo foo;
  foo.test();
  metafuncinj::testB(2);
  metafuncinj::B<3> b3;
  b3.test();
}