// RUN: %clang_cc1 -std=c++2a -ftemplate-for -verify %s
// expected-no-diagnostics

#define assert(expr) ((expr) ? (void)(0) : __builtin_abort())

// ----- Dummy structs ------ //
template <typename T, typename U>
struct Struct {
  const T i = T();
  const T j = T();
  const U k = U();

//private:
//  const T inaccessible_field = T(); //error
//public:

  struct something {
    const int f = 10;
  };

  const U l = U();
};
struct StructB { int z = 3; int zz = 2;};

// Non-dependent tests

constexpr int test_struct() {
  Struct<int, char> s;
  StructB s2;
  int res = 0;
  float f = 0;
  // NB struct keyword must prepend range expression to iterate over fields.
  template for (auto x : struct s) {
    f += x;
    ++res;
  }
  int i = 0;
  template for (auto x : struct s) {
    template for (auto y : struct s2) {
      f += x;
      f += y;
      ++res;
    }
    template for (auto y : struct s)
      ;
  }
  return res;
}
static_assert(test_struct()==12);

constexpr int test_constexpr_struct() {
  constexpr Struct<double, char> s;
  int i = 0;
  float f = 0;
  template for (constexpr auto x : struct s) {
    f += x;
    ++i;
  }
  template for (constexpr auto x : struct s) {
    template for (constexpr auto x : struct(s))
      ;
  }
  return i;
}
static_assert(test_constexpr_struct()==4);


// Dependent transformation tests:

template<typename T, typename U>
struct struct_exp_tester {
  constexpr int f(T s, U s2) {
    int res = 0;
    float f = 0;
    template for (auto x : struct s) {
      f += x;
      ++res;
    }
    template for (auto x : struct s) {
      template for (auto y : struct s2) {
        f += x;
        f += y;
        ++res;
      }
      template for (auto y : struct s )
        ;
    }
    return res;
  }
};

template<typename T, typename U>
struct dep_to_dep_transf : struct_exp_tester<Struct<T,U>, StructB> {
  using base = struct_exp_tester<Struct<T,U>, StructB>;

  constexpr int f(Struct<T,U> s = {}, StructB s2 = {}) {
    return base::f(s, s2);
  }
};

static_assert(dep_to_dep_transf<int, char>().f() == 12);


template<typename T, typename U>
constexpr int test_struct_dep(T s, U s2) {
  int res = 0;
  float f = 0;
  // NB struct keyword must prepend range expression to iterate over fields.
  template for (auto x : struct s) {
    f += x;
    ++res;
  }
  template for (auto x : struct s) {
    template for (auto y : struct s2) {
      f += x;
      f += y;
      ++res;
    }
    template for (auto y : struct s )
      ;
  }
  return res;
}
static_assert(test_struct_dep(Struct<int, char>(), StructB())==12);


template<typename T>
struct StructTestA {
  template<typename U>
  static constexpr int test_constexpr_struct_dep() {
    constexpr Struct<T, U> s;
    int i = 0;
    float f = 0;
    template for (constexpr auto x : struct s) {
      f += x;
      ++i;
    }
    template for (constexpr auto x : struct s) {
      template for (constexpr auto x : struct s)
        ;
    }
    return i;
  }
};
// Dependent->dependent transformation:
template struct StructTestA<float>;
// Dependent->nondep transformation:
static_assert(StructTestA<float>::test_constexpr_struct_dep<char>()==4);


// Non-constexpr versions of some of above to test Codegen:

template<typename T>
struct StructTest_runtime {
  template<typename U>
  int test_constexpr_struct_dep() {
    constexpr Struct<T, U> s;
    int i = 0;
    float f = 0;
    template for (constexpr auto x : struct s) {
      f += x;
      ++i;
    }
    template for (constexpr auto x : struct s) {
      template for (constexpr auto x : struct s)
        ;
    }
    return i;
  }
};
// Dependent->dependent transformation:
template struct StructTest_runtime<float>;


template<typename T, typename U>
struct struct_exp_tester_runtime {
  int f(T s, U s2) {
    int res = 0;
    float f = 0;
    template for (auto x : struct s) {
      f += x;
      ++res;
    }
    template for (auto x : struct s) {
      template for (auto y : struct s2) {
        f += x;
        f += y;
        ++res;
      }
      template for (auto y : struct s )
        ;
    }
    return res;
  }
};

template<typename T, typename U>
struct dep_to_dep_transf_runtime : struct_exp_tester_runtime<Struct<T,U>, StructB> {
  using base = struct_exp_tester_runtime<Struct<T,U>, StructB>;

  int f(Struct<T,U> s = {}, StructB s2 = {}) {
    return base::f(s, s2);
  }
};

int main() {
  assert(StructTest_runtime<float>().test_constexpr_struct_dep<char>() == 4);
  assert((dep_to_dep_transf_runtime<int, char>().f() == 12));
}