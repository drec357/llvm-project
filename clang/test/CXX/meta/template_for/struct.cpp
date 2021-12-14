// RUN: %clang_cc1 -std=c++2a -ftemplate-for -verify %s
// expected-no-diagnostics

#define assert(expr) ((expr) ? (void)(0) : __builtin_abort())

template <typename T, typename U>
struct Struct {
  const T i = T();
  const T j = T();
  const U k = U();

  struct something {
    const int f = 10;
  };

  const U l = U();
};

struct StructB { int z = 3; };

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
      ++i;
    }
    template for (auto y : struct s)
      ;
  }
  return res;
}
static_assert(test_struct()==4);

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

// Dependent test:



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

int main() {

}