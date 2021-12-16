// RUN: %clang_cc1 -std=c++2a -ftemplate-for -verify %s
// expected-no-diagnostics

#define assert(expr) ((expr) ? (void)(0) : __builtin_abort())

template<typename... Targs>
constexpr int test_pack(Targs... args) {
  int i = 0;
  template for (auto x : args)
    i += x;

  int j = 0;
  template for (auto x : args) {
    int k = x;
    template for (auto y : args) {
      j += (y + x + k);
      template for (auto x : args) {
        j += k + y + x + i;
        ++i;
      }
    }
  }
  if (j > 0)
    return i;
  return 0;
}
static_assert(test_pack(3, 4, 5, 6, 7) == (3+4+5+6+7) + (5*5*5));

template<auto... Args>
struct PackTestA {
  template<typename T>
  int methodA() {
    T res = 0;
    template for (T x : Args)
      ;
    template for (T x : Args)
      template for (T x : Args)
        ;
    template for (T x : Args)
      ++res;
    return res;
  }

  template<typename T>
  T methodB() {
    T res = 0;
    template for (T x : Args)
      ;
    template for (T x : Args)
      template for (T x : Args)
        ;
    template for (T x : Args)
      ++x;
    return res;
  }

  int methodC() {
    int res = 0;
    template for (int x : Args)
      ;
    template for (int x : Args)
      template for (int x : Args)
        ;
    template for (int x : Args)
      ++res;
    return res;
  }
};

int main() {
  // Test various kinds of transformations
  PackTestA<3, 5, 2> p;
  assert(p.methodA<int>()==3);
  assert(p.methodB<int>()==10);
  assert(p.methodC()==3);
}