// RUN: %clang_cc1 -std=c++2a -ftemplate-for -verify %s
// expected-no-diagnostics

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