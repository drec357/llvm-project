// RUN: %clang_cc1 -std=c++2a -ftemplate-for -verify %s
// expected-no-diagnostics

#define assert(expr) ((expr) ? (void)(0) : __builtin_abort())

// ---Dummy range setup --- //
struct counter {
  constexpr explicit counter(int n) : num(n) { }

  constexpr counter& operator++() { ++num; return *this; }
  constexpr counter operator++(int) { counter x(*this); ++num; return x; }

  constexpr int operator*() const { return num; }

  constexpr friend bool operator==(counter a, counter b) { return a.num == b.num; }
  constexpr friend bool operator!=(counter a, counter b) { return a.num != b.num; }

  int num;
};
template<int N>
struct range
{
  constexpr counter begin() const { return counter(0); }
  constexpr counter end() const { return counter(N); }

//   constexpr static int size() { return N; }
};

// std::distance and std::next overloads
namespace std {
  template<typename I>
  constexpr int distance(I first, I limit) {
    int n = 0;
    while (first != limit) {
      ++n;
      ++first;
    }
    return n;
  }
  template<typename I>
  constexpr I next(I iter, int n = 1) {
    while (n != 0) {
      ++iter;
      --n;
    }
    return iter;
  }
}

template<typename T>
struct RangeTestB {
  template<int I>
  T test_nonstatic_constexpr_range() {
    constexpr range<I> ints;
    int i = 0;
    template for (constexpr int n : ints)
      ++i;
    return i;
  }
  // IMPORTANT TEST: instantiating loop that doesn't depend on template params.
  // Need to retain the proper outer layers of unsubstituted template arguments
  // or will crash.
  template<int I>
  T test_nonstatic_constexpr_rangeB() {
    constexpr range<3> ints;
    int i = 0;
    template for (constexpr int n : ints)
      ++i;
    return I;
  }
};

int main() {
  // Dependent->dependent transformation:
  struct RangeTestB<int> oa;
  // Dependent->nondep transformation:
  assert(oa.test_nonstatic_constexpr_range<7>() == 7);
  assert(oa.test_nonstatic_constexpr_rangeB<7>() == 7);
}
