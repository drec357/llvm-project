// RUN: %clang_cc1 -std=c++2a -ftemplate-for -verify %s
// expected-no-diagnostics

#define assert(expr) ((expr) ? (void)(0) : __builtin_abort())

// ---Dummy range setup --- //
// Overload std::distance and std::next
// for our class to be considered a range.
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
};
struct counter
{
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

  // constexpr static int size() { return N; }
};


// --- Tests --- //
void test_static_constexpr_range() {
  static constexpr range<7> ints;
  int i = 0;
  template for (constexpr int n : ints)
    i += n;
  template for (constexpr int n : ints) {
    template for (constexpr int n : ints)
      ;
  }
}

void test_nonstatic_constexpr_range() {
  constexpr range<7> ints;
  template for (constexpr int n : ints)
    ;
  template for (constexpr int n : ints) {
    template for (constexpr int n : ints)
      ;
  }
}

template<typename T>
struct RangeTestA {
  template<int I>
  T test_nonstatic_constexpr_range() {
    constexpr range<I> ints;
    T i = 0;
    template for (constexpr int n : ints)
      ++i;
    template for (constexpr int n : ints) {
      template for (constexpr int n : ints)
        ++i;
    }
    return i;
  }
};

int main() {
  test_static_constexpr_range();
  // Dependent->dependent transformation:
  struct RangeTestA<int> oa;
  // Dependent->nondep transformation:
  assert(oa.test_nonstatic_constexpr_range<7>() == 7 * 8);
}
