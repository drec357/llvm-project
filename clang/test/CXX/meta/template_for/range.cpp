// RUN: %clang_cc1 -std=c++2a -ftemplate-for -verify %s
// expected-no-diagnostics

#define assert(expr) ((expr) ? (void)(0) : __builtin_abort())

/*
 * Note: expansion over ranges must *always* use constexpr loop variables
 * (unlike constant arrays and tuples and packs, since their size is known
 * without requiring constexpr computation).
 */

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


template<int I>
struct MyStruct {
    static constexpr range<2> member_range2_static;
    constexpr range<2> get_member_range2_static() const { return member_range2_static; }
    range<2> member_range2_nonstatic = {};
    constexpr range<2> get_member_range2_nonstatic() const { return member_range2_nonstatic; }
};


template<typename T>
struct RangeTestB {
  template<T I>
  T test_nonstatic_constexpr_range() {
    constexpr range<I> ints;
    int i = I;
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
    for (constexpr int n : ints)
      ++i;
    return I;
  }
};

int test_constexpr_range() {
  int i = 0;
  constexpr range<4> range4;
  constexpr range<2> range2;
  constexpr range<1> range1;
  constexpr range<0> range0;

  // Zero-length ranges
  template for (constexpr int elem : range0)
    ++i;
  template for (constexpr int elem : range0) {
    template for (constexpr int elem : range0) {
      ++i;
      template for (constexpr int elem : range2)
        ;
    }
  }

  // Null statement bodies
  template for (constexpr int elem : range0)
    ;
  template for (constexpr int elem : range2)
    ;
  template for (constexpr int elem : range2) {
    // IMPORTANT TEST: *Nested* expansion with null body; CodeGen
    // needs to handle this case specially to avoid an assert fail.
    template for (constexpr int elem : range2)
      ;
    template for (constexpr int elem : range0)
      ;
  }

  // Loop variables used as template arguments within body
  int j;
  template for (constexpr int a : range2) {
    template for (constexpr int b : range2) {

      template for (constexpr int c : range2) {
        constexpr MyStruct<a> mconstA;
        constexpr range<2> range2 = mconstA.member_range2_nonstatic;
        template for (constexpr int elem : range2) ;
        template for (constexpr int elem : mconstA.member_range2_nonstatic) ;

        constexpr MyStruct<1> mconstB;
        template for (constexpr int elem : mconstB.get_member_range2_nonstatic()) ;
        template for (constexpr int elem : mconstB.get_member_range2_static()) ;

        constexpr MyStruct<c> mconstC;
        template for (constexpr int elem : mconstC.get_member_range2_nonstatic()) ;

        constexpr MyStruct<b> mconst;
        template for (constexpr int elem : mconst.get_member_range2_nonstatic()) {
          j = elem * 4;
        }
      }
      constexpr MyStruct<b> mconst;
      template for (constexpr int elem : mconst.get_member_range2_static()) ;
      constexpr auto range = mconst.get_member_range2_static();
    }
  }
  return j;
}


template<int I>
struct OuterClass {
  template<typename U>
  int lambda_testA(U u) {
    int res = 0;
    constexpr range<2> range2;

    auto Lambda = [&](int v) {
      template for (constexpr auto a : range2)
        template for (constexpr auto b : range2)
          ++res;

      template for (constexpr auto a : range2)
        ;

      template for (constexpr auto b : range2) {
        constexpr MyStruct<b> mconst;
        template for (constexpr int elem : mconst.get_member_range2_static())
          ++res;
      }
      template for (constexpr auto b : range2) {
        constexpr MyStruct<I> mconst;
        template for (constexpr int elem : mconst.get_member_range2_static())
          res = b * 4;
      }
    };
    Lambda(u);
    return res;
  }
};


template<typename L>
constexpr void call(L &&l, int i) noexcept { l(i); }

struct Functor {
  template<typename V>
  constexpr void operator()(V v) const {
    constexpr range<2> range2;
    template for (constexpr auto c : range2)
      ;
  }
};

static const auto StaticLambda = []<typename V>(V v) {
  constexpr range<2> range2;
  template for (constexpr auto c : range2)
    ;
};

void dep_lambda_testA(int u) {
  int res = 0;
  constexpr auto Lambda = [&]<typename V>(V v) {
    constexpr range<2> range2;
    template for (constexpr V c : range2)
      int res = c + v; //<^ this expansion stmt doesn't instantiate
  };
  constexpr Functor functor{};
  constexpr range<2> range2;
  template for (constexpr int d : range2) {
    Lambda(d);
    Lambda(u);
    StaticLambda(u);
    call(Lambda, d); call(Lambda, u); call(StaticLambda, u);
    functor((float)d)     ; functor(u)     ; Functor()(3.3);
  }
}

template<typename U>
void dep_lambda_testB(U u) {
  int res = 0;
  constexpr auto Lambda = [&]<typename V>(V v) {
    constexpr range<2> range2;
    template for (constexpr V c : range2)
      int res = c + v; //<^ this expansion stmt doesn't instantiate
  };
  constexpr Functor functor{};
  constexpr range<2> range2;
  template for (constexpr int d : MyStruct<U{}>().get_member_range2_static()) {
    Lambda(d);
    Lambda(u);
    StaticLambda(u);
    call(Lambda, d); call(Lambda, u); call(StaticLambda, u);
    functor((float)d)     ; functor(u)     ; Functor()(3.3);
  }
}

int main() {
  // Dependent->dependent transformation:
  struct RangeTestB<int> oa;
  // Dependent->nondep transformation:
  assert(oa.test_nonstatic_constexpr_range<7>() == 14);
  assert(oa.test_nonstatic_constexpr_rangeB<7>() == 7);

  assert(test_constexpr_range() != 0);
  assert(test_constexpr_range() == 4);
  OuterClass<3> o;
  assert(o.lambda_testA(0) == 4);
  dep_lambda_testA(5);
  dep_lambda_testB(5);
  return 0;
}
