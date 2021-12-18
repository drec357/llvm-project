// RUN: %clang_cc1 -std=c++2a -ftemplate-for -verify %s
// expected-no-diagnostics

#define assert(expr) ((expr) ? (void)(0) : __builtin_abort())

constexpr int simpletest0() {
  int i = 0;
  int arr[] = {};
  template for (int elem : arr) {
    ++i;
  }
  return i;
}
static_assert(simpletest0() == 0);

constexpr int simpletest1() {
  int i = 0;
  int arr[] = {1, 2, 3};
  template for (int elem : arr)
    i += elem;
  return i;
}
static_assert(simpletest1() == 6);


constexpr bool canTakeAddressOf_YesRefElem_NoValElem() {
  int arr[] = {1, 2, 3};
  int *ptrToRefElem = nullptr, *ptrToValElem = nullptr;
  template for (int &refElem : arr) {
    ptrToRefElem = &refElem;
  }
  template for (int valElem : arr) {
    ptrToValElem = &valElem;
  }
//  const int i = *ptrToVal; //Expected error

  return (ptrToRefElem == &arr[2] &&
          ptrToValElem != &arr[2]);
}
static_assert(canTakeAddressOf_YesRefElem_NoValElem());


constexpr bool illegal_address_if_constexpr() {
  constexpr int arr[] = {1, 2, 3};
  const int *ptr = nullptr, *ptrB = nullptr;
//  template for (constexpr int &elem : arr) ; //Expected error
  template for (constexpr int elem : arr)
    ptrB = &elem;
//  const int i = *ptrB; //Expected error

  return true;
}
static_assert(illegal_address_if_constexpr());


constexpr int test1() {
  int i = 0, dummy = 0;
  int arr[] = {1, 2, 3};
  int arrB[] = {1};

  template for (int elem : arr) {
    int j = 0;
    template for (int elem : arrB)
      i += elem;
    template for (int elem : arrB) {
      dummy += (j + i);
      template for (int elem : arr)
        dummy += (j + i);
    }
  }
  return i;
}
static_assert(test1() == 3);


constexpr int nested_index_test_nonconstexpr() {
  int num = 0, asum = 0;
  int arr[] = {1, 2, 3};
  int arrB[] = {1};
  template for (int a : arr) {
    template for (int b : arrB) {
      ++num;
      asum += a;
    }
  }
  return num + asum;
}
static_assert(nested_index_test_nonconstexpr() == 9);

constexpr int nested_index_test_constexpr() {
  int num = 0, asum = 0;
  constexpr int arr[] = {1, 2, 3};
  constexpr int arrB[] = {1};
  template for (constexpr int a : arr) {
    int j = a;
    template for (constexpr int b : arrB) {
      ++num;
      asum += a;
    }
    int k = a;
  }
  return num;
}
static_assert(nested_index_test_constexpr() == 3);

constexpr int misc_array_types() {
  int i = 1;
  int arr[] = {1, 2, 3};
  auto &&arrB = arr;
  template for (auto elem : arrB) {
    i -= elem;
  }
  int(&arrC)[3] = arrB;
  template for (int elem : arrC) {
    i += 2 * elem;
    template for (int elem : arrC);
  }
  return i;
}
static_assert(misc_array_types() == 7);

constexpr int global_arr3[] = {3, 4, 5};
constexpr auto &&getGlobalArr3RRef() { return global_arr3; }

constexpr int test_constexpr_global_array_1() {
  int res = 0;
  template for (constexpr int a : global_arr3) {
    res += a;
    template for (constexpr int a : getGlobalArr3RRef())
      ;
  }
  return res;
}
static_assert(test_constexpr_global_array_1() == 12);

constexpr int global_arr2[] = {1, 2};
constexpr int test_constexpr_global_array_2() {
  int res = 0;
  template for (constexpr int a : global_arr3) {
    template for (constexpr int b : global_arr2)
      res += (a + b);
  }
  return res;
}
static_assert(test_constexpr_global_array_2() == 33);

template<int I>
struct MyStruct {
    static constexpr int member_arr2_static[2] = {2, 4};
    constexpr auto&& get_member_arr2_static() const { return member_arr2_static; }
    int member_arr2_nonstatic[4] = {2, 4};
    constexpr auto&& get_member_arr2_nonstatic() const { return member_arr2_nonstatic; }

    int get() const { return I; }
    constexpr int getB() const { return I; }
    int getC() { return I; }
    int j = I;
    static int K;
    static const int L = I;
};

template<int I>
int MyStruct<I>::K = 1;


// FIXME more tests using constexpr arrays
//
// This test is important, as a special change was required to make it work.
// Search "NonGlobalRefs" in clang, anything added there was needed to allow
// constexpr to be added without an error (due to the arrays being interpreted
// as pointers)
//
// Also, it is important to have a non-constexpr function compilatio tests
// to make sure CodeGen works, since some unusual stuff is required there
// as well, particularly in the case of nested expansions, which this test
// handles as well.
//
// FIXME break this into separate tests

int test_constexpr_array() {
  int i = 0;
  constexpr int arr4[] = { 1, 2, 3, 4 };
  constexpr int arr2[] = { 2, 4 };
  constexpr int arr1[] = { 2 };
  constexpr int arr0[] = {};

  // Zero-length arrays
  template for (constexpr int elem : arr0)
    ++i;
  template for (constexpr int elem : arr0) {
    template for (constexpr int elem : arr0) {
      ++i;
      template for (constexpr int elem : arr2)
        ;
    }
  }

  // Null statement bodies
  template for (constexpr int elem : arr0)
    ;
  template for (constexpr int elem : arr2)
    ;
  template for (constexpr int elem : arr2) {
    // IMPORTANT TEST: *Nested* expansion with null body; CodeGen
    // needs to handle this case specially to avoid an assert fail.
    template for (constexpr int elem : arr2)
      ;
  }

  // Loop variables used as template arguments within body
  int j;
  template for (constexpr int a : arr2) {
    template for (constexpr int b : arr2) {


      template for (constexpr int c : arr2) {
        constexpr MyStruct<a> mconstA;
        template for (int elem : mconstA.get_member_arr2_nonstatic()) ;
        constexpr MyStruct<1> mconstB;
        template for (int elem : mconstB.get_member_arr2_nonstatic()) ;
        constexpr MyStruct<c> mconstC;
        template for (int elem : mconstC.get_member_arr2_nonstatic()) ;
        constexpr MyStruct<b> mconst;

        auto &&arr = mconst.get_member_arr2_nonstatic();
        template for (int elem : arr) ;
        template for (int elem : mconst.get_member_arr2_nonstatic()) {
          j = mconst.getB();
        }
      }
      constexpr MyStruct<b> mconst;
      j = mconst.get();
      template for (constexpr int elem : mconst.get_member_arr2_static()) ;
      template for (int elem : mconst.get_member_arr2_static()) ;
      template for (constexpr int elem : mconst.get_member_arr2_nonstatic()) ;
      auto &&arr = mconst.get_member_arr2_nonstatic();
      template for (int elem : arr) ;
      template for (int elem : mconst.get_member_arr2_nonstatic()) ;
    }
  }
  return j;
}

template<int I>
struct OuterClass {
  template<typename U>
  int lambda_testA(U u) {
    int res = 0;
    constexpr int arr2[] = { 2, 4 };

    auto Lambda = [&](int v) {
      template for (constexpr auto a : arr2)
        template for (constexpr auto b : arr2)
          ++res;

      template for (constexpr auto a : arr2)
        ;

      template for (constexpr auto b : arr2) {
        constexpr MyStruct<b> mconst;
        template for (constexpr int elem : mconst.get_member_arr2_static())
          ++res;
      }
      template for (auto b : arr2) {
        constexpr MyStruct<I> mconst;
        template for (constexpr int elem : mconst.get_member_arr2_static())
          res = b;
      }
    };
    Lambda(u);
    return res;
  }
};

template struct OuterClass<3>;


int nondep_lambda_test(int u) {
  int res = 0;
  constexpr int arr2[] = { 2, 4 };

  auto Lambda = [&](int v) {
    template for (constexpr auto a : arr2)
      template for (constexpr auto b : arr2)
        res = b + v;

    template for (constexpr auto a : arr2)
      ;
  };
  Lambda(u);
  return res;
}

template<typename U>
int dep_testA(U u) {
  int res = 0;
  constexpr int arr2[] = { 2, 4 };

  template for (constexpr auto a : arr2)
    template for (constexpr auto b : arr2)
      res = b + u;

  template for (constexpr auto a : arr2)
    ;

  return res;
}

template<typename L>
constexpr void call(L &&l, int i) noexcept { l(i); }

struct Functor {
  template<typename V>
  constexpr void operator()(V v) const {
    constexpr V arr2[] = { 2, 4 };
    template for (constexpr auto c : arr2)
      ;
  }
};

static const auto StaticLambda = []<typename V>(V v) {
  constexpr int arr2[] = { 2, 4 };
  template for (constexpr auto c : arr2)
    ;
};

void dep_lambda_testA(int u) {
  int res = 0;
  constexpr auto Lambda = [&]<typename V>(V v) {
    constexpr int arr2[] = { 2, 4 };
    template for (constexpr V c : arr2)
      int res = c + v; //<^ this expansion stmt doesn't instantiate
  };
  constexpr Functor functor{};
  constexpr int arr2[] = { 2, 4 };
  template for (constexpr int d : arr2) {
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
    constexpr int arr2[] = { 2, 4 };
    template for (constexpr V c : arr2)
      int res = c + v; //<^ this expansion stmt doesn't instantiate
  };
  constexpr Functor functor{};
  constexpr int arr2[] = { 2, 4 };
  template for (constexpr int d : MyStruct<U{}>().get_member_arr2_static()) {
    Lambda(d);
    Lambda(u);
    StaticLambda(u);
    call(Lambda, d); call(Lambda, u); call(StaticLambda, u);
    functor((float)d)     ; functor(u)     ; Functor()(3.3);
  }
}

int main() {
  assert(test_constexpr_array() == 4);
  OuterClass<3> o;
  assert(o.lambda_testA(0) == 4);
  assert(nondep_lambda_test(5)==9);
  assert(dep_testA(5)==9);
  dep_lambda_testA(5);
  dep_lambda_testB(5);
  return 0;
}