// RUN: %clang_cc1 -std=c++2a -ftemplate-for -verify %s
// expected-no-diagnostics

#define assert(expr) ((expr) ? (void)(0) : __builtin_abort())

constexpr int test0() {
  int i = 0;
  int arr[] = {};
  template for (int elem : arr) {
    ++i;
  }
  return i;
}
static_assert(test0() == 0);

constexpr int test1() {
  int i = 0;
  int arr[] = {1, 2, 3};
  template for (int elem : arr)
    i += elem;
  return i;
}
static_assert(test1() == 6);


constexpr bool canTakeAddressOf_YesRefElem_NoValElem() {
  int arr[] = {1, 2, 3};
  int *ptrToRefElem, *ptrToValElem;
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
  const int *ptr, *ptrB;
//  template for (constexpr int &elem : arr) ; //Expected error
  template for (constexpr int elem : arr)
    ptrB = &elem;
//  const int i = *ptrB; //Expected error

  return true;
}
static_assert(illegal_address_if_constexpr());


//constexpr int test1() {
//  int i = 0, dummy = 0;
//  int arr[] = {1, 2, 3};
//  int arrB[] = {1};
//
//  template for (int elem : arr) {
//    int j = 0;
//    template for (int elem : arrB) //FIXME (index issues)
//      i += elem;
//    template for (int elem : arrB) { //FIXME (index issues)
//      dummy += (j + i);
//      template for (int elem : arr)
//        dummy += (j + i);
//    }
//  }
//  return 0;
//}
//static_assert(test1() == 3);


//constexpr int nested_index_test_nonconstexpr() {
//  int num = 0, asum = 0;
//  int arr[] = {1, 2, 3};
//  int arrB[] = {1};
//  template for (int a : arr) {
//    template for (int b : arrB) { //WARNING (BAD): index issues
//      ++num;
//      asum += a;
//    }
//  }
//  return num + asum;
//}
//static_assert(nested_index_test_nonconstexpr() == 9);

//constexpr int nested_index_test_constexpr() {
//  int num = 0, asum = 0;
//  constexpr int arr[] = {1, 2, 3};
//  constexpr int arrB[] = {1};
//  template for (constexpr int a : arr) {
//    template for (constexpr int b : arrB) { //ERROR index issues
//      ++num;
//      asum += a;
//    }
//  }
//  return num + asum;
//}
//static_assert(nested_index_test_constexpr() == 9);

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

//constexpr int global_arr2[] = {1, 2};
//constexpr int test_constexpr_global_array_2() {
//  int res = 0;
//  template for (constexpr int a : global_arr3) {
//    template for (constexpr int b : global_arr2) //FIXME (index issues)
//      res += (a + b);
//  }
//  return res;
//}
//static_assert(test_constexpr_global_array_2() == 33);

template<int I>
struct MyStruct {
    static constexpr int member_arr4_static[4] = {2, 4, 6, 8};
    constexpr auto&& get_member_arr4_static() const { return member_arr4_static; }
    int member_arr4_nonstatic[4] = {2, 4, 6, 8};
    constexpr auto&& get_member_arr4_nonstatic() const { return member_arr4_nonstatic; }

    int get() { return I; }
    constexpr int getB() { return I; }
    int getC() { return I; }
    int j = I;
    static int K;
    static const int L = I;
};

template<int I>
int MyStruct<I>::K = 1;

//FIXME NEED MORE TESTS USING CONSTEXPR ARRAYS
// This one is important, required special changes to constexpr checking
// (search NonGlobalRefs, VarDecl::setNonGlobalRefsInConstInitOkay() etc);
// need to make sure doing that doesn't break anything.
int test_constexpr_array() {
  int i = 0;
  constexpr int arr4[] = { 1, 2, 3, 4 };
  constexpr int arr2[] = { 2, 4 };
  template for (constexpr int elem : arr4)
    i += elem;
  template for (constexpr int a : arr4) {
    int j;
    template for (constexpr int b : arr4) {
      MyStruct<a + b> m;
      j += m.get() + i + m.getB() + m.getC()
          + m.j + m.K + m.L;
      constexpr MyStruct<a + b> mconst;
      template for (constexpr int elem : mconst.get_member_arr4_static())
        ;
      template for (int elem : mconst.get_member_arr4_static())
        ;
//      template for (constexpr int elem : mconst.get_member_arr4_nonstatic()) ; //expected error
      template for (int elem : mconst.get_member_arr4_nonstatic())
        ;
    }
//    template for (constexpr int c : arr2) //FIXME (index issues)
//      ;
  }
  return i;
}

int main() {
  assert(test_constexpr_array() == 10);
}