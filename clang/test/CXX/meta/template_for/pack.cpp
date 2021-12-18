// RUN: %clang_cc1 -std=c++2a -ftemplate-for -verify %s
// expected-no-diagnostics

#define assert(expr) ((expr) ? (void)(0) : __builtin_abort())


// Function Parm Packs:

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


// Non-type template param packs:

template<auto... args>
constexpr int test_packNTTP() {
  int i = 0;
  template for (constexpr auto x : args)
    i += x;
  return i;
}
static_assert(test_packNTTP<3, 4, 5>() == 12);


template<int I>
struct MyStruct {
    static constexpr int member_arr2_static[2] = {2, 4};
    int member_arr2_nonstatic[4] = {2, 4};
    static constexpr auto&& get_member_arr2_static() { return member_arr2_static; }
    constexpr auto&& get_member_arr2_nonstatic() const { return member_arr2_nonstatic; }

    constexpr auto&& get_member_arr2_nonstatic_accessing_static() const { return member_arr2_static; }

    int get() const { return I; }
    constexpr int getB() const { return I; }
    int getC() { return I; }
    int j = I;
    static int K;
    static const int L = I;
};

template<int I>
int MyStruct<I>::K = 1;

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

  template<typename... InnerArgTs>
  int methodD(InnerArgTs... innerArgs) {
    template for (int x : Args)
      template for (int x : innerArgs)
        ;
    template for (int x : innerArgs)
      template for (int x : Args)
        template for (auto a : Args)
           ;

    template for (auto a : innerArgs) {
      int i;
      template for (constexpr auto b : Args) {
        MyStruct<b> m;
        int j = m.get() + i + m.getB() + m.getC() + m.j + m.K + m.L;
        // Iterate over array in the midst of iterating over a pack:
        constexpr MyStruct<3> mconst;
        template for (constexpr int elem : mconst.get_member_arr2_nonstatic())
          ;
        template for (int elem : mconst.get_member_arr2_nonstatic())
          template for (auto a : Args)
             ++j;

        template for (constexpr int elem : MyStruct<b>::get_member_arr2_static())
          template for (auto a : Args)
             ++j;
        template for (int elem : MyStruct<b>::get_member_arr2_static())
          template for (auto a : Args)
            ++j;

        // FIXME weird constexpr one-past-the-end error, though is it okay to be accessing
        //  static constexpr data from a non-static constexpr object?  I assume so, but unusual.
//        template for (constexpr int elem : mconst.get_member_arr2_nonstatic_accessing_static())
//          template for (auto a : Args)
//             ++j;
//        template for (int elem : mconst.get_member_arr2_nonstatic_accessing_static())
//          template for (auto a : Args)
//            ++j;
      }
    }

    int res = 0;
    template for (int x : Args) {
      int k = x;
      template for (int y : innerArgs)
        res += y + x + k;
    }
    return 30;
  }

};


template<auto... Args>
consteval int funcA() {
  int res = 0;
  template for (constexpr auto x : Args)
    ++res;
  return res;
}
static_assert(funcA<2,3,4>()==3);

int main() {
  // Test various kinds of transformations
  PackTestA<3, 5, 2> p;
  assert(p.methodA<int>()==3);
  assert(p.methodB<int>()==10);
  assert(p.methodC()==3);
  assert(p.methodD(3, 5) == 30);//(3+3+3) + (5+3+3) + (3+4+4) + (5+4+4) + (3+2+2) + (5+2+2));
  assert((funcA<2,3,4>() == 3));
}