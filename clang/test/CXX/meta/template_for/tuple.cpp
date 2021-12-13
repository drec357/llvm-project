// RUN: %clang_cc1 -std=c++2a -ftemplate-for -verify %s
// expected-no-diagnostics

#define assert(expr) ((expr) ? (void)(0) : __builtin_abort())

static_assert(__has_extension(template_for), "");

// --- Dummy tuple setup --- //
// Overload std::get and specialize std::tuple_size
// for our class to be considered a mytuple.
template<typename T, typename U, typename V>
struct mytuple
{
  T t = (T)10;
  U u = (U)3.14;
  V v = (V)'a';
};
template<typename T, typename U, typename V, int N>
struct getter { constexpr static void get(mytuple<T,U,V> const& t) { } };
template<typename T, typename U, typename V>
struct getter<T,U,V,0> { constexpr static T get(mytuple<T,U,V> const& t) { return t.t; } };
template<typename T, typename U, typename V>
struct getter<T,U,V,1> { constexpr static U get(mytuple<T,U,V> const& t) { return t.u; } };
template<typename T, typename U, typename V>
struct getter<T,U,V,2> { constexpr static V get(mytuple<T,U,V> const& t) { return t.v; } };

template<int N, typename T, typename U, typename V>
constexpr inline decltype(auto)
get(mytuple<T,U,V> const& t) {
  return getter<T,U,V,N>::get(t);
}
namespace std {
  template<typename T>
  struct tuple_size;

  template<typename T>
  struct tuple_size<T const> : tuple_size<T> {
  };

  template<typename T>
  struct tuple_size<T volatile> : tuple_size<T> {
  };

  template<typename T>
  struct tuple_size<T const volatile> : tuple_size<T> {
  };

  template<typename T, typename U, typename V>
  struct tuple_size<::mytuple<T,
                            U,
                            V>> {
      static constexpr int value = 3;
  };
} //std


// --- Tests --- //
using tuple3 = mytuple<int, float, char>;
static constexpr float tuple3_manually_calcd_sum = tuple3().t + tuple3().u + tuple3().v;

constexpr float test_tuple() {
  tuple3 tup;
  float sum = 0;
  template for (auto x : tup)
    sum += x;
  sum = 0;
  template for (auto x : tup) {
    template for (auto x : tup)
      sum += x;
  }
  return sum;
}
static_assert(test_tuple() < std::tuple_size<tuple3>::value * tuple3_manually_calcd_sum + 0.001);
static_assert(test_tuple() > tuple3_manually_calcd_sum * 3 - 0.001); //

void test_static_constexpr_tuple() {
  static constexpr tuple3 tup;
  int i = 0;
  template for (constexpr auto x : tup)
    ++i;
  assert(i==3);

  template for (constexpr auto x : tup) {
    template for (constexpr auto x : tup) {
      template for (constexpr auto x : tup)
        ;
    }
  }
}

template<int I>
struct SomeTemplate {};

constexpr int test_nonstatic_constexpr_tuple() {
  constexpr mytuple<int,int,int> tup{};
  int i = 0;
  template for (constexpr auto x : tup) {
    SomeTemplate<x> m;
    ++i;
  }
  template for (constexpr auto x : tup) {
    template for (constexpr auto x : tup)
      ++i;
  }
  return i;
}
static_assert(test_nonstatic_constexpr_tuple() == 12);

//Dependent
template<typename T>
struct TupleTestA {
  template<typename U, typename V>
  constexpr int test_tuple() {
    mytuple<T,U,V> tupNonCE;
    int i = 0;
    auto j = T(0) + U(0) + V(0);
    template for (auto x : tupNonCE) {
      auto x2 = x + j;
      template for (auto x : tupNonCE) {
        j += (x + x2);
        ++i;
      }
    }
    return i;
  }
  template<typename U, typename V>
  constexpr int test_nonstatic_constexpr_tuple() {
    constexpr mytuple<T,U,V> tup;
    int i = 0;
    template for (constexpr auto x : tup)
      ++i;
    template for (constexpr auto x : tup) {
      template for (constexpr auto x : tup)
        ++i;
    }
    return i;
  }
};
// Dependent->dependent transformation
template struct TupleTestA<int>;
// Dependent->non-dependent transformation
static_assert(TupleTestA<int>().test_tuple<float, char>() == 9);
static_assert(TupleTestA<int>().test_nonstatic_constexpr_tuple<float, char>() == 12);


int main() {
  test_static_constexpr_tuple();
}