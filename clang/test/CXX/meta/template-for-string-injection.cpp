// RUN: %clang_cc1 -std=c++2a -fstring-injection -ftemplate-for -verify %s
// expected-no-diagnostics

/*
 * Note: you don't actually need `template for` intrinsically to use __inj()/__injf();
 * ordinary `for` etc. work fine.  `template for` is mostly useful for iterating
 * over reflections, particularly in implementations that rely on separate instantiations
 * for each reflected entity.
 * However if you want to inject while iterating over reflections this just tests that
 * there are no issues using __inj in `template for` on its own.
 */

// --- Dummy tuple setup --- //
template<typename T, typename U, typename V>
struct tuple
{
  T t;
  U u;
  V v;
};
template<typename T, typename U, typename V, int N>
struct getter { constexpr static void get(tuple<T,U,V> const& t) { } };
template<typename T, typename U, typename V>
struct getter<T,U,V,0> { constexpr static T get(tuple<T,U,V> const& t) { return t.t; } };
template<typename T, typename U, typename V>
struct getter<T,U,V,1> { constexpr static U get(tuple<T,U,V> const& t) { return t.u; } };
template<typename T, typename U, typename V>
struct getter<T,U,V,2> { constexpr static V get(tuple<T,U,V> const& t) { return t.v; } };

template<int N, typename T, typename U, typename V>
constexpr inline decltype(auto)
get(tuple<T,U,V> const& t) {
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
  struct tuple_size<::tuple<T,U,V>> {
      static constexpr int value = 3;
  };
} //std


template<typename TUP>
consteval void declareNumPositive(TUP &&tup, const char *countName, int init = 0) {
  __injf("int {} = {};", countName, init);
  template for (auto elem : tup) {
    if (elem > 0)
      __injf("++{};", countName);
  }
}

constexpr int testInjTupleFor() {
  consteval {
    declareNumPositive(tuple<int, float, char>{10, 3.14, 'a'},
                       "count");
    __injf("return count;");
  }
}
static_assert(testInjTupleFor() == 3);