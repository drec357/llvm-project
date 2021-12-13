#include <experimental/reflect>

enum class weekdays {
  monday = 1, tuesday, wednesday, thursday, friday, saturday, sunday
};

struct mystruct {
  constexpr mystruct() noexcept = default;
  constexpr mystruct(int i) noexcept
    : i{i} {}

  int i{0};
  float f{1.F};
};

template <typename T>
void foo(T x) {
  using namespace std::experimental::reflect;
  using mt = reflexpr(T);
  static_assert(Alias<mt>);
  static_assert(Type<get_aliased_t<mt>>);
}

template <typename T>
void bar(T x) {
  using namespace std::experimental::reflect;
  using mt = reflexpr(T);
  static_assert(Alias<mt>);
  static_assert(Enum<get_aliased_t<mt>>);
}

const int y = -1;

auto main() -> int {
  using namespace std::experimental::reflect;
  mystruct z{};

  static_assert(!Type<int>);
  using me = reflexpr();
  using mg = reflexpr(::);
  using mi = reflexpr(int);
  using ms = reflexpr(std);
  using mw = reflexpr(weekdays);
  using mm = reflexpr(mystruct);
  using my = reflexpr(y);
  using mz = reflexpr(z);

  static_assert(!Object<me>);
  static_assert(Object<mi>);
  static_assert(Object<ms>);
  static_assert(!ObjectSequence<mi>);
  static_assert(Type<mi>);
  static_assert(Type<mw>);
  static_assert(Type<mm>);
  static_assert(Enum<mw>);
  static_assert(Record<mm>);
  static_assert(Namespace<ms>);
  static_assert(GlobalScope<mg>);
  static_assert(GlobalScope<get_scope_t<ms>>);
  static_assert(ObjectSequence<get_enumerators_t<mw>>);
  static_assert(ObjectSequence<get_data_members_t<mm>>);
  static_assert(reflects_same_v<mg, get_scope_t<ms>>);
  static_assert(get_name_v<mi>[0] == 'i');
  static_assert(get_name_v<mi>[1] == 'n');
  static_assert(get_name_v<mi>[2] == 't');
  static_assert(get_name_v<mi>[3] == '\0');
  static_assert(get_source_line_v<mw> > 0);
  static_assert(get_size_v<get_enumerators_t<mw>> == 7);
  static_assert(Type<get_underlying_type_t<mw>>);
  static_assert(Constant<get_element_t<0, get_enumerators_t<mw>>>);
  static_assert(get_constant_v<get_element_t<0, get_enumerators_t<mw>>> == weekdays::monday);
  static_assert(get_size_v<get_enumerators_t<mw>> == 7);
  static_assert(get_size_v<get_data_members_t<mm>> == 2);
  static_assert(Variable<my>);
  static_assert(get_pointer_v<my>);
  static_assert(Variable<get_element_t<1, get_data_members_t<mm>>>);
  static_assert(Variable<mz>);
  static_assert(get_pointer_v<get_element_t<1, get_data_members_t<mm>>>);
  static_assert(get_size_v<get_constructors_t<mm>>);
  static_assert(Constructor<get_element_t<0, get_constructors_t<mm>>>);

  get_reflected_type_t<mi> i = -2;
  z.i = 3;

  foo(0);
  foo(mystruct{});
  foo(weekdays::monday);
  bar(weekdays::monday);

  if (get_source_file_name_v<ms> != nullptr) {
  }
  if (get_name_v<mi> != nullptr) {
  }
  if (get_display_name_v<mi> != nullptr) {
  }

  return i+
    (*get_pointer_v<my>)+
    (z.*get_pointer_v<get_element_t<0, get_data_members_t<mm>>>);
}
