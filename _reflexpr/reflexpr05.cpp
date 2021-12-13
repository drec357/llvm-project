#include <experimental/reflect>
#include <array>
#include <iostream>
#include <string_view>
#include <tuple>

enum class weekdays : int {
  monday = 1, tuesday, wednesday, thursday, friday, saturday, sunday
};

namespace meta = std::experimental::reflect;

template <typename E>
class enumerator_map_type {
private:
  template <typename... MEC>
  struct helper {
    static std::array<std::tuple<std::string_view, E>, sizeof...(MEC)>
    make_map() noexcept {
      return {{{meta::get_name_v<MEC>, meta::get_constant_v<MEC>}...}};
    }
  };
public:
  auto operator ()() const noexcept {
      return meta::unpack_sequence_t<
          helper,
          meta::get_enumerators_t<reflexpr(E)>
        >::make_map();
  }
};

template <typename E>
constinit const enumerator_map_type<E> enumerator_map{};

int main() {
  for(const auto& [name, value] : enumerator_map<weekdays>()) {
    std::cout << name << ": " << int(value) << std::endl;
  }
  return 0;
}
