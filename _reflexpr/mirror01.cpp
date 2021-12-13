#include <experimental/mirror>
#include <iostream>

enum class weekdays : int {
  monday = 1, tuesday, wednesday, thursday, friday, saturday, sunday
};

int main() {
  using namespace std::experimental::mirror;
  for_each(get_enumerators(mirror(weekdays)), [](auto mo){
      std::cout << get_name(mo)
                << ": "
                << int(get_constant(mo))
                << std::endl;
    });

  std::cout << enum_to_string(weekdays::monday) << std::endl;
  std::cout << enum_to_string(weekdays::tuesday) << std::endl;
  std::cout << enum_to_string(weekdays::wednesday) << std::endl;
  std::cout << enum_to_string(weekdays::thursday) << std::endl;
  std::cout << enum_to_string(weekdays::friday) << std::endl;
  std::cout << enum_to_string(weekdays::saturday) << std::endl;
  std::cout << enum_to_string(weekdays::sunday) << std::endl;
  return 0;
}
