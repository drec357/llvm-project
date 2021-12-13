#include <experimental/mirror>
#include <iostream>

enum class weekdays : int {
  monday, tuesday, wednesday, thursday, friday, saturday, sunday
};

static weekdays next_day(weekdays d) {
  return weekdays((int(d) + 1) % 7);
}

static void print_next_day(std::string_view n) {
  namespace m = std::experimental::mirror;
  auto d = m::string_to_enum<weekdays>(n);
  std::cout << n 
            << " -> "
            << m::enum_to_string(next_day(d))
            << std::endl;
}

int main() {
  namespace m = std::experimental::mirror;
  m::for_each(m::get_enumerators(mirror(weekdays)), [](auto mo){
      print_next_day(m::get_name(mo));
    });

  return 0;
}
