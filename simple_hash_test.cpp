#include <algorithm>  // std::copy
#include <cassert>    // assert
#include <functional> // std::hash
#include <iostream>   // std::cout

#include "hash.hpp"

enum class color { black, white, red, yellow, green, blue, pink, orange, gray };

struct hash_color {
    int operator()(const color& c) {
        return static_cast<int>(c);
    }
};

auto copy_hash_table(const container::hash_table<int, double, 20>& hasht) {
    container::hash_table<int, double, 20> new_hasht;
    std::for_each(hasht.begin(), hasht.end(), [&new_hasht](auto val) {
        new_hasht.insert(val);
    });
    for (auto i : new_hasht) {
        std::cout << i.first << std::endl;
    }
}

int main() {
    container::hash_table<color, int, 10, hash_color> hasht;
    assert(0 == hasht.size());
    assert(true == hasht.empty());

    auto val1 = hasht.insert(std::pair(color::black, 50));
    auto val2 = hasht.insert(std::pair(color::white, 200));
    auto val3 = hasht.insert(std::pair(color::red, 2000));
    auto val4 = hasht.insert(std::pair(color::yellow, 13));
    auto val5 = hasht.insert(std::pair(color::green, 55));
    auto val6 = hasht.insert(std::pair(color::blue, 100));
    auto val7 = hasht.insert(std::pair(color::pink, -4));
    auto val8 = hasht.insert(std::pair(color::orange, -85000));
    auto val9 = hasht.insert(std::pair(color::gray, 114236));

    assert(false == hasht.empty());
    assert(9 == hasht.size());
    for (auto i : hasht) {
        std::cout << i.second << std::endl;
    }

    hasht[color::red] = 0;
    std::cout << (hasht.find(color::red))->second << std::endl;

    container::hash_table<int, double, 20> hasht2;
    auto val10 = hasht2.insert(std::pair(0, 0.0));
    auto val11 = hasht2.insert(std::pair(1, 1.0));
    copy_hash_table(hasht2);

    auto const_iter = hasht2.cbegin();
    const_iter = hasht2.end();
    container::hash_table<int, double, 20>::const_iterator other_const_iter =
        hasht2.begin();

    return 0;
}