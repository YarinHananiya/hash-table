#include <functional> // std::hash

#include "hash.hpp"

int main() {
    ilrd::hash_table<int, int, 10, std::hash<int>> hasht;
    return 0;
}