//
// Created by friedel on 8/22/22.
//

#include "utils/tools.hpp"

#include <random>

// https://stackoverflow.com/questions/440133/how-do-i-create-a-random-alpha-numeric-string-in-c
std::string YggInterface::utils::random_string(std::string::size_type length) {
  static const char* chrs = "0123456789"
    "abcdefghijklmnopqrstuvwxyz"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  thread_local static std::mt19937 rg{std::random_device{}()};
  thread_local static std::uniform_int_distribution<std::string::size_type> pick(0, 60);
  std::string s;
  s.reserve(length);
  while(length--)
    s += chrs[pick(rg)];
  return s;
}

