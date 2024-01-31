//
// Created by friedel on 8/22/22.
//

#include "utils/tools.hpp"

#include <random>

// https://stackoverflow.com/questions/440133/how-do-i-create-a-random-alpha-numeric-string-in-c
std::string YggInterface::utils::random_string(std::string::size_type length,
					       bool numeric) {
  static size_t N_numeric = 10, N_all = 62;
  static const char* chrs = "0123456789"
    "abcdefghijklmnopqrstuvwxyz"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  std::string s;
  s.reserve(length);
  size_t N = N_all;
  if (numeric) {
    N = N_numeric;
  }
  thread_local static std::mt19937 rg{std::random_device{}()};
  thread_local static std::uniform_int_distribution<std::string::size_type> pick(0, N - 2);
  while(length--)
    s += chrs[pick(rg)];
  if (numeric && s[0] == '0')
    s[0] = '1';
  return s;
}

