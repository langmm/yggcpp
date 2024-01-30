#include "utils/enums_utils.hpp"
#include "utils/enums.hpp"
#include <istream>
#include <iostream>
#include <algorithm>

int HEAD_BUFFER_MASK = (HEAD_FLAG_ALLOW_REALLOC |
			HEAD_FLAG_OWNSDATA);

using namespace YggInterface::utils;

std::string str_toupper(const std::string& inStr) {
  std::string outStr(inStr);
  std::transform(inStr.begin(), inStr.end(), outStr.begin(),
		 [](unsigned char c) { return std::toupper(c); });
  return outStr;
}

std::string str_tolower(const std::string& inStr) {
  std::string outStr(inStr);
  std::transform(inStr.begin(), inStr.end(), outStr.begin(),
		 [](unsigned char c) { return std::tolower(c); });
  return outStr;
}

template<typename T1>
bool enum_value_search(const std::map<const T1, const std::string> map,
		       const std::string& val,
		       T1& key, bool allow_anycase,
		       std::string prefix,
		       std::string suffix) {
  for (typename std::map<const T1, const std::string>::const_iterator it = map.cbegin();
       it != map.cend(); it++) {
    if ((it->second == val) ||
	(allow_anycase && (val == str_toupper(it->second) ||
			   val == str_tolower(it->second))) ||
	((!prefix.empty()) && (val == (prefix + it->second))) ||
	((!suffix.empty()) && (val == (it->second + suffix)))) {
      key = it->first;
      return true;
    }
  }
  return false;
}

template<typename T1, typename T2>
T1 max_enum_value(const std::map<const T1, const T2> map) {
  return map.crbegin()->first;
}
