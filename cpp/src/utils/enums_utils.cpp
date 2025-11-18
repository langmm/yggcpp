#include "utils/enums_utils.hpp"
#include "utils/enums.hpp"
#include "utils/tools.hpp"
#include <istream>
#include <iostream>
#include <algorithm>

HEAD_FLAG_TYPE HEAD_BUFFER_MASK = (HEAD_FLAG_ALLOW_REALLOC |
				   HEAD_FLAG_OWNSDATA);

std::string YggInterface::utils::str_toupper(const std::string& inStr) {
  std::string outStr(inStr);
  std::transform(inStr.begin(), inStr.end(), outStr.begin(),
		 [](unsigned char c) { return std::toupper(c); });
  return outStr;
}

std::string YggInterface::utils::str_tolower(const std::string& inStr) {
  std::string outStr(inStr);
  std::transform(inStr.begin(), inStr.end(), outStr.begin(),
		 [](unsigned char c) { return std::tolower(c); });
  return outStr;
}

std::string YggInterface::utils::getenv(const ENV_VAR& env) {
  std::string name = ENV_VAR_map().find(env)->second;
  return YggInterface::utils::getenv(name);
}

