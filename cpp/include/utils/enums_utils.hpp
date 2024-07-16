#pragma once

#include "YggInterface_export.h"
#include "utils/enums_maps.hpp"

YGG_API extern HEAD_FLAG_TYPE HEAD_BUFFER_MASK;

namespace YggInterface {
  namespace utils {

    YGG_API std::string str_toupper(const std::string& inStr);
    YGG_API std::string str_tolower(const std::string& inStr);
    template<typename T1>
    bool enum_key_search(const std::map<const T1, const std::string> map,
			 const T1& key,
			 std::string& val) {
      for (typename std::map<const T1, const std::string>::const_iterator it = map.cbegin();
	   it != map.cend(); it++) {
	if (it->first == key) {
	  val = it->second;
	  return true;
	}
      }
      return false;
    }
			 
    template<typename T1>
    bool enum_value_search(const std::map<const T1, const std::string> map,
			   const std::string& val,
			   T1& key, bool allow_anycase=false,
			   std::string prefix="",
			   std::string suffix="") {
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


    template<typename T, typename Tflag>
    std::string strBitFlags(const T& flag,
			    const std::map<const Tflag, const std::string>& map,
			    const std::string& prefix="",
			    const std::string& suffix="\n") {
      std::string out;
      for (typename std::map<const Tflag, const std::string>::const_iterator it = map.begin();
	   it != map.end(); it++) {
	if (flag & it->first)
	  out += prefix + it->second + suffix;
      }
      return out;
    }
  }
}
