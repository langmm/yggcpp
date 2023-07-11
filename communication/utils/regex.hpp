#pragma once
#include <regex>
#include <cstdio>
#include <cstring>
#include <iostream>
#define FMT_LEN 100

namespace communication {
namespace utils {

size_t find_match(const std::regex &regex, const std::string &to_match,
               const size_t& start, size_t &sind, size_t &eind);
// Currently unused
// static
// size_t find_match(const std::regex &regex, const std::string &to_match,
//                size_t &sind, size_t &eind) {
//     return find_match(regex, to_match, 0, sind, eind);
// }
// static
// size_t find_match(const std::string &regex_text, const std::string &to_match,
//                const size_t& start, size_t &sind, size_t &eind) {
//     std::regex regex(regex_text, std::regex::extended);
//     return find_match(regex, to_match, start, sind, eind);
// }
// static
// size_t find_match(const std::string &regex_text, const std::string &to_match,
//                size_t &sind, size_t &eind) {
//     return find_match(regex_text, to_match, 0, sind, eind);
// }
int find_match_c(const char *regex_text, const char *to_match,
		 size_t *sind, size_t *eind);


/*!
  @brief Count the number of times a regular expression is matched in a string.
  @param[in] regex_text constant character pointer to string that should be
  compiled into a regex.
  @param[in] to_match constant character pointer to string that should be
  checked for matches.
  @return int Number of matches found. -1 is returned if the regex could not be
  compiled.
*/
size_t count_matches(const std::regex &re, const std::string &to_match);

// Currently unused
// static
// size_t count_matches(const std::string &regex_text, const std::string &to_match) {
//     std::regex re(regex_text);
//     return count_matches(re,to_match);
// }


size_t find_matches(const std::string &regex_text, const std::string &to_match,
                 std::vector<size_t> &sind, std::vector<size_t> &eind);

size_t regex_replace(std::string &buf, const std::regex &re, const std::string &rp,
                     const size_t &nreplace=0);
static
size_t regex_replace(std::string &buf, const std::string &re, const std::string &rp,
                      const size_t &nreplace=0) {
    std::regex regex(re, std::regex::extended);
    return regex_replace(buf, regex, rp, nreplace);
}
static inline
int regex_replace_c(char *buf, const size_t len_buf,
		    const char *re, const char *rp,
		    const size_t nreplace) {
  std::string buff_s(buf);
  int out = (int)(regex_replace(buff_s, re, rp, nreplace));
  if ((buff_s.size() + 1) > len_buf)
    return -1;
  strncpy(buf, buff_s.c_str(), buff_s.size());
  buf[buff_s.size()] = '\0';
  return out;
}

const std::string sre_fmt = "%[^\t\n ]+[\t\n ]";
const std::string sre_fmt_eof = "%[^\t\n ]+";
const std::string FLOAT_STR = "%([[:digit:]]*)(\\.)?([[:digit:]]*)[eEfFgG]";
const std::regex RE_FMT(sre_fmt, std::regex::extended);
const std::regex RE_FMT_EOF(sre_fmt_eof, std::regex::extended);
const std::regex RE_STRING("%([[:digit:]])*s", std::regex::extended);  // string
#ifdef _WIN32
const std::regex RE_COMPLEX("(%.*[fFeEgG]){2}j", std::regex::extended);
#else
const std::regex RE_COMPLEX(FLOAT_STR + FLOAT_STR + "j", std::regex::extended);
#endif
const std::regex RE_FLOAT(FLOAT_STR, std::regex::extended);
const std::regex RE_CHAR("%([[:digit:]]*)hh[id]", std::regex::extended);
const std::regex RE_SHORT("%([[:digit:]]*)h[id]", std::regex::extended);
//const std::regex RE_LONG_LONG("%([[:digit:]]*)ll[id]", std::regex::extended);
//const std::regex RE_LONG_LONG2("%([[:digit:]]*)l64[id]", std::regex::extended);
const std::regex RE_LONG("%([[:digit:]]*)l[id]", std::regex::extended);
const std::regex RE_INT("%([[:digit:]]*)[id]", std::regex::extended);
const std::regex RE_UCHAR("%([[:digit:]]*)hh[uoxX]", std::regex::extended);
const std::regex RE_USHORT("%([[:digit:]]*)h[uoxX]", std::regex::extended);
//const std::regex RE_ULONG_LONG("%([[:digit:]]*)ll[uoxX]", std::regex::extended);
//const std::regex RE_ULONG_LONG2("%([[:digit:]]*)l64[uoxX]", std::regex::extended);
const std::regex RE_ULONG("%([[:digit:]]*)l[uoxX]", std::regex::extended);
const std::regex RE_UINT("%([[:digit:]]*)[uoxX]", std::regex::extended);
//const std::regex RE_STRLEN("%(\\.)?([[:digit:]]*)s(.*)", std::regex::extended);
//const std::regex (, std::regex::extended);
//const std::regex (, std::regex::extended);

std::vector<std::string> split(const std::string &x,
			       const std::string& substr);
  
}
}
