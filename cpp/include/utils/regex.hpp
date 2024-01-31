#pragma once
#include "YggInterface_export.h"
#include <regex>
#include <cstdio>
#include <cstring>
#include <iostream>
#define FMT_LEN 100

namespace YggInterface {
namespace utils {

/*!
  @brief Find first match to regex.
  @param[in] regex constant character pointer to string that should be
  compiled into a regex.
  @param[in] to_match constant character pointer to string that should be
  checked for matches.
  @param[in] start Index to start searching at
  @param[out] sind size_t index where match begins.
  @param[out] eind size_t index where match ends.
  @return int Number of matches found. -1 is returned if the regex could not be
  compiled.
*/
YGG_API size_t find_match(const std::regex &regex,
			  const std::string &to_match,
			  const size_t& start, size_t &sind,
			  size_t &eind);
// Currently unused
// YGG_API static
// size_t find_match(const std::regex &regex, const std::string &to_match,
//                size_t &sind, size_t &eind) {
//     return find_match(regex, to_match, 0, sind, eind);
// }
// YGG_API static
// size_t find_match(const std::string &regex_text, const std::string &to_match,
//                const size_t& start, size_t &sind, size_t &eind) {
//     std::regex regex(regex_text, std::regex::extended);
//     return find_match(regex, to_match, start, sind, eind);
// }
// YGG_API static
// size_t find_match(const std::string &regex_text, const std::string &to_match,
//                size_t &sind, size_t &eind) {
//     return find_match(regex_text, to_match, 0, sind, eind);
// }
YGG_API int find_match_c(const char *regex_text, const char *to_match,
			 size_t *sind, size_t *eind);


/*!
  @brief Count the number of times a regular expression is matched in a string.
  @param[in] re constant character pointer to string that should be
  compiled into a regex.
  @param[in] to_match constant character pointer to string that should be
  checked for matches.
  @return int Number of matches found. -1 is returned if the regex could not be
  compiled.
*/
YGG_API size_t count_matches(const std::regex &re,
			     const std::string &to_match);

// Currently unused
// static
// size_t count_matches(const std::string &regex_text, const std::string &to_match) {
//     std::regex re(regex_text);
//     return count_matches(re,to_match);
// }

/*!
 * @brief Find the location of the given substring inside the given string.
 * @param[in] regex_text The string to search for mateches in
 * @param[in] to_match The string to search for.
 * @param[out] sind Vector of the starting positions of any matches.
 * @param[out] eind Vector of the ending positions of any matches
 * @return The number of matches found (also the length of sind an eind)
 */
YGG_API size_t find_matches(const std::string &regex_text,
			    const std::string &to_match,
			    std::vector<size_t> &sind,
			    std::vector<size_t> &eind);
/*!
 * @brief Replace instances of a substring with another string
 * @param[in, out] buf The string to have characters replaced
 * @param[in] re The regular expression to use to find the substring to replace
 * @param[in] rp The string to substitute into the string
 * @param[in] nreplace The number to replace, 0 means all.
 * @return The number of replacements made.
 */
YGG_API size_t regex_replace(std::string &buf, const std::regex &re,
			     const std::string &rp,
			     const size_t &nreplace=0);
/*!
 * @brief Replace instances of a substring with another string
 * @param[in, out] buf The string to have characters replaced
 * @param[in] re The regular expression to use to find the substring to replace
 * @param[in] rp The string to substitute into the string
 * @param[in] nreplace The number to replace, 0 means all.
 * @return The number of replacements made.
 */
static size_t regex_replace(std::string &buf,
			    const std::string &re,
			    const std::string &rp,
			    const size_t &nreplace=0) {
    std::regex regex(re, std::regex::extended);
    return regex_replace(buf, regex, rp, nreplace);
}
/*!
 * @brief Replace instances of a set of characters in a c-string
 * @param[in, out] buf The c-string to have characters replaced
 * @param[in] len_buf The size of buf
 * @param[in] re The regular expression to use to find the characters to replace
 * @param[in] rp The characters to substitute into the c-string
 * @param[in] nreplace The number to replace
 * @return The number of replacements made.
 */
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

/*!
 * @brief Split a string into a vector of strings
 * @param[in] x The string to split up
 * @param[in] substr The delimiter to use
 * @return The split string
 */
YGG_API std::vector<std::string> split(const std::string &x,
				       const std::string& substr);
  
}
}
