#pragma once

namespace communication {
namespace utils {

#include <regex.h>
#include <cstdio>

bool compile_regex(regex_t *r, const char *regex_text);

int find_match(const char *regex_text, const char *to_match,
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
int count_matches(const char *regex_text, const char *to_match);

int find_matches(const char *regex_text, const char *to_match,
                 size_t **sind, size_t **eind);
}
}