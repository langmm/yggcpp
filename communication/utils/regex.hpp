#pragma once

namespace communication {
namespace utils {

#include <regex.h>
#include <cstdio>

bool compile_regex(regex_t *r, const char *regex_text);

int find_match(const char *regex_text, const char *to_match,
               size_t *sind, size_t *eind);

}
}