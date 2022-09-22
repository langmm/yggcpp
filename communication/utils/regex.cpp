#include <memory>
#include "regex.hpp"
using namespace communication::utils;
/*!
  @brief Find first match to regex.
  @param[in] regex_text constant character pointer to string that should be
  compiled into a regex.
  @param[in] to_match constant character pointer to string that should be
  checked for matches.
  @param[out] sind size_t index where match begins.
  @param[out] eind size_t index where match ends.
  @return int Number of matches found. -1 is returned if the regex could not be
  compiled.
*/
int find_match(const char *regex_text, const char *to_match,
               size_t *sind, size_t *eind) {
    int ret;
    int n_match = 0;
    regex_t r;
    // Compile
    ret = compile_regex(&r, regex_text);
    if (ret)
        return -1;
    // Loop until string done
    const char * p = to_match;
    const size_t n_sub_matches = 10;
    regmatch_t m[n_sub_matches];
    int nomatch = regexec(&r, p, n_sub_matches, m, 0);
    if (!(nomatch)) {
        *sind = m[0].rm_so;
        *eind = m[0].rm_eo;
        n_match++;
    }
    regfree(&r);
    return n_match;
}

/*!
  @brief Create a regex from a character array.
  Adapted from https://www.lemoda.net/c/unix-regex/
  @param[out] r pointer to regex_t. Resutling regex expression.
  @param[in] regex_text constant character pointer to text that should be
  compiled.
  @return static int Success or failure of compilation.
*/
bool compile_regex (regex_t * r, const char * regex_text)
{
    int status = regcomp (r, regex_text, REG_EXTENDED);//|REG_NEWLINE);
    if (status != 0) {
        char error_message[2048];
        regerror (status, r, error_message, 2048);
        printf ("Regex error compiling '%s': %s\n",
                regex_text, error_message);
        return false;
    }
    return true;
}

int count_matches(const char *regex_text, const char *to_match) {
    bool ret;
    int n_match = 0;
    regex_t r;
    // Compile
    ret = communication::utils::compile_regex(&r, regex_text);
    if (ret)
        return -1;
    // Loop until string done
    const char * p = to_match;
    const size_t n_sub_matches = 10;
    regmatch_t m[n_sub_matches];
    while (1) {
        int nomatch = regexec(&r, p, n_sub_matches, m, 0);
        if (nomatch)
            break;
        n_match++;
        p += m[0].rm_eo;
    }
    regfree(&r);
    return n_match;
}

int find_matches(const char *regex_text, const char *to_match,
                 size_t **sind, size_t **eind) {
    bool ret;
    int n_match = 0;
    regex_t r;
    // Compile
    ret = communication::utils::compile_regex(&r, regex_text);
    if (ret)
        return -1;
    // Loop until string done
    const size_t n_sub_matches = 50;
    regmatch_t m[n_sub_matches];
    int nomatch = regexec(&r, to_match, n_sub_matches, m, 0);
    if (!(nomatch)) {
        // Count
        while (n_match < (int)n_sub_matches) {
            if ((m[n_match].rm_so == -1) && (m[n_match].rm_eo == -1)) {
                break;
            }
            n_match++;
        }
        // Realloc
        *sind = (size_t*)realloc(*sind, n_match*sizeof(size_t));
        *eind = (size_t*)realloc(*eind, n_match*sizeof(size_t));
        // Record
        int i;
        for (i = 0; i < n_match; i++) {
            (*sind)[i] = m[i].rm_so;
            (*eind)[i] = m[i].rm_eo;
        }
    }
    regfree(&r);
    return n_match;
}

