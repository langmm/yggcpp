#include <memory>
#include "regex.hpp"

namespace communication {
namespace utils {

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
size_t find_match(const std::regex &regex, const std::string &to_match,
               const size_t& start, size_t &sind, size_t &eind) {
    size_t n_match = 0;

    // Loop until string done
    std::smatch sm;
    std::regex_search(to_match.begin() + static_cast<long>(start), to_match.end(), sm, regex);
    if (!sm.empty()) {
        n_match = sm.size();
        sind = sm.position(0);
        eind = sind + sm.length(0);
    }
    return n_match;
}

int find_match_c(const char *regex_text, const char *to_match,
		 size_t *sind, size_t *eind) {
  std::regex re(regex_text, std::regex::extended);
  std::string str(to_match);
  return (int)(find_match(re, str, 0, *sind, *eind));
}
  
size_t count_matches(const std::regex &re, const std::string &to_match) {
    // Loop until string done
    std::smatch sm;
    size_t count = 0;
    std::string subtext = to_match;
    while (std::regex_search(subtext, sm, re)) {
        count++;
        subtext = sm.suffix().str();
    }
    return count;
}

size_t find_matches(const std::string &regex_text, const std::string &to_match,
                 std::vector<size_t> &sind, std::vector<size_t> &eind) {

    size_t n_match = 0;
    std::string subtext = to_match;
    // Compile
    std::regex rx(regex_text, std::regex::extended);
    // Loop until string done
    sind.clear();
    eind.clear();
    std::smatch sm;

    while (std::regex_search(subtext, sm, rx)) {
        n_match++;
        sind.push_back(sm.position(0));
        eind.push_back(sm.position(0) + sm.length(0));
        subtext = sm.suffix().str();
    }

    return n_match;
}

size_t regex_replace(std::string &buf, const std::regex &re, const std::string &rp,
                     const size_t &nreplace) {
    //printf("regex_replace_nosub(%s, %s)\n", buf.c_str(), rp.c_str());
    size_t nrep = 0;
    // Loop making replacements
    if (nreplace == 0) {
        nrep = count_matches(re, buf);
        buf = std::regex_replace(buf, re, rp);
        return nrep;
    }
    std::string temp;
    while (nrep < nreplace) {
        temp = std::regex_replace(buf, re, rp, std::regex_constants::format_first_only);
        if (temp == buf)
            break;
        nrep++;
        buf = temp;
    }
    return nrep;
}

}
}
