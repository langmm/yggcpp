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
    std::regex_match(to_match.begin() + static_cast<long>(start), to_match.end(), sm, regex);
    if (!sm.empty()) {
        n_match = sm.size();
        sind = sm.position(0);
        eind = sind + sm.length(0);
    }
    return n_match;
}

size_t count_matches(const std::string &regex_text, const std::string &to_match) {
    // Compile
    std::regex rx(regex_text, std::regex::extended);
    // Loop until string done
    std::smatch sm;
    std::regex_match(to_match, sm, rx);
    if (!sm.empty()) {
        return static_cast<int>(sm.size());
    }
    return 0;
}

size_t find_matches(const std::string &regex_text, const std::string &to_match,
                 std::vector<size_t> &sind, std::vector<size_t> &eind) {

    size_t n_match = 0;

    // Compile
    std::regex rx(regex_text, std::regex::extended);
    // Loop until string done
    std::smatch sm;
    std::regex_match(to_match, sm, rx);
    if (sm.empty()) {
        sind.clear();
        eind.clear();
        return 0;
    }
    n_match = sm.size();
    sind.resize(n_match);
    eind.resize(n_match);
    for (auto i = 0; i < n_match; i++) {
        sind[i] = sm.position(i);
        eind[i] = sind[i] + sm.length(i);
    }
    return n_match;
}

size_t regex_replace_sub(std::string &buf, const std::regex &re, const std::string &rp,
                         const size_t &nreplace) {
    std::smatch sm;
    char re_sub[buf.size()];
    size_t len_m, delta_siz, len_rp;
    size_t cur_pos = 0;
    size_t cur_siz = buf.size();
    size_t creplace = 0;
    size_t i;
    size_t ret;
    bool match = true;
    int offset = 0;
    while (match) {
        std::string p = buf;
        if ((nreplace > 0) && (creplace >= nreplace)) {
            printf("regex_replace_nosub: Maximum of %d replacements reached\n",
                   (int) creplace);
            break;
        }
        match = std::regex_match(p, sm, re);
        if (!match) {
            //printf("regex_replace_sub: nomatch for in %s\n", p.c_str());
            break;
        }
        // Get list of subrefs
        std::vector<size_t> refs;

        // For each subref complete replacements
        std::string rp_sub = rp;
        for (auto j = 0; j < get_subrefs(rp, refs); j++) {
            i = refs[j];
            std::string igrp = p.substr(sm.position(i), std::string::npos);
            igrp.erase(sm.length(i) - sm.position(i) + 1);
            sprintf(re_sub, "\\$%d", (int) i);
            ret = regex_replace_nosub(rp_sub, re_sub, igrp, 0);
        }
        refs.clear();
        // Ensure replacement will not exceed buffer
        len_rp = ret;
        len_m = sm.length(0) - sm.position(0);
        delta_siz = len_rp - len_m;
        // Move trailing
        buf.replace(offset, sm.length(0), rp_sub);

        // Advance
        offset += static_cast<int>(sm.position(0) + len_rp);
        cur_pos += sm.position(0) + len_rp;
        cur_siz += delta_siz;
        creplace += 1;
    }
    return cur_siz;
}

size_t regex_replace_nosub(std::string &buf, const std::regex &re, const std::string &rp,
                           const size_t &nreplace) {
    //printf("regex_replace_nosub(%s, %s)\n", buf.c_str(), rp.c_str());

    // Loop making replacements
    size_t len_rp = rp.size();
    size_t len_m, rem_s, delta_siz;
    size_t cur_pos = 0;
    size_t cur_siz = buf.size();
    size_t creplace = 0;
    std::smatch sm;
    bool match = true;
    int offset = 0;
    while (match) {
        const std::string p = buf;
        if ((nreplace > 0) && (creplace >= nreplace)) {
            printf("regex_replace_nosub: Maximum of %d replacements reached\n",
                   (int) creplace);
            break;
        }
        if (offset >= buf.size()) {
            break;
        }
        match = std::regex_match(p.begin() + offset, p.end(), sm, re);
        if (!match) {
            //printf("regex_replace_nosub: nomatch for in %s\n", p.c_str());
            break;
        }
        len_m = sm.length(0) - sm.position(0);
        delta_siz = len_rp - len_m;
        // Move trailing
        rem_s = sm.position(0) + len_rp;
        // Copy replacement
        buf = std::regex_replace(buf, re, rp);
        // Advance
        offset += static_cast<int>(rem_s);
        cur_pos += rem_s;
        cur_siz += delta_siz;
        creplace += 1;
    }
    //printf("regex_replace_nosub() = %s\n", buf.c_str());

    return cur_siz;
}

size_t get_subrefs(const std::string &buf, std::vector<size_t> &refs) {

    // Allocate;
    const size_t ngroups = RE_SUBREFS.mark_count() + 1;
    if (ngroups != 2) {
        throw std::runtime_error("ERROR: regex could not find subgroup\n");
    }
    //regmatch_t *m = (regmatch_t *) malloc(ngroups * sizeof(regmatch_t));
    // Prepare "bitmap"
    const size_t max_ref = 10; //99;
    size_t i;
    std::vector<uint8_t> ref_bytes(10, 0);
    //uint8_t *ref_bytes = (uint8_t *) malloc((max_ref + 1) * sizeof(uint8_t));
    //for (i = 0; i <= max_ref; i++)
    //    ref_bytes[i] = 0;
    // Locate matches
    //const char *p = buf;
    const size_t max_grp = 2;  // Digits in max_ref
    size_t igrp_len;
    //char igrp[max_grp];
    size_t iref;
    std::smatch sm;
    bool match = true;
    int offset = 0;
    while (match) {
        match = std::regex_match(buf.begin() + offset, buf.end(), sm, RE_SUBREFS);
        if (!match || sm.size() != 2) {
            break;
        }
        // Lone $ without digit
        if ((sm.position(1) == -1) && (sm.length(1) == -1)) {
            offset += static_cast<int>(sm.length(0));
            continue;
        }
        // Substring
        igrp_len = sm.length(1) - sm.position(1) + 1;
        if (igrp_len > max_grp) {
            throw std::runtime_error(std::string("Number longer than ") + std::to_string(max_grp) + " digits unlikely");
        }
        std::string igrp = buf.substr(offset + sm.position(1), igrp_len);
        //igrp[igrp_len] = 0;
        // Extract ref number
        iref = std::stoi(igrp);
        if (iref > max_ref) {
            throw std::runtime_error(std::string("Reference to substr ") + std::to_string(iref) + " exceeds limit (" + std::to_string(max_ref) + "\n");
        }
        ref_bytes[iref] = 1;
        offset += static_cast<int>(sm.length(0));
    }
    // Get unique refs
    int nref = 0;
    for (i = 0; i <= max_ref; i++) {
        if (ref_bytes[i])
            nref++;
    }
    refs.resize(nref);
    size_t ir;
    for (i = 0, ir = 0; i <= max_ref; i++) {
        if (ref_bytes[i]) {
            refs[ir] = i;
            ir++;
        }
    }
    return nref;
}

}
}