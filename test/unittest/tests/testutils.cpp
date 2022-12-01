#include <sstream>
#include <algorithm>
#include <cstdlib>
#include "../unittest.hpp"
#include "utils/complex_type.hpp"
#include "utils/enums.hpp"
#include "utils/Address.hpp"
#include "utils/logging.hpp"
#include "utils/tools.hpp"
#include "utils/regex.hpp"

#define BEGIN_CAPTURE(B) \
std::stringstream B;     \
std::streambuf* prevbuf = std::cout.rdbuf(B.rdbuf());

#define END_CAPTURE std::cout.rdbuf(prevbuf);

using namespace communication::utils;
namespace {
#define COMPLEX_UNIT_TEST(type, typenm, comparator, real, img) { \
complex_ ## type ## _t cmplx;                                    \
complex_ ## type ## _t ncmplx;                                   \
typenm r1, i1;                                                   \
cmplx.re = real;                                                 \
cmplx.im = img;                                                  \
std::stringstream ss;                                            \
ss << cmplx;                                                     \
ss >> ncmplx;                                                    \
EXPECT_ ## comparator ## _EQ(cmplx.re, ncmplx.re);               \
EXPECT_ ## comparator ## _EQ(cmplx.im, ncmplx.im);               \
}

TEST(Value, Complex) {
    COMPLEX_UNIT_TEST(float, float, FLOAT, 1.2, 6.4)
    COMPLEX_UNIT_TEST(double, double, DOUBLE, 1.64452778, 9.28667775882)
    COMPLEX_UNIT_TEST(long_double, long double, DOUBLE, 162.2235992765, -84.22876639)
}

TEST(ADDRESS, Init) {
    const std::string astr = "this.is.a.test";
    std::string cstr = "this.is.C.test";
    auto *adr = new Address(astr);
    EXPECT_TRUE(adr->valid());
    EXPECT_EQ(adr->address(), astr);
    EXPECT_EQ(0, adr->key());

    Address adrc(cstr.c_str());
    EXPECT_TRUE(adrc.valid());
    EXPECT_EQ(adrc.address(), cstr);
    EXPECT_EQ(0, adrc.key());
    EXPECT_NE(adrc.address(), adr->address());

    auto* adrcmp = new Address(adr);
    EXPECT_TRUE(adrcmp->valid());
    EXPECT_EQ(adrcmp->address(), adr->address());
    EXPECT_NE(adr, adrcmp);
    delete adr;
    delete adrcmp;
}

TEST(ADDRESS, set) {
    Address adr;
    EXPECT_FALSE(adr.valid());
    const int val = 12345;
    adr.address(std::to_string(val));
    EXPECT_TRUE(adr.valid());
    EXPECT_EQ(adr.key(), val);
    EXPECT_EQ(val, stoi(adr.address()));

    std::string longadr(COMM_ADDRESS_SIZE+5, 'x');
    Address adr1(longadr);
    EXPECT_NE(adr1.address(), longadr);
    EXPECT_TRUE(adr1.valid());
}

TEST(ADDRESS, operators) {
    const std::string a1 = "this.is.a.test";
    const std::string a2 = "this.is.b.test";
    auto* adr1 = new Address(a1);
    auto* adr11 = new Address(a1);
    auto* adr2 = new Address(a2);
    EXPECT_TRUE(*adr1 == *adr11);
    EXPECT_FALSE(*adr1 == *adr2);

    delete adr1;
    delete adr11;
    delete adr2;
}

TEST(LOGGING, fulltest) {
    BEGIN_CAPTURE(buffer)
    ygglog_error << "An error occurred at " << 5;
    std::string temp = buffer.str();
    auto loc = temp.find("at");
    EXPECT_EQ(std::count(temp.begin() + loc, temp.end(),'5'), 1);
    EXPECT_EQ(temp.find("ERROR"), 0);
    buffer.str(std::string());
    ygglog_info << "This is informational";
    temp = buffer.str();
    EXPECT_EQ(temp.find("INFO"), 0);
    buffer.str(std::string());
    ygglog_debug << "This is a debug message";
    temp = buffer.str();
    EXPECT_EQ(temp.find("DEBUG"), 0);
    buffer.str(std::string());
    char* eval = getenv("YGG_MODEL_NAME");
    setenv("YGG_MODEL_NAME", "MY_TEST_MODEL", 1);
    ygglog_info << "Another info message";
    temp = buffer.str();
    EXPECT_NE(temp.find("MY_TEST_MODEL"), std::string::npos);
    EXPECT_EQ(temp.find("MY_TEST_COPY"), std::string::npos);
    buffer.str(std::string());
    char* eval2 = getenv("YGG_MODEL_COPY");
    setenv("YGG_MODEL_COPY", "MY_TEST_COPY", 1);
    ygglog_info << "Another info message";
    temp = buffer.str();
    EXPECT_NE(temp.find("MY_TEST_MODEL"), std::string::npos);
    EXPECT_NE(temp.find("MY_TEST_COPY"), std::string::npos);
    if (eval2 != nullptr) {
        setenv("YGG_MODEL_COPY", eval2, 1);
    } else {
        unsetenv("YGG_MODEL_COPY");
    }
    if (eval != nullptr) {
        setenv("MY_TEST_MODEL", eval, 1);
    } else {
        unsetenv("MY_TEST_MODEL");
    }
    buffer.str(std::string());
    EXPECT_ANY_THROW(ygglog_throw_error("Another fatal error occurred"));
    temp = buffer.str();
    EXPECT_EQ(temp.find("ERROR"), 0);
    END_CAPTURE
}

TEST(TOOLS, joinParse) {
    std::vector<int> ivals = {2, 5, 18, 226, -19};
    std::stringstream ss;
    join(ivals, ss);
    std::string temp = ss.str();
    EXPECT_EQ(std::count(temp.begin(), temp.end(), DELIMITER), ivals.size() - 1);

    std::vector<int> retvals;
    parse(retvals, ivals.size(), ss);
    EXPECT_EQ(ivals, retvals);

    ss.str(std::string());
    ss.clear();

    std::vector<SUBTYPE> svals = {T_FLOAT, T_UINT, T_INT, T_COMPLEX, T_BYTES, T_UNICODE, T_UINT, T_INT};
    join(svals, ss);
    temp = ss.str();
    EXPECT_EQ(std::count(temp.begin(), temp.end(), DELIMITER), svals.size() - 1);
    std::vector<SUBTYPE> sretval;
    parse(sretval, svals.size(), ss);
    EXPECT_EQ(svals, sretval);

    ss.str(std::string());
    ss.clear();

    std::vector<std::string> strvals = {"hello", "goodbye", "this is a , test"};
    join(strvals, ss);
    temp = ss.str();
    std::cout << temp << std::endl;
    EXPECT_EQ(std::count(temp.begin(), temp.end(), REPLACE_SPACE[1]), strvals.size());
    std::vector<std::string> strvec;
    parse(strvec, strvals.size(), ss);
    EXPECT_EQ(strvec, strvals);

    ss.str(std::string());
    ss.clear();

    std::vector<VTYPE> vvals = {T_ARRAY1D};
    join(vvals, ss);
    temp = ss.str();
    EXPECT_EQ(std::count(temp.begin(), temp.end(), DELIMITER), 0);
    std::vector<VTYPE> vret;
    EXPECT_ANY_THROW(parse(vret, vvals.size() + 1, ss));
    ss.str(std::string());
    ss.clear();

    join(strvals, ss);
    temp = ss.str();
    EXPECT_EQ(std::count(temp.begin(), temp.end(), REPLACE_SPACE[1]), strvals.size());
    EXPECT_ANY_THROW(parse(strvec, strvals.size() + 1, ss));
    EXPECT_ANY_THROW(parse(strvec, strvals.size() + 5, ss));
}

TEST(REGEX, findmatch) {
    std::string input = "This is the text to match";
    size_t res, start, sind, eind;
    res = find_match(" to", input, sind, eind);
    EXPECT_EQ(sind, 16);
    EXPECT_EQ(eind, 19);
    EXPECT_EQ(res, 1);
    sind = 0;
    res = find_match(" to", input, eind, sind, eind);
    EXPECT_EQ(sind, 0);
    EXPECT_EQ(res, 0);
}

TEST(REGEX, countmatches) {
    std::string input = "This is the line of text to be matched";
    EXPECT_EQ(count_matches(" t", input), 3);
    EXPECT_EQ(count_matches(" to", input), 1);
    EXPECT_EQ(count_matches(" a", input), 0);
}

TEST(REGEX, findmatches) {
    std::string input = "This is the line of text to be matched";
    std::vector<size_t> sind, eind;
    EXPECT_EQ(find_matches(" t", input, sind, eind), 3);
    EXPECT_EQ(3, sind.size());
    EXPECT_EQ(eind.size(), sind.size());
    EXPECT_EQ(sind[0], 7);
    for (auto i = 0; i < 3; i++) {
        EXPECT_EQ(sind[i] + 2, eind[i]);
    }
    EXPECT_EQ(find_matches("bob", input, sind, eind), 0);
    EXPECT_EQ(eind.size(), 0);
}

TEST(REGEX, replacenosub) {
    std::string input = "This is the line of text to be matched";
    EXPECT_EQ(count_matches(" t", input), 3);
    EXPECT_EQ(count_matches(" QQ", input), 0);
    EXPECT_EQ(regex_replace(input, " t", " QQ", 1), 1);
    EXPECT_EQ(count_matches(" t", input), 2);
    EXPECT_EQ(count_matches(" QQ", input), 1);
    EXPECT_EQ(regex_replace(input, " t", " QQ"), 2);
    EXPECT_EQ(count_matches(" t", input), 0);
    EXPECT_EQ(count_matches(" QQ", input), 3);
    input = "This is the line of text to be matched";
    EXPECT_EQ(count_matches(" t", input), 3);
    EXPECT_EQ(count_matches(" QQ", input), 0);
    EXPECT_EQ(regex_replace(input, " t", " QQ", 6), 3);
    EXPECT_EQ(count_matches(" t", input), 0);
    EXPECT_EQ(count_matches(" QQ", input), 3);
    input = "This is the line of text to be matched";
    const size_t size = input.size();
    EXPECT_EQ(count_matches(" t", input), 3);
    EXPECT_EQ(count_matches(" q-", input), 0);
    EXPECT_EQ(regex_replace(input, " (t)([^ ]*)", " q-$2"), 3);
    EXPECT_EQ(input.size(), size + 3);
    EXPECT_EQ(count_matches(" t", input), 0);
    EXPECT_EQ(count_matches(" q-", input), 3);
}
}