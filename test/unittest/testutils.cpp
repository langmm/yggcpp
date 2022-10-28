#include <sstream>
#include "unittest.hpp"
#include "utils/complex_type.hpp"
#include "utils/enums.hpp"
#include "utils/Address.hpp"

namespace {
#define COMPLEX_UNIT_TEST(type, typenm, comparator, real, img) { \
complex_ ## type ## _t cmplx;                                      \
complex_ ## type ## _t ncmplx;                                     \
typenm r1, i1;                                                   \
cmplx.re = real;                                                   \
cmplx.im = img;                                                    \
std::stringstream ss;                                              \
ss << cmplx;                                                       \
ss >> ncmplx;                                                      \
EXPECT_ ## comparator ## _EQ(cmplx.re, ncmplx.re);                 \
EXPECT_ ## comparator ## _EQ(cmplx.im, ncmplx.im);                 \
}

TEST(Value, Complex) {
    COMPLEX_UNIT_TEST(float, float, FLOAT, 1.2, 6.4)
    COMPLEX_UNIT_TEST(double, double, DOUBLE, 1.64452778, 9.28667775882)
    COMPLEX_UNIT_TEST(long_double, long double, DOUBLE, 162.2235992765, -84.22876639)
}

TEST(VALUE, Enum) {
    SUBTYPE st = T_STRING;
    SUBTYPE et = T_UNICODE;
    EXPECT_NE(st, et);
    std::stringstream ss;
    ss << st;
    ss >> et;
    EXPECT_EQ(st, et);

    VTYPE vt = T_SCALABLE;
    VTYPE evt = T_ARRAY1D;
    EXPECT_NE(vt, evt);
    std::stringstream ss1;
    ss1 << vt;
    ss1 >> evt;
    EXPECT_EQ(vt, evt);
}

TEST(ADDRESS, Init) {
    const std::string astr = "this.is.a.test";
    const char* cstr = "this.is.C.test";
    communication::utils::Address adr(astr);
    EXPECT_TRUE(adr.valid());
    EXPECT_EQ(adr.address(), astr);
    EXPECT_EQ(0, adr.key());

    communication::utils::Address adrc(cstr);
    EXPECT_TRUE(adrc.valid());
    EXPECT_EQ(adrc.address().c_str(), astr);
    EXPECT_EQ(0, adrc.key());

}
}