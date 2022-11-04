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

}
}