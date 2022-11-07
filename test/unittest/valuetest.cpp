#include <sstream>
#include "unittest.hpp"
#include "datatypes/Value.hpp"

using namespace communication;
using namespace communication::datatypes;

namespace {

TEST(VALUE, Constructor) {
    Value<int32_t> val(63);
    int32_t v;
    val.get(v);
    EXPECT_EQ(val.get(), 63);
    EXPECT_EQ(v, 63);
}

TEST(VALUE, read_write) {
    Value<float> val;
    float v = 32.54;
    val.set(v);
    std::stringstream ss;
    ss << val;
    std::string s = ss.str();
    EXPECT_NE(s.find("scalarvalue"), std::string::npos);
    Value<float> val2;
    ss >> val2;
    EXPECT_EQ(val, val2);
}

TEST(VALUE, operators) {
    Value<complex_float_t> val;
    complex_float_t v = {2.5, -3.446};
    val.set(v);
    Value<complex_float_t> val2(v);
    EXPECT_EQ(val, val2);
    val.set(v, "pixels");
    EXPECT_NE(val, val2);
    val2.set(v, "pixels");
    EXPECT_EQ(val, val2);
    v.re += 5.1;
    val2.set(v);
    EXPECT_NE(val, val2);
}

TEST(VALUE, misc) {
    Value<bool> val;
    EXPECT_EQ(val.nargs_exp(), 2);
    EXPECT_EQ(val.getType(), T_SCALAR);
}
}