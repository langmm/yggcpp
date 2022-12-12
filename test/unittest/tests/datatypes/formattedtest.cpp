#include <sstream>
#include <random>
#include <map>
#include <tuple>
#include "../unittest.hpp"
#include "datatypes/FormattedData.hpp"

typedef const std::tuple<const std::string, const unsigned short> identifier;
typedef long double ldouble;
using namespace communication;
using namespace communication::datatypes;

const std::map<const std::string, identifier > regexmap =
        {{"string", std::make_tuple("a=%5s", 0)},
         {"float", std::make_tuple("a=%.5f", 4)},
         {"double", std::make_tuple("b=%5.3e", 8)},
         {"ldouble", std::make_tuple("a=%12.8g", 12)},
         {"int8_t", std::make_tuple("st=%2i", 8)},
         {"int16_t", std::make_tuple("q=%4d", 16)},
         {"int32_t", std::make_tuple("ww=%8i", 32)},
         {"int64_t", std::make_tuple("%16i", 64)},
         {"uint8_t", std::make_tuple("st=%2x", 8)},
         {"uint16_t", std::make_tuple("q=%4X", 16)},
         {"uint32_t", std::make_tuple("ww=%8o", 32)},
         {"uint64_t", std::make_tuple("%16u", 64)},
         {"char", std::make_tuple("%hhi", 8)},
         {"short", std::make_tuple("%2hd", 16)},
         {"uchar", std::make_tuple("%hhu", 8)},
         {"ushort", std::make_tuple("%3hu", 16)},
         {"long", std::make_tuple("%7ld", 64)},
         {"ulong", std::make_tuple("%lu", 64)},
         {"complex_float_t", std::make_tuple("%3.3f%2.5fj", 4)},
         {"complex_double_t", std::make_tuple("%8.3f%2.5fj", 8)},
         {"complex_long_double_t", std::make_tuple("%13.5f%2.5fj", 12)}

        };

#define CONSTRUCTOR(X) \
TEST(FormattedDataTest, constructor_ ## X) { \
identifier data = regexmap.at(#X);           \
FormattedData fd(std::get<0>(data));         \
EXPECT_EQ(fd.size(), 1);                     \
EXPECT_EQ(fd[0]->type, submap.at(#X));       \
EXPECT_EQ(fd[0]->getPrecision(), std::get<1>(data)); \
EXPECT_EQ(fd[0]->vtype, T_SCALABLE);         \
fd.display("");        \
}

#define CONSTRUCTOR_ARRAY(X) \
TEST(FormattedDataTest, constructor_array_ ## X) { \
identifier data = regexmap.at(#X);                 \
FormattedData fd(std::get<0>(data), true);         \
EXPECT_EQ(fd.size(), 1);     \
EXPECT_EQ(fd[0]->type, submap.at(#X));             \
EXPECT_EQ(fd[0]->getPrecision(), std::get<1>(data)); \
EXPECT_EQ(fd[0]->vtype, T_ARRAY1D);                \
}


namespace {
TEST(FormattedDataTest, misc) {
    FormattedData fd(" ", false);
    EXPECT_EQ(fd.nargs_exp(), 2);
    EXPECT_EQ(fd.getType(), T_FORMATTED);
}

EVAL(MAP(CONSTRUCTOR, string, float, double, ldouble))
EVAL(MAP(CONSTRUCTOR, int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t, uint32_t, uint64_t))
EVAL(MAP(CONSTRUCTOR, char, uchar, short, ushort, long, ulong))
EVAL(MAP(CONSTRUCTOR, complex_float_t, complex_double_t, complex_long_double_t))

EVAL(MAP(CONSTRUCTOR_ARRAY, string, float, double, ldouble))
EVAL(MAP(CONSTRUCTOR_ARRAY, int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t, uint32_t, uint64_t))
EVAL(MAP(CONSTRUCTOR_ARRAY, char, uchar, short, ushort, long, ulong))
EVAL(MAP(CONSTRUCTOR_ARRAY, complex_float_t, complex_double_t, complex_long_double_t))

}