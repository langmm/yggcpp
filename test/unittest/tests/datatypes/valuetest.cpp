#include <sstream>
#include <random>
#include <cmath>
#include "../../unittest.hpp"
#include "datatypes/Value.hpp"

using namespace communication;
using namespace communication::datatypes;

std::random_device rd;
std::mt19937 mt(rd());
std::uniform_real_distribution<double> dist(-1000, 1000);

class ValueTest : public ::testing::Test {
public:
    static int rand_int[4];
    static unsigned int rand_uint[4];
    static double rand_float[4];
    static complex_double_t rand_com[4];
    static std::vector<std::string> rand_str;
};

template<typename T>
static inline
std::stringstream makeInvalid(const T& val) {
    std::stringstream ss;
    ss << "scalarvaue" << std::endl << "type " << T_STRING << std::endl << "precision " << 8 << std::endl
       << "unit None" << std::endl << "value " << val<< std::endl;
    return ss;
}
template<typename T>
static inline
std::stringstream makeInvalidtype(const T& val) {
    std::stringstream ss;
    ss << "scalarvalue" << std::endl << "tpe " << T_STRING << std::endl << "precision " << 8 << std::endl
       << "unit None" << std::endl << "value " << val<< std::endl;
    return ss;
}
template<typename T>
static inline
std::stringstream makeInvalidprecision(const T& val) {
    std::stringstream ss;
    ss << "scalarvalue" << std::endl << "type " << T_STRING << std::endl << "prcision " << 8 << std::endl
       << "unit None" << std::endl << "value " << val<< std::endl;
    return ss;
}
template<typename T>
static inline
std::stringstream makeInvalidunit(const T& val) {
    std::stringstream ss;
    ss << "scalarvalue" << std::endl << "type " << T_STRING << std::endl << "precision " << 8 << std::endl
       << "uit None" << std::endl << "value " << val<< std::endl;
    return ss;
}
template<typename T>
static inline
std::stringstream makeInvalidvalue(const T& val) {
    std::stringstream ss;
    ss << "scalarvalue" << std::endl << "type " << T_STRING << std::endl << "precision " << 8 << std::endl
       << "unit None" << std::endl << "vaue " << val<< std::endl;
    return ss;
}


int ValueTest::rand_int[4] = {static_cast<int>(dist(mt)),
                              static_cast<int>(dist(mt)),
                              static_cast<int>(dist(mt)),
                              static_cast<int>(dist(mt))};
unsigned int ValueTest::rand_uint[4] = {static_cast<unsigned int>(abs(dist(mt)))%256,
                                        static_cast<unsigned int>(abs(dist(mt)))%256,
                                        static_cast<unsigned int>(abs(dist(mt)))%256,
                                        static_cast<unsigned int>(abs(dist(mt)))%256};
double ValueTest::rand_float[4] = {dist(mt),dist(mt),dist(mt),dist(mt)};
complex_double_t ValueTest::rand_com[4] = {{dist(mt), dist(mt)}, {dist(mt), dist(mt)},
                                           {dist(mt), dist(mt)}, {dist(mt), dist(mt)}};
std::vector<std::string> ValueTest::rand_str = {"Hello", "This is a Test", "More tests", "The end"};
typedef long double ldouble;
#define CONSTRUCTOR(T, V) \
TEST(ValueTest, constructor_ ## T) { \
    Value<T> val(V);      \
    T v;                  \
    val.get(v);           \
    EXPECT_EQ(val.get(), V);         \
    EXPECT_EQ(v, V);      \
    const T cval = V;     \
    Value<T> val2(cval, "mm");       \
}

#define READ_WRITE(T, V) \
TEST(ValueTest, read_write_ ## T) {\
    Value<T> val;        \
    T v = V;             \
    val.set(v);          \
    std::stringstream ss;\
    ss << val;           \
    std::string s = ss.str();      \
    EXPECT_NE(s.find("scalarvalue"), std::string::npos); \
    Value<T> val2;       \
    ss >> val2;          \
    EXPECT_TRUE(val == val2);      \
    Value<T> val3;       \
    ss = makeInvalid(V); \
    try {                \
        val3.read(ss);   \
        EXPECT_TRUE(false) << "Make invalid failed";     \
    } catch (std::invalid_argument) {}                   \
    ss = makeInvalidtype(V);       \
    try {                \
        val3.read(ss);   \
        EXPECT_TRUE(false) << "Make invalid type failed";\
    } catch (std::invalid_argument) {}                   \
    ss = makeInvalidprecision(V);  \
    try {                \
        val3.read(ss);   \
        EXPECT_TRUE(false) << "Make invalid precision failed"; \
    } catch (std::invalid_argument) {}                   \
    ss = makeInvalidunit(V);       \
    try {                \
        val3.read(ss);   \
        EXPECT_TRUE(false) << "Make invalid unit failed";\
    } catch (std::invalid_argument) {}                   \
    ss = makeInvalidvalue(V);      \
    try {                \
        val3.read(ss);   \
        EXPECT_TRUE(false) << "Make invalid value failed";     \
    } catch (std::invalid_argument) {}                   \
}

#define OPERATORS(T, V) \
TEST(ValueTest, operators_ ## T) { \
    Value<T> val;       \
    T v = V;            \
    val.set(v);         \
    Value<T> val2(v);   \
    EXPECT_EQ(val, val2);          \
    val.set(v, "pixels");          \
    EXPECT_NE(val, val2);          \
    val2.set(v, "pixels");         \
    EXPECT_EQ(val, val2);          \
}

#define TMISC(T) \
TEST(ValueTest, misc_ ## T) { \
    Value<T> val;\
    EXPECT_EQ(val.nargs_exp(), 2); \
    EXPECT_EQ(val.getType(), T_SCALAR); \
    val.display("");          \
}

#define SET_GET(T, V, U, W) \
TEST(ValueTest, set_get_ ## T) { \
    Value<T> val;           \
    T *v = new T(V);        \
    std::string un = "mm";  \
    val.set(v, un);         \
    T q;                    \
    std::string unit;       \
    val.get(q, unit);       \
    EXPECT_EQ(*v, q);       \
    EXPECT_EQ(unit, un);    \
    val.set(U);             \
    T *vv = new T();        \
    val.get(vv);            \
    EXPECT_EQ(U, *vv);      \
    val.set(U, "pixel");    \
    *v = W;                 \
    val.set(v);             \
    val.get(vv, unit);      \
    EXPECT_EQ(*v, *vv);     \
    EXPECT_EQ(unit, "pixel");    \
}

#define GET_INT_RAND(T, I) static_cast<T>(ValueTest::rand_int[I])
#define GET_UINT_RAND(T, I) static_cast<T>(ValueTest::rand_uint[I])
#define GET_FLOAT_RAND(T, I) static_cast<T>(ValueTest::rand_float[I])
#define GET_COMPLEXF_RAND(T, I) complex_f(ValueTest::rand_com[I])
#define GET_COMPLEXD_RAND(T, I) ValueTest::rand_com[I]
#define GET_COMPLEXLD_RAND(T, I) complex_ld(ValueTest::rand_com[I])
#define GET_STR_RAND(T, I) ValueTest::rand_str[I]

#define MAKE_TEST(T, U) \
CONSTRUCTOR(T, GET_ ## U ##_RAND(T, 0)) \
READ_WRITE(T, GET_## U ##_RAND(T, 1))  \
OPERATORS(T, GET_## U ##_RAND(T, 2))   \
TMISC(T)             \
SET_GET(T, GET_## U ##_RAND(T, 0), GET_## U ##_RAND(T, 3), GET_## U ##_RAND(T, 1))

#define MAKE_INT_TEST(T) MAKE_TEST(T, INT) MAKE_TEST(u ## T, UINT)
#define MAKE_FLOAT_TEST(T) MAKE_TEST(T, FLOAT)

namespace {
using namespace std;
EVAL(MAP(MAKE_INT_TEST, int8_t, int16_t, int32_t, int64_t))
EVAL(MAP(MAKE_FLOAT_TEST, float, double, ldouble))
MAKE_TEST(complex_float_t, COMPLEXF)
MAKE_TEST(complex_double_t, COMPLEXD)
MAKE_TEST(complex_long_double_t, COMPLEXLD)
MAKE_TEST(string, STR)

}