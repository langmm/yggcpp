#include <sstream>
#include <random>
#include "../unittest.hpp"
#include "datatypes/ValueArray1D.hpp"

using namespace communication;
using namespace communication::datatypes;

std::random_device rd;
std::mt19937 mt(rd());
std::uniform_real_distribution<double> dist(-1000, 1000);

class ValueArrayTest : public ::testing::Test {
public:
    static std::vector<std::vector<int> > rand_int;
    static std::vector<std::vector<unsigned int> > rand_uint;
    static std::vector<std::vector<double> > rand_float;
    static std::vector<std::vector<complex_double_t> > rand_com;
    static std::vector<std::vector<std::string> > rand_str;
};

std::vector<std::vector<int> > ValueArrayTest::rand_int = {{static_cast<int>(dist(mt)),
                                                            static_cast<int>(dist(mt)),
                                                            static_cast<int>(dist(mt)),
                                                            static_cast<int>(dist(mt))},
                                                           {static_cast<int>(dist(mt)),
                                                            static_cast<int>(dist(mt)),
                                                            static_cast<int>(dist(mt))},
                                                           {static_cast<int>(dist(mt)),
                                                            static_cast<int>(dist(mt))},
                                                           {static_cast<int>(dist(mt)),
                                                            static_cast<int>(dist(mt)),
                                                            static_cast<int>(dist(mt)),
                                                            static_cast<int>(dist(mt)),
                                                            static_cast<int>(dist(mt)),
                                                            static_cast<int>(dist(mt))}};
std::vector<std::vector<unsigned int> > ValueArrayTest::rand_uint = {{static_cast<unsigned int>(abs(dist(mt)))%256,
                                                                      static_cast<unsigned int>(abs(dist(mt)))%256,
                                                                      static_cast<unsigned int>(abs(dist(mt)))%256,
                                                                      static_cast<unsigned int>(abs(dist(mt)))%256},
                                                                     {static_cast<unsigned int>(abs(dist(mt)))%256,
                                                                      static_cast<unsigned int>(abs(dist(mt)))%256,
                                                                      static_cast<unsigned int>(abs(dist(mt)))%256},
                                                                     {static_cast<unsigned int>(abs(dist(mt)))%256,
                                                                      static_cast<unsigned int>(abs(dist(mt)))%256},
                                                                     {static_cast<unsigned int>(abs(dist(mt)))%256,
                                                                      static_cast<unsigned int>(abs(dist(mt)))%256,
                                                                      static_cast<unsigned int>(abs(dist(mt)))%256,
                                                                      static_cast<unsigned int>(abs(dist(mt)))%256,
                                                                      static_cast<unsigned int>(abs(dist(mt)))%256,
                                                                      static_cast<unsigned int>(abs(dist(mt)))%256}
                                                                     };
std::vector<std::vector<double> > ValueArrayTest::rand_float = {{dist(mt), dist(mt), dist(mt), dist(mt)},
                                                                {dist(mt), dist(mt), dist(mt)},
                                                                {dist(mt), dist(mt)},
                                                                {dist(mt), dist(mt), dist(mt), dist(mt), dist(mt), dist(mt)}
                                                                };
std::vector<std::vector<complex_double_t> > ValueArrayTest::rand_com = {{{dist(mt), dist(mt)}, {dist(mt), dist(mt)},
                                                                         {dist(mt), dist(mt)}, {dist(mt), dist(mt)}},
                                                                        {{dist(mt), dist(mt)}, {dist(mt), dist(mt)},
                                                                         {dist(mt), dist(mt)}},
                                                                        {{dist(mt), dist(mt)}, {dist(mt), dist(mt)}},
                                                                        {{dist(mt), dist(mt)}, {dist(mt), dist(mt)},
                                                                         {dist(mt), dist(mt)}, {dist(mt), dist(mt)},
                                                                         {dist(mt), dist(mt)}, {dist(mt), dist(mt)}}
                                                                        };
std::vector<std::vector<std::string> > ValueArrayTest::rand_str = {{"Hello", "This is a Test", "More tests", "The end"},
                                                                   {"ABCde", "ghi jkl"},
                                                                   {"qwert iop", "asdfg jkl;'", "zxcvbn"},
                                                                   {"16dfg", "QTZ{|} RFQ", "POIUYT", "QAZXSW EDCVfrt", "jUyhn bv gf"}
                                                                   };
typedef long double ldouble;

#define GET_INT_RAND(T, I) cast_vector<T, int>(ValueArrayTest::rand_int[I])
#define GET_UINT_RAND(T, I) cast_vector<T, unsigned int>(ValueArrayTest::rand_uint[I])
#define GET_FLOAT_RAND(T, I) cast_vector<T, double>(ValueArrayTest::rand_float[I])
#define GET_STR_RAND(T, I) ValueArrayTest::rand_str[I]
#define GET_COMPLEXF_RAND(T, I) cast_complex_vector<T, float>(ValueArrayTest::rand_com[I])
#define GET_COMPLEXD_RAND(T, I) cast_complex_vector<T, double>(ValueArrayTest::rand_com[I])
#define GET_COMPLEXLD_RAND(T, I) cast_complex_vector<T, long double>(ValueArrayTest::rand_com[I])

template<typename T, typename U>
std::vector<T> cast_vector(std::vector<U>& input) {
    std::vector<T> output(input.begin(), input.end());
    return output;
}

template<typename T, typename U>
std::vector<T> cast_complex_vector(std::vector<complex_double_t> &input) {
    std::vector<T> output;
    for (auto inp : input) {
        T val = {static_cast<U>(inp.re), static_cast<U>(inp.im)};
        output.push_back(val);
    }
    return output;
}

template<typename T>
static inline
std::stringstream makeInvalid(const std::vector<T> &val) {
    std::stringstream ss;
    ss << "scalararry" << std::endl << "type " << T_STRING << std::endl << "precision " << 8 << std::endl
       << "dim1 " << 1 << std::endl << "dims " << val.size() << std::endl
       << "unit None" << std::endl << "value ";
    communication::utils::join<T>(val, ss);
    return ss;
}
template<typename T>
static inline
std::stringstream makeInvalidtype(const std::vector<T> &val) {
    std::stringstream ss;
    ss << "scalararray" << std::endl << "tpe " << T_STRING << std::endl << "precision " << 8 << std::endl
       << "dim1 " << 1 << std::endl << "dims " << val.size() << std::endl
       << "unit None" << std::endl << "value ";
    communication::utils::join<T>(val, ss);
    return ss;
}
template<typename T>
static inline
std::stringstream makeInvalidprecision(const std::vector<T> &val) {
    std::stringstream ss;
    ss << "scalararray" << std::endl << "type " << T_STRING << std::endl << "prcision " << 8 << std::endl
       << "dim1 " << 1 << std::endl << "dims " << val.size() << std::endl
       << "unit None" << std::endl << "value ";
    communication::utils::join<T>(val, ss);
    return ss;
}
template<typename T>
static inline
std::stringstream makeInvaliddim1(const std::vector<T> &val) {
    std::stringstream ss;
    ss << "scalararray" << std::endl << "type " << T_STRING << std::endl << "precision " << 8 << std::endl
       << "dim " << 1 << std::endl << "dims " << val.size() << std::endl
       << "unit None" << std::endl << "value ";
    communication::utils::join<T>(val, ss);
    return ss;
}
template<typename T>
static inline
std::stringstream makeInvaliddims(const std::vector<T> &val) {
    std::stringstream ss;
    ss << "scalararray" << std::endl << "type " << T_STRING << std::endl << "precision " << 8 << std::endl
       << "dim1 " << 1 << std::endl << "dms " << val.size() << std::endl
       << "unt None" << std::endl << "value ";
    communication::utils::join<T>(val, ss);
    return ss;
}

template<typename T>
static inline
std::stringstream makeInvalidunit(const std::vector<T> &val) {
    std::stringstream ss;
    ss << "scalararray" << std::endl << "type " << T_STRING << std::endl << "precision " << 8 << std::endl
       << "dim1 " << 1 << std::endl << "dims " << val.size() << std::endl
       << "unt None" << std::endl << "value ";
    communication::utils::join<T>(val, ss);
    return ss;
}
template<typename T>
static inline
std::stringstream makeInvalidvalue(const std::vector<T> &val) {
    std::stringstream ss;
    ss << "scalararray" << std::endl << "type " << T_STRING << std::endl << "precision " << 8 << std::endl
       << "dim1 " << 1 << std::endl << "dims " << val.size() << std::endl
       << "unit pixel" << std::endl << "vale ";
    communication::utils::join<T>(val, ss);
    return ss;
}


#define CONSTRUCTOR(T, U) \
TEST(ValueArrayTest, constructor_ ## T) { \
    std::vector<T> input = GET_ ## U ## _RAND(T, 0); \
    ValueArray1D<T> val(input);           \
    EXPECT_EQ(val.getDims(), input.size());          \
    EXPECT_NE(val.getDims(), 0);          \
    ValueArray1D<T> val2("pixels");       \
    EXPECT_NE(val, val2); \
    auto dv = (T *) malloc(sizeof(T) * input.size());\
    for (auto i = 0; i < input.size(); i++) {        \
        dv[i] = input[i]; \
    }                     \
    ValueArray1D<T> val3(dv, input.size());          \
    EXPECT_EQ(val, val3); \
    free(dv);             \
    ValueArray1D<T> val4(input.size());   \
    EXPECT_EQ(val.getDims(), val4.getDims());        \
    val4.display("");     \
}

#define CONSTRUCTOR_STR(T) \
TEST(ValueArrayTest, constructor_ ## T) { \
    std::vector<T> input = GET_STR_RAND(T, 0); \
    ValueArray1D<T> val(input);           \
    EXPECT_EQ(val.getDims(), input.size());    \
    EXPECT_NE(val.getDims(), 0);          \
    ValueArray1D<T> val2("pixels");       \
    EXPECT_NE(val, val2);  \
    ValueArray1D<T> val4(input.size());   \
    EXPECT_EQ(val.getDims(), val4.getDims());  \
    val4.display("");      \
}

#define SET_GET(T, U) \
TEST(ValueArrayTest, set_get_ ## T) { \
    std::vector<T> v = GET_ ## U ## _RAND(T, 1); \
    ValueArray1D<T> val;              \
    val.set(v);       \
    std::vector<T> v2;\
    v2 = val.get();   \
    EXPECT_EQ(v, v2); \
    ValueArray1D<T> val3;             \
    auto ii = (T*)malloc(sizeof(T) * v.size());  \
    for (auto i = 0; i < v.size(); i++) {        \
        ii[i] = v[i]; \
    }                 \
    try{              \
        val3.set(ii); \
        EXPECT_TRUE(false) << "Set invalid failed"; \
    } catch (...) {}  \
    std::string units = "pixels";     \
    try {             \
        val3.set(ii, units);          \
        EXPECT_TRUE(false) << "Set invalid failed"; \
    } catch(...) {}   \
    val3.set(ii, v.size(), units);    \
    auto jj = (T*)malloc(sizeof(T)*v.size());    \
    size_t size;      \
    val3.get(jj, size);               \
    EXPECT_EQ(size, v.size());        \
    EXPECT_EQ(jj[2], ii[2]);          \
    free(ii);         \
    free(jj);         \
    std::vector<T> zz = GET_ ## U ## _RAND(T, 3);\
    ValueArray1D<T> val4;             \
    val4.set(zz, "mm");               \
    std::vector<T> yy, xx, qq;        \
    std::string un;   \
    val4.get(yy);     \
    EXPECT_EQ(yy, zz);\
    val4.get(xx, un); \
    EXPECT_EQ(xx, zz);\
    EXPECT_EQ(un, "mm");              \
    auto hh = (T*)malloc(sizeof(T) * zz.size()); \
    size_t hh_size;   \
    val4.get(hh, hh_size, un);        \
    EXPECT_EQ(hh_size, zz.size());    \
    EXPECT_EQ(zz[0], hh[0]);          \
    free(hh);         \
}

#define SET_GET_STR(T, U) \
TEST(ValueArrayTest, set_get_ ## T) { \
    std::vector<T> v = GET_ ## U ## _RAND(T, 1); \
    ValueArray1D<T> val;  \
    val.set(v);           \
    std::vector<T> v2;    \
    v2 = val.get();       \
    EXPECT_EQ(v, v2);     \
    ValueArray1D<T> val3; \
    std::vector<T> zz = GET_ ## U ## _RAND(T, 3);\
    ValueArray1D<T> val4; \
    val4.set(zz, "mm");   \
    std::vector<T> yy, xx, qq;        \
    std::string un;       \
    val4.get(yy);         \
    EXPECT_EQ(yy, zz);    \
    val4.get(xx, un);     \
    EXPECT_EQ(xx, zz);    \
    EXPECT_EQ(un, "mm");  \
}

#define TMISC(T) \
TEST(ValueArrayTest, misc_ ## T) { \
    ValueArray1D<T> val;           \
    EXPECT_EQ(val.nargs_exp(), 4); \
    EXPECT_EQ(val.getType(), T_NDARRAY); \
    val.display("");               \
}

#define READ_WRITE(T, U) \
TEST(ValueArrayTest, read_write_ ## T) { \
    ValueArray1D<T> val; \
    std::vector<T> v = GET_ ## U ## _RAND(T, 3); \
    val.set(v);          \
    std::stringstream ss;\
    ss << val;           \
    std::cout << ss.str() << std::endl;                     \
    std::string s = ss.str();            \
    EXPECT_NE(s.find("scalararray"), std::string::npos); \
    ValueArray1D<T> val2;\
    ss >> val2;          \
    EXPECT_TRUE(val == val2);            \
    std::vector<T> vv = GET_ ## U ## _RAND(T, 2);\
    ValueArray1D<T> val3;\
    ss = makeInvalid(vv);\
    try {                \
        val3.read(ss);   \
        EXPECT_TRUE(false) << "Make invalid failed";     \
    } catch (std::invalid_argument) {}   \
    ss = makeInvalidtype(vv);            \
    try {                \
        val3.read(ss);   \
        EXPECT_TRUE(false) << "Make invalid type failed";\
    } catch (std::invalid_argument) {}   \
    ss = makeInvalidprecision(vv);       \
    try {                \
        val3.read(ss);   \
        EXPECT_TRUE(false) << "Make invalid precision failed"; \
    } catch (std::invalid_argument) {}   \
    ss = makeInvalidunit(vv);            \
    try {                \
        val3.read(ss);   \
        EXPECT_TRUE(false) << "Make invalid unit failed";\
    } catch (std::invalid_argument) {}   \
    ss = makeInvalidvalue(vv);           \
    try {                \
        val3.read(ss);   \
        EXPECT_TRUE(false) << "Make invalid value failed";     \
    } catch (std::invalid_argument) {}   \
    ss = makeInvaliddim1(vv);            \
    try {                \
        val3.read(ss);   \
        EXPECT_TRUE(false) << "Make invalid dim1 failed";\
    } catch (std::invalid_argument) {}   \
    ss = makeInvaliddims(vv);            \
    try {                \
        val3.read(ss);   \
        EXPECT_TRUE(false) << "Make invalid dims failed";\
    } catch (std::invalid_argument) {}   \
}

#define MAKE_TEST(T, U) \
CONSTRUCTOR(T, U)       \
SET_GET(T, U)           \
TMISC(T)                \
READ_WRITE(T, U)

#define MAKE_STR_TEST(T, U) \
CONSTRUCTOR_STR(T)          \
SET_GET_STR(T, U)               \
TMISC(T)                    \
READ_WRITE(T, U)

#define MAKE_INT_TEST(T) MAKE_TEST(T, INT) MAKE_TEST(u ## T, UINT)
#define MAKE_FLOAT_TEST(T) MAKE_TEST(T, FLOAT)

namespace {
using namespace std;
EVAL(MAP(MAKE_INT_TEST, int8_t, int16_t, int32_t, int64_t))
EVAL(MAP(MAKE_FLOAT_TEST, float, double, ldouble))
MAKE_STR_TEST(string, STR)
MAKE_TEST(complex_float_t, COMPLEXF)
MAKE_TEST(complex_double_t, COMPLEXD)
MAKE_TEST(complex_long_double_t, COMPLEXLD)

}
