#pragma once
#include <string>
#include "utils/macros.hpp"
#include "utils/enums.hpp"
#include "utils/tools.hpp"


#define MAKE_FUNC(T) virtual void get(T &v) const {} \
virtual void get(T &v, std::string& un) const {}     \
virtual void set(T &v) {}                            \
virtual void set(T &v, std::string& un) {}
#define MAKE_VECTOR_FUNC(T) virtual void get(std::vector<T> &val) const {} \
virtual void get(std::vector<T> &val, std::string& un) const {}

#define MAKE_TWODVECTOR_FUNC(T) virtual void get(std::vector<std::vector<T> > &v) const {} \
virtual void get(std::vector<std::vector<T> > &v, std::string& un) const {}

namespace communication {
namespace datatypes {
class ValueItem {
public:
    ValueItem() = delete;
    std::string getUnit() const {
        return unit;
    }
    EVAL(MAP(MAKE_FUNC, int8_t, int16_t,int32_t, int64_t, uint8_t, uint16_t,uint32_t, uint64_t, float, bool, double, long double, complex_float_t, complex_double_t, complex_long_double_t, std::string))
    //EVAL(MAP(MAKE_VECTOR_FUNC, int, float, bool, uint, complex_float_t, std::string))
    //EVAL(MAP(MAKE_TWODVECTOR_FUNC, int, float, bool, uint, complex_float_t))
    const SUBTYPE type;
    int getPrecision() const {
        return precision;
    }
protected:
    explicit ValueItem(const SUBTYPE t, const std::string& val = "", const int &precision=0) :
            type(t), unit(val), precision(precision) {}
    std::string unit;
    const int precision;
};

void set_unit(char* v, const char* unit, const size_t &unit_size);

}
}