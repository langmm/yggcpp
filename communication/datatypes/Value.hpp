#pragma once
#include <string>
#include <vector>
#include "utils/macros.hpp"
#include "ValueItem.hpp"
#include "utils/tools.hpp"

namespace communication {
namespace datatypes {

template<typename T>
class Value : public ValueItem {
public:
    using ValueItem::get;
    using ValueItem::set;
    Value() = delete;
    Value(const T &val, SUBTYPE st, const std::string &unit="", const int& precision=0);
    explicit Value(SUBTYPE st, const std::string &unit="", const int& precision=0);
    //explicit Value(const value_t* val);
    ~Value() = default;
    void get(T &val) const override;
    void get(T &val, std::string& un) const override;
    void set(T &val) override;
    void set(T &val, std::string& un) override;
    //value_t* toValue_t();
    T getValue() const;
private:
    T value;
};

template<typename T>
Value<T>::Value(SUBTYPE st, const std::string &unit, const int &precision) : ValueItem(st, unit, precision){}
//template<typename T>
//Value<T>* toValue(const value_t* val);

template<typename T>
void Value<T>::set(T &val) {
    value = val;
}

template<typename T>
void Value<T>::set(T &val, std::string &un) {
    value = val;
    unit = un;
}

template<typename T>
void Value<T>::get(T& val) const {
    val = value;
}

template<typename T>                                \
Value<T>::Value(const T &val, SUBTYPE st, const std::string &unit, const int& precision) : \
    ValueItem(st, unit, precision) { \
    value = val;                          \
}

template<>
Value<complex_float_t>::Value(const complex_float_t &val, SUBTYPE st, const std::string &unit, const int& precision) :
        ValueItem(T_COMPLEX, unit, 4) {
    value.re = val.re;
    value.im = val.im;
}
template<>
Value<complex_double_t>::Value(const complex_double_t &val, SUBTYPE st, const std::string &unit, const int& precision) :
        ValueItem(T_COMPLEX, unit, 8) {
    value.re = val.re;
    value.im = val.im;
}
template<>
Value<complex_long_double_t>::Value(const complex_long_double_t &val, SUBTYPE st, const std::string &unit, const int& precision) :
        ValueItem(T_COMPLEX, unit, 12) {
    value.re = val.re;
    value.im = val.im;
}
template<>
void Value<complex_float_t>::set(complex_float_t &val, std::string &unit) {
    value.re = val.re;
    value.im = val.im;
    this->unit = unit;
}

template<>
void Value<complex_float_t>::set(complex_float_t &val) {
    value.re = val.re;
    value.im = val.im;
}

template<>
void Value<complex_double_t>::set(complex_double_t &val, std::string &unit) {
    value.re = val.re;
    value.im = val.im;
    this->unit = unit;
}

template<>
void Value<complex_double_t>::set(complex_double_t &val) {
    value.re = val.re;
    value.im = val.im;
}

template<>
void Value<complex_long_double_t>::set(complex_long_double_t &val, std::string &unit) {
    value.re = val.re;
    value.im = val.im;
    this->unit = unit;
}

template<>
void Value<complex_long_double_t>::set(complex_long_double_t &val) {
    value.re = val.re;
    value.im = val.im;
}

template<typename T>
void Value<T>::get(T &val, std::string &un) const {
    val = value;
    un = unit;
}

template<typename T>
T Value<T>::getValue() const {
    return value;
}

//EVAL(MAP(VALUE_T_GET_METHOD, int, bool, float, uint, complex_float_t))

/*template<>
value_t* Value<std::string>::toValue_t() {
    auto val = (value_t*)malloc(sizeof(value_t));
    val->obj = (void*)malloc(sizeof(char) * value.size());
    memcpy(val->obj, value.c_str(), value.size());
    val->unit = (char*)malloc(sizeof(char) * unit.size());
    memcpy(val->unit, unit.c_str(), unit.size());
    val->type = T_STRING;
    return val;
}*/


} // communication
} // datatype
