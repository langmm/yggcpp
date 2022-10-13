#pragma once
#include <string>
#include <vector>
#include "utils/macros.hpp"
#include "ValueItem.hpp"
#include "utils/tools.hpp"

//#define INIT_VALUE_METHODS(type) \
//template<>                                \
//Value<type>::Value(const type &val, const std::string &unit, const int& precision) : \
//    ValueItem(submap.at(#type), unit, precision) { \
//    value = val;                          \
//}

//#define INIT_VALUE_FROM_VT(type) \
//template<>                       \
//Value<type>::Value(const value_t* val) : \
//    ValueItem(submap.at(#type), val->unit, val->precision) { \
//    value = *(static_cast<type*>(val->obj)); \
//    }
//                                 \
//
//#define VALUE_T_GET_METHOD(tp)          \
//template<>                                 \
//value_t* Value<tp>::toValue_t() {        \
//    auto val = (value_t*)malloc(sizeof(value_t)); \
//    val->obj = static_cast<void*>(&value); \
//    val->unit = (char*)malloc(sizeof(char) * unit.size()); \
//    memcpy(val->unit, unit.c_str(), unit.size()); \
//    val->type = type;                      \
//    return val;                            \
//}




namespace communication {
namespace datatypes {


template<typename T>
class Value : public ValueItem {
public:
    Value() = delete;
    Value(const T &val, SUBTYPE st, const std::string &unit="", const int& precision=0);
    //explicit Value(const value_t* val);
    ~Value() = default;
    void get(T &val) const override;
    void get(T &val, std::string& un) const override;
    //value_t* toValue_t();
    T getValue() const;
private:
    T value;
};

//template<typename T>
//Value<T>* toValue(const value_t* val);

template<typename T>
void Value<T>::get(T& val) const {
    val = value;
}

//EVAL(MAP(INIT_VALUE_METHODS, int, bool, std::string, float, uint))
//EVAL(MAP(INIT_VALUE_FROM_VT, int, bool, float, uint))
template<typename T>                                \
Value<T>::Value(const T &val, SUBTYPE st, const std::string &unit, const int& precision) : \
    ValueItem(st, unit, precision) { \
    value = val;                          \
}
//template<>
//Value<std::string>::Value(const value_t* val) : ValueItem(T_STRING, val->unit, 0) {
//    value = std::string(static_cast<char*>(val->obj));
//}

//template<>
//Value<complex_float_t>::Value(const value_t* val) : ValueItem(T_COMPLEX, val->unit, sizeof(float)/8) {
////    auto temp = static_cast<complex_float_t*>(val->obj);
//    value.re = temp->re;
//    value.im = temp->im;
//}

template<>
Value<complex_float_t>::Value(const complex_float_t &val, const std::string &unit, const int& precision) :
        ValueItem(T_COMPLEX, unit, sizeof(float)/8) {
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
