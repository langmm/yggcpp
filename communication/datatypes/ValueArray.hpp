#pragma once
#include "ValueItem.hpp"
#include "value_t.h"
#include <vector>

#define INIT_VALUE_ARRAY(type) \
template<>                              \
ValueArray<type>::ValueArray(std::vector<type> &val, const std::string& unit) : ValueItem(submap.at(#type), unit) { \
    value = val;                        \
}
#define INIT_VALUE_POINTER_ARRAY(X)        \
template<>                              \
ValueArray<X>::ValueArray(X* val, const size_t& N, const std::string& unit) : ValueItem(submap.at(#X), unit) { \
    value = std::vector<X>(val, val + N);   \
}

#define GET_ARRAY(X) \
template<>              \
void ValueArray<X>::get(std::vector<X> &val) const { \
    val = value;        \
}                       \
template<>              \
void ValueArray<X>::get(std::vector<X> &val, std::string &un) const { \
    val = value;        \
    un = unit;          \
}

#define GET_ARRAY_POINTER(X) \
template<>                      \
void ValueArray<X>::get(X* val, size_t &N) { \
    if (val != nullptr)             \
        free(val);                   \
    val = (X*)malloc(sizeof(X) * value.size()); \
    val = &value[0];         \
    N = value.size();        \
}                               \
template<>                      \
void ValueArray<X>::get(X* val, size_t& N, std::string &un) { \
    get(val, N);                   \
    un = unit; \
}

#define VALUE_ARRAY_TO_VAT(X) \
template<>                   \
value_array_t* ValueArray<X>::toValue_t() { \
    auto val = (value_array_t*)malloc(sizeof(value_array_t)); \
    X* temp = nullptr;       \
    get(temp, val->N);       \
    val->type = submap.at(#X); \
    val->unit = (char*)malloc(sizeof(char) * unit.size());           \
    memcpy(val->unit, unit.c_str(), unit.size()); \
    }

#define VALUE_ARRAY_FROM_VT(X) \
template<>                     \
ValueArray<X>::ValueArray(value_array_t* val) : ValueArray(static_cast<X*>(val->obj), val->N, val->unit) {}


namespace communication {
namespace datatypes {

template<typename T>
class ValueArray : public ValueItem {
public:
    ValueArray() = delete;
    explicit ValueArray(std::vector<T> &val, const std::string& unit="");
    explicit ValueArray(value_array_t* val);
    ValueArray(T* val, const size_t &N, const std::string& unit="");
    ~ValueArray();
    using ValueItem::get;
    void get(T* val, size_t &N);
    void get(std::vector<T> &val) const;
    void get(T* val, size_t &N, std::string &un);
    void get(std::vector<T> &val, std::string& un) const;
    value_array_t* toValue_t();
    std::vector<T>& getValue() const;
    T& operator[](const size_t& idx) const;
private:
    std::vector<T> value;

};

template<typename T>
ValueArray<T>::~ValueArray() {
    value.clear();
}

template<typename T>
T& ValueArray<T>::operator[](const size_t& idx) const {
    if (idx >= value.size())
        return;
    return value[idx];
}

template<typename T>
std::vector<T>& ValueArray<T>::getValue() const {
    return value;
}

EVAL(MAP(INIT_VALUE_ARRAY, int, float, uint, complex_float_t))
EVAL(MAP(INIT_VALUE_POINTER_ARRAY, int, float, uint, complex_float_t))
EVAL(MAP(GET_ARRAY, int, float, uint, complex_float_t))
EVAL(MAP(GET_ARRAY_POINTER, int, float, uint, complex_float_t))
EVAL(MAP(VALUE_ARRAY_TO_VAT, int, float, uint, complex_float_t))
EVAL(MAP(VALUE_ARRAY_FROM_VT, int, float, uint, complex_float_t))
} // communication
} // datatypes
