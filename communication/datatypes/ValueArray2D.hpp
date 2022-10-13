#pragma once
#include "ValueItem.hpp"
#include <vector>
#include "value_t.h"

#define INIT_VALUE_2DARRAY(type) \
template<>                              \
ValueArray2D<type>::ValueArray2D(std::vector<std::vector<type> > &val, const std::string& unit) : ValueItem(submap.at(#type), unit) { \
    value = val;                        \
}
#define INIT_VALUE_POINTER_2DARRAY(type)        \
template<>                              \
ValueArray2D<type>::ValueArray2D(type** val, const size_t& N, const size_t& M, const std::string& unit) : ValueItem(submap.at(#type), unit) { \
    value.resize(N);                            \
    for (auto i = 0; i < N; i++) { \
        value[i] = std::vector<type>(val[i], val[i] + M); \
    } \
}

#define GET_2DARRAY(X) \
template<>              \
void ValueArray2D<X>::get(std::vector<std::vector<X> > &val) const { \
    val = value;        \
}                       \
template<>              \
void ValueArray2D<X>::get(std::vector<std::vector<X> > &val, std::string &un) const { \
    val = value;        \
    un = unit;          \
}

// TODO FIX for free
#define GET_2DARRAY_POINTER(X) \
template<>                      \
void ValueArray2D<X>::get(X** val, size_t &N, size_t& M) { \
    if (val != nullptr)             \
        free(val);                   \
    val = (X**)malloc(sizeof(X*) * value.size()); \
    for (auto i = 0; i < value[0].size(); i++) {           \
        val[i] = (X*)malloc(sizeof(X) * value[0].size());  \
        val[i] = &value[i][0]; \
    } \
    M = value[0].size();         \
    N = value.size();        \
}                               \
template<>                      \
void ValueArray2D<X>::get(X** val, size_t& N, size_t& M, std::string &un) { \
    get(val, N, M);                   \
    un = unit; \
}

#define VALUE_2DARRAY_TO_VAT(X) \
template<>                   \
value_2Darray_t* ValueArray2D<X>::toValue_t() { \
    auto val = (value_2Darray_t*)malloc(sizeof(value_2Darray_t)); \
    X** temp = nullptr;       \
    get(temp, val->N, val->M);       \
    val->type = submap.at(#X); \
    val->unit = (char*)malloc(sizeof(char) * unit.size());           \
    memcpy(val->unit, unit.c_str(), unit.size()); \
    }

#define VALUE_ARRAY_FROM_VT(X) \
template<>                     \
ValueArray2D<X>::ValueArray2D(value_2Darray_t* val) : ValueArray2D(static_cast<X**>(val->obj), val->N, val->M, val->unit) {}

namespace communication {
namespace datatypes {

template<typename T>
class ValueArray2D : public ValueItem {
public:
    ValueArray2D() = delete;
    explicit ValueArray2D(std::vector<std::vector<T> > &val, const std::string& unit="");
    ValueArray2D(T** val, const size_t &N, const size_t &M, const std::string& unit="");
    explicit ValueArray2D(value_2Darray_t* val);
    ~ValueArray2D();
    void get(T** val, size_t& N, size_t& M);
    void get(std::vector<std::vector<T> > &val) const;
    void get(T** val, size_t& N, size_t& M, std::string &unit);
    void get(std::vector<std::vector<T> > &val, std::string& un) const;
    value_2Darray_t* toValue_t();
    std::vector<std::vector<T> >  getValue() const;
    std::vector<T>& operator[](const size_t& idxn);
private:
    std::vector<std::vector<T> > value;
};

template<typename T>
ValueArray2D<T>::~ValueArray2D() {
    for (auto &v : value)
        v.clear();
    value.clear();
}

EVAL(MAP(INIT_VALUE_2DARRAY, int, float, uint, complex_float_t))
EVAL(MAP(INIT_VALUE_POINTER_2DARRAY, int, float, uint, complex_float_t))
EVAL(MAP(GET_2DARRAY, int, float, uint, complex_float_t))
EVAL(MAP(GET_2DARRAY_POINTER, int, float, uint, complex_float_t))
EVAL(MAP(VALUE_2DARRAY_TO_VAT, int, float, uint, complex_float_t))
EVAL(MAP(VALUE_ARRAY_FROM_VT, int, float, uint, complex_float_t))
} // communicator
} // datatypes
