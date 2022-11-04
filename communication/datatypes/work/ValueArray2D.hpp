#pragma once
#include "ValueItem.hpp"
#include <vector>

namespace communication {
namespace datatypes {

template<typename T>
class ValueArray2D : public ValueItem {
public:
    ValueArray2D() = delete;
    ValueArray2D(std::vector<std::vector<T> > &val, SUBTYPE st, const std::string& unit="", const int& precision=0);
    ValueArray2D(T** val, const size_t &N, const size_t &M, SUBTYPE st, const std::string& unit="", const int& precision=0);
    ValueArray2D(SUBTYPE st, const std::string& un, const int& precision=0);
    ~ValueArray2D();
    void get(T** val, size_t& N, size_t& M);
    void get(std::vector<std::vector<T> > &val) const;
    void get(T** val, size_t& N, size_t& M, std::string &unit);
    void get(std::vector<std::vector<T> > &val, std::string& un) const;
    void set(T** val, const size_t &N, const size_t& M);
    void set(std::vector<std::vector<T> > &val);
    void set(T** val, const size_t &N, const size_t& M, std::string &un);
    void set(std::vector<std::vector<T> > &val, std::string& un);

    std::vector<std::vector<T> >  getValue() const;
    std::vector<T>& operator[](const size_t& idxn) const;
private:
    std::vector<std::vector<T> > value;
};

template<typename T>
ValueArray2D<T>::ValueArray2D(SUBTYPE st, const std::string& un, const int& precision) :
        ValueItem(st, un, precision) {}

template<typename T>
std::vector<std::vector<T> > ValueArray2D<T>::getValue() const {
    return value;
}
template<typename T>
ValueArray2D<T>::ValueArray2D(std::vector<std::vector<T> > &val, SUBTYPE st, const std::string& unit, const int& precision) :
        ValueItem(st, unit, precision) {
    value = val;
}

template<typename T>
ValueArray2D<T>::ValueArray2D(T** val, const size_t& N, const size_t& M, SUBTYPE st, const std::string& unit,
                              const int& precision) : ValueItem(st, unit, precision) {
    value.resize(N);
    for (auto i = 0; i < N; i++) {
        value[i] = std::vector<T>(val[i], val[i] + M);
    }
}

template<typename T>
void ValueArray2D<T>::get(std::vector<std::vector<T> > &val) const {
    val = value;
}
template<typename T>
void ValueArray2D<T>::get(std::vector<std::vector<T> > &val, std::string &un) const {
    val = value;
    un = unit;
}

template<typename T>
void ValueArray2D<T>::get(T** val, size_t &N, size_t& M) {
    if (val == nullptr)
        return;
    val = realloc(sizeof(T*) * value.size());
    for (auto i = 0; i < value[0].size(); i++) {
        val[i] = realloc(sizeof(T) * value[0].size());
        val[i] = &value[i][0];
    }
    M = value[0].size();
    N = value.size();
}

template<typename T>
void ValueArray2D<T>::get(T** val, size_t& N, size_t& M, std::string &un) {
    get(val, N, M);
    un = unit;
}

template<typename T>
ValueArray2D<T>::~ValueArray2D() {
    for (auto &v : value)
        v.clear();
    value.clear();
}

template<typename T>
std::vector<T>& ValueArray2D<T>::operator[](const size_t& idx) const {
    if (idx >= value.size())
        return;
    return value[idx];
}
template<typename T>
void ValueArray2D<T>::set(T** val, const size_t &N, const size_t& M) {
    value.resize(N);
    for (auto i = 0; i < N; i++) {
        value[i] = std::vector<T>(val[i], val[i] + M);
    }

}
template<typename T>
void ValueArray2D<T>::set(std::vector<std::vector<T> > &val) {
    value = val;
}
template<typename T>
void ValueArray2D<T>::set(T** val, const size_t &N, const size_t& M, std::string &un) {
    set(val, N, M);
    unit = un;
}
template<typename T>
void ValueArray2D<T>::set(std::vector<std::vector<T> > &val, std::string& un) {
    value = val;
    unit = un;
}

} // communicator
} // datatypes
