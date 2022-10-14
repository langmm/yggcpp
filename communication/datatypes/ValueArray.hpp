#pragma once
#include "ValueItem.hpp"
#include <vector>

namespace communication {
namespace datatypes {

template<typename T>
class ValueArray : public ValueItem {
public:
    ValueArray() = delete;
    ValueArray(std::vector<T> &val, SUBTYPE st, const std::string& unit="", const int& precision=0);
    //explicit ValueArray(value_array_t* val);
    explicit ValueArray(SUBTYPE st, const std::string& un="", const int& precision=0);
    ValueArray(T* val, const size_t &N, SUBTYPE st, const std::string& unit="", const int& precision=0);
    ~ValueArray();
    using ValueItem::get;
    void get(T* val, size_t &N);
    void get(std::vector<T> &val) const;
    void get(T* val, size_t &N, std::string &un);
    void get(std::vector<T> &val, std::string& un) const;
    void set(T* val, const size_t &N);
    void set(std::vector<T> &val);
    void set(T* val, const size_t &N, std::string &un);
    void set(std::vector<T> &val, std::string& un);

    std::vector<T>& getValue() const;
    T& operator[](const size_t& idx) const;
private:
    std::vector<T> value;

};

template<typename T>
void ValueArray<T>::set(T* val, const size_t &N) {
    value = std::vector<T>(val, val + N);
}
template<typename T>
void ValueArray<T>::set(std::vector<T> &val) {
    value = val;
}
template<typename T>
void ValueArray<T>::set(T* val, const size_t &N, std::string &un) {
    value = std::vector<T>(val, val + N);
    unit = un;
}
template<typename T>
void ValueArray<T>::set(std::vector<T> &val, std::string& un) {
    value = val;
    unit = un;
}

template<typename T>
ValueArray<T>::ValueArray(std::vector<T> &val, SUBTYPE st, const std::string& unit, const int& precision) :
        ValueItem(st, unit, precision) {
    value = val;
}

template<typename T>
ValueArray<T>::ValueArray(SUBTYPE st, const std::string& un, const int& precision) :
        ValueItem(st, un, precision) {}

template<typename T>
ValueArray<T>::ValueArray(T* val, const size_t& N, SUBTYPE st, const std::string& unit, const int& precision) :
        ValueItem(st, unit, precision) {
    value = std::vector<T>(val, val + N);
}

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

template<typename T>
void ValueArray<T>::get(std::vector<T> &val) const {
    val = value;
}
template<typename T>
void ValueArray<T>::get(std::vector<T> &val, std::string &un) const {
    val = value;
    un = unit;
}

template<typename T>
void ValueArray<T>::get(T* val, size_t &N) {
    if (val == nullptr)
        return;
    val = realloc(val, sizeof(T) * value.size());
    val = &value[0];         
    N = value.size();        
}                               
template<typename T>
void ValueArray<T>::get(T* val, size_t& N, std::string &un) {
    get(val, N);                   
    un = unit; 
}
} // communication
} // datatypes
