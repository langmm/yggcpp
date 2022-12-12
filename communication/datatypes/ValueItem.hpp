#pragma once
#include <string>
#include "utils/macros.hpp"
#include "utils/enums.hpp"
#include "utils/complex_type.hpp"
#include "datatypes.hpp"


#define MAKE_FUNC(T) virtual void get(T &v) const {} \
virtual void get(T &v, std::string& un) const {}     \
virtual void set(T &v) {}                            \
virtual void set(T &v, std::string& un) {}           \
virtual void set(T* v) {}
//virtual void set(T v) {}
//virtual void set(T v, std::string& un) {}
#define MAKE_VECTOR_FUNC(T) virtual void get(std::vector<T> &val) const {} \
virtual void get(std::vector<T> &val, std::string& un) const {}

#define MAKE_TWODVECTOR_FUNC(T) virtual void get(std::vector<std::vector<T> > &v) const {} \
virtual void get(std::vector<std::vector<T> > &v, std::string& un) const {}

namespace communication {
namespace datatypes {

class ValueItem : public DataType {
public:
    ValueItem() = delete;
    std::string getUnit() const {
        return unit;
    }
    //EVAL(MAP(MAKE_FUNC, int8_t, int16_t,int32_t, int64_t, uint8_t, uint16_t,uint32_t, uint64_t, float, bool, double, long double, complex_float_t, complex_double_t, complex_long_double_t, std::string))
    //EVAL(MAP(MAKE_VECTOR_FUNC, int, float, bool, uint, complex_float_t, std::string))
    //EVAL(MAP(MAKE_TWODVECTOR_FUNC, int, float, bool, uint, complex_float_t))

    template<typename T>
    void set(T* v);

    template<typename T>
    void set(T* v, std::string un);

    template<typename T>
    T get() const;

    template<typename T>
    void get(T &v, std::string &un) const;

    template<typename T>
    void get(T &v) const;

    template<typename T>
    void get(T* v, std::string &un) const;

    template<typename T>
    void get(T* v) const;

    template<typename T>
    void set(T v, std::string un);

    template<typename T>
    void set(T v);

    SUBTYPE type;
    VTYPE vtype;
    unsigned short getPrecision() const {
        return precision;
    }
    virtual size_t size() const = 0;
    virtual bool operator==(const ValueItem &b) const = 0;
    virtual bool operator!=(const ValueItem &b) const = 0;
    friend std::ostream &operator>>(std::ostream &out, ValueItem &v) {
        return v.write(out);
    }
    friend std::istream &operator<<(std::istream &in, ValueItem &v) {
        return v.read(in);
    }
protected:
    explicit ValueItem(const SUBTYPE t, const VTYPE v, const std::string& val = "", const unsigned short &precision=0) :
            type(t), vtype(v), unit(val), precision(precision) {}
    std::string unit;
    unsigned short precision;
};

void set_unit(char* v, const char* unit, const size_t &unit_size);
ValueItem* createArray(SUBTYPE t, const unsigned short &precision, const size_t &size, const char* units);
ValueItem* createValue(SUBTYPE t, const unsigned short &precision, const char* units);
ValueItem* createFormatted();

}
}