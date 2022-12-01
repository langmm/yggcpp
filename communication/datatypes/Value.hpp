#pragma once
#include <string>
#include <vector>
#include <stdexcept>
#include <algorithm>
#include "utils/macros.hpp"
#include "ValueItem.hpp"
#include "utils/complex_type.hpp"
#include "utils/templates.hpp"

namespace communication {
namespace datatypes {

ENABLE_TYPECHECK
class Value : public ValueItem {
public:

    Value() : ValueItem(GET_ST<T>(), T_SCALABLE, "", PRECISION<T>()) {}
    Value(const T &val, const std::string &unit="") :
            ValueItem(GET_ST<T>(), T_SCALABLE, unit, PRECISION<T>()) {
        value = val;
    }
    //explicit Value(const value_t* val);
    ~Value() = default;

    T get() const {
        return value;
    }

    void get(T &val) const {
        val = value;
    }

    void get(T &val, std::string& un) const {
        val = value;
        un = unit;
    }

    void get(T *val) const {
        *val = value;
    }

    void get(T* val, std::string& un) const {
        *val = value;
        un = unit;
    }

    void set(T* val) {
        value = *val;
    }

    void set(T* val, std::string un) {
        value = *val;
        unit = un;
    }

    void set(T val) {
        value = val;
    }

    void set(T val, std::string un) {
        value = val;
        unit = un;
    }

    void display(const std::string& indent) const override {
        printf("%s%-15s = %s\n", indent.c_str(), "type", "SCALAR");
        printf("%s%-15s = %s\n", indent.c_str(), "subtype", mapsub.at(type).c_str());
        printf("%s%-15s = %d\n", indent.c_str(), "precision", precision);
        //printf("%s%-15s = %s\n", indent.c_str(), "value", make_string(value).c_str());
        //printf("%s%-15s = %s\n");
    }

    int nargs_exp() const override {return 2;}
    DTYPE getType() const override {return T_SCALAR;}

    void process_value(std::ostream& out) {
        out << value;
    }

    std::ostream& write(std::ostream& out) override {
        out << "scalarvalue" << std::endl
            << "type " << type << std::endl
            << "precision " << precision << std::endl
            << "unit ";
        if (unit.empty())
            out << "None";
        else
            out << unit;
        out << std::endl << " value ";
        process_value(out);
        out << std::endl;
        return out;
    }

    void reprocess_value(std::istream& in) {
        in >> value;
    }

    std::istream& read(std::istream& in) override {
        std::string word;
        in >> std::ws;
        in >> word;
        if (word != "scalarvalue")
            throw std::invalid_argument("Missing keyword scalarvalue, got " + word);
        in >> word;
        if (word != "type")
            throw std::invalid_argument("Missing keyword type, got " + word);
        in >> type;
        in >> word;
        if (word != "precision")
            throw std::invalid_argument("Missing keyword precision, got " + word);
        in >> precision;
        in >> word;
        if (word != "unit")
            throw std::invalid_argument("Missing keyword unit, got " + word);
        in >> unit;
        in >> word;
        if (word != "value")
            throw std::invalid_argument("Missing keyword value, got " + word);
        reprocess_value(in);
        if (unit == "None")
            unit .clear();
        return in;
    }

    friend std::ostream &operator<<(std::ostream &out, Value<T> &v) {
        return v.write(out);
    }
    friend std::istream &operator>>(std::istream &in, Value<T> &v) {
        return v.read(in);
    }
    bool operator==(const Value<T> &b) const {
        if (!COMPARE<T>(this->value, b.value)) {
            return false;
        }
        if (this->precision != b.precision) {
            return false;
        }
        if (this->unit != b.unit)
            return false;
        return true;
    }
    bool operator==(const ValueItem &b) const override {
        if (b.vtype != T_SCALABLE)
            return false;
        if (b.type != this->type)
            return false;
        if (b.getPrecision() != this->getPrecision())
            return false;
        return *(static_cast<Value<T>*>(const_cast<ValueItem*>(&b))) == *this;
    }
    bool operator!=(const ValueItem &b) const override {return !this->operator==(b);}
    template<typename H, std::enable_if_t<!std::is_same<T, H>::value, bool> = true>
    bool operator==(const Value<H> &b) const {return false;}
    template<typename H, std::enable_if_t<!std::is_same<T, H>::value, bool> = true>
    bool operator!=(const Value<H> &b) const {return true;}
    bool operator!=(const Value<T> &b) const {
        return !(this->operator==(b));
    }

private:
    T value;
};

template<>
inline
void Value<float>::process_value(std::ostream &out) {
    out.setf(std::ios::fixed);
    out.precision(std::numeric_limits<float>::digits10);
    out << value;
}

template<>
inline
void Value<double>::process_value(std::ostream &out) {
    out.setf(std::ios::fixed);
    out.precision(std::numeric_limits<double>::digits10);
    out << value;
}

template<>
inline
void Value<long double>::process_value(std::ostream &out) {
    out.setf(std::ios::fixed);
    out.precision(std::numeric_limits<long double>::digits10);
    out << value;
}

template<>
inline
void Value<uint8_t>::process_value(std::ostream &out) {
    out << +value;
}

template<>
inline
void Value<int8_t>::process_value(std::ostream &out) {
    out << +value;
}


template<>
inline
void Value<std::string>::process_value(std::ostream& out) {
    std::string temp = value;
    int i;
    for (i = 0; i < REPLACE_SPACE.size(); i++) {
        if (temp.find(REPLACE_SPACE[i]) == std::string::npos) {
            break;
        }
    }
    std::replace(temp.begin(), temp.end(), ' ', REPLACE_SPACE[i]);
    out << REPLACE_SPACE[i] << " " << temp;
}

template<>
inline
void Value<std::string>::reprocess_value(std::istream &in) {
    char spacer;
    in >> spacer;
    in >> value;
    std::replace(value.begin(), value.end(), spacer, ' ');
}

template<>
inline
void Value<uint8_t>::reprocess_value(std::istream &in) {
    int v;
    in >> v;
    value = static_cast<uint8_t>(v);
}

template<>
inline
void Value<int8_t>::reprocess_value(std::istream &in) {
    int v;
    in >> v;
    value = static_cast<int8_t>(v);
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
