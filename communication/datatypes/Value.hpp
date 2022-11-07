#pragma once
#include <string>
#include <vector>
#include "utils/macros.hpp"
#include "ValueItem.hpp"
#include "utils/complex_type.hpp"
#include "utils/templates.hpp"

namespace communication {
namespace datatypes {

ENABLE_TYPECHECK
class Value : public ValueItem {
public:
    using ValueItem::get;
    using ValueItem::set;
    Value() = delete;
    Value(const T &val, SUBTYPE st, const std::string &unit="", const uint8_t& precision=0) :
            ValueItem(st, T_SCALABLE, unit, precision) {
        value = val;
    }
    explicit Value(SUBTYPE st, const std::string &unit="", const uint8_t& precision=0) :
            ValueItem(st, T_SCALABLE, unit, precision){}
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

    void set(T* val, std::string& un) {
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

    int nargs_exp() const override {return 4;}
    DTYPE getType() const override {return T_SCALAR;}

    std::ostream& write(std::ostream& out) override {
        out << "scalarvalue" << std::endl
            << " type " << type << std::endl
            << " precision " << precision << std::endl
            << " unit ";
        if (unit.empty())
            out << "None";
        else
            out << unit;
        out << std::endl << " value " << value;
        out << std::endl;
        return out;
    }

    std::istream& read(std::istream& in) override {
        std::string word;
        in >> std::ws;
        in >> word;
        if (word != "scalarvalue")
            throw std::exception();
        in >> type;
        in >> precision;
        in >> value;
        return in;
    }
    }
private:
    T value;
};

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
