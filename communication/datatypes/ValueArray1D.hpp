#pragma once
#include "ValueItem.hpp"
#include "utils/tools.hpp"
#include <vector>
#include <numeric>

namespace communication {
namespace datatypes {


//template<typename T, std::enable_if_t<std::is_arithmetic<T>::value ||
//                                      is_complex<T>::value || std::is_same<T, std::string>::value, bool> = true>
ENABLE_TYPECHECK
class ValueArray1D : public ValueItem {
public:
    ValueArray1D() = delete;
    //ValueArray(std::vector<T> &val, SUBTYPE st, const std::string& unit="", const int& precision=0);
    explicit ValueArray1D(const size_t& dim, const std::string& un="") :
            ValueItem(GET_ST<T>(), T_ARRAY1D, un, PRECISION<T>()), dims(dim) {}
    explicit ValueArray1D(const std::string& un="") :
            ValueItem(GET_ST<T>(), T_ARRAY1D, un, PRECISION<T>()) {}
    explicit ValueArray1D(std::vector<T> &val, const std::string& un="") :
            ValueItem(GET_ST<T>(), T_ARRAY1D, un, PRECISION<T>()), dims(val.size()){
        value = val;
    }
    ValueArray1D(T* val, const size_t& dim, const std::string& unit="") :
            ValueItem(GET_ST<T>(), T_ARRAY1D, unit, PRECISION<T>()), dims(dim) {
        value = val;
    }

    ~ValueArray1D() = default;
    using ValueItem::get;
    void get(T* val, size_t& dim) {
        if (val == nullptr)
            return;
        val = value;
        dim = this->dims;
    }
//    void get(std::vector<T> &val) const;
    void get(T* val, size_t& dim, std::string &un) {
        get(val, dim);
        un = unit;
    }
//    void get(std::vector<T> &val, std::string& un) const;
    void set(T* val) {
        if (dims == 0)
            throw std::exception();
        value.reserve(dims);
        for (auto i = 0; i < dims; i++)
            value[i] = val[i];
    }
    void set(T* val, const size_t& dim, const std::string& un="") {
        dims = dim;
        set(val);
        unit = un;
    }
//    void set(std::vector<T> &val);
    void set(T* val, std::string &un) {
        set(val);
        unit = un;
    }

    void set(std::vector<T> &val, std::string un) {
        value = val;
        dims = val.size();
        unit = un;
    }

    void set(std::vector<T> &val) {
        value = val;
        dims = val.size();
    }

    void display(const std::string& indent) const override {
        printf("%s%-15s = %s\n", indent.c_str(), "type", "ARRAY");
        printf("%s%-15s = %s\n", indent.c_str(), "subtype", mapsub.at(type).c_str());
        printf("%s%-15s = %d\n", indent.c_str(), "precision", precision);
        printf("%s%-15s = %d\n", indent.c_str(), "dimensions", 1);
        //printf("%s%-15s = %s\n");
    }

    int nargs_exp() const override {return 4;}
    DTYPE getType() const override {return T_NDARRAY;}
    std::ostream& write(std::ostream& out) override {
        out << "scalararray" << std::endl
            << " type " << type << std::endl
            << " precision " << precision << std::endl
            << " dim1 " << 1 << std::endl
            << " dims " << dims << std::endl
            << " unit ";
        if (unit.empty())
            out << "None";
        else
            out << unit;
        out << std::endl << " value ";
        communication::utils::join<T>(value, out);
        return out << std::endl;
    }

    std::istream& read(std::istream& in) override {
        std::string word;
        in >> std::ws;
        in >> word;
        if (word != "scalararray")
            throw std::exception();
        in >> word;
        if (word != "type")
            throw std::exception();
        in >> type;
        in >> word;
        if (word != "precision")
            throw std::exception();
        in >> precision;
        in >> word;
        if (word != "dim1")
            throw std::exception();
        in >> dims;
        in >> word;
        if (word != "dims")
            throw std::exception();
        in >> dims;
        in >> word;
        if (word != "unit")
            throw std::exception();
        in >> unit;
        if (unit == "None")
            unit .clear();
        in >> word;
        if (word != "value")
            throw std::exception();
        communication::utils::parse<T>(value, dims, in);
        return in;
    }


//    void set(std::vector<T> &val, std::string& un);

//    std::vector<T>& getValue() const;
    T& operator[](const size_t& idx) const {
        return value[idx];
    }
private:
    std::vector<T> value;
    size_t dims;

};

template<typename T>
void walk(std::string& out, T* values,  const size_t& count, const std::vector<size_t>& dims) {

}

//template<>
//ValueArray1D<std::vector<std::string> >::~ValueArray1D() {
//    value->clear();
//    delete value;
//}

//template<typename T>
//ValueArray1D<T>::~ValueArray1D() {}


/*template<typename T>
ValueArray<T>::~ValueArray() {
    value.clear();
}*/

/*template<typename T>
T& ValueArray<T>::getValue() const {
    return value;
}*/

/*template<typename T>
void ValueArray<T>::get(std::vector<T> &val) const {
    val = value;
}
template<typename T>
void ValueArray<T>::get(std::vector<T> &val, std::string &un) const {
    val = value;
    un = unit;
}*/

} // communication
} // datatypes
