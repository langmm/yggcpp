#pragma once
#include "ValueItem.hpp"
#include "utils/tools.hpp"
#include <vector>
#include <numeric>
#include <memory>

namespace communication {
namespace datatypes {


//template<typename T, std::enable_if_t<std::is_arithmetic<T>::value ||
//                                      is_complex<T>::value || std::is_same<T, std::string>::value, bool> = true>
ENABLE_TYPECHECK
class ValueArray1D : public ValueItem {
public:
    ValueArray1D() : ValueItem(GET_ST<T>(), T_ARRAY1D, "", PRECISION<T>()), dims(0){}
    //ValueArray(std::vector<T> &val, SUBTYPE st, const std::string& unit="", const int& precision=0);
    explicit ValueArray1D(const size_t& dim, const std::string& un="") :
            ValueItem(GET_ST<T>(), T_ARRAY1D, un, PRECISION<T>()), dims(dim) {
        value.reserve(dim);
    }
    explicit ValueArray1D(const std::string& un) :
            ValueItem(GET_ST<T>(), T_ARRAY1D, un, PRECISION<T>()) {}
    explicit ValueArray1D(std::vector<T> &val, const std::string& un="") :
            ValueItem(GET_ST<T>(), T_ARRAY1D, un, PRECISION<T>()), dims(val.size()){
        value = val;
    }
    ValueArray1D(T* val, const size_t& dim, const std::string& unit="") :
            ValueItem(GET_ST<T>(), T_ARRAY1D, unit, PRECISION<T>()), dims(dim) {
        //value.resize(dim);
        //for (auto i = 0; i < dim; i++) {
        //    value[i] = val[i];
        //}
        value.insert(value.end(), val, val + dim);
    }

    ~ValueArray1D() = default;
    using ValueItem::get;
    void get(T* val, size_t& dim) {
        //if (val == nullptr)
        //    return;
        //std::cout << val << "  ";
        //val = (T*)realloc(val, sizeof(T) * dims);
        //std::cout << val << std::endl;
        for(auto i = 0; i < dims; i++)
            val[i] = value[i];
        dim = this->dims;
    }
//    void get(std::vector<T> &val) const;
    void get(T* val, size_t& dim, std::string &un) {
        get(val, dim);
        un = unit;
    }

    void get(std::vector<T> &val, std::string &un) {
        val = value;
        un = unit;
    }

    void get(std::vector<T> &val) {
        val = value;
    }

    std::vector<T> &get() {
        return value;
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
            throw std::invalid_argument("Missing keyword scalararray, got " + word);
        in >> word;
        if (word != "type")
            throw std::invalid_argument("Missing keyword type, got " + word);
        in >> type;
        in >> word;
        if (word != "precision")
            throw std::invalid_argument("Missing keyword precision, got " + word);
        in >> precision;
        in >> word;
        if (word != "dim1")
            throw std::invalid_argument("Missing keyword dim1, got " + word);
        in >> dims;
        in >> word;
        if (word != "dims")
            throw std::invalid_argument("Missing keyword dims, got " + word);
        in >> dims;
        in >> word;
        if (word != "unit")
            throw std::invalid_argument("Missing keyword unit, got " + word);
        in >> unit;
        if (unit == "None")
            unit .clear();
        in >> word;
        if (word != "value")
            throw std::invalid_argument("Missing keyword value, got " + word);
        communication::utils::parse<T>(value, dims, in);
        return in;
    }


    friend std::ostream &operator<<(std::ostream &out, ValueArray1D<T> &v) {
        return v.write(out);
    }

    friend std::istream &operator>>(std::istream &in, ValueArray1D<T> &v) {
        return v.read(in);
    }
    bool operator==(const ValueArray1D<T> &b) const {
        if (this->dims != b.dims) {
            return false;
        }
        if (!COMPARE<T>(this->value, b.value)) {
            return false;
        }
        if (this->unit != b.unit)
            return false;
        return true;
    }
    bool operator==(const ValueItem &b) const override {
        if (b.vtype != T_ARRAY1D)
            return false;
        if (b.type != this->type)
            return false;
        if (b.getPrecision() != this->getPrecision())
            return false;
        return *(static_cast<ValueArray1D<T>*>(const_cast<ValueItem*>(&b))) == *this;
    }
    bool operator!=(const ValueItem &b) const override {
        return !(this->operator==(b));
    }
    template<typename H, std::enable_if_t<!std::is_same<T, H>::value, bool> = true>
    bool operator==(const ValueArray1D<H> &b) const {return false;}
    template<typename H, std::enable_if_t<!std::is_same<T, H>::value, bool> = true>
    bool operator!=(const ValueArray1D<H> &b) const {return true;}
    bool operator!=(const ValueArray1D<T> &b) const {
        return !(this->operator==(b));
    }
    T& operator[](const size_t& idx) const {
        return value[idx];
    }
    size_t getDims() const {return dims;}
    size_t size() const override {return dims;}
private:
    std::vector<T> value;
    size_t dims;

};

} // communication
} // datatypes
