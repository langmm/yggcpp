#pragma once
#include "ValueGroup.hpp"
#include "utils/va_list.hpp"

namespace communication {
namespace datatypes {

class FormattedData : public ValueGroup {
public:
    FormattedData() = delete;
    FormattedData(const std::string& format, bool as_array=false);

    template<typename T>
    void updateItem(const size_t itemid, const T& value) {
        if (itemid >= size())
            return;
        ValueItem* vi = this->operator[](itemid);
        _updateItem(vi, value);
    }

    void set(size_t& count, va_list_t& val);
    void set(size_t& count, ...);

    template<typename T>
    void updateArrayItem(const size_t itemid, std::vector<T>& value) {
        if (itemid >= size())
            return;
        ValueItem* vi = this->operator[](itemid);
        if (vi->vtype == T_ARRAY1D)
            return _updateArrayItem(vi, value);
        return;
    }

    template<typename T>
    void updateArrayItem(const size_t itemid, T* value, const size_t dim=0) {
        if (itemid >= size())
            return;
        ValueItem* vi = this->operator[](itemid);
        if (vi->vtype == T_ARRAY1D)
            if (vi->size() == 0 && dim == 0)
                return;
        return _updateArrayItem(vi, value, dim);
        return;
    }

    template<typename T>
    void updateItem(const size_t itemid, std::vector<T>& value) {
        updateArrayItem(itemid, value);
    }

    template<typename T>
    void updateItem(const size_t itemid, T* value, const size_t dim=0) {
        if (itemid >= size())
            return;
        ValueItem* vi = this->operator[](itemid);
        if (vi->vtype == T_ARRAY1D)
            return _updateArrayItem(vi, value, dim);
        return _updateItem(vi, *value);
    }

    void display(const std::string& indent) const override;
    int nargs_exp() const override {return 2;}
    DTYPE getType() const override {return T_FORMATTED;}

    ~FormattedData() = default;

private:
    template<typename T>
    auto _updateItem(ValueItem* vi, const T& value) -> EnableForInts<T>{
        if (vi->type == T_INT) {
            size_t prec = sizeof(T) * 8;
            if (prec > vi->getPrecision()) {
                return;
            } else if (prec ==  vi->getPrecision()) {
                vi->set(value);
            } else {
                switch (vi->getPrecision()) {
                    case 8:
                        vi->set(int8_t(value));
                        break;
                    case 16:
                        vi->set(int16_t(value));
                        break;
                    case 32:
                        vi->set(int32_t(value));
                        break;
                    case 64:
                        vi->set(int64_t(value));
                        break;
                }
            }
        } else if (vi->type == T_FLOAT) {
            size_t prec = sizeof(T);
            if (prec > vi->getPrecision()) {
                return;
            } else if(prec == vi->getPrecision()) {
                vi->set(value);
            } else {
                switch (vi->getPrecision()) {
                    case 4:
                        vi->set(float(value));
                        break;
                    case 8:
                        vi->set(double(value));
                        break;
                    case 16:
                        vi->set(ldouble(value));
                        break;
                }
            }
        } else {
            return;
        }
    }

    template<typename T>
    auto _updateItem(ValueItem* vi, const T& value) -> EnableForUInts<T>{
        if (vi->type == T_INT) {
            if (sizeof(T) * 8 > vi->getPrecision())
                return;
            switch (vi->getPrecision()) {
                case 8:
                    vi->set(int8_t(value));
                    break;
                case 16:
                    vi->set(int16_t(value));
                    break;
                case 32:
                    vi->set(int32_t(value));
                    break;
                case 64:
                    vi->set(int64_t(value));
                    break;
            }
        } else if (vi->type == T_UINT) {
            if (sizeof(T) * 8 > vi->getPrecision()) {
                return;
            } else if (sizeof(T) * 8 == vi->getPrecision()) {
                vi->set(value);
            } else {
                switch (vi->getPrecision()) {
                    case 8:
                        vi->set(uint8_t(value));
                        break;
                    case 16:
                        vi->set(uint16_t(value));
                        break;
                    case 32:
                        vi->set(uint32_t(value));
                        break;
                    case 64:
                        vi->set(uint64_t(value));
                        break;
                }
            }
        } else if (vi->type == T_FLOAT) {
            if (sizeof(T) > vi->getPrecision())
                return;
            switch (vi->getPrecision()) {
                case 4:
                    vi->set(float(value));
                    break;
                case 8:
                    vi->set(double(value));
                    break;
                case 16:
                    vi->set(ldouble(value));
                    break;
            }
        } else {
            return;
        }

    }
    template<typename T>
    auto _updateItem(ValueItem* vi, const T& value) -> EnableForString<T>{
        if (vi->type != T_STRING)
            return;
        vi->set(value);
    }

    template<typename T>
    auto _updateItem(ValueItem* vi, const T& value) -> EnableForComplex<T>{
        if (vi->type != T_COMPLEX)
            return;
        if (sizeof(T) > vi->getPrecision() * 2) {
            return;
        } else if (sizeof(T) == vi->getPrecision() * 2) {
            vi->set(value);
        } else {
            switch (vi->getPrecision()) {
                case 4:
                    vi->set(complex_f(value));
                    break;
                case 8:
                    vi->set(complex_d(value));
                    break;
                case 16:
                    vi->set(complex_ld(value));
                    break;
            }
        }
    }

    template<typename T, std::enable_if_t<std::is_floating_point<T>::value, bool> = true>
    void _updateItem(ValueItem* vi, const T& value) {
        if (vi->type == T_FLOAT) {
            if (sizeof(T) > vi->getPrecision()) {
                return;
            } else if (sizeof(T) == vi->getPrecision()) {
                vi->set(value);
            } else {
                switch (vi->getPrecision()) {
                    case 4:
                        vi->set(float(value));
                        break;
                    case 8:
                        vi->set(double(value));
                        break;
                    case 16:
                        vi->set(ldouble(value));
                        break;
                }
            }
        } else {
            return;
        }
    }

    template<typename T, std::enable_if_t<std::is_same<T, bool>::value, bool> = true>
    void _updateItem(ValueItem* vi, const T& value) {
        if (vi->type != T_BOOLEAN)
            return;
        vi->set(value);
    }

    template<typename T, std::enable_if_t<std::is_floating_point<T>::value, bool> = true>
    void _updateArrayItem(ValueItem* vi, std::vector<T>& value) {
        if (vi->type == T_FLOAT) {
            if (sizeof(T) > vi->getPrecision()) {
                return;
            } else if (sizeof(T) == vi->getPrecision()) {
                vi->set(value);
            } else {
                switch (vi->getPrecision()) {
                    case 4: {  // NOT SURE THIS IS NEEDED
                        std::vector<float> fv(value.begin(), value.end());
                        vi->set(fv);
                        break;
                    }
                    case 8: {
                        std::vector<double> dv(value.begin(), value.end());
                        vi->set(dv);
                        break;
                    }
                    case 16: {
                        std::vector<long double> ldv(value.begin(), value.end());
                        vi->set(ldv);
                        break;
                    }
                }
            }
        } else {
            return;
        }
    }

    template<typename T>
    auto _updateArrayItem(ValueItem* vi, std::vector<T>& value) -> EnableForInts<T>{
        if (vi->type == T_INT) {
            size_t prec = sizeof(T) * 8;
            if (prec > vi->getPrecision()) {
                return;
            } else if (prec ==  vi->getPrecision()) {
                vi->set(value);
            } else {
                switch (vi->getPrecision()) {
                    case 8: {
                        std::vector<int8_t> fv(value.begin(), value.end());
                        vi->set(fv);
                        break;
                    }
                    case 16: {
                        std::vector<int16_t> fv(value.begin(), value.end());
                        vi->set(fv);
                        break;
                    }
                    case 32: {
                        std::vector<int32_t> fv(value.begin(), value.end());
                        vi->set(fv);
                        break;
                    }
                    case 64: {
                        std::vector<int64_t> fv(value.begin(), value.end());
                        vi->set(fv);
                        break;
                    }
                }
            }
        } else if (vi->type == T_FLOAT) {
            size_t prec = sizeof(T);
            if (prec > vi->getPrecision()) {
                return;
            } else if(prec == vi->getPrecision()) {
                vi->set(value);
            } else {
                switch (vi->getPrecision()) {
                    case 4: {
                        std::vector<float> fv(value.begin(), value.end());
                        vi->set(fv);
                        break;
                    }
                    case 8: {
                        std::vector<double> dv(value.begin(), value.end());
                        vi->set(dv);
                        break;
                    }
                    case 16: {
                        std::vector<long double> ldv(value.begin(), value.end());
                        vi->set(ldv);
                        break;
                    }
                }
            }
        } else {
            return;
        }
    }

    template<typename T>
    auto _updateArrayItem(ValueItem* vi, std::vector<T>& value) -> EnableForUInts<T>{
        if (vi->type == T_INT) {
            if (sizeof(T) * 8 > vi->getPrecision())
                return;
            switch (vi->getPrecision()) {
                case 8: {
                    std::vector<int8_t> fv(value.begin(), value.end());
                    vi->set(fv);
                    break;
                }
                case 16: {
                    std::vector<int16_t> fv(value.begin(), value.end());
                    vi->set(fv);
                    break;
                }
                case 32: {
                    std::vector<int32_t> fv(value.begin(), value.end());
                    vi->set(fv);
                    break;
                }
                case 64: {
                    std::vector<int64_t> fv(value.begin(), value.end());
                    vi->set(fv);
                    break;
                }
            }
        } else if (vi->type == T_UINT) {
            if (sizeof(T) * 8 > vi->getPrecision()) {
                return;
            } else if (sizeof(T) * 8 == vi->getPrecision()) {
                vi->set(value);
            } else {
                switch (vi->getPrecision()) {
                    case 8: {
                        std::vector<uint8_t> fv(value.begin(), value.end());
                        vi->set(fv);
                        break;
                    }
                    case 16: {
                        std::vector<uint16_t> fv(value.begin(), value.end());
                        vi->set(fv);
                        break;
                    }
                    case 32: {
                        std::vector<uint32_t> fv(value.begin(), value.end());
                        vi->set(fv);
                        break;
                    }
                    case 64: {
                        std::vector<uint64_t> fv(value.begin(), value.end());
                        vi->set(fv);
                        break;
                    }
                }
            }
        } else if (vi->type == T_FLOAT) {
            if (sizeof(T) > vi->getPrecision())
                return;
            switch (vi->getPrecision()) {
                case 4:
                    vi->set(float(value));
                    break;
                case 8:
                    vi->set(double(value));
                    break;
                case 16:
                    vi->set(ldouble(value));
                    break;
            }
        } else {
            return;
        }

    }
    template<typename T>
    auto _updateArrayItem(ValueItem* vi, std::vector<T>& value) -> EnableForString<T>{
        if (vi->type != T_STRING)
            return;
        vi->set(value);
    }

    template<typename T>
    auto _updateArrayItem(ValueItem* vi, std::vector<T>& value) -> EnableForComplex<T>{
        if (vi->type != T_COMPLEX)
            return;
        if (sizeof(T) > vi->getPrecision() * 2) {
            return;
        } else if (sizeof(T) == vi->getPrecision() * 2) {
            vi->set(value);
        } else {
            switch (vi->getPrecision()) {
                case 4: {
                    std::vector<complex_float_t> fv;
                    fv.reserve(value.size());
                    for (auto v : value)
                        fv.push_back(complex_f(v));
                    vi->set(fv);
                    break;
                }
                case 8:{
                    std::vector<complex_double_t> fv;
                    fv.reserve(value.size());
                    for (auto v : value)
                        fv.push_back(complex_d(v));
                    vi->set(fv);
                    break;
                }
                case 16:{
                    std::vector<complex_long_double_t> fv;
                    fv.reserve(value.size());
                    for (auto v : value)
                        fv.push_back(complex_ld(v));
                    vi->set(fv);
                    break;
                }
            }
        }
    }

    template<typename T>
    void _updateArrayItem(ValueItem* vi, T* value, const size_t dim=0) {
        const size_t count = std::max(dim, vi->size());
        std::vector<T> fv;
        fv.reserve(count);
        for (auto i = 0; i < count; i++)
            fv[i] = value[i];
        _updateArrayItem(vi, fv);
    }
};


//template<>
//inline void FormattedData::updateItem(const size_t itemid, const & value) {

//}



} // communication
} // datatypes
