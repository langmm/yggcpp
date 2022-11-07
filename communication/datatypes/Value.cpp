#include "Value.hpp"
#include "ValueGroup.hpp"
#include "dtype_t.hpp"
#include "utils/logging.hpp"
namespace communication {
namespace datatypes {
void set_unit(char* u, const char* unit, const size_t &unit_size) {
    if (unit_size != 0) {
        u = (char*)malloc(sizeof(char) * unit_size);
        memcpy(u, unit, unit_size);
    } else {
        u = nullptr;
    }
}

} // communication
} // datatype


int add_VtoGroup(dtype_t* val, dtype_t* grp) {
    if (val->type != T_SCALAR) {
        ygglog_error << "Incorrect datatype given";
        return -1;
    }
    auto v = static_cast<communication::datatypes::ValueItem*>(val->obj);
    switch (v->type) {
        case T_INT:
            switch (v->getPrecision()) {
                case 8:
                    v = static_cast<communication::datatypes::Value<int8_t> *>(val->obj);
                    break;
                case 16:
                    v = static_cast<communication::datatypes::Value<int16_t> *>(val->obj);
                    break;
                case 32:
                    v = static_cast<communication::datatypes::Value<int32_t> *>(val->obj);
                    break;
                case 64:
                    v = static_cast<communication::datatypes::Value<int64_t> *>(val->obj);
                    break;
            }
            break;
        case T_BOOLEAN:
            v = static_cast<communication::datatypes::Value<bool> *>(val->obj);
            break;
        case T_FLOAT:
            switch (v->getPrecision()) {
                case 4:
                    v = static_cast<communication::datatypes::Value<float> *>(val->obj);
                    break;
                case 8:
                    v = static_cast<communication::datatypes::Value<double> *>(val->obj);
                    break;
                case 10:
                case 12:
                    v = static_cast<communication::datatypes::Value<long double> *>(val->obj);
                    break;
            }
            break;
        case T_UINT:
            switch (v->getPrecision()) {
                case 8:
                    v = static_cast<communication::datatypes::Value<uint8_t> *>(val->obj);
                    break;
                case 16:
                    v = static_cast<communication::datatypes::Value<uint16_t> *>(val->obj);
                    break;
                case 32:
                    v = static_cast<communication::datatypes::Value<uint32_t> *>(val->obj);
                    break;
                case 64:
                    v = static_cast<communication::datatypes::Value<uint64_t> *>(val->obj);
                    break;
            }
            break;
        case T_STRING:
            v = static_cast<communication::datatypes::Value<std::string> *>(val->obj);
            break;
        case T_COMPLEX:
            switch (v->getPrecision()) {
                case 4:
                    v = static_cast<communication::datatypes::Value<complex_float_t> *>(val->obj);
                    break;
                case 8:
                    v = static_cast<communication::datatypes::Value<complex_double_t> *>(val->obj);
                    break;
                case 10:
                case 12:
                    v = static_cast<communication::datatypes::Value<complex_long_double_t> *>(val->obj);
                    break;
            }
            break;
    }
    static_cast<communication::datatypes::ValueGroup*>(grp->obj)->addItem(v);
    return 0;
}

dtype_t* create_dtype_scalar(SUBTYPE type, uint8_t precision, const char* units) {
    auto dt = (dtype_t*)malloc(sizeof(dtype_t));
    dt->type = T_SCALAR;
    dt->use_generic = false;
    communication::datatypes::ValueItem* vi = communication::datatypes::createValue(type, precision, units);
    dt->obj = vi;
    return dt;
}

dtype_t* create_dtype_short(int8_t &val, const char* units) {
    auto dt = create_dtype_scalar(T_INT, 8, units);
    static_cast<communication::datatypes::Value<int8_t>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_int(int16_t &val, const char* units) {
    auto dt = create_dtype_scalar(T_INT, 16, units);
    static_cast<communication::datatypes::Value<int16_t>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_long(int32_t &val, const char* units) {
    auto dt = create_dtype_scalar(T_INT, 32, units);
    static_cast<communication::datatypes::Value<int32_t>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_llong(int64_t &val, const char* units) {
    auto dt = create_dtype_scalar(T_INT, 64, units);
    static_cast<communication::datatypes::Value<int64_t>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_bool(bool val, const char* units) {
    auto dt = create_dtype_scalar(T_BOOLEAN, 1, units);
    static_cast<communication::datatypes::Value<bool>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_float(float &val, const char* units) {
    auto dt = create_dtype_scalar(T_FLOAT, 4, units);
    static_cast<communication::datatypes::Value<float>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_double(double &val, const char* units) {
    auto dt = create_dtype_scalar(T_FLOAT, 8, units);
    static_cast<communication::datatypes::Value<double>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_ldouble(long double &val, const char* units) {
    auto dt = create_dtype_scalar(T_FLOAT, 12, units);
    static_cast<communication::datatypes::Value<long double>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_ushort(uint8_t &val, const char* units) {
    auto dt = create_dtype_scalar(T_UINT, 8, units);
    static_cast<communication::datatypes::Value<uint8_t>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_uint(uint16_t &val, const char* units) {
    auto dt = create_dtype_scalar(T_UINT, 16, units);
    static_cast<communication::datatypes::Value<uint16_t>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_ulong(uint32_t &val, const char* units) {
    auto dt = create_dtype_scalar(T_UINT, 32, units);
    static_cast<communication::datatypes::Value<uint32_t>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_ullong(uint64_t &val, const char* units) {
    auto dt = create_dtype_scalar(T_UINT, 64, units);
    static_cast<communication::datatypes::Value<uint64_t>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_complex_float(complex_float_t &val, const char* units) {
    auto dt = create_dtype_scalar(T_COMPLEX, 4, units);
    static_cast<communication::datatypes::Value<complex_float_t>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_complex_double(complex_double_t &val, const char* units) {
    auto dt = create_dtype_scalar(T_COMPLEX, 8, units);
    static_cast<communication::datatypes::Value<complex_double_t>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_complex_ldouble(complex_long_double_t &val, const char* units) {
    auto dt = create_dtype_scalar(T_COMPLEX, 12, units);
    static_cast<communication::datatypes::Value<complex_long_double_t>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_string(char* val) {
    auto dt = create_dtype_scalar(T_STRING, 0);
    std::string temp(val);
    static_cast<communication::datatypes::Value<std::string>*>(dt->obj)->set(temp);
    return dt;
}


void set_dtype_short(dtype_t* dt, int8_t &val, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::Value<int8_t>*>(dt->obj)->set(val, temp);
}
void set_dtype_int(dtype_t* dt, int16_t &val, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::Value<int16_t>*>(dt->obj)->set(val, temp);
}
void set_dtype_long(dtype_t* dt, int32_t &val, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::Value<int32_t>*>(dt->obj)->set(val, temp);
}
void set_dtype_llong(dtype_t* dt, int64_t &val, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::Value<int64_t>*>(dt->obj)->set(val, temp);
}
void set_dtype_bool(bool val, char* units=nullptr);
void set_dtype_float(dtype_t* dt, float &val, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::Value<float>*>(dt->obj)->set(val, temp);
}
void set_dtype_double(dtype_t* dt, double &val, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::Value<double>*>(dt->obj)->set(val, temp);
}
void set_dtype_ldouble(dtype_t* dt, long double &val, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::Value<long double>*>(dt->obj)->set(val, temp);
}
void set_dtype_ushort(dtype_t* dt, uint8_t &val, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::Value<uint8_t>*>(dt->obj)->set(val, temp);
}
void set_dtype_uint(dtype_t* dt, uint16_t &val, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::Value<uint16_t>*>(dt->obj)->set(val, temp);
}
void set_dtype_ulong(dtype_t* dt, uint32_t &val, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::Value<uint32_t>*>(dt->obj)->set(val, temp);
}
void set_dtype_ullong(dtype_t* dt, uint64_t &val, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::Value<uint64_t>*>(dt->obj)->set(val, temp);
}
void set_dtype_complex_float(dtype_t* dt, complex_float_t &val, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::Value<complex_float_t>*>(dt->obj)->set(val, temp);
}
void set_dtype_complex_double(dtype_t* dt, complex_double_t &val, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::Value<complex_double_t>*>(dt->obj)->set(val, temp);
}
void set_dtype_complex_ldouble(dtype_t* dt, complex_long_double_t &val, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::Value<complex_long_double_t>*>(dt->obj)->set(val, temp);
}
void set_dtype_string(dtype_t* dt, char* val) {
    std::string temp(val);
    static_cast<communication::datatypes::Value<std::string>*>(dt->obj)->set(temp);
}

communication::datatypes::ValueItem* communication::datatypes::createValue(SUBTYPE t, const uint8_t &precision, const char* units) {
    communication::datatypes::ValueItem* vi;
    switch (t) {
        case T_INT: {
            switch (precision) {
                case 8:
                    vi = new communication::datatypes::Value<int8_t>(T_INT, units, 8);
                    break;
                case 16:
                    vi = new communication::datatypes::Value<int16_t>(T_INT, units, 16);
                    break;
                case 32:
                    vi = new communication::datatypes::Value<int32_t>(T_INT, units, 32);
                    break;
                case 64:
                    vi = new communication::datatypes::Value<int64_t>(T_INT, units, 64);
                    break;
                default:
                    ygglog_error << "Invalid precision given.";
                    return nullptr;
            }
            break;
        }
        case T_BOOLEAN: {
            vi = new communication::datatypes::Value<bool>(T_BOOLEAN, units, 1);
            break;
        }
        case T_FLOAT: {
            switch (precision) {
                case 4:
                    vi = new communication::datatypes::Value<float>(T_FLOAT, units, 4);
                    break;
                case 8:
                    vi = new communication::datatypes::Value<double>(T_FLOAT, units, 8);
                    break;
                case 10:
                case 12:
                    vi = new communication::datatypes::Value<long double>(T_FLOAT, units, 12);
                    break;
                default:
                    ygglog_error << "Invalid precision given.";
                    return nullptr;
            }
            break;
        }
        case T_UINT: {
            switch (precision) {
                case 8:
                    vi = new communication::datatypes::Value<uint8_t>(T_UINT, units, 8);
                    break;
                case 16:
                    vi = new communication::datatypes::Value<uint16_t>(T_UINT, units, 16);
                    break;
                case 32:
                    vi = new communication::datatypes::Value<uint32_t>(T_UINT, units, 32);
                    break;
                case 64:
                    vi = new communication::datatypes::Value<uint64_t>(T_UINT, units, 64);
                    break;
                default:
                    ygglog_error << "Invalid precision given.";
                    return nullptr;
            }
            break;
        }
        case T_STRING: {
            vi = new communication::datatypes::Value<std::string>(T_STRING, units, 0);
            break;
        }
        case T_COMPLEX: {
            switch (precision) {
                case 4:
                    vi = new communication::datatypes::Value<complex_float_t>(T_COMPLEX, units, 4);
                    break;
                case 8:
                    vi = new communication::datatypes::Value<complex_double_t>(T_COMPLEX, units, 8);
                    break;
                case 10:
                case 12:
                    vi = new communication::datatypes::Value<complex_long_double_t>(T_COMPLEX, units, 12);
                    break;
                default:
                    ygglog_error << "Invalid precision given.";
                    return nullptr;

            }
            break;
        }
    }
    return vi;


    //auto temp = create_dtype_scalar(t, precision, units);
    //auto res = static_cast<communication::datatypes::ValueItem*>(temp->obj);
    //free(temp);
    //return res;
}

