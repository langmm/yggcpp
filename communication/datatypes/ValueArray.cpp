#include "ValueArray.hpp"
#include "ValueGroup.hpp"
#include "dtype_t.hpp"

namespace communication {
namespace datatypes {
} // communication
} // datatypes


int add_AtoGroup(dtype_t* val, dtype_t* grp) {
    if (val->type != T_1DARRAY) {
        communication::utils::ygglog_error("Incorrect datatype given");
        return -1;
    }
    auto v = static_cast<communication::datatypes::ValueItem*>(val->obj);
    switch (v->type) {
        case T_INT:
            switch (v->getPrecision()) {
                case 8:
                    v = static_cast<communication::datatypes::ValueArray<int8_t> *>(val->obj);
                    break;
                case 16:
                    v = static_cast<communication::datatypes::ValueArray<int16_t> *>(val->obj);
                    break;
                case 32:
                    v = static_cast<communication::datatypes::ValueArray<int32_t> *>(val->obj);
                    break;
                case 64:
                    v = static_cast<communication::datatypes::ValueArray<int64_t> *>(val->obj);
                    break;
            }
            break;
        case T_BOOLEAN:
            v = static_cast<communication::datatypes::ValueArray<bool> *>(val->obj);
            break;
        case T_FLOAT:
            switch (v->getPrecision()) {
                case 4:
                    v = static_cast<communication::datatypes::ValueArray<float> *>(val->obj);
                    break;
                case 8:
                    v = static_cast<communication::datatypes::ValueArray<double> *>(val->obj);
                    break;
                case 10:
                case 12:
                    v = static_cast<communication::datatypes::ValueArray<long double> *>(val->obj);
                    break;
            }
            break;
        case T_UINT:
            switch (v->getPrecision()) {
                case 8:
                    v = static_cast<communication::datatypes::ValueArray<uint8_t> *>(val->obj);
                    break;
                case 16:
                    v = static_cast<communication::datatypes::ValueArray<uint16_t> *>(val->obj);
                    break;
                case 32:
                    v = static_cast<communication::datatypes::ValueArray<uint32_t> *>(val->obj);
                    break;
                case 64:
                    v = static_cast<communication::datatypes::ValueArray<uint64_t> *>(val->obj);
                    break;
            }
            break;
        case T_STRING:
            v = static_cast<communication::datatypes::ValueArray<std::string> *>(val->obj);
            break;
        case T_COMPLEX:
            switch (v->getPrecision()) {
                case 4:
                    v = static_cast<communication::datatypes::ValueArray<complex_float_t> *>(val->obj);
                    break;
                case 8:
                    v = static_cast<communication::datatypes::ValueArray<complex_double_t> *>(val->obj);
                    break;
                case 10:
                case 12:
                    v = static_cast<communication::datatypes::ValueArray<complex_long_double_t> *>(val->obj);
                    break;
            }
            break;
    }
    static_cast<communication::datatypes::ValueGroup*>(grp->obj)->addItem(v);
    return 0;
}

dtype_t* create_dtype_1darray(SUBTYPE type, const size_t& precision, const char* units) {
    auto dt = (dtype_t*)malloc(sizeof(dtype_t));
    dt->type = T_1DARRAY;
    dt->use_generic = false;
    communication::datatypes::ValueItem* vi;
    switch (type) {
        case T_INT: {
            switch (precision) {
                case 8:
                    vi = new communication::datatypes::ValueArray<int8_t>(T_INT, units, 8);
                    break;
                case 16:
                    vi = new communication::datatypes::ValueArray<int16_t>(T_INT, units, 16);
                    break;
                case 32:
                    vi = new communication::datatypes::ValueArray<int32_t>(T_INT, units, 32);
                    break;
                case 64:
                    vi = new communication::datatypes::ValueArray<int64_t>(T_INT, units, 64);
                    break;
                default:
                    communication::utils::ygglog_error("Invalid precision given.");
                    return nullptr;
            }
            break;
        }
        case T_BOOLEAN: {
            vi = new communication::datatypes::ValueArray<bool>(T_BOOLEAN, units, 1);
            break;
        }
        case T_FLOAT: {
            switch (precision) {
                case 4:
                    vi = new communication::datatypes::ValueArray<float>(T_FLOAT, units, 4);
                    break;
                case 8:
                    vi = new communication::datatypes::ValueArray<double>(T_FLOAT, units, 8);
                    break;
                case 10:
                case 12:
                    vi = new communication::datatypes::ValueArray<long double>(T_FLOAT, units, 12);
                    break;
                default:
                    communication::utils::ygglog_error("Invalid precision given.");
                    return nullptr;
            }
            break;
        }
        case T_UINT: {
            switch (precision) {
                case 8:
                    vi = new communication::datatypes::ValueArray<uint8_t>(T_UINT, units, 8);
                    break;
                case 16:
                    vi = new communication::datatypes::ValueArray<uint16_t>(T_UINT, units, 16);
                    break;
                case 32:
                    vi = new communication::datatypes::ValueArray<uint32_t>(T_UINT, units, 32);
                    break;
                case 64:
                    vi = new communication::datatypes::ValueArray<uint64_t>(T_UINT, units, 64);
                    break;
                default:
                    communication::utils::ygglog_error("Invalid precision given.");
                    return nullptr;
            }
            break;
        }
        case T_STRING: {
            vi = new communication::datatypes::ValueArray<std::string>(T_STRING, units, 0);
            break;
        }
        case T_COMPLEX: {
            switch (precision) {
                case 4:
                    vi = new communication::datatypes::ValueArray<complex_float_t>(T_COMPLEX, units, 4);
                    break;
                case 8:
                    vi = new communication::datatypes::ValueArray<complex_double_t>(T_COMPLEX, units, 4);
                    break;
                case 10:
                case 12:
                    vi = new communication::datatypes::ValueArray<complex_long_double_t>(T_COMPLEX, units, 12);
                    break;
                default:
                    communication::utils::ygglog_error("Invalid precision given.");
                    return nullptr;

            }
            break;
        }
    }
    dt->obj = vi;
    return dt;
}

dtype_t* create_dtype_1dshort(int8_t *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_INT, 8, units);
    static_cast<communication::datatypes::ValueArray<int8_t>*>(dt->obj)->set(val, N);
    return dt;
}
dtype_t* create_dtype_1dint(int16_t *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_INT, 16, units);
    static_cast<communication::datatypes::ValueArray<int16_t>*>(dt->obj)->set(val, N);
    return dt;
}
dtype_t* create_dtype_1dlong(int32_t *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_INT, 32, units);
    static_cast<communication::datatypes::ValueArray<int32_t>*>(dt->obj)->set(val, N);
    return dt;
}
dtype_t* create_dtype_1dllong(int64_t *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_INT, 64, units);
    static_cast<communication::datatypes::ValueArray<int64_t>*>(dt->obj)->set(val, N);
    return dt;
}
dtype_t* create_dtype_1dbool(bool* val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_BOOLEAN, 1, units);
    static_cast<communication::datatypes::ValueArray<bool>*>(dt->obj)->set(val, N);
    return dt;
}
dtype_t* create_dtype_1dfloat(float *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_FLOAT, 4, units);
    static_cast<communication::datatypes::ValueArray<float>*>(dt->obj)->set(val, N);
    return dt;
}
dtype_t* create_dtype_1ddouble(double *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_FLOAT, 8, units);
    static_cast<communication::datatypes::ValueArray<double>*>(dt->obj)->set(val, N);
    return dt;
}
dtype_t* create_dtype_1dldouble(long double *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_FLOAT, 12, units);
    static_cast<communication::datatypes::ValueArray<long double>*>(dt->obj)->set(val, N);
    return dt;
}
dtype_t* create_dtype_1dushort(uint8_t *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_UINT, 8, units);
    static_cast<communication::datatypes::ValueArray<uint8_t>*>(dt->obj)->set(val, N);
    return dt;
}
dtype_t* create_dtype_1duint(uint16_t *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_UINT, 16, units);
    static_cast<communication::datatypes::ValueArray<uint16_t>*>(dt->obj)->set(val, N);
    return dt;
}
dtype_t* create_dtype_1dulong(uint32_t *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_UINT, 32, units);
    static_cast<communication::datatypes::ValueArray<uint32_t>*>(dt->obj)->set(val, N);
    return dt;
}
dtype_t* create_dtype_1dullong(uint64_t *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_UINT, 64, units);
    static_cast<communication::datatypes::ValueArray<uint64_t>*>(dt->obj)->set(val, N);
    return dt;
}
dtype_t* create_dtype_1dcomplex_float(complex_float_t *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_COMPLEX, 4, units);
    static_cast<communication::datatypes::ValueArray<complex_float_t>*>(dt->obj)->set(val, N);
    return dt;
}
dtype_t* create_dtype_1dcomplex_double(complex_double_t *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_COMPLEX, 8, units);
    static_cast<communication::datatypes::ValueArray<complex_double_t>*>(dt->obj)->set(val, N);
    return dt;
}
dtype_t* create_dtype_1dcomplex_ldouble(complex_long_double_t *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_COMPLEX, 12, units);
    static_cast<communication::datatypes::ValueArray<complex_long_double_t>*>(dt->obj)->set(val, N);
    return dt;
}
dtype_t* create_dtype_1dstring(char** val, const size_t& N) {
    auto dt = create_dtype_1darray(T_STRING, 0);
    std::vector<std::string> temp(N);
    for (auto i = 0; i < N; i++) {
        temp[i] = val[i];
    }
    static_cast<communication::datatypes::ValueArray<std::string>*>(dt->obj)->set(temp);
    return dt;
}

void set_dtype_1dshort(dtype_t* dt, int8_t *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray<int8_t>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1dint(dtype_t* dt, int16_t *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray<int16_t>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1dlong(dtype_t* dt, int32_t *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray<int32_t>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1dllong(dtype_t* dt, int64_t *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray<int64_t>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1dbool(dtype_t* dt, bool* val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray<bool>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1dfloat(dtype_t* dt, float *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray<float>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1ddouble(dtype_t* dt, double *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray<double>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1dldouble(dtype_t* dt, long double *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray<long double>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1dushort(dtype_t* dt, uint8_t *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray<uint8_t>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1duint(dtype_t* dt, uint16_t *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray<uint16_t>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1dulong(dtype_t* dt, uint32_t *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray<uint32_t>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1dullong(dtype_t* dt, uint64_t *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray<uint64_t>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1dcomplex_float(dtype_t* dt, complex_float_t *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray<complex_float_t>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1dcomplex_double(dtype_t* dt, complex_double_t *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray<complex_double_t>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1dcomplex_ldouble(dtype_t* dt, complex_long_double_t *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray<complex_long_double_t>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1dstring(dtype_t* dt, char** val, const size_t& N) {
    std::vector<std::string> temp(N);
    for (auto i = 0; i < N; i++) {
        temp[i] = val[i];
    }

    static_cast<communication::datatypes::ValueArray<std::string>*>(dt->obj)->set(temp);
}
