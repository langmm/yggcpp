#include "ValueArray1D.hpp"
#include "ValueGroup.hpp"
#include "dtype_t.hpp"
#include "utils/logging.hpp"

namespace communication {
namespace datatypes {
} // communication
} // datatypes


int add_AtoGroup(dtype_t* val, dtype_t* grp) {
    if (val->type != T_NDARRAY || grp->type != T_GROUP) {
        ygglog_error << "Incorrect datatype given";
        return -1;
    }
    auto v = static_cast<communication::datatypes::ValueItem*>(val->obj);
    switch (v->type) {
        case T_INT:
            switch (v->getPrecision()) {
                case 8:
                    v = static_cast<communication::datatypes::ValueArray1D<int8_t> *>(val->obj);
                    break;
                case 16:
                    v = static_cast<communication::datatypes::ValueArray1D<int16_t> *>(val->obj);
                    break;
                case 32:
                    v = static_cast<communication::datatypes::ValueArray1D<int32_t> *>(val->obj);
                    break;
                case 64:
                    v = static_cast<communication::datatypes::ValueArray1D<int64_t> *>(val->obj);
                    break;
            }
            break;
        case T_BOOLEAN:
            v = static_cast<communication::datatypes::ValueArray1D<bool> *>(val->obj);
            break;
        case T_FLOAT:
            switch (v->getPrecision()) {
                case 4:
                    v = static_cast<communication::datatypes::ValueArray1D<float> *>(val->obj);
                    break;
                case 8:
                    v = static_cast<communication::datatypes::ValueArray1D<double> *>(val->obj);
                    break;
                case 10:
                case 12:
                    v = static_cast<communication::datatypes::ValueArray1D<long double> *>(val->obj);
                    break;
            }
            break;
        case T_UINT:
            switch (v->getPrecision()) {
                case 8:
                    v = static_cast<communication::datatypes::ValueArray1D<uint8_t> *>(val->obj);
                    break;
                case 16:
                    v = static_cast<communication::datatypes::ValueArray1D<uint16_t> *>(val->obj);
                    break;
                case 32:
                    v = static_cast<communication::datatypes::ValueArray1D<uint32_t> *>(val->obj);
                    break;
                case 64:
                    v = static_cast<communication::datatypes::ValueArray1D<uint64_t> *>(val->obj);
                    break;
            }
            break;
        case T_STRING:
            v = static_cast<communication::datatypes::ValueArray1D<std::string> *>(val->obj);
            break;
        case T_COMPLEX:
            switch (v->getPrecision()) {
                case 4:
                    v = static_cast<communication::datatypes::ValueArray1D<complex_float_t> *>(val->obj);
                    break;
                case 8:
                    v = static_cast<communication::datatypes::ValueArray1D<complex_double_t> *>(val->obj);
                    break;
                case 10:
                case 12:
                    v = static_cast<communication::datatypes::ValueArray1D<complex_long_double_t> *>(val->obj);
                    break;
            }
            break;
    }
    static_cast<communication::datatypes::ValueGroup*>(grp->obj)->addItem(v);
    return 0;
}

/*dtype_t* create_dtype_1darray(SUBTYPE type, const size_t& size, const size_t& precision, const char* units) {
    const size_t dims[1] = {size};
    const size_t ndim = 1;
    return create_dtype_empty_array(type, ndim, dims, precision, units);
}*/

dtype_t* create_dtype_1darray(SUBTYPE type, const size_t& dim, const uint8_t& precision,
                                  const char* units) {
    auto dt = (dtype_t*)malloc(sizeof(dtype_t));
    dt->type = T_NDARRAY;
    dt->use_generic = false;

    dt->obj = communication::datatypes::createArray(type, dim, precision, units);
    return dt;
}

dtype_t* create_dtype_1dshort(int8_t *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_INT, N, 8, units);
    static_cast<communication::datatypes::ValueArray1D<int8_t>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_1dint(int16_t *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_INT, N, 16, units);
    static_cast<communication::datatypes::ValueArray1D<int16_t>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_1dlong(int32_t *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_INT, N, 32, units);
    static_cast<communication::datatypes::ValueArray1D<int32_t>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_1dllong(int64_t *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_INT, N, 64, units);
    static_cast<communication::datatypes::ValueArray1D<int64_t>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_1dbool(bool* val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_BOOLEAN, N, 1, units);
    static_cast<communication::datatypes::ValueArray1D<bool>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_1dfloat(float *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_FLOAT, N, 4, units);
    static_cast<communication::datatypes::ValueArray1D<float>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_1ddouble(double *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_FLOAT, N, 8, units);
    static_cast<communication::datatypes::ValueArray1D<double>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_1dldouble(long double *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_FLOAT, N, 12, units);
    static_cast<communication::datatypes::ValueArray1D<long double>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_1dushort(uint8_t *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_UINT, N, 8, units);
    static_cast<communication::datatypes::ValueArray1D<uint8_t>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_1duint(uint16_t *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_UINT, N, 16, units);
    static_cast<communication::datatypes::ValueArray1D<uint16_t>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_1dulong(uint32_t *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_UINT, N, 32, units);
    static_cast<communication::datatypes::ValueArray1D<uint32_t>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_1dullong(uint64_t *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_UINT, N, 64, units);
    static_cast<communication::datatypes::ValueArray1D<uint64_t>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_1dcomplex_float(complex_float_t *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_COMPLEX, N, 4, units);
    static_cast<communication::datatypes::ValueArray1D<complex_float_t>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_1dcomplex_double(complex_double_t *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_COMPLEX, N, 8, units);
    static_cast<communication::datatypes::ValueArray1D<complex_double_t>*>(dt->obj)->set(val);
    return dt;
}
dtype_t* create_dtype_1dcomplex_ldouble(complex_long_double_t *val, const size_t& N, const char* units) {
    auto dt = create_dtype_1darray(T_COMPLEX, N, 12, units);
    static_cast<communication::datatypes::ValueArray1D<complex_long_double_t>*>(dt->obj)->set(val);
    return dt;
}
//dtype_t* create_dtype_1dstring(char** val, const size_t& N, const size_t dims[]) {
//    auto dt = create_dtype_empty_array(T_STRING, N, dims);
//    static_cast<communication::datatypes::ValueArray1D<char*>*>(dt->obj)->set(val);
//    return dt;
//}

void set_dtype_1dshort(dtype_t* dt, int8_t *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray1D<int8_t>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1dint(dtype_t* dt, int16_t *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray1D<int16_t>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1dlong(dtype_t* dt, int32_t *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray1D<int32_t>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1dllong(dtype_t* dt, int64_t *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray1D<int64_t>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1dbool(dtype_t* dt, bool* val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray1D<bool>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1dfloat(dtype_t* dt, float *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray1D<float>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1ddouble(dtype_t* dt, double *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray1D<double>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1dldouble(dtype_t* dt, long double *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray1D<long double>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1dushort(dtype_t* dt, uint8_t *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray1D<uint8_t>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1duint(dtype_t* dt, uint16_t *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray1D<uint16_t>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1dulong(dtype_t* dt, uint32_t *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray1D<uint32_t>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1dullong(dtype_t* dt, uint64_t *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray1D<uint64_t>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1dcomplex_float(dtype_t* dt, complex_float_t *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray1D<complex_float_t>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1dcomplex_double(dtype_t* dt, complex_double_t *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray1D<complex_double_t>*>(dt->obj)->set(val, N, temp);
}
void set_dtype_1dcomplex_ldouble(dtype_t* dt, complex_long_double_t *val, const size_t& N, const char* units) {
    std::string temp(units);
    static_cast<communication::datatypes::ValueArray1D<complex_long_double_t>*>(dt->obj)->set(val, N, temp);
}
//void set_dtype_1dstring(dtype_t* dt, char** val, const size_t dims[]) {
//    static_cast<communication::datatypes::ValueArray1D<char*>*>(dt->obj)->set(val, dims);
//}


communication::datatypes::ValueItem* createArray(SUBTYPE t, const uint8_t &precision, const size_t &dim, const char* units) {
    communication::datatypes::ValueItem* vi;
    switch (t) {
        case T_INT: {
            switch (precision) {
                case 8:
                    vi = new communication::datatypes::ValueArray1D<int8_t>(T_INT, dim, units, 8);
                    break;
                case 16:
                    vi = new communication::datatypes::ValueArray1D<int16_t>(T_INT, dim, units, 16);
                    break;
                case 32:
                    vi = new communication::datatypes::ValueArray1D<int32_t>(T_INT, dim, units, 32);
                    break;
                case 64:
                    vi = new communication::datatypes::ValueArray1D<int64_t>(T_INT, dim, units, 64);
                    break;
                default:
                    ygglog_error << "Invalid precision given.";
                    return nullptr;
            }
            break;
        }
        case T_BOOLEAN: {
            vi = new communication::datatypes::ValueArray1D<bool>(T_BOOLEAN, dim, units, 1);
            break;
        }
        case T_FLOAT: {
            switch (precision) {
                case 4:
                    vi = new communication::datatypes::ValueArray1D<float>(T_FLOAT, dim, units, 4);
                    break;
                case 8:
                    vi = new communication::datatypes::ValueArray1D<double>(T_FLOAT, dim, units, 8);
                    break;
                case 10:
                case 12:
                    vi = new communication::datatypes::ValueArray1D<long double>(T_FLOAT, dim, units, 12);
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
                    vi = new communication::datatypes::ValueArray1D<uint8_t>(T_UINT, dim, units, 8);
                    break;
                case 16:
                    vi = new communication::datatypes::ValueArray1D<uint16_t>(T_UINT, dim, units, 16);
                    break;
                case 32:
                    vi = new communication::datatypes::ValueArray1D<uint32_t>(T_UINT, dim, units, 32);
                    break;
                case 64:
                    vi = new communication::datatypes::ValueArray1D<uint64_t>(T_UINT, dim, units, 64);
                    break;
                default:
                    ygglog_error << "Invalid precision given.";
                    return nullptr;
            }
            break;
        }
        case T_STRING: {
            vi = new communication::datatypes::ValueArray1D<std::string>(T_STRING, dim, units, 0);
            break;
        }
        case T_COMPLEX: {
            switch (precision) {
                case 4:
                    vi = new communication::datatypes::ValueArray1D<complex_float_t>(T_COMPLEX, dim, units, 4);
                    break;
                case 8:
                    vi = new communication::datatypes::ValueArray1D<complex_double_t>(T_COMPLEX, dim, units, 4);
                    break;
                case 10:
                case 12:
                    vi = new communication::datatypes::ValueArray1D<complex_long_double_t>(T_COMPLEX, dim, units, 12);
                    break;
                default:
                    ygglog_error << "Invalid precision given.";
                    return nullptr;

            }
            break;
        }
    }
    return vi;
}
