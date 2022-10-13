#include "ValueArray2D.hpp"
#include "ValueGroup.hpp"

namespace communicator {
namespace datatypes {
} // communicator
} // datatypes


value_2Darray_t* Int_2Darray(int* value, const size_t &N, const size_t &M, const char* unit, const size_t &unit_size) {
    auto va_t = (value_2Darray_t*)malloc(sizeof(value_2Darray_t));
    auto vv = (int**)malloc(sizeof(int*) * N);
    for (int i = 0; i < N; i++) {
        auto v = (int *) malloc(sizeof(int) * M);
        memcpy(v, value, sizeof(int) * N);
        vv[i] = v;
    }
    va_t->obj = vv;
    va_t->type = T_INT;
    va_t->N = N;
    va_t->M = M;
    communication::datatypes::set_unit(va_t->unit, unit, unit_size);
    return va_t;
}

value_2Darray_t* Float_2Darray(int* value, const size_t &N, const size_t &M, const char* unit, const size_t &unit_size) {
    auto va_t = (value_2Darray_t*)malloc(sizeof(value_2Darray_t));
    auto vv = (float**)malloc(sizeof(float*) * N);
    for (int i = 0; i < N; i++) {
        auto v = (float *) malloc(sizeof(float) * M);
        memcpy(v, value, sizeof(float) * N);
        vv[i] = v;
    }
    va_t->obj = vv;
    va_t->type = T_FLOAT;
    va_t->N = N;
    va_t->M = M;
    communication::datatypes::set_unit(va_t->unit, unit, unit_size);
    return va_t;
}

value_2Darray_t* UInt_2Darray(int* value, const size_t &N, const size_t &M, const char* unit, const size_t &unit_size) {
    auto va_t = (value_2Darray_t*)malloc(sizeof(value_2Darray_t));
    auto vv = (unsigned int**)malloc(sizeof(uint*) * N);
    for (int i = 0; i < N; i++) {
        auto v = (unsigned int *) malloc(sizeof(uint) * M);
        memcpy(v, value, sizeof(uint) * N);
        vv[i] = v;
    }
    va_t->obj = vv;
    va_t->type = T_UINT;
    va_t->N = N;
    va_t->M = M;
    communication::datatypes::set_unit(va_t->unit, unit, unit_size);
    return va_t;
}

value_2Darray_t* ComplexFloat_2Darray(int* value, const size_t &N, const size_t &M, const char* unit, const size_t &unit_size) {
    auto va_t = (value_2Darray_t*)malloc(sizeof(value_2Darray_t));
    auto vv = (complex_float_t**)malloc(sizeof(complex_float_t*) * N);
    for (int i = 0; i < N; i++) {
        auto v = (complex_float_t *) malloc(sizeof(complex_float_t) * M);
        memcpy(v, value, sizeof(complex_float_t) * N);
        vv[i] = v;
    }
    va_t->obj = vv;
    va_t->type = T_INT;
    va_t->N = N;
    va_t->M = M;
    communication::datatypes::set_unit(va_t->unit, unit, unit_size);
    return va_t;
}

void add_2DAtoGroup(value_2Darray_t* val, value_group_t* grp) {
    communication::datatypes::ValueItem* v;
    switch (val->type) {
        case T_INT:
            v = new communication::datatypes::ValueArray2D<int>(val);
            break;
        case T_FLOAT:
            v = new communication::datatypes::ValueArray2D<float>(val);
            break;
        case T_UINT:
            v = new communication::datatypes::ValueArray2D<uint>(val);
            break;
        case T_COMPLEX:
            v = new communication::datatypes::ValueArray2D<complex_float_t>(val);
            break;
    }
    static_cast<communication::datatypes::ValueGroup*>(grp->obj)->addItem(v);
}