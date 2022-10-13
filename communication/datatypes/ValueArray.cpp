#include "ValueArray.hpp"
#include "ValueGroup.hpp"

namespace communication {
namespace datatypes {
} // communication
} // datatypes

value_array_t* Int_array(int* value, const size_t &N, const char* unit, const size_t &unit_size) {
    auto va_t = (value_array_t*)malloc(sizeof(value_array_t));
    auto v = (int*)malloc(sizeof(int) * N);
    memcpy(v, value, sizeof(int)*N);
    va_t->obj = v;
    va_t->type = T_INT;
    va_t->N = N;
    communication::datatypes::set_unit(va_t->unit, unit, unit_size);
    return va_t;
}

value_array_t* Float_array(int* value, const size_t &N, const char* unit, const size_t &unit_size) {
    auto va_t = (value_array_t*)malloc(sizeof(value_array_t));
    auto v = (float*)malloc(sizeof(float) * N);
    memcpy(v, value, sizeof(float)*N);
    va_t->obj = v;
    va_t->type = T_FLOAT;
    va_t->N = N;
    communication::datatypes::set_unit(va_t->unit, unit, unit_size);
    return va_t;
}

value_array_t* UInt_array(int* value, const size_t &N, const char* unit, const size_t &unit_size) {
    auto va_t = (value_array_t*)malloc(sizeof(value_array_t));
    auto v = (unsigned int*)malloc(sizeof(uint) * N);
    memcpy(v, value, sizeof(uint)*N);
    va_t->obj = v;
    va_t->type = T_UINT;
    va_t->N = N;
    communication::datatypes::set_unit(va_t->unit, unit, unit_size);
    return va_t;
}

value_array_t* ComplexFloat_array(int* value, const size_t &N, const char* unit, const size_t &unit_size) {
    auto va_t = (value_array_t*)malloc(sizeof(value_array_t));
    auto v = (complex_float_t*)malloc(sizeof(complex_float_t) * N);
    memcpy(v, value, sizeof(complex_float_t)*N);
    va_t->obj = v;
    va_t->type = T_COMPLEX;
    va_t->N = N;
    communication::datatypes::set_unit(va_t->unit, unit, unit_size);
    return va_t;
}

void add_AtoGroup(value_array_t* val, value_group_t* grp) {
    communication::datatypes::ValueItem* v;
    switch (val->type) {
        case T_INT:
            v = new communication::datatypes::ValueArray<int>(val);
            break;
        case T_FLOAT:
            v = new communication::datatypes::ValueArray<float>(val);
            break;
        case T_UINT:
            v = new communication::datatypes::ValueArray<uint>(val);
            break;
        case T_COMPLEX:
            v = new communication::datatypes::ValueArray<complex_float_t>(val);
            break;
    }
    static_cast<communication::datatypes::ValueGroup*>(grp->obj)->addItem(v);
}