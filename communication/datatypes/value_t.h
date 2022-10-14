#pragma once
#include "utils/enums.hpp"
#include "dtype_t.hpp"
#include "utils/tools.hpp"
#ifdef __cplusplus
extern "C" {
#endif

typedef struct value_t {
    void* obj;
    char* unit;
    SUBTYPE type;
    int precision;
} value_t;

typedef struct value_group_t {
    void* obj;
}value_group_t;

typedef struct value_array_t {
    void* obj;
    SUBTYPE type;
    char* unit;
    size_t N;
    int precision;
}value_array_t;

typedef struct value_2Darray_t {
    void* obj;
    SUBTYPE type;
    char* unit;
    size_t N;
    size_t M;
    int precision;
}value_2Darray_t;

value_t* Int_value(int &value, const char* unit="", const size_t &unit_size=0);
value_t* Bool_value(bool value, const char* unit="", const size_t &unit_size=0);
value_t* String_value(char* value, const size_t& size, const char* unit="", const size_t &unit_size=0);
value_t* Number_value();
value_t* Float_value(float &value, const char* unit="", const size_t &unit_size=0);
value_t* UInt_value(uint& value, const char* unit="", const size_t &unit_size=0);
value_t* ComplexFloat_value(complex_float_t& value, const char* unit="", const size_t &unit_size=0);
value_t* Bytes_value();
value_t* Unicode_value(char* value, const char* unit="", const size_t &unit_size=0);

value_array_t* Int_array(int* value, const size_t &N, const char* unit="", const size_t &unit_size=0);
value_array_t* Float_array(int* value, const size_t &N, const char* unit="", const size_t &unit_size=0);
value_array_t* UInt_array(int* value, const size_t &N, const char* unit="", const size_t &unit_size=0);
value_array_t* ComplexFloat_array(int* value, const size_t &N, const char* unit="", const size_t &unit_size=0);

value_2Darray_t* Int_2Darray(int** value, const size_t &N, const size_t &M, const char* unit="", const size_t &unit_size=0);
value_2Darray_t* Float_2Darray(int** value, const size_t &N, const size_t &M, const char* unit="", const size_t &unit_size=0);
value_2Darray_t* UInt_2Darray(int** value, const size_t &N, const size_t &M, const char* unit="", const size_t &unit_size=0);
value_2Darray_t* ComplexFloat_2Darray(int** value, const size_t &N, const size_t &M, const char* unit="", const size_t &unit_size=0);

#ifdef __cplusplus
}
#endif