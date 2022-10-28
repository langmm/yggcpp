#pragma once
#include "dtype_t.h"
#include "utils/complex_type.hpp"

#ifdef __cplusplus
extern "C" {
#endif

void set_dtype_short(dtype_t* dt, int8_t &val, const char* units="");
void set_dtype_int(dtype_t* dt, int16_t &val, const char* units="");
void set_dtype_long(dtype_t* dt, int32_t &val, const char* units="");
void set_dtype_llong(dtype_t* dt, int64_t &val, const char* units="");
void set_dtype_bool(bool val, const char* units="");
void set_dtype_float(dtype_t* dt, float &val, const char* units="");
void set_dtype_double(dtype_t* dt, double &val, const char* units="");
void set_dtype_ldouble(long double &val, const char* units="");
void set_dtype_ushort(dtype_t* dt, uint8_t &val, const char* units="");
void set_dtype_uint(dtype_t* dt, uint16_t &val, const char* units="");
void set_dtype_ulong(dtype_t* dt, uint32_t &val, const char* units="");
void set_dtype_ullong(dtype_t* dt, uint64_t &val, const char* units="");
void set_dtype_complex_float(dtype_t* dt, complex_float_t &val, const char* units="");
void set_dtype_complex_double(dtype_t* dt, complex_double_t &val, const char* units="");
void set_dtype_complex_ldouble(dtype_t* dt, complex_long_double_t &val, const char* units="");
void set_dtype_string(dtype_t* dt, char* val);

/*void set_dtype_short_array(dtype_t* dt, int8_t *val, const char* units="");
void set_dtype_int_array(dtype_t* dt, int16_t *val, const char* units="");
void set_dtype_long_array(dtype_t* dt, int32_t *val, const char* units="");
void set_dtype_llong_array(dtype_t* dt, int64_t *val, const char* units="");
void set_dtype_bool_array(dtype_t* dt, bool *val, const char* units="");
void set_dtype_float_array(dtype_t* dt, float *val, const char* units="");
void set_dtype_double_array(dtype_t* dt, double *val, const char* units="");
void set_dtype_ldouble_array(dtype_t* dt, long double *val, const char* units="");
void set_dtype_ushort_array(dtype_t* dt, uint8_t *val, const char* units="");
void set_dtype_uint_array(dtype_t* dt, uint16_t *val, const char* units="");
void set_dtype_ulong_array(dtype_t* dt, uint32_t *val, const char* units="");
void set_dtype_ullong_array(dtype_t* dt, uint64_t *val, const char* units="");
void set_dtype_complex_float_array(dtype_t* dt, complex_float_t *val, const char* units="");
void set_dtype_complex_double_array(dtype_t* dt, complex_double_t *val, const char* units="");
void set_dtype_complex_ldouble_array(dtype_t* dt, complex_long_double_t *val, const char* units="");
void set_dtype_string_array(dtype_t* dt, char** val);*/

/*void set_dtype_1dshort(dtype_t* dt, int8_t *val, const size_t& N, const char* units="");
void set_dtype_1dint(dtype_t* dt, int16_t *val, const size_t& N, const char* units="");
void set_dtype_1dlong(dtype_t* dt, int32_t *val, const size_t& N, const char* units="");
void set_dtype_1dllong(dtype_t* dt, int64_t *val, const size_t& N, const char* units="");
void set_dtype_1dbool(dtype_t* dt, bool val, const size_t& N, const char* units="");
void set_dtype_1dfloat(dtype_t* dt, float *val, const size_t& N, const char* units="");
void set_dtype_1ddouble(dtype_t* dt, double *val, const size_t& N, const char* units="");
void set_dtype_1dldouble(dtype_t* dt, long double *val, const size_t& N, const char* units="");
void set_dtype_1dushort(dtype_t* dt, uint8_t *val, const size_t& N, const char* units="");
void set_dtype_1duint(dtype_t* dt, uint16_t *val, const size_t& N, const char* units="");
void set_dtype_1dulong(dtype_t* dt, uint32_t *val, const size_t& N, const char* units="");
void set_dtype_1dullong(dtype_t* dt, uint64_t *val, const size_t& N, const char* units="");
void set_dtype_1dcomplex_float(dtype_t* dt, complex_float_t *val, const size_t& N, const char* units="");
void set_dtype_1dcomplex_double(dtype_t* dt, complex_double_t *val, const size_t& N, const char* units="");
void set_dtype_1dcomplex_ldouble(dtype_t* dt, complex_long_double_t *val, const size_t& N, const char* units="");
void set_dtype_1dstring(dtype_t* dt, char** val, const size_t& N);*/

/*void set_dtype_2dshort(dtype_t* dt, int8_t *val, const size_t& N, const size_t &M, const char* units="");
void set_dtype_2dint(dtype_t* dt, int16_t *val, const size_t& N, const size_t &M, const char* units="");
void set_dtype_2dlong(dtype_t* dt, int32_t *val, const size_t& N, const size_t &M, const char* units="");
void set_dtype_2dllong(dtype_t* dt, int64_t *val, const size_t& N, const size_t &M, const char* units="");
void set_dtype_2dbool(dtype_t* dt, bool val, const size_t& N, const size_t &M, const char* units="");
void set_dtype_2dfloat(dtype_t* dt, float *val, const size_t& N, const size_t &M, const char* units="");
void set_dtype_2ddouble(dtype_t* dt, double *val, const size_t& N, const size_t &M, const char* units="");
void set_dtype_2dldouble(dtype_t* dt, long double *val, const size_t& N, const size_t &M, const char* units="");
void set_dtype_2dushort(dtype_t* dt, uint8_t *val, const size_t& N, const size_t &M, const char* units="");
void set_dtype_2duint(dtype_t* dt, uint16_t *val, const size_t& N, const size_t &M, const char* units="");
void set_dtype_2dulong(dtype_t* dt, uint32_t *val, const size_t& N, const size_t &M, const char* units="");
void set_dtype_2dullong(dtype_t* dt, uint64_t *val, const size_t& N, const size_t &M, const char* units="");
void set_dtype_2dcomplex_float(dtype_t* dt, complex_float_t *val, const size_t& N, const size_t &M, const char* units="");
void set_dtype_2dcomplex_double(dtype_t* dt, complex_double_t *val, const size_t& N, const size_t &M, const char* units="");
void set_dtype_2dcomplex_ldouble(dtype_t* dt, complex_long_double_t *val, const size_t& N, const size_t &M, const char* units="");
*/

int add_VtoGroup(dtype_t* val, dtype_t* grp);
int add_AtoGroup(dtype_t* val, dtype_t* grp);
//int add_2DAtoGroup(dtype_t* val, dtype_t* grp);

#ifdef __cplusplus
}
#endif