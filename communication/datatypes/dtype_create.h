#pragma once
#include "dtype_t.h"
#include "utils/complex_type.hpp"

#ifdef __cplusplus
extern "C" {
#endif
//#include <Python.h>

dtype_t* create_dtype(DTYPE dtype, ushort precision=0, bool use_generic=false);

/*!
  @brief Construct and empty type object.
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
*/
dtype_t* create_dtype_empty(bool use_generic);


/*!
  @brief Create a datatype based on a JSON document.
  @param type_doc void* Pointer to const rapidjson::Value type doc.
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
 */
 //TODO
//dtype_t* create_dtype_doc(void* type_doc, bool use_generic);


/*!
  @brief Create a datatype based on a Python dictionary.
  @param[in] pyobj PyObject* Python dictionary.
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
 */
 //TODO
//dtype_t* create_dtype_python(PyObject* pyobj, bool use_generic);


/*!
  @brief Construct a Direct type object.
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
*/
//dtype_t* create_dtype_direct(bool use_generic);



/*!
  @brief Construct a type object for one of the default JSON types.
  @param[in] type char* Name of the type.
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
*/
//dtype_t* create_dtype_default(const char* type,
//                              bool use_generic);


/*!
  @brief Construct a Value type object.
  @param[in] subtype char* Name of the scalar subtype (e.g. int, uint, float, bytes).
  @param[in] precision size_t Precision of the scalar in bits.
  @param[in] units char* Units for scalar. (e.g. "cm", "g", "" for unitless)
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
*/
dtype_t* create_dtype_scalar(SUBTYPE type, uint8_t precision=0,
                             const char* units="");
dtype_t* create_dtype_short(int8_t &val, const char* units="");
dtype_t* create_dtype_int(int16_t &val, const char* units="");
dtype_t* create_dtype_long(int32_t &val, const char* units="");
dtype_t* create_dtype_llong(int64_t &val, const char* units="");
dtype_t* create_dtype_bool(bool val, const char* units="");
dtype_t* create_dtype_float(float &val, const char* units="");
dtype_t* create_dtype_double(double &val, const char* units="");
dtype_t* create_dtype_ldouble(long double &val, const char* units="");
dtype_t* create_dtype_ushort(uint8_t &val, const char* units="");
dtype_t* create_dtype_uint(uint16_t &val, const char* units="");
dtype_t* create_dtype_ulong(uint32_t &val, const char* units="");
dtype_t* create_dtype_ullong(uint64_t &val, const char* units="");
dtype_t* create_dtype_complex_float(complex_float_t &val, const char* units="");
dtype_t* create_dtype_complex_double(complex_double_t &val, const char* units="");
dtype_t* create_dtype_complex_ldouble( complex_long_double_t &val, const char* units="");
dtype_t* create_dtype_string(char* val);

/*dtype_t* create_dtype_empty_array(SUBTYPE st, const size_t& ndim, const size_t dim[],
                                  const size_t &precision=0, const char* units="");
dtype_t* create_dtype_short_array(int8_t *val, const size_t& N, const size_t dim[], const char* units="");
dtype_t* create_dtype_int_array(int16_t *val, const size_t& N, const size_t dim[], const char* units="");
dtype_t* create_dtype_long_array(int32_t *val, const size_t& N, const size_t dim[], const char* units="");
dtype_t* create_dtype_llong_array(int64_t *val, const size_t& N, const size_t dim[], const char* units="");
dtype_t* create_dtype_bool_array(bool val, const size_t& N, const size_t dim[], const char* units="");
dtype_t* create_dtype_float_array(float *val, const size_t& N, const size_t dim[], const char* units="");
dtype_t* create_dtype_double_array(double *val, const size_t& N, const size_t dim[], const char* units="");
dtype_t* create_dtype_ldouble_array(long double *val, const size_t& N, const size_t dim[], const char* units="");
dtype_t* create_dtype_ushort_array(uint8_t *val, const size_t& N, const size_t dim[], const char* units="");
dtype_t* create_dtype_uint_array(uint16_t *val, const size_t& N, const size_t dim[], const char* units="");
dtype_t* create_dtype_ulong_array(uint32_t *val, const size_t& N, const size_t dim[], const char* units="");
dtype_t* create_dtype_ullong_array(uint64_t *val, const size_t& N, const size_t dim[], const char* units="");
dtype_t* create_dtype_complex_float_array(complex_float_t *val, const size_t& N, const size_t dim[], const char* units="");
dtype_t* create_dtype_complex_double_array(complex_double_t *val, const size_t& N, const size_t dim[], const char* units="");
dtype_t* create_dtype_complex_ldouble_array(complex_long_double_t *val, const size_t& N, const size_t dim[], const char* units="");
dtype_t* create_dtype_string_array(char** val, const size_t& N, const size_t dim[]);
*/
/*!
  @brief Construct a 1D array type object.
  @param[in] subtype char* Name of the array subtype (e.g. int, uint, float, bytes).
  @param[in] precision size_t Precision of the array in bits.
  @param[in] length size_t Number of elements in the array.
  @param[in] units char* Units for array elements. (e.g. "cm", "g", "" for unitless)
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
*/
dtype_t* create_dtype_1darray(SUBTYPE subtype, const size_t& size=1, const uint8_t & precision=0, const char* units="");
dtype_t* create_dtype_1dshort(int8_t *val, const size_t& N, const char* units="");
dtype_t* create_dtype_1dint(int16_t *val, const size_t& N, const char* units="");
dtype_t* create_dtype_1dlong(int32_t *val, const size_t& N, const char* units="");
dtype_t* create_dtype_1dllong(int64_t *val, const size_t& N, const char* units="");
dtype_t* create_dtype_1dbool(bool val, const size_t& N, const char* units="");
dtype_t* create_dtype_1dfloat(float *val, const size_t& N, const char* units="");
dtype_t* create_dtype_1ddouble(double *val, const size_t& N, const char* units="");
dtype_t* create_dtype_1dldouble(long double *val, const size_t& N, const char* units="");
dtype_t* create_dtype_1dushort(uint8_t *val, const size_t& N, const char* units="");
dtype_t* create_dtype_1duint(uint16_t *val, const size_t& N, const char* units="");
dtype_t* create_dtype_1dulong(uint32_t *val, const size_t& N, const char* units="");
dtype_t* create_dtype_1dullong(uint64_t *val, const size_t& N, const char* units="");
dtype_t* create_dtype_1dcomplex_float(complex_float_t *val, const size_t& N, const char* units="");
dtype_t* create_dtype_1dcomplex_double(complex_double_t *val, const size_t& N, const char* units="");
dtype_t* create_dtype_1dcomplex_ldouble(complex_long_double_t *val, const size_t& N, const char* units="");
dtype_t* create_dtype_1dstring(char** val, const size_t& N);

 //dtype_t* create_dtype_2darray(SUBTYPE subtype, const size_t &ndim, const size_t dim[], const size_t& precision=0, const char* units="");
/*dtype_t* create_dtype_2dshort(int8_t *val, const size_t& N, const size_t &M, const char* units="");
dtype_t* create_dtype_2dint(int16_t *val, const size_t& N, const size_t &M, const char* units="");
dtype_t* create_dtype_2dlong(int32_t *val, const size_t& N, const size_t &M, const char* units="");
dtype_t* create_dtype_2dllong(int64_t *val, const size_t& N, const size_t &M, const char* units="");
dtype_t* create_dtype_2dbool(bool val, const size_t& N, const size_t &M, const char* units="");
dtype_t* create_dtype_2dfloat(float *val, const size_t& N, const size_t &M, const char* units="");
dtype_t* create_dtype_2ddouble(double *val, const size_t& N, const size_t &M, const char* units="");
dtype_t* create_dtype_2dldouble(long double *val, const size_t& N, const size_t &M, const char* units="");
dtype_t* create_dtype_2dushort(uint8_t *val, const size_t& N, const size_t &M, const char* units="");
dtype_t* create_dtype_2duint(uint16_t *val, const size_t& N, const size_t &M, const char* units="");
dtype_t* create_dtype_2dulong(uint32_t *val, const size_t& N, const size_t &M, const char* units="");
dtype_t* create_dtype_2dullong(uint64_t *val, const size_t& N, const size_t &M, const char* units="");
dtype_t* create_dtype_2dcomplex_float(complex_float_t *val, const size_t& N, const size_t &M, const char* units="");
dtype_t* create_dtype_2dcomplex_double(complex_double_t *val, const size_t& N, const size_t &M, const char* units="");
dtype_t* create_dtype_2dcomplex_ldouble(complex_long_double_t *val, const size_t& N, const size_t &M, const char* units="");
*/
 /*!
  @brief Construct a ND array type object.
  @param[in] subtype char* Name of the array subtype (e.g. int, uint, float, bytes).
  @param[in] precision size_t Precision of the array in bits.
  @param[in] ndim size_t Number of dimensions in the array (and therefore also the
  number of elements in shape).
  @param[in] shape size_t* Pointer to array where each element is the size of the
  array in that dimension.
  @param[in] units char* Units for array elements. (e.g. "cm", "g", "" for unitless)
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
*/
//dtype_t* create_dtype_ndarray(const char* subtype, const size_t precision,
//                              const size_t ndim, const size_t* shape,
//                              const char* units, const bool use_generic);


/*!
  @brief Construct a ND array type object.
  @param[in] subtype char* Name of the array subtype (e.g. int, uint, float, bytes).
  @param[in] precision size_t Precision of the array in bits.
  @param[in] ndim size_t Number of dimensions in the array (and therefore also the
  number of elements in shape).
  @param[in] shape[] size_t Array where each element is the size of the
  array in that dimension.
  @param[in] units char* Units for array elements. (e.g. "cm", "g", "" for unitless)
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
*/
//dtype_t* create_dtype_ndarray_arr(const char* subtype, const size_t precision,
//                                  const size_t ndim, const int64_t shape[],
//                                  const char* units, const bool use_generic);


/*!
  @brief Construct a JSON array type object.
  @param[in] nitems size_t Number of types in items.
  @param[in] items dtype_t** Pointer to array of types describing the array
  elements.
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
*/
//dtype_t* create_dtype_json_array(const size_t nitems, dtype_t** items,
//                                 const bool use_generic);


/*!
  @brief Construct a JSON object type object.
  @param[in] nitems size_t Number of keys/types in keys and values.
  @param[in] keys char** Pointer to array of keys for each type.
  @param[in] values dtype_t** Pointer to array of types describing the values
  for each key.
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
*/
//dtype_t* create_dtype_json_object(const size_t nitems, char** keys,
//                                  dtype_t** values, const bool use_generic);

/*!
  @brief Construct a Ply type object.
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
*/
//dtype_t* create_dtype_ply(bool use_generic);


/*!
  @brief Construct a Obj type object.
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
*/
//dtype_t* create_dtype_obj(bool use_generic);


/*!
  @brief Construct a type object based on the provided format string.
  @param[in] format_str const char* C-style format string that will be used to determine
  the type of elements in arrays that will be serialized/deserialized using
  the resulting type.
  @param[in] as_array int If 1, the types will be arrays. Otherwise they will be
  scalars.
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
*/
dtype_t* create_dtype_format(const char *format_str, const int& as_array,
                             bool use_generic);


/*!
  @brief Construct a type object for Python objects.
  @param[in] type char* Type string.
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
 */
//dtype_t* create_dtype_pyobj(const char* type, bool use_generic);


/*!
  @brief Construct a type object for Python object instances.
  @param[in] class_name char* Python class name.
  @param[in] args_dtype dtype_t* Datatype describing the arguments
  creating the instance.
  @param[in] kwargs_dtype dtype_t* Datatype describing the keyword
  arguments creating the instance.
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
 */
//dtype_t* create_dtype_pyinst(const char* class_name,
//                             const dtype_t* args_dtype,
//                             const dtype_t* kwargs_dtype,
//                             bool use_generic);


/*!
  @brief Construct a type object for a schema.
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
 */
//dtype_t* create_dtype_schema(bool use_generic);


/*!
  @brief Construct a type object for receiving any type.
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
 */
//dtype_t* create_dtype_any(bool use_generic);

dtype_t* init_valgroup();

#ifdef __cplusplus
}
#endif