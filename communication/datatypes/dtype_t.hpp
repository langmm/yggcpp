#pragma once
#include <cstdlib>
#include "utils/enums.hpp"
#include "utils/macros.hpp"
#include "utils/tools.hpp"
#ifdef __cplusplus
extern "C" {
#endif
#include <Python.h>
struct dtype_t {
    void* obj;
    DTYPE type;
    bool use_generic;
};

/*!
  @brief Skip datatype arguments.
  @param[in] dtype dtype_t* Type structure to skip arguments for.
  @param[in, out] nargs Pointer to number of arguments in ap.
  @param[in, out] ap va_list_t Variable argument list.
  @returns int 0 if there are no errors, 1 otherwise.
 */
int skip_va_elements(const dtype_t* dtype, size_t *nargs, struct va_list_t *ap);


/*!
  @brief Determine if a datatype is empty.
  @param[in] dtype dtype_t* Type structure to test.
  @returns int 1 if dtype is empty, 0 otherwise.
 */
int is_empty_dtype(const dtype_t* dtype);


/*!
  @brief Get the name of the type from the class.
  @param[in] type_class dtype_t* Type structure/class.
  @returns const char* Type name.
*/
const char* dtype_name(const dtype_t* type_class);


/*!
  @brief Get the subtype of the type.
  @param[in] type_class dtype_t* Type structure/class.
  @returns const char* The subtype of the class, "" if there is an error.
*/
const char* dtype_subtype(const dtype_t* type_class);


/*!
  @brief Get the precision of the type.
  @param[in] type_class dtype_t* Type structure/class.
  @returns const size_t The precision of the class, 0 if there is an error.
*/
const size_t dtype_precision(const dtype_t* type_class);

/*!
  @brief Initialize a datatype structure including setting the type string.
  @param[in] dtype dtype_t* Type structure/class.
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Initialized type structure/class.
*/
dtype_t* complete_dtype(dtype_t *dtype, const bool use_generic);


/*!
  @brief Get a copy of a type structure.
  @param[in] dtype dtype_t* Wrapper struct for C++ Metaschema type class.
  @returns: dtype_t* Type class.
*/
dtype_t* copy_dtype(const dtype_t* dtype);


/*!
  @brief Wrapper for updating a type object with information from another.
  @param[in] dtype1 dtype_t* Wrapper struct for C++ Metaschema type class that should be updated.
  @param[in] dtype2 dtype_t* Wrapper struct for C++ Metaschema type class that should be updated from.
  @returns: int 0 if successfull, -1 if there was an error.
*/
int update_dtype(dtype_t* dtype1, dtype_t* dtype2);


/*!
  @brief Wrapper for updatining a type object with information from
  the provided variable arguments if a generic structure is present.
  @param[in] dtype1 dtype_t* Wrapper struct for C++ Metaschema type class that should be updated.
  @param[in] nargs size_t Number of arguments in ap.
  @param[in] ap va_list_t Variable argument list.
  @returns: int 0 if successfull, -1 if there was an error.
 */
int update_dtype_from_generic_ap(dtype_t* dtype1, size_t nargs, struct va_list_t ap);


/*!
  @brief Wrapper for updating the precision of a bytes or unicode scalar type.
  @param[in] dtype dtype_t* Wrapper struct for C++ Metaschema type class.
  @param[in] new_precision size_t New precision.
  @returns: int 0 if free was successfull, -1 if there was an error.
*/
int update_precision_dtype(const dtype_t* dtype,
                           const size_t new_precision);

/*!
  @brief Wrapper for deserializing from a data type.
  @param[in] dtype dtype_t* Wrapper struct for C++ Metaschema type class.
  @param[in] buf character pointer to serialized message.
  @param[in] buf_siz size_t Size of buf.
  @param[in] allow_realloc int If 1, variables being filled are assumed to be
  pointers to pointers for heap memory. If 0, variables are assumed to be pointers
  to stack memory. If allow_realloc is set to 1, but stack variables are passed,
  a segfault can occur.
  @param[in, out] nargs int Number of arguments remaining in argument list.
  @param[in] ap va_list Arguments to be parsed from message.
  returns: int The number of populated arguments. -1 indicates an error.
*/
int deserialize_dtype(const dtype_t *dtype, const char *buf, const size_t buf_siz,
                      const int allow_realloc, size_t *nargs, struct va_list_t ap);


/*!
  @brief Wrapper for serializing from a data type.
  @param[in] dtype dtype_t* Wrapper struct for C++ Metaschema type class.
  @param[in] buf character pointer to pointer to memory where serialized message
  should be stored.
  @param[in] buf_siz size_t Size of memory allocated to buf.
  @param[in] allow_realloc int If 1, buf will be realloced if it is not big
  enough to hold the serialized emssage. If 0, an error will be returned.
  @param[in, out] nargs int Number of arguments remaining in argument list.
  @param[in] ap va_list Arguments to be formatted.
  returns: int The length of the serialized message or -1 if there is an error.
*/
int serialize_dtype(const dtype_t *dtype, char **buf, size_t *buf_siz,
                    const int allow_realloc, size_t *nargs, struct va_list_t ap);


/*!
  @brief Wrapper for displaying a data type.
  @param[in] dtype dtype_t* Wrapper struct for C++ Metaschema type class.
  @param[in] indent char* Indentation to add to display output.
*/
void display_dtype(const dtype_t *dtype, const char* indent);


/*!
  @brief Wrapper for determining how many arguments a data type expects.
  @param[in] dtype dtype_t* Wrapper struct for C++ Metaschema type class.
*/
size_t nargs_exp_dtype(const dtype_t *dtype);

dtype_t* create_dtype(DTYPE dtype, ushort precision=0, bool use_generic=false);

/*!
  @brief Construct and empty type object.
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
*/
dtype_t* create_dtype_empty(const bool use_generic);


/*!
  @brief Create a datatype based on a JSON document.
  @param type_doc void* Pointer to const rapidjson::Value type doc.
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
 */
dtype_t* create_dtype_doc(void* type_doc, const bool use_generic);


/*!
  @brief Create a datatype based on a Python dictionary.
  @param[in] pyobj PyObject* Python dictionary.
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
 */
dtype_t* create_dtype_python(PyObject* pyobj, const bool use_generic);


/*!
  @brief Construct a Direct type object.
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
*/
dtype_t* create_dtype_direct(const bool use_generic);



/*!
  @brief Construct a type object for one of the default JSON types.
  @param[in] type char* Name of the type.
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
*/
dtype_t* create_dtype_default(const char* type,
                              const bool use_generic);


/*!
  @brief Construct a Value type object.
  @param[in] subtype char* Name of the scalar subtype (e.g. int, uint, float, bytes).
  @param[in] precision size_t Precision of the scalar in bits.
  @param[in] units char* Units for scalar. (e.g. "cm", "g", "" for unitless)
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
*/
dtype_t* create_dtype_scalar(SUBTYPE type, size_t precision=0,
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
dtype_t* create_dtype_1darray(SUBTYPE subtype, const size_t& precision, const char* units="");
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
void set_dtype_1dshort(dtype_t* dt, int8_t *val, const size_t& N, const char* units="");
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
void set_dtype_1dstring(dtype_t* dt, char** val, const size_t& N);

dtype_t* create_dtype_2darray(SUBTYPE subtype, const size_t& precision, const char* units="");
dtype_t* create_dtype_2dshort(int8_t *val, const size_t& N, const size_t &M, const char* units="");
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
void set_dtype_2dshort(dtype_t* dt, int8_t *val, const size_t& N, const size_t &M, const char* units="");
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
dtype_t* create_dtype_ndarray(const char* subtype, const size_t precision,
                              const size_t ndim, const size_t* shape,
                              const char* units, const bool use_generic);


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
dtype_t* create_dtype_ndarray_arr(const char* subtype, const size_t precision,
                                  const size_t ndim, const int64_t shape[],
                                  const char* units, const bool use_generic);


/*!
  @brief Construct a JSON array type object.
  @param[in] nitems size_t Number of types in items.
  @param[in] items dtype_t** Pointer to array of types describing the array
  elements.
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
*/
dtype_t* create_dtype_json_array(const size_t nitems, dtype_t** items,
                                 const bool use_generic);


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
dtype_t* create_dtype_json_object(const size_t nitems, char** keys,
                                  dtype_t** values, const bool use_generic);

/*!
  @brief Construct a Ply type object.
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
*/
dtype_t* create_dtype_ply(const bool use_generic);


/*!
  @brief Construct a Obj type object.
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
*/
dtype_t* create_dtype_obj(const bool use_generic);


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
dtype_t* create_dtype_format(const char *format_str, const int as_array,
                             const bool use_generic);


/*!
  @brief Construct a type object for Python objects.
  @param[in] type char* Type string.
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
 */
dtype_t* create_dtype_pyobj(const char* type, const bool use_generic);


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
dtype_t* create_dtype_pyinst(const char* class_name,
                             const dtype_t* args_dtype,
                             const dtype_t* kwargs_dtype,
                             const bool use_generic);


/*!
  @brief Construct a type object for a schema.
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
 */
dtype_t* create_dtype_schema(const bool use_generic);


/*!
  @brief Construct a type object for receiving any type.
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
 */
dtype_t* create_dtype_any(const bool use_generic);


/*!
  @brief Wrapper for freeing MetaschemaType class wrapper struct.
  @param[in] dtype dtype_t** Wrapper struct for C++ Metaschema type class.
  @returns: int 0 if free was successfull, -1 if there was an error.
*/
int destroy_dtype(dtype_t** dtype);

int set_dtype_name(dtype_t* dtype, const char* name);

dtype_t* create_dtype_format(const char* format_str, const int as_array=0, const bool use_generic=false);

dtype_t* init_valgroup();
int add_VtoGroup(dtype_t* val, dtype_t* grp);
int add_AtoGroup(dtype_t* val, dtype_t* grp);
int add_2DAtoGroup(dtype_t* val, dtype_t* grp);

#ifdef __cplusplus
}
#endif