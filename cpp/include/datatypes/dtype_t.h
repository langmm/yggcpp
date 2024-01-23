#pragma once
#include "utils/enums.hpp"
#include "utils/complex_type.hpp"
#include "rapidjson/pyrj_c.h"
#ifdef __cplusplus
#include <cstdlib>
#else
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*! @brief C-friendly definition of rapidjson::Document. */
typedef struct dtype_t {
  void *metadata; //!< Pointer to wrapped Metadata object.
} dtype_t;

/*! @brief C-friendly wrapper for rapidjson::Document. */
typedef struct generic_t {
  void *obj; //!< Pointer to rapidjson::Document.
} generic_t;

/*! @brief C-friendly wrapper for rapidjson::Value. */
typedef struct generic_ref_t {
  void *obj; //!< Pointer to rapidjson::Value.
  void *allocator; //!< Pointer to rapidjson Allocator used to allocated obj.
} generic_ref_t;

/*! @brief Structure used to wrap Python objects. */
typedef struct python_t {
  PyObject *obj; //!< Python object.
} python_t;

/*! @brief C-friendly definition of vector object. */
typedef generic_t json_array_t;

/*! @brief C-friendly definition of map object. */
typedef generic_t json_object_t;

/*! @brief C-friendly definition of schema object. */
typedef generic_t schema_t;

/*! @brief C-friendly defintion of Python class object. */
typedef python_t python_class_t;

/*! @brief C-friendly defintion of Python function object. */
typedef python_t python_function_t;

/*! @brief C-friendly defintion of Python instance object. */
typedef generic_t python_instance_t;

/*! @brief Obj structure. */
typedef struct obj_t {
  void* obj; //!< Pointer to rapidjson::ObjWavefront instance.
} obj_t;

/*! @brief Ply structure. */
typedef struct ply_t {
  void* obj; //!< Pointer to rapidjson::Ply instance.
} ply_t;

/*!
  @brief Initialize an empty generic object.
  @returns generic_t New generic object structure.
 */
generic_t init_generic();

/*!
  @brief Initialize an empty generic reference object.
  @param[in] parent Generic object that will be the parent of the
    returned reference object.
  @returns New generic reference object structure.
 */
generic_ref_t init_generic_ref(generic_t parent);

/*!
  @brief Initialize an empty generic object with a null JSON document
  @returns generic_t New generic object structure.
 */
generic_t init_generic_null();
  
/*!
  @brief Initialize an empty array of mixed types with generic wrappers.
  @returns generic_t New generic object structure containing an empty
    array.
*/
generic_t init_generic_array();

/*!
  @brief Initialize an empty map (JSON object) of mixed types with
    generic wrappers.
  @returns New generic object structure contaiing an empty map (JSON
    object).
 */
generic_t init_generic_map();

/*!
  @brief Initialize a generic object from a JSON string.
  @param[in] json JSON encoded string.
  @returns New generic object structure wrapping a rapidjson::Document
    instance.
 */
generic_t init_generic_json(const char* json);

/*!
  @brief Initialize a generic object from a JSON string.
  @param[in] schema JSON encoded schema describing object to generate.
  @returns New generic object structure wrapping a rapidjson::Document
    instance.
 */
generic_t init_generic_generate(const char* schema);
  
/*!
  @brief Determine if a generic structure is initialized.
  @param[in] x Generic structure to test.
  @returns 1 if the structure is initialized, 0 otherwise.
 */
int is_generic_init(generic_t x);

/*!
  @brief Determine if a generic reference structure is initialized.
  @param[in] x Generic reference structure to test.
  @returns 1 if the structure is initialized, 0 otherwise.
 */
int is_generic_ref_init(generic_ref_t x);

//
//  @brief Create a generic object from the provided information.
//  @param[in] type_class Type structure/class.
//  @param[in] data Pointer to data.
//  @param[in] nbytes Size of data.
//  @returns generic_t Pointer to new generic object structure.
//
// generic_t create_generic(dtype_t type_class, void* data, size_t nbytes);

  
/*!
  @brief Destroy a generic object.
  @param[in] x generic_t* Pointer to generic object structure to destory.
  @returns int -1 if unsuccessful, 0 otherwise.
 */
int destroy_generic(generic_t* x);


/*!
  @brief Copy data from one generic object into another.
  @param[in,out] dst Pointer to destination object.
  @param[in] src Source object.
  @returns int -1 if unsuccessful, 0 otherwise.
*/
int copy_generic_into(generic_t* dst, generic_t src);


/*!
  @brief Copy data from one generic object to the other.
  @param[in] src generic_t Generic structure that data should be copied from.
  @returns generic_t Copied structure.
 */
generic_t copy_generic(generic_t src);

/*!
  @brief Compare two generic objects.
  @param[in] a First object for comparison.
  @param[in] b Second object for comparison.
  @returns true if the two objects are equivalent, false otherwise.
 */
bool compare_generic(generic_t a, generic_t b);

/*!
  @brief Display information about the generic type.
  @param[in] x generic_t* Wrapper for generic object.
 */
void display_generic(generic_t x);


#define NESTED_BASICS_(base, idx, idxType)	\
  void* generic_ ## base ## _get_item(generic_t x, idxType idx, const char *type); \
  int generic_ ## base ## _get_item_nbytes(generic_t x, idxType idx, const char *type); \
  void* generic_ ## base ## _get_scalar(generic_t x, idxType idx, const char *subtype, const size_t precision); \
  size_t generic_ ## base ## _get_1darray(generic_t x, idxType idx, const char *subtype, const size_t precision, void** data); \
  size_t generic_ ## base ## _get_ndarray(generic_t x, idxType idx, const char *subtype, const size_t precision, void** data, size_t** shape); \
  int generic_ ## base ## _set_item(generic_t x, idxType idx, const char *type, void* value); \
  int generic_ ## base ## _set_scalar(generic_t x, idxType idx,		\
				      void* value,			\
				      const char *subtype,		\
				      const size_t precision,		\
				      const char *units);		\
  int generic_ ## base ## _set_1darray(generic_t x, idxType idx,	\
				       void* value,			\
				       const char *subtype,		\
				       const size_t precision,		\
				       const size_t length,		\
				       const char *units);		\
  int generic_ ## base ## _set_ndarray(generic_t x, idxType idx,	\
				       void* value,			\
				       const char *subtype,		\
				       const size_t precision,		\
				       const size_t ndim,		\
				       const size_t* shape,		\
				       const char *units);
  
  NESTED_BASICS_(array, index, const size_t)
  NESTED_BASICS_(map, key, const char*)

#undef NESTED_BASICS_
    
/*!
  @brief Add an element to the end of an array of generic elements.
  @param[in] arr generic_t Array to add element to.
  @param[in] x generic_t Element to add.
  @returns int Flag that is 1 if there is an error and 0 otherwise.
 */
int add_generic_array(generic_t arr, generic_t x);


/*!
  @brief Set an element in the array at a given index to a new value.
  @param[in] arr generic_t Array to add element to.
  @param[in] i size_t Index where element should be added.
  @param[in] x generic_t Element to add.
  @returns int Flag that is 1 if there is an error and 0 otherwise.
 */
int set_generic_array(generic_t arr, const size_t i, generic_t x);


/*!
  @brief Get an element from an array.
  @param[in] arr generic_t Array to get element from.
  @param[in] i size_t Index of element to get.
  @param[out] x generic_t* Pointer to address where element should be
    stored.
  @returns int Flag that is 1 if there is an error and 0 otherwise.
 */
int get_generic_array(generic_t arr, const size_t i, generic_t *x);
int get_generic_array_ref(generic_t arr, const size_t i, generic_ref_t *x);


/*!
  @brief Set an element in the object at for a given key to a new value.
  @param[in] arr generic_t Object to add element to.
  @param[in] k const char* Key where element should be added.
  @param[in] x generic_t Element to add.
  @returns int Flag that is 1 if there is an error and 0 otherwise.
 */
int set_generic_object(generic_t arr, const char* k, generic_t x);


/*!
  @brief Get an element from an object.
  @param[in] arr generic_t Object to get element from.
  @param[in] k const char* Key of element to return.
  @param[out] x generic_t* Pointer to address where element should be
  stored.
  @returns int Flag that is 1 if there is an error and 0 otherwise.
 */
int get_generic_object(generic_t arr, const char* k, generic_t *x);
int get_generic_object_ref(generic_t arr, const char* k, generic_ref_t *x);


#define set_generic_map set_generic_object
#define get_generic_map get_generic_object
#define get_generic_map_ref get_generic_object_ref


/*!
  @brief Get the number of elements in an array object.
  @param[in] x generic_t Generic object that is presumed to contain an array.
  @returns size_t Number of elements in array.
 */
size_t generic_array_get_size(generic_t x);

/*!
  @brief Get the number of elements in an map object.
  @param[in] x generic_t Generic object that is presumed to contain a map.
  @returns size_t Number of elements in map.
 */
size_t generic_map_get_size(generic_t x);

/*!
  @brief Determine if a map object has a certain key.
  @param[in] x generic_t Generic object that is presumed to contain a map.
  @param[in] key char* Key to check for.
  @returns int 1 if the key is present, 0 otherwise.
 */
int generic_map_has_key(generic_t x, const char* key);

/*!
  @brief Get the keys in a map object.
  @param[in] x generic_t Generic object that is presumed to contain a map.
  @param[out] keys char*** Pointer to memory where array of keys should be stored.
  @returns size_t Number of keys in map.
 */
size_t generic_map_get_keys(generic_t x, char*** keys);

/**
 * @brief Get a pointer to the given generic reference item
 * @param[in] x The generic reference item to use
 * @param[in] type The type x should be
 * @return Pointer to the reference item or NULL on error
 */
void* generic_ref_get_item(generic_ref_t x, const char *type);
/**
 * @brief Get a pointer to the generic item
 * @param[in] x The generic item to use
 * @param[in] type The type x should be
 * @return Pointer to the item or NULL on error
 */
void* generic_get_item(generic_t x, const char *type);
/**
 * @brief Get the size of the reference item
 * @param[in] x The reference item to use
 * @param[in] type The type x should be
 * @return The size in bytes or -1 on error
 */
int generic_ref_get_item_nbytes(generic_ref_t x, const char *type);
/**
 * @brief Get the size of the item
 * @param[in] x The item to use
 * @param[in] type The type x should be
 * @return The size in bytes or -1 on error
 */
int generic_get_item_nbytes(generic_t x, const char *type);
/**
 * @brief Set the data in the given item to the given value
 * @param[in, out] x The item to set
 * @param[in] type The type x should be
 * @param[in] value The value to give to x
 * @return 0 on success, 1 on error
 */
int generic_set_item(generic_t x, const char *type, void* value);
/**
 * @brief Set the data in the given item to the value given by the json character string
 * @param[in, out] x The item to set
 * @param[in] json The value to set x to
 * @return 0 on success, 1 on error
 */
int generic_set_json(generic_t x, const char *json);
/**
 * @brief Get x as a scalar value
 * @param[in] x The data to retrieve
 * @param[in] subtype The subtype x should be
 * @param[in] precision The precision of the scalar
 * @return Pointer to the scalar value, NULL on error
 */
void* generic_ref_get_scalar(generic_ref_t x, const char *subtype, const size_t precision);
/**
 * @brief Get x as a scalar value
 * @param[in] x The data to retrieve
 * @param[in] subtype The subtype x should be
 * @param[in] precision The precision of the scalar
 * @return Pointer to the scalar value, NULL on error
 */
void* generic_get_scalar(generic_t x, const char *subtype, const size_t precision);
/**
 * @brief Get x as a 1D array of values
 * @param[in] x The data to retrieve
 * @param[in] subtype The subtype x should be
 * @param[in] precision The precision of the array values
 * @param[out] data Pointer to a pointer array for the values, will be resized as needed
 * @return The number of elements in data, 0 on error
 */
size_t generic_ref_get_1darray(generic_ref_t x, const char *subtype, const size_t precision, void** data);
/**
 * @brief Get x as a 1D array of values
 * @param[in] x The data to retrieve
 * @param[in] subtype The subtype x should be
 * @param[in] precision The precision of the array values
 * @param[out] data Pointer to a pointer array for the values, will be resized as needed
 * @return The number of elements in data, 0 on error
 */
size_t generic_get_1darray(generic_t x, const char *subtype, const size_t precision, void** data);
/**
 * @brief Get x as an N dimensionnal array of pointers
 * @param[in] x The data to retrieve
 * @param[in] subtype The subtype x should be
 * @param[in] precision The precision of the array data members
 * @param[out] data Pointer to an array of pointers for the values of the array, will be resized as needed
 * @param[out] shape Pointer to an array of pointers, one for each dimension of data, giving the number of elements in each
 * @return The number of dimensions, or 0 on error
 */
size_t generic_ref_get_ndarray(generic_ref_t x, const char *subtype, const size_t precision, void** data, size_t** shape);
/**
 * @brief Get x as an N dimensionnal array of pointers
 * @param[in] x The data to retrieve
 * @param[in] subtype The subtype x should be
 * @param[in] precision The precision of the array data members
 * @param[out] data Pointer to an array of pointers for the values of the array, will be resized as needed
 * @param[out] shape Pointer to an array of pointers, one for each dimension of data, giving the number of elements in each
 * @return The number of dimensions, or 0 on error
 */
size_t generic_get_ndarray(generic_t x, const char *subtype, const size_t precision, void** data, size_t** shape);
/**
 * @brief Set the given item to the given scalar value
 * @param[in] x The item to set
 * @param[in] value The value to give to x
 * @param[in] subtype The expected subtype of x
 * @param[in] precision The precision of value
 * @param[in] units Th units of value
 * @return 0 on success, 1 on error
 */
int generic_set_scalar(generic_t x, void* value, const char *subtype,
		       const size_t precision, const char *units);
/**
 * @brief Set the given item to the given array
 * @param[in] x The item to set
 * @param[in] value The array of values to give to x
 * @param[in] subtype The expected subtype of x
 * @param[in] precision The precision of value
 * @param[in] length The number of elements in value
 * @param[in] units The units of value
 * @return 0 on success, 1 on error
 */
int generic_set_1darray(generic_t x, void* value, const char *subtype,
			const size_t precision, const size_t length,
			const char* units);
/**
 * @brief Set the given item to the given array
 * @param[in] x The item to set
 * @param[in] data The values to give to x
 * @param[in] subtype The expected subtype of x
 * @param[in] precision The precision of data
 * @param[in] ndim The number of dimensions in data
 * @param[in] shape Array giving the number of elements in each dimension
 * @param[in] units The units of data
 * @return 0 on success, 1 on error
 */
int generic_set_ndarray(generic_t x, void* data, const char *subtype,
			const size_t precision, const size_t ndim,
			const size_t* shape, const char* units);

  
#define NESTED_BASE_SET_(base, idx, idxType, name, ...)			\
  int generic_ ## base ## _set_ ## name(generic_t x, idxType idx, __VA_ARGS__)
#define NESTED_BASE_GET_(base, idx, idxType, name, type, ...)		\
  type generic_ ## base ## _get_ ## name(generic_t x, idxType idx, __VA_ARGS__)
#define NESTED_BASE_GET_NOARGS_(base, idx, idxType, name, type)	\
  type generic_ ## base ## _get_ ## name(generic_t x, idxType idx)
#define NESTED_SET_(name, ...)						\
  NESTED_BASE_SET_(array, index, const size_t, name, __VA_ARGS__);	\
  NESTED_BASE_SET_(map, key, const char*, name, __VA_ARGS__)
#define NESTED_GET_(name, type, ...)					\
  NESTED_BASE_GET_(array, index, const size_t, name, type, __VA_ARGS__); \
  NESTED_BASE_GET_(map, key, const char*, name, type, __VA_ARGS__)
#define NESTED_GET_NOARGS_(name, type)		\
  NESTED_BASE_GET_NOARGS_(array, index, const size_t, name, type);	\
  NESTED_BASE_GET_NOARGS_(map, key, const char*, name, type)
#define STD_JSON_NESTED_(name)						\
  generic_t generic_array_get_ ## name(generic_t x, const size_t index); \
  generic_t generic_map_get_ ## name(generic_t x, const char* key);	\
  int generic_array_set_ ## name(generic_t x, const size_t index, generic_t item); \
  int generic_map_set_ ## name(generic_t x, const char* key, generic_t item)
#define STD_JSON_(name, type)						\
  type generic_ref_get_ ## name(generic_ref_t x);			\
  type generic_get_ ## name(generic_t x);				\
  int generic_set_ ## name(generic_t x, type value);			\
  NESTED_GET_NOARGS_(name, type);					\
  NESTED_SET_(name, type value)
#define STD_UNITS_(name, type)			\
  type generic_ref_get_ ## name(generic_ref_t x);			\
  type generic_get_ ## name(generic_t x);				\
  int generic_set_ ## name(generic_t x, type value, const char* units); \
  NESTED_GET_NOARGS_(name, type);					\
  NESTED_SET_(name, type value, const char* units)
#define GEOMETRY_(name, type)			\
  STD_JSON_(name, type)
// TODO: Allow units when calling "get" methods?
#define ARRAY_(name, type)						\
  size_t generic_ref_get_1darray_ ## name(generic_ref_t x, type** data); \
  size_t generic_get_1darray_ ## name(generic_t x, type** data);	\
  size_t generic_ref_get_ndarray_ ## name(generic_ref_t x, type** data, size_t** shape); \
  size_t generic_get_ndarray_ ## name(generic_t x, type** data, size_t** shape); \
  NESTED_GET_(1darray_ ## name, size_t, type** data);			\
  NESTED_GET_(ndarray_ ## name, size_t, type** data, size_t** shape);	\
  NESTED_SET_(1darray_ ## name, type* value, const size_t length, const char* units); \
  NESTED_SET_(ndarray_ ## name, type* data, const size_t ndim, const size_t* shape, const char* units)
#define SCALAR_(name, type)		\
  STD_UNITS_(name, type);		\
  ARRAY_(name, type)
#define COMPLEX_(name, type)			\
  SCALAR_(name, type)
#define PYTHON_(name)				\
  STD_JSON_(name, python_t)
  
  STD_JSON_(bool, bool);
  STD_JSON_(integer, int);
  STD_JSON_(null, void*);
  STD_JSON_(number, double);
  STD_JSON_(string, const char*);
  STD_JSON_NESTED_(object);
  STD_JSON_NESTED_(array);
  STD_JSON_NESTED_(any);
  STD_JSON_NESTED_(schema);
  SCALAR_(int8, int8_t);
  SCALAR_(int16, int16_t);
  SCALAR_(int32, int32_t);
  SCALAR_(int64, int64_t);
  SCALAR_(uint8, uint8_t);
  SCALAR_(uint16, uint16_t);
  SCALAR_(uint32, uint32_t);
  SCALAR_(uint64, uint64_t);
  SCALAR_(float, float);
  SCALAR_(double, double);
  COMPLEX_(complex_float, complex_float_t);
  COMPLEX_(complex_double, complex_double_t);
#ifdef YGGDRASIL_LONG_DOUBLE_AVAILABLE
  SCALAR_(long_double, long double);
  COMPLEX_(complex_long_double, complex_long_double_t);
#endif // YGGDRASIL_LONG_DOUBLE_AVAILABLE
  /* SCALAR_(bytes, const char*); */
  /* SCALAR_(unicode, const char*); */
  PYTHON_(python_class);
  PYTHON_(python_function);
  PYTHON_(python_instance);
  GEOMETRY_(obj, obj_t);
  GEOMETRY_(ply, ply_t);

#undef GEOMETRY_
#undef COMPLEX_
#undef PYTHON_
#undef SCALAR_
#undef ARRAY_
#undef STD_UNITS_
#undef STD_JSON_
#undef STD_JSON_NESTED_
#undef NESTED_SET_
#undef NESTED_GET_
#undef NESTED_GET_NOARGS_
#undef NESTED_BASE_SET_
#undef NESTED_BASE_GET_
#undef NESTED_BASE_GET_NOARGS_
#undef GENERIC_ERROR_
#undef GENERIC_SUCCESS_

/*!
  @brief Initialize Python if it is not initialized.
  @returns int 0 if successful, other values indicate errors.
 */
int init_python_API();
  
/*!
  @brief Initialize a Python wrapper object.
  @returns Initialized object.
 */
python_t init_python();
  
/*!
  @brief Destroy a structure containing a Python object.
  @param[in] x Pointer to Python object structure that should be freed.
*/
void destroy_python(python_t *x);


/*!
  @brief Copy a Python object structure (NOTE: this dosn't copy the
    underlying Python object but does increment the reference count).
  @param[in] x Structure containing Python object to copy.
  @returns python_t Copy of x.
 */
python_t copy_python(python_t x);


/*!
  @brief Display a Python object structure.
  @param[in] x Structure containing Python object to display.
 */
void display_python(python_t x);

  
/*!
  @brief Destroy a structure containing a Python function object.
  @param[in] x Pointer to Python function structure that should be freed.
*/
void destroy_python_function(python_function_t *x);


/*!
  @brief Determine if a datatype is empty.
  @param[in] dtype structure to test.
  @returns int 1 if dtype is empty, 0 otherwise.
 */
int is_empty_dtype(const dtype_t dtype);

  
/*!
  @brief Determine if a datatype was created from a format.
  @param[in] type_struct Datatype structure.
  @returns 1 if the datatype was created from a format, 0 if it was not,
    -1 if there is an error.
 */
int is_dtype_format_array(const dtype_t type_struct);
  
  
/*!
  @brief Get the name of the type from the class.
  @param[in] type_class Type structure/class.
  @returns Type name.
*/
const char* dtype_name(const dtype_t type_class);


/*!
  @brief Get the subtype of the type.
  @param[in] type_class Type structure/class.
  @returns The subtype of the class, "" if there is an error.
*/
const char* dtype_subtype(const dtype_t type_class);


/*!
  @brief Get the precision of the type.
  @param[in] type_class Type structure/class.
  @returns The precision of the class, 0 if there is an error.
*/
size_t dtype_precision(const dtype_t type_class);


/*!
  @brief Set the type name in the datatype structure.
  @param[in,out] dtype Datatype structure to update. It must have
    been initialized.
  @param[in] name Type name to set in dtype.
  @returns 0 on success, -1 if there is an error.
 */
int set_dtype_name(dtype_t dtype, const char* name);

  
/*!
  @brief Initialize a datatype structure including setting the type string.
  @param[in] dtype Type structure/class.
  @param[in] use_generic If true, serialized/deserialized
    objects will be expected to be generic_t instances.
  @returns Initialized type structure/class.
*/
dtype_t complete_dtype(dtype_t dtype, const bool use_generic);


/*!
  @brief Construct a type object from a JSON schema.
  @param[in] schema Serialized JSON schema.
  @param[in] use_generic If true, serialized/deserialized objects will
    be expected to be generic_t instances.
  @returns Type structure/class.
 */
dtype_t create_dtype_from_schema(const char* schema,
				 const bool use_generic);


/*!
  @brief Construct and empty type object.
  @param[in] use_generic If true, serialized/deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure/class.
*/
dtype_t create_dtype_empty(const bool use_generic);


/*!
  @brief Create a datatype based on a Python dictionary.
  @param[in] pyobj Python dictionary.
  @param[in] use_generic If true, serialized/deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure/class.
 */
dtype_t create_dtype_python(PyObject* pyobj, const bool use_generic);


/*!
  @brief Construct a Direct type object.
  @param[in] use_generic If true, serialized/deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure/class.
*/
dtype_t create_dtype_direct(const bool use_generic);


  
/*!
  @brief Construct a type object for one of the default JSON types.
  @param[in] type Name of the type.
  @param[in] use_generic If true, serialized/deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure/class.
*/
dtype_t create_dtype_default(const char* type,
			     const bool use_generic);


/*!
  @brief Construct a Scalar type object.
  @param[in] subtype Name of the scalar subtype (e.g. int, uint, float,
    bytes).
  @param[in] precision Precision of the scalar in bits.
  @param[in] units Units for scalar. (e.g. "cm", "g", "" for unitless)
  @param[in] use_generic If true, serialized/deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure/class.
*/
dtype_t create_dtype_scalar(const char* subtype, const size_t precision,
			    const char* units, const bool use_generic);


/*!
  @brief Construct a 1D array type object.
  @param[in] subtype Name of the array subtype (e.g. int, uint, float,
    bytes).
  @param[in] precision Precision of the array in bits.
  @param[in] length Number of elements in the array.
  @param[in] units Units for array elements. (e.g. "cm", "g", "" for
    unitless)
  @param[in] use_generic If true, serialized/deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure/class.
*/
dtype_t create_dtype_1darray(const char* subtype, const size_t precision,
			     const size_t length, const char* units,
			     const bool use_generic);


/*!
  @brief Construct a ND array type object.
  @param[in] subtype Name of the array subtype (e.g. int, uint, float,
    bytes).
  @param[in] precision Precision of the array in bits.
  @param[in] ndim Number of dimensions in the array (and therefore also
    the number of elements in shape).
  @param[in] shape Pointer to array where each element is the
    size of the array in that dimension.
  @param[in] units Units for array elements. (e.g. "cm", "g", "" for
    unitless)
  @param[in] use_generic If true, serialized/deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure/class.
*/
dtype_t create_dtype_ndarray(const char* subtype, const size_t precision,
			     const size_t ndim, const size_t* shape,
			     const char* units, const bool use_generic);

  
/*!
  @brief Construct a ND array type object.
  @param[in] subtype Name of the array subtype (e.g. int, uint, float,
    bytes).
  @param[in] precision Precision of the array in bits.
  @param[in] ndim Number of dimensions in the array (and therefore also
    the number of elements in shape).
  @param[in] shape[] Array where each element is the size of the
    array in that dimension.
  @param[in] units Units for array elements. (e.g. "cm", "g", "" for
    unitless)
  @param[in] use_generic If true, serialized/deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure/class.
*/
dtype_t create_dtype_ndarray_arr(const char* subtype, const size_t precision,
				 const size_t ndim, const int64_t shape[],
				 const char* units, const bool use_generic);

  
/*!
  @brief Construct a JSON array type object.
  @param[in] nitems Number of types in items.
  @param[in] items Pointer to array of types describing the array
    elements.
  @param[in] use_generic If true, serialized/deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure/class.
*/
dtype_t create_dtype_json_array(const size_t nitems, dtype_t* items,
				const bool use_generic);


/*!
  @brief Construct a JSON object type object.
  @param[in] nitems Number of keys/types in keys and values.
  @param[in] keys Pointer to array of keys for each type.
  @param[in] values Pointer to array of types describing the values
    for each key.
  @param[in] use_generic If true, serialized/deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure/class.
*/
dtype_t create_dtype_json_object(const size_t nitems, const char** keys,
				 dtype_t* values, const bool use_generic);

/*!
  @brief Construct a Ply type object.
  @param[in] use_generic If true, serialized/deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure/class.
*/
dtype_t create_dtype_ply(const bool use_generic);


/*!
  @brief Construct a Obj type object.
  @param[in] use_generic If true, serialized/deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure/class.
*/
dtype_t create_dtype_obj(const bool use_generic);


/*!
  @brief Construct an AsciiTable type object.
  @param[in] format_str C-style format string that will be used to
    determine the type of elements in arrays that will be
    serialized/deserialized using the resulting type.
  @param[in] as_array If true, the types will be arrays. Otherwise they
    will be scalars.
  @param[in] use_generic If true, serialized/deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure/class.
*/
dtype_t create_dtype_ascii_table(const char *format_str,
				 const bool as_array,
				 const bool use_generic);


/*!
  @brief Construct a type object based on the provided format string.
  @param[in] format_str C-style format string that will be used to
    determine the type of elements in arrays that will be
    serialized/deserialized using the resulting type.
  @param[in] as_array If true, the types will be arrays. Otherwise they
    will be scalars.
  @param[in] use_generic If true, serialized/deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure/class.
*/
dtype_t create_dtype_format(const char *format_str, const bool as_array,
			    const bool use_generic);

  
/*!
  @brief Construct a type object for Python objects.
  @param[in] type Type string.
  @param[in] use_generic If true, serialized/deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure/class.
 */
dtype_t create_dtype_pyobj(const char* type, const bool use_generic);
  

/*!
  @brief Construct a type object for Python object instances.
  @param[in] class_name Python class name that instance should be a
    subclass of. If NULL or an empty string, no class constraints will
    be placed on the instance.
  @param[in] args_dtype Datatype describing the arguments creating the
    instance. The datatype will be consumed and does not need to be freed.
  @param[in] kwargs_dtype Datatype describing the keyword arguments
    creating the instance. The datatype will be consumed and does not
    need to be freed.
  @param[in] use_generic If true, serialized/deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure/class.
 */
dtype_t create_dtype_pyinst(const char* class_name,
			    dtype_t* args_dtype,
			    dtype_t* kwargs_dtype,
			    const bool use_generic);

  
/*!
  @brief Construct a type object for a schema.
  @param[in] use_generic If true, serialized/deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure/class.
 */
dtype_t create_dtype_schema(const bool use_generic);


/*!
  @brief Construct a type object for receiving any type.
  @param[in] use_generic If true, serialized/deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure/class.
 */
dtype_t create_dtype_any(const bool use_generic);


/*!
  @brief Wrapper for freeing rapidjson::Document class.
  @param[in] obj Pointer to rapidjson::Document.
  @returns int 0 if free was successfull, -1 if there was an error.
*/
int destroy_document(void** obj);
  
/*!
  @brief Wrapper for freeing rapidjson::Document class wrapper struct.
  @param[in] dtype Wrapper struct for C++ Metadata.
  @returns int 0 if free was successfull, -1 if there was an error.
*/
int destroy_dtype(dtype_t* dtype);
  
/*!
  @brief Get a copy of a type structure.
  @param[in] dtype Wrapper struct for C++ Metadata.
  @returns: Type class.
*/
dtype_t copy_dtype(const dtype_t dtype);

/*!
  @brief Determine if a type structure indicates that generic objects
    should be used.
  @param[in] dtype Wrapper struct for C++ Metadata.
  @returns 1 if generic objects will be used, 0 if not, -1 for errors.
*/
int dtype_uses_generic(dtype_t dtype);

/*!
  @brief Display a datatype.
  @param[in] dtype Wrapper struct for C++ Metadata.
  @param[in] indent Indentation to add to display output.
 */
void display_dtype(const dtype_t dtype, const char* indent);

/*!
  @brief Wrapper for updating a type object with information from another.
  @param[in] dtype1 struct for C++ Metadata that should be updated.
  @param[in] schema2 C++ rapidjson::Document that should be updated from.
  @returns: int 0 if successfull, -1 if there was an error.
*/
int update_dtype(dtype_t* dtype1, void* schema2);


/*!
  @brief Wrapper for updating the precision of a bytes or unicode scalar
    type.
  @param[in] dtype Wrapper struct for C++ Metadata.
  @param[in] new_precision New precision.
  @returns: int 0 if free was successfull, -1 if there was an error.
*/
int update_precision_dtype(dtype_t* dtype,
                           const size_t new_precision);

#define free_generic destroy_generic
#define init_json_object init_generic_map
#define init_json_array init_generic_array
#define init_schema init_generic
#define free_json_object free_generic
#define free_json_array free_generic
#define free_schema free_generic
#define copy_json_object copy_generic
#define copy_json_array copy_generic
#define copy_schema copy_generic
#define display_json_object display_generic
#define display_json_array display_generic
#define display_schema display_generic

#define GEOM_INTERFACE(name)						\
  name ## _t init_ ## name();						\
  name ## _t generate_ ## name();					\
  void free_ ## name(name ## _t *p);					\
  void set_ ## name(name ## _t* x, void* obj, int copy);		\
  name ## _t copy_ ## name(name ## _t src);				\
  void display_ ## name ## _indent(name ## _t p, const char* indent);	\
  void display_ ## name(name ## _t p);					\
  int nelements_ ## name(name ## _t p, const char* name);		\
  bool compare_ ## name(const name ## _t a, const name ## _t b);

// ObjWavefront wrapped methods

  GEOM_INTERFACE(obj)

/*!
  @fn init_obj
  @brief Initialize empty obj structure.
  @returns obj_t Obj structure.

  @fn generate_obj
  @brief Create a obj structure with generated data.
  @returns obj_t Obj structure.

  @fn set_obj
  @brief Set parameters from a rapidjson::ObjWavefront object.
  @param[in,out] x Structure to modify.
  @param[in] obj rapidjson::ObjWavefront object to copy.
  @param[in] copy If 1, the provided object will be copied, otherwise the
    pointer will be added to the structured directly and it will be freed
    on destruction.

  @fn free_obj
  @brief Free obj structure.
  @param[in] p *obj_t Pointer to obj structure.

  @fn copy_obj
  @brief Copy an obj structure.
  @param[in] src obj_t Obj structure that should be copied.
  @returns Copy of obj structure.

  @fn display_obj_indent
  @brief Display the information contained by an Obj struct.
  @param[in] p obj_t Obj structure.
  @param[in] indent const char* Indentation that should be added to each line.

  @fn display_obj
  @brief Display the information contained by an Obj struct.
  @param[in] p obj_t Obj structure.

  @fn nelements_obj
  @brief Get the number of elements of a certain type in the structure.
  @param[in] p obj_t ObjWavefront structure.
  @param[in] name Name of element type to count.
  @returns Number of elements of the specified type.

  @fn compare_obj
  @brief Compare two obj structures for equality.
  @param[in] a First structure for comparison.
  @param[in] b Second structure for comparison.
  @returns true if a and b are equal, false otherwise.
  
*/
  
// Ply wrapped methods

  GEOM_INTERFACE(ply)

/*!
  @fn init_ply
  @brief Initialize empty ply structure.
  @returns ply_t Ply structure.

  @fn generate_ply
  @brief Create a ply structure with generated data.
  @returns ply_t Ply structure.

  @fn set_ply
  @brief Set parameters from a rapidjson::Ply object.
  @param[in,out] x Structure to modify.
  @param[in] obj rapidjson::Ply object to copy.
  @param[in] copy If 1, the provided object will be copied, otherwise the
    pointer will be added to the structured directly and it will be freed
    on destruction.

  @fn free_ply
  @brief Free ply structure.
  @param[in] p *ply_t Pointer to ply structure.

  @fn copy_ply
  @brief Copy a ply structure.
  @param[in] src ply_t Ply structure that should be copied.
  @returns Copy of ply structure.

  @fn display_ply_indent
  @brief Display the information contained by a Ply struct.
  @param[in] p ply_t Ply structure.
  @param[in] indent const char* Indentation that should be added to each line.

  @fn display_ply
  @brief Display the information contained by a Ply struct.
  @param[in] p ply_t Ply structure.

  @fn nelements_ply
  @brief Get the number of elements of a certain type in the structure.
  @param[in] p ply_t Ply structure.
  @param[in] name Name of element type to count.
  @returns Number of elements of the specified type.

  @fn compare_obj
  @brief Compare two obj structures for equality.
  @param[in] a First structure for comparison.
  @param[in] b Second structure for comparison.
  @returns true if a and b are equal, false otherwise.
  
*/

#undef GEOM_INTERFACE

#ifdef __cplusplus
}
#endif