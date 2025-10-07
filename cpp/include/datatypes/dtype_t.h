#pragma once
#include "YggInterface_export.h"
#include "utils/tools.hpp"
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

/*! @brief C function type */
#ifdef _WIN32
typedef bool (__cdecl *c_function) (generic_t, generic_t);
#else
typedef bool (*c_function) (generic_t, generic_t);
#endif


/*!
  @brief Call a C function pointer.
  @param[in] ptr Function pointer.
  @param[in] data_send Function input data.
  @param[in] data_recv Function output data.
 */
YGG_API bool _call_pointer(void* ptr, generic_t data_send, generic_t data_recv);

/*!
  @brief Get the length of a C string stored in a pointer.
  @param[in] x String pointer.
  @returns Length of the string.
 */
YGG_API size_t pointer_strlen(const void* x);

/*!
  @brief Initialize an empty generic object.
  @returns generic_t New generic object structure.
 */
YGG_API generic_t init_generic();

/*!
  @brief Initialize an empty generic reference object.
  @param[in] parent Generic object that will be the parent of the
    returned reference object.
  @returns New generic reference object structure.
 */
YGG_API generic_ref_t init_generic_ref(generic_t parent);

/*!
  @brief Initialize an empty generic object with a null JSON document
  @returns generic_t New generic object structure.
 */
YGG_API generic_t init_generic_null();
  
/*!
  @brief Initialize an empty array of mixed types with generic wrappers.
  @returns generic_t New generic object structure containing an empty
    array.
*/
YGG_API generic_t init_generic_array();

/*!
  @brief Initialize an empty map (JSON object) of mixed types with
    generic wrappers.
  @returns New generic object structure contaiing an empty map (JSON
    object).
 */
YGG_API generic_t init_generic_map();

/*!
  @brief Initialize a generic object from a JSON string.
  @param[in] json JSON encoded string.
  @returns New generic object structure wrapping a rapidjson::Document
    instance.
 */
YGG_API generic_t init_generic_json(const char* json);

/*!
  @brief Initialize a generic object from a JSON string.
  @param[in] schema JSON encoded schema describing object to generate.
  @returns New generic object structure wrapping a rapidjson::Document
    instance.
 */
YGG_API generic_t init_generic_generate(const char* schema);
  
/*!
  @brief Determine if a generic structure is initialized.
  @param[in] x Generic structure to test.
  @returns 1 if the structure is initialized, 0 otherwise.
 */
YGG_API int is_generic_init(const generic_t x);

/*!
  @brief Determine if a generic reference structure is initialized.
  @param[in] x Generic reference structure to test.
  @returns 1 if the structure is initialized, 0 otherwise.
 */
YGG_API int is_generic_ref_init(const generic_ref_t x);

//
//  @brief Create a generic object from the provided information.
//  @param[in] type_class Type structure..
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
YGG_API int destroy_generic(generic_t* x);


/*!
  @brief Copy data from one generic object into another.
  @param[in,out] dst Pointer to destination object.
  @param[in] src Source object.
  @returns int -1 if unsuccessful, 0 otherwise.
*/
YGG_API int copy_generic_into(generic_t* dst, const generic_t src);


/*!
  @brief Copy data from one generic object to the other.
  @param[in] src generic_t Generic structure that data should be copied from.
  @returns generic_t Copied structure.
 */
YGG_API generic_t copy_generic(const generic_t src);

/*!
  @brief Compare two generic objects.
  @param[in] a First object for comparison.
  @param[in] b Second object for comparison.
  @returns true if the two objects are equivalent, false otherwise.
 */
YGG_API bool compare_generic(const generic_t a, const generic_t b);

/*!
  @brief Display information about the generic type.
  @param[in] x generic_t* Wrapper for generic object.
 */
YGG_API void display_generic(const generic_t x);
    
/*!
  @brief Add an element to the end of an array of generic elements.
  @param[in] arr generic_t Array to add element to.
  @param[in] x generic_t Element to add.
  @returns int Flag that is 1 if there is an error and 0 otherwise.
 */
YGG_API int add_generic_array(generic_t arr, const generic_t x);


/*!
  @brief Set an element in the array at a given index to a new value.
  @param[in] arr generic_t Array to add element to.
  @param[in] i size_t Index where element should be added.
  @param[in] x generic_t Element to add.
  @returns int Flag that is 1 if there is an error and 0 otherwise.
 */
YGG_API int set_generic_array(generic_t arr, const size_t i, const generic_t x);


/*!
  @brief Get an element from an array.
  @param[in] arr generic_t Array to get element from.
  @param[in] i size_t Index of element to get.
  @param[out] x generic_t* Pointer to address where element should be
    stored.
  @returns int Flag that is 1 if there is an error and 0 otherwise.
 */
YGG_API int get_generic_array(const generic_t arr, const size_t i, generic_t *x);
YGG_API int get_generic_array_ref(const generic_t arr, const size_t i, generic_ref_t *x);


/*!
  @brief Set an element in the object at for a given key to a new value.
  @param[in] arr generic_t Object to add element to.
  @param[in] k const char* Key where element should be added.
  @param[in] x generic_t Element to add.
  @returns int Flag that is 1 if there is an error and 0 otherwise.
 */
YGG_API int set_generic_object(generic_t arr, const char* k, const generic_t x);


/*!
  @brief Get an element from an object.
  @param[in] arr generic_t Object to get element from.
  @param[in] k const char* Key of element to return.
  @param[out] x generic_t* Pointer to address where element should be
  stored.
  @returns int Flag that is 1 if there is an error and 0 otherwise.
 */
YGG_API int get_generic_object(const generic_t arr, const char* k, generic_t *x);
YGG_API int get_generic_object_ref(const generic_t arr, const char* k, generic_ref_t *x);


#define set_generic_map set_generic_object
#define get_generic_map get_generic_object
#define get_generic_map_ref get_generic_object_ref

/*!
  @brief Determine if a map object has a certain key.
  @param[in] x generic_t Generic object that is presumed to contain a map.
  @param[in] key char* Key to check for.
  @returns int 1 if the key is present, 0 otherwise.
 */
YGG_API int generic_map_has_key(const generic_t x, const char* key);

/*!
  @brief Get the keys in a map object.
  @param[in] x generic_t Generic object that is presumed to contain a map.
  @param[out] keys char*** Pointer to memory where array of keys should be stored.
  @returns size_t Number of keys in map.
 */
YGG_API size_t generic_map_get_keys(generic_t x, char*** keys);

/**
 * @brief Set the data in the given item to the value given by the json character string
 * @param[in, out] x The item to set
 * @param[in] json The value to set x to
 * @return 0 on success, 1 on error
 */
YGG_API int generic_set_json(generic_t x, const char *json);


/*!
  @brief Initialize Python if it is not initialized.
  @returns int 0 if successful, other values indicate errors.
 */
YGG_API int init_python_API();
  
/*!
  @brief Initialize a Python wrapper object.
  @returns Initialized object.
 */
YGG_API python_t init_python();
  
/*!
  @brief Destroy a structure containing a Python object.
  @param[in] x Pointer to Python object structure that should be freed.
*/
YGG_API void destroy_python(python_t *x);


/*!
  @brief Copy a Python object structure (NOTE: this dosn't copy the
    underlying Python object but does increment the reference count).
  @param[in] x Structure containing Python object to copy.
  @returns python_t Copy of x.
 */
YGG_API python_t copy_python(python_t x);


/*!
  @brief Display a Python object structure.
  @param[in] x Structure containing Python object to display.
 */
YGG_API void display_python(python_t x);

  
/*!
  @brief Determine if a datatype is empty.
  @param[in] dtype structure to test.
  @returns int 1 if dtype is empty, 0 otherwise.
 */
YGG_API int is_empty_dtype(const dtype_t dtype);

  
/*!
  @brief Determine if a datatype was created from a format.
  @param[in] type_struct Datatype structure.
  @returns 1 if the datatype was created from a format, 0 if it was not,
    -1 if there is an error.
 */
YGG_API int is_dtype_format_array(const dtype_t type_struct);
  
  
/*!
  @brief Get the name of the type from the class.
  @param[in] type_class Type structure..
  @returns Type name.
*/
YGG_API const char* dtype_name(const dtype_t type_class);


/*!
  @brief Get the subtype of the type.
  @param[in] type_class Type structure..
  @returns The subtype of the class, "" if there is an error.
*/
YGG_API const char* dtype_subtype(const dtype_t type_class);


/*!
  @brief Compare two datatypes structures.
  @param[in] a First datatype for comparison
  @param[in] b Second datatype for comparison
*/
YGG_API bool compare_dtype(const dtype_t a, const dtype_t b);

/*!
  @brief Get the precision of the type.
  @param[in] type_class Type structure..
  @returns The precision of the class, 0 if there is an error.
*/
YGG_API size_t dtype_precision(const dtype_t type_class);


/*!
  @brief Set the type name in the datatype structure.
  @param[in,out] dtype Datatype structure to update. It must have
    been initialized.
  @param[in] name Type name to set in dtype.
  @returns 0 on success, -1 if there is an error.
 */
YGG_API int set_dtype_name(dtype_t dtype, const char* name);

  
/*!
  @brief Initialize a datatype structure including setting the type string.
  @param[in] dtype Type structure.
  @param[in] use_generic If true, serialized or deserialized
    objects will be expected to be generic_t instances.
  @returns Initialized type structure.
*/
YGG_API dtype_t complete_dtype(dtype_t dtype, const bool use_generic);


/*!
  @brief Construct a type object from a JSON schema.
  @param[in] schema Serialized JSON schema.
  @param[in] use_generic If true, serialized or deserialized objects will
    be expected to be generic_t instances.
  @returns Type structure.
 */
YGG_API dtype_t create_dtype_from_schema(const char* schema,
					 const bool use_generic);


/*!
  @brief Construct and empty type object.
  @param[in] use_generic If true, serialized or deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure.
*/
YGG_API dtype_t create_dtype_empty(const bool use_generic);


/*!
  @brief Create a datatype based on a Python dictionary.
  @param[in] pyobj Python dictionary.
  @param[in] use_generic If true, serialized or deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure.
 */
YGG_API dtype_t create_dtype_python(PyObject* pyobj, const bool use_generic);


/*!
  @brief Construct a Direct type object.
  @param[in] use_generic If true, serialized or deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure.
*/
YGG_API dtype_t create_dtype_direct(const bool use_generic);


  
/*!
  @brief Construct a type object for one of the default JSON types.
  @param[in] type Name of the type.
  @param[in] use_generic If true, serialized or deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure.
*/
YGG_API dtype_t create_dtype_default(const char* type,
				     const bool use_generic);


/*!
  @brief Construct a Scalar type object.
  @param[in] subtype Name of the scalar subtype (e.g. int, uint, float,
    bytes).
  @param[in] precision Precision of the scalar in bits.
  @param[in] units Units for scalar. (e.g. "cm", "g", "" for unitless)
  @param[in] use_generic If true, serialized or deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure.
*/
YGG_API dtype_t create_dtype_scalar(const char* subtype,
				    const size_t precision,
				    const char* units,
				    const bool use_generic);


/*!
  @brief Construct a 1D array type object.
  @param[in] subtype Name of the array subtype (e.g. int, uint, float,
    bytes).
  @param[in] precision Precision of the array in bits.
  @param[in] length Number of elements in the array.
  @param[in] units Units for array elements. (e.g. "cm", "g", "" for
    unitless)
  @param[in] use_generic If true, serialized or deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure.
*/
YGG_API dtype_t create_dtype_1darray(const char* subtype,
				     const size_t precision,
				     const size_t length,
				     const char* units,
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
  @param[in] use_generic If true, serialized or deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure.
*/
YGG_API dtype_t create_dtype_ndarray(const char* subtype,
				     const size_t precision,
				     const size_t ndim,
				     const size_t* shape,
				     const char* units,
				     const bool use_generic);

  
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
  @param[in] use_generic If true, serialized or deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure.
*/
YGG_API dtype_t create_dtype_ndarray_arr(const char* subtype,
					 const size_t precision,
					 const size_t ndim,
					 const int64_t shape[],
					 const char* units,
					 const bool use_generic);

  
/*!
  @brief Construct a JSON array type object.
  @param[in] nitems Number of types in items.
  @param[in] items Pointer to array of types describing the array
    elements.
  @param[in] use_generic If true, serialized or deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure.
*/
YGG_API dtype_t create_dtype_json_array(const size_t nitems,
					dtype_t* items,
					const bool use_generic);


/*!
  @brief Construct a JSON object type object.
  @param[in] nitems Number of items in keys and values.
  @param[in] keys Pointer to array of keys for each type.
  @param[in] values Pointer to array of types describing the values
    for each key.
  @param[in] use_generic If true, serialized or deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure.
*/
YGG_API dtype_t create_dtype_json_object(const size_t nitems,
					 const char** keys,
					 dtype_t* values,
					 const bool use_generic);

/*!
  @brief Construct a Ply type object.
  @param[in] use_generic If true, serialized or deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure.
*/
YGG_API dtype_t create_dtype_ply(const bool use_generic);


/*!
  @brief Construct a Obj type object.
  @param[in] use_generic If true, serialized or deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure.
*/
YGG_API dtype_t create_dtype_obj(const bool use_generic);


/*!
  @brief Construct an AsciiTable type object.
  @param[in] format_str C-style format string that will be used to
    determine the type of elements in arrays that will be
    serialized or deserialized using the resulting type.
  @param[in] as_array If true, the types will be arrays. Otherwise they
    will be scalars.
  @param[in] use_generic If true, serialized or deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure.
*/
YGG_API dtype_t create_dtype_ascii_table(const char *format_str,
					 const bool as_array,
					 const bool use_generic);


/*!
  @brief Construct a type object based on the provided format string.
  @param[in] format_str C-style format string that will be used to
    determine the type of elements in arrays that will be
    serialized or deserialized using the resulting type.
  @param[in] as_array If true, the types will be arrays. Otherwise they
    will be scalars.
  @param[in] use_generic If true, serialized or deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure.
*/
YGG_API dtype_t create_dtype_format(const char *format_str,
				    const bool as_array,
				    const bool use_generic);

  
/*!
  @brief Construct a type object for Python objects.
  @param[in] type Type string.
  @param[in] use_generic If true, serialized or deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure.
 */
YGG_API dtype_t create_dtype_pyobj(const char* type,
				   const bool use_generic);
  

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
  @param[in] use_generic If true, serialized or deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure.
 */
YGG_API dtype_t create_dtype_pyinst(const char* class_name,
				    dtype_t* args_dtype,
				    dtype_t* kwargs_dtype,
				    const bool use_generic);

  
/*!
  @brief Construct a type object for a schema.
  @param[in] use_generic If true, serialized or deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure.
 */
YGG_API dtype_t create_dtype_schema(const bool use_generic);


/*!
  @brief Construct a type object for receiving any type.
  @param[in] use_generic If true, serialized or deserialized
    objects will be expected to be generic_t instances.
  @returns Type structure.
 */
YGG_API dtype_t create_dtype_any(const bool use_generic);


/*
  @brief Wrapper for freeing rapidjson::Document class.
  @param[in] obj Pointer to rapidjson::Document.
  @returns int 0 if free was successfull, -1 if there was an error.
*/
/* YGG_API int destroy_document(void** obj); */
  
/*!
  @brief Wrapper for freeing rapidjson::Document class wrapper struct.
  @param[in] dtype Wrapper struct for C++ Metadata.
  @returns int 0 if free was successfull, -1 if there was an error.
*/
YGG_API int destroy_dtype(dtype_t* dtype);
  
/*!
  @brief Get a copy of a type structure.
  @param[in] dtype Wrapper struct for C++ Metadata.
  @returns: Type class.
*/
YGG_API dtype_t copy_dtype(const dtype_t dtype);

/*!
  @brief Determine if a type structure indicates that generic objects
    should be used.
  @param[in] dtype Wrapper struct for C++ Metadata.
  @returns 1 if generic objects will be used, 0 if not, -1 for errors.
*/
YGG_API int dtype_uses_generic(dtype_t dtype);

/*!
  @brief Display a datatype.
  @param[in] dtype Wrapper struct for C++ Metadata.
  @param[in] indent Indentation to add to display output.
 */
YGG_API void display_dtype(const dtype_t dtype, const char* indent);

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

  // ObjWavefront wrapped methods

  /**
   * @brief Initialize empty obj structure.
   * @returns obj_t Obj structure.
   */
  YGG_API obj_t init_obj();
  /**
   * @brief Create a obj structure with generated data.
   * @returns obj_t Obj structure.
   */
  YGG_API obj_t generate_obj();
  /**
   * @brief Free obj structure.
   * @param[in] p *obj_t Pointer to obj structure.
   */
  YGG_API void free_obj(obj_t *p);
  /**
   * @brief Set parameters from a rapidjson::ObjWavefront object.
   * @param[in,out] x Structure to modify.
   * @param[in] obj rapidjson::ObjWavefront object to copy.
   * @param[in] copy If 1, the provided object will be copied, otherwise
   *   the pointer will be added to the structured directly and it will
   *   be freed on destruction.
   */
  YGG_API void set_obj(obj_t* x, void* obj, int copy);
  /**
   * @brief Copy an obj structure.
   * @param[in] src obj_t Obj structure that should be copied.
   * @returns Copy of obj structure.
   */
  YGG_API obj_t copy_obj(obj_t src);
  /**
   * @brief Display the information contained by an Obj struct.
   * @param[in] p obj_t Obj structure.
   * @param[in] indent const char* Indentation that should be added to
   *   each line.
   */
  YGG_API void display_obj_indent(obj_t p, const char* indent);
  /**
   * @brief Display the information contained by an Obj struct.
   * @param[in] p obj_t Obj structure.
   */
  YGG_API void display_obj(obj_t p);
  /**
   * @brief Get the number of elements of a certain type in the structure.
   * @param[in] p obj_t ObjWavefront structure.
   * @param[in] name Name of element type to count.
   * @returns Number of elements of the specified type.
   */
  YGG_API int nelements_obj(obj_t p, const char* name);
  /**
   * @brief Compare two obj structures for equality.
   * @param[in] a First structure for comparison.
   * @param[in] b Second structure for comparison.
   * @returns true if a and b are equal, false otherwise.
   */
  YGG_API bool compare_obj(const obj_t a, const obj_t b);

// Ply wrapped methods

  /**
   * @brief Initialize empty ply structure.
   * @returns ply_t Ply structure.
   */
  YGG_API ply_t init_ply();
  /**
   * @brief Create a ply structure with generated data.
   * @returns ply_t Ply structure.
   */
  YGG_API ply_t generate_ply();
  /**
   * @brief Free ply structure.
   * @param[in] p *ply_t Pointer to ply structure.
   */
  YGG_API void free_ply(ply_t *p);
  /**
   * @brief Set parameters from a rapidjson::Ply object.
   * @param[in,out] x Structure to modify.
   * @param[in] ply rapidjson::Ply object to copy.
   * @param[in] copy If 1, the provided object will be copied, otherwise
   *   the pointer will be added to the structured directly and it will
   *   be freed on destruction.
   */
  YGG_API void set_ply(ply_t* x, void* ply, int copy);
  /**
   * @brief Copy an ply structure.
   * @param[in] src ply_t Ply structure that should be copied.
   * @returns Copy of ply structure.
   */
  YGG_API ply_t copy_ply(ply_t src);
  /**
   * @brief Display the information contained by an Ply struct.
   * @param[in] p ply_t Ply structure.
   * @param[in] indent const char* Indentation that should be added to
   *   each line.
   */
  YGG_API void display_ply_indent(ply_t p, const char* indent);
  /**
   * @brief Display the information contained by an Ply struct.
   * @param[in] p ply_t Ply structure.
   */
  YGG_API void display_ply(ply_t p);
  /**
   * @brief Get the number of elements of a certain type in the structure.
   * @param[in] p ply_t Ply structure.
   * @param[in] name Name of element type to count.
   * @returns Number of elements of the specified type.
   */
  YGG_API int nelements_ply(ply_t p, const char* name);
  /**
   * @brief Compare two ply structures for equality.
   * @param[in] a First structure for comparison.
   * @param[in] b Second structure for comparison.
   * @returns true if a and b are equal, false otherwise.
   */
  YGG_API bool compare_ply(const ply_t a, const ply_t b);


// LINES AFTER THIS WERE GENERATED AND SHOULD NOT BE MODIFIED DIRECTLY
//====================================================================
/**
 * @brief Get the number of elements in a array
 * @param[in] x generic_t Generic object that is presumed to contain a
 *   array
 * @returns size_t Number of elements in array
 */
YGG_API size_t generic_array_get_size(generic_t x);
/**
 * @brief Get the number of elements in a object
 * @param[in] x generic_t Generic object that is presumed to contain a
 *   object
 * @returns size_t Number of elements in object
 */
YGG_API size_t generic_object_get_size(generic_t x);
#define generic_map_get_size generic_object_get_size
/**
 * @brief Set a given generic item to a null
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_null(generic_t x, const void* value);
/**
 * @brief Set a given generic item to a boolean
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_bool(generic_t x, const bool value);
/**
 * @brief Set a given generic item to a integer
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_integer(generic_t x, const int value);
/**
 * @brief Set a given generic item to a number
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_number(generic_t x, const double value);
/**
 * @brief Set a given generic item to a string
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_string(generic_t x, const char* value);
/**
 * @brief Set a given generic item to a item
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_item(generic_t x, const char* type, void* value);
/**
 * @brief Set a given generic item to a array
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_array(generic_t x, const generic_t value);
/**
 * @brief Set a given generic item to a object
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_object(generic_t x, const generic_t value);
#define generic_set_map generic_set_object
/**
 * @brief Set a given generic item to a ply
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_ply(generic_t x, const ply_t value);
/**
 * @brief Set a given generic item to a obj
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_obj(generic_t x, const obj_t value);
/**
 * @brief Set a given generic item to a class
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_python_class(generic_t x, const python_t value);
/**
 * @brief Set a given generic item to a function
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_python_function(generic_t x, const python_t value);
/**
 * @brief Set a given generic item to a instance
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_python_instance(generic_t x, const python_t value);
/**
 * @brief Set a given generic item to a scalar
 * @param[in] x The generic item to set
 * @param[in] value Pointer to the memory containing the value to assign to x
 * @param[in] subtype Subtype of data contained in value
 * @param[in] precision The precision of the data in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_scalar(generic_t x, const void* value, const char* subtype, const size_t precision, const char* units);
/**
 * @brief Set a given generic item to a int scalar
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_int8(generic_t x, const int8_t value, const char* units);
/**
 * @brief Set a given generic item to a int scalar
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_int16(generic_t x, const int16_t value, const char* units);
/**
 * @brief Set a given generic item to a int scalar
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_int32(generic_t x, const int32_t value, const char* units);
/**
 * @brief Set a given generic item to a int scalar
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_int64(generic_t x, const int64_t value, const char* units);
/**
 * @brief Set a given generic item to a uint scalar
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_uint8(generic_t x, const uint8_t value, const char* units);
/**
 * @brief Set a given generic item to a uint scalar
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_uint16(generic_t x, const uint16_t value, const char* units);
/**
 * @brief Set a given generic item to a uint scalar
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_uint32(generic_t x, const uint32_t value, const char* units);
/**
 * @brief Set a given generic item to a uint scalar
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_uint64(generic_t x, const uint64_t value, const char* units);
/**
 * @brief Set a given generic item to a float scalar
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_float(generic_t x, const float value, const char* units);
/**
 * @brief Set a given generic item to a float scalar
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_double(generic_t x, const double value, const char* units);
/**
 * @brief Set a given generic item to a complex scalar
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_complex_float(generic_t x, const complex_float_t value, const char* units);
/**
 * @brief Set a given generic item to a complex scalar
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_complex_double(generic_t x, const complex_double_t value, const char* units);
/**
 * @brief Set a given generic item to a 1darray
 * @param[in] x The generic item to set
 * @param[in] value Pointer to the memory containing the array to assign
 *   to x
 * @param[in] subtype Subtype of data contained in value
 * @param[in] precision The precision of the elements in value
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_1darray(generic_t x, const void* value, const char* subtype, const size_t precision, const size_t length, const char* units);
/**
 * @brief Set a given generic item to a int 1darray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_1darray_int8(generic_t x, const int8_t* value, const size_t length, const char* units);
/**
 * @brief Set a given generic item to a int 1darray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_1darray_int16(generic_t x, const int16_t* value, const size_t length, const char* units);
/**
 * @brief Set a given generic item to a int 1darray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_1darray_int32(generic_t x, const int32_t* value, const size_t length, const char* units);
/**
 * @brief Set a given generic item to a int 1darray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_1darray_int64(generic_t x, const int64_t* value, const size_t length, const char* units);
/**
 * @brief Set a given generic item to a uint 1darray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_1darray_uint8(generic_t x, const uint8_t* value, const size_t length, const char* units);
/**
 * @brief Set a given generic item to a uint 1darray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_1darray_uint16(generic_t x, const uint16_t* value, const size_t length, const char* units);
/**
 * @brief Set a given generic item to a uint 1darray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_1darray_uint32(generic_t x, const uint32_t* value, const size_t length, const char* units);
/**
 * @brief Set a given generic item to a uint 1darray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_1darray_uint64(generic_t x, const uint64_t* value, const size_t length, const char* units);
/**
 * @brief Set a given generic item to a float 1darray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_1darray_float(generic_t x, const float* value, const size_t length, const char* units);
/**
 * @brief Set a given generic item to a float 1darray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_1darray_double(generic_t x, const double* value, const size_t length, const char* units);
/**
 * @brief Set a given generic item to a complex 1darray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_1darray_complex_float(generic_t x, const complex_float_t* value, const size_t length, const char* units);
/**
 * @brief Set a given generic item to a complex 1darray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_1darray_complex_double(generic_t x, const complex_double_t* value, const size_t length, const char* units);
/**
 * @brief Set a given generic item to a ndarray
 * @param[in] x The generic item to set
 * @param[in] value Pointer to the memory containing the array to assign
 *   to x
 * @param[in] subtype Subtype of data contained in value
 * @param[in] precision The precision of the elements in value
 * @param[in] ndim The number of dimensions in value
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_ndarray(generic_t x, const void* value, const char* subtype, const size_t precision, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set a given generic item to a int ndarray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_ndarray_int8(generic_t x, const int8_t* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set a given generic item to a int ndarray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_ndarray_int16(generic_t x, const int16_t* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set a given generic item to a int ndarray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_ndarray_int32(generic_t x, const int32_t* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set a given generic item to a int ndarray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_ndarray_int64(generic_t x, const int64_t* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set a given generic item to a uint ndarray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_ndarray_uint8(generic_t x, const uint8_t* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set a given generic item to a uint ndarray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_ndarray_uint16(generic_t x, const uint16_t* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set a given generic item to a uint ndarray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_ndarray_uint32(generic_t x, const uint32_t* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set a given generic item to a uint ndarray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_ndarray_uint64(generic_t x, const uint64_t* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set a given generic item to a float ndarray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_ndarray_float(generic_t x, const float* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set a given generic item to a float ndarray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_ndarray_double(generic_t x, const double* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set a given generic item to a complex ndarray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_ndarray_complex_float(generic_t x, const complex_float_t* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set a given generic item to a complex ndarray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_ndarray_complex_double(generic_t x, const complex_double_t* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set a given generic item to a schema
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_schema(generic_t x, const generic_t value);
/**
 * @brief Set a given generic item to a any
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_any(generic_t x, const generic_t value);
/**
 * @brief Get a null from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API void* generic_get_null(generic_t x);
/**
 * @brief Get a boolean from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API bool generic_get_bool(generic_t x);
/**
 * @brief Get a integer from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API int generic_get_integer(generic_t x);
/**
 * @brief Get a number from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API double generic_get_number(generic_t x);
/**
 * @brief Get a string from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API const char* generic_get_string(generic_t x);
/**
 * @brief Get the raw item data
 * @param[in] x Generic item to retrieve data from
 * @param[in] type Type of item to retrieve
 * @returns Pointer to data containing raw item data, NULL on error
 */
YGG_API void* generic_get_item(generic_t x, const char* type);
/**
 * @brief Get the size of the raw item data
 * @param[in] x Generic item to retrieve data size from
 * @param[in] type Type of item to retrieve
 * @returns Number of bytes in raw item data, 0 on error
 */
YGG_API int generic_get_item_nbytes(generic_t x, const char* type);
/**
 * @brief Get a array from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API generic_t generic_get_array(generic_t x);
/**
 * @brief Get a object from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API generic_t generic_get_object(generic_t x);
#define generic_get_map generic_get_object
/**
 * @brief Get a ply from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API ply_t generic_get_ply(generic_t x);
/**
 * @brief Get a obj from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API obj_t generic_get_obj(generic_t x);
/**
 * @brief Get a class from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API python_t generic_get_python_class(generic_t x);
/**
 * @brief Get a function from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API python_t generic_get_python_function(generic_t x);
/**
 * @brief Get a instance from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API python_t generic_get_python_instance(generic_t x);
/**
 * @brief Get a scalar from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[in] subtype Subtype of data to return
 * @param[in] precision Precision of the data to return
 * @returns Pointer to value in x
 */
YGG_API void* generic_get_scalar(generic_t x, const char* subtype, const size_t precision);
/**
 * @brief Get a int scalar from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API int8_t generic_get_int8(generic_t x);
/**
 * @brief Get a int scalar from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API int16_t generic_get_int16(generic_t x);
/**
 * @brief Get a int scalar from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API int32_t generic_get_int32(generic_t x);
/**
 * @brief Get a int scalar from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API int64_t generic_get_int64(generic_t x);
/**
 * @brief Get a uint scalar from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API uint8_t generic_get_uint8(generic_t x);
/**
 * @brief Get a uint scalar from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API uint16_t generic_get_uint16(generic_t x);
/**
 * @brief Get a uint scalar from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API uint32_t generic_get_uint32(generic_t x);
/**
 * @brief Get a uint scalar from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API uint64_t generic_get_uint64(generic_t x);
/**
 * @brief Get a float scalar from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API float generic_get_float(generic_t x);
/**
 * @brief Get a float scalar from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API double generic_get_double(generic_t x);
/**
 * @brief Get a complex scalar from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API complex_float_t generic_get_complex_float(generic_t x);
/**
 * @brief Get a complex scalar from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API complex_double_t generic_get_complex_double(generic_t x);
/**
 * @brief Get a 1darray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[in] subtype Subtype of data to return
 * @param[in] precision Precision of the data to return
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_get_1darray(generic_t x, const char* subtype, const size_t precision, void** value);
/**
 * @brief Get a int 1darray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_get_1darray_int8(generic_t x, int8_t** value);
/**
 * @brief Get a int 1darray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_get_1darray_int16(generic_t x, int16_t** value);
/**
 * @brief Get a int 1darray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_get_1darray_int32(generic_t x, int32_t** value);
/**
 * @brief Get a int 1darray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_get_1darray_int64(generic_t x, int64_t** value);
/**
 * @brief Get a uint 1darray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_get_1darray_uint8(generic_t x, uint8_t** value);
/**
 * @brief Get a uint 1darray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_get_1darray_uint16(generic_t x, uint16_t** value);
/**
 * @brief Get a uint 1darray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_get_1darray_uint32(generic_t x, uint32_t** value);
/**
 * @brief Get a uint 1darray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_get_1darray_uint64(generic_t x, uint64_t** value);
/**
 * @brief Get a float 1darray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_get_1darray_float(generic_t x, float** value);
/**
 * @brief Get a float 1darray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_get_1darray_double(generic_t x, double** value);
/**
 * @brief Get a complex 1darray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_get_1darray_complex_float(generic_t x, complex_float_t** value);
/**
 * @brief Get a complex 1darray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_get_1darray_complex_double(generic_t x, complex_double_t** value);
/**
 * @brief Get a ndarray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[in] subtype Subtype of data to return
 * @param[in] precision Precision of the data to return
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_get_ndarray(generic_t x, const char* subtype, const size_t precision, void** value, size_t** shape);
/**
 * @brief Get a int ndarray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_get_ndarray_int8(generic_t x, int8_t** value, size_t** shape);
/**
 * @brief Get a int ndarray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_get_ndarray_int16(generic_t x, int16_t** value, size_t** shape);
/**
 * @brief Get a int ndarray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_get_ndarray_int32(generic_t x, int32_t** value, size_t** shape);
/**
 * @brief Get a int ndarray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_get_ndarray_int64(generic_t x, int64_t** value, size_t** shape);
/**
 * @brief Get a uint ndarray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_get_ndarray_uint8(generic_t x, uint8_t** value, size_t** shape);
/**
 * @brief Get a uint ndarray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_get_ndarray_uint16(generic_t x, uint16_t** value, size_t** shape);
/**
 * @brief Get a uint ndarray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_get_ndarray_uint32(generic_t x, uint32_t** value, size_t** shape);
/**
 * @brief Get a uint ndarray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_get_ndarray_uint64(generic_t x, uint64_t** value, size_t** shape);
/**
 * @brief Get a float ndarray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_get_ndarray_float(generic_t x, float** value, size_t** shape);
/**
 * @brief Get a float ndarray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_get_ndarray_double(generic_t x, double** value, size_t** shape);
/**
 * @brief Get a complex ndarray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_get_ndarray_complex_float(generic_t x, complex_float_t** value, size_t** shape);
/**
 * @brief Get a complex ndarray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_get_ndarray_complex_double(generic_t x, complex_double_t** value, size_t** shape);
/**
 * @brief Get a schema from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API generic_t generic_get_schema(generic_t x);
/**
 * @brief Get a any from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API generic_t generic_get_any(generic_t x);
/**
 * @brief Get a null from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API void* generic_ref_get_null(generic_ref_t x);
/**
 * @brief Get a boolean from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API bool generic_ref_get_bool(generic_ref_t x);
/**
 * @brief Get a integer from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API int generic_ref_get_integer(generic_ref_t x);
/**
 * @brief Get a number from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API double generic_ref_get_number(generic_ref_t x);
/**
 * @brief Get a string from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API const char* generic_ref_get_string(generic_ref_t x);
/**
 * @brief Get the raw item data
 * @param[in] x Generic item to retrieve data from
 * @param[in] type Type of item to retrieve
 * @returns Pointer to data containing raw item data, NULL on error
 */
YGG_API void* generic_ref_get_item(generic_ref_t x, const char* type);
/**
 * @brief Get the size of the raw item data
 * @param[in] x Generic item to retrieve data size from
 * @param[in] type Type of item to retrieve
 * @returns Number of bytes in raw item data, 0 on error
 */
YGG_API int generic_ref_get_item_nbytes(generic_ref_t x, const char* type);
/**
 * @brief Get a array from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API generic_t generic_ref_get_array(generic_ref_t x);
/**
 * @brief Get a object from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API generic_t generic_ref_get_object(generic_ref_t x);
#define generic_ref_get_map generic_ref_get_object
/**
 * @brief Get a ply from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API ply_t generic_ref_get_ply(generic_ref_t x);
/**
 * @brief Get a obj from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API obj_t generic_ref_get_obj(generic_ref_t x);
/**
 * @brief Get a class from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API python_t generic_ref_get_python_class(generic_ref_t x);
/**
 * @brief Get a function from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API python_t generic_ref_get_python_function(generic_ref_t x);
/**
 * @brief Get a instance from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API python_t generic_ref_get_python_instance(generic_ref_t x);
/**
 * @brief Get a scalar from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[in] subtype Subtype of data to return
 * @param[in] precision Precision of the data to return
 * @returns Pointer to value in x
 */
YGG_API void* generic_ref_get_scalar(generic_ref_t x, const char* subtype, const size_t precision);
/**
 * @brief Get a int scalar from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API int8_t generic_ref_get_int8(generic_ref_t x);
/**
 * @brief Get a int scalar from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API int16_t generic_ref_get_int16(generic_ref_t x);
/**
 * @brief Get a int scalar from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API int32_t generic_ref_get_int32(generic_ref_t x);
/**
 * @brief Get a int scalar from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API int64_t generic_ref_get_int64(generic_ref_t x);
/**
 * @brief Get a uint scalar from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API uint8_t generic_ref_get_uint8(generic_ref_t x);
/**
 * @brief Get a uint scalar from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API uint16_t generic_ref_get_uint16(generic_ref_t x);
/**
 * @brief Get a uint scalar from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API uint32_t generic_ref_get_uint32(generic_ref_t x);
/**
 * @brief Get a uint scalar from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API uint64_t generic_ref_get_uint64(generic_ref_t x);
/**
 * @brief Get a float scalar from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API float generic_ref_get_float(generic_ref_t x);
/**
 * @brief Get a float scalar from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API double generic_ref_get_double(generic_ref_t x);
/**
 * @brief Get a complex scalar from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API complex_float_t generic_ref_get_complex_float(generic_ref_t x);
/**
 * @brief Get a complex scalar from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API complex_double_t generic_ref_get_complex_double(generic_ref_t x);
/**
 * @brief Get a 1darray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[in] subtype Subtype of data to return
 * @param[in] precision Precision of the data to return
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_1darray(generic_ref_t x, const char* subtype, const size_t precision, void** value);
/**
 * @brief Get a int 1darray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_1darray_int8(generic_ref_t x, int8_t** value);
/**
 * @brief Get a int 1darray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_1darray_int16(generic_ref_t x, int16_t** value);
/**
 * @brief Get a int 1darray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_1darray_int32(generic_ref_t x, int32_t** value);
/**
 * @brief Get a int 1darray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_1darray_int64(generic_ref_t x, int64_t** value);
/**
 * @brief Get a uint 1darray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_1darray_uint8(generic_ref_t x, uint8_t** value);
/**
 * @brief Get a uint 1darray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_1darray_uint16(generic_ref_t x, uint16_t** value);
/**
 * @brief Get a uint 1darray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_1darray_uint32(generic_ref_t x, uint32_t** value);
/**
 * @brief Get a uint 1darray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_1darray_uint64(generic_ref_t x, uint64_t** value);
/**
 * @brief Get a float 1darray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_1darray_float(generic_ref_t x, float** value);
/**
 * @brief Get a float 1darray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_1darray_double(generic_ref_t x, double** value);
/**
 * @brief Get a complex 1darray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_1darray_complex_float(generic_ref_t x, complex_float_t** value);
/**
 * @brief Get a complex 1darray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_1darray_complex_double(generic_ref_t x, complex_double_t** value);
/**
 * @brief Get a ndarray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[in] subtype Subtype of data to return
 * @param[in] precision Precision of the data to return
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_ndarray(generic_ref_t x, const char* subtype, const size_t precision, void** value, size_t** shape);
/**
 * @brief Get a int ndarray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_ndarray_int8(generic_ref_t x, int8_t** value, size_t** shape);
/**
 * @brief Get a int ndarray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_ndarray_int16(generic_ref_t x, int16_t** value, size_t** shape);
/**
 * @brief Get a int ndarray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_ndarray_int32(generic_ref_t x, int32_t** value, size_t** shape);
/**
 * @brief Get a int ndarray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_ndarray_int64(generic_ref_t x, int64_t** value, size_t** shape);
/**
 * @brief Get a uint ndarray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_ndarray_uint8(generic_ref_t x, uint8_t** value, size_t** shape);
/**
 * @brief Get a uint ndarray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_ndarray_uint16(generic_ref_t x, uint16_t** value, size_t** shape);
/**
 * @brief Get a uint ndarray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_ndarray_uint32(generic_ref_t x, uint32_t** value, size_t** shape);
/**
 * @brief Get a uint ndarray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_ndarray_uint64(generic_ref_t x, uint64_t** value, size_t** shape);
/**
 * @brief Get a float ndarray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_ndarray_float(generic_ref_t x, float** value, size_t** shape);
/**
 * @brief Get a float ndarray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_ndarray_double(generic_ref_t x, double** value, size_t** shape);
/**
 * @brief Get a complex ndarray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_ndarray_complex_float(generic_ref_t x, complex_float_t** value, size_t** shape);
/**
 * @brief Get a complex ndarray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_ndarray_complex_double(generic_ref_t x, complex_double_t** value, size_t** shape);
/**
 * @brief Get a schema from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API generic_t generic_ref_get_schema(generic_ref_t x);
/**
 * @brief Get a any from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API generic_t generic_ref_get_any(generic_ref_t x);
/**
 * @brief Set an element in a array to a null
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_null(generic_t x, const size_t index, const void* value);
/**
 * @brief Set an element in a array to a boolean
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_bool(generic_t x, const size_t index, const bool value);
/**
 * @brief Set an element in a array to a integer
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_integer(generic_t x, const size_t index, const int value);
/**
 * @brief Set an element in a array to a number
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_number(generic_t x, const size_t index, const double value);
/**
 * @brief Set an element in a array to a string
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_string(generic_t x, const size_t index, const char* value);
/**
 * @brief Set an element in a array to a item
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_item(generic_t x, const size_t index, const char* type, void* value);
/**
 * @brief Set an element in a array to a array
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_array(generic_t x, const size_t index, const generic_t value);
/**
 * @brief Set an element in a array to a object
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_object(generic_t x, const size_t index, const generic_t value);
#define generic_array_set_map generic_array_set_object
/**
 * @brief Set an element in a array to a ply
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_ply(generic_t x, const size_t index, const ply_t value);
/**
 * @brief Set an element in a array to a obj
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_obj(generic_t x, const size_t index, const obj_t value);
/**
 * @brief Set an element in a array to a class
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_python_class(generic_t x, const size_t index, const python_t value);
/**
 * @brief Set an element in a array to a function
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_python_function(generic_t x, const size_t index, const python_t value);
/**
 * @brief Set an element in a array to a instance
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_python_instance(generic_t x, const size_t index, const python_t value);
/**
 * @brief Set an element in a array to a scalar
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value Pointer to the memory containing the value to assign to x
 * @param[in] subtype Subtype of data contained in value
 * @param[in] precision The precision of the data in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_scalar(generic_t x, const size_t index, const void* value, const char* subtype, const size_t precision, const char* units);
/**
 * @brief Set an element in a array to a int scalar
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_int8(generic_t x, const size_t index, const int8_t value, const char* units);
/**
 * @brief Set an element in a array to a int scalar
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_int16(generic_t x, const size_t index, const int16_t value, const char* units);
/**
 * @brief Set an element in a array to a int scalar
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_int32(generic_t x, const size_t index, const int32_t value, const char* units);
/**
 * @brief Set an element in a array to a int scalar
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_int64(generic_t x, const size_t index, const int64_t value, const char* units);
/**
 * @brief Set an element in a array to a uint scalar
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_uint8(generic_t x, const size_t index, const uint8_t value, const char* units);
/**
 * @brief Set an element in a array to a uint scalar
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_uint16(generic_t x, const size_t index, const uint16_t value, const char* units);
/**
 * @brief Set an element in a array to a uint scalar
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_uint32(generic_t x, const size_t index, const uint32_t value, const char* units);
/**
 * @brief Set an element in a array to a uint scalar
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_uint64(generic_t x, const size_t index, const uint64_t value, const char* units);
/**
 * @brief Set an element in a array to a float scalar
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_float(generic_t x, const size_t index, const float value, const char* units);
/**
 * @brief Set an element in a array to a float scalar
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_double(generic_t x, const size_t index, const double value, const char* units);
/**
 * @brief Set an element in a array to a complex scalar
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_complex_float(generic_t x, const size_t index, const complex_float_t value, const char* units);
/**
 * @brief Set an element in a array to a complex scalar
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_complex_double(generic_t x, const size_t index, const complex_double_t value, const char* units);
/**
 * @brief Set an element in a array to a 1darray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value Pointer to the memory containing the array to assign
 *   to x
 * @param[in] subtype Subtype of data contained in value
 * @param[in] precision The precision of the elements in value
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_1darray(generic_t x, const size_t index, const void* value, const char* subtype, const size_t precision, const size_t length, const char* units);
/**
 * @brief Set an element in a array to a int 1darray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_1darray_int8(generic_t x, const size_t index, const int8_t* value, const size_t length, const char* units);
/**
 * @brief Set an element in a array to a int 1darray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_1darray_int16(generic_t x, const size_t index, const int16_t* value, const size_t length, const char* units);
/**
 * @brief Set an element in a array to a int 1darray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_1darray_int32(generic_t x, const size_t index, const int32_t* value, const size_t length, const char* units);
/**
 * @brief Set an element in a array to a int 1darray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_1darray_int64(generic_t x, const size_t index, const int64_t* value, const size_t length, const char* units);
/**
 * @brief Set an element in a array to a uint 1darray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_1darray_uint8(generic_t x, const size_t index, const uint8_t* value, const size_t length, const char* units);
/**
 * @brief Set an element in a array to a uint 1darray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_1darray_uint16(generic_t x, const size_t index, const uint16_t* value, const size_t length, const char* units);
/**
 * @brief Set an element in a array to a uint 1darray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_1darray_uint32(generic_t x, const size_t index, const uint32_t* value, const size_t length, const char* units);
/**
 * @brief Set an element in a array to a uint 1darray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_1darray_uint64(generic_t x, const size_t index, const uint64_t* value, const size_t length, const char* units);
/**
 * @brief Set an element in a array to a float 1darray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_1darray_float(generic_t x, const size_t index, const float* value, const size_t length, const char* units);
/**
 * @brief Set an element in a array to a float 1darray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_1darray_double(generic_t x, const size_t index, const double* value, const size_t length, const char* units);
/**
 * @brief Set an element in a array to a complex 1darray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_1darray_complex_float(generic_t x, const size_t index, const complex_float_t* value, const size_t length, const char* units);
/**
 * @brief Set an element in a array to a complex 1darray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_1darray_complex_double(generic_t x, const size_t index, const complex_double_t* value, const size_t length, const char* units);
/**
 * @brief Set an element in a array to a ndarray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value Pointer to the memory containing the array to assign
 *   to x
 * @param[in] subtype Subtype of data contained in value
 * @param[in] precision The precision of the elements in value
 * @param[in] ndim The number of dimensions in value
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_ndarray(generic_t x, const size_t index, const void* value, const char* subtype, const size_t precision, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set an element in a array to a int ndarray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_ndarray_int8(generic_t x, const size_t index, const int8_t* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set an element in a array to a int ndarray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_ndarray_int16(generic_t x, const size_t index, const int16_t* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set an element in a array to a int ndarray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_ndarray_int32(generic_t x, const size_t index, const int32_t* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set an element in a array to a int ndarray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_ndarray_int64(generic_t x, const size_t index, const int64_t* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set an element in a array to a uint ndarray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_ndarray_uint8(generic_t x, const size_t index, const uint8_t* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set an element in a array to a uint ndarray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_ndarray_uint16(generic_t x, const size_t index, const uint16_t* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set an element in a array to a uint ndarray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_ndarray_uint32(generic_t x, const size_t index, const uint32_t* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set an element in a array to a uint ndarray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_ndarray_uint64(generic_t x, const size_t index, const uint64_t* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set an element in a array to a float ndarray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_ndarray_float(generic_t x, const size_t index, const float* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set an element in a array to a float ndarray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_ndarray_double(generic_t x, const size_t index, const double* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set an element in a array to a complex ndarray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_ndarray_complex_float(generic_t x, const size_t index, const complex_float_t* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set an element in a array to a complex ndarray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_ndarray_complex_double(generic_t x, const size_t index, const complex_double_t* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set an element in a array to a schema
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_schema(generic_t x, const size_t index, const generic_t value);
/**
 * @brief Set an element in a array to a any
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_any(generic_t x, const size_t index, const generic_t value);
/**
 * @brief Get a null from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API void* generic_array_get_null(generic_t x, const size_t index);
/**
 * @brief Get a boolean from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API bool generic_array_get_bool(generic_t x, const size_t index);
/**
 * @brief Get a integer from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API int generic_array_get_integer(generic_t x, const size_t index);
/**
 * @brief Get a number from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API double generic_array_get_number(generic_t x, const size_t index);
/**
 * @brief Get a string from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API const char* generic_array_get_string(generic_t x, const size_t index);
/**
 * @brief Get a item from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[in] type Type of item to retrieve
 * @returns Pointer to data containing raw item data, NULL on error
 */
YGG_API void* generic_array_get_item(generic_t x, const size_t index, const char* type);
/**
 * @brief Get a item_nbytes from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[in] type Type of item to retrieve
 * @returns Number of bytes in raw item data, 0 on error
 */
YGG_API int generic_array_get_item_nbytes(generic_t x, const size_t index, const char* type);
/**
 * @brief Get a array from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API generic_t generic_array_get_array(generic_t x, const size_t index);
/**
 * @brief Get a object from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API generic_t generic_array_get_object(generic_t x, const size_t index);
#define generic_array_get_map generic_array_get_object
/**
 * @brief Get a ply from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API ply_t generic_array_get_ply(generic_t x, const size_t index);
/**
 * @brief Get a obj from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API obj_t generic_array_get_obj(generic_t x, const size_t index);
/**
 * @brief Get a class from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API python_t generic_array_get_python_class(generic_t x, const size_t index);
/**
 * @brief Get a function from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API python_t generic_array_get_python_function(generic_t x, const size_t index);
/**
 * @brief Get a instance from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API python_t generic_array_get_python_instance(generic_t x, const size_t index);
/**
 * @brief Get a scalar from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[in] subtype Subtype of data to return
 * @param[in] precision Precision of the data to return
 * @returns Pointer to value in x
 */
YGG_API void* generic_array_get_scalar(generic_t x, const size_t index, const char* subtype, const size_t precision);
/**
 * @brief Get a int scalar from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API int8_t generic_array_get_int8(generic_t x, const size_t index);
/**
 * @brief Get a int scalar from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API int16_t generic_array_get_int16(generic_t x, const size_t index);
/**
 * @brief Get a int scalar from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API int32_t generic_array_get_int32(generic_t x, const size_t index);
/**
 * @brief Get a int scalar from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API int64_t generic_array_get_int64(generic_t x, const size_t index);
/**
 * @brief Get a uint scalar from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API uint8_t generic_array_get_uint8(generic_t x, const size_t index);
/**
 * @brief Get a uint scalar from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API uint16_t generic_array_get_uint16(generic_t x, const size_t index);
/**
 * @brief Get a uint scalar from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API uint32_t generic_array_get_uint32(generic_t x, const size_t index);
/**
 * @brief Get a uint scalar from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API uint64_t generic_array_get_uint64(generic_t x, const size_t index);
/**
 * @brief Get a float scalar from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API float generic_array_get_float(generic_t x, const size_t index);
/**
 * @brief Get a float scalar from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API double generic_array_get_double(generic_t x, const size_t index);
/**
 * @brief Get a complex scalar from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API complex_float_t generic_array_get_complex_float(generic_t x, const size_t index);
/**
 * @brief Get a complex scalar from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API complex_double_t generic_array_get_complex_double(generic_t x, const size_t index);
/**
 * @brief Get a 1darray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[in] subtype Subtype of data to return
 * @param[in] precision Precision of the data to return
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_array_get_1darray(generic_t x, const size_t index, const char* subtype, const size_t precision, void** value);
/**
 * @brief Get a int 1darray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_array_get_1darray_int8(generic_t x, const size_t index, int8_t** value);
/**
 * @brief Get a int 1darray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_array_get_1darray_int16(generic_t x, const size_t index, int16_t** value);
/**
 * @brief Get a int 1darray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_array_get_1darray_int32(generic_t x, const size_t index, int32_t** value);
/**
 * @brief Get a int 1darray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_array_get_1darray_int64(generic_t x, const size_t index, int64_t** value);
/**
 * @brief Get a uint 1darray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_array_get_1darray_uint8(generic_t x, const size_t index, uint8_t** value);
/**
 * @brief Get a uint 1darray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_array_get_1darray_uint16(generic_t x, const size_t index, uint16_t** value);
/**
 * @brief Get a uint 1darray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_array_get_1darray_uint32(generic_t x, const size_t index, uint32_t** value);
/**
 * @brief Get a uint 1darray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_array_get_1darray_uint64(generic_t x, const size_t index, uint64_t** value);
/**
 * @brief Get a float 1darray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_array_get_1darray_float(generic_t x, const size_t index, float** value);
/**
 * @brief Get a float 1darray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_array_get_1darray_double(generic_t x, const size_t index, double** value);
/**
 * @brief Get a complex 1darray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_array_get_1darray_complex_float(generic_t x, const size_t index, complex_float_t** value);
/**
 * @brief Get a complex 1darray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_array_get_1darray_complex_double(generic_t x, const size_t index, complex_double_t** value);
/**
 * @brief Get a ndarray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[in] subtype Subtype of data to return
 * @param[in] precision Precision of the data to return
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_array_get_ndarray(generic_t x, const size_t index, const char* subtype, const size_t precision, void** value, size_t** shape);
/**
 * @brief Get a int ndarray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_array_get_ndarray_int8(generic_t x, const size_t index, int8_t** value, size_t** shape);
/**
 * @brief Get a int ndarray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_array_get_ndarray_int16(generic_t x, const size_t index, int16_t** value, size_t** shape);
/**
 * @brief Get a int ndarray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_array_get_ndarray_int32(generic_t x, const size_t index, int32_t** value, size_t** shape);
/**
 * @brief Get a int ndarray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_array_get_ndarray_int64(generic_t x, const size_t index, int64_t** value, size_t** shape);
/**
 * @brief Get a uint ndarray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_array_get_ndarray_uint8(generic_t x, const size_t index, uint8_t** value, size_t** shape);
/**
 * @brief Get a uint ndarray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_array_get_ndarray_uint16(generic_t x, const size_t index, uint16_t** value, size_t** shape);
/**
 * @brief Get a uint ndarray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_array_get_ndarray_uint32(generic_t x, const size_t index, uint32_t** value, size_t** shape);
/**
 * @brief Get a uint ndarray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_array_get_ndarray_uint64(generic_t x, const size_t index, uint64_t** value, size_t** shape);
/**
 * @brief Get a float ndarray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_array_get_ndarray_float(generic_t x, const size_t index, float** value, size_t** shape);
/**
 * @brief Get a float ndarray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_array_get_ndarray_double(generic_t x, const size_t index, double** value, size_t** shape);
/**
 * @brief Get a complex ndarray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_array_get_ndarray_complex_float(generic_t x, const size_t index, complex_float_t** value, size_t** shape);
/**
 * @brief Get a complex ndarray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_array_get_ndarray_complex_double(generic_t x, const size_t index, complex_double_t** value, size_t** shape);
/**
 * @brief Get a schema from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API generic_t generic_array_get_schema(generic_t x, const size_t index);
/**
 * @brief Get a any from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API generic_t generic_array_get_any(generic_t x, const size_t index);
/**
 * @brief Set an element in a object to a null
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_null(generic_t x, const char* key, const void* value);
#define generic_map_set_null generic_object_set_null
/**
 * @brief Set an element in a object to a boolean
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_bool(generic_t x, const char* key, const bool value);
#define generic_map_set_bool generic_object_set_bool
/**
 * @brief Set an element in a object to a integer
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_integer(generic_t x, const char* key, const int value);
#define generic_map_set_integer generic_object_set_integer
/**
 * @brief Set an element in a object to a number
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_number(generic_t x, const char* key, const double value);
#define generic_map_set_number generic_object_set_number
/**
 * @brief Set an element in a object to a string
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_string(generic_t x, const char* key, const char* value);
#define generic_map_set_string generic_object_set_string
/**
 * @brief Set an element in a object to a item
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_item(generic_t x, const char* key, const char* type, void* value);
#define generic_map_set_item generic_object_set_item
/**
 * @brief Set an element in a object to a array
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_array(generic_t x, const char* key, const generic_t value);
#define generic_map_set_array generic_object_set_array
/**
 * @brief Set an element in a object to a object
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_object(generic_t x, const char* key, const generic_t value);
#define generic_object_set_map generic_object_set_object
#define generic_map_set_object generic_object_set_object
/**
 * @brief Set an element in a object to a ply
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_ply(generic_t x, const char* key, const ply_t value);
#define generic_map_set_ply generic_object_set_ply
/**
 * @brief Set an element in a object to a obj
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_obj(generic_t x, const char* key, const obj_t value);
#define generic_map_set_obj generic_object_set_obj
/**
 * @brief Set an element in a object to a class
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_python_class(generic_t x, const char* key, const python_t value);
#define generic_map_set_python_class generic_object_set_python_class
/**
 * @brief Set an element in a object to a function
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_python_function(generic_t x, const char* key, const python_t value);
#define generic_map_set_python_function generic_object_set_python_function
/**
 * @brief Set an element in a object to a instance
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_python_instance(generic_t x, const char* key, const python_t value);
#define generic_map_set_python_instance generic_object_set_python_instance
/**
 * @brief Set an element in a object to a scalar
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value Pointer to the memory containing the value to assign to x
 * @param[in] subtype Subtype of data contained in value
 * @param[in] precision The precision of the data in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_scalar(generic_t x, const char* key, const void* value, const char* subtype, const size_t precision, const char* units);
#define generic_map_set_scalar generic_object_set_scalar
/**
 * @brief Set an element in a object to a int scalar
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_int8(generic_t x, const char* key, const int8_t value, const char* units);
#define generic_map_set_int8 generic_object_set_int8
/**
 * @brief Set an element in a object to a int scalar
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_int16(generic_t x, const char* key, const int16_t value, const char* units);
#define generic_map_set_int16 generic_object_set_int16
/**
 * @brief Set an element in a object to a int scalar
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_int32(generic_t x, const char* key, const int32_t value, const char* units);
#define generic_map_set_int32 generic_object_set_int32
/**
 * @brief Set an element in a object to a int scalar
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_int64(generic_t x, const char* key, const int64_t value, const char* units);
#define generic_map_set_int64 generic_object_set_int64
/**
 * @brief Set an element in a object to a uint scalar
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_uint8(generic_t x, const char* key, const uint8_t value, const char* units);
#define generic_map_set_uint8 generic_object_set_uint8
/**
 * @brief Set an element in a object to a uint scalar
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_uint16(generic_t x, const char* key, const uint16_t value, const char* units);
#define generic_map_set_uint16 generic_object_set_uint16
/**
 * @brief Set an element in a object to a uint scalar
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_uint32(generic_t x, const char* key, const uint32_t value, const char* units);
#define generic_map_set_uint32 generic_object_set_uint32
/**
 * @brief Set an element in a object to a uint scalar
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_uint64(generic_t x, const char* key, const uint64_t value, const char* units);
#define generic_map_set_uint64 generic_object_set_uint64
/**
 * @brief Set an element in a object to a float scalar
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_float(generic_t x, const char* key, const float value, const char* units);
#define generic_map_set_float generic_object_set_float
/**
 * @brief Set an element in a object to a float scalar
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_double(generic_t x, const char* key, const double value, const char* units);
#define generic_map_set_double generic_object_set_double
/**
 * @brief Set an element in a object to a complex scalar
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_complex_float(generic_t x, const char* key, const complex_float_t value, const char* units);
#define generic_map_set_complex_float generic_object_set_complex_float
/**
 * @brief Set an element in a object to a complex scalar
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_complex_double(generic_t x, const char* key, const complex_double_t value, const char* units);
#define generic_map_set_complex_double generic_object_set_complex_double
/**
 * @brief Set an element in a object to a 1darray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value Pointer to the memory containing the array to assign
 *   to x
 * @param[in] subtype Subtype of data contained in value
 * @param[in] precision The precision of the elements in value
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_1darray(generic_t x, const char* key, const void* value, const char* subtype, const size_t precision, const size_t length, const char* units);
#define generic_map_set_1darray generic_object_set_1darray
/**
 * @brief Set an element in a object to a int 1darray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_1darray_int8(generic_t x, const char* key, const int8_t* value, const size_t length, const char* units);
#define generic_map_set_1darray_int8 generic_object_set_1darray_int8
/**
 * @brief Set an element in a object to a int 1darray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_1darray_int16(generic_t x, const char* key, const int16_t* value, const size_t length, const char* units);
#define generic_map_set_1darray_int16 generic_object_set_1darray_int16
/**
 * @brief Set an element in a object to a int 1darray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_1darray_int32(generic_t x, const char* key, const int32_t* value, const size_t length, const char* units);
#define generic_map_set_1darray_int32 generic_object_set_1darray_int32
/**
 * @brief Set an element in a object to a int 1darray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_1darray_int64(generic_t x, const char* key, const int64_t* value, const size_t length, const char* units);
#define generic_map_set_1darray_int64 generic_object_set_1darray_int64
/**
 * @brief Set an element in a object to a uint 1darray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_1darray_uint8(generic_t x, const char* key, const uint8_t* value, const size_t length, const char* units);
#define generic_map_set_1darray_uint8 generic_object_set_1darray_uint8
/**
 * @brief Set an element in a object to a uint 1darray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_1darray_uint16(generic_t x, const char* key, const uint16_t* value, const size_t length, const char* units);
#define generic_map_set_1darray_uint16 generic_object_set_1darray_uint16
/**
 * @brief Set an element in a object to a uint 1darray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_1darray_uint32(generic_t x, const char* key, const uint32_t* value, const size_t length, const char* units);
#define generic_map_set_1darray_uint32 generic_object_set_1darray_uint32
/**
 * @brief Set an element in a object to a uint 1darray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_1darray_uint64(generic_t x, const char* key, const uint64_t* value, const size_t length, const char* units);
#define generic_map_set_1darray_uint64 generic_object_set_1darray_uint64
/**
 * @brief Set an element in a object to a float 1darray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_1darray_float(generic_t x, const char* key, const float* value, const size_t length, const char* units);
#define generic_map_set_1darray_float generic_object_set_1darray_float
/**
 * @brief Set an element in a object to a float 1darray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_1darray_double(generic_t x, const char* key, const double* value, const size_t length, const char* units);
#define generic_map_set_1darray_double generic_object_set_1darray_double
/**
 * @brief Set an element in a object to a complex 1darray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_1darray_complex_float(generic_t x, const char* key, const complex_float_t* value, const size_t length, const char* units);
#define generic_map_set_1darray_complex_float generic_object_set_1darray_complex_float
/**
 * @brief Set an element in a object to a complex 1darray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_1darray_complex_double(generic_t x, const char* key, const complex_double_t* value, const size_t length, const char* units);
#define generic_map_set_1darray_complex_double generic_object_set_1darray_complex_double
/**
 * @brief Set an element in a object to a ndarray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value Pointer to the memory containing the array to assign
 *   to x
 * @param[in] subtype Subtype of data contained in value
 * @param[in] precision The precision of the elements in value
 * @param[in] ndim The number of dimensions in value
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_ndarray(generic_t x, const char* key, const void* value, const char* subtype, const size_t precision, const size_t ndim, const size_t* shape, const char* units);
#define generic_map_set_ndarray generic_object_set_ndarray
/**
 * @brief Set an element in a object to a int ndarray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_ndarray_int8(generic_t x, const char* key, const int8_t* value, const size_t ndim, const size_t* shape, const char* units);
#define generic_map_set_ndarray_int8 generic_object_set_ndarray_int8
/**
 * @brief Set an element in a object to a int ndarray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_ndarray_int16(generic_t x, const char* key, const int16_t* value, const size_t ndim, const size_t* shape, const char* units);
#define generic_map_set_ndarray_int16 generic_object_set_ndarray_int16
/**
 * @brief Set an element in a object to a int ndarray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_ndarray_int32(generic_t x, const char* key, const int32_t* value, const size_t ndim, const size_t* shape, const char* units);
#define generic_map_set_ndarray_int32 generic_object_set_ndarray_int32
/**
 * @brief Set an element in a object to a int ndarray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_ndarray_int64(generic_t x, const char* key, const int64_t* value, const size_t ndim, const size_t* shape, const char* units);
#define generic_map_set_ndarray_int64 generic_object_set_ndarray_int64
/**
 * @brief Set an element in a object to a uint ndarray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_ndarray_uint8(generic_t x, const char* key, const uint8_t* value, const size_t ndim, const size_t* shape, const char* units);
#define generic_map_set_ndarray_uint8 generic_object_set_ndarray_uint8
/**
 * @brief Set an element in a object to a uint ndarray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_ndarray_uint16(generic_t x, const char* key, const uint16_t* value, const size_t ndim, const size_t* shape, const char* units);
#define generic_map_set_ndarray_uint16 generic_object_set_ndarray_uint16
/**
 * @brief Set an element in a object to a uint ndarray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_ndarray_uint32(generic_t x, const char* key, const uint32_t* value, const size_t ndim, const size_t* shape, const char* units);
#define generic_map_set_ndarray_uint32 generic_object_set_ndarray_uint32
/**
 * @brief Set an element in a object to a uint ndarray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_ndarray_uint64(generic_t x, const char* key, const uint64_t* value, const size_t ndim, const size_t* shape, const char* units);
#define generic_map_set_ndarray_uint64 generic_object_set_ndarray_uint64
/**
 * @brief Set an element in a object to a float ndarray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_ndarray_float(generic_t x, const char* key, const float* value, const size_t ndim, const size_t* shape, const char* units);
#define generic_map_set_ndarray_float generic_object_set_ndarray_float
/**
 * @brief Set an element in a object to a float ndarray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_ndarray_double(generic_t x, const char* key, const double* value, const size_t ndim, const size_t* shape, const char* units);
#define generic_map_set_ndarray_double generic_object_set_ndarray_double
/**
 * @brief Set an element in a object to a complex ndarray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_ndarray_complex_float(generic_t x, const char* key, const complex_float_t* value, const size_t ndim, const size_t* shape, const char* units);
#define generic_map_set_ndarray_complex_float generic_object_set_ndarray_complex_float
/**
 * @brief Set an element in a object to a complex ndarray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_ndarray_complex_double(generic_t x, const char* key, const complex_double_t* value, const size_t ndim, const size_t* shape, const char* units);
#define generic_map_set_ndarray_complex_double generic_object_set_ndarray_complex_double
/**
 * @brief Set an element in a object to a schema
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_schema(generic_t x, const char* key, const generic_t value);
#define generic_map_set_schema generic_object_set_schema
/**
 * @brief Set an element in a object to a any
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_any(generic_t x, const char* key, const generic_t value);
#define generic_map_set_any generic_object_set_any
/**
 * @brief Get a null from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API void* generic_object_get_null(generic_t x, const char* key);
#define generic_map_get_null generic_object_get_null
/**
 * @brief Get a boolean from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API bool generic_object_get_bool(generic_t x, const char* key);
#define generic_map_get_bool generic_object_get_bool
/**
 * @brief Get a integer from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API int generic_object_get_integer(generic_t x, const char* key);
#define generic_map_get_integer generic_object_get_integer
/**
 * @brief Get a number from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API double generic_object_get_number(generic_t x, const char* key);
#define generic_map_get_number generic_object_get_number
/**
 * @brief Get a string from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API const char* generic_object_get_string(generic_t x, const char* key);
#define generic_map_get_string generic_object_get_string
/**
 * @brief Get a item from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[in] type Type of item to retrieve
 * @returns Pointer to data containing raw item data, NULL on error
 */
YGG_API void* generic_object_get_item(generic_t x, const char* key, const char* type);
#define generic_map_get_item generic_object_get_item
/**
 * @brief Get a item_nbytes from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[in] type Type of item to retrieve
 * @returns Number of bytes in raw item data, 0 on error
 */
YGG_API int generic_object_get_item_nbytes(generic_t x, const char* key, const char* type);
#define generic_map_get_item_nbytes generic_object_get_item_nbytes
/**
 * @brief Get a array from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API generic_t generic_object_get_array(generic_t x, const char* key);
#define generic_map_get_array generic_object_get_array
/**
 * @brief Get a object from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API generic_t generic_object_get_object(generic_t x, const char* key);
#define generic_object_get_map generic_object_get_object
#define generic_map_get_object generic_object_get_object
/**
 * @brief Get a ply from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API ply_t generic_object_get_ply(generic_t x, const char* key);
#define generic_map_get_ply generic_object_get_ply
/**
 * @brief Get a obj from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API obj_t generic_object_get_obj(generic_t x, const char* key);
#define generic_map_get_obj generic_object_get_obj
/**
 * @brief Get a class from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API python_t generic_object_get_python_class(generic_t x, const char* key);
#define generic_map_get_python_class generic_object_get_python_class
/**
 * @brief Get a function from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API python_t generic_object_get_python_function(generic_t x, const char* key);
#define generic_map_get_python_function generic_object_get_python_function
/**
 * @brief Get a instance from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API python_t generic_object_get_python_instance(generic_t x, const char* key);
#define generic_map_get_python_instance generic_object_get_python_instance
/**
 * @brief Get a scalar from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[in] subtype Subtype of data to return
 * @param[in] precision Precision of the data to return
 * @returns Pointer to value in x
 */
YGG_API void* generic_object_get_scalar(generic_t x, const char* key, const char* subtype, const size_t precision);
#define generic_map_get_scalar generic_object_get_scalar
/**
 * @brief Get a int scalar from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API int8_t generic_object_get_int8(generic_t x, const char* key);
#define generic_map_get_int8 generic_object_get_int8
/**
 * @brief Get a int scalar from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API int16_t generic_object_get_int16(generic_t x, const char* key);
#define generic_map_get_int16 generic_object_get_int16
/**
 * @brief Get a int scalar from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API int32_t generic_object_get_int32(generic_t x, const char* key);
#define generic_map_get_int32 generic_object_get_int32
/**
 * @brief Get a int scalar from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API int64_t generic_object_get_int64(generic_t x, const char* key);
#define generic_map_get_int64 generic_object_get_int64
/**
 * @brief Get a uint scalar from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API uint8_t generic_object_get_uint8(generic_t x, const char* key);
#define generic_map_get_uint8 generic_object_get_uint8
/**
 * @brief Get a uint scalar from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API uint16_t generic_object_get_uint16(generic_t x, const char* key);
#define generic_map_get_uint16 generic_object_get_uint16
/**
 * @brief Get a uint scalar from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API uint32_t generic_object_get_uint32(generic_t x, const char* key);
#define generic_map_get_uint32 generic_object_get_uint32
/**
 * @brief Get a uint scalar from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API uint64_t generic_object_get_uint64(generic_t x, const char* key);
#define generic_map_get_uint64 generic_object_get_uint64
/**
 * @brief Get a float scalar from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API float generic_object_get_float(generic_t x, const char* key);
#define generic_map_get_float generic_object_get_float
/**
 * @brief Get a float scalar from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API double generic_object_get_double(generic_t x, const char* key);
#define generic_map_get_double generic_object_get_double
/**
 * @brief Get a complex scalar from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API complex_float_t generic_object_get_complex_float(generic_t x, const char* key);
#define generic_map_get_complex_float generic_object_get_complex_float
/**
 * @brief Get a complex scalar from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API complex_double_t generic_object_get_complex_double(generic_t x, const char* key);
#define generic_map_get_complex_double generic_object_get_complex_double
/**
 * @brief Get a 1darray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[in] subtype Subtype of data to return
 * @param[in] precision Precision of the data to return
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_object_get_1darray(generic_t x, const char* key, const char* subtype, const size_t precision, void** value);
#define generic_map_get_1darray generic_object_get_1darray
/**
 * @brief Get a int 1darray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_object_get_1darray_int8(generic_t x, const char* key, int8_t** value);
#define generic_map_get_1darray_int8 generic_object_get_1darray_int8
/**
 * @brief Get a int 1darray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_object_get_1darray_int16(generic_t x, const char* key, int16_t** value);
#define generic_map_get_1darray_int16 generic_object_get_1darray_int16
/**
 * @brief Get a int 1darray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_object_get_1darray_int32(generic_t x, const char* key, int32_t** value);
#define generic_map_get_1darray_int32 generic_object_get_1darray_int32
/**
 * @brief Get a int 1darray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_object_get_1darray_int64(generic_t x, const char* key, int64_t** value);
#define generic_map_get_1darray_int64 generic_object_get_1darray_int64
/**
 * @brief Get a uint 1darray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_object_get_1darray_uint8(generic_t x, const char* key, uint8_t** value);
#define generic_map_get_1darray_uint8 generic_object_get_1darray_uint8
/**
 * @brief Get a uint 1darray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_object_get_1darray_uint16(generic_t x, const char* key, uint16_t** value);
#define generic_map_get_1darray_uint16 generic_object_get_1darray_uint16
/**
 * @brief Get a uint 1darray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_object_get_1darray_uint32(generic_t x, const char* key, uint32_t** value);
#define generic_map_get_1darray_uint32 generic_object_get_1darray_uint32
/**
 * @brief Get a uint 1darray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_object_get_1darray_uint64(generic_t x, const char* key, uint64_t** value);
#define generic_map_get_1darray_uint64 generic_object_get_1darray_uint64
/**
 * @brief Get a float 1darray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_object_get_1darray_float(generic_t x, const char* key, float** value);
#define generic_map_get_1darray_float generic_object_get_1darray_float
/**
 * @brief Get a float 1darray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_object_get_1darray_double(generic_t x, const char* key, double** value);
#define generic_map_get_1darray_double generic_object_get_1darray_double
/**
 * @brief Get a complex 1darray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_object_get_1darray_complex_float(generic_t x, const char* key, complex_float_t** value);
#define generic_map_get_1darray_complex_float generic_object_get_1darray_complex_float
/**
 * @brief Get a complex 1darray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_object_get_1darray_complex_double(generic_t x, const char* key, complex_double_t** value);
#define generic_map_get_1darray_complex_double generic_object_get_1darray_complex_double
/**
 * @brief Get a ndarray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[in] subtype Subtype of data to return
 * @param[in] precision Precision of the data to return
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_object_get_ndarray(generic_t x, const char* key, const char* subtype, const size_t precision, void** value, size_t** shape);
#define generic_map_get_ndarray generic_object_get_ndarray
/**
 * @brief Get a int ndarray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_object_get_ndarray_int8(generic_t x, const char* key, int8_t** value, size_t** shape);
#define generic_map_get_ndarray_int8 generic_object_get_ndarray_int8
/**
 * @brief Get a int ndarray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_object_get_ndarray_int16(generic_t x, const char* key, int16_t** value, size_t** shape);
#define generic_map_get_ndarray_int16 generic_object_get_ndarray_int16
/**
 * @brief Get a int ndarray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_object_get_ndarray_int32(generic_t x, const char* key, int32_t** value, size_t** shape);
#define generic_map_get_ndarray_int32 generic_object_get_ndarray_int32
/**
 * @brief Get a int ndarray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_object_get_ndarray_int64(generic_t x, const char* key, int64_t** value, size_t** shape);
#define generic_map_get_ndarray_int64 generic_object_get_ndarray_int64
/**
 * @brief Get a uint ndarray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_object_get_ndarray_uint8(generic_t x, const char* key, uint8_t** value, size_t** shape);
#define generic_map_get_ndarray_uint8 generic_object_get_ndarray_uint8
/**
 * @brief Get a uint ndarray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_object_get_ndarray_uint16(generic_t x, const char* key, uint16_t** value, size_t** shape);
#define generic_map_get_ndarray_uint16 generic_object_get_ndarray_uint16
/**
 * @brief Get a uint ndarray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_object_get_ndarray_uint32(generic_t x, const char* key, uint32_t** value, size_t** shape);
#define generic_map_get_ndarray_uint32 generic_object_get_ndarray_uint32
/**
 * @brief Get a uint ndarray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_object_get_ndarray_uint64(generic_t x, const char* key, uint64_t** value, size_t** shape);
#define generic_map_get_ndarray_uint64 generic_object_get_ndarray_uint64
/**
 * @brief Get a float ndarray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_object_get_ndarray_float(generic_t x, const char* key, float** value, size_t** shape);
#define generic_map_get_ndarray_float generic_object_get_ndarray_float
/**
 * @brief Get a float ndarray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_object_get_ndarray_double(generic_t x, const char* key, double** value, size_t** shape);
#define generic_map_get_ndarray_double generic_object_get_ndarray_double
/**
 * @brief Get a complex ndarray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_object_get_ndarray_complex_float(generic_t x, const char* key, complex_float_t** value, size_t** shape);
#define generic_map_get_ndarray_complex_float generic_object_get_ndarray_complex_float
/**
 * @brief Get a complex ndarray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_object_get_ndarray_complex_double(generic_t x, const char* key, complex_double_t** value, size_t** shape);
#define generic_map_get_ndarray_complex_double generic_object_get_ndarray_complex_double
/**
 * @brief Get a schema from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API generic_t generic_object_get_schema(generic_t x, const char* key);
#define generic_map_get_schema generic_object_get_schema
/**
 * @brief Get a any from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API generic_t generic_object_get_any(generic_t x, const char* key);
#define generic_map_get_any generic_object_get_any
#ifdef YGGDRASIL_LONG_DOUBLE_AVAILABLE
/**
 * @brief Set a given generic item to a float scalar
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_long_double(generic_t x, const long double value, const char* units);
/**
 * @brief Set a given generic item to a complex scalar
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_complex_long_double(generic_t x, const complex_long_double_t value, const char* units);
/**
 * @brief Set a given generic item to a float 1darray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_1darray_long_double(generic_t x, const long double* value, const size_t length, const char* units);
/**
 * @brief Set a given generic item to a complex 1darray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_1darray_complex_long_double(generic_t x, const complex_long_double_t* value, const size_t length, const char* units);
/**
 * @brief Set a given generic item to a float ndarray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_ndarray_long_double(generic_t x, const long double* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set a given generic item to a complex ndarray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_set_ndarray_complex_long_double(generic_t x, const complex_long_double_t* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Get a float scalar from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API long double generic_get_long_double(generic_t x);
/**
 * @brief Get a complex scalar from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
YGG_API complex_long_double_t generic_get_complex_long_double(generic_t x);
/**
 * @brief Get a float 1darray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_get_1darray_long_double(generic_t x, long double** value);
/**
 * @brief Get a complex 1darray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_get_1darray_complex_long_double(generic_t x, complex_long_double_t** value);
/**
 * @brief Get a float ndarray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_get_ndarray_long_double(generic_t x, long double** value, size_t** shape);
/**
 * @brief Get a complex ndarray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_get_ndarray_complex_long_double(generic_t x, complex_long_double_t** value, size_t** shape);
/**
 * @brief Get a float scalar from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API long double generic_ref_get_long_double(generic_ref_t x);
/**
 * @brief Get a complex scalar from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
YGG_API complex_long_double_t generic_ref_get_complex_long_double(generic_ref_t x);
/**
 * @brief Get a float 1darray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_1darray_long_double(generic_ref_t x, long double** value);
/**
 * @brief Get a complex 1darray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_1darray_complex_long_double(generic_ref_t x, complex_long_double_t** value);
/**
 * @brief Get a float ndarray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_ndarray_long_double(generic_ref_t x, long double** value, size_t** shape);
/**
 * @brief Get a complex ndarray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_ref_get_ndarray_complex_long_double(generic_ref_t x, complex_long_double_t** value, size_t** shape);
/**
 * @brief Set an element in a array to a float scalar
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_long_double(generic_t x, const size_t index, const long double value, const char* units);
/**
 * @brief Set an element in a array to a complex scalar
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_complex_long_double(generic_t x, const size_t index, const complex_long_double_t value, const char* units);
/**
 * @brief Set an element in a array to a float 1darray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_1darray_long_double(generic_t x, const size_t index, const long double* value, const size_t length, const char* units);
/**
 * @brief Set an element in a array to a complex 1darray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_1darray_complex_long_double(generic_t x, const size_t index, const complex_long_double_t* value, const size_t length, const char* units);
/**
 * @brief Set an element in a array to a float ndarray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_ndarray_long_double(generic_t x, const size_t index, const long double* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Set an element in a array to a complex ndarray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_array_set_ndarray_complex_long_double(generic_t x, const size_t index, const complex_long_double_t* value, const size_t ndim, const size_t* shape, const char* units);
/**
 * @brief Get a float scalar from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API long double generic_array_get_long_double(generic_t x, const size_t index);
/**
 * @brief Get a complex scalar from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
YGG_API complex_long_double_t generic_array_get_complex_long_double(generic_t x, const size_t index);
/**
 * @brief Get a float 1darray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_array_get_1darray_long_double(generic_t x, const size_t index, long double** value);
/**
 * @brief Get a complex 1darray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_array_get_1darray_complex_long_double(generic_t x, const size_t index, complex_long_double_t** value);
/**
 * @brief Get a float ndarray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_array_get_ndarray_long_double(generic_t x, const size_t index, long double** value, size_t** shape);
/**
 * @brief Get a complex ndarray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_array_get_ndarray_complex_long_double(generic_t x, const size_t index, complex_long_double_t** value, size_t** shape);
/**
 * @brief Set an element in a object to a float scalar
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_long_double(generic_t x, const char* key, const long double value, const char* units);
#define generic_map_set_long_double generic_object_set_long_double
/**
 * @brief Set an element in a object to a complex scalar
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_complex_long_double(generic_t x, const char* key, const complex_long_double_t value, const char* units);
#define generic_map_set_complex_long_double generic_object_set_complex_long_double
/**
 * @brief Set an element in a object to a float 1darray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_1darray_long_double(generic_t x, const char* key, const long double* value, const size_t length, const char* units);
#define generic_map_set_1darray_long_double generic_object_set_1darray_long_double
/**
 * @brief Set an element in a object to a complex 1darray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_1darray_complex_long_double(generic_t x, const char* key, const complex_long_double_t* value, const size_t length, const char* units);
#define generic_map_set_1darray_complex_long_double generic_object_set_1darray_complex_long_double
/**
 * @brief Set an element in a object to a float ndarray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_ndarray_long_double(generic_t x, const char* key, const long double* value, const size_t ndim, const size_t* shape, const char* units);
#define generic_map_set_ndarray_long_double generic_object_set_ndarray_long_double
/**
 * @brief Set an element in a object to a complex ndarray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
YGG_API int generic_object_set_ndarray_complex_long_double(generic_t x, const char* key, const complex_long_double_t* value, const size_t ndim, const size_t* shape, const char* units);
#define generic_map_set_ndarray_complex_long_double generic_object_set_ndarray_complex_long_double
/**
 * @brief Get a float scalar from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API long double generic_object_get_long_double(generic_t x, const char* key);
#define generic_map_get_long_double generic_object_get_long_double
/**
 * @brief Get a complex scalar from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
YGG_API complex_long_double_t generic_object_get_complex_long_double(generic_t x, const char* key);
#define generic_map_get_complex_long_double generic_object_get_complex_long_double
/**
 * @brief Get a float 1darray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_object_get_1darray_long_double(generic_t x, const char* key, long double** value);
#define generic_map_get_1darray_long_double generic_object_get_1darray_long_double
/**
 * @brief Get a complex 1darray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
YGG_API size_t generic_object_get_1darray_complex_long_double(generic_t x, const char* key, complex_long_double_t** value);
#define generic_map_get_1darray_complex_long_double generic_object_get_1darray_complex_long_double
/**
 * @brief Get a float ndarray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_object_get_ndarray_long_double(generic_t x, const char* key, long double** value, size_t** shape);
#define generic_map_get_ndarray_long_double generic_object_get_ndarray_long_double
/**
 * @brief Get a complex ndarray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
YGG_API size_t generic_object_get_ndarray_complex_long_double(generic_t x, const char* key, complex_long_double_t** value, size_t** shape);
#define generic_map_get_ndarray_complex_long_double generic_object_get_ndarray_complex_long_double
#endif // YGGDRASIL_LONG_DOUBLE_AVAILABLE

#ifdef __cplusplus
}
#endif
