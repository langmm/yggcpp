#pragma once
#include <cstring>
#include <string>
#include <map>
#include <Python.h>
#include "utils/va_list_t.hpp"
#include "rapidjson/document.h"
#define MSG_HEAD_SEP "YGG_MSG_HEAD"

enum DTYPE {
    T_BOOLEAN, T_INTEGER, T_NULL, T_NUMBER, T_STRING, T_ARRAY, T_OBJECT,
    T_DIRECT, T_1DARRAY, T_NDARRAY, T_SCALAR, T_FLOAT, T_UINT, T_INT, T_COMPLEX,
    T_BYTES, T_UNICODE, T_PLY, T_OBJ, T_ASCII_TABLE,
    T_CLASS, T_FUNCTION, T_INSTANCE, T_SCHEMA, T_ANY
};

namespace communication {
namespace datatypes {
namespace Metaschema {
class MetaschemaType;
}

/*!
  @brief String comparison structure.
 */
struct strcomp : public std::binary_function<const std::string, const std::string, bool>
{
    /*!
      @brief Comparison operator.
      @param[in] a char const * First string for comparison.
      @param[in] b char const * Second string for comparison.
      @returns bool true if the strings are equivalent, false otherwise.
     */
    bool operator()(const std::string &a, const std::string &b) const
    {
        return a < b;
    }
};


std::map<const char*, int, strcomp> get_type_map();


int split_head_body(const char *buf, const size_t &buf_siz,
                    char **head, size_t *headsiz);

Metaschema::MetaschemaType* type_from_header_doc(const rapidjson::Value &header_doc,
                                                 const bool use_generic=true);

Metaschema::MetaschemaType* type_from_doc(const rapidjson::Value &type_doc,
                                          const bool use_generic=true,
                                          const rapidjson::Value *header_doc=nullptr);
Metaschema::MetaschemaType* type_from_doc_c(const void* type_doc, const bool use_generic=false);
}
}

extern "C" {

struct dtype_t {
    void* obj;
    char* type;
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
  @brief Get the ascii table data structure.
  @param[in] dtype dtype_t* Wrapper struct for C++ Metaschema type class.
  @returns: void* Cast pointer to ascii table.
*/
void* dtype_ascii_table(const dtype_t* dtype);


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
  @brief Construct a Scalar type object.
  @param[in] subtype char* Name of the scalar subtype (e.g. int, uint, float, bytes).
  @param[in] precision size_t Precision of the scalar in bits.
  @param[in] units char* Units for scalar. (e.g. "cm", "g", "" for unitless)
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
*/
dtype_t* create_dtype_scalar(const char* subtype, const size_t precision,
                             const char* units, const bool use_generic);


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
dtype_t* create_dtype_1darray(const char* subtype, const size_t precision,
                              const size_t length, const char* units,
                              const bool use_generic);


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
  @brief Construct an AsciiTable type object.
  @param[in] format_str const char* C-style format string that will be used to determine
  the type of elements in arrays that will be serialized/deserialized using
  the resulting type.
  @param[in] as_array int If 1, the types will be arrays. Otherwise they will be
  scalars.
  @param[in] use_generic bool If true, serialized/deserialized
  objects will be expected to be YggGeneric classes.
  @returns dtype_t* Type structure/class.
*/
dtype_t* create_dtype_ascii_table(const char *format_str, const int as_array,
                                  const bool use_generic);


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

int destroy_dtype(dtype_t **dtype);

dtype_t* create_dtype_format(const char* format_str, const int as_array=0, const bool use_generic=false);
JSONArrayMetaschemaType *create_dtype_format_class(const char *format_str,
                                                   const int as_array = 0,
                                                   const bool use_generic = false);
}