#pragma once
#include "utils/enums.hpp"
#ifdef __cplusplus
extern "C" {
#endif

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
//int update_dtype_from_generic_ap(dtype_t* dtype1, size_t nargs, struct va_list_t ap);


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
//int deserialize_dtype(const dtype_t *dtype, const char *buf, const size_t buf_siz,
//                      const int allow_realloc, size_t *nargs, struct va_list_t ap);


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
//int serialize_dtype(const dtype_t *dtype, char **buf, size_t *buf_siz,
//                    const int allow_realloc, size_t *nargs, struct va_list_t ap);


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
  @brief Wrapper for freeing MetaschemaType class wrapper struct.
  @param[in] dtype dtype_t** Wrapper struct for C++ Metaschema type class.
  @returns: int 0 if free was successfull, -1 if there was an error.
*/
int destroy_dtype(dtype_t** dtype);

int set_dtype_name(dtype_t* dtype, const char* name);


#ifdef __cplusplus
}
#endif