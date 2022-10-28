#pragma once
#include <cstdio>

#ifdef __cplusplus
extern "C" {
#endif

/*! @brief Structure used to wrap va_list and allow pointer passing.
@param va va_list Wrapped variable argument list.
*/
typedef struct va_list_t {
    va_list va;  //!< Traditional variable argument list.
    int using_ptrs; //!< Flag that is 1 if the arguments are stored using pointers.
    void **ptrs; //!< Variable arguments stored as pointers.
    int nptrs; //!< The number of variable arguments stored as pointers.
    int iptr; //!< The index of the current variable argument pointer.
    int for_fortran; //!< Flag that is 1 if this structure will be accessed by fortran.

} va_list_t;

va_list_t init_va_list();

va_list_t copy_va_list(va_list_t ap);

/*! Initialize a variable argument list from an array of pointers.
  @param[in] nptrs int Number of pointers.
  @param[in] ptrs void** Array of pointers.
*/
va_list_t init_va_ptrs(const int nptrs, void** ptrs);

/*! @brief Method for skipping a number of bytes in the argument list.
  @param[in] nbytes size_t Number of bytes that should be skipped.
 */
void va_list_t_skip(va_list_t *ap, size_t nbytes);

/*! Finalize a variable argument list.
*/
void end_va_list(va_list_t *ap);

#ifdef __cplusplus
}

/*! C++ wrapper to get a pointer from the variable argument list and
  advancing the position.
  @param[in] ap va_list_t Variable argument list.
  @param[in] allow_null int If 0, an error will be raised if the
  selected pointer is null, otherwise the null pointer will be returned.
  @returns void* Pointer.
*/
void* get_va_list_ptr_cpp(va_list_t *ap, int allow_null = 0);

/*! C++ wrapper to get a pointer to a pointer from the variable
  argument list and advancing the position.
  @param[in] ap va_list_t Variable argument list.
  @param[in] allow_null int If 0, an error will be raised if the
  selected pointer is null, otherwise the null pointer will be returned.
  @returns void* Pointer.
*/
void** get_va_list_ptr_ref_cpp(va_list_t *ap, int allow_null = 0);
#endif

