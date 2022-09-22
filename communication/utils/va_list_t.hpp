#pragma once
#include <cstdio>

#ifdef __cplusplus
extern "C" {
#endif

/*! @brief Structure used to wrap va_list and allow pointer passing.
@param va va_list Wrapped variable argument list.
*/
struct va_list_t {

    va_list_t();

    explicit va_list_t(va_list_t* src);

    /*! Initialize a variable argument list from an array of pointers.
      @param[in] nptrs int Number of pointers.
      @param[in] ptrs void** Array of pointers.
    */
    va_list_t(const int &nptrs, void **ptrs);

    void init(const int &nptr, void **ptr);

    /*! @brief Method for skipping a number of bytes in the argument list.
      @param[in] nbytes size_t Number of bytes that should be skipped.
     */
    void skip(size_t nbytes);

    /*! Finalize a variable argument list.
    */
    void end();
    va_list va;  //!< Traditional variable argument list.
    int using_ptrs; //!< Flag that is 1 if the arguments are stored using pointers.
    void **ptrs; //!< Variable arguments stored as pointers.
    int nptrs; //!< The number of variable arguments stored as pointers.
    int iptr; //!< The index of the current variable argument pointer.
    int for_fortran; //!< Flag that is 1 if this structure will be accessed by fortran.

} va_list_t;

#ifdef __cplusplus
}
#endif