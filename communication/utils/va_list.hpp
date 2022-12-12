#pragma once
#include <stdio.h>
#include <stdarg.h>
#ifdef __cplusplus
extern "C" {
#endif

/*!
  @brief Structure used to wrap va_list and allow pointer passing.
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

/*!
  @brief Initialize a variable argument list from an existing va_list.
  @returns va_list_t New variable argument list structure.
 */
static inline
va_list_t init_va_list() {
    va_list_t out;
    out.using_ptrs = 0;
    out.ptrs = nullptr;
    out.nptrs = 0;
    out.iptr = 0;
    out.for_fortran = 0;
    return out;
};

/*! Initialize a variable argument list from an array of pointers.
  @param[in] nptrs int Number of pointers.
  @param[in] ptrs void** Array of pointers.
  @returns va_list_t New variable argument list structure.
*/
static inline
va_list_t init_va_ptrs(const int nptrs, void** ptrs) {
    va_list_t out;
    out.using_ptrs = 1;
    out.ptrs = ptrs;
    out.nptrs = nptrs;
    out.iptr = 0;
    out.for_fortran = 0;
    return out;
};


/*! Finalize a variable argument list.
  @param[in] ap va_list_t Variable argument list.
*/
static inline
void end_va_list(va_list_t *ap) {
    if (!(ap->using_ptrs)) {
        va_end(ap->va);
    }
};


/*! Copy a variable argument list.
  @param[in] ap va_list_t Variable argument list structure to copy.
  @returns va_list_t New variable argument list structure.
*/
static inline
va_list_t copy_va_list(va_list_t ap) {
    va_list_t out;
    if (ap.using_ptrs) {
        out = init_va_ptrs(ap.nptrs, ap.ptrs);
        out.iptr = ap.iptr;
    } else {
        out = init_va_list();
        va_copy(out.va, ap.va);
    }
    out.for_fortran = ap.for_fortran;
    return out;
};


/*! @brief Method for skipping a number of bytes in the argument list.
  @param[in] ap va_list_t* Structure containing variable argument list.
  @param[in] nbytes size_t Number of bytes that should be skipped.
 */
static inline
void va_list_t_skip(va_list_t *ap, size_t nbytes) {
    if (ap->using_ptrs) {
        ap->iptr++;
    } else {
        if (nbytes == sizeof(void*)) {
            va_arg(ap->va, void*);
        } else if (nbytes == sizeof(size_t)) {
            va_arg(ap->va, size_t);
        } else if (nbytes == sizeof(char*)) {
            va_arg(ap->va, char*);
        } else {
            printf("WARNING: Cannot get argument of size %ld.\n", nbytes);
            va_arg(ap->va, void*);
            // va_arg(ap->va, char[nbytes]);
        }
    }
};

#ifdef __cplusplus
}
#endif