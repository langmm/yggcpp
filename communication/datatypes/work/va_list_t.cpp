#include <cstdarg>
#include "va_list_t.hpp"
#include "logging.hpp"

va_list_t init_va_list() {
    va_list_t out;
    out.using_ptrs = 0;
    out.ptrs = nullptr;
    out.nptrs = 0;
    out.iptr = 0;
    out.for_fortran = 0;
    return out;
}

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
}

va_list_t init_va_ptrs(const int &nptr, void **ptr) {
    va_list_t out;
    out.using_ptrs = 1;
    out.ptrs = ptr;
    out.nptrs = nptr;
    out.iptr = 0;
    out.for_fortran = 0;
    return out;
}

void end_va_list(va_list_t *ap) {
    if (!(ap->using_ptrs)) {
        va_end(ap->va);
    }

}

void va_list_t_skip(va_list_t *ap, size_t nbytes) {
    if (ap->using_ptrs) {
        ap->iptr++;
    } else {
        if (nbytes == sizeof(void *)) {
            va_arg(ap->va, void *);
        } else if (nbytes == sizeof(size_t)) {
            va_arg(ap->va, size_t);
        } else if (nbytes == sizeof(char *)) {
            va_arg(ap->va, char *);
        } else {
            printf("WARNING: Cannot get argument of size %ld.\n", nbytes);
            va_arg(ap->va, void *);
            // va_arg(ap->va, char[nbytes]);
        }
    }
}

void* get_va_list_ptr_cpp(va_list_t *ap, int allow_null) {
    void *out = NULL;
    if (ap->using_ptrs) {
        if (ap->ptrs == NULL) {
            communication::utils::ygglog_throw_error("get_va_list_ptr: Pointers is NULL.");
        } else if (ap->iptr >= ap->nptrs) {
            communication::utils::ygglog_throw_error("get_va_list_ptr: Current index %d exceeds total number of pointers %d.",
                               ap->iptr, ap->nptrs);
        } else {
            out = ap->ptrs[ap->iptr];
            ap->iptr++;
            if ((out == NULL) && (allow_null == 0)) {
                communication::utils::ygglog_throw_error("get_va_list_ptr: Argument %d is NULL.", ap->iptr - 1);
            }
        }
    } else {
        communication::utils::ygglog_throw_error("get_va_list_ptr: Variable argument list is not stored in pointers.");
    }
    return out;
}

void** get_va_list_ptr_ref_cpp(va_list_t *ap, int allow_null) {
    void **out = NULL;
    if (ap->using_ptrs) {
        if (ap->ptrs == NULL) {
            communication::utils::ygglog_throw_error("get_va_list_ptr_ref: Pointers is NULL.");
        } else if (ap->iptr >= ap->nptrs) {
            communication::utils::ygglog_throw_error("get_va_list_ptr_ref: Current index %d exceeds total number of pointers %d.",
                               ap->iptr, ap->nptrs);
        } else {
            out = ap->ptrs + ap->iptr;
            ap->iptr++;
            if (((out == NULL) || (*out == NULL)) && (allow_null == 0)) {
                communication::utils::ygglog_throw_error("get_va_list_ptr_ref: Argument is NULL.");
            }
        }
    } else {
        communication::utils::ygglog_throw_error("get_va_list_ptr_ref: Variable argument list is not stored in pointers.");
    }
    return out;
}
