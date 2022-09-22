#include <cstdarg>
#include "va_list_t.hpp"

va_list_t::va_list_t() {
    using_ptrs = 0;
    ptrs = nullptr;
    nptrs = 0;
    iptr = 0;
    for_fortran = 0;
}

va_list_t::va_list_t(va_list_t *src) : va_list_t() {
    if (src->using_ptrs) {
        init(src->nptrs, src->ptrs);
        this->iptr = src->iptr;
    } else {
        va_copy(this->va, src->va);
    }
    this->for_fortran = src->for_fortran;
}

va_list_t::va_list_t(const int &nptrs, void **ptrs) : va_list_t() {init(nptrs, ptrs);}

void va_list_t::init(const int &nptr, void **ptr) {
    using_ptrs = 1;
    this->ptrs = ptr;
    this->nptrs = nptr;
    iptr = 0;
    for_fortran = 0;
}

void va_list_t::end() {
    if (!(using_ptrs)) {
        va_end(va);
    }

}

void va_list_t::skip(size_t nbytes) {
    if (using_ptrs) {
        iptr++;
    } else {
        if (nbytes == sizeof(void *)) {
            va_arg(va, void *);
        } else if (nbytes == sizeof(size_t)) {
            va_arg(va, size_t);
        } else if (nbytes == sizeof(char *)) {
            va_arg(va, char *);
        } else {
            printf("WARNING: Cannot get argument of size %ld.\n", nbytes);
            va_arg(va, void *);
            // va_arg(ap->va, char[nbytes]);
        }
    }
}