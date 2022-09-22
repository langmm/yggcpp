#include "DirectMetaschemaType.hpp"

using namespace communication::datatypes::Metaschema;
using namespace communication::utils;

int DirectMetaschemaType::serialize(char **buf, size_t *buf_siz,
                                    const int allow_realloc, size_t *nargs, utils::va_list_t &ap) {
    if (nargs_exp() != *nargs) {
        ygglog_throw_error("DirectMetaschemaType::serialize: %d arguments expected, but %d provided.",
                           nargs_exp(), *nargs);
    }
    *nargs = *nargs - nargs_exp();
    // Assumes null termination
    char *msg;
    size_t msg_siz;
    if (ap.using_ptrs) {
        msg = ((char *) get_va_list_ptr_cpp(&ap));
        msg_siz = ((size_t *) get_va_list_ptr_cpp(&ap))[0];
    } else {
        msg = va_arg(ap.va, char*);
        msg_siz = va_arg(ap.va, size_t);
    }
    if (*nargs != 0) {
        ygglog_error("DirectMetaschemaType::serialize: %d arguments were not used.", *nargs);
        return -1;
    }
    // Copy message to buffer
    return copy_to_buffer(msg, msg_siz, buf, *buf_siz, allow_realloc);
}

int DirectMetaschemaType::serialize(char **buf, size_t *buf_siz,
                                    const int allow_realloc, YggGeneric *x) {
// Assumes null termination
    char *msg = NULL;
    size_t msg_siz = 0;
    x->get_data_realloc(&msg, &msg_siz);
// Copy message to buffer
    int out = copy_to_buffer(msg, msg_siz, buf, *buf_siz, allow_realloc);
    if (msg != NULL) {
        free(msg);
        msg = NULL;
    }
    return out;
}

int DirectMetaschemaType::deserialize(const char *buf, const size_t buf_siz,
                                      const int allow_realloc, size_t *nargs, va_list_t &ap) {
    if (nargs_exp() != *nargs) {
        ygglog_throw_error("DirectMetaschemaType::deserialize: %d arguments expected, but %d provided.",
                           nargs_exp(), *nargs);
    }
    const size_t nargs_orig = *nargs;
    *nargs = *nargs - nargs_exp();
// Assumes reallocation is allowed
    char **msg;
    char *msg_base;
    size_t *msg_siz;
    if (ap.using_ptrs) {
        if (allow_realloc) {
            msg = (char **) get_va_list_ptr_cpp(&ap);
        } else {
            msg_base = (char *) get_va_list_ptr_cpp(&ap);
            msg = &msg_base;
        }
        msg_siz = (size_t *) get_va_list_ptr_cpp(&ap);
    } else {
        if (allow_realloc) {
            msg = va_arg(ap.va, char**);
        } else {
            msg_base = va_arg(ap.va, char*);
            msg = &msg_base;
        }
        msg_siz = va_arg(ap.va, size_t*);
    }
// Copy message from buffer
    if (copy_to_buffer(buf, buf_siz, msg, *msg_siz, allow_realloc) < 0) {
        return -1;
    }
    if (*nargs != 0) {
        ygglog_error("DirectMetaschemaType::deserialize: %d arguments were not used.", *nargs);
        return -1;
    }
    return (int) (nargs_orig - *nargs);
}
