#include "AsciiTableMetaschemaType.hpp"

using namespace communication::datatypes::Metaschema;
using namespace communication::utils;

AsciiTableMetaschemaType::AsciiTableMetaschemaType(const char *format_str, const int as_array,
                                                   const bool use_generic) :
        MetaschemaType("ascii_table", use_generic), as_array_(as_array), table_(NULL) {
    table_ = (asciiTable_t *) malloc(sizeof(asciiTable_t));
    if (table_ == NULL)
        ygglog_throw_error("AsciiTableMetaschemaType: Failed to allocate table.");
    table_[0] = asciiTable("seri", "0", format_str, NULL, NULL, NULL);
}

AsciiTableMetaschemaType::AsciiTableMetaschemaType(PyObject *pyobj, const bool use_generic) :
        MetaschemaType(pyobj, use_generic), as_array_(0), table_(NULL) {
    // As array
    int as_array = 0;
    get_item_python_dict_c(pyobj, "as_array", &as_array,
                           "AsciiTableMetaschemaType: as_array: ",
                           T_INT, sizeof(int), true);
    update_as_array(as_array, true);
    // Format string
    char format_str[200];
    get_item_python_dict_c(pyobj, "format_str", format_str,
                           "AsciiTableMetaschemaType: format_str: ",
                           T_STRING, 200);
    // Create table
    table_ = (asciiTable_t *) malloc(sizeof(asciiTable_t));
    if (table_ == NULL)
        ygglog_throw_error("AsciiTableMetaschemaType: Failed to allocate table.");
    table_[0] = asciiTable("seri", "0", format_str, NULL, NULL, NULL);
}

AsciiTableMetaschemaType::~AsciiTableMetaschemaType() {
    if (table_ != NULL) {
        at_cleanup(table_);
        free(table_);
    }
}

bool AsciiTableMetaschemaType::operator==(const MetaschemaType &Ref) const {
    if (!(MetaschemaType::operator==(Ref)))
        return false;
    const AsciiTableMetaschemaType *pRef = dynamic_cast<const AsciiTableMetaschemaType *>(&Ref);
    if (as_array_ != pRef->as_array())
        return false;
    if (strcmp(format_str(), pRef->format_str()) != 0)
        return false;
    return true;
}

void AsciiTableMetaschemaType::display(const char *indent) const {
    MetaschemaType::display(indent);
    printf("%s%-15s = %s\n", indent, "format_str", format_str());
    printf("%s%-15s = %d\n", indent, "as_array", as_array_);
}

PyObject *AsciiTableMetaschemaType::as_python_dict() const {
    PyObject *out = MetaschemaType::as_python_dict();
    set_item_python_dict_c(out, "format_str", format_str(),
                           "AsciiTableMetaschemaType::as_python_dict: ",
                           T_STRING, LINE_SIZE_MAX);
    set_item_python_dict_c(out, "as_array", &as_array_,
                           "AsciiTableMetaschemaType::as_python_dict: ",
                           T_INT, sizeof(int) * 8);
    return out;
}

void AsciiTableMetaschemaType::update_as_array(const int new_as_array, bool force) {
    if ((!(force)) && (as_array_ != new_as_array)) {
        ygglog_throw_error("AsciiTableMetaschemaType::update_as_array: Cannot update as_array from %d to %d.",
                           as_array_, new_as_array);
    }
    int *as_array_modifier = const_cast<int *>(&as_array_);
    *as_array_modifier = new_as_array;
}

int serialize(char **buf, size_t *buf_siz,
              const int allow_realloc, size_t *nargs, va_list_t &ap) {
    UNUSED(allow_realloc);
    if (nargs_exp() != *nargs) {
        ygglog_throw_error("AsciiTableMetaschemaType::serialize: %d arguments expected, but %d provided.",
                           nargs_exp(), *nargs);
    }
    if (ap.using_ptrs) {
        ygglog_throw_error(
                "AsciiTableMetaschemaType::serialize: Pointer representation of variable argument list not yet supported.");
    }
    *nargs = *nargs - nargs_exp();
    // Assumes null termination
    int ret;
    if (as_array_) {
        ret = at_varray_to_bytes(*table_, *buf, *buf_siz, ap.va);
    } else {
        ret = at_vrow_to_bytes(*table_, *buf, *buf_siz, ap.va);
    }
    if (*nargs != 0) {
        ygglog_error("AsciiTableMetaschemaType::serialize: %d arguments were not used.", *nargs);
        return -1;
    }
    return ret;
}

int AsciiTableMetaschemaType::deserialize(const char *buf, const size_t buf_siz,
                                          const int allow_realloc, size_t *nargs, va_list_t &ap) {
    if (nargs_exp() != *nargs) {
        ygglog_throw_error("AsciiTableMetaschemaType::deserialize: %d arguments expected, but %d provided.",
                           nargs_exp(), *nargs);
    }
    if (ap.using_ptrs) {
        ygglog_throw_error(
                "AsciiTableMetaschemaType::deserialize: Pointer representation of variable argument list not yet supported.");
    }
    const size_t nargs_orig = *nargs;
    *nargs = *nargs - nargs_exp();
    int ret;
    if (as_array_) {
        ret = at_vbytes_to_array(*table_, buf, buf_siz, ap.va);
    } else {
        if (allow_realloc) {
            ygglog_error("AsciiTableMetaschemaType::deserialize: allow_realloc not supported for rows.");
            return -1;
        }
        ret = at_vbytes_to_row(*table_, buf, ap.va);
    }
    if (ret < 0) {
        ygglog_error("AsciiTableMetaschemaType::deserialize: Error using table.");
        return -1;
    } else if ((size_t) ret != nargs_exp()) {
        ygglog_error("AsciiTableMetaschemaType::deserialize: Table used %d arguments, but was expected to used %d.",
                     ret, nargs_exp());
        return -1;
    }
    if (*nargs != 0) {
        ygglog_error("AsciiTableMetaschemaType::deserialize: %d arguments were not used.", *nargs);
        return -1;
    }
    return (int) (nargs_orig - *nargs);
}
