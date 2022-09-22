#include "MetaschemaType.hpp"

using namespace communication::datatypes::Metaschema;
using namespace communication::utils;

MetaschemaType::MetaschemaType(const char *type, const bool use_generic,
               const bool always_generic) :
        type_((const char *) malloc(STRBUFF)), type_code_(-1), updated_(false),
        nbytes_(0), use_generic_(use_generic), always_generic_(always_generic) {
    if (always_generic_)
        update_use_generic(true);
    update_type(type);
}

MetaschemaType::MetaschemaType(const rapidjson::Value &type_doc,
               const bool use_generic,
               const bool always_generic) :
        type_((const char *) malloc(STRBUFF)), type_code_(-1), updated_(false),
        nbytes_(0), use_generic_(use_generic), always_generic_(always_generic) {
    if (always_generic_)
        update_use_generic(true);
    if (!(type_doc.IsObject()))
        ygglog_throw_error("MetaschemaType: Parsed document is not an object.");
    if (!(type_doc.HasMember("type")))
        ygglog_throw_error("MetaschemaType: Parsed header dosn't contain a type.");
    if (!(type_doc["type"].IsString()))
        ygglog_throw_error("MetaschemaType: Type in parsed header is not a string.");
    update_type(type_doc["type"].GetString());
}

MetaschemaType::MetaschemaType(PyObject *pyobj, const bool use_generic, const bool always_generic) :
type_((const char *) malloc(STRBUFF)), type_code_(-1), updated_(false),
nbytes_(0), use_generic_(use_generic), always_generic_(always_generic) {
    if (always_generic_)
        update_use_generic(true);
    if (!(PyDict_Check(pyobj))) {
        ygglog_throw_error("MetaschemaType: Python object must be a dict.");
    }
    char ctype[STRBUFF] = "";
    get_item_python_dict_c(pyobj, "type", ctype,
                           "MetaschemaType: type: ",
                           T_STRING, STRBUFF);
    update_type(ctype);
}

~MetaschemaType() {
    free((char *) type_);
}

bool operator==(const MetaschemaType &Ref) const {
    if (strcmp(type_, Ref.type()) != 0)
        return false;
    if (type_code_ != Ref.type_code())
        return false;
    return true;
}

bool operator!=(const MetaschemaType &Ref) const {
    if (operator==(Ref))
        return false;
    else
        return true;
}

void* MetaschemaType::copy_generic(const YggGeneric *data, void *orig_data) const {
    if (data == NULL) {
        ygglog_throw_error("MetaschemaType::copy_generic: Generic object is NULL.");
    }
    void *out = NULL;
    if (orig_data == NULL) {
        orig_data = data->get_data();
    }
    if (orig_data != NULL) {
        size_t nbytes_data = data->get_nbytes();
        void *temp = (void *) realloc(out, nbytes_data);
        if (temp == NULL) {
            ygglog_throw_error("MetaschemaType::copy_generic: Failed to realloc output pointer.");
        }
        out = temp;
        memcpy(out, orig_data, nbytes_data);
    }
    return out;
}

void MetaschemaType::free_generic(YggGeneric *data) const {
    if (data == NULL) {
        ygglog_throw_error("MetaschemaType::free_generic: Generic object is NULL.");
    }
    void **ptr = data->get_data_pointer();
    if (ptr[0] != NULL) {
        free(ptr[0]);
        ptr[0] = NULL;
    }
}

void MetaschemaType::display_generic(const YggGeneric *data, const char *indent) const {
    if (data == NULL) {
        ygglog_throw_error("MetaschemaType::display_generic: Generic object is NULL.");
    }
    const MetaschemaType *data_type = data->get_type();
    std::cout << indent;
    switch (data_type->type_code()) {
        case T_BOOLEAN: {
            bool arg = false;
            data->get_data(arg);
            if (arg)
                std::cout << "true" << std::endl;
            else
                std::cout << "false" << std::endl;
            return;
        }
        case T_INTEGER: {
            int arg = 0;
            data->get_data(arg);
            std::cout << arg << std::endl;
            return;
        }
        case T_NULL: {
            std::cout << "NULL" << std::endl;
            return;
        }
        case T_NUMBER: {
            double arg = 0.0;
            data->get_data(arg);
            std::cout << arg << std::endl;
            return;
        }
        case T_STRING: {
            char *arg = (char *) (data->get_data());
            std::cout << arg << std::endl;
            return;
        }
    }
    ygglog_throw_error("MetaschemaType::display_generic: Cannot display type '%s'.", data_type->type());
}

int MetaschemaType::check_type() const {
    std::map<const char *, int, strcomp> type_map = get_type_map();
    std::map<const char *, int, strcomp>::iterator it = type_map.find(type_);
    if (it == type_map.end()) {
        ygglog_throw_error("MetaschemaType: Unsupported type '%s'.", type_);
    }
    return it->second;
}

void MetaschemaType::update(const MetaschemaType *new_info) {
    if (new_info == NULL) {
        ygglog_throw_error("MetaschemaType::update: New type information is NULL.");
    }
    if (strcmp(type_, new_info->type()) != 0) {
        printf("New type:\n");
        new_info->display();
        printf("Existing type:\n");
        display();
        ygglog_throw_error("MetaschemaType::update: Cannot update type %s to type %s.",
                           type_, new_info->type());
    }
    updated_ = true;
}

size_t MetaschemaType::update_from_serialization_args(size_t *nargs, va_list_t &ap) {
    skip_before_.clear();
    skip_after_.clear();
    if (use_generic()) {
        YggGeneric gen_arg = pop_generic(nargs, ap, true);
        update_from_serialization_args((YggGeneric *) (gen_arg.obj));
        return 1;
    } else {
        switch (type_code_) {
            case T_BOOLEAN:
            case T_INTEGER: {
                if (ap.using_ptrs) {
                    get_va_list_ptr_cpp(&ap);
                } else {
                    va_arg(ap.va, int);
                }
                return 1;
            }
            case T_NULL: {
                if (ap.using_ptrs) {
                    get_va_list_ptr_cpp(&ap);
                } else {
                    va_arg(ap.va, void*);
                }
                return 1;
            }
            case T_NUMBER: {
                if (ap.using_ptrs) {
                    get_va_list_ptr_cpp(&ap);
                } else {
                    va_arg(ap.va, double);
                }
                return 1;
            }
            case T_STRING: {
                if (ap.using_ptrs) {
                    get_va_list_ptr_cpp(&ap);
                    get_va_list_ptr_cpp(&ap);
                } else {
                    va_arg(ap.va, char*);
                    va_arg(ap.va, size_t);
                }
                return 2;
            }
        }
    }
    return 0;
}

size_t MetaschemaType::update_from_deserialization_args(size_t *nargs, va_list_t &ap) {
    skip_before_.clear();
    skip_after_.clear();
    if (use_generic()) {
        YggGeneric *gen_arg = pop_generic_ptr(nargs, ap, true);
        update_from_deserialization_args((YggGeneric *) (gen_arg->obj));
        return 1;
    }
    return 0;
}


const size_t MetaschemaType::nbytes() const {
    switch (type_code_) {
        case T_BOOLEAN: {
            return sizeof(bool);
        }
        case T_INTEGER: {
            return sizeof(int);
        }
        case T_NULL: {
            return sizeof(NULL);
        }
        case T_NUMBER: {
            return sizeof(double);
        }
        case T_STRING: {
            if (nbytes_ == 0) {
                ygglog_throw_error("MetaschemaType::nbytes: String cannot have size of 0.");
            } else {
                return nbytes_;
            }
        }
    }
    ygglog_throw_error("MetaschemaType::nbytes: Cannot get number of bytes for type '%s'.", type_);
    return 0;
}

/*!
  @brief Get the number of bytes occupied by a variable of the type in a variable argument list.
  @returns std::vector<size_t> Number of bytes/variables occupied by the type.
 */
std::vector<size_t> MetaschemaType::nbytes_va_core() const {
    std::vector<size_t> out;
    if (use_generic()) {
        out.push_back(sizeof(YggGeneric));
    } else {
        switch (type_code_) {
            case T_NULL: {
                out.push_back(sizeof(void *));
                break;
            }
            case T_STRING: {
                out.push_back(sizeof(char *));
                out.push_back(sizeof(size_t));
                break;
            }
            default: {
                out.push_back(nbytes());
            }
        }
    }
    return out;
}

void MetaschemaType::skip_va_elements_core(size_t *nargs, va_list_t *ap) const {
    switch (type_code_) {
        case T_BOOLEAN: {
            va_arg(ap->va, int);
            (*nargs)--;
            return;
        }
        case T_INTEGER: {
            va_arg(ap->va, int);
            (*nargs)--;
            return;
        }
        case T_NULL: {
            va_arg(ap->va, void*);
            (*nargs)--;
            return;
        }
        case T_NUMBER: {
            va_arg(ap->va, double);
            (*nargs)--;
            return;
        }
        case T_STRING: {
            va_arg(ap->va, char*);
            (*nargs)--;
            return;
        }
        default: {
            ygglog_error("MetaschemaType::skip_va_elements_core: Cannot skip arguments for type '%s'.", type_);
        }
    }
}

/*!
  @brief Skip arguments that make of this type.
  @param[in, out] nargs Pointer to number of arguments in ap.
  @param[in, out] ap va_list_t Variable argument list.
 */
void MetaschemaType::skip_va_elements_wrap(size_t *nargs, va_list_t *ap) const {
    size_t i;
    if (ap->using_ptrs) {
        ap->iptr = ap->iptr + nargs_exp();
        (*nargs) = (*nargs) - nargs_exp();
        return;
    }
    for (i = 0; i < skip_before_.size(); i++) {
        va_list_t_skip(ap, skip_before_[i]);
        (*nargs)--;
    }
    if (use_generic()) {
        pop_generic(nargs, *ap);
    } else {
        skip_va_elements_core(nargs, ap);
    }
    for (i = 0; i < skip_after_.size(); i++) {
        va_list_t_skip(ap, skip_after_[i]);
        (*nargs)--;
    }
}

size_t MetaschemaType::nargs_exp() const {
    switch (type_code_) {
        case T_BOOLEAN:
        case T_INTEGER:
        case T_NULL:
        case T_NUMBER: {
            return 1;
        }
        case T_STRING: {
            // Add length of sting to be consistent w/ bytes and unicode types
            return 2;
        }
    }
    ygglog_throw_error("MetaschemaType::nargs_exp: Cannot get number of expected arguments for type '%s'.", type_);
    return 0;
}

YggGeneric* MetaschemaType::python2c(PyObject *pyobj) const {
YggGeneric *cobj = new YggGeneric(this, NULL, 0);
void **data = cobj->get_data_pointer();
void *idata = (void *) realloc(data[0], nbytes());
if (idata == NULL) {
ygglog_throw_error("MetaschemaType::python2c: Failed to realloc data.");
}
void *dst = idata;
size_t precision = 0;
switch (type_code_) {
case T_BOOLEAN: {
precision = 8;
break;
}
case T_INTEGER: {
precision = 8 * sizeof(int);
break;
}
case T_NULL: {
break;
}
case T_NUMBER: {
precision = 8 * sizeof(double);
break;
}
case T_STRING: {
dst = (void *) (&idata);
break;
}
default: {
ygglog_throw_error("MetaschemaType::python2c: Cannot convert type '%s'.", type_);
}
}
convert_python2c(pyobj, dst, type_code_,
"MetaschemaType::python2c: ",
precision);
if (type_code_ == T_STRING) {
cobj->set_nbytes(strlen((char *) idata));
}
data[0] = idata;
return cobj;
}

PyObject* MetaschemaType::c2python(YggGeneric *cobj) const {
    PyObject *pyobj = NULL;
    void *src = cobj->get_data();
    size_t precision = 0;
    switch (type_code_) {
        case T_BOOLEAN: {
            precision = 8 * sizeof(bool);
            break;
        }
        case T_INTEGER: {
            precision = 8 * sizeof(int);
            break;
        }
        case T_NULL: {
            break;
        }
        case T_NUMBER: {
            precision = 8 * sizeof(double);
            break;
        }
        case T_STRING: {
            src = (void *) (cobj->get_data_pointer());
            break;
        }
        default: {
            ygglog_throw_error("MetaschemaType::c2python: Cannot convert type '%s'.", type_);
        }
    }
    pyobj = convert_c2python(src, type_code_,
                             "MetaschemaType::c2python: ",
                             precision);
    return pyobj;
}

/*!
  @brief Return the recovered generic structure if one is present in
  the variable argument list by removing it.
  @param[in] nargs size_t* Pointer to number of arguments present in ap
  that will be decremented by 1.
  @param[in] ap va_list_t Variable argument list.
  @param[in] skip_nargs_dec bool If true, nargs will not be modified.
  Defaults to false.
  @returns YggGeneric* Generic structure if one is present, NULL otherwise.
*/
YggGeneric* MetaschemaType::pop_generic_ptr(size_t *nargs, va_list_t &ap, bool skip_nargs_dec = false) const {
    if (skip_nargs_dec)
        (*nargs)++;
    YggGeneric *gen_arg = pop_generic_va_ptr(nargs, &ap);
    if (gen_arg == NULL) {
        ygglog_throw_error(
                "MetaschemaType::pop_generic_ptr: Type expects pointer to generic object, but one was not provided.");
    }
    if (gen_arg->obj == NULL) {
        gen_arg->obj = (void *) (new YggGeneric(this, NULL, 0));
    }
    return gen_arg;
}

// Encoding


/*!
  @brief Encode arguments describine an instance of this type into a JSON string.
  @param[in] writer rapidjson::Writer<rapidjson::StringBuffer> rapidjson writer.
  @param[in,out] nargs size_t * Pointer to the number of arguments contained in
  ap. On return it will be set to the number of arguments used.
  @param[in] ap va_list_t Variable number of arguments that should be encoded
  as a JSON string.
  @returns bool true if the encoding was successful, false otherwise.
 */
bool MetaschemaType::encode_data(rapidjson::Writer<rapidjson::StringBuffer> *writer,
                         size_t *nargs, va_list_t &ap) const {
    if (nargs_exp() > *nargs)
        ygglog_throw_error("MetaschemaType::encode_data: %d arguments expected, but only %d provided.",
                           nargs_exp(), *nargs);
    switch (type_code_) {
        case T_BOOLEAN: {
            int arg;
            if (ap.using_ptrs) {
                arg = ((int *) get_va_list_ptr_cpp(&ap))[0];
            } else {
                arg = va_arg(ap.va, int);
            }
            (*nargs)--;
            if (arg == 0)
                writer->Bool(false);
            else
                writer->Bool(true);
            return true;
        }
        case T_INTEGER: {
            int arg;
            if (ap.using_ptrs) {
                arg = ((int *) get_va_list_ptr_cpp(&ap))[0];
            } else {
                arg = va_arg(ap.va, int);
            }
            (*nargs)--;
            writer->Int(arg);
            return true;
        }
        case T_NULL: {
            if (ap.using_ptrs) {
                get_va_list_ptr_cpp(&ap);
            } else {
                va_arg(ap.va, void*);
            }
            (*nargs)--;
            writer->Null();
            return true;
        }
        case T_NUMBER: {
            double arg;
            if (ap.using_ptrs) {
                arg = ((double *) get_va_list_ptr_cpp(&ap))[0];
            } else {
                arg = va_arg(ap.va, double);
            }
            (*nargs)--;
            writer->Double(arg);
            return true;
        }
        case T_STRING: {
            char *arg;
            size_t arg_siz;
            if (ap.using_ptrs) {
                arg = (char *) get_va_list_ptr_cpp(&ap);
                arg_siz = ((size_t *) get_va_list_ptr_cpp(&ap))[0];
            } else {
                arg = va_arg(ap.va, char*);
                arg_siz = va_arg(ap.va, size_t);
            }
            (*nargs)--;
            (*nargs)--;
            writer->String(arg, (rapidjson::SizeType) arg_siz);
            return true;
        }
    }
    ygglog_error("MetaschemaType::encode_data: Cannot encode data of type '%s'.", type_);
    return false;
}

bool MetaschemaType::encode_data_wrap(rapidjson::Writer<rapidjson::StringBuffer> *writer,
                      size_t *nargs, va_list_t &ap) const {
    bool out;
    size_t i;
    for (i = 0; i < skip_before_.size(); i++) {
        va_list_t_skip(&ap, skip_before_[i]);
        (*nargs)--;
    }
    if (use_generic()) {
        YggGeneric gen_arg = pop_generic(nargs, ap);
        out = encode_data(writer, (YggGeneric *) (gen_arg.obj));
    } else {
        out = encode_data(writer, nargs, ap);
    }
    for (i = 0; i < skip_after_.size(); i++) {
        va_list_t_skip(&ap, skip_after_[i]);
        (*nargs)--;
    }
    return out;
}

bool encode_data_wrap(rapidjson::Writer<rapidjson::StringBuffer> *writer,
                      size_t *nargs, ...) const {
    va_list_t ap_s = init_va_list();
    va_start(ap_s.va, nargs);
    bool out = encode_data_wrap(writer, nargs, ap_s);
    va_end(ap_s.va);
    return out;
}

bool encode_data(rapidjson::Writer<rapidjson::StringBuffer> *writer,
                         YggGeneric *x) const {
    size_t nargs = 1;
    switch (type_code_) {
        case T_BOOLEAN: {
            bool arg = false;
            x->get_data(arg);
            return encode_data(writer, &nargs, arg);
        }
        case T_INTEGER: {
            int arg = 0;
            x->get_data(arg);
            return encode_data(writer, &nargs, arg);
        }
        case T_NULL: {
            void *arg = NULL;
            return encode_data(writer, &nargs, arg);
        }
        case T_NUMBER: {
            double arg = 0.0;
            x->get_data(arg);
            return encode_data(writer, &nargs, arg);
        }
        case T_STRING: {
            nargs = 2;
            char *arg = NULL;
            size_t arg_siz = 0;
            x->get_data_realloc(&arg, &arg_siz);
            bool out = encode_data(writer, &nargs, arg, arg_siz);
            if (arg != NULL) {
                free(arg);
                arg = NULL;
            }
            return out;
        }
    }
    ygglog_error("MetaschemaType::encode_data: Cannot encode data of type '%s'.", type_);
    return false;
}

int MetaschemaType::copy_to_buffer(const char *src_buf, const size_t src_buf_siz,
                           char **dst_buf, size_t &dst_buf_siz,
                           const int allow_realloc, bool skip_terminal = false) const {
    size_t src_buf_siz_term = src_buf_siz;
    if (!(skip_terminal))
        src_buf_siz_term++;
    if (src_buf_siz_term > dst_buf_siz) {
        if (allow_realloc == 1) {
            dst_buf_siz = src_buf_siz_term;
            char *temp = (char *) realloc(*dst_buf, dst_buf_siz);
            if (temp == NULL) {
                ygglog_error("MetaschemaType::copy_to_buffer: Failed to realloc destination buffer to %lu bytes.",
                             dst_buf_siz);
                return -1;
            }
            *dst_buf = temp;
            ygglog_debug("MetaschemaType::copy_to_buffer: Reallocated to %lu bytes.",
                         dst_buf_siz);
        } else {
            if (!(skip_terminal)) {
                ygglog_error(
                        "MetaschemaType::copy_to_buffer: Source with termination character (%lu + 1) exceeds size of destination buffer (%lu).",
                        src_buf_siz, dst_buf_siz);
            } else {
                ygglog_error(
                        "MetaschemaType::copy_to_buffer: Source (%lu) exceeds size of destination buffer (%lu).",
                        src_buf_siz, dst_buf_siz);
            }
            return -1;
        }
    }
    memcpy(*dst_buf, src_buf, src_buf_siz);
    if (!(skip_terminal)) {
        size_t i;
        for (i = src_buf_siz; i < dst_buf_siz; i++)
            (*dst_buf)[i] = '\0';
    }
    return (int) src_buf_siz;
}

int MetaschemaType::serialize(char **buf, size_t *buf_siz,
                      const int allow_realloc, size_t *nargs, va_list_t &ap) {
    if (use_generic()) {
        YggGeneric gen_arg = pop_generic(nargs, ap);
        return serialize(buf, buf_siz, allow_realloc,
                         (YggGeneric *) (gen_arg.obj));
    }
    va_list_t ap_copy = copy_va_list(ap);
    update_from_serialization_args(nargs, ap_copy);
    if (nargs_exp() != *nargs) {
        ygglog_throw_error("MetaschemaType::serialize: %d arguments expected, but %d provided.",
                           nargs_exp(), *nargs);
    }
    rapidjson::StringBuffer body_buf;
    rapidjson::Writer<rapidjson::StringBuffer> body_writer(body_buf);
    bool out = encode_data_wrap(&body_writer, nargs, ap);
    if (!(out)) {
        return -1;
    }
    if (*nargs != 0) {
        ygglog_error("MetaschemaType::serialize: %d arguments were not used.", *nargs);
        return -1;
    }
    // Copy message to buffer
    return copy_to_buffer(body_buf.GetString(), body_buf.GetSize(),
                          buf, *buf_siz, allow_realloc);
}

int MetaschemaType::serialize(char **buf, size_t *buf_siz,
                      const int allow_realloc, YggGeneric *x) {
    update_from_serialization_args(x);
    if (*(x->get_type()) != (*this)) {
        ygglog_throw_error("MetaschemaType::serialize: "
                           "Type associated with provided generic "
                           "object is not equivalent to the type "
                           "associated with the communication object "
                           "performing the serialization.");
    }
    rapidjson::StringBuffer body_buf;
    rapidjson::Writer<rapidjson::StringBuffer> body_writer(body_buf);
    bool out = encode_data(&body_writer, x);
    if (!(out)) {
        return -1;
    }
    // Copy message to buffer
    return copy_to_buffer(body_buf.GetString(), body_buf.GetSize(),
                          buf, *buf_siz, allow_realloc);
}

bool MetaschemaType::decode_data(rapidjson::Value &data, const int allow_realloc,
                         size_t *nargs, va_list_t &ap) const {
    if (nargs_exp() != *nargs) {
        ygglog_throw_error("MetaschemaType::decode_data: %d arguments expected, but %d provided.",
                           nargs_exp(), *nargs);
    }
    switch (type_code_) {
        case T_BOOLEAN: {
            if (!(data.IsBool()))
                ygglog_throw_error("MetaschemaType::decode_data: Data is not a bool.");
            bool *arg;
            bool **p;
            if (allow_realloc) {
                if (ap.using_ptrs) {
                    p = (bool **) get_va_list_ptr_ref_cpp(&ap);
                } else {
                    p = va_arg(ap.va, bool**);
                }
                if (ap.for_fortran) {
                    arg = *p;
                } else {
                    arg = (bool *) realloc(*p, sizeof(bool));
                }
                if (arg == NULL)
                    ygglog_throw_error("MetaschemaType::decode_data: could not realloc bool pointer.");
                *p = arg;
            } else {
                if (ap.using_ptrs) {
                    arg = (bool *) get_va_list_ptr_cpp(&ap);
                } else {
                    arg = va_arg(ap.va, bool*);
                }
            }
            (*nargs)--;
            arg[0] = data.GetBool();
            return true;
        }
        case T_INTEGER: {
            if (!(data.IsInt()))
                ygglog_throw_error("MetaschemaType::decode_data: Data is not an int.");
            int *arg;
            int **p;
            if (allow_realloc) {
                if (ap.using_ptrs) {
                    p = (int **) get_va_list_ptr_ref_cpp(&ap);
                } else {
                    p = va_arg(ap.va, int**);
                }
                if (ap.for_fortran) {
                    arg = *p;
                } else {
                    arg = (int *) realloc(*p, sizeof(int));
                }
                if (arg == NULL)
                    ygglog_throw_error("MetaschemaType::decode_data: could not realloc int pointer.");
                *p = arg;
            } else {
                if (ap.using_ptrs) {
                    arg = (int *) get_va_list_ptr_cpp(&ap);
                } else {
                    arg = va_arg(ap.va, int*);
                }
            }
            (*nargs)--;
            arg[0] = data.GetInt();
            return true;
        }
        case T_NULL: {
            if (!(data.IsNull()))
                ygglog_throw_error("MetaschemaType::decode_data: Data is not null.");
            void **arg;
            void ***p;
            if (allow_realloc) {
                if (ap.using_ptrs) {
                    p = (void ***) get_va_list_ptr_ref_cpp(&ap);
                } else {
                    p = va_arg(ap.va, void***);
                }
                if (ap.for_fortran) {
                    arg = *p;
                } else {
                    arg = (void **) realloc(*p, sizeof(void *));
                }
                if (arg == NULL)
                    ygglog_throw_error("MetaschemaType::decode_data: could not realloc void* pointer.");
                *p = arg;
            } else {
                if (ap.using_ptrs) {
                    arg = (void **) get_va_list_ptr_cpp(&ap);
                } else {
                    arg = va_arg(ap.va, void**);
                }
            }
            (*nargs)--;
            arg[0] = NULL;
            return true;
        }
        case T_NUMBER: {
            if (!(data.IsDouble()))
                ygglog_throw_error("MetaschemaType::decode_data: Data is not a double.");
            double *arg;
            double **p;
            if (allow_realloc) {
                if (ap.using_ptrs) {
                    p = (double **) get_va_list_ptr_ref_cpp(&ap);
                } else {
                    p = va_arg(ap.va, double**);
                }
                if (ap.for_fortran) {
                    arg = *p;
                } else {
                    arg = (double *) realloc(*p, sizeof(double));
                }
                if (arg == NULL)
                    ygglog_throw_error("MetaschemaType::decode_data: could not realloc double pointer.");
                *p = arg;
            } else {
                if (ap.using_ptrs) {
                    arg = (double *) get_va_list_ptr_cpp(&ap);
                } else {
                    arg = va_arg(ap.va, double*);
                }
            }
            (*nargs)--;
            arg[0] = data.GetDouble();
            return true;
        }
        case T_STRING: {
            if (!(data.IsString()))
                ygglog_throw_error("MetaschemaType::decode_data: Data is not a string.");
            char *arg;
            char **p;
            if (allow_realloc) {
                if (ap.using_ptrs) {
                    p = (char **) get_va_list_ptr_ref_cpp(&ap);
                } else {
                    p = va_arg(ap.va, char**);
                }
                arg = *p;
            } else {
                if (ap.using_ptrs) {
                    arg = (char *) get_va_list_ptr_cpp(&ap);
                } else {
                    arg = va_arg(ap.va, char*);
                }
                p = &arg;
            }
            size_t *arg_siz;
            if (ap.using_ptrs) {
                arg_siz = (size_t *) get_va_list_ptr_cpp(&ap);
            } else {
                arg_siz = va_arg(ap.va, size_t*);
            }
            (*nargs)--;
            (*nargs)--;
            int ret = copy_to_buffer(data.GetString(), data.GetStringLength(),
                                     p, *arg_siz, allow_realloc);
            if (ret < 0) {
                ygglog_error("MetaschemaType::decode_data: Failed to copy string buffer.");
                return false;
            }
            return true;
        }
    }
    ygglog_error("MetaschemaType::decode_data: Cannot decode data of type '%s'.", type_);
    return false;
}

bool MetaschemaType::decode_data_wrap(rapidjson::Value &data, const int allow_realloc,
                      size_t *nargs, va_list_t &ap) const {
    bool out;
    size_t i;
    for (i = 0; i < skip_before_.size(); i++) {
        va_list_t_skip(&ap, sizeof(void *));
        (*nargs)--;
    }
    if (use_generic()) {
        YggGeneric *gen_arg = pop_generic_ptr(nargs, ap);
        out = decode_data(data, (YggGeneric *) (gen_arg->obj));
    } else {
        out = decode_data(data, allow_realloc, nargs, ap);
    }
    for (i = 0; i < skip_after_.size(); i++) {
        va_list_t_skip(&ap, sizeof(void *));
        (*nargs)--;
    }
    return out;
}

bool MetaschemaType::decode_data(rapidjson::Value &data, YggGeneric *x) const {
    size_t nargs = 1;
    int allow_realloc = 1;
    if (x == NULL) {
        ygglog_throw_error("MetaschemaType::decode_data: Generic wrapper is not initialized.");
    }
    void **arg = x->get_data_pointer();
    if (type_code_ == T_STRING) {
        nargs = 2;
        size_t *arg_siz = x->get_nbytes_pointer();
        return decode_data(data, allow_realloc, &nargs, arg, arg_siz);
    } else {
        return decode_data(data, allow_realloc, &nargs, arg);
    }
}

int MetaschemaType::deserialize(const char *buf, const size_t buf_siz,
                        const int allow_realloc, size_t *nargs, va_list_t &ap) {
    if (use_generic()) {
        YggGeneric *gen_arg = pop_generic_ptr(nargs, ap);
        return deserialize(buf, buf_siz, (YggGeneric *) (gen_arg->obj));
    }
    const size_t nargs_orig = *nargs;
    va_list_t ap_copy = copy_va_list(ap);
    update_from_deserialization_args(nargs, ap_copy);
    if (nargs_exp() != *nargs) {
        ygglog_throw_error("MetaschemaType::deserialize: %d arguments expected, but %d provided.",
                           nargs_exp(), *nargs);
    }
    // Parse body
    rapidjson::Document body_doc;
    body_doc.Parse(buf, buf_siz);
    bool out = decode_data_wrap(body_doc, allow_realloc, nargs, ap);
    if (!(out)) {
        ygglog_error("MetaschemaType::deserialize: One or more errors while parsing body.");
        return -1;
    }
    if (*nargs != 0) {
        ygglog_error("MetaschemaType::deserialize: %d arguments were not used.", *nargs);
        return -1;
    }
    return (int) (nargs_orig - *nargs);
}

int MetaschemaType::deserialize(const char *buf, const size_t buf_siz,
                        YggGeneric *x) {
    update_from_deserialization_args(x);
    if (x->get_type() == NULL) {
        ygglog_throw_error("MetaschemaType::deserialize: "
                           "The type associated with the generic "
                           "object is NULL.");
    }
    if (*(x->get_type()) != (*this)) {
        printf("Generic object's type:\n");
        x->get_type()->display();
        printf("Deserializing type:\n");
        display();
        ygglog_throw_error("MetaschemaType::deserialize: "
                           "Type associated with provided generic "
                           "object is not equivalent to the type "
                           "associated with the communication object "
                           "performing the deserialization.");
    }
    // Parse body
    rapidjson::Document body_doc;
    body_doc.Parse(buf, buf_siz);
    if (x->get_data() != NULL) {
        ygglog_info("MetaschemaType::deserialize: The generic object where deserialized results are to be stored already contains information. Freeing it.");
        x->free_data();
    }
    bool out = decode_data(body_doc, x);
    if (!(out)) {
        ygglog_error("MetaschemaType::deserialize: One or more errors while parsing body.");
        return -1;
    }
    return 0;
}
