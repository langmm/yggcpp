#include "ScalarMetaschemaType.hpp"
#include "JSONArrayMetaschemaType.hpp"
#include <iostream>

using namespace communication::datatypes::Metaschema;
using namespace communication::utils;

ScalarMetaschemaType::ScalarMetaschemaType(const char *subtype, const size_t precision,
                                           const char *units, const bool use_generic) :
        MetaschemaType("scalar", use_generic), subtype_((const char *) malloc(STRBUFF)), subtype_code_(-1),
        precision_(precision), units_((const char *) malloc(STRBUFF)), cast_precision_(0) {
    if (subtype_ == NULL) {
        ygglog_throw_error("ScalarMetaschemaType: Failed to malloc subtype.");
    }
    if (units_ == NULL) {
        ygglog_throw_error("ScalarMetaschemaType: Failed to malloc units.");
    }
    if (precision_ == 0)
        _variable_precision = true;
    else
        _variable_precision = false;
    update_subtype(subtype, true);
    update_units(units, true);
    _in_table = false;
}

ScalarMetaschemaType::ScalarMetaschemaType(const rapidjson::Value &type_doc,
                                           const bool use_generic) :
        MetaschemaType(type_doc, use_generic), subtype_((const char *) malloc(STRBUFF)), subtype_code_(-1),
        precision_(0), units_((const char *) malloc(STRBUFF)), cast_precision_(0) {
    if (subtype_ == NULL) {
        ygglog_throw_error("ScalarMetaschemaType: Failed to malloc subtype.");
    }
    if (units_ == NULL) {
        ygglog_throw_error("ScalarMetaschemaType: Failed to malloc units.");
    }
    switch (type_code()) {
        case T_1DARRAY:
        case T_NDARRAY:
        case T_SCALAR:
            // Subtype
            if (!(type_doc.HasMember("subtype"))) {
                ygglog_throw_error("ScalarMetaschemaType: %s type must include 'subtype'.", type());
            }
            if (!(type_doc["subtype"].IsString())) {
                ygglog_throw_error("ScalarMetaschemaType: 'subtype' value must be a string.");
            }
            update_subtype(type_doc["subtype"].GetString(), true);
            break;
        default:
            update_subtype(type(), true);
            update_type("scalar");
    }
    // Precision
    if (!(type_doc.HasMember("precision")))
        ygglog_throw_error("ScalarMetaschemaType: Precision missing.");
    if (type_doc["precision"].IsInt()) {
        set_precision(type_doc["precision"].GetInt(), true);
    } else if (type_doc["precision"].IsDouble()) {
        set_precision((size_t) (type_doc["precision"].GetDouble(), true));
    } else {
        ygglog_throw_error("ScalarMetaschemaType: Precision must be a number.");
    }
    // Units
    if (type_doc.HasMember("units")) {
        if (!type_doc["units"].IsString())
            ygglog_throw_error("ScalarMetaschemaType: Units must be a string.");
        update_units(type_doc["units"].GetString(), true);
    } else {
        update_units("", true);
    }
    // Set variable precision
    if (precision_ == 0)
        _variable_precision = true;
    else
        _variable_precision = false;
    _in_table = false;
}

ScalarMetaschemaType::ScalarMetaschemaType(PyObject *pyobj, const bool use_generic) :
        MetaschemaType(pyobj, use_generic), subtype_((const char *) malloc(STRBUFF)), subtype_code_(-1),
        precision_(0), units_((const char *) malloc(STRBUFF)), cast_precision_(0) {
// Subtype
    char subtype[STRBUFF] = "";
    get_item_python_dict_c(pyobj, "subtype", subtype,
                           "ScalarMetaschemaType: subtype: ",
                           T_STRING, STRBUFF);
    update_subtype(subtype, true);
// Precision
    size_t precision = 0;
    get_item_python_dict_c(pyobj, "precision", &precision,
                           "ScalarMetaschemaType: precision: ",
                           T_INT, sizeof(size_t) * 8);
    set_precision(precision, true);
// Units
    char units[STRBUFF] = "";
    get_item_python_dict_c(pyobj, "units", units,
                           "ScalarMetaschemaType: units: ",
                           T_STRING, STRBUFF, true);
    update_units(units, true);
// Set variable precision
    if (precision_ == 0)
        _variable_precision = true;
    else
        _variable_precision = false;
    _in_table = false;
}

bool ScalarMetaschemaType::operator==(const MetaschemaType &Ref) const {
    if (!(MetaschemaType::operator==(Ref)))
        return false;
    const ScalarMetaschemaType *pRef = dynamic_cast<const ScalarMetaschemaType *>(&Ref);
    if (!pRef)
        return false;
    if (strcmp(subtype_, pRef->subtype()) != 0)
        return false;
    if (subtype_code_ != pRef->subtype_code())
        return false;
    if ((!(_variable_precision)) && (precision_ != pRef->precision()))
        return false;
    if (strcmp(units_, pRef->units()) != 0)
        return false;
    return true;
}

PyObject *ScalarMetaschemaType::as_python_dict() const {
    PyObject *out = MetaschemaType::as_python_dict();
    set_item_python_dict_c(out, "subtype", subtype_,
                           "ScalarMetaschemaType::as_python_dict: ",
                           T_STRING, STRBUFF);
    set_item_python_dict_c(out, "precision", &precision_,
                           "ScalarMetaschemaType::as_python_dict: ",
                           T_INT, sizeof(size_t) * 8);
    set_item_python_dict_c(out, "units", units_,
                           "ScalarMetaschemaType::as_python_dict: ",
                           T_STRING, STRBUFF);
    return out;
}

void ScalarMetaschemaType::display_generic(const YggGeneric *data, const char *indent) const {
    size_t i;
    if (data == NULL) {
        ygglog_throw_error("ScalarMetaschemaType::display_generic: Generic object is NULL.");
    }
    size_t bytes_precision = (data->get_nbytes()) / (data->get_nelements());
    std::cout << indent;
    switch (subtype_code_) {
        case T_INT: {
            switch (precision_) {
                case 8: {
                    int8_t *arg = (int8_t *) (data->get_data());
                    for (i = 0; i < data->get_nelements(); i++)
                        std::cout << arg[i] << " ";
                    std::cout << std::endl;
                    return;
                }
                case 16: {
                    int16_t *arg = (int16_t *) (data->get_data());
                    for (i = 0; i < data->get_nelements(); i++)
                        std::cout << arg[i] << " ";
                    std::cout << std::endl;
                    return;
                }
                case 32: {
                    int32_t *arg = (int32_t *) (data->get_data());
                    for (i = 0; i < data->get_nelements(); i++)
                        std::cout << arg[i] << " ";
                    std::cout << std::endl;
                    return;
                }
                case 64: {
                    int64_t *arg = (int64_t *) (data->get_data());
                    for (i = 0; i < data->get_nelements(); i++)
                        std::cout << arg[i] << " ";
                    std::cout << std::endl;
                    return;
                }
                default: {
                    ygglog_error("ScalarMetaschemaType::display_generic: Unsupported integer precision '%lu'.",
                                 precision_);
                    return;
                }
            }
            break;
        }
        case T_UINT: {
            switch (precision_) {
                case 8: {
                    uint8_t *arg = (uint8_t *) (data->get_data());
                    for (i = 0; i < data->get_nelements(); i++)
                        std::cout << arg[i] << " ";
                    std::cout << std::endl;
                    return;
                }
                case 16: {
                    uint16_t *arg = (uint16_t *) (data->get_data());
                    for (i = 0; i < data->get_nelements(); i++)
                        std::cout << arg[i] << " ";
                    std::cout << std::endl;
                    return;
                }
                case 32: {
                    uint32_t *arg = (uint32_t *) (data->get_data());
                    for (i = 0; i < data->get_nelements(); i++)
                        std::cout << arg[i] << " ";
                    std::cout << std::endl;
                    return;
                }
                case 64: {
                    uint64_t *arg = (uint64_t *) (data->get_data());
                    for (i = 0; i < data->get_nelements(); i++)
                        std::cout << arg[i] << " ";
                    std::cout << std::endl;
                    return;
                }
                default: {
                    ygglog_error(
                            "ScalarMetaschemaType::display_generic: Unsupported unsigned integer precision '%lu'.",
                            precision_);
                    return;
                }
            }
            break;
        }
        case T_FLOAT: {
            if (sizeof(float) == bytes_precision) {
                float *arg = (float *) (data->get_data());
                for (i = 0; i < data->get_nelements(); i++)
                    std::cout << arg[i] << " ";
                std::cout << std::endl;
                return;
            } else if (sizeof(double) == bytes_precision) {
                double *arg = (double *) (data->get_data());
                for (i = 0; i < data->get_nelements(); i++)
                    std::cout << arg[i] << " ";
                std::cout << std::endl;
                return;
            } else if (sizeof(long double) == bytes_precision) {
                long double *arg = (long double *) (data->get_data());
                for (i = 0; i < data->get_nelements(); i++)
                    std::cout << arg[i] << " ";
                std::cout << std::endl;
                return;
            } else {
                ygglog_error(
                        "ScalarMetaschemaType::display_generic: Unsupported float precision '%lu bit' (%lu bytes).",
                        precision_, bytes_precision);
                return;
            }
            break;
        }
        case T_COMPLEX: {
            if (sizeof(float) == (bytes_precision / 2)) {
#ifdef _WIN32
                complex_double_t* arg = (complex_double_t*)(data->get_data());
#else
                complex_float_t *arg = (complex_float_t *) (data->get_data());
#endif
                for (i = 0; i < data->get_nelements(); i++)
                    std::cout << arg[i].re << "+" << arg[i].im << "j ";
                std::cout << std::endl;
                return;
            } else if (sizeof(double) == (bytes_precision / 2)) {
                complex_double_t *arg = (complex_double_t *) (data->get_data());
                for (i = 0; i < data->get_nelements(); i++)
                    std::cout << arg[i].re << "+" << arg[i].im << "j ";
                std::cout << std::endl;
                return;
            } else if (sizeof(long double) == (bytes_precision / 2)) {
                complex_long_double_t *arg = (complex_long_double_t *) (data->get_data());
                for (i = 0; i < data->get_nelements(); i++)
                    std::cout << arg[i].re << "+" << arg[i].im << "j ";
                std::cout << std::endl;
                return;
            } else {
                ygglog_error("ScalarMetaschemaType::display_generic: Unsupported complex precision '%lu'.",
                             precision_);
                return;
            }
            break;
        }
        case T_BYTES: {
// TODO: Handle array of char arrays
            char *arg = (char *) (data->get_data());
            std::cout << arg << std::endl;
            return;
        }
        case T_UNICODE: {
// TODO: Handle array of char arrays
            char *arg = (char *) (data->get_data());
            for (i = 0; i < data->get_nbytes(); i += 4) {
                std::cout << arg + i;
            }
            std::cout << std::endl;
            return;
        }
        default: {
            ygglog_error("ScalarMetaschemaType::display_generic: Unsupported subtype '%s'.",
                         subtype_);
            return;
        }
    }
}


std::vector<size_t> ScalarMetaschemaType::nbytes_va_core() const {
    std::vector<size_t> out;
    if (!(use_generic())) {
        switch (type_code()) {
            case T_1DARRAY:
            case T_NDARRAY: {
                out.push_back(sizeof(unsigned char *));
                return out;
            }
            case T_SCALAR: {
                switch (subtype_code_) {
                    case T_BYTES:
                    case T_UNICODE: {
                        out.push_back(sizeof(char *));
                        out.push_back(sizeof(size_t));
                        return out;
                    }
                }
            }
        }
    }
    return MetaschemaType::nbytes_va_core();
}

/*!
  @brief Skip arguments that make of this type.
  @param[in, out] nargs Pointer to number of arguments in ap.
  @param[in, out] ap va_list_t Variable argument list.
 */
void ScalarMetaschemaType::skip_va_elements_core(size_t *nargs, struct va_list_t *ap) const {
    switch (type_code()) {
        case T_1DARRAY:
        case T_NDARRAY: {
            va_arg(ap->va, unsigned char*);
            (*nargs)--;
            return;
        }
        case T_SCALAR: {
            switch (subtype_code_) {
                case T_INT: {
                    switch (precision_) {
                        case 8: {
                            va_arg(ap->va, int);
                            break;
                        }
                        case 16: {
                            va_arg(ap->va, int);
                            break;
                        }
                        case 32: {
                            va_arg(ap->va, int32_t);
                            break;
                        }
                        case 64: {
                            va_arg(ap->va, int64_t);
                            break;
                        }
                        default: {
                            ygglog_throw_error(
                                    "ScalarMetaschemaType::skip_va_elements_core: Unsupported integer precision '%lu'.",
                                    precision_);
                        }
                    }
                    break;
                }
                case T_UINT: {
                    switch (precision_) {
                        case 8: {
                            va_arg(ap->va, unsigned int);
                            break;
                        }
                        case 16: {
                            va_arg(ap->va, unsigned int);
                            break;
                        }
                        case 32: {
                            va_arg(ap->va, uint32_t);
                            break;
                        }
                        case 64: {
                            va_arg(ap->va, uint64_t);
                            break;
                        }
                        default: {
                            ygglog_throw_error(
                                    "ScalarMetaschemaType::skip_va_elements_core: Unsupported unsigned integer precision '%lu'.",
                                    precision_);
                        }
                    }
                    break;
                }
                case T_FLOAT: {
                    size_t bytes_precision = nbytes();
                    if (sizeof(float) == bytes_precision) {
                        va_arg(ap->va, double);
                    } else if (sizeof(double) == bytes_precision) {
                        va_arg(ap->va, double);
                    } else if (sizeof(long double) == bytes_precision) {
                        va_arg(ap->va, long double);
                    } else {
                        ygglog_throw_error(
                                "ScalarMetaschemaType::skip_va_elements_core: Unsupported float precision '%lu'.",
                                precision_);
                    }
                    break;
                }
                case T_COMPLEX: {
                    size_t bytes_precision = nbytes();
                    if (sizeof(float) == (bytes_precision / 2)) {
                        va_arg(ap->va, complex_float_t);
                    } else if (sizeof(double) == (bytes_precision / 2)) {
                        va_arg(ap->va, complex_double_t);
                    } else if (sizeof(long double) == (bytes_precision / 2)) {
                        va_arg(ap->va, complex_long_double_t);
                    } else {
                        ygglog_throw_error(
                                "ScalarMetaschemaType::skip_va_elements_core: Unsupported complex precision '%lu'.",
                                precision_);
                    }
                    break;
                }
                case T_BYTES:
                case T_UNICODE: {
                    va_arg(ap->va, char*);
                    va_arg(ap->va, size_t);
                    (*nargs)--;
                    break;
                }
                default: {
                    ygglog_throw_error("ScalarMetaschemaType::skip_va_elements_core: Unsupported subtype '%s'.",
                                       subtype_);
                }
            }
            break;
        }
        default: {
            ygglog_error("ScalarMetaschemaType::skip_va_elements_core: Cannot skip arguments for type '%s'.",
                         type());
        }
    }
    (*nargs)--;
}

/*!
  @brief Update the type object with info from another type object.
  @param[in] new_info MetaschemaType* type object.
 */
void ScalarMetaschemaType::update(const MetaschemaType *new_info) {
    if (strcmp(new_info->type(), "array") == 0) {
        const JSONArrayMetaschemaType *new_info_array = dynamic_cast<const JSONArrayMetaschemaType *>(new_info);
        if (new_info_array->nitems() == 1) {
            update(new_info_array->items()[0]);
            return;
        }
    }
    MetaschemaType::update(new_info);
    const ScalarMetaschemaType *new_info_scalar = dynamic_cast<const ScalarMetaschemaType *>(new_info);
    update_subtype(new_info_scalar->subtype());
    if ((strcmp(type(), "scalar") == 0) &&
        ((strcmp(subtype(), "bytes") == 0) ||
         (strcmp(subtype(), "unicode") == 0))) {
        _variable_precision = true;
    }
    set_precision(new_info_scalar->precision());
    update_units(new_info_scalar->units());
}

size_t ScalarMetaschemaType::update_from_serialization_args(size_t *nargs, struct va_list_t &ap) {
    size_t out = MetaschemaType::update_from_serialization_args(nargs, ap);
    if (use_generic())
        return out;
    size_t bytes_precision = nbytes();
    switch (type_code()) {
        case T_SCALAR: {
            switch (subtype_code_) {
                case T_INT: {
                    switch (precision_) {
                        case 8:
                        case 16: {
                            if (ap.using_ptrs) {
                                va_list_t_skip(&ap, sizeof(int));
                            } else {
                                va_arg(ap.va, int);
                            }
                            break;
                        }
                        case 32: {
                            if (ap.using_ptrs) {
                                va_list_t_skip(&ap, sizeof(int32_t));
                            } else {
                                va_arg(ap.va, int32_t);
                            }
                            break;
                        }
                        case 64: {
                            if (ap.using_ptrs) {
                                va_list_t_skip(&ap, sizeof(int64_t));
                            } else {
                                va_arg(ap.va, int64_t);
                            }
                            break;
                        }
                    }
                    out = out + 1;
                    break;
                }
                case T_UINT: {
                    switch (precision_) {
                        case 8:
                        case 16: {
                            if (ap.using_ptrs) {
                                va_list_t_skip(&ap, sizeof(unsigned int));
                            } else {
                                va_arg(ap.va, unsigned int);
                            }
                            break;
                        }
                        case 32: {
                            if (ap.using_ptrs) {
                                va_list_t_skip(&ap, sizeof(uint32_t));
                            } else {
                                va_arg(ap.va, uint32_t);
                            }
                            break;
                        }
                        case 64: {
                            if (ap.using_ptrs) {
                                va_list_t_skip(&ap, sizeof(uint64_t));
                            } else {
                                va_arg(ap.va, uint64_t);
                            }
                            break;
                        }
                    }
                    out = out + 1;
                    break;
                }
                case T_FLOAT: {
                    if (sizeof(float) == bytes_precision) {
                        if (ap.using_ptrs) {
                            va_list_t_skip(&ap, sizeof(double));
                        } else {
                            va_arg(ap.va, double);
                        }
                    } else if (sizeof(double) == bytes_precision) {
                        if (ap.using_ptrs) {
                            va_list_t_skip(&ap, sizeof(double));
                        } else {
                            va_arg(ap.va, double);
                        }
                    } else if (sizeof(long double) == bytes_precision) {
                        if (ap.using_ptrs) {
                            va_list_t_skip(&ap, sizeof(long double));
                        } else {
                            va_arg(ap.va, long double);
                        }
                    }
                    out = out + 1;
                    break;
                }
                case T_COMPLEX: {
                    if (sizeof(float) == (bytes_precision / 2)) {
                        if (ap.using_ptrs) {
                            va_list_t_skip(&ap, sizeof(complex_float_t));
                        } else {
                            va_arg(ap.va, complex_float_t);
                        }
                    } else if (sizeof(double) == (bytes_precision / 2)) {
                        if (ap.using_ptrs) {
                            va_list_t_skip(&ap, sizeof(complex_double_t));
                        } else {
                            va_arg(ap.va, complex_double_t);
                        }
                    } else if (sizeof(long double) == (bytes_precision / 2)) {
                        if (ap.using_ptrs) {
                            va_list_t_skip(&ap, sizeof(complex_long_double_t));
                        } else {
                            va_arg(ap.va, complex_long_double_t);
                        }
                    }
                    out = out + 1;
                    break;
                }
                case T_BYTES:
                case T_UNICODE: {
                    if (_variable_precision) {
                        if (ap.using_ptrs) {
                            va_list_t_skip(&ap, sizeof(char *));
                            const size_t arg0_siz = ((size_t *) get_va_list_ptr_cpp(&ap))[0];
                            set_precision(8 * arg0_siz);
                        } else {
                            va_arg(ap.va, char*);
                            const size_t arg0_siz = va_arg(ap.va, size_t);
                            set_precision(8 * arg0_siz);
                        }
                    } else {
                        if (ap.using_ptrs) {
                            va_list_t_skip(&ap, sizeof(char *));
                            va_list_t_skip(&ap, sizeof(size_t));
                        } else {
                            va_arg(ap.va, char*);
                            va_arg(ap.va, size_t);
                        }
                    }
                    out = out + 2;
                    break;
                }
            }
        }
    }
    return out;
}

void ScalarMetaschemaType::update_subtype(const char *new_subtype, bool force) {
    if ((!(force)) && (strcmp(subtype_, new_subtype) != 0)) {
        ygglog_throw_error("ScalarMetaschemaType::update_subtype: Cannot update subtype from %s to subtype %s.",
                           subtype_, new_subtype);
    }
    char **subtype_modifier = const_cast<char **>(&subtype_);
    strncpy(*subtype_modifier, new_subtype, STRBUFF);
    int *subtype_code_modifier = const_cast<int *>(&subtype_code_);
    *subtype_code_modifier = check_subtype();
}

void ScalarMetaschemaType::update_units(const char *new_units, bool force) {
    if ((!(force)) && (strcmp(units_, new_units) != 0)) {
        if (strlen(new_units) == 0) {
            return;
        } else if (strlen(units_) == 0) {
            // pass
        } else if (!(are_compat_units(new_units, units_))) {
            ygglog_throw_error("ScalarMetaschemaType::update_units: Cannot update units %s to %s.",
                               units_, new_units);
        }
    }
    char **units_modifier = const_cast<char **>(&units_);
    strncpy(*units_modifier, new_units, STRBUFF);
}

/*!
  @brief Update the instance's precision.
  @param[in] new_precision size_t New precision.
  @param[in] force bool True if the precision should be updated even if it
  is not compatible with the existing value.
 */
void ScalarMetaschemaType::set_precision(const size_t new_precision, bool force) {
    if (precision_ != new_precision) {
        if (!(force)) {
            if (precision_ == 0) {
                // Pass
            } else if (_variable_precision) {
                // Pass
            } else if ((strcmp(subtype(), "float") == 0) &&
                       ((precision_ == 32) || (precision_ == 64)) &&
                       ((new_precision == 32) || (new_precision == 64)) &&
                       (strcmp(type(), "1darray") != 0) &&
                       (strcmp(type(), "ndarray") != 0)) {
                if (cast_precision_ == 0) {
                    cast_precision_ = precision_;
                }
            } else {
                ygglog_throw_error(
                        "ScalarMetaschemaType::set_precision: Cannot update precision from %ld to %ld for %s of subtype %s.",
                        precision_, new_precision, type(), subtype());
            }
        }
        size_t *precision_modifier = const_cast<size_t *>(&precision_);
        *precision_modifier = new_precision;
    }
    // if ((strcmp(subtype_, "bytes") != 0) &&
    // 	(strcmp(subtype_, "unicode") != 0)) {
    //   ygglog_throw_error("ScalarMetaschemaType::set_precision: Variable precision only allowed for bytes and unicode, not '%s'.", subtype_);
    // }
    // size_t *precision_modifier = const_cast<size_t*>(&precision_);
    // *precision_modifier = new_precision;
}

size_t ScalarMetaschemaType::nargs_exp() const {
    switch (subtype_code_) {
        case T_BYTES:
        case T_UNICODE: {
            if (strcmp(type(), "scalar") == 0) {
                return 2;
            }
        }
    }
    return 1;
}

/*!
  @brief Convert a Python representation to a C representation.
  @param[in] pyobj PyObject* Pointer to Python object.
  @returns YggGeneric* Pointer to C object.
 */
YggGeneric *ScalarMetaschemaType::python2c(PyObject *pyobj) const {
    YggGeneric *cobj = new YggGeneric(this, NULL, 0);
    void **data = cobj->get_data_pointer();
    if ((size_t) (PyArray_NBYTES(pyobj)) != nbytes()) {
        ygglog_throw_error(
                "ScalarMetaschemaType::python2c: Python object has a size of %lu bytes, but %lu were expected.",
                PyArray_NBYTES(pyobj), nbytes());
    }
    void *idata = (void *) realloc(data[0], nbytes());
    if (data == NULL) {
        ygglog_throw_error("ScalarMetaschemaType::python2c: Failed to realloc data.");
    }
    memcpy(idata, PyArray_DATA(pyobj), nbytes());
    data[0] = idata;
    return cobj;
}

/*!
  @brief Convert a C representation to a Python representation.
  @param[in] cobj YggGeneric* Pointer to C object.
  @returns PyObject* Pointer to Python object.
 */
PyObject *ScalarMetaschemaType::c2python(YggGeneric *cobj) const {
    initialize_python("ScalarMetaschemaType::c2python: ");
    int nd = 1;
    npy_intp *dims = NULL;
    numpy_dims(&nd, &dims);
    int np_type = -1;
    void *data = cobj->copy_data();
    if (data == NULL) {
        ygglog_throw_error("ScalarMetaschemaType::c2python: Data pointer is NULL.");
    }
    size_t itemsize = precision_ / 8;
    int flags = NPY_OWNDATA;
    switch (subtype_code_) {
        case T_INT: {
            switch (precision_) {
                case 8: {
                    np_type = NPY_INT8;
                    break;
                }
                case 16: {
                    np_type = NPY_INT16;
                    break;
                }
                case 32: {
                    np_type = NPY_INT32;
                    break;
                }
                case 64: {
                    np_type = NPY_INT64;
                    break;
                }
                default: {
                    ygglog_throw_error("ScalarMetaschemaType::c2python: Unsupported integer precision '%lu'.",
                                       precision_);
                }
            }
            break;
        }
        case T_UINT: {
            switch (precision_) {
                case 8: {
                    np_type = NPY_UINT8;
                    break;
                }
                case 16: {
                    np_type = NPY_UINT16;
                    break;
                }
                case 32: {
                    np_type = NPY_UINT32;
                    break;
                }
                case 64: {
                    np_type = NPY_UINT64;
                    break;
                }
                default: {
                    ygglog_throw_error(
                            "ScalarMetaschemaType::c2python: Unsupported unsigned integer precision '%lu'.",
                            precision_);
                }
            }
            break;
        }
        case T_FLOAT: {
            switch (precision_) {
                case 16: {
                    np_type = NPY_FLOAT16;
                    break;
                }
                case 32: {
                    np_type = NPY_FLOAT32;
                    break;
                }
                case 64: {
                    np_type = NPY_FLOAT64;
                    break;
                }
                default: {
                    ygglog_throw_error("ScalarMetaschemaType::c2python: Unsupported float precision '%lu'.",
                                       precision_);
                }
            }
            break;
        }
        case T_COMPLEX: {
            switch (precision_) {
                case 64: {
                    np_type = NPY_COMPLEX64;
                    break;
                }
                case 128: {
                    np_type = NPY_COMPLEX128;
                    break;
                }
                default: {
                    ygglog_throw_error("ScalarMetaschemaType::c2python: Unsupported complex precision '%lu'.",
                                       precision_);
                }
            }
            break;
        }
        case T_BYTES: {
            np_type = NPY_BYTE;
            break;
        }
        case T_UNICODE: {
            np_type = NPY_UNICODE;
            break;
        }
        default: {
            ygglog_throw_error("ScalarMetaschemaType::c2python: Unsupported subtype '%s'.",
                               subtype_);
        }
    }
    PyObject *pyobj = PyArray_New(&PyArray_Type, nd, dims, np_type,
                                  nullptr, data, (int) itemsize, flags, nullptr);
    if (pyobj == NULL) {
        ygglog_throw_error("MetaschemaType::c2python: Creation of Numpy array failed.");
    }
    if (dims != NULL)
        free(dims);
    return pyobj;
}

// Encoding
/*!
  @brief Encode the type's properties in a JSON string.
  @param[in] writer rapidjson::Writer<rapidjson::StringBuffer> rapidjson writer.
  @returns bool true if the encoding was successful, false otherwise.
 */
bool ScalarMetaschemaType::ScalarMetaschemaType::encode_type_prop(
        rapidjson::Writer<rapidjson::StringBuffer> *writer) const {
    if (!MetaschemaType::encode_type_prop(writer)) { return false; }
    writer->Key("subtype");
    writer->String(subtype_, (rapidjson::SizeType) strlen(subtype_));
    writer->Key("precision");
    writer->Int((rapidjson::SizeType) precision_);
    if (strlen(units_) == 0) {
        writer->Key("units");
        writer->String("");
    } else {
        writer->Key("units");
        writer->String(units_, (rapidjson::SizeType) strlen(units_));
    }
    return true;
}


bool ScalarMetaschemaType::encode_data(rapidjson::Writer<rapidjson::StringBuffer> *writer,
                                       size_t *nargs, struct va_list_t &ap) const {
    size_t bytes_precision = nbytes();
    unsigned char *arg = (unsigned char *) malloc(bytes_precision + 1);
    if (arg == NULL) {
        ygglog_error(
                "ScalarMetaschemaType::encode_data: Failed to malloc for %lu bytes (%lu elements w/ precison of %lu bits).",
                bytes_precision + 1, nelements(), precision());
        return false;
    }
    switch (type_code()) {
        case T_1DARRAY:
        case T_NDARRAY: {
            unsigned char *arg0;
            if (ap.using_ptrs) {
                arg0 = (unsigned char *) get_va_list_ptr_cpp(&ap);
            } else {
                arg0 = va_arg(ap.va, unsigned char*);
            }
            if (nelements() == 0) {
                ygglog_error(
                        "ScalarMetaschemaType::encode_data: Array types require the number of elements be non-zero.");
                return false;
            }
            memcpy(arg, arg0, bytes_precision);
            break;
        }
        case T_SCALAR: {
            switch (subtype_code_) {
                case T_INT: {
                    switch (precision_) {
                        case 8: {
                            int8_t arg0;
                            if (ap.using_ptrs) {
                                arg0 = ((int8_t *) get_va_list_ptr_cpp(&ap))[0];
                            } else {
                                arg0 = (int8_t) va_arg(ap.va, int);
                            }
                            memcpy(arg, &arg0, bytes_precision);
                            break;
                        }
                        case 16: {
                            int16_t arg0;
                            if (ap.using_ptrs) {
                                arg0 = ((int16_t *) get_va_list_ptr_cpp(&ap))[0];
                            } else {
                                arg0 = (int16_t) va_arg(ap.va, int);
                            }
                            memcpy(arg, &arg0, bytes_precision);
                            break;
                        }
                        case 32: {
                            int32_t arg0;
                            if (ap.using_ptrs) {
                                arg0 = ((int32_t *) get_va_list_ptr_cpp(&ap))[0];
                            } else {
                                arg0 = va_arg(ap.va, int32_t);
                            }
                            memcpy(arg, &arg0, bytes_precision);
                            break;
                        }
                        case 64: {
                            int64_t arg0;
                            if (ap.using_ptrs) {
                                arg0 = ((int64_t *) get_va_list_ptr_cpp(&ap))[0];
                            } else {
                                arg0 = va_arg(ap.va, int64_t);
                            }
                            memcpy(arg, &arg0, bytes_precision);
                            break;
                        }
                        default: {
                            ygglog_error("ScalarMetaschemaType::encode_data: Unsupported integer precision '%lu'.",
                                         precision_);
                            return false;
                        }
                    }
                    break;
                }
                case T_UINT: {
                    switch (precision_) {
                        case 8: {
                            uint8_t arg0;
                            if (ap.using_ptrs) {
                                arg0 = ((uint8_t *) get_va_list_ptr_cpp(&ap))[0];
                            } else {
                                arg0 = (uint8_t) va_arg(ap.va, unsigned int);
                            }
                            memcpy(arg, &arg0, bytes_precision);
                            break;
                        }
                        case 16: {
                            uint16_t arg0;
                            if (ap.using_ptrs) {
                                arg0 = ((uint16_t *) get_va_list_ptr_cpp(&ap))[0];
                            } else {
                                arg0 = (uint16_t) va_arg(ap.va, unsigned int);
                            }
                            memcpy(arg, &arg0, bytes_precision);
                            break;
                        }
                        case 32: {
                            uint32_t arg0;
                            if (ap.using_ptrs) {
                                arg0 = ((uint32_t *) get_va_list_ptr_cpp(&ap))[0];
                            } else {
                                arg0 = va_arg(ap.va, uint32_t);
                            }
                            memcpy(arg, &arg0, bytes_precision);
                            break;
                        }
                        case 64: {
                            uint64_t arg0;
                            if (ap.using_ptrs) {
                                arg0 = ((uint64_t *) get_va_list_ptr_cpp(&ap))[0];
                            } else {
                                arg0 = va_arg(ap.va, uint64_t);
                            }
                            memcpy(arg, &arg0, bytes_precision);
                            break;
                        }
                        default: {
                            ygglog_error(
                                    "ScalarMetaschemaType::encode_data: Unsupported unsigned integer precision '%lu'.",
                                    precision_);
                            return false;
                        }
                    }
                    break;
                }
                case T_FLOAT: {
                    if (sizeof(float) == bytes_precision) {
                        float arg0;
                        if (ap.using_ptrs) {
                            arg0 = ((float *) get_va_list_ptr_cpp(&ap))[0];
                        } else {
                            arg0 = (float) va_arg(ap.va, double);
                        }
                        memcpy(arg, &arg0, bytes_precision);
                    } else if (sizeof(double) == bytes_precision) {
                        double arg0;
                        if (ap.using_ptrs) {
                            arg0 = ((double *) get_va_list_ptr_cpp(&ap))[0];
                        } else {
                            arg0 = va_arg(ap.va, double);
                        }
                        memcpy(arg, &arg0, bytes_precision);
                    } else if (sizeof(long double) == bytes_precision) {
                        long double arg0;
                        if (ap.using_ptrs) {
                            arg0 = ((long double *) get_va_list_ptr_cpp(&ap))[0];
                        } else {
                            arg0 = va_arg(ap.va, long double);
                        }
                        memcpy(arg, &arg0, bytes_precision);
                    } else {
                        ygglog_error("ScalarMetaschemaType::encode_data: Unsupported float precision '%lu'.",
                                     precision_);
                        return false;
                    }
                    break;
                }
                case T_COMPLEX: {
                    if (sizeof(float) == (bytes_precision / 2)) {
                        complex_float_t arg0;
                        if (ap.using_ptrs) {
                            arg0 = ((complex_float_t *) get_va_list_ptr_cpp(&ap))[0];
                        } else {
                            arg0 = (complex_float_t) va_arg(ap.va, complex_float_t);
                        }
                        memcpy(arg, &arg0, bytes_precision);
                    } else if (sizeof(double) == (bytes_precision / 2)) {
                        complex_double_t arg0;
                        if (ap.using_ptrs) {
                            arg0 = ((complex_double_t *) get_va_list_ptr_cpp(&ap))[0];
                        } else {
                            arg0 = va_arg(ap.va, complex_double_t);
                        }
                        memcpy(arg, &arg0, bytes_precision);
                    } else if (sizeof(long double) == (bytes_precision / 2)) {
                        complex_long_double_t arg0;
                        if (ap.using_ptrs) {
                            arg0 = ((complex_long_double_t *) get_va_list_ptr_cpp(&ap))[0];
                        } else {
                            arg0 = va_arg(ap.va, complex_long_double_t);
                        }
                        memcpy(arg, &arg0, bytes_precision);
                    } else {
                        ygglog_error("ScalarMetaschemaType::encode_data: Unsupported complex precision '%lu'.",
                                     precision_);
                        return false;
                    }
                    break;
                }
                case T_BYTES:
                case T_UNICODE: {
                    char *arg0;
                    size_t arg0_siz_x;
                    if (ap.using_ptrs) {
                        arg0 = (char *) get_va_list_ptr_cpp(&ap);
                        arg0_siz_x = ((size_t *) get_va_list_ptr_cpp(&ap))[0];
                    } else {
                        arg0 = va_arg(ap.va, char*);
                        arg0_siz_x = va_arg(ap.va, size_t);
                    }
                    const size_t arg0_siz = arg0_siz_x;
                    int allow_realloc = (int) _variable_precision;
                    (*nargs)--;
                    size_t arg_siz = bytes_precision + 1;
                    int ret = copy_to_buffer(arg0, arg0_siz, (char **) (&arg), arg_siz,
                                             allow_realloc);
                    if (ret < 0) {
                        ygglog_error(
                                "ScalarMetaschemaType::encode_data: Failed to copy bytes/unicode variable to buffer.");
                        free(arg);
                        return false;
                    }
                    break;
                }
                default: {
                    ygglog_error("ScalarMetaschemaType::encode_data: Unsupported subtype '%s'.",
                                 subtype_);
                    return false;
                }
            }
        }
    }
    (*nargs)--;
    size_t encoded_len = 0;
    unsigned char *encoded_bytes = base64_encode(arg, nbytes(), &encoded_len);
    bool out = writer->String((char *) encoded_bytes, (rapidjson::SizeType) encoded_len);
    free(arg);
    free(encoded_bytes);
    return out;
}

bool ScalarMetaschemaType::encode_data(rapidjson::Writer<rapidjson::StringBuffer> *writer,
                                       YggGeneric *x) const {
    size_t nargs = 1;
    size_t bytes_precision = nbytes();
    switch (type_code()) {
        case T_1DARRAY:
        case T_NDARRAY: {
            void *arg = x->get_data();
            return MetaschemaType::encode_data(writer, &nargs, arg);
        }
        case T_SCALAR: {
            switch (subtype_code_) {
                case T_INT: {
                    switch (precision_) {
                        case 8: {
                            int8_t arg = 0;
                            x->get_data(arg);
                            return MetaschemaType::encode_data(writer, &nargs, arg);
                        }
                        case 16: {
                            int16_t arg = 0;
                            x->get_data(arg);
                            return MetaschemaType::encode_data(writer, &nargs, arg);
                        }
                        case 32: {
                            int32_t arg = 0;
                            x->get_data(arg);
                            return MetaschemaType::encode_data(writer, &nargs, arg);
                        }
                        case 64: {
                            int64_t arg = 0;
                            x->get_data(arg);
                            return MetaschemaType::encode_data(writer, &nargs, arg);
                        }
                        default: {
                            ygglog_error("ScalarMetaschemaType::encode_data: Unsupported integer precision '%lu'.",
                                         precision_);
                            return false;
                        }
                    }
                }
                case T_UINT: {
                    switch (precision_) {
                        case 8: {
                            uint8_t arg = 0;
                            x->get_data(arg);
                            return MetaschemaType::encode_data(writer, &nargs, arg);
                        }
                        case 16: {
                            uint16_t arg = 0;
                            x->get_data(arg);
                            return MetaschemaType::encode_data(writer, &nargs, arg);
                        }
                        case 32: {
                            uint32_t arg = 0;
                            x->get_data(arg);
                            return MetaschemaType::encode_data(writer, &nargs, arg);
                        }
                        case 64: {
                            uint64_t arg = 0;
                            x->get_data(arg);
                            return MetaschemaType::encode_data(writer, &nargs, arg);
                        }
                        default: {
                            ygglog_error(
                                    "ScalarMetaschemaType::encode_data: Unsupported unsigned integer precision '%lu'.",
                                    precision_);
                            return false;
                        }
                    }
                }
                case T_FLOAT: {
                    if (sizeof(float) == bytes_precision) {
                        float arg = 0.0;
                        x->get_data(arg);
                        return MetaschemaType::encode_data(writer, &nargs, arg);
                    } else if (sizeof(double) == bytes_precision) {
                        double arg = 0.0;
                        x->get_data(arg);
                        return MetaschemaType::encode_data(writer, &nargs, arg);
                    } else if (sizeof(long double) == bytes_precision) {
                        long double arg = 0.0;
                        x->get_data(arg);
                        return MetaschemaType::encode_data(writer, &nargs, arg);
                    } else {
                        ygglog_error("ScalarMetaschemaType::encode_data: Unsupported float precision '%lu'.",
                                     precision_);
                        return false;
                    }
                }
                case T_COMPLEX: {
                    if (sizeof(float) == (bytes_precision / 2)) {
                        complex_float_t arg;
                        x->get_data(arg);
                        return MetaschemaType::encode_data(writer, &nargs, arg);
                    } else if (sizeof(double) == (bytes_precision / 2)) {
                        complex_double_t arg;
                        x->get_data(arg);
                        return MetaschemaType::encode_data(writer, &nargs, arg);
                    } else if (sizeof(long double) == (bytes_precision / 2)) {
                        complex_long_double_t arg;
                        x->get_data(arg);
                        return MetaschemaType::encode_data(writer, &nargs, arg);
                    } else {
                        ygglog_error("ScalarMetaschemaType::encode_data: Unsupported complex precision '%lu'.",
                                     precision_);
                        return false;
                    }
                }
                case T_BYTES:
                case T_UNICODE: {
                    nargs = 2;
                    char *arg = NULL;
                    size_t arg_siz = 0;
                    x->get_data_realloc(&arg, &arg_siz);
                    bool out = MetaschemaType::encode_data(writer, &nargs, arg, arg_siz);
                    if (arg != NULL) {
                        free(arg);
                        arg = NULL;
                    }
                    return out;
                }
                default: {
                    ygglog_error("ScalarMetaschemaType::encode_data: Unsupported subtype '%s'.",
                                 subtype_);
                    return false;
                }
            }
        }
    }
    ygglog_error("ScalarMetaschemaType::encode_data: Cannot encode data of type '%s'.", type());
    return false;
}

bool ScalarMetaschemaType::decode_data(rapidjson::Value &data, const int allow_realloc,
                                       size_t *nargs, struct va_list_t &ap) const {
    if ((data.IsArray()) && (data.Size() == 1)) {
        data = data[0];
    }
    if (!(data.IsString())) {
        ygglog_error("ScalarMetaschemaType::decode_data: Raw data is not a string.");
        return false;
    }
    unsigned char *encoded_bytes = (unsigned char *) data.GetString();
    size_t encoded_len = data.GetStringLength();
    size_t decoded_len = 0;
    unsigned char *decoded_bytes = base64_decode(encoded_bytes, encoded_len,
                                                 &decoded_len);
    size_t nbytes_expected = nbytes();
    if ((!(_variable_precision)) && (nbytes_expected != decoded_len)) {
        ygglog_error("ScalarMetaschemaType::decode_data: %lu bytes were expected, but %lu were decoded.",
                     nbytes_expected, decoded_len);
        return false;
    }
// Transfer data to array memory
    char *arg;
    char **p;
    if (allow_realloc) {
        if (ap.using_ptrs) {
            p = (char **) get_va_list_ptr_ref_cpp(&ap, 1);
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
    (*nargs)--;
    bool skip_terminal;
    if ((type_code() == T_SCALAR) &&
        ((subtype_code_ == T_BYTES) || (subtype_code_ == T_UNICODE))) {
        size_t *arg_siz_x;
        if (ap.using_ptrs) {
            arg_siz_x = (size_t *) get_va_list_ptr_cpp(&ap);
        } else {
            arg_siz_x = va_arg(ap.va, size_t*);
        }
        size_t *const arg_siz = arg_siz_x;
        (*nargs)--;
        skip_terminal = false;
        if (ap.for_fortran)
            skip_terminal = true;
        int ret = copy_to_buffer((char *) decoded_bytes, decoded_len,
                                 p, arg_siz[0], allow_realloc, skip_terminal);
        if (ret < 0) {
            ygglog_error("ScalarMetaschemaType::decode_data: Failed to copy buffer for %s.",
                         subtype());
            free(decoded_bytes);
            return false;
        }
        arg_siz[0] = (size_t) ret;
    } else {
        size_t *arg_siz = &nbytes_expected;
        if ((allow_realloc) && (!((type_code() == T_SCALAR) && ap.for_fortran))) {
            arg_siz[0] = 0;
        }
        skip_terminal = true;
        if ((cast_precision_ != 0) && (cast_precision_ != precision_)) {
            try {
                decoded_len = cast_bytes(&decoded_bytes, decoded_len);

                if (!(allow_realloc)) {
                    arg_siz[0] = decoded_len;
                }
            } catch (...) {
                ygglog_error(
                        "ScalarMetaschemaType::decode_data: Cannot cast subtype '%s' and precision %ld to precision %ld.",
                        subtype_, precision_, cast_precision_);
                free(decoded_bytes);
                return false;
            }
        }
        int ret = copy_to_buffer((char *) decoded_bytes, decoded_len,
                                 p, *arg_siz, allow_realloc, skip_terminal);
        if (ret < 0) {
            ygglog_error("ScalarMetaschemaType::decode_data: Failed to copy buffer for %s.",
                         subtype());
            free(decoded_bytes);
            return false;
        }
    }
    free(decoded_bytes);
    return true;
}

bool ScalarMetaschemaType::decode_data(rapidjson::Value &data, YggGeneric *x) {
    switch (type_code()) {
        case T_SCALAR: {
            switch (subtype_code_) {
                case T_BYTES:
                case T_UNICODE: {
                    size_t nargs = 2;
                    int allow_realloc = 1;
                    if (x == NULL) {
                        ygglog_throw_error("MetaschemaType::decode_data: Generic wrapper is not initialized.");
                    }
                    void **arg = x->get_data_pointer();
                    size_t *arg_siz = x->get_nbytes_pointer();
                    return MetaschemaType::decode_data(data, allow_realloc, &nargs, arg, arg_siz);
                }
            }
        }
        default: {
            return MetaschemaType::decode_data(data, x);
        }
    }
}

size_t ScalarMetaschemaType::cast_bytes(unsigned char **bytes, const size_t nbytes) const {
    bool raise_error = false;
    size_t from_precision = precision_;
    size_t to_precision = cast_precision_;
    if (((nbytes * to_precision) % from_precision) != 0) {
        ygglog_throw_error("cast_bytes: Cannot cast %ld bytes from precision %ld to %ld.",
                           nbytes, from_precision, to_precision);
    }
    size_t nbytes_new = nbytes * to_precision / from_precision;
    if (strcmp(subtype(), "float") == 0) {
        if (from_precision == 32) {
            float *tmp_val1 = (float *) (bytes[0]);
            if (to_precision == 64) {
                double tmp_val2 = (double) (tmp_val1[0]);
                bytes[0] = (unsigned char *) realloc(bytes[0], sizeof(double));
                if (bytes[0] == NULL) {
                    raise_error = true;
                } else {
                    memcpy(bytes[0], &tmp_val2, sizeof(double));
                }
            } else {
                raise_error = true;
            }
        } else if (from_precision == 64) {
            double *tmp_val1 = (double *) (bytes[0]);
            if (to_precision == 32) {
                float tmp_val2 = (float) (tmp_val1[0]);
                bytes[0] = (unsigned char *) realloc(bytes[0], sizeof(float));
                if (bytes[0] == NULL) {
                    raise_error = true;
                } else {
                    memcpy(bytes[0], &tmp_val2, sizeof(float));
                }
            } else {
                raise_error = true;
            }
        } else {
            raise_error = true;
        }
    } else {
        raise_error = true;
    }
    if (raise_error) {
        ygglog_throw_error("cast_bytes: Cannot change precision of %s type with precision %d to %d.",
                           subtype(), from_precision, to_precision);
    }
    return nbytes_new;
}
