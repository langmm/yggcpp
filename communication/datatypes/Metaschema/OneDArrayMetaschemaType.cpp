#include "OneDArrayMetaschemaType.hpp"
#include "NDArrayMetaschemaType.hpp"
using namespace communication::datatypes::Metaschema;
using namespace communication::utils;

OneDArrayMetaschemaType::OneDArrayMetaschemaType(const rapidjson::Value &type_doc,
                            const bool use_generic) :
            ScalarMetaschemaType(type_doc, use_generic) {
        if (!(type_doc.HasMember("length")))
            ygglog_throw_error("OneDArrayMetaschemaType: 1darray types must include 'length'.");
        if (type_doc["length"].IsInt()) {
            length_ = type_doc["length"].GetInt();
        } else if (type_doc["length"].IsDouble()) {
            length_ = (size_t) (type_doc["length"].GetDouble());
        } else {
            ygglog_throw_error("OneDArrayMetaschemaType: 1darray 'length' value must be a number.");
        }
        update_type("1darray");
        if (length_ == 0)
            _variable_length = true;
        else
            _variable_length = false;
    }

    OneDArrayMetaschemaType::OneDArrayMetaschemaType(PyObject *pyobj, const bool use_generic) :
            ScalarMetaschemaType(pyobj, use_generic), length_(0) {
        update_type("1darray");
        get_item_python_dict_c(pyobj, "length", &length_,
                               "OneDArrayMetaschemaType: length: ",
                               T_INT, sizeof(size_t) * 8);
        // Set variable length
        if (length_ == 0)
            _variable_length = true;
        else
            _variable_length = false;
    }

    bool OneDArrayMetaschemaType::operator==(const MetaschemaType &Ref) const {
        if (!(ScalarMetaschemaType::operator==(Ref)))
            return false;
        const OneDArrayMetaschemaType *pRef = dynamic_cast<const OneDArrayMetaschemaType *>(&Ref);
        if (!pRef)
            return false;
        if (length_ != pRef->length())
            return false;
        return true;
    }

    void OneDArrayMetaschemaType::numpy_dims(int *nd, npy_intp **dims) const {
        nd[0] = 1;
        npy_intp *idim = (npy_intp *) realloc(dims[0], sizeof(npy_intp));
        if (idim == NULL) {
            ygglog_throw_error("OneDArrayMetaschemaType::numpy_dims: Failed to realloc dims.");
        }
        idim[0] = (npy_intp)(length());
        dims[0] = idim;
    }

    void OneDArrayMetaschemaType::update(const MetaschemaType *new_info) {
        if (new_info->type_code() == T_NDARRAY) {
            const NDArrayMetaschemaType *new_info_nd = dynamic_cast<const NDArrayMetaschemaType *>(new_info);
            OneDArrayMetaschemaType *new_info_oned = new OneDArrayMetaschemaType(new_info_nd->subtype(),
                                                                                 new_info_nd->precision(),
                                                                                 new_info_nd->nelements(),
                                                                                 new_info_nd->units());
            update(new_info_oned);
            delete new_info_oned;
        } else {
            ScalarMetaschemaType::update(new_info);
            const OneDArrayMetaschemaType *new_info_oned = dynamic_cast<const OneDArrayMetaschemaType *>(new_info);
            set_length(new_info_oned->length());
        }
    }

    size_t OneDArrayMetaschemaType::update_from_serialization_args(size_t *nargs, struct va_list_t &ap) {
        size_t out = ScalarMetaschemaType::update_from_serialization_args(nargs, ap);
        if (use_generic())
            return out;
        if ((_variable_length) && (*nargs >= 2)) {
            size_t new_length;
            va_list_t_skip(&ap, sizeof(unsigned char *));
            if (ap.using_ptrs) {
                new_length = ((size_t *) get_va_list_ptr_cpp(&ap))[0];
            } else {
                new_length = va_arg(ap.va, size_t);
            }
            skip_after_.push_back(sizeof(size_t));
            set_length(new_length);
            out = out + 2;
        } else {
            va_list_t_skip(&ap, sizeof(unsigned char *));
            out = out + 1;
        }
        return out;
    }

    size_t OneDArrayMetaschemaType::update_from_deserialization_args(size_t *nargs, struct va_list_t &ap) {
        size_t out = MetaschemaType::update_from_deserialization_args(nargs, ap);
        if (use_generic())
            return out;
        if ((_variable_length) && (*nargs >= 2)) {
            va_list_t_skip(&ap, sizeof(unsigned char **));
            if (ap.using_ptrs) {
                size_t *const new_length = (size_t *const) get_va_list_ptr_cpp(&ap);
                new_length[0] = length_;
            } else {
                size_t *const new_length = va_arg(ap.va, size_t*);
                new_length[0] = length_;
            }
            skip_after_.push_back(sizeof(size_t *));
            out = out + 2;
        }
        if ((ap.for_fortran) && (ap.using_ptrs)) {
            if ((!(_variable_length)) || (*nargs < 2)) {
                va_list_t_skip(&ap, sizeof(unsigned char **));
                out = out + 1;
            }
            if (!(_variable_length) && (!(in_table()))) {
                ap.nptrs++;
                size_t *const new_length = (size_t *const) get_va_list_ptr_cpp(&ap);
                new_length[0] = length_;
            }
            if ((subtype_code() == T_BYTES) || (subtype_code() == T_UNICODE)) {
                ap.nptrs++;
                size_t *arg_prec = (size_t *) get_va_list_ptr_cpp(&ap);
                arg_prec[0] = (size_t) (precision() / 8);
            }
        }
        return out;
    }

    void OneDArrayMetaschemaType::set_length(size_t new_length, bool force) {
        if (length_ != new_length) {
            if (!(force)) {
                if (length_ == 0) {
                    // Pass
                } else if (_variable_length) {
                    // Pass
                } else {
                    ygglog_throw_error(
                            "OneDArrayMetaschemaType::set_length: Cannot update precision from %ld to %ld for %s of subtype %s.",
                            length_, new_length, type(), subtype());
                }
            }
            size_t *length_modifier = const_cast<size_t *>(&length_);
            *length_modifier = new_length;
        }
    }

    bool OneDArrayMetaschemaType::decode_data(rapidjson::Value &data, const int allow_realloc,
                     size_t *nargs, struct va_list_t &ap) const {
        bool out = ScalarMetaschemaType::decode_data(data, allow_realloc,
                                                     nargs, ap);
        if (out) {
            if ((ap.for_fortran) && (ap.using_ptrs)) {
                if ((!(_variable_length)) && (!(in_table()))) {
                    ap.nptrs++;
                    va_list_t_skip(&ap, sizeof(size_t *));
                }
                if ((subtype_code() == T_BYTES) || (subtype_code() == T_UNICODE)) {
                    ap.nptrs++;
                    va_list_t_skip(&ap, sizeof(size_t *));
                }
            }
        }
        return out;
    }
