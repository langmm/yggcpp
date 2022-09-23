#include "NDArrayMetaschemaType.hpp"

using namespace communication::datatypes::Metaschema;
using namespace communication::utils;

NDArrayMetaschemaType::~NDArrayMetaschemaType() {}

NDArrayMetaschemaType::NDArrayMetaschemaType(const char *subtype, const size_t precision,
                                             const std::vector<size_t> shape,
                                             const char *units,
                                             const bool use_generic) :
        ScalarMetaschemaType(subtype, precision, units, use_generic),
        shape_(shape) {
    update_type("ndarray");
    if (shape_.size() == 0) {
        _variable_shape = true;
    } else {
        _variable_shape = false;
    }
}

NDArrayMetaschemaType::NDArrayMetaschemaType(const rapidjson::Value &type_doc,
                                             const bool use_generic) :
        ScalarMetaschemaType(type_doc, use_generic) {
    if (!(type_doc.HasMember("shape")))
        ygglog_throw_error("NDArrayMetaschemaType: ndarray types must include 'shape'.");
    if (!(type_doc["shape"].IsArray()))
        ygglog_throw_error("NDArrayMetaschemaType: ndarray 'shape' value must be an array.");
    size_t ndim = type_doc["shape"].Size();
    size_t i;
    for (i = 0; i < ndim; i++) {
        if (type_doc["shape"][(rapidjson::SizeType) i].IsInt()) {
            shape_.push_back(type_doc["shape"][(rapidjson::SizeType) i].GetInt());
        } else if (type_doc["shape"][(rapidjson::SizeType) i].IsDouble()) {
            shape_.push_back((size_t) (type_doc["shape"][(rapidjson::SizeType) i].GetDouble()));
        } else {
            ygglog_throw_error("NDArrayMetaschemaType: ndarray 'shape' elements must be numbers.");
        }
    }
    update_type("ndarray");
}

NDArrayMetaschemaType::NDArrayMetaschemaType(PyObject *pyobj,
                                             const bool use_generic) :
        ScalarMetaschemaType(pyobj, use_generic) {
    update_type("ndarray");
    // Shape
    PyObject *pyshape = get_item_python_dict(pyobj, "shape",
                                             "NDArrayMetaschemaType: shape: ",
                                             T_ARRAY);
    size_t i, ishape, ndim = PyList_Size(pyshape);
    for (i = 0; i < ndim; i++) {
        get_item_python_list_c(pyobj, i, &ishape,
                               "NDArrayMetaschemaType: shape: ",
                               T_INT, sizeof(size_t) * 8);
        shape_.push_back(ishape);
    }
    Py_DECREF(pyshape);
    // Set variable shape
    if (shape_.size() == 0) {
        _variable_shape = true;
    } else {
        _variable_shape = false;
    }
}

NDArrayMetaschemaType::NDArrayMetaschemaType(const NDArrayMetaschemaType &other) :
        NDArrayMetaschemaType(other.subtype(), other.precision(),
                              other.shape(), other.units(),
                              other.use_generic()) {};

bool NDArrayMetaschemaType::operator==(const MetaschemaType &Ref) const {
    if (!(ScalarMetaschemaType::operator==(Ref)))
        return false;
    const NDArrayMetaschemaType *pRef = dynamic_cast<const NDArrayMetaschemaType *>(&Ref);
    if (!pRef)
        return false;
    if (shape_ != pRef->shape())
        return false;
    return true;
}

NDArrayMetaschemaType *NDArrayMetaschemaType::copy() const {
    return (new NDArrayMetaschemaType(subtype(), precision(), shape(), units(), use_generic()));
}

void NDArrayMetaschemaType::display(const char *indent) const {
    ScalarMetaschemaType::display(indent);
    printf("%s%-15s = [ ", indent, "shape");
    if (ndim() > 0) {
        size_t i;
        printf("%ld", shape_[0]);
        for (i = 1; i < ndim(); i++) {
            printf(", %ld", shape_[i]);
        }
    }
    printf(" ]\n");
}

PyObject *NDArrayMetaschemaType::as_python_dict() const {
    PyObject *out = ScalarMetaschemaType::as_python_dict();
    PyObject *pyshape = PyList_New(ndim());
    if (pyshape == NULL) {
        ygglog_throw_error("NDArrayMetaschemaType::as_python_dict: Failed to create new Python list for shape.");
    }
    size_t i;
    for (i = 1; i < ndim(); i++) {
        set_item_python_list_c(pyshape, i, &(shape_[i]),
                               "NDArrayMetaschemaType::as_python_dict: shape: ",
                               T_INT, sizeof(size_t) * 8);
    }
    set_item_python_dict(out, "shape", pyshape,
                         "NDArrayMetaschemaType::as_python_dict: ",
                         T_ARRAY);
    return out;
}

const size_t NDArrayMetaschemaType::ndim() const {
    return shape_.size();
}

std::vector<size_t> NDArrayMetaschemaType::shape() const {
    return shape_;
}

const size_t NDArrayMetaschemaType::nelements() const {
    size_t nelements = 0;
    if (ndim() > 0) {
        size_t i;
        nelements = 1;
        for (i = 0; i < ndim(); i++) {
            nelements = nelements * shape_[i];
        }
    }
    return nelements;
}

const bool NDArrayMetaschemaType::variable_nelements() const {
    return _variable_shape;
}

void NDArrayMetaschemaType::numpy_dims(int *nd, npy_intp **dims) const {
    int i;
    nd[0] = (int) ndim();
    npy_intp *idim = (npy_intp *) realloc(dims[0], nd[0] * sizeof(npy_intp));
    if (idim == NULL) {
        ygglog_throw_error("NDArrayMetaschemaType::numpy_dims: Failed to realloc dims.");
    }
    for (i = 0; i < nd[0]; i++) {
        idim[i] = (npy_intp)(shape_[i]);
    }
    dims[0] = idim;
}

size_t NDArrayMetaschemaType::nargs_exp() const {
    size_t out = 1;
    if (_variable_shape)
        out = out + 2;
    return out;
}

void NDArrayMetaschemaType::update(const MetaschemaType *new_info) {
    ScalarMetaschemaType::update(new_info);
    const NDArrayMetaschemaType *new_info_nd = dynamic_cast<const NDArrayMetaschemaType *>(new_info);
    set_shape(new_info_nd->shape());
}

size_t NDArrayMetaschemaType::update_from_serialization_args(size_t *nargs, va_list_t &ap) {
    size_t out = MetaschemaType::update_from_serialization_args(nargs, ap);
    if (use_generic())
        return out;
    if ((_variable_shape) && (*nargs >= 3)) {
        size_t new_ndim;
        size_t *new_shape_ptr;
        va_list_t_skip(&ap, sizeof(unsigned char *));
        if (ap.using_ptrs) {
            new_ndim = ((size_t *) get_va_list_ptr_cpp(&ap))[0];
            new_shape_ptr = (size_t *) get_va_list_ptr_cpp(&ap);
        } else {
            new_ndim = va_arg(ap.va, size_t);
            new_shape_ptr = va_arg(ap.va, size_t*);
        }
        skip_after_.push_back(sizeof(size_t));
        skip_after_.push_back(sizeof(size_t *));
        std::vector<size_t> new_shape(new_shape_ptr, new_shape_ptr + new_ndim);
        set_shape(new_shape);
        out = out + 3;
    } else {
        va_list_t_skip(&ap, sizeof(unsigned char *));
        out = out + 1;
    }
    return out;
}

size_t NDArrayMetaschemaType::update_from_deserialization_args(size_t *nargs, va_list_t &ap) {
    size_t out = MetaschemaType::update_from_deserialization_args(nargs, ap);
    if (use_generic())
        return out;
    if ((_variable_shape) && (*nargs >= 3)) {
        va_list_t_skip(&ap, sizeof(unsigned char **));
        size_t **new_shape;
        if (ap.using_ptrs) {
            size_t *const new_ndim = (size_t *const) get_va_list_ptr_cpp(&ap);
            new_ndim[0] = ndim();
            new_shape = (size_t **) get_va_list_ptr_ref_cpp(&ap, 1);
        } else {
            size_t *const new_ndim = va_arg(ap.va, size_t*);
            new_ndim[0] = ndim();
            new_shape = va_arg(ap.va, size_t**);
        }
        skip_after_.push_back(sizeof(size_t *));
        skip_after_.push_back(sizeof(size_t **));
        size_t *new_shape_temp = (size_t *) realloc(new_shape[0], ndim() * sizeof(size_t));
        if (new_shape_temp == NULL) {
            ygglog_throw_error(
                    "NDArrayMetaschemaType::update_from_deseriali: Failed to realloc memory for the provided shape array.");
        }
        new_shape[0] = new_shape_temp;
        size_t i;
        for (i = 0; i < ndim(); i++) {
            (*new_shape)[i] = shape_[i];
        }
        out = out + 3;
    }
    if ((ap.for_fortran) && (ap.using_ptrs)) {
        if ((!(_variable_shape)) || (*nargs < 3)) {
            va_list_t_skip(&ap, sizeof(unsigned char **));
            out = out + 1;
        }
        if (!(_variable_shape)) {
            ap.nptrs++;
            size_t *new_ndim = (size_t *) get_va_list_ptr_cpp(&ap);
            new_ndim[0] = ndim();
            ap.nptrs++;
            size_t **new_shape = (size_t **) get_va_list_ptr_ref_cpp(&ap, 1);
            // size_t* new_shape_temp = (size_t*)realloc(new_shape[0], ndim()*sizeof(size_t));
            // if (new_shape_temp == NULL) {
            // 	ygglog_throw_error("NDArrayMetaschemaType::update_from_deseriali: Failed to realloc memory for the provided shape array.");
            // }
            // new_shape[0] = new_shape_temp;
            size_t i;
            for (i = 0; i < ndim(); i++) {
                (*new_shape)[i] = shape_[i];
            }
        }
        if ((subtype_code() == T_BYTES) || (subtype_code() == T_UNICODE)) {
            ap.nptrs++;
            size_t *arg_prec = (size_t *) get_va_list_ptr_cpp(&ap);
            arg_prec[0] = (size_t) (precision() / 8);
        }
    }
    return out;
}

void NDArrayMetaschemaType::set_shape(std::vector<size_t> new_shape, bool force) {
    bool match = (ndim() == new_shape.size());
    size_t i;
    if (match) {
        for (i = 0; i < ndim(); i++) {
            if (shape_[i] != new_shape[i]) {
                match = false;
                break;
            }
        }
    }
    if (!(match)) {
        if (!(force)) {
            if (ndim() == 0) {
                // Pass
            } else if (_variable_shape) {
                // Pase
            } else {
                ygglog_throw_error("NDArrayMetaschemaType::set_shape: Cannot update shape.");
            }
        }
        shape_.resize(new_shape.size());
        for (i = 0; i < new_shape.size(); i++) {
            shape_[i] = new_shape[i];
        }
    }
}

bool NDArrayMetaschemaType::encode_type_prop(rapidjson::Writer<rapidjson::StringBuffer> *writer) const {
    if (!(ScalarMetaschemaType::encode_type_prop(writer))) { return false; }
    writer->Key("shape");
    writer->StartArray();
    size_t i;
    for (i = 0; i < ndim(); i++) {
        writer->Int((int) (shape_[i]));
    }
    writer->EndArray();
    return true;
}

bool NDArrayMetaschemaType::decode_data(rapidjson::Value &data, const int allow_realloc,
                                        size_t *nargs, struct va_list_t &ap) const {
    bool out = ScalarMetaschemaType::decode_data(data, allow_realloc,
                                                 nargs, ap);
    if (out) {
        if ((ap.for_fortran) && (ap.using_ptrs)) {
            if (!(_variable_shape)) {
                ap.nptrs++;
                va_list_t_skip(&ap, sizeof(size_t *));
                ap.nptrs++;
                va_list_t_skip(&ap, sizeof(size_t **));
            }
            if ((subtype_code() == T_BYTES) || (subtype_code() == T_UNICODE)) {
                ap.nptrs++;
                va_list_t_skip(&ap, sizeof(size_t *));
            }
        }
    }
    return out;
}
