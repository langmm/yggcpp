#include "SchemaMetaschemaType.hpp"
using namespace communication::datatypes::Metaschema;
using namespace communication::utils;

    void* SchemaMetaschemaType::copy_generic(const YggGeneric* data, void* orig_data) const {
        if (data == NULL) {
            ygglog_throw_error("SchemaMetaschemaType::copy_generic: Generic object is NULL.");
        }
        void* out = NULL;
        if (orig_data == NULL) {
            orig_data = data->get_data();
        }
        if (orig_data != NULL) {
            dtype_t* old_data = (dtype_t*)orig_data;
            dtype_t* new_data = copy_dtype(old_data);
            if (new_data == NULL) {
                ygglog_throw_error("SchemaMetaschemaType::copy_generic: Failed to copy datatype struct.");
            }
            out = (void*)new_data;
        }
        return out;
    }

    void SchemaMetaschemaType::free_generic(YggGeneric* data) const {
        if (data == NULL) {
            ygglog_throw_error("SchemaMetaschemaType::free_generic: Generic object is NULL.");
        }
        dtype_t **ptr = (dtype_t**)(data->get_data_pointer());
        if (destroy_dtype(ptr) < 0) {
            ygglog_throw_error("SchemaMetaschemaType::free_generic: Failed to destroy datatype struct.");
        }
    }

    void SchemaMetaschemaType::display_generic(const YggGeneric* data, const char* indent) const {
        if (data == NULL) {
            ygglog_throw_error("SchemaMetaschemaType::display_generic: Generic object is NULL.");
        }
        dtype_t *arg = (dtype_t*)(data->get_data());
        display_dtype(arg, indent);
    }

    size_t SchemaMetaschemaType::update_from_serialization_args(size_t *nargs, struct va_list_t &ap) {
        size_t out = MetaschemaType::update_from_serialization_args(nargs, ap);
        if (use_generic())
            return out;
        if (ap.using_ptrs) {
            va_list_t_skip(&ap, sizeof(dtype_t*));
        } else {
            va_arg(ap.va, dtype_t*);
        }
        out++;
        return out;
    }

    YggGeneric* SchemaMetaschemaType::python2c(PyObject* pyobj) const {
        YggGeneric* cobj = new YggGeneric(this, NULL, 0);
        dtype_t** data = (dtype_t**)(cobj->get_data_pointer());
        data[0] = create_dtype_python(pyobj, false);
        return cobj;
    }

    PyObject* SchemaMetaschemaType::c2python(YggGeneric *cobj) const {
        PyObject *pyobj = NULL;
        dtype_t *src = (dtype_t*)(cobj->get_data());
        if (src != NULL) {
            MetaschemaType* obj = (MetaschemaType*)(src->obj);
            if (obj != NULL) {
                pyobj = obj->as_python_dict();
            }
        }
        return pyobj;
    }

    bool SchemaMetaschemaType::encode_data(rapidjson::Writer<rapidjson::StringBuffer> *writer,
                     size_t *nargs, struct va_list_t &ap) const {
        dtype_t* arg;
        if (ap.using_ptrs) {
            arg = (dtype_t*)get_va_list_ptr_cpp(&ap);
        } else {
            arg = va_arg(ap.va, dtype_t*);
        }
        MetaschemaType* obj = (MetaschemaType*)(arg->obj);
        (*nargs)--;
        return obj->encode_type(writer);
    }

    bool SchemaMetaschemaType::encode_data(rapidjson::Writer<rapidjson::StringBuffer> *writer,
                     YggGeneric* x) const {
        size_t nargs = 1;
        dtype_t* arg = (dtype_t*)(x->get_data());
        return MetaschemaType::encode_data(writer, &nargs, arg);
    }

    bool SchemaMetaschemaType::decode_data(rapidjson::Value &data, const int allow_realloc,
                     size_t *nargs, struct va_list_t &ap) const {
        dtype_t *arg;
        dtype_t **p;
        if (allow_realloc) {
            if (ap.using_ptrs) {
                p = (dtype_t**)get_va_list_ptr_ref_cpp(&ap);
            } else {
                p = va_arg(ap.va, dtype_t**);
            }
            bool new_obj = false;
            if (p[0] == NULL)
                new_obj = true;
            dtype_t *temp = (dtype_t*)realloc(p[0], sizeof(dtype_t));
            if (temp == NULL) {
                ygglog_throw_error("SchemaMetaschemaType::decode_data: Failed to realloc variable.");
            }
            if (new_obj) {
                temp->type[0] = '\0';
                temp->use_generic = false;
                temp->obj = NULL;
            }
            p[0] = temp;
            arg = *p;
        } else {
            if (ap.using_ptrs) {
                arg = (dtype_t*)get_va_list_ptr_cpp(&ap);
            } else {
                arg = va_arg(ap.va, dtype_t*);
            }
            p = &arg;
        }
        (*nargs)--;
        arg->type[0] = '\0';
        arg->use_generic = false;
        if (arg->obj != NULL) {
            ygglog_info("SchemaMetaschemaType::decode_data: Datatype has existing type. Deleting.");
            MetaschemaType* old_obj = (MetaschemaType*)(arg->obj);
            delete old_obj;
        }
        arg->obj = NULL;
        MetaschemaType* obj = (MetaschemaType*)type_from_doc_c(&data, use_generic());
        if (obj == NULL) {
            ygglog_throw_error("SchemaMetaschemaType::decode_data: Failed to decode type from JSON document.");
        }
        arg->obj = obj;
        arg->use_generic = obj->use_generic();
        strncpy(arg->type, obj->type(), COMMBUFFSIZ);
        display_dtype(arg, "");
        return true;
    }