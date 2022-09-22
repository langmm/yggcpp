#include "JSONObjectMetaschemaType.hpp"

using namespace communication::datatypes::Metaschema;
using namespace communication::utils;

JSONObjectMetaschemaType::JSONObjectMetaschemaType(const rapidjson::Value &type_doc,
                                                   const bool use_generic,
                                                   const char prop_key[100]) :
// Always generic
        MetaschemaType(type_doc, true) {
    UNUSED(use_generic);
    prop_key_[0] = '\0';
    strncpy(prop_key_, prop_key, 100);
    if (!(type_doc.HasMember(prop_key_)))
        ygglog_throw_error("JSONObjectMetaschemaType: Properties missing.");
    if (!(type_doc[prop_key_].IsObject()))
        ygglog_throw_error("JSONObjectMetaschemaType: Properties must be an object.");
    MetaschemaTypeMap properties;
    for (rapidjson::Value::ConstMemberIterator itr = type_doc[prop_key_].MemberBegin();
         itr != type_doc[prop_key_].MemberEnd(); ++itr) {
        MetaschemaType *iprop = (MetaschemaType *) type_from_doc_c(&(itr->value), MetaschemaType::use_generic());
        properties[itr->name.GetString()] = iprop;
    }
    update_properties(properties, true);
}

JSONObjectMetaschemaType::JSONObjectMetaschemaType(PyObject *pyobj, const bool use_generic,
                                                   const char prop_key[100]) :
// Always generic
        MetaschemaType(pyobj, true) {
    UNUSED(use_generic);
    prop_key_[0] = '\0';
    strncpy(prop_key_, prop_key, 100);
    PyObject *pyprops = get_item_python_dict(pyobj, prop_key_,
                                             "JSONObjectMetaschemaType: properties: ",
                                             T_OBJECT);
    MetaschemaTypeMap properties;
    PyObject *pykeys = PyDict_Keys(pyprops);
    if (pykeys == NULL) {
        ygglog_throw_error("JSONObjectMetaschemaType: Failed to get keys from Python dictionary.");
    }
    size_t i, nkeys = PyList_Size(pykeys);
    for (i = 0; i < nkeys; i++) {
        char ikey[100] = "";
        get_item_python_list_c(pykeys, i, ikey,
                               "JSONObjectMetaschemaType: keys: ",
                               T_STRING, 100);
        PyObject *ipyprop = get_item_python_dict(pyprops, ikey,
                                                 "JSONObjectMetaschemaType: properties: ",
                                                 T_OBJECT);
        MetaschemaType *iprop = (MetaschemaType *) type_from_pyobj_c(ipyprop, MetaschemaType::use_generic());
        if (iprop == NULL) {
            ygglog_throw_error(
                    "JSONObjectMetaschemaType: Failed to reconstruct type for property '%s' from the Python object.",
                    ikey);
        }
        properties[ikey] = iprop;
    }
    update_properties(properties, true);
}

bool JSONObjectMetaschemaType::operator==(const MetaschemaType &Ref) const {
    if (!(MetaschemaType::operator==(Ref)))
        return false;
    const JSONObjectMetaschemaType *pRef = dynamic_cast<const JSONObjectMetaschemaType *>(&Ref);
    if (!pRef)
        return false;
    if (nitems() != pRef->nitems())
        return false;
    MetaschemaTypeMap::const_iterator it;
    MetaschemaTypeMap::const_iterator oit;
    MetaschemaTypeMap new_properties = pRef->properties();
    for (it = properties_.begin(); it != properties_.end(); it++) {
        oit = new_properties.find(it->first);
        if (oit == new_properties.end()) {
            return false;
        }
        if (*(it->second) != *(oit->second)) {
            return false;
        }
    }
    return true;
}

void JSONObjectMetaschemaType::display(const char *indent) const {
    MetaschemaType::display(indent);
    MetaschemaTypeMap::const_iterator it;
    char new_indent[100] = "";
    strcat(new_indent, indent);
    strcat(new_indent, "    ");
    for (it = properties_.begin(); it != properties_.end(); it++) {
        printf("%sElement %s:\n", indent, it->first.c_str());
        it->second->display(new_indent);
    }
}

PyObject *JSONObjectMetaschemaType::as_python_dict() const {
    PyObject *out = MetaschemaType::as_python_dict();
    PyObject *pyprops = PyDict_New();
    MetaschemaTypeMap::const_iterator it;
    for (it = properties_.begin(); it != properties_.end(); it++) {
        PyObject *ipyitem = it->second->as_python_dict();
        set_item_python_dict(pyprops, it->first.c_str(), ipyitem,
                             "JSONObjectMetaschemaType::as_python_dict: properties: ",
                             T_OBJECT);
    }
    set_item_python_dict(out, prop_key_, pyprops,
                         "JSONObjectMetaschemaType::as_python_dict: ",
                         T_OBJECT);
    return out;
}

Dict *JSONObjectMetaschemaType::copy_generic(const YggGeneric *data, Dict *orig_data) const {
    if (data == NULL) {
        ygglog_throw_error("JSONObjectMetaschemaType::copy_generic: Generic object is NULL.");
    }
    void *out = NULL;
    if (orig_data == NULL) {
        orig_data = data->get_data();
    }
    if (orig_data != NULL) {
        YggGenericMap *old_data = (YggGenericMap *) orig_data;
        YggGenericMap *new_data = new YggGenericMap();
        YggGenericMap::iterator it;
        for (it = old_data->begin(); it != old_data->end(); it++) {
            (*new_data)[it->first] = (it->second)->copy();
        }
        out = (void *) (new_data);
    }
    return out;
}

void JSONObjectMetaschemaType::free_generic(YggGeneric *data) const {
    if (data == NULL) {
        ygglog_throw_error("JSONObjectMetaschemaType::free_generic: Generic object is NULL.");
    }
    YggGenericMap **ptr = (YggGenericMap **) (data->get_data_pointer());
    if (ptr[0] != NULL) {
        YggGenericMap::iterator it;
        for (it = (*ptr)->begin(); it != (*ptr)->end(); it++) {
            delete it->second;
        }
        delete ptr[0];
        ptr[0] = NULL;
    }
}

void JSONObjectMetaschemaType::display_generic(const YggGeneric *data, const char *indent) const {
    if (data == NULL) {
        ygglog_throw_error("JSONObjectMetaschemaType::display_generic: Generic object is NULL.");
    }
    YggGenericMap arg;
    YggGenericMap::iterator it;
    char new_indent[100] = "";
    strcat(new_indent, indent);
    strcat(new_indent, "    ");
    data->get_data(arg);
    printf("%sObject with %zu elements:\n", indent, arg.size());
    for (it = arg.begin(); it != arg.end(); it++) {
        std::cout << new_indent << std::left << std::setw(10) << it->first << " ";
        (it->second)->display(new_indent);
    }
}

void JSONObjectMetaschemaType::update_type_element(const char *k, const MetaschemaType *x) {
    MetaschemaTypeMap::iterator it;
    MetaschemaTypeMap::iterator old_it;
    old_it = properties_.find(k);
    if (old_it != properties_.end()) {
        MetaschemaType *old = old_it->second;
        delete old;
        old = NULL;
    }
    properties_[k] = x->copy();
}

void JSONObjectMetaschemaType::update_properties(const MetaschemaTypeMap new_properties,
                                                 bool force) {
    if (force) {
        free_properties();
    }
    if (properties_.size() > 0) {
        if (properties_.size() != new_properties.size()) {
            ygglog_throw_error(
                    "JSONObjectMetaschemaType::update_properties: Cannot update object with %ld elements from an object with %ld elements.",
                    properties_.size(), new_properties.size());
        }
        MetaschemaTypeMap::iterator it;
        MetaschemaTypeMap::const_iterator new_it;
        for (it = properties_.begin(); it != properties_.end(); it++) {
            new_it = new_properties.find(it->first);
            if (new_it == new_properties.end()) {
                ygglog_throw_error(
                        "JSONObjectMetaschemaType::update_properties: New property map dosn't include old property '%s'.",
                        it->first.c_str());
            }
            if (it->second == NULL) {
                ygglog_throw_error(
                        "JSONObjectMetaschemaType::update_properties: Existing value for property '%s' is NULL.",
                        it->first.c_str());
            } else {
                it->second->update(new_it->second);
            }
        }
    } else {
        MetaschemaTypeMap::const_iterator it;
        for (it = new_properties.begin(); it != new_properties.end(); it++) {
            if (it->second == NULL) {
                ygglog_throw_error("JSONObjectMetaschemaType::update_properties: New value for property '%s' is NULL.",
                                   it->first.c_str());
            } else {
                properties_[it->first] = it->second->copy();
            }
        }
    }
    // Force children to follow parent use_generic
    update_use_generic(use_generic());
}

void JSONObjectMetaschemaType::update_use_generic(const bool new_use_generic) {
    MetaschemaType::update_use_generic(new_use_generic);
// Force children to follow parent use_generic
    MetaschemaTypeMap::iterator it;
    for (it = properties_.begin(); it != properties_.end(); it++) {
        if (it->second == NULL) {
            ygglog_throw_error("JSONObjectMetaschemaType::update_use_generic: Value for property %s is NULL.",
                               it->first.c_str());
        } else {
            if ((it->second->type_code() == T_ARRAY) ||
                (it->second->type_code() == T_OBJECT)) {
                it->second->update_use_generic(true);
            } else {
                it->second->update_use_generic(use_generic());
            }
        }
    }
}

void JSONObjectMetaschemaType::set_property_type(const char *key, const MetaschemaType *proptype) {
    MetaschemaTypeMap::const_iterator it = properties_.find(key);
    if (it != properties_.end()) {
        if ((*(it->second)) != (*proptype)) {
            printf("New type:\n");
            proptype->display();
            printf("Existing type:\n");
            it->second->display();
            ygglog_throw_error(
                    "JSONObjectMetaschemaType::set_property_type: New type dosn't match existing type for property '%s'",
                    key);
        }
    } else {
        properties_[key] = proptype->copy();
    }
}

size_t JSONObjectMetaschemaType::update_from_serialization_args(size_t *nargs, va_list_t &ap) {
    size_t iout;
    size_t out = MetaschemaType::update_from_serialization_args(nargs, ap);
    if (use_generic())
        return out;
    MetaschemaTypeMap::const_iterator it;
    size_t new_nargs;
    for (it = properties_.begin(); it != properties_.end(); it++) {
        new_nargs = nargs[0] - out;
        iout = it->second->update_from_serialization_args(&new_nargs, ap);
        if (iout == 0) {
            iout += it->second->nargs_exp();
// Can't use void* because serialization uses non-pointer arguments
            std::vector<size_t> iva_skip = it->second->nbytes_va();
            if (iva_skip.size() != iout) {
                ygglog_throw_error(
                        "JSONObjectMetaschemaType::update_from_serialization_args: nargs = %lu, size(skip) = %lu",
                        iout, iva_skip.size());
            }
            size_t iskip;
            for (iskip = 0; iskip < iva_skip.size(); iskip++) {
                va_list_t_skip(&ap, iva_skip[iskip]);
            }
        }
        out = out + iout;
    }
    return out;
}

size_t JSONObjectMetaschemaType::update_from_deserialization_args(size_t *nargs, va_list_t &ap) {
    size_t iout;
    size_t out = MetaschemaType::update_from_deserialization_args(nargs, ap);
    if (use_generic())
        return out;
    MetaschemaTypeMap::const_iterator it;
    size_t new_nargs;
    for (it = properties_.begin(); it != properties_.end(); it++) {
        new_nargs = nargs[0] - out;
        iout = it->second->update_from_deserialization_args(&new_nargs, ap);
        if (iout == 0) {
            for (iout = 0; iout < it->second->nargs_exp(); iout++) {
                va_list_t_skip(&ap, sizeof(void *));
            }
        }
        out = out + iout;
    }
    return out;
}

std::vector<size_t> JSONObjectMetaschemaType::nbytes_va_core() const {
    if (!(use_generic())) {
        MetaschemaTypeMap::const_iterator it;
        std::vector<size_t> out;
        std::vector<size_t> iout;
        for (it = properties_.begin(); it != properties_.end(); it++) {
            iout = it->second->nbytes_va();
            out.insert(out.end(), iout.begin(), iout.end());
        }
        return out;
    }
    return MetaschemaType::nbytes_va_core();
}

size_t JSONObjectMetaschemaType::nargs_exp() const {
    size_t nargs = 0;
    if (use_generic()) {
        nargs = 1;
    } else {
        MetaschemaTypeMap::const_iterator it;
        for (it = properties_.begin(); it != properties_.end(); it++) {
            nargs = nargs + it->second->nargs_exp();
        }
    }
    return nargs;
}

YggGeneric *JSONObjectMetaschemaType::python2c(PyObject *pyobj) const {
    if (!(PyDict_Check(pyobj))) {
        ygglog_throw_error("JSONObjectMetaschemaType::python2c: Python object must be a dict.");
    }
    if ((size_t) (PyDict_Size(pyobj)) != nitems()) {
        ygglog_throw_error(
                "JSONObjectMetaschemaType::python2c: Python dict has %lu elements, but the type expects %lu.",
                PyDict_Size(pyobj), nitems());
    }
    YggGenericMap *cmap = new YggGenericMap();
    MetaschemaTypeMap::const_iterator it;
    for (it = properties_.begin(); it != properties_.end(); it++) {
        PyObject *ipy_item = PyDict_GetItemString(pyobj, it->first.c_str());
        if (ipy_item == NULL) {
            ygglog_throw_error("JSONObjectMetaschemaType::python2c: Failed to get item %s out of the Python dict.",
                               it->first.c_str());
        }
        YggGeneric *ic_item = it->second->python2c(ipy_item);
        (*cmap)[it->first] = ic_item;
    }
    YggGeneric *cobj = new YggGeneric(this, cmap);
    return cobj;
}

PyObject *JSONObjectMetaschemaType::c2python(YggGeneric *cobj) const {
    initialize_python("JSONObjectMetaschemaType::c2python: ");
    PyObject *pyobj = PyDict_New();
    if (pyobj == NULL) {
        ygglog_throw_error("JSONObjectMetaschemaType::c2python: Failed to create new Python dict.");
    }
    YggGenericMap c_map;
    cobj->get_data(c_map);
    if (c_map.size() != nitems()) {
        ygglog_throw_error("JSONObjectMetaschemaType::c2python: Type has %lu elements but object has %lu.", nitems(),
                           c_map.size());
    }
    MetaschemaTypeMap::const_iterator it;
    for (it = properties_.begin(); it != properties_.end(); it++) {
        YggGenericMap::iterator ic_item = c_map.find(it->first);
        if (ic_item == c_map.end()) {
            ygglog_throw_error("JSONObjectMetaschemaType::c2python: C object does not have element %s.",
                               it->first.c_str());
        }
        PyObject *ipy_item = it->second->c2python(ic_item->second);
        if (PyDict_SetItemString(pyobj, it->first.c_str(), ipy_item) < 0) {
            ygglog_throw_error("JSONObjectMetaschemaType::c2python: Error setting item %s in the Python dict.",
                               it->first.c_str());
        }
    }
    return pyobj;
}

bool JSONObjectMetaschemaType::encode_type_prop(rapidjson::Writer<rapidjson::StringBuffer> *writer) const {
    if (!(MetaschemaType::encode_type_prop(writer))) { return false; }
    writer->Key(prop_key_);
    writer->StartObject();
    MetaschemaTypeMap::const_iterator it = properties_.begin();
    for (it = properties_.begin(); it != properties_.end(); it++) {
        writer->Key(it->first.c_str());
        if (!(it->second->encode_type(writer)))
            return false;
    }
    writer->EndObject();
    return true;
}

bool JSONObjectMetaschemaType::encode_data(rapidjson::Writer<rapidjson::StringBuffer> *writer,
                                           size_t *nargs, va_list_t &ap) const {
    writer->StartObject();
    MetaschemaTypeMap::const_iterator it;
    for (it = properties_.begin(); it != properties_.end(); it++) {
        writer->Key(it->first.c_str());
        if (!(it->second->encode_data_wrap(writer, nargs, ap)))
            return false;
    }
    writer->EndObject();
    return true;
}

bool JSONObjectMetaschemaType::encode_data(rapidjson::Writer<rapidjson::StringBuffer> *writer,
                                           YggGenericMap arg) const {
    writer->StartObject();
    MetaschemaTypeMap::const_iterator it;
    for (it = properties_.begin(); it != properties_.end(); it++) {
        YggGenericMap::iterator iarg = arg.find(it->first);
        if (iarg == arg.end()) {
            ygglog_throw_error("JSONObjectMetaschemaType::encode_data: Object does not have element %s.",
                               it->first.c_str());
            return false;
        }
        writer->Key(it->first.c_str());
        if (!(it->second->encode_data(writer, iarg->second)))
            return false;
    }
    writer->EndObject();
    return true;
}

bool JSONObjectMetaschemaType::decode_data(rapidjson::Value &data, const int allow_realloc,
                                           size_t *nargs, va_list_t &ap) const {
    if (!(data.IsObject())) {
        ygglog_error("JSONObjectMetaschemaType::decode_data: Raw data is not an object.");
        return false;
    }
    MetaschemaTypeMap::const_iterator it;
    for (it = properties_.begin(); it != properties_.end(); it++) {
        if (!(data.HasMember(it->first.c_str()))) {
            ygglog_error("JSONObjectMetaschemaType::decode_data: Data doesn't have member '%s'.",
                         it->first.c_str());
            return false;
        }
        if (!(it->second->decode_data_wrap(data[it->first.c_str()], allow_realloc, nargs, ap)))
            return false;
    }
    return true;
}

bool JSONObjectMetaschemaType::decode_data(rapidjson::Value &data, YggGeneric *x) {
    if (!(data.IsObject())) {
        ygglog_error("JSONObjectMetaschemaType::decode_data: Raw data is not an object.");
        return false;
    }
    if (x == NULL) {
        ygglog_error("JSONObjectMetaschemaType::decode_data: Generic object is NULL.");
        return false;
    }
    MetaschemaTypeMap::const_iterator it;
    YggGenericMap **arg = (YggGenericMap **) (x->get_data_pointer());
    if (arg == NULL) {
        ygglog_error("JSONObjectMetaschemaType::decode_data: Data pointer is NULL.");
        return false;
    }
    if (arg[0] == NULL) {
        arg[0] = new YggGenericMap();
        for (it = properties_.begin(); it != properties_.end(); it++) {
            (**arg)[it->first] = (new YggGeneric(it->second, NULL, 0));
        }
    } else if ((arg[0])->size() == 0) {
        for (it = properties_.begin(); it != properties_.end(); it++) {
            (**arg)[it->first] = (new YggGeneric(it->second, NULL, 0));
        }
    }
    for (it = properties_.begin(); it != properties_.end(); it++) {
        if (!(data.HasMember(it->first.c_str()))) {
            ygglog_error("JSONObjectMetaschemaType::decode_data: Data doesn't have member '%s'.",
                         it->first.c_str());
            return false;
        }
        YggGenericMap::iterator iarg = (*arg)->find(it->first);
        if (iarg == (*arg)->end()) {
            ygglog_error("JSONObjectMetaschemaType::decode_data: Destination dosn't have member '%s'.",
                         it->first.c_str());
            return false;
        }
        if (!(it->second->decode_data(data[it->first.c_str()], iarg->second)))
            return false;
    }
    return true;
}
