#include "YggGeneric.hpp"
#include "utils/tools.hpp"
#include "MetaschemaType.hpp"

using namespace communication::datatypes::Metaschema;
using namespace communication::utils;

YggGeneric::YggGeneric() : type(NULL), data(NULL), nbytes(0) {}

YggGeneric::YggGeneric(const MetaschemaType *in_type, Dict *in_data, size_t in_nbytes) : type(NULL), data(NULL),
                                                                                         nbytes(in_nbytes) {
    set_type(in_type);
    if (nbytes == 0) {
        nbytes = type->nbytes();
    }
    set_data(in_data);
}

YggGeneric::YggGeneric(const YggGeneric &other) :
        YggGeneric(other.get_type(), other.get_data(), other.get_nbytes()) {}

YggGeneric::YggGeneric(const YggGeneric* other) :
        YggGeneric(other->get_type(), other->get_data(), other->get_nbytes()){}

YggGeneric::~YggGeneric() {
    free_data();
    free_type();
}

void YggGeneric::display(const char *indent) const {
    type->display_generic(this, indent);
}

void *YggGeneric::copy_data(Dict *orig_data) const {
    if (orig_data == nullptr)
        orig_data = data;
    if (orig_data == nullptr)
        return NULL;
    return type->copy_generic(this, orig_data);
}

void YggGeneric::free_data() {
    if ((data != NULL) && (type != NULL)) {
        type->free_generic(this);
    }
    data = NULL;
}

void YggGeneric::free_type() {
    if (type != NULL) {
        delete type;
        type = NULL;
    }
}

void YggGeneric::set_type(const MetaschemaType *new_type) {
    type = new_type->copy();
}

MetaschemaType *YggGeneric::get_type() const {
    return type;
}

void YggGeneric::set_nbytes(size_t new_nbytes) {
    nbytes = new_nbytes;
}

size_t YggGeneric::get_nbytes() const {
    return nbytes;
}

size_t *YggGeneric::get_nbytes_pointer() {
    return &nbytes;
}

size_t YggGeneric::get_nelements() const {
    try {
        return type->nelements();
    } catch (...) {
        return 1;
    }
}

void YggGeneric::set_data(Dict *new_data) {
    free_data();
    data = copy_data(new_data);
}

Dict *YggGeneric::get_data() const {
    return data;
}

Dict **YggGeneric::get_data_pointer() {
    return &data;
}

template<typename T>
void YggGeneric::get_data(T *obj, size_t nelements, bool is_char) const {
    size_t obj_size = nelements * sizeof(T);
    bool check;
    if (is_char) {
        check = (obj_size > nbytes);
    } else {
        check = (obj_size != nbytes);
    }
    if (check) {
        ygglog_throw_error(
                "YggGeneric::get_data: Type indicates the data has a size of %d bytes, but the provided pointer is to an object with a size of %d bytes.",
                nbytes, sizeof(T));
    }
    memcpy((void *) obj, data, nbytes);
}

template<typename T>
void YggGeneric::get_data(T &obj) const {
    if (nbytes != sizeof(T)) {
        ygglog_throw_error(
                "YggGeneric::get_data: There are %d elements in the data, but this call signature returns one (provided type has size %d bytes, but object stores %d bytes).",
                nbytes / sizeof(T),
                sizeof(T), nbytes);
    }
    T *ptr = static_cast<T *>(data);
    obj = *ptr;
}

template<typename T>
void YggGeneric::get_data_realloc(T **obj, size_t *nelements) const {
    T *new_obj = (T *) realloc(obj[0], nbytes);
    if (new_obj == NULL) {
        ygglog_throw_error("YggGeneric::get_data_realloc: Failed to reallocated input variables.");
    }
    obj[0] = new_obj;
    if (nelements != NULL) {
        nelements[0] = nbytes / sizeof(T);
    }
    get_data(obj[0], nbytes / sizeof(T));
}

void YggGeneric::get_data(char *obj, size_t nelements) const {
    get_data(obj, nelements, true);
}

void YggGeneric::add_array_element(YggGeneric *x) {
    if (type->type_code() != T_ARRAY)
        ygglog_throw_error("YggGeneric::add_array_element: Generic object is not an array.");
    YggGenericVector *v = (YggGenericVector *) get_data();
    size_t i = v->size();
    set_array_element(i, x);
}

void YggGeneric::set_array_element(size_t i, YggGeneric *x) {
    if (type->type_code() != T_ARRAY)
        ygglog_throw_error("YggGeneric::set_array_element: Generic object is not an array.");
    YggGenericVector *v = (YggGenericVector *) get_data();
    if (i > v->size()) {
        ygglog_throw_error(
                "YggGeneric::set_array_element: Cannot set element %lu, there are only %lu elements in the array.",
                i, v->size());
    } else if (i == v->size()) {
        v->push_back(x->copy());
    } else {
        YggGeneric *old = (*v)[i];
        delete old;
        (*v)[i] = x->copy();
    }
    type->update_type_element(i, x->type);
}

YggGeneric *YggGeneric::get_array_element(size_t i) {
    if (type->type_code() != T_ARRAY)
        ygglog_throw_error("YggGeneric::get_array_element: Generic object is not an array.");
    YggGenericVector *v = (YggGenericVector *) get_data();
    if (i >= v->size()) {
        ygglog_throw_error(
                "YggGeneric::get_array_element: Cannot get element %lu, there are only %lu elements in the array.",
                i, v->size());
    }
    YggGeneric *out = (*v)[i];
    return out;
}

void YggGeneric::set_object_element(const char *k, YggGeneric *x) {
    if (type->type_code() != T_OBJECT)
        ygglog_throw_error("YggGeneric::set_object_element: Generic object is not an object.");
    YggGenericMap *v = (YggGenericMap *) get_data();
    YggGenericMap::iterator it;
    it = v->find(k);
    if (it != v->end()) {
        delete it->second;
    }
    (*v)[k] = x;
    type->update_type_element(k, x->type);
}

YggGeneric *YggGeneric::get_object_element(const char *k) {
    if (type->type_code() != T_OBJECT)
        ygglog_throw_error("YggGeneric::get_object_element: Generic object is not an object.");
    YggGenericMap *v = (YggGenericMap *) get_data();
    YggGenericMap::iterator it;
    it = v->find(k);
    if (it == v->end()) {
        ygglog_throw_error("YggGeneric::get_object_element: Cannot get element for key %s, it does not exist.", k);
    }
    YggGeneric *out = it->second;
    return out;
}

void *YggGeneric::get_data(const MetaschemaType *exp_type) const {
    if (*type != *exp_type) {
        printf("Wrapped Type:\n");
        type->display();
        printf("Excepted Type:\n");
        exp_type->display();
        ygglog_throw_error("YggGeneric::get_data: Type of generic wrapped data does not match expected type.");
    }
    return get_data();
}

size_t YggGeneric::get_data_array_size() const {
    if (type->type_code() != T_ARRAY) {
        ygglog_throw_error("YggGeneric::get_data_array_size: Object is not an array.");
    }
    YggGenericVector *arr = (YggGenericVector *) get_data();
    if (arr == NULL) {
        ygglog_throw_error("YggGeneric::get_data_array_size: Array is NULL.");
    }
    return arr->size();
}

size_t YggGeneric::get_data_map_size() const {
    if (type->type_code() != T_OBJECT) {
        ygglog_throw_error("YggGeneric::get_data_map_size: Object is not a map.");
    }
    YggGenericMap *map = (YggGenericMap *) get_data();
    if (map == NULL) {
        ygglog_throw_error("YggGeneric::get_data_map_size: Map is NULL.");
    }
    return map->size();
}

bool YggGeneric::has_data_map_key(char *key) const {
    if (type->type_code() != T_OBJECT) {
        ygglog_throw_error("YggGeneric::get_data_map_keys: Object is not a map.");
    }
    YggGenericMap *map = (YggGenericMap *) get_data();
    if (map == NULL) {
        ygglog_throw_error("YggGeneric::get_data_map_keys: Map is NULL.");
    }
    YggGenericMap::iterator it = map->find(key);
    if (it != map->end()) {
        return true;
    } else {
        return false;
    }
}

size_t YggGeneric::get_data_map_keys(char ***keys) const {
    size_t i;
    if (type->type_code() != T_OBJECT) {
        ygglog_throw_error("YggGeneric::get_data_map_keys: Object is not a map.");
    }
    YggGenericMap *map = (YggGenericMap *) get_data();
    if (map == NULL) {
        ygglog_throw_error("YggGeneric::get_data_map_keys: Map is NULL.");
    }
    keys[0] = (char **) realloc(keys[0], map->size() * sizeof(char *));
    YggGenericMap::iterator it;
    for (i = 0, it = map->begin(); it != map->end(); it++, i++) {
        keys[0][i] = (char *) malloc(it->first.length() + 1);
        // keys[0][i] = (char*)realloc(keys[0][i], it->first.length() + 1);
        strcpy(keys[0][i], it->first.c_str());
    }
    return map->size();
}

void *YggGeneric::get_data_array_item(const size_t i,
                                      const MetaschemaType *item_type,
                                      bool return_generic) const {
    if (type->type_code() != T_ARRAY) {
        ygglog_throw_error("YggGeneric::get_data_array_item: Object is not an array.");
    }
    YggGenericVector *arr = (YggGenericVector *) get_data();
    if (arr == NULL) {
        ygglog_throw_error("YggGeneric::get_data_array_item: Array is NULL.");
    }
    if (i > arr->size()) {
        ygglog_throw_error("YggGeneric::get_data_array_item: Array has %lu elements, but %lu were requested.",
                           arr->size(), i);
    }
    YggGeneric *out = (*arr)[i];
    if (return_generic) {
        return (void *) out;
    } else {
        return out->get_data(item_type);
    }
}

size_t YggGeneric::get_nbytes_array_item(const size_t i) const {
    if (type->type_code() != T_ARRAY) {
        ygglog_throw_error("YggGeneric::get_nbytes_array_item: Object is not an array.");
    }
    YggGenericVector *arr = (YggGenericVector *) get_data();
    if (arr == NULL) {
        ygglog_throw_error("YggGeneric::get_nbytes_array_item: Array is NULL.");
    }
    if (i > arr->size()) {
        ygglog_throw_error("YggGeneric::get_nbytes_array_item: Array has %lu elements, but %lu were requested.",
                           arr->size(), i);
    }
    YggGeneric *out = (*arr)[i];
    return out->nbytes;
}

void YggGeneric::set_data_array_item(const size_t index,
                                     const YggGeneric *value) {
    if (type->type_code() != T_ARRAY) {
        ygglog_throw_error("YggGeneric::set_data_array_item: Object is not an array.");
    }
    YggGenericVector *arr = (YggGenericVector *) get_data();
    if (arr == NULL) {
        ygglog_throw_error("YggGeneric::set_data_array_item: Array is NULL.");
    }
    type->set_item_type(index, value->get_type());
    if (index < arr->size()) {
        (*arr)[index] = value->copy();
    } else {
        arr->push_back(value->copy());
    }
}

void *YggGeneric::get_data_map_item(const char *key,
                                    const MetaschemaType *item_type,
                                    bool return_generic) const {
    if (type->type_code() != T_OBJECT) {
        ygglog_throw_error("YggGeneric::get_data_map_item: Object is not a map.");
    }
    YggGenericMap *map = (YggGenericMap *) get_data();
    if (map == NULL) {
        ygglog_throw_error("YggGeneric::get_data_map_item: Map is NULL.");
    }
    YggGenericMap::iterator map_it = map->find(key);
    if (map_it == map->end()) {
        ygglog_throw_error("YggGeneric::get_data_map_item: Could not located item for key '%s'.", key);
    }
    YggGeneric *out = map_it->second;
    if (return_generic) {
        return (void *) out;
    } else {
        return out->get_data(item_type);
    }
}

size_t YggGeneric::get_nbytes_map_item(const char *key) const {
    if (type->type_code() != T_OBJECT) {
        ygglog_throw_error("YggGeneric::get_nbytes_map_item: Object is not a map.");
    }
    YggGenericMap *map = (YggGenericMap *) get_data();
    if (map == NULL) {
        ygglog_throw_error("YggGeneric::get_nbytes_map_item: Map is NULL.");
    }
    YggGenericMap::iterator map_it = map->find(key);
    if (map_it == map->end()) {
        ygglog_throw_error("YggGeneric::get_nbytes_map_item: Could not located item for key '%s'.", key);
    }
    YggGeneric *out = map_it->second;
    return out->nbytes;
}

void YggGeneric::set_data_map_item(const char *key,
                                   const YggGeneric *value) {
    if (type->type_code() != T_OBJECT) {
        ygglog_throw_error("YggGeneric::set_data_map_item: Object is not a map.");
    }
    YggGenericMap *map = (YggGenericMap *) get_data();
    if (map == NULL) {
        ygglog_throw_error("YggGeneric::set_data_map_item: Map is NULL.");
    }
    type->set_property_type(key, value->get_type());
    (*map)[key] = value->copy();
}
