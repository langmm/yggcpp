#include "datatypes.hpp"
#include "dtype_t.hpp"
#include "utils/tools.hpp"
#include "utils/regex.hpp"

namespace communication {
namespace datatypes {

#define CSafe(x)  \
  try          \
    {          \
      x;      \
    }          \
  catch(...)      \
    {          \
      utils::ygglog_error("C++ exception thrown.");    \
    }

/*!
  @brief Split header and body of message.
  @param[in] buf const char* Message that should be split.
  @param[in] buf_siz size_t Size of buf.
  @param[out] head const char** pointer to buffer where the extracted header
  should be stored.
  @param[out] headsiz size_t reference to memory where size of extracted header
  should be stored.
  @returns: int 0 if split is successful, -1 if there was an error.
*/
int split_head_body(const char *buf, const size_t buf_siz,
                    char **head, size_t *headsiz) {
    // Split buffer into head and body
    int ret;
    size_t sind, eind, sind_head, eind_head;
    sind = 0;
    eind = 0;
#ifdef _WIN32
    // Windows regex of newline is buggy
  UNUSED(buf_siz);
  size_t sind1, eind1, sind2, eind2;
  char re_head_tag[COMMBUFFSIZ];
  sprintf(re_head_tag, "(%s)", MSG_HEAD_SEP);
  ret = find_match(re_head_tag, buf, &sind1, &eind1);
  if (ret > 0) {
    sind = sind1;
    ret = find_match(re_head_tag, buf + eind1, &sind2, &eind2);
    if (ret > 0)
      eind = eind1 + eind2;
  }
#else
    // Extract just header
    char re_head[COMMBUFFSIZ] = MSG_HEAD_SEP;
    strcat(re_head, "(.*)");
    strcat(re_head, MSG_HEAD_SEP);
    // strcat(re_head, ".*");
    ret = static_cast<int>(communication::utils::find_match(re_head, buf, sind, eind));
#endif
    if (ret < 0) {
        utils::ygglog_error("split_head_body: Could not find header in '%.1000s'", buf);
        return -1;
    } else if (ret == 0) {
#ifdef YGG_DEBUG
        utils::ygglog_debug("split_head_body: No header in '%.1000s...'", buf);
#endif
        sind_head = 0;
        eind_head = 0;
    } else {
        sind_head = sind + strlen(MSG_HEAD_SEP);
        eind_head = eind - strlen(MSG_HEAD_SEP);
    }
    headsiz[0] = (eind_head - sind_head);
    char* temp = (char*)realloc(*head, *headsiz + 1);
    if (temp == nullptr) {
        utils::ygglog_error("split_head_body: Failed to reallocate header.");
        return -1;
    }
    *head = temp;
    memcpy(*head, buf + sind_head, *headsiz);
    (*head)[*headsiz] = '\0';
    return 0;
}

std::map<const char*, int, strcomp> datatypes::get_type_map() {
    std::map<const char*, int, strcomp> global_type_map;
    if (global_type_map.empty()) {
        // Standard types
        global_type_map["boolean"] = T_BOOLEAN;
        global_type_map["integer"] = T_INT;
        global_type_map["null"] = T_NULL;
        global_type_map["number"] = T_FLOAT;
        global_type_map["string"] = T_STRING;
        // Enhanced types
        global_type_map["array"] = T_1DARRAY;
        global_type_map["object"] = T_OBJECT;
        // Non-standard types
        global_type_map["direct"] = T_DIRECT;
        global_type_map["1darray"] = T_1DARRAY;
        global_type_map["ndarray"] = T_NDARRAY;
        global_type_map["scalar"] = T_SCALAR;
        global_type_map["float"] = T_FLOAT;
        global_type_map["uint"] = T_UINT;
        global_type_map["int"] = T_INT;
        global_type_map["complex"] = T_COMPLEX;
        global_type_map["bytes"] = T_BYTES;
        global_type_map["unicode"] = T_UNICODE;
        global_type_map["ply"] = T_PLY;
        global_type_map["obj"] = T_OBJ;
        global_type_map["class"] = T_CLASS;
        global_type_map["function"] = T_FUNCTION;
        global_type_map["instance"] = T_INSTANCE;
        global_type_map["schema"] = T_SCHEMA;
        global_type_map["any"] = T_ANY;
        // Aliases
        global_type_map["map"] = T_OBJECT;
    }
    return global_type_map;
}
DataType* type_from_header_doc (const rapidjson::Value &header_doc,
                                                  const bool use_generic) {
    if (!(header_doc.IsObject()))
        utils::ygglog_throw_error("type_from_header_doc: Parsed document is not an object.");
    if (!(header_doc.HasMember("datatype")))
        utils::ygglog_throw_error("type_from_header_doc: Parsed header dosn't contain a 'datatype' entry.");
    if (!(header_doc["datatype"].IsObject()))
        utils::ygglog_throw_error("type_from_header_doc: Parsed datatype is not an object.");
    return type_from_doc(header_doc["datatype"], use_generic, &header_doc);

}

DataType* type_from_doc(const rapidjson::Value &type_doc,
                        const bool use_generic,
                        const rapidjson::Value *header_doc){
    if (!(type_doc.IsObject()))
        utils::ygglog_throw_error("type_from_doc: Parsed document is not an object.");
    if (!(type_doc.HasMember("type")))
        utils::ygglog_throw_error("type_from_doc: Parsed header dosn't contain a type.");
    if (!(type_doc["type"].IsString()))
        utils::ygglog_throw_error("type_from_doc: Type in parsed header is not a string.");
    const char *type = type_doc["type"].GetString();
    auto type_map = datatypes::get_type_map();
    auto it = type_map.find(type);
    if (it != type_map.end()) {
        switch (it->second) {
            // Standard types
            case T_BOOLEAN:
            case T_INTEGER:
            case T_NULL:
            case T_NUMBER:
            case T_STRING:
                return new MetaschemaType(type_doc, use_generic);
                // Enhanced types
            case T_ARRAY: {
                char format_str[1001] = "";
                if (header_doc != nullptr) {
                    if (header_doc->HasMember("format_str")) {
                        if (!((*header_doc)["format_str"].IsString()))
                            utils::ygglog_throw_error("type_from_doc: JSONArrayMetaschemaType: format_str must be a string.");
                        strncpy(format_str, (*header_doc)["format_str"].GetString(), 1000);
                    }
                }
                return new JSONArrayMetaschemaType(type_doc, format_str, use_generic);
            }
            case T_OBJECT:
                return new JSONObjectMetaschemaType(type_doc, use_generic);
                // Non-standard types
            case T_DIRECT:
                return new DirectMetaschemaType(type_doc, use_generic);
            case T_1DARRAY:
                return new OneDArrayMetaschemaType(type_doc, use_generic);
            case T_NDARRAY:
                return new NDArrayMetaschemaType(type_doc, use_generic);
            case T_SCALAR:
            case T_FLOAT:
            case T_UINT:
            case T_INT:
            case T_COMPLEX:
            case T_BYTES:
            case T_UNICODE:
                return new ScalarMetaschemaType(type_doc, use_generic);
            case T_PLY:
                return new PlyMetaschemaType(type_doc, use_generic);
            case T_OBJ:
                return new ObjMetaschemaType(type_doc, use_generic);
            case T_CLASS:
            case T_FUNCTION:
                return new PyObjMetaschemaType(type_doc, use_generic);
            case T_INSTANCE:
                return new PyInstMetaschemaType(type_doc, use_generic);
            case T_SCHEMA:
                return new SchemaMetaschemaType(type_doc, use_generic);
            case T_ANY:
                return new AnyMetaschemaType(type_doc, use_generic);
        }
    }
    utils::ygglog_throw_error("Could not find class from doc for type '%s'.", type);
    return nullptr;
}

Metaschema::MetaschemaType* type_from_doc_c(const rapidjson::Value* type_doc, const bool use_generic) {
    MetaschemaType* out = nullptr;
    try {
        auto type_doc_cpp = dynamic_cast<const rapidjson::Value*>(type_doc);
        out = type_from_doc(*type_doc_cpp, use_generic);
    } catch(...) {
        utils::ygglog_error("type_from_doc_c: C++ exception thrown.");
        if (out != nullptr) {
            delete out;
            out = nullptr;
        }
    }
    return out;
}

Metaschema::MetaschemaType *dtype2class(const dtype_t *dtype) {
    if (dtype == nullptr) {
        utils::ygglog_throw_error("dtype2class: Pointer to data structure is NULL.");
    } else if (dtype->obj == nullptr) {
        utils::ygglog_throw_error("dtype2class: C++ data type structure is NULL.");
    }
    auto type_map = datatypes::get_type_map();
    auto it = type_map.find(dtype->type);
    if (it != type_map.end()) {
        switch (it->second) {
            case T_BOOLEAN:
            case T_INTEGER:
            case T_NULL:
            case T_NUMBER:
            case T_STRING:
                return static_cast<MetaschemaType *>(dtype->obj);
            case T_ARRAY:
                return static_cast<JSONArrayMetaschemaType *>(dtype->obj);
            case T_OBJECT:
                return static_cast<JSONObjectMetaschemaType *>(dtype->obj);
            case T_DIRECT:
                return static_cast<DirectMetaschemaType *>(dtype->obj);
            case T_1DARRAY:
                return static_cast<OneDArrayMetaschemaType *>(dtype->obj);
            case T_NDARRAY:
                return static_cast<NDArrayMetaschemaType *>(dtype->obj);
            case T_SCALAR:
            case T_FLOAT:
            case T_UINT:
            case T_INT:
            case T_COMPLEX:
            case T_BYTES:
            case T_UNICODE:
                return static_cast<ScalarMetaschemaType *>(dtype->obj);
            case T_PLY:
                return static_cast<PlyMetaschemaType *>(dtype->obj);
            case T_OBJ:
                return static_cast<ObjMetaschemaType *>(dtype->obj);
            case T_ASCII_TABLE:
                return static_cast<AsciiTableMetaschemaType *>(dtype->obj);
            case T_CLASS:
            case T_FUNCTION:
                return static_cast<PyObjMetaschemaType *>(dtype->obj);
            case T_INSTANCE:
                return static_cast<PyInstMetaschemaType *>(dtype->obj);
            case T_SCHEMA:
                return static_cast<SchemaMetaschemaType *>(dtype->obj);
            case T_ANY:
                return static_cast<AnyMetaschemaType *>(dtype->obj);
        }
    } else {
        utils::ygglog_throw_error("dtype2class: No handler for type '%s'.", dtype->type);
    }
    return nullptr;
}

void init_dtype_class(dtype_t *dtype, MetaschemaType *type_class) {
    if (dtype == nullptr) {
        utils::ygglog_throw_error("init_dtype_class: data type structure is NULL.");
    } else if (dtype->obj != nullptr) {
        utils::ygglog_throw_error("init_dtype_class: Data type class already set.");
    } else if (strlen(dtype->type) != 0) {
        utils::ygglog_throw_error("init_dtype_class: Data type string already set.");
    }
    dtype->obj = type_class;
    dtype->use_generic = type_class->use_generic();
    strncpy(dtype->type, type_class->type(), COMMBUFFSIZ);
}

/*dtype_t* create_dtype(DTYPE dtype, ushort precision, bool use_generic) {
    switch (dtype) {
        case T_NULL:
            return create_dtype_empty(use_generic);
            break;
        case T_BOOLEAN:
            break;
        case T_INTEGER:
            break;
        case T_NUMBER:
            break;
        case T_STRING:
            break;
        case T_ARRAY:
            break;
        case T_OBJECT:
            break;
        case T_DIRECT:
            break;
        case T_1DARRAY:
            break;
        case T_NDARRAY:
            break;
        case T_SCALAR:
        case T_FLOAT:
        case T_UINT:
        case T_INT:
        case T_COMPLEX:
        case T_BYTES:
        case T_UNICODE:
            return create_dtype_scalar(dtype, precision, use_generic);
            break;
        case T_PLY:
            break;
        case T_OBJ:
            break;
        case T_CLASS:
            break;
        case T_FUNCTION:
            break;
        case T_INSTANCE:
            break;
        case T_SCHEMA:
            break;
        case T_ANY:
            break;
    }
    dtype_t *out = nullptr;
    out = (dtype_t *) malloc(sizeof(dtype_t));
    if (out == nullptr) {
        utils::ygglog_throw_error("create_dtype: Failed to malloc for datatype.");
    }
    out->type = dtype
    out->use_generic = use_generic;
    out->obj = nullptr;
    if (type_class != nullptr) {
        try {
            init_dtype_class(out, type_class);
        } catch (...) {
            free(out);
            out = nullptr;
            utils::ygglog_throw_error("create_dtype: Failed to initialized data type structure with class information.");
        }
    }
    return out;

}*/

int skip_va_elements(const dtype_t* dtype, size_t *nargs, struct va_list_t *ap) {
    try {
        if (dtype == nullptr) {
            return 1;
        }
        if (dtype->obj == nullptr) {
            return 1;
        }
        MetaschemaType *obj = dtype2class(dtype);
        obj->skip_va_elements(nargs, ap);
    } catch(...) {
        utils::ygglog_error("skip_va_elements: C++ exception thrown.");
        return 1;
    }
    return 0;
}


int is_empty_dtype(const dtype_t* dtype) {
    if (dtype == nullptr) {
        return 1;
    }
    if (dtype->obj == nullptr) {
        return 1;
    }
    if (strlen(dtype->type) == 0) {
        return 1;
    }
    try {
        MetaschemaType *obj = dtype2class(dtype);
        if (obj->is_empty())
            return 1;
    } catch(...) {
        utils::ygglog_error("is_empty_dtype: C++ exception thrown.");
        return 1;
    }
    return 0;
}


const char* dtype_name(const dtype_t* type_struct) {
    try {
        MetaschemaType* type_class = dtype2class(type_struct);
        return type_class->type();
    } catch(...) {
        utils::ygglog_error("dtype_name: C++ exception thrown.");
        return "";
    }
}

const char* dtype_subtype(const dtype_t* type_struct) {
    try {
        if (strcmp(type_struct->type, "scalar") != 0) {
            utils::ygglog_throw_error("dtype_subtype: Only scalars have subtype.");
        }
        ScalarMetaschemaType* scalar_class = static_cast<ScalarMetaschemaType*>(type_struct->obj);
        return scalar_class->subtype();
    } catch(...) {
        utils::ygglog_error("dtype_subtype: C++ exception thrown.");
        return "";
    }
}

const size_t dtype_precision(const dtype_t* type_struct) {
    try {
        if (strcmp(type_struct->type, "scalar") != 0) {
            utils::ygglog_throw_error("dtype_precision: Only scalars have precision.");
        }
        ScalarMetaschemaType* scalar_class = static_cast<ScalarMetaschemaType*>(type_struct->obj);
        return scalar_class->precision();
    } catch(...) {
        utils::ygglog_error("dtype_precision: C++ exception thrown.");
        return 0;
    }
}

int set_dtype_name(dtype_t* dtype, const char* name) {
    if (dtype == nullptr) {
        utils::ygglog_error("set_dtype_name: data type structure is nullptr.");
        return -1;
    }
    strncpy(dtype->type, name, COMMBUFFSIZ);
    return 0;
}

dtype_t* complete_dtype(dtype_t *dtype, const bool use_generic) {
    try {
        if (dtype == nullptr) {
            return create_dtype(nullptr, use_generic);
        } else if ((dtype->obj != nullptr) && (strlen(dtype->type) == 0)){
            int ret = set_dtype_name(dtype, dtype_name(dtype));
            if (ret != 0) {
                utils::ygglog_throw_error("complete_dtype: Failed to set data type name.");
            }
        }
    } catch (...) {
        utils::ygglog_error("complete_dtype: C++ exception thrown.");
        return nullptr;
    }
    return dtype;
}

int destroy_dtype(dtype_t **dtype) {
    int ret = 0;
    if (dtype != nullptr) {
        if (dtype[0] != nullptr) {
            if ((dtype[0])->obj != nullptr) {
                try {
                    MetaschemaType *type_class = dtype2class(dtype[0]);
                    ret = destroy_dtype_class_safe(type_class);
                } catch (...) {
                    utils::ygglog_error("destroy_dtype: C++ exception thrown in dtype2class.");
                    ret = -1;
                }
            }
            free(dtype[0]);
            dtype[0] = nullptr;
        }
    }
    return ret;
}


dtype_t* copy_dtype(const dtype_t* dtype) {
    if (dtype == nullptr) {
      return nullptr;
    }
    dtype_t* out = nullptr;
    try {
      MetaschemaType *type = dtype2class(dtype);
      if (type == nullptr) {
	utils::ygglog_throw_error("copy_dtype: Could not recover the type class.");
      }
      out = create_dtype(type->copy());
      return out;
    } catch (...) {
      utils::ygglog_error("copy_dtype: C++ exception thrown.");
      destroy_dtype(&out);
      return nullptr;
    }
}


int update_dtype(dtype_t* dtype1, dtype_t* dtype2) {
    try {
      if ((dtype2 == nullptr) || (dtype2->obj == nullptr)) {
	utils::ygglog_throw_error("update_dtype: Could not recover type to update from.");
      } else if (dtype1 == nullptr) {
	utils::ygglog_throw_error("update_dtype: Could not recover type for update.");
      } else if (dtype1->obj == nullptr) {
	MetaschemaType *type2 = dtype2class(dtype2);
	MetaschemaType *type1 = type2->copy();
	dtype1->obj = type1;
	strcpy(dtype1->type, type1->type());
	type1->update_use_generic(dtype1->use_generic);
      } else {
	MetaschemaType *type1 = dtype2class(dtype1);
	MetaschemaType *type2 = dtype2class(dtype2);
	type1->update(type2);
	strcpy(dtype1->type, type1->type());
      }
    } catch (...) {
      utils::ygglog_error("update_dtype: C++ exception thrown.");
      return -1;
    }
    return 0;
}


int update_dtype_from_generic_ap(dtype_t* dtype1, size_t nargs, struct va_list_t ap) {
    if (!(is_empty_dtype(dtype1))) {
      return 0;
    }
    if (!(dtype1->use_generic)) {
      return 0;
    }
    try {
      generic_t gen_arg = get_generic_va(nargs, ap);
      if (!(is_generic_init(gen_arg))) {
	utils::ygglog_throw_error("update_dtype_from_generic_ap: Type expects generic object, but provided object is not generic.");
      } else {
	dtype_t dtype2;
	auto ygg_gen_arg = (YggGeneric*)(gen_arg.obj);
	MetaschemaType *type_class = ygg_gen_arg->get_type();
	if (type_class == nullptr) {
	  utils::ygglog_throw_error("update_dtype_from_generic_ap: Type in generic class is nullptr.");
	}
	dtype2.obj = (void*)(type_class);
	if (set_dtype_name(&dtype2, type_class->type()) < 0) {
	  return -1;
	}
	if (update_dtype(dtype1, &dtype2) < 0) {
	  return -1;
	}
      }
    } catch (...) {
      utils::ygglog_error("update_dtype_from_generic_ap: C++ exception thrown.");
      return -1;
    }
    return 0;
}


int update_precision_dtype(const dtype_t* dtype,
                           const size_t new_precision) {
    try {
      if (strcmp(dtype->type, "scalar") != 0) {
	utils::ygglog_throw_error("update_precision_dtype: Can only update precision for bytes or unicode scalars.");
      }
      ScalarMetaschemaType *type = dynamic_cast<ScalarMetaschemaType*>(dtype2class(dtype));
      type->set_precision(new_precision);
    } catch (...) {
      utils::ygglog_error("update_precision_dtype: C++ exception thrown.");
      return -1;
    }
    return 0;
}

int deserialize_dtype(const dtype_t *dtype, const char *buf, const size_t buf_siz,
                      const int allow_realloc, size_t *nargs, struct va_list_t ap) {
    try {
      MetaschemaType* type = dtype2class(dtype);
      return type->deserialize(buf, buf_siz, allow_realloc, nargs, ap);
    } catch (...) {
      utils::ygglog_error("deserialize_dtype: C++ exception thrown.");
      return -1;
    }
}


int serialize_dtype(const dtype_t *dtype, char **buf, size_t *buf_siz,
                    const int allow_realloc, size_t *nargs, struct va_list_t ap) {
    try {
      MetaschemaType* type = dtype2class(dtype);
      return type->serialize(buf, buf_siz, allow_realloc, nargs, ap);
    } catch (...) {
      utils::ygglog_error("serialize_dtype: C++ exception thrown.");
      return -1;
    }
}


void display_dtype(const dtype_t *dtype, const char* indent) {
    try {
      MetaschemaType* type = dtype2class(dtype);
      type->display(indent);
    } catch(...) {
      utils::ygglog_error("display_dtype: C++ exception thrown.");
    }
}


size_t nargs_exp_dtype(const dtype_t *dtype) {
    try {
      MetaschemaType* type = dtype2class(dtype);
      return type->nargs_exp();
    } catch(...) {
      utils::ygglog_error("nargs_exp_dtype: C++ exception thrown.");
    }
    return 0;
}



dtype_t* create_dtype_empty(bool use_generic) {
    try {
        auto out = (dtype_t*) malloc(sizeof(dtype_t));
        if (out == nullptr)
            utils::ygglog_throw_error("create_dtype: Failed to malloc for empty dtype_t.");
        out->type = T_NULL;
        out->use_generic = use_generic;
        out->obj = nullptr;
    } catch(...) {
        utils::ygglog_error("create_dtype_empty: C++ exception thrown.");
        return nullptr;
    }
}


/*dtype_t* create_dtype_doc(void* type_doc, const bool use_generic) {
    MetaschemaType* obj = nullptr;
    try {
        obj = (MetaschemaType*)type_from_doc_c(type_doc, use_generic);
        return create_dtype(obj);
    } catch(...) {
        utils::ygglog_error("create_dtype_doc: C++ exception thrown.");
        return nullptr;
    }
}*/


/*dtype_t* create_dtype_python(PyObject* pyobj, const bool use_generic) {
    MetaschemaType* obj = nullptr;
    try {
        obj = type_from_pyobj(pyobj, use_generic);
        return create_dtype(obj);
    } catch(...) {
        utils::ygglog_error("create_dtype_python: C++ exception thrown.");
        return nullptr;
    }
}*/


/*dtype_t* create_dtype_direct(const bool use_generic) {
    DirectMetaschemaType* obj = nullptr;
    try {
        obj = new DirectMetaschemaType(use_generic);
        return create_dtype(obj);
    } catch(...) {
        utils::ygglog_error("create_dtype_direct: C++ exception thrown.");
        CSafe(delete obj);
        return nullptr;
    }
}*/

/*dtype_t* create_dtype_default(const DTYPE type,
                              const bool use_generic) {
    auto dt = (dtype_t*)malloc(sizeof(dtype_t));
    try {

        obj = new MetaschemaType(type, use_generic);
        return create_dtype(obj);
    } catch(...) {
        utils::ygglog_error("create_dtype_default: C++ exception thrown.");
        CSafe(delete obj);
        return nullptr;
    }
}*/



/*dtype_t* create_dtype_ndarray(const char* subtype, const size_t precision,
                              const size_t ndim, const size_t* shape,
                              const char* units, const bool use_generic) {
    NDArrayMetaschemaType* obj = nullptr;
    try {
        std::vector<size_t> shape_vec;
        size_t i;
        for (i = 0; i < ndim; i++) {
            shape_vec.push_back(shape[i]);
        }
        obj = new NDArrayMetaschemaType(subtype, precision, shape_vec, units, use_generic);
        return create_dtype(obj);
    } catch(...) {
        utils::ygglog_error("create_dtype_ndarray: C++ exception thrown.");
        CSafe(delete obj);
        return nullptr;
    }
}*/


/*dtype_t* create_dtype_ndarray_arr(const char* subtype, const size_t precision,
                                  const size_t ndim, const int64_t shape[],
                                  const char* units, const bool use_generic) {
    size_t *shape_ptr = (size_t*)malloc(ndim*sizeof(size_t));
    // size_t shape_size_t[ndim];
    size_t i;
    for (i = 0; i < ndim; i++) {
        shape_ptr[i] = (size_t)shape[i];
        // shape_size_t[i] = (size_t)shape[i];
    }
    // size_t* shape_ptr = shape_size_t;
    // const size_t* shape_ptr = shape;
    dtype_t* out = create_dtype_ndarray(subtype, precision, ndim, shape_ptr, units, use_generic);
    free(shape_ptr);
    return out;
}*/


/*dtype_t* create_dtype_json_array(const size_t nitems, dtype_t** items,
                                 const bool use_generic) {
    JSONArrayMetaschemaType* obj = nullptr;
    try {
        MetaschemaTypeVector items_vec;
        size_t i;
        if ((nitems > 0) && (items == nullptr)) {
            utils::ygglog_throw_error("create_dtype_json_array: %d items expected, but the items parameter is nullptr.", nitems);
        }
        for (i = 0; i < nitems; i++) {
            MetaschemaType* iitem = dtype2class(items[i]);
            items_vec.push_back(iitem);
        }
        obj = new JSONArrayMetaschemaType(items_vec, "", use_generic);
        return create_dtype(obj);
    } catch(...) {
        utils::ygglog_error("create_dtype_json_array: C++ exception thrown.");
        CSafe(delete obj);
        return nullptr;
    }
}*/


/*dtype_t* create_dtype_json_object(const size_t nitems, char** keys,
                                  dtype_t** values, const bool use_generic) {
    JSONObjectMetaschemaType* obj = nullptr;
    try {
        MetaschemaTypeMap properties;
        size_t i;
        if ((nitems > 0) && ((keys == nullptr) || (values == nullptr))) {
            utils::ygglog_throw_error("create_dtype_json_object: %d items expected, but the keys and/or values parameter is nullptr.", nitems);
        }
        for (i = 0; i < nitems; i++) {
            MetaschemaType* iitem = dtype2class(values[i]);
            properties[keys[i]] = iitem;
        }
        obj = new JSONObjectMetaschemaType(properties, use_generic);
        return create_dtype(obj);
    } catch(...) {
        utils::ygglog_error("create_dtype_json_object: C++ exception thrown.");
        CSafe(delete obj);
        return nullptr;
    }
}*/

/*dtype_t* create_dtype_ply(const bool use_generic) {
    PlyMetaschemaType* obj = nullptr;
    try {
        obj = new PlyMetaschemaType(use_generic);
        return create_dtype(obj);
    } catch(...) {
        utils::ygglog_error("create_dtype_ply: C++ exception thrown.");
        CSafe(delete obj);
        return nullptr;
    }
}*/


/*dtype_t* create_dtype_obj(const bool use_generic) {
    ObjMetaschemaType* obj = nullptr;
    try {
        obj = new ObjMetaschemaType(use_generic);
        return create_dtype(obj);
    } catch(...) {
        utils::ygglog_error("create_dtype_obj: C++ exception thrown.");
        CSafe(delete obj);
        return nullptr;
    }
}*/


/*dtype_t* create_dtype_pyobj(const char* type, const bool use_generic=false) {
    PyObjMetaschemaType* obj = nullptr;
    try {
        obj = new PyObjMetaschemaType(type, use_generic);
        return create_dtype(obj);
    } catch(...) {
        utils::ygglog_error("create_dtype_pyobj: C++ exception thrown.");
        CSafe(delete obj);
        return nullptr;
    }
}*/
/*dtype_t* create_dtype_pyinst(const char* class_name,
                             const dtype_t* args_dtype,
                             const dtype_t* kwargs_dtype,
                             const bool use_generic=true) {
    PyInstMetaschemaType* obj = nullptr;
    JSONArrayMetaschemaType* args_type = nullptr;
    JSONObjectMetaschemaType* kwargs_type = nullptr;
    try {
        if (args_dtype != nullptr) {
            args_type = dynamic_cast<JSONArrayMetaschemaType*>(dtype2class(args_dtype));
        }
        if (kwargs_dtype != nullptr) {
            kwargs_type = dynamic_cast<JSONObjectMetaschemaType*>(dtype2class(kwargs_dtype));
        }
        obj = new PyInstMetaschemaType(class_name, args_type, kwargs_type, use_generic);
        return create_dtype(obj);
    } catch(...) {
        utils::ygglog_error("create_dtype_pyinst: C++ exception thrown.");
        CSafe(delete obj);
        return nullptr;
    }
}*/
/*dtype_t* create_dtype_schema(const bool use_generic=true) {
    SchemaMetaschemaType* obj = nullptr;
    try {
        obj = new SchemaMetaschemaType(use_generic);
        return create_dtype(obj);
    } catch(...) {
        utils::ygglog_error("create_dtype_schema: C++ exception thrown.");
        CSafe(delete obj);
        return nullptr;
    }
}*/
/*dtype_t* create_dtype_any(const bool use_generic=true) {
    AnyMetaschemaType* obj = nullptr;
    try {
        obj = new AnyMetaschemaType(use_generic);
        return create_dtype(obj);
    } catch(...) {
        utils::ygglog_error("create_dtype_any: C++ exception thrown.");
        CSafe(delete obj);
        return nullptr;
    }
}*/


}