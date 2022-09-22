#include <map>
#include "DataType.hpp"
#include "datatypes.hpp"
#include "utils/tools.hpp"
#include "Metaschema/metaschematypes.hpp"

using namespace communication::datatypes;
using namespace communication::utils;
using namespace communication::datatypes::Metaschema;

DataType::DataType(MetaschemaType* type_class, const bool use_generic) {
    type = "";
    this->use_generic = use_generic;
    obj = nullptr;
    if (type_class != nullptr) {
        try {
            init_dtype_class(type_class);
        } catch (...) {
            ygglog_throw_error("create_dtype: Failed to initialized data type structure with class information.");
        }
    }
}

DataType::DataType(DataType *data_type) {
    type = data_type->type;
    use_generic = data_type->use_generic;
    obj = data_type->obj;
    // TODO
    //if (data_type->obj != nullptr) {
    //    //*obj = *(data_type->obj);
    //} else {
    //    obj = nullptr;
    //}
}
void DataType::init_dtype_class(MetaschemaType* type_class) {
    if (obj != nullptr) {
        ygglog_throw_error("init_dtype_class: Data type class already set.");
    } else if (!type.empty()) {
        ygglog_throw_error("init_dtype_class: Data type string already set.");
    }
    obj = type_class;
    //TODO:
    ///use_generic = type_class->use_generic();
    ///strncpy(dtype->type, type_class->type(), COMMBUFFSIZ);
}

DataType* DataType::complete_dtype(DataType* dtype, const bool use_generic){
    try {
        if (dtype == nullptr) {
            return new DataType(nullptr, use_generic);
        } else if ((dtype->obj != nullptr) && (dtype->type.empty())){
            int ret = set_dtype_name(dtype, dtype_name(dtype));
            if (ret != 0) {
                ygglog_throw_error("complete_dtype: Failed to set data type name.");
            }
        }
    } catch (...) {
        ygglog_error("complete_dtype: C++ exception thrown.");
        return nullptr;
    }
    return dtype;
}

MetaschemaType* DataType::ToClass() const {
    if (obj == nullptr) {
        ygglog_throw_error("dtype2class: C++ data type structure is NULL.");
    }
    std::map<const std::string, int, strcomp> type_map = get_type_map();
    std::map<const std::string, int, strcomp>::iterator it = type_map.find(type);
    if (it != type_map.end()) {
        switch (it->second) {
            case T_BOOLEAN:
            case T_INTEGER:
            case T_NULL:
            case T_NUMBER:
            case T_STRING:
                return static_cast<MetaschemaType*>(dtype->obj);
            case T_ARRAY:
                return static_cast<JSONArrayMetaschemaType*>(dtype->obj);
            case T_OBJECT:
                return static_cast<JSONObjectMetaschemaType*>(dtype->obj);
            case T_DIRECT:
                return static_cast<DirectMetaschemaType*>(dtype->obj);
            case T_1DARRAY:
                return static_cast<OneDArrayMetaschemaType*>(dtype->obj);
            case T_NDARRAY:
                return static_cast<NDArrayMetaschemaType*>(dtype->obj);
            case T_SCALAR:
            case T_FLOAT:
            case T_UINT:
            case T_INT:
            case T_COMPLEX:
            case T_BYTES:
            case T_UNICODE:
                return static_cast<ScalarMetaschemaType*>(dtype->obj);
            case T_PLY:
                return static_cast<PlyMetaschemaType*>(dtype->obj);
            case T_OBJ:
                return static_cast<ObjMetaschemaType*>(dtype->obj);
            case T_ASCII_TABLE:
                return static_cast<AsciiTableMetaschemaType*>(dtype->obj);
            case T_CLASS:
            case T_FUNCTION:
                return static_cast<PyObjMetaschemaType*>(dtype->obj);
            case T_INSTANCE:
                return static_cast<PyInstMetaschemaType*>(dtype->obj);
            case T_SCHEMA:
                return static_cast<SchemaMetaschemaType*>(dtype->obj);
            case T_ANY:
                return static_cast<AnyMetaschemaType*>(dtype->obj);
        }
    } else {
        ygglog_throw_error("dtype2class: No handler for type '%s'.", dtype->type);
    }
    return nullptr;

}

DataType::DataType(const bool use_generic) : DataType(nullptr, use_generic) {
}

DataType::DataType(void* type_doc, const bool use_generic) {
    try {
        MetaschemaType* mobj = type_from_doc_c(type_doc, use_generic);
        *this = DataType(mobj);
    } catch(...) {
        ygglog_error("create_dtype_doc: C++ exception thrown.");
    }
}

DataType::DataType(PyObject* pyobj, const bool use_generic) {
    MetaschemaType* out = NULL;
    try {
        out = type_from_pyobj(pyobj, use_generic);
    } catch(...) {
        ygglog_error("type_from_pyobj_c: C++ exception thrown.");
        if (out != NULL) {
            delete out;
            out = NULL;
        }
    }

}
DataType::DataType(const char* type, const bool use_generic) {
}
DataType* DataType::DataType_direct(const bool use_generic) {
}
DataType::DataType(const char* subtype, const size_t precision, const char* units, const bool use_generic) {
}
DataType::DataType(const char* format_str, const int as_array, const bool use_generic) {
}
DataType::DataType(const char* subtype, const size_t precision, const size_t length, const char* units, const bool use_generic) {
}
DataType::DataType(const char* subtype, const size_t precision, const size_t ndim, const size_t* shape, const char* units, const bool use_generic) {
}
DataType::DataType(const char* subtype, const size_t precision, const size_t ndim, const int64_t shape[], const char* units, const bool use_generic) {
}
DataType::DataType(const size_t nitems, DataType** items, const bool use_generic) {
}
DataType::DataType(const size_t nitems, char** keys, DataType** values, const bool use_generic) {
}
DataType* DataType::DataType_ply(const bool use_generic) {
}
DataType* DataType::DataType_obj(const bool use_generic) {
}
DataType* DataType::DataType_table(const char* format_str, const int as_array, const bool use_generic) {
}
DataType* DataType::DataType_pyobj(const char* type, const bool use_generic) {
}
DataType::DataType(const char* class_name, const DataType* args_dtype, const DataType* kwargs_dtype, const bool use_generic) {
}
DataType* DataType::DataType_schema(const bool use_generic) {
}
DataType* DataType::DataType_any(const bool use_generic) {
}
