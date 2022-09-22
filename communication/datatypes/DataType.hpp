#pragma once

#include <string>

class PyObject;
namespace communication {
namespace datatypes {
namespace Metaschema {
class MetaschemaType;
}

class DataType {
public:
    explicit DataType(Metaschema::MetaschemaType *type_class = nullptr, const bool use_generic = false);

    explicit DataType(DataType *data_type);
    DataType(const bool use_generic=false);
    DataType(void* type_doc, const bool use_generic=false);
    DataType(PyObject* pyobj, const bool use_generic=false);
    DataType(const char* type, const bool use_generic=false);
    static DataType* DataType_direct(const bool use_generic=false);
    DataType(const char* subtype, const size_t precision, const char* units, const bool use_generic=false);
    DataType(const char* format_str, const int as_array=0, const bool use_generic=false);
    DataType(const char* subtype, const size_t precision, const size_t length, const char* units, const bool use_generic=false);
    DataType(const char* subtype, const size_t precision, const size_t ndim, const size_t* shape, const char* units, const bool use_generic=false);
    DataType(const char* subtype, const size_t precision, const size_t ndim, const int64_t shape[], const char* units, const bool use_generic=false);
    DataType(const size_t nitems, DataType** items, const bool use_generic=false);
    DataType(const size_t nitems, char** keys, DataType** values, const bool use_generic=false);
    static DataType* DataType_ply(const bool use_generic=false);
    static DataType* DataType_obj(const bool use_generic=false);
    static DataType* DataType_table(const char* format_str, const int as_array, const bool use_generic=false);
    static DataType* DataType_pyobj(const char* type, const bool use_generic=false);
    DataType(const char* class_name, const DataType* args_dtype, const DataType* kwargs_dtype, const bool use_generic=false);
    static DataType* DataType_schema(const bool use_generic=true);
    static DataType* DataType_any(const bool use_generic=true);



    //explicit DataType(const bool use_generic);
    //DataType(void* type_doc, const bool use_generic);
    //DataType(PyObject* pyobj, const bool use_generic);
    const size_t precision();

    const std::string subtype();

    const std::string name();

    bool empty();

    ~DataType() {
        delete obj;
    }
    static DataType* complete_dtype(DataType* dtype, const bool use_generic=false);
    Metaschema::MetaschemaType* ToClass() const;
protected:
    std::string type; //!< Type name
    bool use_generic; //!< Flag for empty dtypes to specify generic in/out
    Metaschema::MetaschemaType *obj; //!< MetaschemaType Pointer
    void init_dtype_class(Metaschema::MetaschemaType *type_class);
};

}
}