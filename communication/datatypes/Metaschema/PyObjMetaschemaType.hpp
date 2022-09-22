#pragma once
#include "MetaschemaType.hpp"
#include "utils/tools.hpp"

namespace communication {
namespace datatypes {
namespace Metaschema {


class PyObjMetaschemaType : public MetaschemaType {
public:
    PyObjMetaschemaType() = delete;

    /*!
      @brief Constructor for PyObjMetaschemaType.
      @param[in] type const character pointer to the name of the type.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
     */
    PyObjMetaschemaType(const char* type, const bool use_generic=false) :
            MetaschemaType(type, use_generic) {}
    /*!
      @brief Constructor for PyObjMetaschemaType from a JSON type defintion.
      @param[in] type_doc rapidjson::Value rapidjson object containing the type
      definition from a JSON encoded header.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
     */
    PyObjMetaschemaType(const rapidjson::Value &type_doc,
                        const bool use_generic=false) :
            MetaschemaType(type_doc, use_generic) {}
    /*!
      @brief Constructor for PyObjMetaschemaType from Python dictionary.
      @param[in] pyobj PyObject* Python object.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
     */
    PyObjMetaschemaType(PyObject* pyobj, const bool use_generic=false) :
            MetaschemaType(pyobj, use_generic) {}

    /*!
      @brief Copy constructor.
      @param[in] other PyObjMetaschemaType* Instance to copy.
     */
    PyObjMetaschemaType(const PyObjMetaschemaType &other) :
            PyObjMetaschemaType(other.type(), other.use_generic()) {}

    /*!
      @brief Create a copy of the type.
      @returns pointer to new PyObjMetaschemaType instance with the same data.
     */
    PyObjMetaschemaType* copy() const override { return (new PyObjMetaschemaType(type(), use_generic())); }

    /*!
      @brief Copy data wrapped in YggGeneric class.
      @param[in] data YggGeneric* Pointer to generic object.
      @param[in] orig_data Pointer to data that should be copied if different
      that the data that is wrapped.
      @returns void* Pointer to copy of data.
     */
    Dict* copy_generic(const YggGeneric* data, Dict* orig_data=NULL) const override;

    /*!
      @brief Free data wrapped in YggGeneric class.
      @param[in] data YggGeneric* Pointer to generic object.
     */
    void free_generic(YggGeneric* data) const override;

    /*!
      @brief Display data.
      @param[in] data YggGeneric* Pointer to generic object.
      @param[in] indent char* Indentation to add to display output.
     */
    void display_generic(const YggGeneric* data, const char* indent="") const override {
        UNUSED(indent);
        if (data == NULL) {
            ygglog_throw_error("PyObjMetaschemaType::display_generic: Generic object is NULL.");
        }
        python_t* arg = (python_t*)(data->get_data());
        display_python_t(*arg);
    }
    /*!
      @brief Update the type object with info from provided variable arguments for serialization.
      @param[in,out] nargs size_t Number of arguments contained in ap. On output
      the number of unused arguments will be assigned to this address.
      @param[in] ap va_list_t Variable argument list.
      @returns size_t Number of arguments in ap consumed.
     */
    size_t update_from_serialization_args(size_t *nargs, utils::va_list_t &ap) override;

    /*!
      @brief Get the size of the type in bytes.
      @returns size_t Type size.
     */
    const size_t nbytes() const override {
        return sizeof(python_t);
    }
    /*!
      @brief Get the number of arguments expected to be filled/used by the type.
      @returns size_t Number of arguments.
     */
    virtual size_t nargs_exp() const override {
        return 1;
    }
    /*!
      @brief Skip arguments that make of this type.
      @param[in, out] nargs Pointer to number of arguments in ap.
      @param[in, out] ap va_list_t Variable argument list.
     */
    void skip_va_elements_core(size_t *nargs, utils::va_list_t *ap) const override {
        va_arg(ap->va, python_t);
        (*nargs)--;
    }
    /*!
      @brief Import a Python object (e.g. class or function).
      @param[in] name const char* Name of Python module and class/function.
      @returns PyObject* Python object.
     */
    PyObject* import_python(const char* name) const;

    /*!
      @brief Convert a Python representation to a C representation.
      @param[in] pyobj PyObject* Pointer to Python object.
      @returns YggGeneric* Pointer to C object.
     */
    YggGeneric* python2c(PyObject* pyobj) const override;

    /*!
      @brief Convert a C representation to a Python representation.
      @param[in] cobj YggGeneric* Pointer to C object.
      @returns PyObject* Pointer to Python object.
     */
    PyObject* c2python(YggGeneric *cobj) const override {
        python_t *arg = (python_t*)(cobj->get_data());
        if (arg == NULL) {
            ygglog_throw_error("PyObjMetaschemaType::c2python: Python structure is NULL.");
        }
        PyObject *pyobj = arg->obj;
        return pyobj;
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
    bool encode_data(rapidjson::Writer<rapidjson::StringBuffer> *writer,
                     size_t *nargs, utils::va_list_t &ap) const override;

    /*!
      @brief Encode arguments describine an instance of this type into a JSON string.
      @param[in] writer rapidjson::Writer<rapidjson::StringBuffer> rapidjson writer.
      @param[in] x YggGeneric* Pointer to generic wrapper for data.
      @returns bool true if the encoding was successful, false otherwise.
     */
    bool encode_data(rapidjson::Writer<rapidjson::StringBuffer> *writer,
                     YggGeneric* x) const override {
        size_t nargs = 1;
        python_t arg;
        arg.name[0] = '\0';
        arg.args = NULL;
        arg.kwargs = NULL;
        arg.obj = NULL;
        x->get_data(arg);
        return MetaschemaType::encode_data(writer, &nargs, arg);
    }

    // Decoding
    /*!
      @brief Decode variables from a JSON string.
      @param[in] data rapidjson::Value Reference to entry in JSON string.
      @param[in] allow_realloc int If 1, the passed variables will be reallocated
      to contain the deserialized data.
      @param[in,out] nargs size_t Number of arguments contained in ap. On return,
      the number of arguments assigned from the deserialized data will be assigned
      to this address.
      @param[out] ap va_list_t Reference to variable argument list containing
      address where deserialized data should be assigned.
      @returns bool true if the data was successfully decoded, false otherwise.
     */
    bool decode_data(rapidjson::Value &data, const int allow_realloc,
                     size_t *nargs, utils::va_list_t &ap) const override;
};

}
}
}
