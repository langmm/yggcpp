#pragma once
#include "MetaschemaType.hpp"
#include "PyObjMetaschemaType.hpp"
#include "JSONArrayMetaschemaType.hpp"
#include "JSONObjectMetaschemaType.hpp"

namespace communication {
namespace datatypes {
namespace Metaschema {

class PyInstMetaschemaType : public MetaschemaType {
public:
    PyInstMetaschemaType() = delete;
    /*!
      @brief Constructor for PyInstMetaschemaType.
      @param[in] class_name char* Name of Python class.
      @param[in] args_type JSONArrayMetaschemaType Type definition for instance arguments.
      @param[in] kwargs_type JSONObjectMetaschemaType Type definition for instance arguments.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
     */
    PyInstMetaschemaType(const char* class_name,
                         const JSONArrayMetaschemaType* args_type,
                         const JSONObjectMetaschemaType* kwargs_type,
                         const bool use_generic=true);

    /*!
      @brief Constructor for PyInstMetaschemaType from a JSON type defintion.
      @param[in] type_doc rapidjson::Value rapidjson object containing the type
      definition from a JSON encoded header.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
     */
    PyInstMetaschemaType(const rapidjson::Value &type_doc,
                         const bool use_generic=false);

    /*!
      @brief Constructor for PyInstMetaschemaType from Python dictionary.
      @param[in] pyobj PyObject* Python object.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
     */
    PyInstMetaschemaType(PyObject* pyobj, const bool use_generic=false);

    /*!
      @brief Copy constructor.
      @param[in] other PyInstMetaschemaType* Instance to copy.
     */
    PyInstMetaschemaType(const PyInstMetaschemaType &other) :
            PyInstMetaschemaType(other.class_name(), other.args_type(),
                                 other.kwargs_type(), other.use_generic()) {}
    /*!
      @brief Destructor for PyInstMetaschemaType.
      Free the type string malloc'd during constructor.
     */
    virtual ~PyInstMetaschemaType() {
        class_name_[0] = '\0';
        delete args_type_;
        args_type_ = NULL;
        delete kwargs_type_;
        kwargs_type_ = NULL;
    }
    /*!
      @brief Equivalence operator.
      @param[in] Ref MetaschemaType instance to compare against.
      @returns bool true if the instance is equivalent, false otherwise.
     */
    bool operator==(const MetaschemaType &Ref) const override;

    /*!
       @brief Create a copy of the type.
       @returns pointer to new MetaschemaType instance with the same data.
      */
    PyInstMetaschemaType* copy() const override {
        return (new PyInstMetaschemaType(class_name_, args_type_,
                                         kwargs_type_, use_generic()));
    }
    /*!
      @brief Print information about the type to stdout.
      @param[in] indent char* Indentation to add to display output.
    */
    void display(const char* indent="") const override;

    /*!
      @brief Get type information as a Python dictionary.
      @returns PyObject* Python dictionary.
     */
    PyObject* as_python_dict() const override;

    /*!
       @brief Get the class name string.
       @returns const char Pointer to the class name string.
      */
    const char* class_name() const { return class_name_; }
    /*!
      @brief Get the argument type.
      @returns JSONArrayMetaschemaType* Arguments type.
     */
    const JSONArrayMetaschemaType* args_type() const { return args_type_; }
    /*!
      @brief Get the keyword argument type.
      @returns JSONObjectMetaschemaType* Keyword arguments type.
     */
    const JSONObjectMetaschemaType* kwargs_type() const { return kwargs_type_; }
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
    void skip_va_elements_core(size_t *nargs, va_list_t *ap) const override {
        va_arg(ap->va, python_t);
        (*nargs)--;
    }
    /*!
      @brief Update the type object with info from another type object.
      @param[in] new_info MetaschemaType* type object.
     */
    void update(const MetaschemaType* new_info) override;

    /*!
      @brief Update the instance's class name.
      @param[in] new_class_name const char * String for new class name.
      @param[in] force bool If true, the class name will be updated reguardless of if it is compatible or not. Defaults to false.
     */
    void update_class_name(const char* new_class_name, bool force=false);

    /*!
      @brief update the instance's args type.
      @param[in] new_args_type JSONArrayMetaschemaType* New args type.
      @param[in] force bool If true, the args type will be updated reguardless of if it is compatible or not. Defaults to false.
     */
    void update_args_type(const JSONArrayMetaschemaType* new_args_type,
                          bool force=false);

    /*!
      @brief update the instance's kwargs type.
      @param[in] new_kwargs_type JSONObjectMetaschemaType* New kwargs type.
      @param[in] force bool If true, the kwargs type will be updated reguardless of if it is compatible or not. Defaults to false.
     */
    void update_kwargs_type(const JSONObjectMetaschemaType* new_kwargs_type,
                            bool force=false);

    /*!
      @brief Update the instance's use_generic flag.
      @param[in] new_use_generic const bool New flag value.
     */
    void update_use_generic(const bool new_use_generic) override {
        MetaschemaType::update_use_generic(new_use_generic);
        // Force children to follow parent use_generic
        if (args_type_ != NULL)
            args_type_->update_use_generic(use_generic());
        if (kwargs_type_ != NULL)
            kwargs_type_->update_use_generic(use_generic());
    }
    /*!
      @brief Update the type object with info from provided variable arguments for serialization.
      @param[in,out] nargs size_t Number of arguments contained in ap. On output
      the number of unused arguments will be assigned to this address.
      @param[in] ap va_list_t Variable argument list.
      @returns size_t Number of arguments in ap consumed.
     */
    size_t update_from_serialization_args(size_t *nargs, va_list_t &ap) override;

    /*!
      @brief Convert a Python representation to a C representation.
      @param[in] pyobj PyObject* Pointer to Python object.
      @returns YggGeneric* Pointer to C object.
     */
    YggGeneric* python2c(PyObject* pyobj) const override;

    // Encoding
    /*!
      @brief Encode the type's properties in a JSON string.
      @param[in] writer rapidjson::Writer<rapidjson::StringBuffer> rapidjson writer.
      @returns bool true if the encoding was successful, false otherwise.
     */
    bool encode_type_prop(rapidjson::Writer<rapidjson::StringBuffer> *writer) const override;

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
                     size_t *nargs, va_list_t &ap) const override;

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
                     size_t *nargs, va_list_t &ap) const override;

private:
    char class_name_[PYTHON_NAME_SIZE];
    JSONArrayMetaschemaType *args_type_;
    JSONObjectMetaschemaType *kwargs_type_;


};
}
}
}
