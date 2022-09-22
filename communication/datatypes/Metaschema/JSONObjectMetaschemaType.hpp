#pragma once

#include "MetaschemaType.hpp"
#include "utils/tools.hpp"

namespace communication {
namespace datatypes {
namespace Metaschema {

class JSONObjectMetaschemaType : public MetaschemaType {
public:
    JSONObjectMetaschemaType() = delete;

    /*!
      @brief Constructor for JSONObjectMetaschemaType.
      @param[in] properties MetaschemaTypeMap Map from
      property names to types.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
    */
    JSONObjectMetaschemaType(const MetaschemaTypeMap properties,
                             const bool use_generic=true) :
    // Always generic
            MetaschemaType("object", true) {
        UNUSED(use_generic);
        strncpy(prop_key_, "properties", 100);
        update_properties(properties, true);
    }
    /*!
      @brief Constructor for JSONObjectMetaschemaType from a JSON type defintion.
      @param[in] type_doc rapidjson::Value rapidjson object containing
      the type definition from a JSON encoded header.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
      @param[in] prop_key const char Key to use for properties. Defaults to "properties".
     */
    JSONObjectMetaschemaType(const rapidjson::Value &type_doc,
                             const bool use_generic=true,
                             const char prop_key[100]="properties");

    /*!
      @brief Constructor for JSONObjectMetaschemaType from Python dictionary.
      @param[in] pyobj PyObject* Python object.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
      @param[in] prop_key const char Key to use for properties. Defaults to "properties".
     */
    JSONObjectMetaschemaType(PyObject* pyobj, const bool use_generic=true,
                             const char prop_key[100]="properties");

    /*!
      @brief Copy constructor.
      @param[in] other JSONObjectMetaschemaType* Instance to copy.
     */
    JSONObjectMetaschemaType(const JSONObjectMetaschemaType &other) :
            JSONObjectMetaschemaType(other.properties(),
                                     other.use_generic()) {}
    /*!
      @brief Destructor for JSONObjectMetaschemaType.
      Free the type string malloc'd during constructor.
     */
    virtual ~JSONObjectMetaschemaType() {
        free_properties();
    }
    /*!
      @brief Free properties.
     */
    void free_properties() {
        MetaschemaTypeMap::iterator it;
        for (it = properties_.begin(); it != properties_.end(); it++) {
            delete it->second;
            it->second = nullptr;
        }
        properties_.clear();
    }
    /*!
      @brief Equivalence operator.
      @param[in] Ref MetaschemaType instance to compare against.
      @returns bool true if the instance is equivalent, false otherwise.
     */
    bool operator==(const MetaschemaType &Ref) const override;

    /*!
      @brief Determine if the datatype is effectively empty.
      @returns bool true if the datatype is empty, false otherwise.
     */
    bool is_empty() const override {
        if (nitems() == 0)
            return true;
        return false;
    }
    /*!
      @brief Create a copy of the type.
      @returns pointer to new JSONObjectMetaschemaType instance with the same data.
     */
    JSONObjectMetaschemaType* copy() const override { return (new JSONObjectMetaschemaType(properties_, use_generic())); }
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
      @brief Copy data wrapped in YggGeneric class.
      @param[in] data YggGeneric* Pointer to generic object.
      @param[in] orig_data Pointer to data that should be copied if different
      that the data that is wrapped.
      @returns void* Pointer to copy of data.
     */
    Dict* copy_generic(const YggGeneric* data, Dict* orig_data=nullptr) const override;

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
    void display_generic(const YggGeneric* data, const char* indent) const override;

    /*!
      @brief Get number of items in type.
      @returns size_t Number of items in type.
     */
    size_t nitems() const { return properties_.size(); }
    /*!
      @brief Get types for properties.
      @returns MetaschemaTypeMap Map from property
      names to types.
     */
    MetaschemaTypeMap properties() const { return properties_; }
    /*!
      @brief Update the type object with info from another type object.
      @param[in] new_info MetaschemaType* type object.
     */
    void update(const MetaschemaType* new_info) override {
        MetaschemaType::update(new_info);
        JSONObjectMetaschemaType* new_info_obj = (JSONObjectMetaschemaType*)new_info;
        update_properties(new_info_obj->properties());
    }
    /*!
      @brief Update the type associated with a key.
      @param[in] k const char* Key where item type should be added.
      @param[in] x MetaschemaType* Type to insert at key k.
    */
    void update_type_element(const char* k, const MetaschemaType* x) override;

    /*!
      @brief Update the property types.
      @param[in] new_properties MetaschemaTypeMap Map of new types describing properties.
      @param[in] force bool If true, the existing properties are overwritten, otherwise they are only updated.
     */
    void update_properties(const MetaschemaTypeMap new_properties,
                           bool force=false);

    /*!
      @brief Update the instance's use_generic flag.
      @param[in] new_use_generic const bool New flag value.
     */
    void update_use_generic(const bool new_use_generic) override;

    /*!
      @brief Get the type associated with a property.
      @param[in] key const char* Property key to get type for.
      @returns MetaschemaType* Pointer to type class for property.
     */
    const MetaschemaType* get_property_type(const char* key) const override {
        MetaschemaType* out = NULL;
        MetaschemaTypeMap::const_iterator it = properties_.find(key);
        if (it == properties_.end()) {
            utils::ygglog_throw_error("JSONObjectMetaschemaType::get_property_type: Could not locate property for key '%s'", key);
        }
        out = it->second;
        return out;
    }
    /*!
      @brief Set the type associated with a property.
      An error will be raised if the property identified by key is
      already present and the provided type does not match the existing
      type.
      @param[in] key const char* Property key to set type for.
      @param[in] proptype const MetaschemaType* Pointer to property type
      that should be associated with the provided key.
     */
    void set_property_type(const char* key, const MetaschemaType* proptype) override;

    /*!
      @brief Update the type object with info from provided variable arguments for serialization.
      @param[in,out] nargs size_t Number of arguments contained in ap. On output
      the number of unused arguments will be assigned to this address.
      @param[in] ap va_list_t Variable argument list.
      @returns size_t Number of arguments in ap consumed.
     */
    size_t update_from_serialization_args(size_t *nargs, utils::va_list_t &ap) override;

    /*!
      @brief Update the type object with info from provided variable arguments for deserialization.
      @param[in,out] nargs size_t Number of arguments contained in ap. On output
      the number of unused arguments will be assigned to this address.
      @param[in] ap va_list_t Variable argument list.
      @returns size_t Number of arguments in ap consumed.
     */
    size_t update_from_deserialization_args(size_t *nargs, utils::va_list_t &ap) override;
    /*!
      @brief Get the item size.
      @returns size_t Size of item in bytes.
     */
    const size_t nbytes() const override {
        return sizeof(YggGenericMap);
    }
    /*!
      @brief Get the number of bytes occupied by a variable of the type in a variable argument list.
      @returns std::vector<size_t> Number of bytes/variables occupied by the type.
     */
    std::vector<size_t> nbytes_va_core() const override;

    /*!
      @brief Get the number of arguments expected to be filled/used by the type.
      @returns size_t Number of arguments.
     */
    size_t nargs_exp() const override;

    /*!
      @brief Skip arguments that make of this type.
      @param[in, out] nargs Pointer to number of arguments in ap.
      @param[in, out] ap va_list_t Variable argument list.
     */
    void skip_va_elements_core(size_t *nargs, utils::va_list_t *ap) const override {
        MetaschemaTypeMap::const_iterator it;
        for (it = properties_.begin(); it != properties_.end(); it++) {
            it->second->skip_va_elements_wrap(nargs, ap);
        }
    }
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
    PyObject* c2python(YggGeneric* cobj) const override;

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
                     size_t *nargs, utils::va_list_t &ap) const override;

    /*!
      @brief Encode arguments describine an instance of this type into a JSON string.
      @param[in] writer rapidjson::Writer<rapidjson::StringBuffer> rapidjson writer.
      @param[in] arg YggGenericMap Mapping between keys and generic wrappers.
      @returns bool true if the encoding was successful, false otherwise.
     */
    bool encode_data(rapidjson::Writer<rapidjson::StringBuffer> *writer,
                     YggGenericMap arg) const;

    /*!
      @brief Encode arguments describine an instance of this type into a JSON string.
      @param[in] writer rapidjson::Writer<rapidjson::StringBuffer> rapidjson writer.
      @param[in] x YggGeneric* Pointer to generic wrapper for data.
      @returns bool true if the encoding was successful, false otherwise.
     */
    bool encode_data(rapidjson::Writer<rapidjson::StringBuffer> *writer,
                     YggGeneric* x) const override {
        YggGenericMap arg;
        x->get_data(arg);
        return encode_data(writer, arg);
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

    /*!
      @brief Decode variables from a JSON string.
      @param[in] data rapidjson::Value Reference to entry in JSON string.
      @param[out] x YggGeneric* Pointer to generic object where data should be stored.
      @returns bool true if the data was successfully decoded, false otherwise.
     */
    bool decode_data(rapidjson::Value &data, YggGeneric* x) override;

private:
    char prop_key_[100];
    MetaschemaTypeMap properties_;

};
}
}
}
