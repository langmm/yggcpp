#pragma once

#include "MetaschemaType.hpp"
#include "utils/tools.hpp"

namespace communication {
namespace datatypes {
namespace Metaschema {

class JSONArrayMetaschemaType : public MetaschemaType {
public:
    JSONArrayMetaschemaType() = delete;

    /*!
      @brief Constructor for JSONArrayMetaschemaType.
      @param[in] items MetaschemaTypeVector Type classes for array items.
      @param[in] format_str const char * (optional) Format string describing the
      item types. Defaults to empty string.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
    */
    JSONArrayMetaschemaType(const MetaschemaTypeVector items,
                            const char *format_str = "",
                            const bool use_generic = true) :
            MetaschemaType("array", use_generic) {
        if ((items.size() == 0) && (strlen(format_str) == 0)) {
            update_use_generic(true);
        }
        strncpy(format_str_, format_str, 1000);
        strncpy(item_key_, "items", 100);
        update_items(items, true);
    }

    /*!
      @brief Constructor for JSONArrayMetaschemaType from a JSON type defintion.
      @param[in] type_doc rapidjson::Value rapidjson object containing
      the type definition from a JSON encoded header.
      @param[in] format_str const char * (optional) Format string describing the
      item types. Defaults to empty string.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
      @param[in] item_key const char Key to use for items. Defaults to "items".
     */
    JSONArrayMetaschemaType(const rapidjson::Value &type_doc,
                            const char *format_str = "",
                            const bool use_generic = true,
                            const char item_key[100] = "items");
    /*!
      @brief Constructor for JSONArrayMetaschemaType from Python dictionary.
      @param[in] pyobj PyObject* Python object.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
      @param[in] item_key const char Key to use for items. Defaults to "items".
     */
    JSONArrayMetaschemaType(PyObject *pyobj, const bool use_generic = true,
                            const char item_key[100] = "items");

    /*!
      @brief Copy constructor.
      @param[in] other JSONArrayMetaschemaType* Instance to copy.
     */
    JSONArrayMetaschemaType(const JSONArrayMetaschemaType &other) :
            JSONArrayMetaschemaType(other.items(), other.format_str(),
                                    other.use_generic()) {}

    /*!
      @brief Destructor for JSONArrayMetaschemaType.
      Free the type string malloc'd during constructor.
     */
    virtual ~JSONArrayMetaschemaType() {
        free_items();
    }

    /*!
      @brief Free the items.
     */
    void free_items() {
        size_t i;
        for (i = 0; i < items_.size(); i++) {
            delete items_[i];
            items_[i] = NULL;
        }
        items_.clear();
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
      @returns pointer to new JSONArrayMetaschemaType instance with the same data.
     */
    JSONArrayMetaschemaType *copy() const override {
        return (new JSONArrayMetaschemaType(items_, format_str_, use_generic()));
    }

    /*!
      @brief Print information about the type to stdout.
      @param[in] indent char* Indentation to add to display output.
    */
    void display(const char *indent = "") const override;

    /*!
      @brief Get type information as a Python dictionary.
      @returns PyObject* Python dictionary.
     */
    PyObject *as_python_dict() const override;

    /*!
      @brief Copy data wrapped in YggGeneric class.
      @param[in] data YggGeneric* Pointer to generic object.
      @param[in] orig_data Pointer to data that should be copied if different
      that the data that is wrapped.
      @returns void* Pointer to copy of data.
     */
    Dict *copy_generic(const YggGeneric *data, Dict *orig_data = nullptr) const override;

    /*!
      @brief Free data wrapped in YggGeneric class.
      @param[in] data YggGeneric* Pointer to generic object.
     */
    void free_generic(YggGeneric *data) const override;

    /*!
      @brief Display data.
      @param[in] data YggGeneric* Pointer to generic object.
      @param[in] indent char* Indentation to add to display output.
     */
    void display_generic(const YggGeneric *data, const char *indent) const override;

    /*!
      @brief Get number of items in type.
      @returns size_t Number of items in type.
     */
    size_t nitems() const { return items_.size(); }

    /*!
      @brief Get types for items.
      @returns MetaschemaTypeVector Array item types.
     */
    MetaschemaTypeVector items() const { return items_; }

    /*!
      @brief Get format string.
      @returns char* Format string.
     */
    const char *format_str() const { return format_str_; }

    /*!
      @brief Determine if the items are all arrays.
      @returns bool true if all items are arrays, false otherwise.
     */
    bool all_arrays() const;

    /*!
      @brief Update the type object with info from another type object.
      @param[in] new_info MetaschemaType* type object.
     */
    void update(const MetaschemaType *new_info) override {
        if ((strcmp(type(), new_info->type()) != 0) && (items_.size() == 1) &&
            (strcmp(items_[0]->type(), new_info->type()) == 0)) {
            items_[0]->update(new_info);
        } else {
            MetaschemaType::update(new_info);
            JSONArrayMetaschemaType *new_info_array = (JSONArrayMetaschemaType *) new_info;
            update_items(new_info_array->items());
        }
    }

    /*!
      @brief Update the type at an index.
      @param[in] i size_t Index where item type should be added.
      @param[in] x MetaschemaType* Type to insert at index i.
    */
    void update_type_element(size_t i, const MetaschemaType *x) override;

    /*!
      @brief Update the item types.
      @param[in] new_items MetaschemaTypeVector Vector of new types describing items.
      @param[in] force bool If true, the existing items are overwritten, otherwise they are only updated.
     */
    void update_items(const MetaschemaTypeVector new_items,
                      bool force = false);
    /*!
      @brief Update the instance's use_generic flag.
      @param[in] new_use_generic const bool New flag value.
     */
    void update_use_generic(const bool new_use_generic) override;

    /*!
      @brief Get the type associated with an item.
      @param[in] index const size_t Index of item to get type for.
      @returns MetaschemaType* Pointer to type class for item.
     */
    const MetaschemaType *get_item_type(const size_t index) const override {
        MetaschemaType *out = NULL;
        if (index >= items_.size()) {
            utils::ygglog_throw_error(
                    "JSONArrayMetaschemaType::get_item_type: There are %lu items, but item %lu was requested.",
                    items_.size(), index);
        }
        out = items_[index];
        return out;
    }

    /*!
      @brief Set the type associated with an item.
      An error will be raised if the property identified by key is
      already present and the provided type does not match the existing
      type.
      @param[in] index const size_t Index of the item to set the type for.
      @param[in] itemtype const MetaschemaType* Pointer to item type that
      should be associated with the provided index.
     */
    void set_item_type(const size_t index, const MetaschemaType *itemtype) override;

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
        return sizeof(YggGenericVector);
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
        size_t i;
        for (i = 0; i < items_.size(); i++) {
            items_[i]->skip_va_elements_wrap(nargs, ap);
        }
    }

    /*!
      @brief Convert a Python representation to a C representation.
      @param[in] pyobj PyObject* Pointer to Python object.
      @returns YggGeneric* Pointer to C object.
     */
    YggGeneric *python2c(PyObject *pyobj) const override;

    /*!
      @brief Convert a C representation to a Python representation.
      @param[in] cobj YggGeneric* Pointer to C object.
      @returns PyObject* Pointer to Python object.
     */
    PyObject *c2python(YggGeneric *cobj) const override;

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
      @param[in] x YggGeneric* Pointer to generic wrapper for data.
      @returns bool true if the encoding was successful, false otherwise.
     */
    bool encode_data(rapidjson::Writer<rapidjson::StringBuffer> *writer,
                     YggGeneric *x) const override;

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
    bool decode_data(rapidjson::Value &data, YggGeneric *x) override;

private:
    char item_key_[100];
    MetaschemaTypeVector items_;
    char format_str_[1001];
};
}
}
}
