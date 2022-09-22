#pragma once
#include <string>
#include <vector>
#include <map>
#include "rapidjson/document.h"
#include "rapidjson/writer.h"
#include <Python.h>
#include "utils/tools.hpp"
#include "datatypes/datatypes.hpp"
#include "YggGeneric.hpp"
#include "Dict.hpp"

namespace communication {
namespace datatypes {
namespace Metaschema {

class MetaschemaType {
public:
    MetaschemaType() = delete;
    /*!
      @brief Constructor for MetaschemaType.
      @param[in] type const character pointer to the name of the type.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
      @param[in] always_generic bool If true, the datatype will always be
      assumed to expect YggGeneric instances.
     */
    MetaschemaType(const char *type, const bool use_generic = false,
                   const bool always_generic = false);

    /*!
      @brief Constructor for MetaschemaType from a JSON type defintion.
      @param[in] type_doc rapidjson::Value rapidjson object containing
      the type definition from a JSON encoded header.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
      @param[in] always_generic bool If true, the datatype will always be
      assumed to expect YggGeneric instances.
     */
    MetaschemaType(const rapidjson::Value &type_doc,
                   const bool use_generic = false,
                   const bool always_generic = false);

    /*!
      @brief Constructor for MetaschemaType from Python dictionary.
      @param[in] pyobj PyObject* Python object.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
      @param[in] always_generic bool If true, the datatype will always be
      assumed to expect YggGeneric instances.
     */
    MetaschemaType(PyObject *pyobj, const bool use_generic = false,
                   const bool always_generic = false);

    /*!
      @brief Copy constructor.
      @param[in] other MetaschemaType* Instance to copy.
     */
    MetaschemaType(const MetaschemaType &other) :
            MetaschemaType(other.type(), other.use_generic()) {}

    /*!
      @brief Destructor for MetaschemaType.
      Free the type string malloc'd during constructor.
     */
    virtual ~MetaschemaType() {
        free((char *) type_);
    }

    /*!
      @brief Equivalence operator.
      @param[in] Ref MetaschemaType instance to compare against.
      @returns bool true if the instance is equivalent, false otherwise.
     */
    virtual bool operator==(const MetaschemaType &Ref) const;

    /*!
      @brief Inequivalence operator.
      @param[in] Ref MetaschemaType instance to compare against.
      @returns bool true if the instances are not equivalent, false otherwise.
     */
    virtual bool operator!=(const MetaschemaType &Ref) const;

    /*!
      @brief Determine if the datatype is effectively empty.
      @returns bool true if the datatype is empty, false otherwise.
     */
    virtual bool is_empty() const {
        return false;
    }

    /*!
      @brief Create a copy of the type.
      @returns pointer to new MetaschemaType instance with the same data.
     */
    virtual MetaschemaType *copy() const {
        return (new MetaschemaType(type_, use_generic_));
    }

    /*!
      @brief Print information about the type to stdout.
      @param[in] indent char* Indentation to add to display output.
    */
    virtual void display(const char *indent = "") const {
        printf("%s%-15s = %s\n", indent, "type", type_);
        printf("%s%-15s = %d\n", indent, "type_code", type_code_);
        printf("%s%-15s = %d\n", indent, "use_generic", use_generic_);
    }

    /*!
      @brief Get type information as a Python dictionary.
      @returns PyObject* Python dictionary.
     */
    virtual PyObject *as_python_dict() const {
        PyObject *out = PyDict_New();
        set_item_python_dict_c(out, "type", type_,
                               "MetaschemaType::as_python_dict: type: ",
                               T_STRING, STRBUFF);
        return out;
    }

    /*!
      @brief Copy data wrapped in YggGeneric class.
      @param[in] data YggGeneric* Pointer to generic object.
      @param[in] orig_data Pointer to data that should be copied if different
      that the data that is wrapped.
      @returns Dict* Pointer to copy of data.
     */
    virtual Dict *copy_generic(const YggGeneric *data, Dict *orig_data = nullptr) const;

    /*!
      @brief Free data wrapped in YggGeneric class.
      @param[in] data YggGeneric* Pointer to generic object.
     */
    virtual void free_generic(YggGeneric *data) const;

    /*!
      @brief Display data.
      @param[in] data YggGeneric* Pointer to generic object.
      @param[in] indent char* Indentation to add to display output.
     */
    virtual void display_generic(const YggGeneric *data, const char *indent = "") const;

    /*!
      @brief Check that the type is correct and get the corresponding code.
      @returns int Type code for the instance's type.
     */
    int check_type() const;

    /*!
      @brief Get the type string.
      @returns const char pointer to the type string.
     */
    const char *type() const { return type_; }

    /*!
      @brief Get the type code.
      @returns int Type code associated with the curent type.
     */
    const int type_code() const { return type_code_; }

    /*!
      @brief Get the value of class attribute use_generic.
      @returns bool Value of use_generic.
     */
    const bool use_generic() const { return use_generic_; }

    /*!
      @brief Update the type object with info from another type object.
      @param[in] new_info MetaschemaType* type object.
     */
    virtual void update(const MetaschemaType *new_info);

    /*!
      @brief Update the type object with info from provided variable arguments for serialization.
      @param[in,out] nargs size_t Number of arguments contained in ap. On output
      the number of unused arguments will be assigned to this address.
      @param[in] ap va_list_t Variable argument list.
      @returns size_t Number of arguments in ap consumed.
     */
    virtual size_t update_from_serialization_args(size_t *nargs, communication::utils::va_list_t &ap);

    /*!
      @brief Update the type object with info from provided variable arguments for deserialization.
      @param[in,out] nargs size_t Number of arguments contained in ap. On output
      the number of unused arguments will be assigned to this address.
      @param[in] ap va_list_t Variable argument list.
      @returns size_t Number of arguments in ap consumed.
     */
    virtual size_t update_from_deserialization_args(size_t *nargs, communication::utils::va_list_t &ap);

    /*!
      @brief Update the type object with info from provided variable arguments for serialization.
      @param[in] x YggGeneric* Pointer to generic object containing data to be serialized.
     */
    virtual void update_from_serialization_args(YggGeneric *x) {
        update(x->get_type());
    }

    /*!
      @brief Update the type object with info from provided variable arguments for deserialization.
      @param[in,out] x YggGeneric* Pointer to generic object where data will be stored.
     */
    virtual void update_from_deserialization_args(YggGeneric *x) {
        x->get_type()->update(this);
    }

    /*!
      @brief Update the instance's type.
      @param[in] new_type const char * String for new type.
     */
    virtual void update_type(const char *new_type) {
        char **type_modifier = const_cast<char **>(&type_);
        strncpy(*type_modifier, new_type, STRBUFF);
        int *type_code_modifier = const_cast<int *>(&type_code_);
        *type_code_modifier = check_type();
    }

    /*!
      @brief Dummy stand-in to allow access to array/object methods.
      @param[in] i size_t Index where item type should be added.
      @param[in] x MetaschemaType* Type to insert at index i.
    */
    virtual void update_type_element(size_t i, const MetaschemaType *x) {
        UNUSED(i);
        UNUSED(x);
        communication::utils::ygglog_throw_error("MetaschemaType::update_type_element: This method is only valid for array types.");
    }

    /*!
      @brief Dummy stand-in to allow access to array/object methods.
      @param[in] k const char* Key where item type should be added.
      @param[in] x MetaschemaType* Type to insert at key k.
    */
    virtual void update_type_element(const char *k, const MetaschemaType *x) {
        // Prevent C4100 warning on windows by referencing param
        UNUSED(k);
        UNUSED(x);
        communication::utils::ygglog_throw_error("MetaschemaType::update_type_element: This method is only valid for object types.");
    }

    /*!
      @brief Update the instance's use_generic flag.
      @param[in] new_use_generic const bool New flag value.
     */
    virtual void update_use_generic(const bool new_use_generic) {
        bool *use_generic_modifier = const_cast<bool *>(&use_generic_);
        if (always_generic_)
            *use_generic_modifier = true;
        else
            *use_generic_modifier = new_use_generic;
    }

    /*!
      @brief Set the type length.
      @param[in] force bool True if the length should be updated even if it
      is not compatible with the existing value.
      @param[in] new_length size_t New length.
     */
    virtual void set_length(size_t new_length, bool force = false) {
        // This virtual class is required to allow setting lengths
        // for table style type where data is an array of 1darrays.
        // Otherwise circular include results as scalar requires
        // JSON array for checking if there is a single element.
        // Prevent C4100 warning on windows by referencing param
        UNUSED(new_length);
        UNUSED(force);
        communication::utils::ygglog_throw_error("MetaschemaType::set_length: Cannot set length for type '%s'.", type_);
    }

    /*!
      @brief Get the type associated with an item.
      @param[in] index const size_t Index of item to get type for.
      @returns MetaschemaType* Pointer to type class for item.
     */
    virtual const MetaschemaType *get_item_type(const size_t index) const {
        MetaschemaType *out = NULL;
        UNUSED(index);
        communication::utils::ygglog_throw_error("MetaschemaType::get_item_type: Cannot get item type for type '%s'.", type_);
        return out;
    }

    /*!
      @brief Set the type associated with an item.
      @param[in] index const size_t Index of the item to set the type for.
      @param[in] itemtype const MetaschemaType* Pointer to item type that
      should be associated with the provided index.
     */
    virtual void set_item_type(const size_t index, const MetaschemaType *itemtype) {
        UNUSED(index);
        UNUSED(itemtype);
        communication::utils::ygglog_throw_error("MetaschemaType::set_item_type: Cannot set item type for type '%s'.", type_);
    }

    /*!
      @brief Get the type associated with a property.
      @param[in] key const char* Property key to get type for.
      @returns MetaschemaType* Pointer to type class for property.
     */
    virtual const MetaschemaType *get_property_type(const char *key) const {
        MetaschemaType *out = NULL;
        UNUSED(key);
        communication::utils::ygglog_throw_error("MetaschemaType::get_property_type: Cannot get property type for type '%s'.", type_);
        return out;
    }

    /*!
      @brief Set the type associated with a property.
      @param[in] key const char* Property key to set type for.
      @param[in] proptype const MetaschemaType* Pointer to property type
      that should be associated with the provided key.
     */
    virtual void set_property_type(const char *key, const MetaschemaType *proptype) {
        UNUSED(key);
        UNUSED(proptype);
        communication::utils::ygglog_throw_error("MetaschemaType::set_property_type: Cannot set property type for type '%s'.", type_);
    }

    /*!
      @brief Set the _variable_length private variable.
      @param[in] new_variable_length bool New value.
     */
    virtual void set_variable_length(bool new_variable_length) {
#ifdef _WIN32
        UNUSED(new_variable_length);
#endif
        communication::utils::ygglog_throw_error("MetaschemaType::set_variable_length: Cannot set variable_length for type '%s'.", type_);
    }

    /*!
      @brief Set the _in_table private variable.
      @param[in] new_in_table bool New value.
     */
    virtual void set_in_table(bool new_in_table) {
#ifdef _WIN32
        UNUSED(new_in_table);
#endif
        communication::utils::ygglog_throw_error("MetaschemaType::set_in_table: Cannot set in_table for type '%s'.", type_);
    }

    /*!
      @brief Get the number of elements in the type.
      @returns size_t Number of elements (1 for scalar).
     */
    virtual const size_t nelements() const { return 1; }

    /*!
      @brief Determine if the number of elements is variable.
      @returns bool true if the number of elements can change, false otherwise.
    */
    virtual const bool variable_nelements() const { return false; }

    /*!
      @brief Get the item size.
      @returns size_t Size of item in bytes.
     */
    virtual const size_t nbytes() const;

    /*!
      @brief Get the number of bytes occupied by a variable of the type in a variable argument list.
      @returns std::vector<size_t> Number of bytes/variables occupied by the type.
     */
    virtual std::vector<size_t> nbytes_va_core() const;

    /*!
      @brief Get the number of bytes occupied by a variable of the type in a variable argument list.
      @returns std::vector<size_t> Number of bytes/variables occupied by the type.
     */
    std::vector<size_t> nbytes_va() const {
        std::vector<size_t> out = nbytes_va_core();
        out.insert(out.begin(), skip_before_.begin(), skip_before_.end());
        out.insert(out.end(), skip_after_.begin(), skip_after_.end());
        return out;
    }

    /*!
      @brief Skip arguments that make of this type.
      @param[in, out] nargs Pointer to number of arguments in ap.
      @param[in, out] ap va_list_t Variable argument list.
     */
    void skip_va_elements(size_t *nargs, communication::utils::va_list_t *ap) const {
        skip_va_elements_wrap(nargs, ap);
        // size_t i;
        // std::vector<size_t> skip_bytes = nbytes_va();
        // for (i = 0; i < skip_bytes.size(); i++) {
        //   printf("Skip %d bytes (sizeof(void*) = %d)\n", (int)(skip_bytes[i]),
        // 	     (int)(sizeof(void*)));
        //   va_list_t_skip(ap, sizeof(void*));  // skip_bytes[i]);
        // }
    }

    /*!
      @brief Skip arguments that make of this type.
      @param[in, out] nargs Pointer to number of arguments in ap.
      @param[in, out] ap va_list_t Variable argument list.
     */
    virtual void skip_va_elements_core(size_t *nargs, communication::utils::va_list_t *ap) const;

    /*!
      @brief Skip arguments that make of this type.
      @param[in, out] nargs Pointer to number of arguments in ap.
      @param[in, out] ap va_list_t Variable argument list.
     */
    void skip_va_elements_wrap(size_t *nargs, communication::utils::va_list_t *ap) const;

    /*!
      @brief Get the number of arguments expected to be filled/used by the type.
      @returns size_t Number of arguments.
     */
    virtual size_t nargs_exp() const;

    /*!
      @brief Convert a Python representation to a C representation.
      @param[in] pyobj PyObject* Pointer to Python object.
      @returns YggGeneric* Pointer to C object.
     */
    virtual YggGeneric *python2c(PyObject *pyobj) const;

    /*!
      @brief Convert a C representation to a Python representation.
      @param[in] cobj YggGeneric* Pointer to C object.
      @returns PyObject* Pointer to Python object.
     */
    virtual PyObject *c2python(YggGeneric *cobj) const;

    /*!
      @brief Return the recovered generic structure if one is present in
      the variable argument list by removing it.
      @param[in] nargs size_t* Pointer to number of arguments present in ap
      that will be decremented by 1.
      @param[in] ap va_list_t Variable argument list.
      @param[in] skip_nargs_dec bool If true, nargs will be advaced in order
      to skip the element defining the number of arguments.
      @returns YggGeneric Generic structure if one is present.
    */
    YggGeneric pop_generic(size_t *nargs, communication::utils::va_list_t &ap, bool skip_nargs_dec = false) const {
        if (skip_nargs_dec)
            (*nargs)++;
        YggGeneric gen_arg = pop_generic_va(nargs, &ap);
        if (!(is_generic_init(gen_arg))) {
            communication::utils::ygglog_throw_error("MetaschemaType::pop_generic: Type expects generic object, but one was not provided.");
        }
        return gen_arg;
    }

    /*!
      @brief Return the recovered generic structure if one is present in
      the variable argument list by removing it.
      @param[in] nargs size_t* Pointer to number of arguments present in ap
      that will be decremented by 1.
      @param[in] ap va_list_t Variable argument list.
      @param[in] skip_nargs_dec bool If true, nargs will not be modified.
      Defaults to false.
      @returns YggGeneric* Generic structure if one is present, NULL otherwise.
    */
    YggGeneric *pop_generic_ptr(size_t *nargs, communication::utils::va_list_t &ap, bool skip_nargs_dec = false) const;

    // Encoding
    /*!
      @brief Encode the type in a JSON string.
      @param[in] writer rapidjson::Writer<rapidjson::StringBuffer> rapidjson writer.
      @returns bool true if the encoding was successful, false otherwise.
     */
    bool encode_type(rapidjson::Writer<rapidjson::StringBuffer> *writer) const {
        writer->StartObject();
        if (!(encode_type_prop(writer)))
            return false;
        writer->EndObject();
        return true;
    }

    /*!
      @brief Encode the type's properties in a JSON string.
      @param[in] writer rapidjson::Writer<rapidjson::StringBuffer> rapidjson writer.
      @returns bool true if the encoding was successful, false otherwise.
     */
    virtual bool encode_type_prop(rapidjson::Writer<rapidjson::StringBuffer> *writer) const {
        writer->Key("type");
        writer->String(type_, (rapidjson::SizeType) strlen(type_));
        return true;
    }

    /*!
      @brief Encode arguments describine an instance of this type into a JSON string.
      @param[in] writer rapidjson::Writer<rapidjson::StringBuffer> rapidjson writer.
      @param[in,out] nargs size_t * Pointer to the number of arguments contained in
      ap. On return it will be set to the number of arguments used.
      @param[in] ap va_list_t Variable number of arguments that should be encoded
      as a JSON string.
      @returns bool true if the encoding was successful, false otherwise.
     */
    virtual bool encode_data(rapidjson::Writer<rapidjson::StringBuffer> *writer,
                             size_t *nargs, communication::utils::va_list_t &ap) const;

    /*!
      @brief Encode arguments describine an instance of this type into a JSON string.
      @param[in] writer rapidjson::Writer<rapidjson::StringBuffer> rapidjson writer.
      @param[in,out] nargs size_t * Pointer to the number of arguments contained in
      ap. On return it will be set to the number of arguments used.
      @param[in] ... Variable number of arguments that should be encoded
      as a JSON string.
      @returns bool true if the encoding was successful, false otherwise.
     */
    bool encode_data(rapidjson::Writer<rapidjson::StringBuffer> *writer,
                     size_t *nargs, ...) const {
        communication::utils::va_list_t ap_s = communication::utils::init_va_list();
        va_start(ap_s.va, nargs);
        bool out = encode_data(writer, nargs, ap_s);
        va_end(ap_s.va);
        return out;
    }

    /*!
      @brief Encode arguments describine an instance of this type into a JSON string
      first checking if the arguments should be generic.
      @param[in] writer rapidjson::Writer<rapidjson::StringBuffer> rapidjson writer.
      @param[in,out] nargs size_t * Pointer to the number of arguments contained in
      ap. On return it will be set to the number of arguments used.
      @param[in] ap va_list_t Variable number of arguments that should be encoded
      as a JSON string.
      @returns bool true if the encoding was successful, false otherwise.
     */
    bool encode_data_wrap(rapidjson::Writer<rapidjson::StringBuffer> *writer,
                          size_t *nargs, communication::utils::va_list_t &ap) const;

    /*!
      @brief Encode arguments describine an instance of this type into a JSON string
      first checking if the arguments should be generic.
      @param[in] writer rapidjson::Writer<rapidjson::StringBuffer> rapidjson writer.
      @param[in,out] nargs size_t * Pointer to the number of arguments contained in
      ap. On return it will be set to the number of arguments used.
      @param[in] ... Variable number of arguments that should be encoded
      as a JSON string.
      @returns bool true if the encoding was successful, false otherwise.
     */
    bool encode_data_wrap(rapidjson::Writer<rapidjson::StringBuffer> *writer,
                          size_t *nargs, ...) const {
        communication::utils::va_list_t ap_s = communication::utils::init_va_list();
        va_start(ap_s.va, nargs);
        bool out = encode_data_wrap(writer, nargs, ap_s);
        va_end(ap_s.va);
        return out;
    }

    /*!
      @brief Encode arguments describine an instance of this type into a JSON string.
      @param[in] writer rapidjson::Writer<rapidjson::StringBuffer> rapidjson writer.
      @param[in] x YggGeneric* Pointer to generic wrapper for data.
      @returns bool true if the encoding was successful, false otherwise.
     */
    virtual bool encode_data(rapidjson::Writer<rapidjson::StringBuffer> *writer,
                             YggGeneric *x) const {
        size_t nargs = 1;
        switch (type_code_) {
            case T_BOOLEAN: {
                bool arg = false;
                x->get_data(arg);
                return encode_data(writer, &nargs, arg);
            }
            case T_INTEGER: {
                int arg = 0;
                x->get_data(arg);
                return encode_data(writer, &nargs, arg);
            }
            case T_NULL: {
                void *arg = NULL;
                return encode_data(writer, &nargs, arg);
            }
            case T_NUMBER: {
                double arg = 0.0;
                x->get_data(arg);
                return encode_data(writer, &nargs, arg);
            }
            case T_STRING: {
                nargs = 2;
                char *arg = NULL;
                size_t arg_siz = 0;
                x->get_data_realloc(&arg, &arg_siz);
                bool out = encode_data(writer, &nargs, arg, arg_siz);
                if (arg != NULL) {
                    free(arg);
                    arg = NULL;
                }
                return out;
            }
        }
        communication::utils::ygglog_error("MetaschemaType::encode_data: Cannot encode data of type '%s'.", type_);
        return false;
    }

    /*!
      @brief Copy data from a source buffer to a destination buffer.
      @param[in] src_buf char* Pointer to source buffer.
      @param[in] src_buf_siz size_t Size of src_buf.
      @param[in,out] dst_buf char** Pointer to memory address of destination buffer.
      @param[in,out] dst_buf_siz size_t Reference to size of destination buffer.
      If dst_buf is reallocated, this will be updated with the size of the buffer
      after reallocation.
      @param[in] allow_realloc int If 1, dst_buf can be reallocated if it is
      not large enough to contain the contents of src_buf. If 0, an error will
      be thrown if dst_buf is not large enough.
      @param[in] skip_terminal bool (optional) If true, the terminal character will
      not be added to the end of the copied buffer. Defaults to false.
      @returns int -1 if there is an error, otherwise its the size of the data
      copied to the destination buffer.
     */
    virtual int copy_to_buffer(const char *src_buf, const size_t src_buf_siz,
                               char **dst_buf, size_t &dst_buf_siz,
                               const int allow_realloc, bool skip_terminal = false) const;

    /*!
      @brief Serialize an instance including it's type and data.
      @param[out] buf char ** Buffer where serialized data should be written.
      @param[in,out] buf_siz size_t* Size of buf. If buf is reallocated, the
      new size of the buffer will be assigned to this address.
      @param[in] allow_realloc int If 1, buf will be reallocated if it is not
      large enough to contain the serialized data. If 0, an error will be raised
      if it is not large enough.
      @param[in,out] nargs size_t Number of arguments contained in ap. On output
      the number of arguments used will be assigned to this address.
      @param[in] ap va_list_t Variable number of arguments that will be serialized.
      @returns int Size of the serialized data in buf.
     */
    virtual int serialize(char **buf, size_t *buf_siz,
                          const int allow_realloc, size_t *nargs, communication::utils::va_list_t &ap);

    /*!
      @brief Serialize an instance including it's type and data.
      @param[out] buf char ** Buffer where serialized data should be written.
      @param[in,out] buf_siz size_t* Size of buf. If buf is reallocated, the
      new size of the buffer will be assigned to this address.
      @param[in] allow_realloc int If 1, buf will be reallocated if it is not
      large enough to contain the serialized data. If 0, an error will be raised
      if it is not large enough.
      @param[in] x Pointer to generic wrapper for object being serialized.
      @returns int Size of the serialized data in buf.
     */
    virtual int serialize(char **buf, size_t *buf_siz,
                          const int allow_realloc, YggGeneric *x);

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
    virtual bool decode_data(rapidjson::Value &data, const int allow_realloc,
                             size_t *nargs, communication::utils::va_list_t &ap) const;

    /*!
      @brief Decode variables from a JSON string.
      @param[in] data rapidjson::Value Reference to entry in JSON string.
      @param[in] allow_realloc int If 1, the passed variables will be reallocated
      to contain the deserialized data.
      @param[in,out] nargs size_t Number of arguments contained in ap. On return,
      the number of arguments assigned from the deserialized data will be assigned
      to this address.
      @param[out] ... Variable number of arguments that contain addresses
      where deserialized data should be assigned.
      @returns bool true if the data was successfully decoded, false otherwise.
     */
    bool decode_data(rapidjson::Value &data, const int allow_realloc,
                     size_t *nargs, ...) const {
        communication::utils::va_list_t ap_s = communication::utils::init_va_list();
        va_start(ap_s.va, nargs);
        bool out = decode_data(data, allow_realloc, nargs, ap_s);
        va_end(ap_s.va);
        return out;
    }

    /*!
      @brief Decode variables from a JSON string, first checking if the
      type expects a generic object and extracting it if it does.
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
    bool decode_data_wrap(rapidjson::Value &data, const int allow_realloc,
                          size_t *nargs, communication::utils::va_list_t &ap) const;

    /*!
      @brief Decode variables from a JSON string, first checking if the
      type expects a generic object and extracting it if it does.
      @param[in] data rapidjson::Value Reference to entry in JSON string.
      @param[in] allow_realloc int If 1, the passed variables will be reallocated
      to contain the deserialized data.
      @param[in,out] nargs size_t Number of arguments contained in ap. On return,
      the number of arguments assigned from the deserialized data will be assigned
      to this address.
      @param[out] ... Variable number of arguments that contain addresses
      where deserialized data should be assigned.
      @returns bool true if the data was successfully decoded, false otherwise.
     */
    bool decode_data_wrap(rapidjson::Value &data, const int allow_realloc,
                          size_t *nargs, ...) const {
        communication::utils::va_list_t ap_s = communication::utils::init_va_list();
        va_start(ap_s.va, nargs);
        bool out = decode_data_wrap(data, allow_realloc, nargs, ap_s);
        va_end(ap_s.va);
        return out;
    }

    /*!
      @brief Decode variables from a JSON string.
      @param[in] data rapidjson::Value Reference to entry in JSON string.
      @param[out] x YggGeneric* Pointer to generic object where data should be stored.
      @returns bool true if the data was successfully decoded, false otherwise.
     */
    virtual bool decode_data(rapidjson::Value &data, YggGeneric* x);

    /*!
      @brief Deserialize variables from a JSON string.
      @param[in] buf char* Buffer containing serialized data.
      @param[in] buf_siz size_t Size of the serialized data.
      @param[in] allow_realloc int If 1, the provided variables will be realloced
      as necessary to house the deserialized data.
      @param[in,out] nargs size_t* Number of arguments contained in ap. On
      return, the number of arguments assigned will be assigned to this address.
      @param[out] ap va_list_t Arguments that should be assigned based on the
      deserialized data.
      @returns int -1 if there is an error, otherwise the number of arguments
      used.
     */
    virtual int deserialize(const char *buf, const size_t buf_siz,
                            const int allow_realloc, size_t *nargs, communication::utils::va_list_t &ap);

    /*!
      @brief Deserialize variables from a JSON string.
      @param[in] buf char* Buffer containing serialized data.
      @param[in] buf_siz size_t Size of the serialized data.
      @param[out] x YggGeneric* Pointer to generic type wrapper where
      deserialized data should be stored.
      @returns int -1 if there is an error, 0 otherwise.
     */
    virtual int deserialize(const char *buf, const size_t buf_siz, YggGeneric *x);

#ifndef DOXYGEN_SHOULD_SKIP_THIS
private:
    const char *type_;
    const int type_code_;
protected:
    bool updated_;
private:
    const int nbytes_;
    const bool use_generic_;
protected:
    bool always_generic_;
    std::vector<size_t> skip_before_;
    std::vector<size_t> skip_after_;
#endif // DOXYGEN_SHOULD_SKIP_THIS
};

typedef std::map<std::string, MetaschemaType *> MetaschemaTypeMap;
typedef std::vector<MetaschemaType *> MetaschemaTypeVector;


}
}
}