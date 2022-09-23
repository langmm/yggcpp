#pragma once

#include "ScalarMetaschemaType.hpp"
#include "utils/tools.hpp"

namespace communication {
namespace datatypes {
namespace Metaschema {

class OneDArrayMetaschemaType : public ScalarMetaschemaType {
public:
    OneDArrayMetaschemaType() = delete;

    /*!
      @brief Constructor for OneDArrayMetaschemaType.
      @param[in] subtype const character pointer to the name of the subtype.
      @param[in] precision size_t Type precision in bits.
      @param[in] length size_t Number of elements in the array.
      @param[in] units const char * (optional) Type units.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
     */
    OneDArrayMetaschemaType(const char *subtype, const size_t precision,
                            const size_t length, const char *units = "",
                            const bool use_generic = false) :
            ScalarMetaschemaType(subtype, precision, units, use_generic), length_(length) {
        update_type("1darray");
        if (length_ == 0)
            _variable_length = true;
        else
            _variable_length = false;
    }

    /*!
      @brief Constructor for OneDArrayMetaschemaType from a JSON type defintion.
      @param[in] type_doc rapidjson::Value rapidjson object containing the type
      definition from a JSON encoded header.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
     */
    OneDArrayMetaschemaType(const rapidjson::Value &type_doc,
                            const bool use_generic = false);

    /*!
      @brief Constructor for OneDArrayMetaschemaType from Python dictionary.
      @param[in] pyobj PyObject* Python object.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
     */
    OneDArrayMetaschemaType(PyObject *pyobj, const bool use_generic = false);

    /*!
      @brief Copy constructor.
      @param[in] other OneDArrayMetaschemaType* Instance to copy.
     */
    OneDArrayMetaschemaType(const OneDArrayMetaschemaType &other) :
            OneDArrayMetaschemaType(other.subtype(), other.precision(),
                                    other.length(), other.units(),
                                    other.use_generic()) {}

    virtual ~OneDArrayMetaschemaType() {}

    /*!
      @brief Equivalence operator.
      @param[in] Ref MetaschemaType instance to compare against.
      @returns bool true if the instance is equivalent, false otherwise.
     */
    bool operator==(const MetaschemaType &Ref) const override;

    /*!
      @brief Create a copy of the type.
      @returns pointer to new OneDArrayMetaschemaType instance with the same data.
     */
    OneDArrayMetaschemaType *copy() const override {
        return (new OneDArrayMetaschemaType(subtype(), precision(), length_, units(), use_generic()));
    }

    /*!
      @brief Print information about the type to stdout.
      @param[in] indent char* Indentation to add to display output.
    */
    void display(const char *indent = "") const override {
        ScalarMetaschemaType::display(indent);
        printf("%s%-15s = %ld\n", indent, "length", length_);
    }

    /*!
      @brief Get type information as a Python dictionary.
      @returns PyObject* Python dictionary.
     */
    PyObject *as_python_dict() const override {
        PyObject *out = ScalarMetaschemaType::as_python_dict();
        set_item_python_dict_c(out, "length", &length_,
                               "OneDArrayMetaschemaType: as_python_dict: ",
                               T_INT, sizeof(size_t) * 8);
        return out;
    }

    /*!
      @brief Get the number of elements in the type.
      @returns size_t Number of elements.
     */
    const size_t nelements() const override {
        return length_;
    }

    /*!
      @brief Determine if the number of elements is variable.
      @returns bool true if the number of elements can change, false otherwise.
    */
    const bool variable_nelements() const override {
        return _variable_length;
    }

    /*!
      @brief Determine the dimensions of the equivalent numpy array.
      @param[in, out] nd int* Address of integer where number of dimensions should be stored.
      @param[in, out] dims npy_intp** Address of pointer to memory where dimensions should be stored.
     */
    void numpy_dims(int *nd, npy_intp **dims) const override;

    /*!
      @brief Update the type object with info from another type object.
      @param[in] new_info MetaschemaType* type object.
     */
    void update(const MetaschemaType *new_info) override;
    /*!
      @brief Update the type object with info from provided variable arguments for serialization.
      @param[in,out] nargs size_t Number of arguments contained in ap. On output
      the number of unused arguments will be assigned to this address.
      @param[in] ap va_list_t Variable argument list.
      @returns size_t Number of arguments in ap consumed.
     */
    size_t update_from_serialization_args(size_t *nargs, struct va_list_t &ap) override;

    /*!
      @brief Update the type object with info from provided variable arguments for deserialization.
      @param[in,out] nargs size_t Number of arguments contained in ap. On output
      the number of unused arguments will be assigned to this address.
      @param[in] ap va_list_t Variable argument list.
      @returns size_t Number of arguments in ap consumed.
     */
    size_t update_from_deserialization_args(size_t *nargs, struct va_list_t &ap) override;

    /*!
      @brief Update the instance's length.
      @param[in] new_length size_t New length.
      @param[in] force bool True if the length should be updated even if it
      is not compatible with the existing value.
     */
    void set_length(size_t new_length, bool force = false) override;

    /*!
      @brief Set the _variable_length private variable.
      @param[in] new_variable_length bool New value.
     */
    void set_variable_length(bool new_variable_length) override {
        _variable_length = new_variable_length;
    }

    /*!
      @brief Get type length.
      @returns size_t Number of elements in the array.
    */
    size_t length() const {
        return length_;
    }

    /*!
      @brief Get the number of arguments expected to be filled/used by the type.
      @returns size_t Number of arguments.
     */
    size_t nargs_exp() const override {
        size_t out = 1;
        if (_variable_length)
            out++;
        return out;
    }

    /*!
      @brief Encode the type's properties in a JSON string.
      @param[in] writer rapidjson::Writer<rapidjson::StringBuffer> rapidjson writer.
      @returns bool true if the encoding was successful, false otherwise.
     */
    bool encode_type_prop(rapidjson::Writer<rapidjson::StringBuffer> *writer) const override {
        if (!(ScalarMetaschemaType::encode_type_prop(writer))) { return false; }
        writer->Key("length");
        writer->Int((int) length_);
        return true;
    }

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
                     size_t *nargs, struct va_list_t &ap) const override;

private:
    size_t length_;
    bool _variable_length;
};
}
}
}
