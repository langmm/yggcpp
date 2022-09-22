#pragma once
#include "MetaschemaType.hpp"
#include "utils/tools.hpp"

namespace communication {
namespace datatypes {
namespace Metaschema {

class AnyMetaschemaType : public MetaschemaType {
public:
    AnyMetaschemaType() = delete;

    /*!
      @brief Constructor for AnyMetaschemaType.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
      @param[in] temp_type const MetaschemaType* Type that should be used for
      the next message.
     */
    AnyMetaschemaType(const bool use_generic=true,
                      const MetaschemaType* temp_type=NULL);
    /*!
      @brief Constructor for AnyMetaschemaType from a JSON type defintion.
      @param[in] type_doc rapidjson::Value rapidjson object containing
      the type definition from a JSON encoded header.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
     */
    AnyMetaschemaType(const rapidjson::Value &type_doc,
                      const bool use_generic=true);
    /*!
      @brief Constructor for AnyMetaschemaType from Python dictionary.
      @param[in] pyobj PyObject* Python object.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
     */
    AnyMetaschemaType(PyObject* pyobj, const bool use_generic=true) :
            MetaschemaType(pyobj, true, use_generic) {}
    /*!
      @brief Copy constructor.
      @param[in] other AnyMetaschemaType* Instance to copy.
     */
    AnyMetaschemaType(const AnyMetaschemaType &other) :
            AnyMetaschemaType(other.use_generic()) {}
    /*!
      @brief Destructor for MetaschemaType.
      Free the type string malloc'd during constructor.
     */
    virtual ~AnyMetaschemaType();

    /*!
      @brief Equivalence operator.
      @param[in] Ref MetaschemaType instance to compare against.
      @returns bool true if the instance is equivalent, false otherwise.
     */
    bool operator==(const MetaschemaType &Ref) const override;

    /*!
      @brief Inequivalence operator.
      @param[in] Ref MetaschemaType instance to compare against.
      @returns bool true if the instances are not equivalent, false otherwise.
     */
    bool operator!=(const AnyMetaschemaType &Ref) const;

    /*!
      @brief Get the temporary type.
      @returns MetaschemaType* Pointer to temporary type.
     */
    const MetaschemaType* temp_type() const {
        return temp_type_;
    }
    /*!
      @brief Create a copy of the type.
      @returns AnyMetaschemaType* Pointer to new AnyMetaschemaType instance with the same data.
     */
    AnyMetaschemaType* copy() const override {
        return (new AnyMetaschemaType(use_generic(),
                                      temp_type()));
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
      @brief Copy data wrapped in YggGeneric class.
      @param[in] data YggGeneric* Pointer to generic object.
      @param[in] orig_data Pointer to data that should be copied if different
      that the data that is wrapped.
      @returns void* Pointer to copy of data.
     */
    void* copy_generic(const YggGeneric* data, void* orig_data=NULL) const override {
        if (temp_type_ == NULL) {
            communication::utils::ygglog_throw_error("AnyMetaschemaType::copy_generic: Temp type is NULL.");
        }
        return temp_type_->copy_generic(data);
    }
    /*!
      @brief Free data wrapped in YggGeneric class.
      @param[in] data YggGeneric* Pointer to generic object.
     */
    void free_generic(YggGeneric* data) const override {
        if (temp_type_ == NULL) {
            communication::utils::ygglog_throw_error("AnyMetaschemaType::free_generic: Temp type is NULL.");
        }
        temp_type_->free_generic(data);
    }
    /*!
      @brief Display data.
      @param[in] data YggGeneric* Pointer to generic object.
      @param[in] indent char* Indentation to add to display output.
     */
    void display_generic(const YggGeneric* data, const char* indent="") const override {
        if (temp_type_ == NULL) {
            communication::utils::ygglog_throw_error("AnyMetaschemaType::display_generic: Temp type is NULL.");
        }
        temp_type_->display_generic(data, indent);
    }
    /*!
      @brief Update the type object with info from another type object.
      @param[in] new_info MetaschemaType* type object.
     */
    void update(const MetaschemaType* new_info) override;

    /*!
      @brief Update the type object with info from provided variable arguments for serialization.
      @param[in,out] nargs size_t Number of arguments contained in ap. On output
      the number of unused arguments will be assigned to this address.
      @param[in] ap va_list_t Variable argument list.
      @returns size_t Number of arguments in ap consumed.
     */
    size_t update_from_serialization_args(size_t *nargs, communication::utils::va_list_t &ap) override;

    // /*!
    //   @brief Update the type object with info from provided variable arguments for deserialization.
    //   @param[in,out] x YggGeneric* Pointer to generic object where data will be stored.
    //  */
    // void update_from_deserialization_args(YggGeneric* x) override {
    //   if (temp_type_ == NULL) {
    //     ygglog_throw_error("AnyMetaschemaType::update_from_deserialization_args: Temp type is NULL.");
    //   }
    //   x->free_data();
    //   x->free_type();
    //   x->set_type(temp_type_);
    //   x->set_nbytes(temp_type_->nbytes());
    // }
    /*!
      @brief Get the number of elements in the type.
      @returns size_t Number of elements (1 for scalar).
     */
    const size_t nelements() const override {
        if (temp_type_ == NULL) {
            communication::utils::ygglog_throw_error("AnyMetaschemaType::nelements: Temp type is NULL.");
        }
        return temp_type_->nelements();
    }
    /*!
      @brief Determine if the number of elements is variable.
      @returns bool true if the number of elements can change, false otherwise.
    */
    const bool variable_nelements() const override {
        if (temp_type_ == NULL) {
            communication::utils::ygglog_throw_error("AnyMetaschemaType::variable_nelements: Temp type is NULL.");
        }
        return temp_type_->variable_nelements();
    }
    /*!
      @brief Get the item size.
      @returns size_t Size of item in bytes.
     */
    const size_t nbytes() const override {
        if (temp_type_ == NULL) {
            // ygglog_throw_error("AnyMetaschemaType::nbytes: Temp type is NULL.");
            return 0;
        }
        return temp_type_->nbytes();
    }
    /*!
      @brief Get the number of arguments expected to be filled/used by the type.
      @returns size_t Number of arguments.
     */
    size_t nargs_exp() const override {
        if (temp_type_ == NULL) {
            communication::utils::ygglog_throw_error("AnyMetaschemaType::nargs_exp: Temp type is NULL.");
        }
        return temp_type_->nargs_exp();
    }
    /*!
      @brief Convert a Python representation to a C representation.
      @param[in] pyobj PyObject* Pointer to Python object.
      @returns YggGeneric* Pointer to C object.
     */
    YggGeneric* python2c(PyObject* pyobj) const override {
        if (temp_type_ == NULL) {
            communication::utils::ygglog_throw_error("AnyMetaschemaType::python2c: Temp type is NULL.");
        }
        return temp_type_->python2c(pyobj);
    }
    /*!
      @brief Convert a C representation to a Python representation.
      @param[in] cobj YggGeneric* Pointer to C object.
      @returns PyObject* Pointer to Python object.
     */
    PyObject* c2python(YggGeneric *cobj) const override {
        if (temp_type_ == NULL) {
            communication::utils::ygglog_throw_error("AnyMetaschemaType::c2python: Temp type is NULL.");
        }
        return temp_type_->c2python(cobj);
    }

    // Encoding
    /*!
      @brief Encode the type's properties in a JSON string.
      @param[in] writer rapidjson::Writer<rapidjson::StringBuffer> rapidjson writer.
      @returns bool true if the encoding was successful, false otherwise.
     */
    bool encode_type_prop(rapidjson::Writer<rapidjson::StringBuffer> *writer) const override {
        MetaschemaType::encode_type_prop(writer);
        if (temp_type_ == NULL) {
            communication::utils::ygglog_throw_error("AnyMetaschemaType::encode_type_prop: Temp type is NULL.");
        }
        writer->Key("temptype");
        return temp_type_->encode_type(writer);
    }
    /*!
      @brief Encode arguments describine an instance of this type into a JSON string.
      @param[in] writer rapidjson::Writer<rapidjson::StringBuffer> rapidjson writer.
      @param[in] x YggGeneric* Pointer to generic wrapper for data.
      @returns bool true if the encoding was successful, false otherwise.
     */
    bool encode_data(rapidjson::Writer<rapidjson::StringBuffer> *writer,
                     YggGeneric* x) const override {
        if (temp_type_ == NULL) {
            communication::utils::ygglog_throw_error("AnyMetaschemaType::encode_data: Temp type is NULL.");
        }
        return temp_type_->encode_data(writer, x);
    }

    // Decoding
    /*!
      @brief Decode variables from a JSON string.
      @param[in] data rapidjson::Value Reference to entry in JSON string.
      @param[out] x YggGeneric* Pointer to generic object where data should be stored.
      @returns bool true if the data was successfully decoded, false otherwise.
     */
    bool decode_data(rapidjson::Value &data, YggGeneric* x) override {
        if (temp_type_ == NULL) {
            communication::utils::ygglog_throw_error("AnyMetaschemaType::decode_data: Temp type is NULL.");
        }
        return temp_type_->decode_data(data, x);
    }

private:
    MetaschemaType *temp_type_;
};

}
}
}