#pragma once

#include "MetaschemaType.hpp"
#include "utils/logging.hpp"
#include "utils/va_list_t.hpp"

namespace communication {
namespace datatypes {
namespace Metaschema {

class ScalarMetaschemaType : public MetaschemaType {
public:
    ScalarMetaschemaType() = delete;

    /*!
      @brief Constructor for ScalarMetaschemaType.
      @param[in] subtype const character pointer to the name of the subtype.
      @param[in] precision size_t Type precision in bits.
      @param[in] units const char * (optional) Type units.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
     */
    ScalarMetaschemaType(const char *subtype, const size_t precision,
                         const char *units = "", const bool use_generic = false);

    /*!
      @brief Constructor for ScalarMetaschemaType from a JSON type defintion.
      @param[in] type_doc rapidjson::Value rapidjson object containing the type
      definition from a JSON encoded header.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
     */
    ScalarMetaschemaType(const rapidjson::Value &type_doc,
                         const bool use_generic = false);

    /*!
      @brief Constructor for ScalarMetaschemaType from Python dictionary.
      @param[in] pyobj PyObject* Python object.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
     */
    ScalarMetaschemaType(PyObject *pyobj, const bool use_generic = false);

    /*!
      @brief Copy constructor.
      @param[in] other ScalarMetaschemaType* Instance to copy.
     */
    ScalarMetaschemaType(const ScalarMetaschemaType &other) :
            ScalarMetaschemaType(other.subtype(), other.precision(),
                                 other.units(), other.use_generic()) {}

    /*!
      @brief Destructor for ScalarMetaschemaType.
      Free the type string malloc'd during constructor.
     */
    virtual ~ScalarMetaschemaType() {
        free((char *) subtype_);
        free((char *) units_);
    }

    /*!
      @brief Equivalence operator.
      @param[in] Ref MetaschemaType instance to compare against.
      @returns bool true if the instance is equivalent, false otherwise.
     */
    virtual bool operator==(const MetaschemaType &Ref) const override;

    /*!
      @brief Determine if the datatype is effectively empty.
      @returns bool true if the datatype is empty, false otherwise.
     */
    bool is_empty() const override {
        if ((type_code() == T_SCALAR)
            && (subtype_code_ == T_BYTES)
            && (precision_ == 0))
            return true;
        return false;
    }

    /*!
      @brief Create a copy of the type.
      @returns pointer to new ScalarMetaschemaType instance with the same data.
     */
    ScalarMetaschemaType *copy() const override {
        return (new ScalarMetaschemaType(subtype_, precision_, units_, use_generic()));
    }

    /*!
      @brief Print information about the type to stdout.
      @param[in] indent char* Indentation to add to display output.
    */
    void display(const char *indent = "") const override {
        MetaschemaType::display(indent);
        printf("%s%-15s = %s\n", indent, "subtype", subtype_);
        printf("%s%-15s = %d\n", indent, "subtype_code", subtype_code_);
        printf("%s%-15s = %ld\n", indent, "precision", precision_);
        printf("%s%-15s = %s\n", indent, "units", units_);
    }

    /*!
      @brief Get type information as a Python dictionary.
      @returns PyObject* Python dictionary.
     */
    PyObject *as_python_dict() const override;

    /*!
      @brief Display data.
      @param[in] data YggGeneric* Pointer to generic object.
      @param[in] indent char* Indentation to add to display output.
     */
    void display_generic(const YggGeneric *data, const char *indent = "") const override;

    /*!
      @brief Check that the subtype is correct and get the corresponding code.
      @returns int Type code for the instance's subtype.
     */
    int check_subtype() const {
        std::map<const char *, int, strcomp> type_map = get_type_map();
        std::map<const char *, int, strcomp>::iterator it = type_map.find(subtype_);
        if (it == type_map.end()) {
            utils::ygglog_throw_error("ScalarMetaschemaType: Unsupported subtype '%s'.", subtype_);
        }
        return it->second;
    }

    /*!
      @brief Get the subtype code.
      @returns const int Subtype code.
     */
    const int subtype_code() const { return subtype_code_; }

    /*!
      @brief Get the subtype string.
      @returns const char pointer to the subtype string.
     */
    const char *subtype() const { return subtype_; }

    /*!
      @brief Get the type precision.
      @returns size_t Type precision in bytes.
     */
    const size_t precision() const { return precision_; }

    /*!
      @brief Get the in_table flag.
      @return bool true if in table, false otherwise.
     */
    const bool in_table() const { return _in_table; }

    /*!
      @brief Get the type units.
      @returns const char* Type units string.
     */
    const char *units() const { return units_; }

    /*!
      @brief Get the size of the type in bits.
      @returns size_t Type size.
     */
    const size_t nbits() const {
        return precision_ * nelements();
    }

    /*!
      @brief Get the size of the type in bytes.
      @returns size_t Type size.
     */
    const size_t nbytes() const override {
        return nbits() / 8;
    }

    /*!
      @brief Get the number of bytes occupied by a variable of the type in a variable argument list.
      @returns std::vector<size_t> Number of bytes/variables occupied by the type.
     */
    std::vector<size_t> nbytes_va_core() const override;

    /*!
      @brief Skip arguments that make of this type.
      @param[in, out] nargs Pointer to number of arguments in ap.
      @param[in, out] ap va_list_t Variable argument list.
     */
    void skip_va_elements_core(size_t *nargs, struct va_list_t *ap) const override;

    /*!
      @brief Determine the dimensions of the equivalent numpy array.
      @param[in, out] nd int* Address of integer where number of dimensions should be stored.
      @param[in, out] dims npy_intp** Address of pointer to memory where dimensions should be stored.
     */
    virtual void numpy_dims(int *nd, npy_intp **dims) const {
        nd[0] = 1;
        npy_intp *idims = (npy_intp *) realloc(dims[0], sizeof(npy_intp));
        if (idims == NULL) {
            utils::ygglog_throw_error("ScalarMetaschemaType::numpy_dims: Failed to realloc dims array.");
        }
        idims[0] = 1;
        dims[0] = idims;
    }

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
    virtual size_t update_from_serialization_args(size_t *nargs, struct va_list_t &ap) override;

    /*!
      @brief Update the instance's type.
      @param[in] new_type const char * String for new type.
     */
    void update_type(const char *new_type) override {
        MetaschemaType::update_type(new_type);
        if (strcmp(type(), "scalar") == 0) {
            _variable_precision = false;
        }
    }

    /*!
      @brief Update the instance's subtype.
      @param[in] new_subtype const char * String for new subtype.
      @param[in] force bool True if the subtype should be updated even if it
      is not compatible with the existing value.
     */
    void update_subtype(const char *new_subtype, bool force = false);

    /*!
      @brief Determine if two units are compatible.
      @param[in] x One set of units.
      @param[in] y Second set of units.
      @returns bool true if they are compat, false otherwise
     */
    bool are_compat_units(const char *x, const char *y) const {
        if ((strlen(x) == (strlen(y) + 1)) && (x[strlen(x)] == 's'))
            return true;
        if (((strlen(x) + 1) == strlen(y)) && (y[strlen(y)] == 's'))
            return true;
        return false;
    }

    /*!
      @brief Update the instance's units.
      @param[in] new_units const char * String for new units.
      @param[in] force bool True if the units should be updated even if it
      is not compatible with the existing value.
     */
    void update_units(const char *new_units, bool force = false);

    /*!
      @brief Update the instance's precision.
      @param[in] new_precision size_t New precision.
      @param[in] force bool True if the precision should be updated even if it
      is not compatible with the existing value.
     */
    void set_precision(const size_t new_precision, bool force = false);

    /*!
      @brief Set the _in_table private variable.
      @param[in] new_in_table bool New value.
     */
    void set_in_table(bool new_in_table) override {
        _in_table = new_in_table;
    }

    /*!
      @brief Get the number of arguments expected to be filled/used by the type.
      @returns size_t Number of arguments.
     */
    virtual size_t nargs_exp() const override;

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
                     size_t *nargs, struct va_list_t &ap) const override;

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
                     size_t *nargs, struct va_list_t &ap) const override;

    /*!
      @brief Decode variables from a JSON string.
      @param[in] data rapidjson::Value Reference to entry in JSON string.
      @param[out] x YggGeneric* Pointer to generic object where data should be stored.
      @returns bool true if the data was successfully decoded, false otherwise.
     */
    bool decode_data(rapidjson::Value &data, YggGeneric *x) override;

    /*!
      @brief Cast raw bytes as the type described by this object.
      @param[in,out] bytes Pointer to memory contain raw bytes to cast. The
         cast version will be stored here after any necessary changes have
         been made.
      @param[in] nbytes The size of the memory pointed to by bytes. An
         error will be raised if the bytes cannot be cast because they
         would exceed this size.
      @returns The size of the re-cast bytes.
     */
    size_t cast_bytes(unsigned char **bytes, const size_t nbytes) const;

private:
    const char *subtype_;
    const int subtype_code_;
    const size_t precision_;
    const char *units_;
    bool _variable_precision;
    size_t cast_precision_;
    bool _in_table;

};
}
}
}
