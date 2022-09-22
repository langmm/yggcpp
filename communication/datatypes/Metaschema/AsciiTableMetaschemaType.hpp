#pragma once
#include "MetaschemaType.hpp"
#include "datatypes/AsciiTable.hpp"

namespace communication {
namespace datatypes {
namespace Metaschema {

class AsciiTableMetaschemaType : public MetaschemaType {
public:
    AsciiTableMetaschemaType() = delete;

    /*!
      @brief Constructor for AsciiTableMetaschemaType.
      @param[in] format_str char * Format string describing table structure.
      @param[in] as_array int (optional) If 1, the instance will act to
      serialize/deserialize table columns. If 0, the instnace will act to
      serialize/deserialize table rows. Defaults to 0.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
     */
    AsciiTableMetaschemaType(const char *format_str, const int as_array=0,
                             const bool use_generic=false);

    /*!
      @brief Constructor for AsciiTableMetaschemaType from Python dictionary.
      @param[in] pyobj PyObject* Python object.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
     */
    AsciiTableMetaschemaType(PyObject* pyobj, const bool use_generic=false);

    /*!
      @brief Copy constructor.
      @param[in] other AsciiTableMetaschemaType* Instance to copy.
     */
    AsciiTableMetaschemaType(const AsciiTableMetaschemaType &other) :
            AsciiTableMetaschemaType(other.format_str(), other.as_array(),
                                     other.use_generic()) {}
    /*!
      @brief Destructor for AsciiTableMetaschemaType.
      Free the table structure created during constructor.
     */
    ~AsciiTableMetaschemaType();

    /*!
      @brief Equivalence operator.
      @param[in] Ref MetaschemaType instance to compare against.
      @returns bool true if the instance is equivalent, false otherwise.
     */
    bool operator==(const MetaschemaType &Ref) const override;

    /*!
      @brief Create a copy of the type.
      @returns pointer to new AsciiTableMetaschemaType instance with the same data.
     */
    AsciiTableMetaschemaType* copy() const override { return (new AsciiTableMetaschemaType(format_str(), as_array(), use_generic())); }
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
      @brief Get format string describing table.
      @returns char * Format string.
     */
    const char* format_str() const { return table_->format_str; }
    /*!
      @brief Get table struct.
      @returns asciiTable_t* Table struct.
     */
    asciiTable_t* table() const { return table_; }
    /*!
      @brief Get as_array.
      @returns bool 1 if elements in table are all arrays, 0 otherwise.
     */
    const int as_array() const { return as_array_; }
    /*!
      @brief Get the number of arguments expected to be filled/used by the type.
      @returns size_t Number of arguments.
     */
    size_t nargs_exp() const override {
        size_t nargs = table_->ncols();
        if (as_array_) {
            nargs++; // For the number of rows
        }
        return nargs;
    }
    /*!
      @brief Update the instance's as_array flag.
      @param[in] new_as_array const int New as_array flag.
      @param[in] force bool True if the as_array parameter should be updated
      even if it is not compatible with the existing value.
     */
    void update_as_array(const int new_as_array, bool force=false);

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
                     size_t *nargs, communication::utils::va_list_t &ap) const override {
        // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
        UNUSED(writer);
    UNUSED(nargs);
    UNUSED(ap);
#endif
        communication::utils::ygglog_error("AsciiTableMetaschemaType::encode_data: AsciiTable data cannot be JSON encoded.");
        return false;
    }
    /*!
      @brief Encode arguments describine an instance of this type into a JSON string.
      @param[in] writer rapidjson::Writer<rapidjson::StringBuffer> rapidjson writer.
      @param[in] x YggGeneric* Pointer to generic wrapper for data.
      @returns bool true if the encoding was successful, false otherwise.
     */
    bool encode_data(rapidjson::Writer<rapidjson::StringBuffer> *writer,
                     YggGeneric* x) const override {
        // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
        UNUSED(writer);
    UNUSED(x);
#endif
        communication::utils::ygglog_error("AsciiTableMetaschemaType::encode_data: AsciiTable type cannot be JSON encoded.");
        return false;
    }
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
    int serialize(char **buf, size_t *buf_siz,
                  const int allow_realloc, size_t *nargs, communication::utils::va_list_t &ap) override;

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
    int serialize(char **buf, size_t *buf_siz,
                  const int allow_realloc, YggGeneric* x) override {
        // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
        UNUSED(buf);
    UNUSED(buf_siz);
    UNUSED(allow_realloc);
    UNUSED(x);
#endif
        communication::utils::ygglog_error("AsciiTableMetaschemaType::deserialize: serialization from generic wrapper for table not supported.");
        return -1;
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
                     size_t *nargs, communication::utils::va_list_t &ap) const override {
        // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
        UNUSED(data);
    UNUSED(allow_realloc);
    UNUSED(nargs);
    UNUSED(ap);
#endif
        communication::utils::ygglog_error("AsciiTableMetaschemaType::decode_data: AsciiTable type cannot be JSON decoded.");
        return false;
    }
    /*!
      @brief Decode variables from a JSON string.
      @param[in] data rapidjson::Value Reference to entry in JSON string.
      @param[out] x YggGeneric* Pointer to generic object where data should be stored.
      @returns bool true if the data was successfully decoded, false otherwise.
     */
    bool decode_data(rapidjson::Value &data, YggGeneric* x) override {
        // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
        UNUSED(data);
    UNUSED(x);
#endif
        communication::utils::ygglog_error("AsciiTableMetaschemaType::decode_data: AsciiTable type cannot be JSON decoded.");
        return false;
    }
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
      remaining in ap.
     */
    int deserialize(const char *buf, const size_t buf_siz,
                    const int allow_realloc, size_t *nargs, communication::utils::va_list_t &ap) override;

    /*!
      @brief Deserialize variables from a JSON string.
      @param[in] buf char* Buffer containing serialized data.
      @param[in] buf_siz size_t Size of the serialized data.
      @param[out] x YggGeneric* Pointer to generic type wrapper where
      deserialized data should be stored.
      @returns int -1 if there is an error, 0 otherwise.
     */
    int deserialize(const char *buf, const size_t buf_siz,
                    YggGeneric* x) override {
        // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
        UNUSED(buf);
    UNUSED(buf_siz);
    UNUSED(x);
#endif
        communication::utils::ygglog_error("AsciiTableMetaschemaType::deserialize: deserialization into generic wrapper for table not supported.");
        return -1;
    }

private:
    const int as_array_;
    asciiTable_t *table_;

};
}
}
}