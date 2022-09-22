#pragma once

#include "rapidjson/document.h"
#include "rapidjson/writer.h"

#include "MetaschemaType.hpp"

namespace communication {
namespace datatypes {
namespace Metaschema {


/*!
  @brief Class for sending strings directly.

  The DirectMetaschemaType provides basic functionality for encoding/decoding
  strings from/to JSON style strings.
 */
class DirectMetaschemaType : public MetaschemaType {
public:
    DirectMetaschemaType() = delete;

    /*!
      @brief Constructor for MetaschemaType.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
     */
    DirectMetaschemaType(const bool use_generic = false) :
            MetaschemaType("direct", use_generic) {}

    /*!
      @brief Constructor for DirectMetaschemaType from a JSON type defintion.
      @param[in] type_doc rapidjson::Value rapidjson object containing the type
      definition from a JSON encoded header.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
     */
    DirectMetaschemaType(const rapidjson::Value &type_doc,
                         const bool use_generic = false) :
            MetaschemaType("direct", use_generic) {
        // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
        UNUSED(type_doc);
#endif
    }

    /*!
      @brief Constructor for DirectMetaschemaType from Python dictionary.
      @param[in] pyobj PyObject* Python object.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
     */
    DirectMetaschemaType(PyObject *pyobj, const bool use_generic = false) :
            MetaschemaType(pyobj, use_generic) {}

    /*!
      @brief Copy constructor.
      @param[in] other DirectMetaschemaType* Instance to copy.
     */
    DirectMetaschemaType(const DirectMetaschemaType &other) :
            DirectMetaschemaType(other.use_generic()) {}

    /*!
      @brief Create a copy of the type.
      @returns pointer to new DirectMetaschemaType instance with the same data.
     */
    DirectMetaschemaType *copy() const override { return (new DirectMetaschemaType(use_generic())); }

    /*!
      @brief Get the number of arguments expected to be filled/used by the type.
      @returns size_t Number of arguments.
     */
    size_t nargs_exp() const override {
        return 2;
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
                     size_t *nargs, utils::va_list_t &ap) const override {
        // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
        UNUSED(writer);
UNUSED(nargs);
UNUSED(ap);
#endif
        utils::ygglog_error("DirectMetaschemaType::encode_data: Direct type cannot be JSON encoded.");
        return false;
    }

    /*!
      @brief Encode arguments describine an instance of this type into a JSON string.
      @param[in] writer rapidjson::Writer<rapidjson::StringBuffer> rapidjson writer.
      @param[in] x YggGeneric* Pointer to generic wrapper for data.
      @returns bool true if the encoding was successful, false otherwise.
     */
    bool encode_data(rapidjson::Writer<rapidjson::StringBuffer> *writer,
                     YggGeneric *x) const override {
        // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
        UNUSED(writer);
UNUSED(x);
#endif
        utils::ygglog_error("DirectMetaschemaType::encode_data: Direct type cannot be JSON encoded.");
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
                  const int allow_realloc, size_t *nargs, utils::va_list_t &ap) override;

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
                  const int allow_realloc, YggGeneric *x) override;

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
                     size_t *nargs, utils::va_list_t &ap) const override {
        // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
        UNUSED(data);
UNUSED(allow_realloc);
UNUSED(nargs);
UNUSED(ap);
#endif
        utils::ygglog_error("DirectMetaschemaType::decode_data: Direct type cannot be JSON decoded.");
        return false;
    }

    /*!
      @brief Decode variables from a JSON string.
      @param[in] data rapidjson::Value Reference to entry in JSON string.
      @param[out] x YggGeneric* Pointer to generic object where data should be stored.
      @returns bool true if the data was successfully decoded, false otherwise.
     */
    bool decode_data(rapidjson::Value &data, YggGeneric *x) override {
        // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
        UNUSED(data);
UNUSED(x);
#endif
        utils::ygglog_error("DirectMetaschemaType::decode_data: Direct type cannot be JSON decoded.");
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
                    const int allow_realloc, size_t *nargs, utils::va_list_t &ap) override;

    /*!
      @brief Deserialize variables from a JSON string.
      @param[in] buf char* Buffer containing serialized data.
      @param[in] buf_siz size_t Size of the serialized data.
      @param[out] x YggGeneric* Pointer to generic type wrapper where
      deserialized data should be stored.
      @returns int -1 if there is an error, 0 otherwise.
     */
    int deserialize(const char *buf, const size_t buf_siz,
                    YggGeneric *x) override {
        // Assumes reallocation is allowed
        int allow_realloc = 1;
        char **msg = (char **) (x->get_data_pointer());
        size_t *msg_siz = x->get_nbytes_pointer();
        // Copy message from buffer
        if (copy_to_buffer(buf, buf_siz, msg, *msg_siz, allow_realloc) < 0) {
            return -1;
        }
        return 0;
    }
};

}
}
}
