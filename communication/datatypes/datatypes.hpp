#pragma once
#include <cstring>
#include <string>
#include <map>
#include <Python.h>
#include "utils/va_list_t.hpp"
#include "rapidjson/document.h"
#define MSG_HEAD_SEP "YGG_MSG_HEAD"

enum DTYPE {
    T_BOOLEAN, T_INTEGER, T_NULL, T_NUMBER, T_STRING, T_ARRAY, T_OBJECT,
    T_DIRECT, T_1DARRAY, T_NDARRAY, T_SCALAR, T_FLOAT, T_UINT, T_INT, T_COMPLEX,
    T_BYTES, T_UNICODE, T_PLY, T_OBJ, T_ASCII_TABLE,
    T_CLASS, T_FUNCTION, T_INSTANCE, T_SCHEMA, T_ANY
};

namespace communication {
namespace datatypes {
namespace Metaschema {
class MetaschemaType;
}

/*!
  @brief String comparison structure.
 */
struct strcomp : public std::binary_function<const std::string, const std::string, bool>
{
    /*!
      @brief Comparison operator.
      @param[in] a char const * First string for comparison.
      @param[in] b char const * Second string for comparison.
      @returns bool true if the strings are equivalent, false otherwise.
     */
    bool operator()(const std::string &a, const std::string &b) const
    {
        return a < b;
    }
};


std::map<const char*, int, strcomp> get_type_map();


int split_head_body(const char *buf, const size_t &buf_siz,
                    char **head, size_t *headsiz);

Metaschema::MetaschemaType* type_from_header_doc(const rapidjson::Value &header_doc,
                                                 const bool use_generic=true);

Metaschema::MetaschemaType* type_from_doc(const rapidjson::Value &type_doc,
                                          const bool use_generic=true,
                                          const rapidjson::Value *header_doc=nullptr);
Metaschema::MetaschemaType* type_from_doc_c(const void* type_doc, const bool use_generic=false);
}
}

JSONArrayMetaschemaType *create_dtype_format_class(const char *format_str,
                                                   const int as_array = 0,
                                                   const bool use_generic = false);