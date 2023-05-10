#pragma once

// Platform specific
#ifdef _WIN32
#include "regex.hpp"
#else
#include "regex.hpp"
#endif
#include "constants.hpp"


#include "rapidjson/document.h"
#include "rapidjson/writer.h"
#include "rapidjson/prettywriter.h"
#include "rapidjson/stringbuffer.h"
#include "rapidjson/schema.h"
#include "rapidjson/va_list.h"
#include <cstring>


#include "logging.hpp"

#define STRLEN_RJ(var)				\
  static_cast<rapidjson::SizeType>(strlen(var))


namespace communication {
namespace utils {

/*!
  @brief Split header and body of message.
  @param[in] buf const char* Message that should be split.
  @param[out] head const char** pointer to buffer where the extracted header
  should be stored.
  @param[out] headsiz size_t reference to memory where size of extracted header
  should be stored.
  @returns: int 0 if split is successful, -1 if there was an error.
*/
static inline
int split_head_body(const char *buf,
		    const char **head, size_t *headsiz) {
  // Split buffer into head and body
  int ret;
  size_t sind, eind, sind_head, eind_head;
  sind = 0;
  eind = 0;
#ifdef _WIN32
  // Windows regex of newline is buggy
  size_t sind1, eind1, sind2, eind2;
  char re_head_tag[COMMBUFFSIZ + 1];
  snprintf(re_head_tag, COMMBUFFSIZ, "(%s)", MSG_HEAD_SEP);
  ret = find_match_c(re_head_tag, buf, &sind1, &eind1);
  if (ret > 0) {
    sind = sind1;
    ret = find_match_c(re_head_tag, buf + eind1, &sind2, &eind2);
    if (ret > 0)
      eind = eind1 + eind2;
  }
#else
  // Extract just header
  char re_head[COMMBUFFSIZ] = MSG_HEAD_SEP;
  strcat(re_head, "(.*)");
  strcat(re_head, MSG_HEAD_SEP);
  // strcat(re_head, ".*");
  ret = find_match_c(re_head, buf, &sind, &eind);
#endif
  if (ret < 0) {
    //sind_head = 0;
    //eind_head = 0;
    ygglog_throw_error_c("split_head_body: Could not find header in '%.1000s'", buf);
  } else if (ret == 0) {
    //sind_head = 0;
    //eind_head = 0;
    headsiz[0] = 0;
    ygglog_debug_c("split_head_body: No header in '%.1000s...'", buf);
  } else {
    sind_head = sind + strlen(MSG_HEAD_SEP);
    eind_head = eind - strlen(MSG_HEAD_SEP);
    headsiz[0] = (eind_head - sind_head);
    head[0] = buf + strlen(MSG_HEAD_SEP);
  }
  // char* temp = (char*)realloc(*head, *headsiz + 1);
  // if (temp == NULL) {
  //   ygglog_throw_error_c("split_head_body: Failed to reallocate header.");
  // }
  // *head = temp;
  // memcpy(*head, buf + sind_head, *headsiz);
  // (*head)[*headsiz] = '\0';
  return 0;
};


template <typename ValueT>
std::string document2string(ValueT& rhs, const char* indent="") {
  rapidjson::StringBuffer sb;
  rapidjson::PrettyWriter<rapidjson::StringBuffer> writer(sb, nullptr, strlen(indent));
  writer.SetYggdrasilMode(true);
  if (!rhs.Accept(writer)) {
    ygglog_error_c("document2string: Error in Accept(writer)");
    return "";
  }
  return sb.GetString();
}

class Metadata {
 public:
    Metadata& operator=(const Metadata&) = delete;
  Metadata(const Metadata& other) = delete;
  Metadata() :
    metadata(rapidjson::kObjectType), schema(nullptr) {}
  void _init(bool use_generic = false);
  void fromSchema(const rapidjson::Value& new_schema,
		  bool isMetadata = false, bool use_generic = false);
  void Normalize();

  void fromSchema(const std::string& schemaStr, bool use_generic = false);
  void fromType(const std::string& type, bool use_generic=false,
		bool dont_init = false);
  void fromScalar(const std::string& subtype, size_t precision,
		  const char* units=nullptr, bool use_generic=false);
  void fromNDArray(const std::string& subtype, size_t precision,
		   const size_t& ndim=0, const size_t* shape=nullptr,
		   const char* units=nullptr, bool use_generic=false);
  void _fromNDArray(const std::string& subtype, size_t precision,
		    const size_t& ndim=0, const size_t* shape=nullptr,
		    const char* units=nullptr, bool use_generic=false,
		    rapidjson::Value* subSchema = nullptr);
  void fromFormat(const std::string& format_str,
		  bool as_array = false, bool use_generic = false);
  void fromMetadata(const Metadata& other, bool use_generic = false);
  void fromMetadata(const char* head, const size_t& headsiz,
		    bool use_generic = false);
  void fromMetadata(const std::string& head, bool use_generic = false);
  void fromEncode(const rapidjson::Value& document,
		  bool use_generic = false);
  rapidjson::Document::AllocatorType& GetAllocator();
  bool isGeneric() const;
  void setGeneric();
  bool empty() const;
  bool hasType() const;
  bool hasSubtype() const;
  const char* typeName() const;
  const char* subtypeName() const;
  void initSchema();
  void initMeta();
  bool addItem(const Metadata& other);
  bool addMember(const std::string& name, const Metadata& other);
  rapidjson::Value& getMeta();
  rapidjson::Value& getSchema();
  bool SetValue(const std::string& name, rapidjson::Value& x,
		rapidjson::Value& subSchema);
#define GET_SET_METHOD_(type_in, type_out, method, setargs)		\
  type_out Get ## method(const std::string& name,			\
			 rapidjson::Value& subSchema) {			\
    if (!(subSchema.HasMember(name.c_str())))				\
      ygglog_throw_error_c("Get%s: No %s information in the schema.",	\
			 #method, name.c_str());			\
    if (!(subSchema[name.c_str()].Is ## method()))			\
      ygglog_throw_error_c("Get%s: %s is not %s.", #method,		\
			 name.c_str(), #type_in);			\
    return subSchema[name.c_str()].Get ## method();			\
  }									\
  type_out Get ## method ## Optional(const std::string& name,		\
    type_out defV,							\
    rapidjson::Value& subSchema) {					\
    if (!(subSchema.HasMember(name.c_str())))				\
      return defV;							\
    if (!(subSchema[name.c_str()].Is ## method()))			\
      ygglog_throw_error_c("GetMeta%s: %s is not %s.",			\
			 #method, name.c_str(), #type_in);		\
    return subSchema[name.c_str()].Get ## method();			\
  }									\
  bool Set ## method(const std::string& name, type_in x,			\
		     rapidjson::Value& subSchema) {			\
    rapidjson::Value x_val setargs;					\
    if (subSchema.HasMember(name.c_str())) {				\
      subSchema[name.c_str()].Swap(x_val);				\
    } else {								\
      subSchema.AddMember(						\
	rapidjson::Value(name.c_str(),		\
			 (rapidjson::SizeType)(name.size()),		\
			 metadata.GetAllocator()).Move(),		\
	x_val, metadata.GetAllocator());				\
    }									\
    return true;							\
  }									\
  type_out GetMeta ## method(const std::string& name) {			\
    return Get ## method(name, getMeta());				\
  }									\
  type_out GetMeta ## method ## Optional(const std::string& name, type_out defV) { \
    if (!(metadata.IsObject() && metadata.HasMember("__meta__")))	\
      return defV;							\
    return Get ## method ## Optional(name, defV, getMeta());		\
  }									\
  bool SetMeta ## method(const std::string& name, type_in x) {		\
    return Set ## method(name, x, getMeta());				\
  }									\
  type_out GetSchema ## method(const std::string& name,			\
			       rapidjson::Value* subSchema = nullptr) {	\
    if (subSchema == NULL)						\
      return Get ## method(name, getSchema());				\
    return Get ## method(name, *subSchema);				\
  }									\
  type_out GetSchema ## method ## Optional(const std::string& name,	\
					   type_out defV,		\
					   rapidjson::Value* subSchema = nullptr) { \
    if (subSchema == NULL) {						\
      if (schema == NULL)						\
	return defV;							\
      return Get ## method ## Optional(name, defV, getSchema());	\
    }									\
    return Get ## method ## Optional(name, defV, *subSchema);		\
  }									\
  bool SetSchema ## method(const std::string& name, type_in x,		\
			   rapidjson::Value* subSchema = nullptr) {	\
    if (subSchema == NULL)						\
      return Set ## method(name, x, getSchema());			\
    return Set ## method(name, x, *subSchema);				\
  }
  GET_SET_METHOD_(int, int, Int, (x));
  GET_SET_METHOD_(uint64_t, uint64_t, Uint, (x));
  GET_SET_METHOD_(bool, bool, Bool, (x));
  GET_SET_METHOD_(const std::string&, const char*, String,
		  (x.c_str(), (rapidjson::SizeType)(x.size()),
		   metadata.GetAllocator()));
#undef GET_SET_METHOD_
  bool SetMetaValue(const std::string& name, rapidjson::Value& x);
  bool SetSchemaValue(const std::string& name, rapidjson::Value& x,
		      rapidjson::Value* subSchema = nullptr);
  bool SetSchemaMetadata(const std::string& name,
			 const Metadata& other);
  bool SetMetaID(const std::string& name, const char** id=nullptr);
  bool SetMetaID(const std::string& name, std::string& id);
  int deserialize(const char* buf, size_t nargs, int allow_realloc, ...);
  int deserialize(const char* buf, rapidjson::VarArgList& ap);
  int serialize(char **buf, size_t *buf_siz, size_t nargs, ...);
  int serialize(char **buf, size_t *buf_siz,
		rapidjson::VarArgList& ap);
  void Display(const char* indent="") const;
  rapidjson::Document metadata;
  rapidjson::Value* schema;
 private:
  void _reset();
  void _update_schema();
};

class Header : public Metadata {
public:
    Header(const Header& other) = delete;
    Header& operator=(const Header&) = delete;
  Header() :
    data_(nullptr), data(nullptr), size_data(0), size_buff(0), size_curr(0),
    size_head(0), flags(HEAD_FLAG_VALID) {}
  ~Header() {
    if ((flags & HEAD_FLAG_OWNSDATA) && data_)
      free(data_);
  }

  bool isValid() const;
  void invalidate();

  void setMessageFlags(const char* msg, const size_t& msg_len);

  /*!
    @brief Set parameters for sending a message.
    @param[in] metadata0 Pointer to metadata object
  */
  void for_send(Metadata* metadata0, const char* msg, const size_t& len);
  /*!
    @brief Set parameters for receiving a message.
    @param[in] buf Message containing header.
    @param[in] buf_siz Size of buffer containing message.
    @param[in] msg_siz Size of message in buffer.
    @param[in] allow_realloc If true, the buffer can be resized to
      receive message larger than buf_siz.
   */
  void for_recv(char** buf, size_t buf_siz, size_t msg_siz,
		bool allow_realloc, bool temp=false);

  void formatBuffer(rapidjson::StringBuffer& buffer, bool metaOnly=false);

  size_t format(const char* buf, size_t buf_siz,
		size_t size_max, bool metaOnly=false);

  void finalize_recv();
  
  char* data_;
  char** data;
  size_t size_data;
  size_t size_buff;
  size_t size_curr;
  size_t size_head;
  uint16_t flags;
};

}
}


// Local Variables:
// mode: c++
// End:
