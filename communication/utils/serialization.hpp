#ifndef YGGDRASIL_SERIALIZATION_H_
#define YGGDRASIL_SERIALIZATION_H_

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
#include <string.h>


#include "logging.hpp"

#define STRLEN_RJ(var)				\
  static_cast<rapidjson::SizeType>(strlen(var))


using namespace communication::utils;
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
    sind_head = 0;
    eind_head = 0;
    ygglog_throw_error_c("split_head_body: Could not find header in '%.1000s'", buf);
  } else if (ret == 0) {
    sind_head = 0;
    eind_head = 0;
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
  rapidjson::PrettyWriter<rapidjson::StringBuffer> writer(sb, 0, strlen(indent));
  writer.SetYggdrasilMode(true);
  if (!rhs.Accept(writer)) {
    ygglog_error_c("document2string: Error in Accept(writer)");
    return std::string("");
  }
  return std::string(sb.GetString());
}

class Metadata {
 public:
  Metadata() :
    metadata(rapidjson::kObjectType), schema(NULL) {}
  void _init(bool use_generic = false) {
    Normalize();
    _update_schema();
    if (use_generic)
      setGeneric();
  }
  void fromSchema(const rapidjson::Value& new_schema,
		  bool isMetadata = false, bool use_generic = false) {
    if (isMetadata) {
      metadata.CopyFrom(new_schema, metadata.GetAllocator(), true);
      _init(use_generic);
    } else if (!hasType()) {
      if (!use_generic)
	use_generic = isGeneric();
      initSchema();
      schema->CopyFrom(new_schema, metadata.GetAllocator(), true);
      _init(use_generic);
    } else {
      rapidjson::SchemaDocument sd_old(*schema);
      rapidjson::SchemaNormalizer n(sd_old);
      if (!n.Compare(new_schema)) {
	std::string s_old_str = document2string(*schema);
	std::string s_new_str = document2string(new_schema);
	rapidjson::Value err;
	n.GetErrorMsg(err, metadata.GetAllocator());
	std::string err_str = document2string(err);
	ygglog_throw_error_c("Schemas incompatible:\n"
			   "old:\n%s\n"
			   "new:\n%s\n"
			   "error:\n%s", s_old_str.c_str(),
			   s_new_str.c_str(), err_str.c_str());
      }
    }
  }
  void Normalize() {
    rapidjson::Document s(rapidjson::kObjectType);
#define ADD_OBJECT_(x, name, len)					\
    x.AddMember(rapidjson::Value("type", 4, s.GetAllocator()).Move(),	\
		rapidjson::Value("object", 6, s.GetAllocator()).Move(),	\
		s.GetAllocator());					\
    x.AddMember(rapidjson::Value("properties", 10,			\
				 s.GetAllocator()).Move(),		\
		rapidjson::Value(rapidjson::kObjectType).Move(),	\
		s.GetAllocator());					\
    x["properties"].AddMember(rapidjson::Value(#name, len,		\
					       s.GetAllocator()).Move(), \
			      rapidjson::Value(rapidjson::kObjectType).Move(), \
			      s.GetAllocator())
    ADD_OBJECT_(s, serializer, 10);
    ADD_OBJECT_(s["properties"]["serializer"], datatype, 8);
#undef ADD_OBJECT_
    s["properties"]["serializer"]["properties"]["datatype"].AddMember(
       rapidjson::Value("type", 4, s.GetAllocator()).Move(),
       rapidjson::Value("schema", 6, s.GetAllocator()).Move(),
       s.GetAllocator());
    rapidjson::StringBuffer sb;
    if (!metadata.Normalize(s, &sb)) {
      ygglog_throw_error_c("Metdata::Normalize: Failed to normalize schema:\n"
			 "%s\nerror =\n%s",
			 document2string(metadata).c_str(),
			 sb.GetString());
    }
  }

  void fromSchema(const std::string schemaStr, bool use_generic = false) {
    rapidjson::Document d;
    d.Parse(schemaStr.c_str());
    fromSchema(d, false, use_generic);
    if (hasType()) {
      typename rapidjson::Value::MemberIterator it = schema->FindMember(rapidjson::Document::GetTypeString());
      if ((!isGeneric()) &&
	  it != schema->MemberEnd() &&
	  (it->value == rapidjson::Document::GetObjectString() ||
	   it->value == rapidjson::Document::GetSchemaString() ||
	   it->value == rapidjson::Document::GetPythonInstanceString() ||
	   it->value == rapidjson::Document::GetAnyString() ||
	   (it->value == rapidjson::Document::GetArrayString() &&
	    !schema->HasMember(rapidjson::Document::GetItemsString()))))
	setGeneric();
    }
  }
  void fromType(const std::string type, bool use_generic=false,
		bool dont_init = false) {
    initSchema();
    SetSchemaString("type", type);
    if (!dont_init)
      _init(use_generic);
  }
  void fromScalar(const std::string subtype, size_t precision,
		  const char* units=NULL, bool use_generic=false) {
    fromType("scalar", use_generic, true);
    _fromNDArray(subtype, precision, 0, NULL, units, use_generic);
  }
  void fromNDArray(const std::string subtype, size_t precision,
		   const size_t ndim=0, const size_t* shape=NULL,
		   const char* units=NULL, bool use_generic=false) {
    fromType("ndarray", use_generic, true);
    _fromNDArray(subtype, precision, ndim, shape, units, use_generic);
  }
  void _fromNDArray(const std::string subtype, size_t precision,
		    const size_t ndim=0, const size_t* shape=NULL,
		    const char* units=NULL, bool use_generic=false,
		    rapidjson::Value* subSchema = NULL) {
    if (subtype == "bytes") {
      SetSchemaString("subtype", "string", subSchema);
    } else if (subtype == "unicode") {
      SetSchemaString("subtype", "string", subSchema);
      SetSchemaString("encoding", "UTF8", subSchema); // UCS4?
    } else {
      SetSchemaString("subtype", subtype, subSchema);
    }
    if (precision > 0)
      SetSchemaUint("precision", precision, subSchema);
    if (ndim > 0) {
      if (shape != NULL) {
	rapidjson::Value shp(rapidjson::kArrayType);
	for (size_t i = 0; i < ndim; i++) {
	  shp.PushBack(rapidjson::Value((uint64_t)(shape[i])).Move(),
		       GetAllocator());
	}
	SetSchemaValue("shape", shp, subSchema);
      } else {
	SetSchemaUint("ndim", ndim, subSchema);
      }
    }
    if (units && strlen(units) > 0) {
      SetSchemaString(units, units, subSchema);
    }
    if (subSchema == NULL)
      _init(use_generic);
  }
  void fromFormat(const std::string& format_str,
		  bool as_array = false, bool use_generic = false) {
    initSchema();
    metadata["serialize"].AddMember(
      rapidjson::Value("format_str", 10, GetAllocator()).Move(),
      rapidjson::Value(format_str.c_str(),
		       (rapidjson::SizeType)(format_str.size()),
		       GetAllocator()).Move(),
      GetAllocator());
    SetSchemaString("type", "array");
    rapidjson::Value items(rapidjson::kArrayType);
    // Loop over string
    int mres;
    size_t sind, eind, beg = 0, end;
    char ifmt[FMT_LEN + 1];
    char re_fmt[FMT_LEN + 1];
    char re_fmt_eof[FMT_LEN + 1];
    snprintf(re_fmt, FMT_LEN, "%%[^%s%s ]+[%s%s ]", "\t", "\n", "\t", "\n");
    snprintf(re_fmt_eof, FMT_LEN, "%%[^%s%s ]+", "\t", "\n");
    size_t iprecision = 0;
    const char* element_type;
    if (as_array)
      element_type = "ndarray";
    else
      element_type = "scalar";
    while (beg < format_str.size()) {
      char isubtype[FMT_LEN] = "";
      mres = find_match_c(re_fmt, format_str.c_str() + beg, &sind, &eind);
      if (mres < 0) {
	ygglog_throw_error_c("Metadata::fromFormat: find_match returned %d", mres);
      } else if (mres == 0) {
	// Make sure its not just a format string with no newline
	mres = find_match_c(re_fmt_eof, format_str.c_str() + beg,
			  &sind, &eind);
	if (mres <= 0) {
	  beg++;
	  continue;
	}
      }
      beg += sind;
      end = beg + (eind - sind);
      strncpy(ifmt, format_str.c_str() + beg, end-beg);
      ifmt[end-beg] = '\0';
      // String
      if (find_match_c("%(.*)s", ifmt, &sind, &eind)) {
	strncpy(isubtype, "string", FMT_LEN); // or unicode
	mres = regex_replace_c(ifmt, FMT_LEN,
				 "%(\\.)?([[:digit:]]*)s(.*)", "$2", 0);
	iprecision = (size_t)atoi(ifmt);
	// Complex
#ifdef _WIN32
      } else if (find_match_c("(%.*[fFeEgG]){2}j", ifmt, &sind, &eind)) {
#else
      } else if (find_match_c("(\%.*[fFeEgG]){2}j", ifmt, &sind, &eind)) {
#endif
	strncpy(isubtype, "complex", FMT_LEN);
	iprecision = 2 * sizeof(double);
      }
      // Floats
      else if (find_match_c("%.*[fFeEgG]", ifmt, &sind, &eind)) {
	strncpy(isubtype, "float", FMT_LEN);
	iprecision = sizeof(double);
      }
      // Integers
      else if (find_match_c("%.*hh[id]", ifmt, &sind, &eind)) {
	strncpy(isubtype, "int", FMT_LEN);
	iprecision = sizeof(char);
      } else if (find_match_c("%.*h[id]", ifmt, &sind, &eind)) {
	strncpy(isubtype, "int", FMT_LEN);
	iprecision = sizeof(short);
      } else if (find_match_c("%.*ll[id]", ifmt, &sind, &eind)) {
	strncpy(isubtype, "int", FMT_LEN);
	iprecision = sizeof(long long);
      } else if (find_match_c("%.*l64[id]", ifmt, &sind, &eind)) {
	strncpy(isubtype, "int", FMT_LEN);
	iprecision = sizeof(long long);
      } else if (find_match_c("%.*l[id]", ifmt, &sind, &eind)) {
	strncpy(isubtype, "int", FMT_LEN);
	iprecision = sizeof(long);
      } else if (find_match_c("%.*[id]", ifmt, &sind, &eind)) {
	strncpy(isubtype, "int", FMT_LEN);
	iprecision = sizeof(int);
      }
      // Unsigned integers
      else if (find_match_c("%.*hh[uoxX]", ifmt, &sind, &eind)) {
	strncpy(isubtype, "uint", FMT_LEN);
	iprecision = sizeof(unsigned char);
      } else if (find_match_c("%.*h[uoxX]", ifmt, &sind, &eind)) {
	strncpy(isubtype, "uint", FMT_LEN);
	iprecision = sizeof(unsigned short);
      } else if (find_match_c("%.*ll[uoxX]", ifmt, &sind, &eind)) {
	strncpy(isubtype, "uint", FMT_LEN);
	iprecision = sizeof(unsigned long long);
      } else if (find_match_c("%.*l64[uoxX]", ifmt, &sind, &eind)) {
	strncpy(isubtype, "uint", FMT_LEN);
	iprecision = sizeof(unsigned long long);
      } else if (find_match_c("%.*l[uoxX]", ifmt, &sind, &eind)) {
	strncpy(isubtype, "uint", FMT_LEN);
	iprecision = sizeof(unsigned long);
      } else if (find_match_c("%.*[uoxX]", ifmt, &sind, &eind)) {
	strncpy(isubtype, "uint", FMT_LEN);
	iprecision = sizeof(unsigned int);
      } else {
	ygglog_throw_error_c("Metadata::fromFormat: Could not parse format string: %s", ifmt);
      }
      ygglog_debug_c("isubtype = %s, iprecision = %lu, ifmt = %s",
		   isubtype, iprecision, ifmt);
      rapidjson::Value item;
      SetString("type", element_type, item);
      _fromNDArray(isubtype, iprecision, 0, NULL, NULL, false, &item);
      items.PushBack(item, GetAllocator());
      beg = end;
    }
    rapidjson::SizeType nItems = items.Size();
    SetSchemaValue("items", items);
    if (nItems == 1) {
      SetSchemaBool("allowSingular", true);
    }
    // if (nItems == 1) {
    //   typename rapidjson::Document::ValueType tmp;
    //   metadata["serializer"]["datatype"].Swap(tmp);
    //   metadata["serializer"]["datatype"].Swap(tmp["items"][0]);
    //   metadata["serializer"].RemoveMember("format_str");
    // }
    _init(use_generic);
  }
  void fromMetadata(const Metadata& other, bool use_generic = false) {
    metadata.CopyFrom(other.metadata, GetAllocator(), true);
    _init(use_generic);
  }
  void fromMetadata(const char* head, const size_t headsiz,
		    bool use_generic = false) {
    metadata.Parse(head, headsiz);
    if (metadata.HasParseError()) {
      ygglog_throw_error_c("Metadata::fromMetadata: Error parsing header: %s.", head);
    }
    if (!(metadata.IsObject()))
      ygglog_throw_error_c("Metadata::fromMetadata: head document must be an object.");
    if (!(metadata.HasMember("__meta__")))
      ygglog_throw_error_c("Metadata::fromMetadata: No __meta__ information in the header.");
    if (!(metadata["__meta__"].IsObject()))
      ygglog_throw_error_c("Metadata::fromMetadata: __meta__ is not an object.");
    _init(use_generic);
  }
  void fromEncode(const rapidjson::Value& document,
		  bool use_generic = false) {
    rapidjson::SchemaEncoder encoder(true);
    if (!document.Accept(encoder)) {
      ygglog_throw_error_c("Metadata::fromEncode: Error in schema encoding.");
    }
    fromSchema(encoder.GetSchema(), false, use_generic);
  }
  rapidjson::Document::AllocatorType& GetAllocator() {
    return metadata.GetAllocator();
  }
  bool isGeneric() const {
    return (schema &&
	    (schema->HasMember("use_generic") &&
	     (*schema)["use_generic"].IsBool() &&
	     (*schema)["use_generic"].GetBool()));
  }
  void setGeneric() {
    initSchema();
    SetSchemaBool("use_generic", true);
  }
  bool empty() const {
    return ((!metadata.IsObject()) || (metadata.MemberCount() == 0));
  }
  bool hasType() const {
    return (schema && schema->HasMember("type"));
  }
  bool hasSubtype() const {
    if (strcmp(typeName(), "scalar") != 0) {
      return false;
    }
    return schema->HasMember("subtype");
  }
  const char* typeName() const {
    if (!hasType())
      return "";
    return (*schema)["type"].GetString();
  }
  const char* subtypeName() const {
    if (!hasSubtype())
      return "";
    return (*schema)["subtype"].GetString();
  }
  void initSchema() {
    if (schema == NULL) {
      if (!metadata.HasMember("serializer"))
	metadata.AddMember(
	  rapidjson::Value("serializer", 10).Move(),
	  rapidjson::Value(rapidjson::kObjectType).Move(),
	  metadata.GetAllocator());
      if (!metadata["serializer"].HasMember("datatype"))
	metadata["serializer"].AddMember(
	  rapidjson::Value("datatype", 8).Move(),
	  rapidjson::Value(rapidjson::kObjectType).Move(),
	  metadata.GetAllocator());
      schema = &(metadata["serializer"]["datatype"]);
    }
  }
  void initMeta() {
    if (!metadata.HasMember("__meta__")) {
      rapidjson::Value meta(rapidjson::kObjectType);
      metadata.AddMember(rapidjson::Value("__meta__", 8).Move(),
			 meta, metadata.GetAllocator());
    }
  }
  bool addItem(const Metadata& other) {
    if (!(schema &&
	  schema->HasMember("items") &&
	  (*schema)["items"].IsArray())) {
      ygglog_throw_error_c("Metadata::addItem: schema does not have items");
    }
    if (!other.schema) {
      ygglog_throw_error_c("Metadata::addItem: item does not have schema");
    }
    rapidjson::Value item;
    item.CopyFrom(*(other.schema), GetAllocator(), true);
    (*schema)["items"].PushBack(item, GetAllocator());
    return true;
  }
  bool addMember(const std::string name, const Metadata& other) {
    if (!(schema &&
	  schema->HasMember("properties") &&
	  (*schema)["properties"].IsObject())) {
      ygglog_throw_error_c("Metadata::addMember: schema does not have properties");
    }
    if (!other.schema) {
      ygglog_throw_error_c("Metadata::addMember: member does not have schema");
    }
    rapidjson::Value item;
    item.CopyFrom(*(other.schema), GetAllocator(), true);
    if ((*schema)["properties"].HasMember(name.c_str())) {
      (*schema)["properties"][name.c_str()].Swap(item);
    } else {
      (*schema)["properties"].AddMember(
	 rapidjson::Value(name.c_str(),
			  (rapidjson::SizeType)(name.size()),
			  GetAllocator()).Move(),
	 item, GetAllocator());
    }
    return true;
  }
  rapidjson::Value& getMeta() {
    if (!(metadata.IsObject() && metadata.HasMember("__meta__")))
      ygglog_throw_error_c("getMeta: No __meta__ in metadata");
    return metadata["__meta__"];
  }
  rapidjson::Value& getSchema() {
    if (schema == NULL)
      ygglog_throw_error_c("getSchema: No datatype in metadata");
    return *schema;
  }
  bool SetValue(const std::string name, rapidjson::Value& x,
		rapidjson::Value& subSchema) {
    if (subSchema.HasMember(name.c_str())) {
      subSchema[name.c_str()].Swap(x);
    } else {
      subSchema.AddMember(
	rapidjson::Value(name.c_str(),
			 (rapidjson::SizeType)(name.size()),
			 GetAllocator()).Move(),
	x, GetAllocator());
    }
    return true;
  }
#define GET_SET_METHOD_(type_in, type_out, method, setargs)		\
  type_out Get ## method(const std::string name,			\
			 rapidjson::Value& subSchema) {			\
    if (!(subSchema.HasMember(name.c_str())))				\
      ygglog_throw_error_c("Get%s: No %s information in the schema.", #method, name.c_str()); \
    if (!(subSchema[name.c_str()].Is ## method()))			\
      ygglog_throw_error_c("Get%s: %s is not %s.", #method, name.c_str(), #type_in); \
    return subSchema[name.c_str()].Get ## method();			\
  }									\
  type_out Get ## method ## Optional(const std::string name,		\
    type_out defV,							\
    rapidjson::Value& subSchema) {					\
    if (!(subSchema.HasMember(name.c_str())))				\
      return defV;							\
    if (!(subSchema[name.c_str()].Is ## method()))			\
      ygglog_throw_error_c("GetMeta%s: %s is not %s.", #method, name.c_str(), #type_in); \
    return subSchema[name.c_str()].Get ## method();			\
				 }					\
				 bool Set ## method(const std::string name, type_in x, rapidjson::Value& subSchema) { \
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
  type_out GetMeta ## method(const std::string name) {			\
    return Get ## method(name, getMeta());				\
  }									\
  type_out GetMeta ## method ## Optional(const std::string name, type_out defV) { \
    if (!(metadata.IsObject() && metadata.HasMember("__meta__")))	\
      return defV;							\
    return Get ## method ## Optional(name, defV, getMeta());		\
  }									\
  bool SetMeta ## method(const std::string name, type_in x) {		\
    return Set ## method(name, x, getMeta());				\
  }									\
  type_out GetSchema ## method(const std::string name,			\
			       rapidjson::Value* subSchema = NULL) {	\
    if (subSchema == NULL)						\
      return Get ## method(name, getSchema());				\
    return Get ## method(name, *subSchema);				\
  }									\
  type_out GetSchema ## method ## Optional(const std::string name,	\
					   type_out defV,		\
					   rapidjson::Value* subSchema = NULL) { \
    if (subSchema == NULL) {						\
      if (schema == NULL)						\
	return defV;							\
      return Get ## method ## Optional(name, defV, getSchema());	\
    }									\
    return Get ## method ## Optional(name, defV, *subSchema);		\
  }									\
  bool SetSchema ## method(const std::string name, type_in x,		\
			   rapidjson::Value* subSchema = NULL) {	\
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
  bool SetMetaValue(const std::string name, rapidjson::Value& x) {
    return SetValue(name, x, getMeta());
  }
  bool SetSchemaValue(const std::string name, rapidjson::Value& x,
		      rapidjson::Value* subSchema = NULL) {
    if (subSchema == NULL)
      return SetValue(name, x, getSchema());
    return SetValue(name, x, *subSchema);
  }
  bool SetSchemaMetadata(const std::string name,
			 const Metadata& other) {
    if (other.schema == NULL)
      ygglog_throw_error_c("SetSchemaMetadata: Value has not datatype");
    rapidjson::Value x;
    x.CopyFrom(*(other.schema), GetAllocator(), true);
    return SetSchemaValue(name, x);
  }
  bool SetMetaID(const std::string name, const char** id=NULL) {
    char new_id[100];
    snprintf(new_id, 100, "%d", rand());
    bool out = SetMetaString(name, new_id);
    if (out && id)
      id[0] = GetMetaString(name);
    return out;
  }
  bool SetMetaID(const std::string name, std::string& id) {
    const char* id_str;
    bool out = SetMetaID(name, &id_str);
    if (out)
      id.assign(id_str);
    return out;
  }
  int deserialize(const char* buf, rapidjson::VarArgList& ap) {
    if (!hasType())
      ygglog_throw_error_c("Metadata::deserialize: No datatype");
    size_t nargs_orig = ap.get_nargs();
    rapidjson::Document d;
    rapidjson::StringStream s(buf);
    d.ParseStream(s);
    if (d.HasParseError())
      ygglog_throw_error_c("Metadata::deserialize: Error parsing JSON");
    // TODO: Initialize schema?
    // if (schema.IsNull()) {
    //   schema = encode_schema(d);
    // } else {
    rapidjson::StringBuffer sb;
    if (!d.Normalize(*schema, &sb)) {
      std::string d_str = document2string(d);
      std::string s_str = document2string(*schema);
      ygglog_throw_error_c("Metadata::deserialize: Error normalizing document:\n%s\ndocument=%s\nschema=%s\nmessage=%s...", sb.GetString(), d_str.c_str(), s_str.c_str(), buf);
    }
    // }
    if (!d.SetVarArgs(*schema, ap)) {
      ygglog_throw_error_c("Metadata::deserialize_args: Error setting arguments from JSON document");
    }
    return (int)(nargs_orig - ap.get_nargs());
  }
  int serialize(char **buf, size_t *buf_siz,
		rapidjson::VarArgList& ap) {
    if (!hasType())
      ygglog_throw_error_c("Metadata::serialize: No datatype");
    rapidjson::Document d;
    if (!d.GetVarArgs(*schema, ap)) {
      std::string s_str = document2string(*schema);
      ygglog_throw_error_c("Metadata::serialize: Error creating JSON document from arguments for schema = %s", s_str.c_str());
    }
    rapidjson::StringBuffer buffer;
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
    if (!d.Accept(writer))
      ygglog_throw_error_c("Metadata::serialize: Error serializing document.");
    if ((size_t)(buffer.GetLength() + 1) > buf_siz[0]) {
      buf_siz[0] = (size_t)(buffer.GetLength() + 1);
      buf[0] = (char*)realloc(buf[0], buf_siz[0]);
    }
    memcpy(buf[0], buffer.GetString(), (size_t)(buffer.GetLength()));
    buf[0][(size_t)(buffer.GetLength())] = '\0';
    return static_cast<int>(buffer.GetLength());
  }
  void Display(const char* indent="") const {
    std::cout << document2string(metadata, indent) << std::endl;
  }
  rapidjson::Document metadata;
  rapidjson::Value* schema;
 private:
  void _reset() {
    metadata.SetObject();
    schema = NULL;
  }
  void _update_schema() {
    if (metadata.HasMember("serializer") &&
	metadata["serializer"].IsObject() &&
	metadata["serializer"].HasMember("datatype") &&
	metadata["serializer"]["datatype"].IsObject()) {
      schema = &(metadata["serializer"]["datatype"]);
    } else {
      schema = NULL;
    }
  }
};

class Header : public Metadata {
public:
  Header() :
    data_(NULL), data(NULL), size_data(0), size_buff(0), size_curr(0),
    size_head(0), flags(HEAD_FLAG_VALID) {}
  ~Header() {
    if ((flags & HEAD_FLAG_OWNSDATA) && data_)
      free(data_);
  }

  bool isValid() {
    return (flags & HEAD_FLAG_VALID);
  }
  void invalidate() {
    flags &= ~HEAD_FLAG_VALID;
  }

  /*!
    @brief Set parameters for sending a message.
    @param[in] metadata0 Pointer to metadata object
  */
  void for_send(Metadata* metadata0) {
    // flags |= (HEAD_FLAG_ALLOW_REALLOC | HEAD_FLAG_OWNSDATA);
    if (metadata0 != NULL)
      fromMetadata(*metadata0);
    initMeta();
    SetMetaID("id");
    char model[100] = "";
    char *model_name = getenv("YGG_MODEL_NAME");
    if (model_name != NULL) {
      strcpy(model, model_name);
    }
    char *model_copy = getenv("YGG_MODEL_COPY");
    if (model_copy != NULL) {
      strcat(model, "_copy");
      strcat(model, model_copy);
    }
    SetMetaString("model", model);
  }
  /*!
    @brief Set parameters for receiving a message.
    @param[in] buf Message containing header.
    @param[in] buf_siz Size of buffer containing message.
    @param[in] msg_siz Size of message in buffer.
    @param[in] allow_realloc If true, the buffer can be resized to
      receive message larger than buf_siz.
   */
  void for_recv(char** buf, size_t buf_siz, size_t msg_siz,
		bool allow_realloc, bool temp=false) {
    data = buf;
    size_buff = buf_siz;
    size_curr = msg_siz;
    if (allow_realloc)
      flags |= HEAD_FLAG_ALLOW_REALLOC;
    if (temp)
      flags |= HEAD_TEMPORARY;
    const char *head = NULL;
    size_t headsiz = 0;
    split_head_body(*buf, &head, &headsiz);
    if (headsiz == 0) {
      size_data = size_curr;
    } else {
      fromMetadata(head, headsiz);
      size_head = headsiz + 2*strlen(MSG_HEAD_SEP);
      // size_t bodysiz = msg_siz - size_head;
      if (!(flags & HEAD_TEMPORARY)) {
	size_curr -= size_head;
	memmove(data[0], data[0] + size_head, size_curr);
	(*data)[size_curr] = '\0';
      }
      // Update parameters from document
      size_data = static_cast<size_t>(GetMetaInt("size"));
      if (GetMetaBoolOptional("in_data", false))
	flags |= HEAD_META_IN_DATA;
      else
	flags &= ~HEAD_META_IN_DATA;
    }
    // Check for flags
    char* data_chk = data[0];
    if (flags & HEAD_TEMPORARY)
      data_chk += size_head;
    if (strcmp(data_chk, YGG_MSG_EOF) == 0)
      flags |= HEAD_FLAG_EOF;
    else if (strcmp(data_chk, YGG_CLIENT_EOF) == 0)
      flags |= HEAD_FLAG_CLIENT_EOF;
    if (size_curr < size_data)
      flags |= HEAD_FLAG_MULTIPART;
    else
      flags &= ~HEAD_FLAG_MULTIPART;
    if ((!(flags & HEAD_TEMPORARY)) && ((size_data + 1) > size_buff)) {
      if (allow_realloc) {
	char *t_data = (char*)realloc(*data, size_data + 1);
	if (t_data == NULL) {
	  ygglog_throw_error_c("Header::for_recv: Failed to realloc buffer");
	}
	data[0] = t_data;
      } else {
	ygglog_throw_error_c("Header::for_recv: Buffer is not large enough");
      }
    }
  }

  void formatBuffer(rapidjson::StringBuffer& buffer, bool metaOnly=true) {
    buffer.Clear();
    if (empty()) {
      ygglog_debug_c("Header::formatBuffer: Empty metadata");
      return;
    }
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
    if (metaOnly) {
      if (metadata.HasMember("__meta__")) {
	writer.StartObject();
	writer.Key("__meta__", 8, true);
	metadata["__meta__"].Accept(writer);
	writer.EndObject(1);
      }
    } else if (GetMetaBoolOptional("in_data", false)) {
      bool hasMeta = metadata.HasMember("__meta__");
      rapidjson::Value tmp;
      if (hasMeta) {
	tmp.Swap(metadata["__meta__"]);
	metadata.RemoveMember("__meta__");
      }
      metadata.Accept(writer);
      if (hasMeta) {
	metadata.AddMember(rapidjson::Value("__meta__", 8).Move(),
			   tmp, GetAllocator());
      }
    } else {
      // rapidjson::Value tmp;
      // if (noType && !metadata.HasMember("serializer"))
      // 	noType = false;
      // if (noType) {
      // 	tmp.Swap(metadata["serializer"]);
      // 	metadata.RemoveMember("serializer");
      // }
      metadata.Accept(writer);
      // if (noType) {
      // 	metadata.AddMember("serializer", tmp, metadata.GetAllocator());
      // 	if (schema != NULL)
      // 	  schema = &(metadata["serializer"]["datatype"]);
      // }
    }
  }

  size_t format(const char* buf, size_t buf_siz,
		size_t size_max, bool metaOnly=false) {
    flags |= (HEAD_FLAG_ALLOW_REALLOC | HEAD_FLAG_OWNSDATA);
    if (strcmp(buf, YGG_MSG_EOF) == 0) {
      flags |= HEAD_FLAG_EOF;
      metaOnly = true;
    }
    data = &data_;
    size_data = buf_siz;
    SetMetaUint("size", buf_siz);
    rapidjson::StringBuffer buffer;
    formatBuffer(buffer, metaOnly);
    rapidjson::StringBuffer buffer_body;
    if (buffer.GetLength() == 0) {
      ygglog_debug_c("Header::format: Empty header");
      return 0;
    }
    size_t size_sep = strlen(MSG_HEAD_SEP);
    size_t size_new = static_cast<size_t>(buffer.GetLength()) + 2 * size_sep;
    if (size_max > 0 && size_new > size_max) {
      if (metaOnly)
	ygglog_throw_error_c("Header::format: Extra data already excluded, cannot make header any smaller.");
      flags |= HEAD_META_IN_DATA;
      SetMetaBool("in_data", true);
      formatBuffer(buffer_body);
      size_data += size_sep + static_cast<size_t>(buffer_body.GetLength());
      SetMetaUint("size", size_data);
      formatBuffer(buffer, true);
      size_new = ((3 * size_sep) +
		  static_cast<size_t>(buffer.GetLength()) +
		  static_cast<size_t>(buffer_body.GetLength()));
    }
    size_new += buf_siz;
    if (size_max > 0 && size_new > size_max &&
	(!(flags & HEAD_FLAG_MULTIPART))) {
      // Early return since comm needs to add to header
      flags |= HEAD_FLAG_MULTIPART;
      return 0;
    }
    if ((size_new + 1) > size_buff) {
      size_buff = size_new + 1;
      data[0] = (char*)realloc(data[0], size_buff);
    }
    int ret;
    if (GetMetaBoolOptional("in_data", false)) {
      ret = snprintf(data[0], size_buff, "%s%s%s%s%s", MSG_HEAD_SEP,
		     buffer.GetString(), MSG_HEAD_SEP,
		     buffer_body.GetString(), MSG_HEAD_SEP);
    } else {
      ret = snprintf(data[0], size_buff, "%s%s%s", MSG_HEAD_SEP,
		     buffer.GetString(), MSG_HEAD_SEP);
    }
    if (((size_t)(ret) + buf_siz) > size_buff)
      ygglog_throw_error_c("Header::format: Message size (%d) exceeds buffer size (%lu): '%s%s%s'.",
			 ret, size_buff, MSG_HEAD_SEP, buffer.GetString(), MSG_HEAD_SEP);
    size_curr = static_cast<size_t>(ret);
    memcpy(data[0] + size_curr, buf, buf_siz);
    size_curr += buf_siz;
    data[0][size_curr] = '\0';
    return size_curr;
  }

  void finalize_recv() {
    if (!GetMetaBoolOptional("in_data", false))
      return;
    size_t sind, eind;
    int ret = find_match_c(MSG_HEAD_SEP, *data, &sind, &eind);
    if (ret < 0)
      ygglog_throw_error_c("Header::finalize_recv: Error locating head separation tag.");
    rapidjson::Document type_doc;
    type_doc.Parse(*data, sind);
    if (type_doc.HasParseError())
      ygglog_throw_error_c("Header::finalize_recv: Error parsing datatype in data");
    fromSchema(type_doc);
    data[0] += eind;
  }
  
  char* data_;
  char** data;
  size_t size_data;
  size_t size_buff;
  size_t size_curr;
  size_t size_head;
  uint16_t flags;
};



#endif /* YGGDRASIL_SERIALIZATION_H_ */
// Local Variables:
// mode: c++
// End:
