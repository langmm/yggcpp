// Import arrays once
// #define RAPIDJSON_FORCE_IMPORT_ARRAY
#include "serialization.hpp"
#include "../communicators/CommBase.hpp"
// Required so that symbol declared by numpy/arrayobject.h is defined
// during compilation of the dynamic library with MSVC
// #ifndef RAPIDJSON_YGGDRASIL_PYTHON
#ifndef RAPIDJSON_FORCE_IMPORT_ARRAY
extern "C" {
  void** rapidjson_ARRAY_API = NULL;
}
#endif // RAPIDJSON_FORCE_IMPORT_ARRAY
// #endif // RAPIDJSON_YGGDRASIL_PYTHON
using namespace communication::utils;

int communication::utils::split_head_body(const char *buf,
					  const char **head,
					  size_t *headsiz) {
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
  if (ret == 0) {
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
  //   ygglog_error << "split_head_body: Failed to reallocate header." << std::endl;
  //   return -1;
  // }
  // *head = temp;
  // memcpy(*head, buf + sind_head, *headsiz);
  // (*head)[*headsiz] = '\0';
  return 0;
}

template <typename ValueT>
std::string communication::utils::document2string(ValueT& rhs,
						  const char* indent) {
  rapidjson::StringBuffer sb;
  rapidjson::PrettyWriter<rapidjson::StringBuffer> writer(sb, 0, strlen(indent));
  writer.SetYggdrasilMode(true);
  rhs.Accept(writer);
  return std::string(sb.GetString());
}

long communication::utils::copyData(char*& dst, const size_t dst_len,
				    const char* src, const size_t src_len,
				    bool allow_realloc) {
  if ((src_len + 1) > dst_len) {
    if (!allow_realloc) {
      ygglog_error << "copyData: Size of message (" <<
	src_len << " + 1 bytes) exceeds buffer size (" << dst_len <<
	" bytes) and the buffer cannot be reallocated." << std::endl;
      return -((long)src_len);
    }
    char* tmp = (char*)realloc(dst, src_len + 1);
    if (tmp == NULL) {
      ygglog_error <<
	"CommBase::copyData: Error reallocating buffer" << std::endl;
      return -1;
    }
    dst = tmp;
  }
  if (src) {
    memcpy(dst, src, src_len);
    dst[src_len] = '\0';
  }
  return (long)src_len;
}

//////////////
// Metadata //
//////////////

Metadata::Metadata() :
  metadata(rapidjson::kObjectType), schema(NULL) {}
bool Metadata::_init(bool use_generic) {
  if (!Normalize())
    return false;
  _update_schema();
  if (use_generic)
    return setGeneric();
  // ygglog_debug << "Metadata::init: metadata = " << metadata << std::endl;
  return true;
}
// Metadata::Metadata(Metadata& rhs) :
//   metadata(rapidjson::kObjectType), schema(NULL) {
//   *this = rhs;
//   metadata = rhs.metadata;
//   _update_schema();
//   rhs._update_schema();
// }
#if RAPIDJSON_HAS_CXX11_RVALUE_REFS
Metadata::Metadata(Metadata&& rhs) :
  metadata(), schema(NULL) {
  metadata.Swap(rhs.metadata);
  _update_schema();
  rhs._update_schema();
}
Metadata& Metadata::operator=(Metadata&& rhs) {
  return *this = rhs.Move();
}
#endif // RAPIDJSON_HAS_CXX11_RVALUE_REFS
Metadata& Metadata::operator=(Metadata& rhs) {
  metadata.Swap(rhs.metadata);
  _update_schema();
  rhs._update_schema();
  return *this;
}
bool Metadata::operator==(const Metadata& rhs) const {
  return (metadata == rhs.metadata);
}
bool Metadata::operator!=(const Metadata& rhs) const {
  return (!(*this == rhs));
}
void Metadata::reset() {
  schema = NULL;
  metadata.SetObject();
}
bool Metadata::fromSchema(const rapidjson::Value& new_schema,
			  bool isMetadata, bool use_generic) {
  if (isMetadata) {
    metadata.CopyFrom(new_schema, metadata.GetAllocator(), true);
    return _init(use_generic);
  } else if (!hasType()) {
    if (!use_generic)
      use_generic = isGeneric();
    initSchema();
    schema->CopyFrom(new_schema, metadata.GetAllocator(), true);
    return _init(use_generic);
  } else {
    rapidjson::SchemaDocument sd_old(*schema);
    rapidjson::SchemaNormalizer n(sd_old);
    if (!n.Compare(new_schema)) {
      std::string s_old_str = document2string(*schema);
      std::string s_new_str = document2string(new_schema);
      rapidjson::Value err;
      n.GetErrorMsg(err, metadata.GetAllocator());
      std::string err_str = document2string(err);
      ygglog_debug << "Schemas incompatible:" << std::endl <<
	"old:" << std::endl << s_old_str << std::endl <<
	"new:" << std::endl << s_new_str << std::endl <<
	"error:" << std::endl << err_str << std::endl;
      return false;
    }
  }
  return true;
}
bool Metadata::Normalize() {
  rapidjson::Document s(rapidjson::kObjectType);
#define ADD_OBJECT_(x, name, len)					\
  x.AddMember(rapidjson::Value("type", 4, s.GetAllocator()).Move(),	\
	      rapidjson::Value("object", 6, s.GetAllocator()).Move(),	\
	      s.GetAllocator());					\
  x.AddMember(rapidjson::Value("properties", 10,			\
			       s.GetAllocator()).Move(),		\
	      rapidjson::Value(rapidjson::kObjectType).Move(),		\
	      s.GetAllocator());					\
  x["properties"].AddMember(rapidjson::Value(#name, len,		\
					     s.GetAllocator()).Move(),	\
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
    ygglog_error << "Metdata::Normalize: Failed to normalize schema:" << std::endl << document2string(metadata) << std::endl << "error =" << std::endl << sb.GetString() << std::endl;
    return false;
  }
  return true;
}

bool Metadata::fromSchema(const std::string schemaStr, bool use_generic) {
  rapidjson::Document d;
  d.Parse(schemaStr.c_str());
  if (d.HasParseError()) {
    ygglog_error << "Metadata::fromSchema: Error parsing string: " << schemaStr << std::endl;
    return false;
  }
  if (!fromSchema(d, false, use_generic))
    return false;
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
      return setGeneric();
  }
  return true;
}
bool Metadata::fromData(const rapidjson::Document& data, bool indirect) {
  rapidjson::SchemaEncoder encoder(true);
  data.Accept(encoder);
  bool has_type = hasType();
  if (!fromSchema(encoder.GetSchema()))
    return false;
  if ((!has_type) && indirect)
    return SetSchemaBool("allowWrapped", true);
  return true;
}
bool Metadata::fromType(const std::string type, bool use_generic,
			bool dont_init) {
  initSchema();
  if (!SetSchemaString("type", type))
    return false;
  if (!dont_init)
    return _init(use_generic);
  return true;
}
bool Metadata::fromScalar(const std::string subtype, size_t precision,
			  const char* units, bool use_generic) {
  if (!fromType("scalar", use_generic, true))
    return false;
  return _fromNDArray(subtype, precision, 0, NULL, units, use_generic);
}
bool Metadata::fromNDArray(const std::string subtype, size_t precision,
			   const size_t ndim, const size_t* shape,
			   const char* units, bool use_generic) {
  if (!fromType("ndarray", use_generic, true))
    return false;
  return _fromNDArray(subtype, precision, ndim, shape, units, use_generic);
}
bool Metadata::_fromNDArray(const std::string subtype, size_t precision,
			    const size_t ndim, const size_t* shape,
			    const char* units, bool use_generic,
			    rapidjson::Value* subSchema) {
  if (subtype == "bytes") {
    if (!SetSchemaString("subtype", "string", subSchema)) return false;
  } else if (subtype == "unicode") {
    if (!SetSchemaString("subtype", "string", subSchema)) return false;
    if (!SetSchemaString("encoding", "UTF8", subSchema)) return false; // UCS4?
  } else {
    if (!SetSchemaString("subtype", subtype, subSchema)) return false;
  }
  if (precision > 0)
    if (!SetSchemaUint("precision", precision, subSchema)) return false;
  if (ndim > 0) {
    if (shape != NULL) {
      rapidjson::Value shp(rapidjson::kArrayType);
      for (size_t i = 0; i < ndim; i++) {
	shp.PushBack(rapidjson::Value((uint64_t)(shape[i])).Move(),
		     GetAllocator());
      }
      if (!SetSchemaValue("shape", shp, subSchema)) return false;
    } else {
      if (!SetSchemaUint("ndim", ndim, subSchema)) return false;
    }
  }
  if (units && strlen(units) > 0) {
    if (!SetSchemaString("units", units, subSchema)) return false;
  }
  if (subSchema == NULL)
    return _init(use_generic);
  return true;
}
bool Metadata::fromFormat(const std::string& format_str,
			  bool as_array, bool use_generic) {
  initSchema();
  metadata["serializer"].AddMember(
      rapidjson::Value("format_str", 10, GetAllocator()).Move(),
      rapidjson::Value(format_str.c_str(),
		       (rapidjson::SizeType)(format_str.size()),
		       GetAllocator()).Move(),
      GetAllocator());
  if (!SetSchemaString("type", "array"))
    return false;
  rapidjson::Value items(rapidjson::kArrayType);
  ygglog_debug << "Metadata::fromFormat: " << format_str << std::endl;
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
    if (mres == 0) {
      // Make sure its not just a format string with no newline
      mres = find_match_c(re_fmt_eof, format_str.c_str() + beg,
			  &sind, &eind);
      if (mres == 0) {
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
      ygglog_error << "Metadata::fromFormat: Could not parse format string: " << ifmt << std::endl;
      return false;
    }
    ygglog_debug_c("isubtype = %s, iprecision = %lu, ifmt = %s",
		   isubtype, iprecision, ifmt);
    rapidjson::Value item(rapidjson::kObjectType);
    if (!SetString("type", element_type, item))
      return false;
    _fromNDArray(isubtype, iprecision, 0, NULL, NULL, false, &item);
    items.PushBack(item, GetAllocator());
    beg = end;
  }
  rapidjson::SizeType nItems = items.Size();
  if (!SetSchemaValue("items", items))
    return false;
  if (nItems == 1) {
    if (!SetSchemaBool("allowSingular", true))
      return false;
  }
  // if (nItems == 1) {
  //   typename rapidjson::Document::ValueType tmp;
  //   metadata["serializer"]["datatype"].Swap(tmp);
  //   metadata["serializer"]["datatype"].Swap(tmp["items"][0]);
  //   metadata["serializer"].RemoveMember("format_str");
  // }
  return _init(use_generic);
}
bool Metadata::fromMetadata(const Metadata& other, bool use_generic) {
  metadata.CopyFrom(other.metadata, GetAllocator(), true);
  return _init(use_generic);
}
bool Metadata::fromMetadata(const char* head, const size_t headsiz,
			    bool use_generic) {
  metadata.Parse(head, headsiz);
  if (metadata.HasParseError()) {
    ygglog_error << "Metadata::fromMetadata: Error parsing header: " << head << std::endl;
    return false;
  }
  if (!(metadata.IsObject())) {
    ygglog_error << "Metadata::fromMetadata: head document must be an object." << std::endl;
    return false;
  }
  if (!(metadata.HasMember("__meta__"))) {
    ygglog_error << "Metadata::fromMetadata: No __meta__ information in the header." << std::endl;
    return false;
  }
  if (!(metadata["__meta__"].IsObject())) {
    ygglog_error << "Metadata::fromMetadata: __meta__ is not an object." << std::endl;
    return false;
  }
  return _init(use_generic);
}
bool Metadata::fromMetadata(const std::string& head, bool use_generic) {
  return fromMetadata(head.c_str(), head.size(), use_generic);
}
bool Metadata::fromEncode(const rapidjson::Value& document,
			  bool use_generic) {
  rapidjson::SchemaEncoder encoder(true);
  document.Accept(encoder);
  return fromSchema(encoder.GetSchema(), false, use_generic);
}
bool Metadata::fromEncode(PyObject* pyobj, bool use_generic) {
  rapidjson::Value::AllocatorType allocator;
  rapidjson::Value d(pyobj, allocator);
  return fromEncode(d, use_generic);
}
rapidjson::Document::AllocatorType& Metadata::GetAllocator() {
  return metadata.GetAllocator();
}
bool Metadata::isGeneric() const {
  return (schema &&
	  ((schema->HasMember("use_generic") &&
	   (*schema)["use_generic"].IsBool() &&
	   (*schema)["use_generic"].GetBool()) ||
	   strcmp(typeName(), "any") == 0));
}
bool Metadata::isFormatArray() const {
  if (!(metadata.IsObject() && metadata.MemberCount() > 0))
    return -1;
  return (metadata.HasMember("serializer") &&
	  metadata["serializer"].HasMember("format_str"));
}
bool Metadata::setGeneric() {
  initSchema();
  return SetSchemaBool("use_generic", true);
}
bool Metadata::empty() const {
  return ((!metadata.IsObject()) || (metadata.MemberCount() == 0));
}
bool Metadata::hasType() const {
  return (schema && schema->HasMember("type"));
}
bool Metadata::hasSubtype() const {
  if (strcmp(typeName(), "scalar") == 0 ||
      strcmp(typeName(), "ndarray") == 0 ||
      strcmp(typeName(), "1darray") == 0) {
    return schema->HasMember("subtype");
  }
  return false;
}
const char* Metadata::typeName() const {
  if (!hasType())
    return "";
  return (*schema)["type"].GetString();
}
const char* Metadata::subtypeName() const {
  if (!hasSubtype())
    return "";
  return (*schema)["subtype"].GetString();
}
void Metadata::initSchema() {
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
void Metadata::initMeta() {
  if (!metadata.HasMember("__meta__")) {
    rapidjson::Value meta(rapidjson::kObjectType);
    metadata.AddMember(rapidjson::Value("__meta__", 8).Move(),
		       meta, metadata.GetAllocator());
  }
}
bool Metadata::addItem(const Metadata& other,
		       rapidjson::Value* subSchema) {
  if (!subSchema)
    subSchema = schema;
  if (!other.schema) {
    ygglog_error << "Metadata::addItem: item does not have schema" << std::endl;
    return false;
  }
  if (!(subSchema && subSchema->IsObject() &&
	subSchema->HasMember("type") &&
	(*subSchema)["type"] == rapidjson::Document::GetArrayString())) {
    ygglog_error << "Metadata::addItem: schema is not for an array." << std::endl;
    return false;
  }
  if (!subSchema->HasMember("items"))
    subSchema->AddMember("items",
			 rapidjson::Value(rapidjson::kArrayType).Move(),
			 GetAllocator());
  if (!(subSchema &&
	subSchema->HasMember("items") &&
	(*subSchema)["items"].IsArray())) {
    ygglog_error << "Metadata::addItem: schema does not have items array" << std::endl;
    return false;
  }
  rapidjson::Value item;
  item.CopyFrom(*(other.schema), GetAllocator(), true);
  (*subSchema)["items"].PushBack(item, GetAllocator());
  return true;
}
bool Metadata::addMember(const std::string name, const Metadata& other,
			 rapidjson::Value* subSchema) {
  if (!subSchema)
    subSchema = schema;
  if (!other.schema) {
    ygglog_error << "Metadata::addMember: member does not have schema" << std::endl;
    return false;
  }
  if (!(subSchema && subSchema->IsObject() &&
	subSchema->HasMember("type") &&
	(*subSchema)["type"] == rapidjson::Document::GetObjectString())) {
    ygglog_error << "Metadata::addMember: schema is not for an object." << std::endl;
    return false;
  }
  if (!subSchema->HasMember("properties"))
    subSchema->AddMember("properties",
			 rapidjson::Value(rapidjson::kObjectType).Move(),
			 GetAllocator());
  // if (!(subSchema &&
  // 	subSchema->HasMember("properties") &&
  // 	(*subSchema)["properties"].IsObject())) {
  //   ygglog_error << "Metadata::addMember: schema does not have properties" << std::endl;
  //   return false;
  // }
  rapidjson::Value item;
  item.CopyFrom(*(other.schema), GetAllocator(), true);
  if ((*subSchema)["properties"].HasMember(name.c_str())) {
    (*subSchema)["properties"][name.c_str()].Swap(item);
  } else {
    (*subSchema)["properties"].AddMember(
	 rapidjson::Value(name.c_str(),
			  (rapidjson::SizeType)(name.size()),
			  GetAllocator()).Move(),
	 item, GetAllocator());
  }
  return true;
}
rapidjson::Value* Metadata::getMeta() {
  if (!(metadata.IsObject() && metadata.HasMember("__meta__"))) {
    ygglog_error << "getMeta: No __meta__ in metadata" << std::endl;
    return nullptr;
  }
  return &(metadata["__meta__"]);
}
const rapidjson::Value* Metadata::getMeta() const {
  if (!(metadata.IsObject() && metadata.HasMember("__meta__"))) {
    ygglog_error << "getMeta: No __meta__ in metadata" << std::endl;
    return nullptr;
  }
  return &(metadata["__meta__"]);
}
rapidjson::Value* Metadata::getSchema() {
  if (schema == NULL) {
    ygglog_error << "getSchema: No datatype in metadata" << std::endl;
    return nullptr;
  }
  return schema;
}
const rapidjson::Value* Metadata::getSchema() const {
  if (schema == NULL) {
    ygglog_error << "getSchema: No datatype in metadata" << std::endl;
    return nullptr;
  }
  return schema;
}
bool Metadata::SetValue(const std::string name, rapidjson::Value& x,
			rapidjson::Value& subSchema) {
  if (!subSchema.IsObject()) {
    ygglog_error << "Metadata::SetValue: subSchema is not an object" << std::endl;
    return false;
  }
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
#define GET_METHOD_(type_out, method)					\
  bool Metadata::Get ## method(const std::string name,			\
			       type_out& out,				\
			       const rapidjson::Value& subSchema) const { \
    if (!(subSchema.HasMember(name.c_str()))) {				\
      ygglog_error << "Get" << #method << ": No " << name << " information in the schema." << std::endl; \
      return false;							\
    }									\
    if (!(subSchema[name.c_str()].Is ## method())) {			\
      ygglog_error << "Get" << #method << ": " << name << " is not " << #type_out << std::endl; \
      return false;							\
    }									\
    out = subSchema[name.c_str()].Get ## method();			\
    return true;							\
  }									\
  bool Metadata::Get ## method ## Optional(const std::string name,	\
					   type_out& out,		\
					   type_out defV,		\
					   const rapidjson::Value& subSchema \
					   ) const {			\
    if (!(subSchema.HasMember(name.c_str()))) {				\
      out = defV;							\
      return true;							\
    }									\
    if (!(subSchema[name.c_str()].Is ## method())) {			\
      ygglog_error << "Get" << #method << "Optional: " << name << " is not " << #type_out << std::endl; \
      return false;							\
    }									\
    out = subSchema[name.c_str()].Get ## method();			\
    return true;							\
  }									\
  bool Metadata::GetMeta ## method(const std::string name,		\
				   type_out& out) const {		\
    const rapidjson::Value* subSchema = getMeta();			\
    if (!subSchema) return false;					\
    return Get ## method(name, out, *subSchema);			\
  }									\
  bool Metadata::GetMeta ## method ## Optional(const std::string name, \
					       type_out& out,		\
					       type_out defV) const {	\
    if (!(metadata.IsObject() && metadata.HasMember("__meta__"))) {	\
      out = defV;							\
      return true;							\
    }									\
    const rapidjson::Value* subSchema = getMeta();			\
    if (!subSchema) return false;					\
    return Get ## method ## Optional(name, out, defV, *subSchema);	\
  }									\
  bool Metadata::GetSchema ## method(const std::string name,	\
				     type_out& out,			\
				     const rapidjson::Value* subSchema	\
				     ) const {				\
    if (subSchema == NULL) {						\
      subSchema = getSchema();						\
      if (!subSchema) return false;					\
    }									\
    return Get ## method(name, out, *subSchema);			\
  }									\
  bool Metadata::GetSchema ## method ## Optional(const std::string name, \
						 type_out& out,		\
						 type_out defV,		\
						 const rapidjson::Value* subSchema) const { \
    if (subSchema == NULL) {						\
      if (schema == NULL) {						\
	out = defV;							\
	return true;							\
      }									\
      subSchema = getSchema();						\
      if (!subSchema) return false;					\
    }									\
    return Get ## method ## Optional(name, out, defV, *subSchema);	\
  }
#define SET_METHOD_(type_in, method, setargs)				\
  bool Metadata::Set ## method(const std::string name, type_in x,	\
			       rapidjson::Value& subSchema) {		\
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
  bool Metadata::SetMeta ## method(const std::string name, type_in x) {	\
    rapidjson::Value* subSchema = getMeta();				\
    if (!subSchema) return false;					\
    return Set ## method(name, x, *subSchema);				\
  }									\
  bool Metadata::SetSchema ## method(const std::string name, type_in x,	\
				     rapidjson::Value* subSchema) {	\
    if (subSchema == NULL) {						\
      subSchema = getSchema();						\
      if (!subSchema) return false;					\
    }									\
    return Set ## method(name, x, *subSchema);				\
  }
#define GET_SET_METHOD_(type_in, type_out, method, setargs)		\
  GET_METHOD_(type_out, method);					\
  SET_METHOD_(type_in, method, setargs)
GET_SET_METHOD_(int, int, Int, (x));
GET_SET_METHOD_(uint64_t, uint64_t, Uint, (x));
GET_SET_METHOD_(bool, bool, Bool, (x));
GET_SET_METHOD_(const std::string&, const char*, String,
		(x.c_str(), (rapidjson::SizeType)(x.size()),
		 metadata.GetAllocator()));
GET_METHOD_(unsigned, Uint);
GET_METHOD_(int32_t, Uint);
GET_METHOD_(std::string, String);
#undef GET_SET_METHOD_
#undef GET_METHOD_
#undef SET_METHOD_
bool Metadata::SetMetaValue(const std::string name, rapidjson::Value& x) {
  rapidjson::Value* subSchema = getMeta();
  if (!subSchema) return false;
  return SetValue(name, x, *subSchema);
}
bool Metadata::SetSchemaValue(const std::string name, rapidjson::Value& x,
			      rapidjson::Value* subSchema) {
  if (subSchema == NULL) {
    subSchema = getSchema();
    if (!subSchema) return false;
  }
  return SetValue(name, x, *subSchema);
}
bool Metadata::SetSchemaMetadata(const std::string name,
				 const Metadata& other) {
  if (other.schema == NULL) {
    ygglog_error << "SetSchemaMetadata: Value has no datatype" << std::endl;
    return false;
  }
  rapidjson::Value x;
  x.CopyFrom(*(other.schema), GetAllocator(), true);
  return SetSchemaValue(name, x);
}
bool Metadata::SetMetaID(const std::string name, const char** id) {
  char new_id[100];
  snprintf(new_id, 100, "%d", rand());
  if (!SetMetaString(name, new_id))
    return false;
  if (id) {
    return GetMetaString(name, id[0]);
  }
  return true;
}
bool Metadata::SetMetaID(const std::string name, std::string& id) {
  const char* id_str;
  if (!SetMetaID(name, &id_str))
    return false;
  id.assign(id_str);
  return true;
}
int Metadata::deserialize(const char* buf, size_t nargs, int allow_realloc, ...) {
  rapidjson::VarArgList va(nargs, allow_realloc);
  va_start(va.va, allow_realloc);
  int out = deserialize(buf, va);
  if (out >= 0 && va.get_nargs() != 0) {
    ygglog_error_c("Metadata::deserialize: %ld of the arguments were not used", va.get_nargs());
    return -1;
  }
  return out;
}
int Metadata::deserialize(const char* buf, rapidjson::VarArgList& ap) {
  size_t nargs_orig = ap.get_nargs();
  rapidjson::Document d;
  rapidjson::StringStream s(buf);
  d.ParseStream(s);
  if (d.HasParseError()) {
    ygglog_error << "Metadata::deserialize: Error parsing JSON" << std::endl;
    return -1;
  }
  ygglog_debug << "Metadata::deserialize: " << d << std::endl;
  if (!hasType()) {
    fromData(d);
  } else {
    rapidjson::StringBuffer sb;
    if (!d.Normalize(*schema, &sb)) {
      std::string d_str = document2string(d);
      std::string s_str = document2string(*schema);
      ygglog_error << "Metadata::deserialize: Error normalizing document:" << std::endl << sb.GetString() << std::endl << "document=" << d_str << std::endl << "schema=" << s_str << std::endl << "message=" << buf << "..." << std::endl;
      return -1;
    }
  }
  ygglog_debug << "Metadata::deserialize: before SetVarArgs: " << *schema << std::endl;
  if (!d.SetVarArgs(*schema, ap)) {
    ygglog_error << "Metadata::deserialize: Error setting arguments from JSON document" << std::endl;
    return -1;
  }
  return (int)(nargs_orig - ap.get_nargs());
}
int Metadata::serialize(char **buf, size_t *buf_siz, size_t nargs, ...) {
  rapidjson::VarArgList va(nargs);
  va_start(va.va, nargs);
  int out = serialize(buf, buf_siz, va);
  if (out >= 0 && va.get_nargs() != 0) {
    ygglog_error << "Metadata::serialize: " << va.get_nargs() << " of the arguments were not used" << std::endl;
    return -1;
  }
  return out;
}
int Metadata::serialize(char **buf, size_t *buf_siz,
			rapidjson::VarArgList& ap) {
  if (!hasType()) {
    ygglog_error << "Metadata::serialize: No datatype" << std::endl;
    return -1;
  }
  rapidjson::Document d;
  if (!d.GetVarArgs(*schema, ap)) {
    std::string s_str = document2string(*schema);
    ygglog_error << "Metadata::serialize: Error creating JSON document from arguments for schema =" << s_str << std::endl;
    return -1;
  }
  rapidjson::StringBuffer buffer;
  rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
  d.Accept(writer);
  if ((size_t)(buffer.GetLength() + 1) > buf_siz[0]) {
    char* buf_t = (char*)(GetAllocator().Realloc(buf[0], buf_siz[0],
						 (size_t)(buffer.GetLength() + 1)));
    if (buf_t == NULL) {
      ygglog_error << "Metadata::serialize: Error in realloc" << std::endl;
      return -1;
    }
    buf_siz[0] = (size_t)(buffer.GetLength() + 1);
    buf[0] = buf_t;
  }
  memcpy(buf[0], buffer.GetString(), (size_t)(buffer.GetLength()));
  buf[0][(size_t)(buffer.GetLength())] = '\0';
  return static_cast<int>(buffer.GetLength());
}
void Metadata::Display(const char* indent) const {
  std::cout << document2string(metadata, indent) << std::endl;
}
void Metadata::_update_schema() {
  if (metadata.HasMember("serializer") &&
      metadata["serializer"].IsObject() &&
      metadata["serializer"].HasMember("datatype") &&
      metadata["serializer"]["datatype"].IsObject()) {
    schema = &(metadata["serializer"]["datatype"]);
  } else {
    schema = NULL;
  }
}

////////////
// Header //
////////////

Header::Header(bool own_data) :
  data_(NULL), data(NULL),
  size_data(0), size_buff(0), size_curr(0),
  size_head(0), size_max(0), size_msg(0),
  flags(HEAD_FLAG_VALID), offset(0) {
  if (own_data) {
    data = &data_;
    flags |= HEAD_BUFFER_MASK;
  }
}
Header::Header(const char* buf, const size_t &len,
	       communication::communicator::Comm_t* comm) :
  Header() {
  Metadata* meta = NULL;
  int comm_flags = 0;
  if (comm) {
    size_max = comm->getMaxMsgSize() - comm->getMsgBufSize();
    meta = &(comm->getMetadata(SEND));
    comm_flags = comm->getFlags();
  }
  if (!for_send(meta, buf, len, comm_flags)) {
    // TODO: Raise?
    flags &= ~HEAD_FLAG_VALID;
  }
}
Header::Header(char*& buf, const size_t &len, bool allow_realloc) :
  Header() {
  for_recv(buf, len, allow_realloc);
}
Header::~Header() {
  reset();
}
#if RAPIDJSON_HAS_CXX11_RVALUE_REFS
Header::Header(Header&& rhs) :
  Metadata(std::forward<Metadata>(rhs)),
  data_(NULL), data(NULL),
  size_data(0), size_buff(0), size_curr(0),
  size_head(0), size_max(0), size_msg(0),
  flags(HEAD_FLAG_VALID), offset(0) {
  RawAssign(rhs);
  rhs.reset(HEAD_RESET_DROP_DATA);
}
Header& Header::operator=(Header&& rhs) {
  return *this = rhs.Move();
}
#endif // RAPIDJSON_HAS_CXX11_RVALUE_REFS
Header& Header::operator=(Header& rhs) {
  if (data && !(flags & HEAD_FLAG_OWNSDATA))
    ygglog_debug << "Header::operator=: Supplied buffer will be displaced by move" << std::endl;
  reset();
  Metadata::operator=(std::forward<Metadata>(rhs));
  RawAssign(rhs);
  rhs.reset(HEAD_RESET_DROP_DATA);
  return *this;
}
bool Header::operator==(const Header& rhs) const {
  if (!Metadata::operator==(rhs))
    return false;
  if (!(size_data == rhs.size_data &&
	size_buff == rhs.size_buff &&
	size_curr == rhs.size_curr &&
	size_head == rhs.size_head &&
	size_max == rhs.size_max &&
	size_msg == rhs.size_msg &&
	flags == rhs.flags &&
	offset == rhs.offset))
    return false;
  if ((!data) || (!rhs.data))
    return (data == rhs.data);
  if ((!data[0]) || (!rhs.data[0]))
    return (data[0] == rhs.data[0]);
  return (strcmp(data[0], rhs.data[0]) == 0);
}
bool Header::operator!=(const Header& rhs) const {
  return (!(*this == rhs));
}
void Header::reset(HEAD_RESET_MODE mode) {
  Metadata::reset();
  int keep_flags = 0;
  if (mode == HEAD_RESET_KEEP_BUFFER) {
    keep_flags = (flags & HEAD_BUFFER_MASK);
  } else {
    if (mode != HEAD_RESET_DROP_DATA) {
      if ((flags & HEAD_FLAG_OWNSDATA) && data_) {
	free(data_);
      }
      data_ = NULL;
      data = NULL;
    }
    data = NULL;
    data_ = NULL;
    size_buff = 0;
    size_max = 0;
    if (mode == HEAD_RESET_OWN_DATA) {
      keep_flags = HEAD_BUFFER_MASK;
      data = &data_;
    }
  }
  size_data = 0;
  size_curr = 0;
  size_head = 0;
  size_msg = 0;
  flags = HEAD_FLAG_VALID | keep_flags;
  offset = 0;
}
bool Header::RawAssign(const Header& rhs, bool keep_buffer) {
  if (keep_buffer) {
    offset = 0;
    if (rhs.data && rhs.size_buff &&
	copyData(rhs.data[0], rhs.size_buff - 1) < 0)
      return false;
    flags = (flags & HEAD_BUFFER_MASK) | (rhs.flags & ~HEAD_BUFFER_MASK);
  } else {
    if (rhs.flags & HEAD_FLAG_OWNSDATA) {
      data_ = rhs.data_;
      data = &data_;
    } else {
      data = rhs.data;
      data_ = NULL;
    }
    size_buff = rhs.size_buff;
    flags = rhs.flags;
  }
  size_data = rhs.size_data;
  size_curr = rhs.size_curr;
  size_head = rhs.size_head;
  size_max = rhs.size_max;
  size_msg = rhs.size_msg;
  offset = rhs.offset;
  return true;
}
long Header::reallocData(const size_t size_new) {
  if ((size_new + 1) <= size_buff)
    return size_buff;
  if (!(flags & HEAD_FLAG_ALLOW_REALLOC)) {
    ygglog_error << "Header::reallocData: Buffer is not large enough and cannot be reallocated" << std::endl;
    return -1;
  }
  size_buff = size_new + 1;
  char* data_t = (char*)realloc(data[0], size_buff);
  if (!data_t) {
    ygglog_error << "Header::reallocData: Error in realloc" << std::endl;
    return -1;
  }
  data[0] = data_t;
  if (flags & HEAD_FLAG_OWNSDATA)
    data_ = data_t;
  return size_buff;
}

long Header::copyData(const char* msg, const size_t msg_siz) {
  long ret = 0;
  char* dst = data_msg();
  if (msg && dst == msg) {
    ret = static_cast<long>(msg_siz);
  } else {
    bool allow_realloc = ((flags & HEAD_FLAG_ALLOW_REALLOC) &&
			  (offset == 0));
    ret = communication::utils::copyData(dst, size_buff - offset,
					 msg, msg_siz, allow_realloc);
    if (allow_realloc && ((ret + 1) > static_cast<long>(size_buff))) {
      data[0] = dst;
      if (flags & HEAD_FLAG_OWNSDATA)
	data_ = dst;
      size_buff = static_cast<size_t>(ret + 1);
    }
  }
  return ret;
}
bool Header::MoveFrom(Header& rhs) {
  if (data && !(flags & HEAD_FLAG_OWNSDATA)) {
    bool out = CopyFrom(rhs);
    rhs.reset(HEAD_RESET_COMPLETE); // HEAD_RESET_KEEP_BUFFER); // can be reused
    return out;
  }
  *this = std::move(rhs);
  return true;
}
bool Header::CopyFrom(const Header& rhs) {
  reset(HEAD_RESET_KEEP_BUFFER);
  if (!Metadata::CopyFrom(rhs))
    return false;
  return RawAssign(rhs, true);
}

void Header::setMessageFlags(const char* msg, const size_t msg_len) {
  if (msg_len == 0)
    return;
  if (strcmp(msg, YGG_MSG_EOF) == 0)
    flags |= HEAD_FLAG_EOF;
  else if (strcmp(msg, YGG_CLIENT_EOF) == 0)
    flags |= HEAD_FLAG_CLIENT_EOF;
  else if (strncmp(msg, YGG_CLIENT_SIGNON, YGG_CLIENT_SIGNON_LEN) == 0)
    flags |= HEAD_FLAG_CLIENT_SIGNON;
  else if (strncmp(msg, YGG_SERVER_SIGNON, YGG_SERVER_SIGNON_LEN) == 0)
    flags |= HEAD_FLAG_SERVER_SIGNON;
}

bool Header::for_send(Metadata* metadata0, const char* msg,
		      const size_t len, int comm_flags) {
  ygglog_debug << "Header::for_send: " << len << " bytes" << std::endl;
  flags |= HEAD_BUFFER_MASK;
  size_buff = len + 1;
  size_data = len;
  size_curr = len;
  data_ = (char*)malloc(size_buff);
  memcpy(data_, msg, size_data);
  data_[size_data] = '\0';
  data = &data_;
  setMessageFlags(msg, len);
  if ((flags & HEAD_FLAG_EOF) || (comm_flags & COMM_FLAGS_USED_SENT)) {
    flags |= HEAD_FLAG_NO_TYPE;
    if ((size_max == 0 || len < size_max) &&
	!(comm_flags & COMM_ALWAYS_SEND_HEADER)) {
      flags |= HEAD_FLAG_NO_HEAD;
      return true;
    }
  }
  if (metadata0 != NULL && !(flags & (HEAD_FLAG_CLIENT_SIGNON |
				      HEAD_FLAG_SERVER_SIGNON))) {
    if (!fromMetadata(*metadata0)) return false;
  }
  initMeta();
  if (!SetMetaID("id"))
    return false;
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
  return SetMetaString("model", model);
}
int Header::on_send(bool dont_advance) {
  ygglog_debug << "Header::on_send: " << size_curr << std::endl;
  if (format() < 0)
    return -1;
  if (!((flags & HEAD_FLAG_ASYNC) || dont_advance)) {
    offset += size_msg;
    size_msg = std::min(size_curr - offset, size_max);
  }
  ygglog_debug << "Header::on_send: size_msg = " << size_msg << std::endl;
  return size_curr;
}

void Header::for_recv(char*& buf, size_t buf_siz, bool allow_realloc) {
  data = &buf;
  size_buff = buf_siz;
  if (allow_realloc)
    flags |= HEAD_FLAG_ALLOW_REALLOC;
}
long Header::on_recv(const char* msg, const size_t& msg_siz) {
  ygglog_debug << "Header::on_recv: " << msg_siz << std::endl;
  bool no_offset = (offset == 0);
  long ret = copyData(msg, msg_siz);
  if (ret >= 0 && msg) {
    size_curr += msg_siz;
    offset += msg_siz;
  }
  if (ret < 0 || !no_offset || !msg) {
    return ret;
  }
  const char *head = NULL;
  size_t headsiz = 0;
  if (split_head_body(*data, &head, &headsiz) < 0)
    return -1;
  if (headsiz == 0) {
    size_data = size_curr;
  } else {
    if (!fromMetadata(head, headsiz))
      return -1;
    size_head = headsiz + 2*strlen(MSG_HEAD_SEP);
    if (size_head > msg_siz) {
      ygglog_error << "Header::on_recv: Header (" << size_head <<
	") is larger than message (" << msg_siz << ")" << std::endl;
      return -1;
    }
    // Move body to front of buffer
    size_curr -= size_head;
    memmove(data[0], data[0] + size_head, size_curr);
    (*data)[size_curr] = '\0';
    // Update parameters from document
    try {
      int size_data_int = 0;
      bool in_data = false;
      if (!(GetMetaInt("size", size_data_int) &&
	    GetMetaBoolOptional("in_data", in_data, false)))
	return -1;
      size_data = static_cast<size_t>(size_data_int);
      if (in_data)
	flags |= HEAD_META_IN_DATA;
      else
	flags &= static_cast<uint16_t>(~HEAD_META_IN_DATA);
    } catch (...) {
      return -1;
    }
  }
  // Check for flags
  setMessageFlags(data[0], size_curr);
  if (size_curr < size_data)
    flags |= HEAD_FLAG_MULTIPART;
  else
    flags &= static_cast<uint16_t>(~HEAD_FLAG_MULTIPART);
  if (reallocData(size_data) < 0) {
    return -1;
  }
  ygglog_debug << "Header::on_recv: done (size_buff = " << size_buff << ")" << std::endl;
  return ret;
}

bool Header::formatBuffer(rapidjson::StringBuffer& buffer, bool metaOnly) {
  buffer.Clear();
  if (empty()) {
    ygglog_debug_c("Header::formatBuffer: Empty metadata");
    return true;
  }
  rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
  bool in_data = false;
  if (!GetMetaBoolOptional("in_data", in_data, false))
    return false;
  if (metaOnly) {
    if (metadata.HasMember("__meta__")) {
      writer.StartObject();
      writer.Key("__meta__", 8, true);
      metadata["__meta__"].Accept(writer);
      writer.EndObject(1);
    }
  } else if (in_data) {
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
  return true;
}
void Header::Display(const char* indent) const {
  Metadata::Display(indent);
  std::cout << indent;
  std::cout << "DATA_PTR = ";
  if (data)
    std::cout << data;
  else
    std::cout << "NULL";
  std::cout << ", DATA = ";
  if (data && data[0] && size_curr) {
    // std::string sData(data[0], size_curr);
    std::cout << data[0];
  } else {
    std::cout << "NULL";
  }
  std::cout << ", FLAGS = " << flags;
  std::cout << std::endl;
}

int Header::format() {
  if (flags & HEAD_FLAG_FORMATTED) {
    return size_curr;
  } else if (flags & HEAD_FLAG_NO_HEAD) {
    size_curr = size_data;
    flags |= HEAD_FLAG_FORMATTED;
    return size_curr;
  }
  flags |= HEAD_BUFFER_MASK;
  data = &data_;
  if (size_data == 0 && empty()) {
    ygglog_debug_c("Header::format: Empty header");
    return 0;
  }
  bool metaOnly = (flags & (HEAD_FLAG_NO_TYPE | HEAD_META_IN_DATA |
			    HEAD_FLAG_CLIENT_SIGNON |
			    HEAD_FLAG_SERVER_SIGNON));
  size_t size_raw = size_data;
  rapidjson::StringBuffer buffer_body;
  std::string sep(MSG_HEAD_SEP);
  if (flags & HEAD_META_IN_DATA) {
    if (!SetMetaBool("in_data", true))
      return -1;
    if (!formatBuffer(buffer_body))
      return -1;
    size_data += sep.size() + static_cast<size_t>(buffer_body.GetLength());
  }
  if (!SetMetaUint("size", size_data))
    return -1;
  rapidjson::StringBuffer buffer;
  if (!formatBuffer(buffer, metaOnly))
    return -1;
  size_t size_head = static_cast<size_t>(buffer.GetLength()) + 2 * sep.size();
  size_t size_new = size_head + size_data;
  if (size_max > 0 && size_new > size_max &&
      (!(flags & HEAD_FLAG_MULTIPART))) {
    // Early return since comm needs to add to header
    flags |= HEAD_FLAG_MULTIPART;
    flags &= ~HEAD_FLAG_FORMATTED;
    if (size_head > size_max) {
      if (metaOnly) {
	ygglog_error << "Header::format: Extra data already excluded, cannot make header any smaller." << std::endl;
	return -1;
      }
      flags |= HEAD_META_IN_DATA;
    }
    return 0;
  }
  if (reallocData(size_new) < 0) {
    return -1;
  }
  memmove(data[0] + static_cast<long>(size_new - size_raw),
	  data[0], size_raw);
  size_curr = 0;
  memcpy(data[0] + size_curr, sep.c_str(), sep.size());
  size_curr += sep.size();
  
  memcpy(data[0] + size_curr, buffer.GetString(), buffer.GetLength());
  size_curr += buffer.GetLength();
  
  memcpy(data[0] + size_curr, sep.c_str(), sep.size());
  size_curr += sep.size();

  bool in_data = false;
  if (!GetMetaBoolOptional("in_data", in_data, false))
    return -1;
  if (in_data) {
    memcpy(data[0] + size_curr, buffer_body.GetString(),
	   buffer_body.GetLength());
    size_curr += buffer_body.GetLength();
    memcpy(data[0] + size_curr, sep.c_str(), sep.size());
    size_curr += sep.size();
  }
  size_curr += size_raw;
  data[0][size_curr] = '\0';
  flags |= HEAD_FLAG_FORMATTED;
  return size_curr;
}

bool Header::finalize_recv() {
  bool in_data = false;
  if (!GetMetaBoolOptional("in_data", in_data, false))
    return false;
  if (!in_data)
    return true;
  ygglog_debug << "Header::finalize_recv: begin" << std::endl;
  size_t sind, eind;
  if (find_match_c(MSG_HEAD_SEP, *data, &sind, &eind) > 0) {
    rapidjson::Document type_doc;
    type_doc.Parse(*data, sind);
    if (type_doc.HasParseError()) {
      ygglog_error << "Header::finalize_recv: Error parsing datatype in data" << std::endl;
      return false;
    }
    if (!fromSchema(type_doc))
      return false;
    size_curr -= eind;
    size_data -= eind;
    memmove(data[0], data[0] + eind, size_curr);
    (*data)[size_curr] = '\0';
  }
  ygglog_debug << "Header::finalize_recv: end" << std::endl;
  return true;
}
