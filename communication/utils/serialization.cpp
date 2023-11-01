#include "serialization.hpp"
#include "../communicators/CommBase.hpp"
using namespace communication::utils;

bool communication::utils::numpy_arrays_imported() {
  bool out = false;
#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
  if (rapidjson_ARRAY_API)
    out = true;
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
  return out;
}

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
    YggLogDebug << "split_head_body: No header in '" <<
      std::string(buf).substr(0, 1000) << "...'" << std::endl;
  } else {
    sind_head = sind + strlen(MSG_HEAD_SEP);
    eind_head = eind - strlen(MSG_HEAD_SEP);
    headsiz[0] = (eind_head - sind_head);
    head[0] = buf + strlen(MSG_HEAD_SEP);
  }
  // char* temp = (char*)realloc(*head, *headsiz + 1);
  // if (temp == NULL) {
  //   YggLogError << "split_head_body: Failed to reallocate header." << std::endl;
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

template <typename T>
long communication::utils::copyData(T*& dst, const size_t dst_len,
				    const T* src, const size_t src_len,
				    bool allow_realloc) {
  if ((src_len + 1) > dst_len) {
    if (!allow_realloc) {
      YggLogError << "copyData: Size of message (" <<
	src_len << " + 1 bytes) exceeds buffer size (" << dst_len <<
	" bytes) and the buffer cannot be reallocated." << std::endl;
      return -((long)src_len);
    }
    T* tmp = (T*)realloc(dst, src_len + 1);
    if (tmp == NULL) {
      YggLogError << "copyData: Error reallocating buffer" << std::endl;
      return -1;
    }
    dst = tmp;
  }
  if (src) {
    memcpy(dst, src, src_len);
    ((char*)dst)[src_len] = '\0';
  }
  return (long)src_len;
}

//////////////
// Metadata //
//////////////

Metadata::Metadata() :
  metadata(rapidjson::kObjectType), raw_schema(NULL),
  filters(), transforms(), skip_last(false) {}
bool Metadata::_init(bool use_generic) {
  if (!Normalize())
    return false;
  if (use_generic)
    return setGeneric();
  // log_debug() << "init: metadata = " << metadata << std::endl;
  return true;
}
// Metadata::Metadata(Metadata& rhs) :
//   metadata(rapidjson::kObjectType),
//   filters(), transforms(), skip_last(false) {
//   *this = rhs;
//   metadata = rhs.metadata;
// }
#if RAPIDJSON_HAS_CXX11_RVALUE_REFS
Metadata::Metadata(Metadata&& rhs) :
  metadata(), raw_schema(NULL),
  filters(), transforms(), skip_last(false) {
  metadata.Swap(rhs.metadata);
  std::swap(raw_schema, rhs.raw_schema);
  filters.swap(rhs.filters);
  transforms.swap(rhs.transforms);
  skip_last = rhs.skip_last;
  rhs.skip_last = false;
}
Metadata& Metadata::operator=(Metadata&& rhs) {
  return *this = rhs.Move();
}
#endif // RAPIDJSON_HAS_CXX11_RVALUE_REFS
Metadata& Metadata::operator=(Metadata& rhs) {
  metadata.Swap(rhs.metadata);
  std::swap(raw_schema, rhs.raw_schema);
  filters.swap(rhs.filters);
  transforms.swap(rhs.transforms);
  skip_last = rhs.skip_last;
  rhs.skip_last = false;
  return *this;
}
bool Metadata::operator==(const Metadata& rhs) const {
  return ((metadata == rhs.metadata) &&
	  (filters == rhs.filters) &&
	  (transforms == rhs.transforms));
}
bool Metadata::operator!=(const Metadata& rhs) const {
  return (!(*this == rhs));
}
bool Metadata::CopyFrom(const Metadata& rhs) {
  metadata.CopyFrom(rhs.metadata, GetAllocator(), true);
  if (rhs.raw_schema) {
    if (!raw_schema) {
      raw_schema = new Metadata();
      if (!raw_schema->CopyFrom(*(rhs.raw_schema)))
	return false;
    }
  }
  return true;
}
std::string Metadata::logInst() const {
  std::string out;
  if (metadata.IsObject() && metadata.HasMember("__meta__") &&
      metadata["__meta__"].IsObject() &&
      metadata["__meta__"].HasMember("id"))
    GetMetaString("id", out);
  return out;
}
void Metadata::resetRawSchema() {
  if (raw_schema) {
    delete raw_schema;
    raw_schema = NULL;
  }
}
void Metadata::reset() {
  metadata.SetObject();
  resetRawSchema();
  filters.clear();
  transforms.clear();
  skip_last = false;
}
bool Metadata::fromSchema(const rapidjson::Value& new_schema,
			  bool isMetadata, bool use_generic) {
  if (isMetadata) {
    metadata.CopyFrom(new_schema, metadata.GetAllocator(), true);
    return _init(use_generic);
  } else if (!hasType()) {
    if (!use_generic)
      use_generic = isGeneric();
    rapidjson::Value* schema = initSchema();
    schema->CopyFrom(new_schema, metadata.GetAllocator(), true);
    return _init(use_generic);
  } else {
    rapidjson::Value* schema = getSchema(true);
    rapidjson::SchemaDocument sd_old(*schema);
    rapidjson::SchemaNormalizer n(sd_old);
    if (!n.Compare(new_schema)) {
      rapidjson::Value err;
      n.GetErrorMsg(err, metadata.GetAllocator());
      log_debug() << "fromSchema: Schemas incompatible:" << std::endl <<
	"old:" << std::endl << *schema << std::endl <<
	"new:" << std::endl << new_schema << std::endl <<
	"error:" << std::endl << err << std::endl;
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
    log_error() << "Normalize: Failed to normalize schema:" <<
      std::endl << metadata << std::endl << "error =" << std::endl <<
      sb.GetString() << std::endl;
    return false;
  }
  return true;
}

bool Metadata::fromSchema(const std::string schemaStr, bool use_generic) {
  rapidjson::Document d;
  d.Parse(schemaStr.c_str());
  if (d.HasParseError()) {
    log_error() << "fromSchema: Error parsing string: " <<
      schemaStr << std::endl;
    return false;
  }
  if (!fromSchema(d, false, use_generic))
    return false;
  if (hasType()) {
    rapidjson::Value* schema = getSchema(true);
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
bool Metadata::fromData(const rapidjson::Document& data,
			bool before_transforms) {
  if (before_transforms) {
    if (!raw_schema) {
      raw_schema = new Metadata();
    }
    return raw_schema->fromData(data);
  }
  rapidjson::SchemaEncoder encoder(!hasType());
  if (!data.Accept(encoder))
    return false;
  return fromSchema(encoder.GetSchema());
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
  log_debug() << "fromFormat: " << format_str << std::endl;
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
      log_error() << "fromFormat: Could not parse format string: " << ifmt << std::endl;
      return false;
    }
    log_debug() << "fromFormat: isubtype = " << isubtype << ", iprecision = " <<
      iprecision << ", ifmt = " << ifmt << std::endl;
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
  filters.clear();
  filters.insert(filters.begin(), other.filters.begin(), other.filters.end());
  transforms.clear();
  transforms.insert(transforms.begin(), other.transforms.begin(), other.transforms.end());
  skip_last = other.skip_last;
  return _init(use_generic);
}
bool Metadata::fromMetadata(const char* head, const size_t headsiz,
			    bool use_generic) {
  metadata.Parse(head, headsiz);
  if (metadata.HasParseError()) {
    log_error() << "fromMetadata: Error parsing header: " << head << std::endl;
    return false;
  }
  if (!(metadata.IsObject())) {
    log_error() << "fromMetadata: head document must be an object." << std::endl;
    return false;
  }
  if (!(metadata.HasMember("__meta__"))) {
    log_error() << "fromMetadata: No __meta__ information in the header." << std::endl;
    return false;
  }
  if (!(metadata["__meta__"].IsObject())) {
    log_error() << "fromMetadata: __meta__ is not an object." << std::endl;
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
void Metadata::addFilter(filterFunc new_filter) {
  filters.push_back(new_filter);
}
void Metadata::addTransform(transformFunc new_transform) {
  transforms.push_back(new_transform);
}
void Metadata::setFilters(std::vector<filterFunc>& new_filters) {
  filters.clear();
  filters.insert(filters.begin(), new_filters.begin(), new_filters.end());
}
void Metadata::setTransforms(std::vector<transformFunc>& new_transforms) {
  transforms.clear();
  transforms.insert(transforms.begin(), new_transforms.begin(), new_transforms.end());
}
rapidjson::Document::AllocatorType& Metadata::GetAllocator() {
  return metadata.GetAllocator();
}
bool Metadata::isGeneric() const {
  const rapidjson::Value* schema = getSchema();
  return (schema &&
	  ((schema->HasMember("use_generic") &&
	   (*schema)["use_generic"].IsBool() &&
	   (*schema)["use_generic"].GetBool()) ||
	   strcmp(typeName(), "any") == 0));
}
int Metadata::isFormatArray() const {
  if (!(metadata.IsObject() && metadata.MemberCount() > 0))
    return -1;
  return (metadata.HasMember("serializer") &&
	  metadata["serializer"].HasMember("format_str"));
}
bool Metadata::setGeneric() {
  initSchema();
  return SetSchemaBool("use_generic", true);
}
bool Metadata::setAllowWrapped() {
  initSchema();
  return SetSchemaBool("allowWrapped", true);
}
bool Metadata::empty() const {
  return ((!metadata.IsObject()) || (metadata.MemberCount() == 0));
}
bool Metadata::hasType() const {
  const rapidjson::Value* schema = getSchema();
  return (schema && schema->HasMember("type"));
}
bool Metadata::hasSubtype() const {
  if (strcmp(typeName(), "scalar") == 0 ||
      strcmp(typeName(), "ndarray") == 0 ||
      strcmp(typeName(), "1darray") == 0) {
    return getSchema(true)->HasMember("subtype");
  }
  return false;
}
const char* Metadata::typeName() const {
  if (!hasType())
    return "";
  return (*getSchema(true))["type"].GetString();
}
const char* Metadata::subtypeName() const {
  if (!hasSubtype())
    return "";
  return (*getSchema(true))["subtype"].GetString();
}
rapidjson::Value* Metadata::initSchema() {
  rapidjson::Value* out = getSchema();
  if (!out) {
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
    out = getSchema(true);
  }
  return out;
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
    subSchema = getSchema(true);
  const rapidjson::Value* other_schema = other.getSchema(true);
  if (!other_schema) {
    log_error() << "addItem: item does not have schema" << std::endl;
    return false;
  }
  if (!(subSchema && subSchema->IsObject() &&
	subSchema->HasMember("type") &&
	(*subSchema)["type"] == rapidjson::Document::GetArrayString())) {
    log_error() << "addItem: schema is not for an array." << std::endl;
    return false;
  }
  if (!subSchema->HasMember("items"))
    subSchema->AddMember("items",
			 rapidjson::Value(rapidjson::kArrayType).Move(),
			 GetAllocator());
  if (!(subSchema &&
	subSchema->HasMember("items") &&
	(*subSchema)["items"].IsArray())) {
    log_error() << "addItem: schema does not have items array" << std::endl;
    return false;
  }
  rapidjson::Value item;
  item.CopyFrom(*other_schema, GetAllocator(), true);
  (*subSchema)["items"].PushBack(item, GetAllocator());
  return true;
}
bool Metadata::addMember(const std::string name, const Metadata& other,
			 rapidjson::Value* subSchema) {
  if (!subSchema)
    subSchema = getSchema(true);
  if (!other.getSchema(true)) {
    log_error() << "addMember: member does not have schema" << std::endl;
    return false;
  }
  if (!(subSchema && subSchema->IsObject() &&
	subSchema->HasMember("type") &&
	(*subSchema)["type"] == rapidjson::Document::GetObjectString())) {
    log_error() << "addMember: schema is not for an object." << std::endl;
    return false;
  }
  if (!subSchema->HasMember("properties"))
    subSchema->AddMember("properties",
			 rapidjson::Value(rapidjson::kObjectType).Move(),
			 GetAllocator());
  // if (!(subSchema &&
  // 	subSchema->HasMember("properties") &&
  // 	(*subSchema)["properties"].IsObject())) {
  //   log_error() << "addMember: schema does not have properties" << std::endl;
  //   return false;
  // }
  rapidjson::Value item;
  item.CopyFrom(*(other.getSchema(true)), GetAllocator(), true);
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
    log_error() << "getMeta: No __meta__ in metadata" << std::endl;
    return nullptr;
  }
  return &(metadata["__meta__"]);
}
const rapidjson::Value* Metadata::getMeta() const {
  if (!(metadata.IsObject() && metadata.HasMember("__meta__"))) {
    log_error() << "getMeta: No __meta__ in metadata" << std::endl;
    return nullptr;
  }
  return &(metadata["__meta__"]);
}
rapidjson::Value* Metadata::getSchema(bool required) {
  if (metadata.HasMember("serializer") &&
      metadata["serializer"].IsObject() &&
      metadata["serializer"].HasMember("datatype") &&
      metadata["serializer"]["datatype"].IsObject()) {
    return &(metadata["serializer"]["datatype"]);
  } else {
    if (required)
      log_error() << "getSchema: No datatype in metadata" << std::endl;
    return nullptr;
  }
}
const rapidjson::Value* Metadata::getSchema(bool required) const {
  if (metadata.HasMember("serializer") &&
      metadata["serializer"].IsObject() &&
      metadata["serializer"].HasMember("datatype") &&
      metadata["serializer"]["datatype"].IsObject()) {
    return &(metadata["serializer"]["datatype"]);
  } else {
    if (required)
      log_error() << "getSchema: No datatype in metadata" << std::endl;
    return nullptr;
  }
}
bool Metadata::SetValue(const std::string name, rapidjson::Value& x,
			rapidjson::Value& subSchema) {
  if (!subSchema.IsObject()) {
    log_error() << "SetValue: subSchema is not an object" << std::endl;
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
      log_error() << "Get" << #method << ": No " << name << " information in the schema." << std::endl; \
      return false;							\
    }									\
    if (!(subSchema[name.c_str()].Is ## method())) {			\
      log_error() << "Get" << #method << ": " << name << " is not " << #type_out << std::endl; \
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
      log_error() << "Get" << #method << "Optional: " << name << " is not " << #type_out << std::endl; \
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
      subSchema = getSchema(true);					\
      if (!subSchema) return false;					\
    }									\
    return Get ## method(name, out, *subSchema);			\
  }									\
  bool Metadata::GetSchema ## method ## Optional(const std::string name, \
						 type_out& out,		\
						 type_out defV,		\
						 const rapidjson::Value* subSchema) const { \
    if (subSchema == NULL) {						\
      subSchema = getSchema();						\
      if (subSchema == NULL) {						\
	out = defV;							\
	return true;							\
      }									\
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
      subSchema = getSchema(true);					\
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
    subSchema = getSchema(true);
    if (!subSchema) return false;
  }
  return SetValue(name, x, *subSchema);
}
bool Metadata::SetSchemaMetadata(const std::string name,
				 const Metadata& other) {
  const rapidjson::Value* other_schema = other.getSchema(true);
  if (!other_schema) {
    log_error() << "SetSchemaMetadata: Value has no datatype" << std::endl;
    return false;
  }
  rapidjson::Value x;
  x.CopyFrom(*other_schema, GetAllocator(), true);
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
bool Metadata::checkFilter() {
  bool out = skip_last;
  skip_last = false;
  return out;
}
bool Metadata::filter(const rapidjson::Document& msg) {
  for (std::vector<filterFunc>::iterator it = filters.begin();
       it != filters.end(); it++) {
    if ((*it)(msg)) {
      skip_last = true;
      break;
    }
  }
  return skip_last;
}
bool Metadata::transform(rapidjson::Document& msg) {
  for (std::vector<transformFunc>::iterator it = transforms.begin();
       it != transforms.end(); it++) {
    if (!(*it)(msg))
      return false;
  }
  return true;
}
int Metadata::deserialize_args(const rapidjson::Document& data,
			       rapidjson::VarArgList& ap) {
  size_t nargs_orig = ap.get_nargs();
  rapidjson::Value* schema = getSchema(true);
  log_debug() << "deserialize_args: data = " << data <<
    ", schema = " << *schema << std::endl;
  if (!data.SetVarArgs(*schema, ap)) {
    log_error() << "deserialize_args: Error setting arguments from JSON document" << std::endl;
    return -1;
  }
  return (int)(nargs_orig - ap.get_nargs());
}
int Metadata::deserialize(const char* buf, rapidjson::Document& d) {
  // Order is: deserialize, set pre-transform schema, transform data,
  //   set schema/normalize data, filter
  rapidjson::StringStream s(buf);
  d.ParseStream(s);
  if (d.HasParseError()) {
    log_error() << "deserialize: Error parsing JSON" << std::endl;
    return -1;
  }
  bool has_raw_schema = (raw_schema != NULL);
  if (transforms.size() > 0) {
    log_debug() << "deserialize: Before transformations " << d << std::endl;
    if (!fromData(d, true)) {
      log_error() << "deserialize: Error updating pre-transformation schema" << std::endl;
      return -1;
    }
    if (!transform(d)) {
      log_error() << "deserialize: Error applying transformations" << std::endl;
      if (!has_raw_schema)
	resetRawSchema();
      return -1;
    }
    log_debug() << "deserialize: After transformations " << d << std::endl;
  }
  bool hasT = hasType();
  if (!hasT) {
    if (!fromData(d)) {
      if (!has_raw_schema)
	resetRawSchema();
      return -1;
    }
  } else {
    rapidjson::StringBuffer sb;
    rapidjson::Value* schema = getSchema(true);
    if (!d.Normalize(*schema, &sb)) {
      log_error() <<
	"deserialize: Error normalizing document:" <<
	std::endl << sb.GetString() <<
	std::endl << "document=" << d <<
	std::endl << "schema=" << *schema <<
	std::endl << "message=" << buf << "..." << std::endl;
      if (!has_raw_schema)
	resetRawSchema();
      return -1;
    }
  }
  if (filter(d)) {
    log_debug() << "deserialize: Message filtered" << std::endl;
    return 0;
  }
  return 1;
}
int Metadata::deserialize(const char* buf, size_t nargs, int allow_realloc, ...) {
  rapidjson::VarArgList va(nargs, allow_realloc);
  va_start(va.va, allow_realloc);
  int out = deserialize(buf, va);
  if (out >= 0 && va.get_nargs() != 0) {
    log_error() << "deserialize: " << va.get_nargs() <<
      " of the arguments were not used" << std::endl;
    return -1;
  }
  return out;
}
int Metadata::deserialize(const char* buf, rapidjson::VarArgList& ap) {
  size_t nargs_orig = ap.get_nargs();
  rapidjson::Document d;
  int ret = deserialize(buf, d);
  if (ret <= 0)
    return ret;
  rapidjson::Value* schema = getSchema(true);
  log_debug() << "deserialize: before SetVarArgs: " << *schema << std::endl;
  if (!d.SetVarArgs(*schema, ap)) {
    log_error() << "deserialize: Error setting arguments from JSON document" << std::endl;
    return -1;
  }
  return (int)(nargs_orig - ap.get_nargs());
}
int Metadata::serialize_args(rapidjson::Document& data,
			     rapidjson::VarArgList& ap) {
  Metadata tmp;
  rapidjson::Value* s = getSchema();
  if (!hasType()) {
    if (isGeneric()) {
      tmp.fromType("any", true);
      s = tmp.getSchema();
    } else {
      log_error() << "serialize_args: No datatype" << std::endl;
      return -1;
    }
  }
  if (!data.GetVarArgs(*s, ap)) {
    log_error() << "serialize_args: Error creating JSON document from arguments for schema =" << *s << std::endl;
    return -1;
  }
  log_debug() << "serialize_args: " << data << std::endl;
  return 1;
}
int Metadata::serialize(char **buf, size_t *buf_siz,
			const rapidjson::Document& data) {
  // Order is: set schema/normalize data, transform data,
  //   set post-transform schema, filter, serialize
  rapidjson::Document d;
  d.CopyFrom(data, d.GetAllocator(), true);
  int hasT = hasType();
  if (!hasT) {
    if (!fromData(d)) {
      return -1;
    }
  } else {
    rapidjson::StringBuffer sb;
    rapidjson::Value* schema = getSchema(true);
    if (!d.Normalize(*schema, &sb)) {
      log_error() << "serialize: Error normalizing document:" <<
	std::endl << sb.GetString() <<
	std::endl << "document=" << d <<
	std::endl << "schema=" << *schema << std::endl;
      return -1;
    }
  }
  if (transforms.size() > 0) {
    log_debug() << "serialize: Before transformations " << d << std::endl;
    if (!transform(d)) {
      log_error() << "serialize: Error applying transformations" << std::endl;
      return -1;
    }
    log_debug() << "serialize: After transformations " << d << std::endl;
    if (!fromData(d, true)) {
      log_error() << "serialize: Error updating post-transformation schema" << std::endl;
      return -1;
    }
    if (!raw_schema) {
      log_error() << "serialize: Error creating raw_schema" << std::endl;
      return -1;
    }
  }
  if (filter(d)) {
    log_debug() << "serialize: Message filtered" << std::endl;
    return 0;
  }
  rapidjson::StringBuffer buffer;
  rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
  d.Accept(writer);
  if ((size_t)(buffer.GetLength() + 1) > buf_siz[0]) {
    char* buf_t = (char*)(GetAllocator().Realloc(buf[0], buf_siz[0],
						 (size_t)(buffer.GetLength() + 1)));
    if (buf_t == NULL) {
      log_error() << "serialize: Error in realloc" << std::endl;
      return -1;
    }
    buf_siz[0] = (size_t)(buffer.GetLength() + 1);
    buf[0] = buf_t;
  }
  memcpy(buf[0], buffer.GetString(), (size_t)(buffer.GetLength()));
  buf[0][(size_t)(buffer.GetLength())] = '\0';
  return static_cast<int>(buffer.GetLength());
}
int Metadata::serialize(char **buf, size_t *buf_siz, size_t nargs, ...) {
  rapidjson::VarArgList va(nargs);
  va_start(va.va, nargs);
  int out = serialize(buf, buf_siz, va);
  if (out >= 0 && va.get_nargs() != 0) {
    log_error() << "serialize: " << va.get_nargs() << " of the arguments were not used" << std::endl;
    return -1;
  }
  return out;
}
int Metadata::serialize(char **buf, size_t *buf_siz,
			rapidjson::VarArgList& ap) {
  rapidjson::Document d;
  if (serialize_args(d, ap) < 0)
    return -1;
  return serialize(buf, buf_siz, d);
}
void Metadata::Display(const char* indent) const {
  std::cout << document2string(metadata, indent) << std::endl;
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
    log_debug() << "operator=: Supplied buffer will be displaced by move" << std::endl;
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
    log_error() << "reallocData: Buffer is not large enough and cannot be reallocated" << std::endl;
    return -1;
  }
  size_buff = size_new + 1;
  char* data_t = (char*)realloc(data[0], size_buff);
  if (!data_t) {
    log_error() << "reallocData: Error in realloc" << std::endl;
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
  log_debug() << "for_send: " << len << " bytes" << std::endl;
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
    rapidjson::Value* metadata0_schema = metadata0->getSchema();
    if (metadata0->raw_schema && metadata0_schema)
      metadata0_schema->Swap(*(metadata0->raw_schema->getSchema(true)));
    bool out = fromMetadata(*metadata0);
    metadata0_schema = metadata0->getSchema();
    if (metadata0->raw_schema && metadata0_schema)
      metadata0_schema->Swap(*(metadata0->raw_schema->getSchema(true)));
    if (!out) return false;
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
  log_debug() << "on_send: " << size_curr << std::endl;
  if (format() < 0)
    return -1;
  if (!((flags & HEAD_FLAG_ASYNC) || dont_advance)) {
    offset += size_msg;
    size_msg = size_curr - offset;
    if (size_max > 0)
      size_msg = std::min(size_msg, size_max);
  }
  log_debug() << "on_send: size_msg = " << size_msg << std::endl;
  return size_curr;
}

void Header::for_recv(char*& buf, size_t buf_siz, bool allow_realloc) {
  data = &buf;
  size_buff = buf_siz;
  if (allow_realloc)
    flags |= HEAD_FLAG_ALLOW_REALLOC;
}
long Header::on_recv(const char* msg, const size_t& msg_siz) {
  log_debug() << "on_recv: " << msg_siz << std::endl;
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
      log_error() << "on_recv: Header (" << size_head <<
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
  log_debug() << "on_recv: done (size_buff = " << size_buff << ")" << std::endl;
  return ret;
}

bool Header::formatBuffer(rapidjson::StringBuffer& buffer, bool metaOnly) {
  buffer.Clear();
  if (empty()) {
    log_debug() << "formatBuffer: Empty metadata" << std::endl;
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
  } else {
    bool useGeneric = isGeneric();
    if (useGeneric) {
      getSchema(true)->RemoveMember("use_generic");
    }
    if (in_data) {
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
      metadata.Accept(writer);
    }
    if (useGeneric) {
      setGeneric();
    }
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
    log_debug() << "format: Empty header" << std::endl;
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
  if (size_max > 0) {
    if (size_new > size_max && (!(flags & HEAD_FLAG_MULTIPART))) {
      // Early return since comm needs to add to header
      flags |= HEAD_FLAG_MULTIPART;
      flags &= ~HEAD_FLAG_FORMATTED;
      if (size_head > size_max) {
	flags |= HEAD_META_IN_DATA;
      }
      return 0;
    } else if (size_head > size_max) {
      log_error() << "format: Extra data already excluded, cannot make header any smaller." << std::endl;
      return -1;
    }
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
  log_debug() << "finalize_recv: begin" << std::endl;
  size_t sind, eind;
  if (find_match_c(MSG_HEAD_SEP, *data, &sind, &eind) > 0) {
    rapidjson::Document type_doc;
    type_doc.Parse(*data, sind);
    if (type_doc.HasParseError()) {
      log_error() << "finalize_recv: Error parsing datatype in data" << std::endl;
      return false;
    }
    if (!fromSchema(type_doc))
      return false;
    size_curr -= eind;
    size_data -= eind;
    memmove(data[0], data[0] + eind, size_curr);
    (*data)[size_curr] = '\0';
  }
  log_debug() << "finalize_recv: end" << std::endl;
  return true;
}
