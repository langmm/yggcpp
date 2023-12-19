#include "../../unittest.hpp"
#include "utils/serialization.hpp"
#include "utils/tools.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"
// #include <sstream>
// #include <algorithm>
// #include <cstdlib>

TEST(Metadata, Utilities) {
    YggInterface::utils::Metadata metadata;
  EXPECT_TRUE(metadata.empty());
  EXPECT_FALSE(metadata.hasType());
  EXPECT_FALSE(metadata.hasSubtype());
  EXPECT_EQ(strcmp(metadata.subtypeName(), ""), 0);
  metadata.setGeneric();
  EXPECT_TRUE(metadata.isGeneric());
  EXPECT_FALSE(metadata.empty());
  metadata.Display();
  EXPECT_TRUE(metadata == metadata);
  EXPECT_FALSE(metadata != metadata);
}

TEST(Metadata, SetAndGet) {
  YggInterface::utils::Metadata metadata;
  EXPECT_EQ(metadata.getMeta(), nullptr);
  EXPECT_EQ(metadata.getSchema(), nullptr);
  EXPECT_FALSE(metadata.SetSchemaMetadata("invalid", metadata));
  // Get errors on missing parent
  int out_int;
  uint64_t out_uint;
  bool out_bool;
  const char* out_string;
  EXPECT_FALSE(metadata.GetMetaInt("test_int", out_int));
  EXPECT_FALSE(metadata.GetMetaUint("test_uint", out_uint));
  EXPECT_FALSE(metadata.GetMetaBool("test_bool", out_bool));
  EXPECT_FALSE(metadata.GetMetaString("test_string", out_string));
  EXPECT_FALSE(metadata.GetSchemaInt("test_int", out_int));
  EXPECT_FALSE(metadata.GetSchemaUint("test_uint", out_uint));
  EXPECT_FALSE(metadata.GetSchemaBool("test_bool", out_bool));
  EXPECT_FALSE(metadata.GetSchemaString("test_string", out_string));
  // Set errors
  rapidjson::Value a(true);
  rapidjson::Value b(true);
  EXPECT_FALSE(metadata.SetMetaInt("test_int", 1));
  EXPECT_FALSE(metadata.SetMetaUint("test_uint", 1u));
  EXPECT_FALSE(metadata.SetMetaBool("test_bool", true));
  EXPECT_FALSE(metadata.SetMetaString("test_string", "a"));
  EXPECT_FALSE(metadata.SetMetaValue("test_value", a));
  EXPECT_FALSE(metadata.SetSchemaInt("test_int", 1));
  EXPECT_FALSE(metadata.SetSchemaUint("test_uint", 1u));
  EXPECT_FALSE(metadata.SetSchemaBool("test_bool", true));
  EXPECT_FALSE(metadata.SetSchemaString("test_string", "a"));
  EXPECT_FALSE(metadata.SetSchemaValue("test_value", b));
  // Init
  metadata.initMeta();
  metadata.initSchema();
  // Set
#define TEST_SET_METHOD(base, name, type, value)			\
  {									\
    type x = value, y;							\
    EXPECT_TRUE(metadata.Set ## base ## name("test_" #name, x));	\
    EXPECT_TRUE(metadata.Get ## base ## name("test_" #name, y));	\
    EXPECT_EQ(x, y);							\
  }
#define TEST_SET_METHOD_STRING(base)					\
  {									\
    std::string x = "a";						\
    const char* y = nullptr;						\
    EXPECT_TRUE(metadata.Set ## base ## String("test_String", x));	\
    EXPECT_TRUE(metadata.Get ## base ## String("test_String", y));	\
    EXPECT_EQ(strcmp(y, x.c_str()), 0);					\
  }
#define TEST_SET_METHOD_VALUE(base)					\
  {									\
    EXPECT_TRUE(metadata.Set ## base ## Value("test_value", b));	\
  }
  TEST_SET_METHOD(Meta, Int, int, 1);
  TEST_SET_METHOD(Meta, Uint, unsigned, 1u);
  TEST_SET_METHOD(Meta, Bool, bool, true);
  TEST_SET_METHOD(Meta, String, std::string, "hello");
  TEST_SET_METHOD_STRING(Meta);
  TEST_SET_METHOD_VALUE(Meta);
  const char* id1 = NULL;
  const char* id1_out = NULL;
  std::string id2 = "", id2_out;
  EXPECT_TRUE(metadata.SetMetaID("test_id1", &id1));
  EXPECT_TRUE(metadata.SetMetaID("test_id2", id2));
  EXPECT_TRUE(metadata.GetMetaString("test_id1", id1_out));
  EXPECT_EQ(strcmp(id1_out, id1), 0);
  EXPECT_TRUE(metadata.GetMetaString("test_id2", id2_out));
  EXPECT_EQ(id2_out, id2);
  TEST_SET_METHOD(Schema, Int, int, 1);
  TEST_SET_METHOD(Schema, Uint, unsigned, 1u);
  TEST_SET_METHOD(Schema, Bool, bool, true);
  TEST_SET_METHOD(Schema, String, std::string, "hello");
  TEST_SET_METHOD_STRING(Schema);
  TEST_SET_METHOD_VALUE(Schema);
  rapidjson::Value* x_root = metadata.getSchema();
  EXPECT_TRUE(metadata.SetSchemaValue("test_value2", b, x_root));
  // Get errors on missing var
  EXPECT_FALSE(metadata.GetMetaInt("invalid", out_int));
  EXPECT_FALSE(metadata.GetMetaUint("invalid", out_uint));
  EXPECT_FALSE(metadata.GetMetaBool("invalid", out_bool));
  EXPECT_FALSE(metadata.GetMetaString("invalid", out_string));
  EXPECT_FALSE(metadata.GetSchemaInt("invalid", out_int));
  EXPECT_FALSE(metadata.GetSchemaUint("invalid", out_uint));
  EXPECT_FALSE(metadata.GetSchemaBool("invalid", out_bool));
  EXPECT_FALSE(metadata.GetSchemaString("invalid", out_string));
  // Get error on wrong type
  EXPECT_FALSE(metadata.GetMetaInt("test_String", out_int));
  EXPECT_FALSE(metadata.GetMetaUint("test_String", out_uint));
  EXPECT_FALSE(metadata.GetMetaBool("test_String", out_bool));
  EXPECT_FALSE(metadata.GetMetaString("test_Bool", out_string));
  EXPECT_FALSE(metadata.GetSchemaInt("test_String", out_int));
  EXPECT_FALSE(metadata.GetSchemaUint("test_String", out_uint));
  EXPECT_FALSE(metadata.GetSchemaBool("test_String", out_bool));
  EXPECT_FALSE(metadata.GetSchemaString("test_Bool", out_string));
  // Get error on wrong type with optional
  EXPECT_FALSE(metadata.GetMetaIntOptional("test_String", out_int, 1));
  EXPECT_FALSE(metadata.GetMetaUintOptional("test_String", out_uint, 1u));
  EXPECT_FALSE(metadata.GetMetaBoolOptional("test_String", out_bool, true));
  EXPECT_FALSE(metadata.GetMetaStringOptional("test_Bool", out_string, "a"));
  EXPECT_FALSE(metadata.GetSchemaIntOptional("test_String", out_int, 1));
  EXPECT_FALSE(metadata.GetSchemaUintOptional("test_String", out_uint, 1u));
  EXPECT_FALSE(metadata.GetSchemaBoolOptional("test_String", out_bool, true));
  EXPECT_FALSE(metadata.GetSchemaStringOptional("test_Bool", out_string, "a"));
  // Get optional
  EXPECT_TRUE(metadata.GetMetaIntOptional("invalid", out_int, 1));
  EXPECT_EQ(out_int, 1);
  EXPECT_TRUE(metadata.GetMetaUintOptional("invalid", out_uint, 1u));
  EXPECT_EQ(out_uint, 1u);
  EXPECT_TRUE(metadata.GetMetaBoolOptional("invalid", out_bool, true));
  EXPECT_EQ(out_bool, true);
  EXPECT_TRUE(metadata.GetMetaStringOptional("invalid", out_string, "a"));
  EXPECT_EQ(strcmp(out_string, "a"), 0);
  EXPECT_TRUE(metadata.GetSchemaIntOptional("invalid", out_int, 1));
  EXPECT_EQ(out_int, 1);
  EXPECT_TRUE(metadata.GetSchemaUintOptional("invalid", out_uint, 1u));
  EXPECT_EQ(out_uint, 1u);
  EXPECT_TRUE(metadata.GetSchemaBoolOptional("invalid", out_bool, true));
  EXPECT_EQ(out_bool, true);
  EXPECT_TRUE(metadata.GetSchemaStringOptional("invalid", out_string, "a"));
  EXPECT_EQ(strcmp(out_string, "a"), 0);
  EXPECT_FALSE(metadata.SetValue("test_value", a,
				 (*metadata.getMeta())["test_id1"]));
  EXPECT_TRUE(metadata.GetMetaBool("test_value", out_bool));
  EXPECT_TRUE(out_bool);
  rapidjson::Value c(false);
  EXPECT_TRUE(metadata.SetValue("test_value", c, *metadata.getMeta()));
  EXPECT_TRUE(metadata.GetMetaBool("test_value", out_bool));
  EXPECT_FALSE(out_bool);
}

TEST(Metadata, components) {
  YggInterface::utils::Metadata x;
  YggInterface::utils::Metadata y;
  YggInterface::utils::Metadata z;
  // Array
  z.fromSchema("{\"type\": \"array\", "
	       "\"items\": [{\"type\": \"integer\"}]}", true);
  EXPECT_FALSE(x.addItem(y));
  EXPECT_TRUE(y.fromSchema("{\"type\": \"integer\"}"));
  EXPECT_FALSE(x.addItem(y));
  EXPECT_TRUE(x.fromSchema("{\"type\": \"array\"}"));
  EXPECT_TRUE(x.addItem(y));
  x.Display();
  z.Display();
  EXPECT_EQ(x, z);
  x.reset();
  EXPECT_TRUE(x.fromSchema("{\"type\": \"array\", "
			   "\"items\": {\"type\": \"integer\"}}"));
  EXPECT_FALSE(x.addItem(y));
  // Object
  x.reset();
  y.reset();
  z.reset();
  EXPECT_TRUE(z.fromSchema("{\"type\": \"object\", "
			   "\"properties\": {\"a\": {\"type\": \"integer\"}}}"));
  EXPECT_FALSE(x.addMember("a", y));
  EXPECT_TRUE(y.fromSchema("{\"type\": \"integer\"}"));
  EXPECT_FALSE(x.addMember("a", y));
  EXPECT_TRUE(x.fromSchema("{\"type\": \"object\"}"));
  EXPECT_TRUE(x.addMember("a", y));
  EXPECT_EQ(x, z);
  EXPECT_TRUE(x.addMember("a", y));
  EXPECT_EQ(x, z);
  // x.reset();
  // EXPECT_TRUE(x.fromSchema("{\"type\": \"object\", "
  // 	       "\"properties\": [{\"a\": {\"type\": \"integer\"}}]}"));
  // EXPECT_FALSE(x.addMember("a", y));
}

TEST(Metadata, fromSchema) {
  YggInterface::utils::Metadata x;
  EXPECT_TRUE(x.setGeneric());
  EXPECT_TRUE(x.isGeneric());
  EXPECT_TRUE(x.fromSchema("{\"type\": \"integer\"}"));
  EXPECT_TRUE(x.isGeneric());
  YggInterface::utils::Metadata y;
  EXPECT_TRUE(y.fromSchema(x.metadata, true));
  EXPECT_EQ(x.metadata, y.metadata);
  EXPECT_TRUE(y.fromSchema(*(x.getSchema())));
  YggInterface::utils::Metadata z;
  EXPECT_TRUE(z.fromSchema("{\"type\": \"string\"}"));
  EXPECT_FALSE(y.fromSchema(*(z.getSchema())));
  YggInterface::utils::Metadata q;
  EXPECT_TRUE(q.fromType("string"));
  EXPECT_EQ(z.metadata, q.metadata);
}

TEST(Metadata, fromScalar) {
  YggInterface::utils::Metadata x;
  EXPECT_TRUE(x.fromSchema(
	       "{"
	       "  \"type\": \"scalar\","
	       "  \"subtype\": \"float\","
	       "  \"precision\": 4,"
	       "  \"units\": \"cm\""
	       "}"));
  YggInterface::utils::Metadata y;
  EXPECT_TRUE(y.fromScalar("float", 4, "cm"));
  EXPECT_EQ(x.metadata, y.metadata);
  std::cerr << x.metadata << std::endl;
  EXPECT_TRUE(y.hasType());
  EXPECT_TRUE(y.hasSubtype());
  EXPECT_EQ(strcmp(y.typeName(), "scalar"), 0);
  EXPECT_EQ(strcmp(y.subtypeName(), "float"), 0);
}
TEST(Metadata, fromNDArray) {
  YggInterface::utils::Metadata x;
  EXPECT_TRUE(x.fromSchema(
	       "{"
	       "  \"type\": \"ndarray\","
	       "  \"subtype\": \"float\","
	       "  \"precision\": 4,"
	       "  \"shape\": [2, 3],"
	       "  \"units\": \"cm\""
	       "}"));
  YggInterface::utils::Metadata y;
  size_t shape[2] = { 2, 3 };
  EXPECT_TRUE(y.fromNDArray("float", 4, 2, shape, "cm"));
  EXPECT_EQ(x.metadata, y.metadata);
  EXPECT_TRUE(y.hasType());
  EXPECT_TRUE(y.hasSubtype());
  EXPECT_EQ(strcmp(y.typeName(), "ndarray"), 0);
  EXPECT_EQ(strcmp(y.subtypeName(), "float"), 0);
  // No shape
  x.reset();
  y.reset();
  EXPECT_TRUE(x.fromSchema(
	       "{"
	       "  \"type\": \"ndarray\","
	       "  \"subtype\": \"float\","
	       "  \"precision\": 4,"
	       "  \"ndim\": 2,"
	       "  \"units\": \"cm\""
	       "}"));
  EXPECT_TRUE(y.fromNDArray("float", 4, 2, NULL, "cm"));
  EXPECT_EQ(x, y);
  // Bytes
  x.reset();
  y.reset();
  EXPECT_TRUE(x.fromSchema(
	       "{"
	       "  \"type\": \"ndarray\","
	       "  \"subtype\": \"string\","
	       "  \"precision\": 4,"
	       "  \"shape\": [2, 3],"
	       "  \"units\": \"cm\""
	       "}"));
  EXPECT_TRUE(y.fromNDArray("bytes", 4, 2, shape, "cm"));
  EXPECT_EQ(x, y);
  // Unicode
  x.reset();
  y.reset();
  EXPECT_TRUE(x.fromSchema(
	       "{"
	       "  \"type\": \"ndarray\","
	       "  \"subtype\": \"string\","
	       "  \"precision\": 4,"
	       "  \"shape\": [2, 3],"
	       "  \"units\": \"cm\","
	       "  \"encoding\": \"UTF8\""
	       "}"));
  EXPECT_TRUE(y.fromNDArray("unicode", 4, 2, shape, "cm"));
  EXPECT_EQ(x, y);
}

TEST(Metadata, fromFormat) {
  std::string formatStr = "%f\t%d\t%5s\n";
  {
    // Scalar
    YggInterface::utils::Metadata x;
    EXPECT_TRUE(x.fromSchema(
		 "{"
		 "  \"type\": \"array\","
		 "  \"items\": ["
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"float\","
		 "      \"precision\": 8"
		 "    },"
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"int\","
		 "      \"precision\": 4"
		 "    },"
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"string\","
		 "      \"precision\": 5"
		 "    }"
		 "  ]"
		 "}"));
    x.SetString("format_str", formatStr, x.metadata["serializer"]);
    YggInterface::utils::Metadata y;
    EXPECT_TRUE(y.fromFormat(formatStr));
    EXPECT_EQ(x.metadata, y.metadata);
  }
  {
    // Arrays
    YggInterface::utils::Metadata x;
    EXPECT_TRUE(x.fromSchema(
		 "{"
		 "  \"type\": \"array\","
		 "  \"items\": ["
		 "    {"
		 "      \"type\": \"ndarray\","
		 "      \"subtype\": \"float\","
		 "      \"precision\": 8"
		 "    },"
		 "    {"
		 "      \"type\": \"ndarray\","
		 "      \"subtype\": \"int\","
		 "      \"precision\": 4"
		 "    },"
		 "    {"
		 "      \"type\": \"ndarray\","
		 "      \"subtype\": \"string\","
		 "      \"precision\": 5"
		 "    }"
		 "  ]"
		 "}"));
    x.SetString("format_str", formatStr, x.metadata["serializer"]);
    YggInterface::utils::Metadata y;
    EXPECT_TRUE(y.fromFormat(formatStr, true));
    EXPECT_EQ(x.metadata, y.metadata);
    EXPECT_TRUE(y.isFormatArray());
  }
  {
    // Additional types
    std::string fmt = "%hhi\t%hi\t%lli\t%l64i\t%li\t%i\t%hhu\t%hu\t%llu\t%l64u\t%lu\t%u\t%f%+fj\n";
    YggInterface::utils::Metadata x;
#ifdef _WIN32
    EXPECT_TRUE(x.fromSchema(
	         "{"
		 "  \"type\": \"array\","
		 "  \"items\": ["
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"int\","
		 "      \"precision\": 1"
		 "    },"
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"int\","
		 "      \"precision\": 2"
		 "    },"
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"int\","
		 "      \"precision\": 8"
		 "    },"
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"int\","
		 "      \"precision\": 8"
		 "    },"
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"int\","
		 "      \"precision\": 4"
		 "    },"
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"int\","
		 "      \"precision\": 4"
		 "    },"
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"uint\","
		 "      \"precision\": 1"
		 "    },"
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"uint\","
		 "      \"precision\": 2"
		 "    },"
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"uint\","
		 "      \"precision\": 8"
		 "    },"
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"uint\","
		 "      \"precision\": 8"
		 "    },"
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"uint\","
		 "      \"precision\": 4"
		 "    },"
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"uint\","
		 "      \"precision\": 4"
		 "    },"
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"complex\","
		 "      \"precision\": 16"
		 "    }"
		 "  ]"
		 "}"));
#else // _WIN32
    EXPECT_TRUE(x.fromSchema(
	         "{"
		 "  \"type\": \"array\","
		 "  \"items\": ["
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"int\","
		 "      \"precision\": 1"
		 "    },"
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"int\","
		 "      \"precision\": 2"
		 "    },"
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"int\","
		 "      \"precision\": 8"
		 "    },"
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"int\","
		 "      \"precision\": 8"
		 "    },"
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"int\","
		 "      \"precision\": 8"
		 "    },"
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"int\","
		 "      \"precision\": 4"
		 "    },"
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"uint\","
		 "      \"precision\": 1"
		 "    },"
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"uint\","
		 "      \"precision\": 2"
		 "    },"
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"uint\","
		 "      \"precision\": 8"
		 "    },"
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"uint\","
		 "      \"precision\": 8"
		 "    },"
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"uint\","
		 "      \"precision\": 8"
		 "    },"
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"uint\","
		 "      \"precision\": 4"
		 "    },"
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"complex\","
		 "      \"precision\": 16"
		 "    }"
		 "  ]"
		 "}"));
#endif // _WIN32
    x.SetString("format_str", fmt, x.metadata["serializer"]);
    YggInterface::utils::Metadata y;
    EXPECT_TRUE(y.fromFormat(fmt));
    EXPECT_EQ(x.metadata, y.metadata);
  }
  {
    // Singular
    std::string fmt = "%d\n";
    YggInterface::utils::Metadata x;
    EXPECT_TRUE(x.fromSchema(
		 "{"
		 "  \"type\": \"array\","
		 "  \"allowSingular\": true,"
		 "  \"items\": ["
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"int\","
		 "      \"precision\": 4"
		 "    }"
		 "  ]"
		 "}"));
    x.SetString("format_str", fmt, x.metadata["serializer"]);
    YggInterface::utils::Metadata y;
    EXPECT_TRUE(y.fromFormat(fmt));
    EXPECT_EQ(x.metadata, y.metadata);
  }
  {
    // Error
    YggInterface::utils::Metadata x;
    EXPECT_FALSE(x.fromFormat("%m"));
  }
}

TEST(Metadata, fromMetadata) {
  YggInterface::utils::Metadata x;
  EXPECT_FALSE(x.fromMetadata("{"));
  EXPECT_FALSE(x.fromMetadata("\"hello\""));
  EXPECT_FALSE(x.fromMetadata("{}"));
  EXPECT_FALSE(x.fromMetadata("{\"__meta__\": \"hello\"}"));
  EXPECT_TRUE(x.fromMetadata("{\"__meta__\": {}}"));
  YggInterface::utils::Metadata y;
  EXPECT_TRUE(y.fromMetadata(x));
  EXPECT_EQ(x.metadata, y.metadata);
}

TEST(Metadata, fromEncode) {
  rapidjson::Value v(true);
  YggInterface::utils::Metadata x;
  x.fromSchema("{\"type\": \"boolean\"}");
  YggInterface::utils::Metadata y;
  y.fromEncode(v);
  EXPECT_EQ(x.metadata, y.metadata);
}

TEST(Metadata, deserialize_errors) {
  rapidjson::VarArgList va;
  YggInterface::utils::Metadata x;
  EXPECT_EQ(x.deserialize("", va), -1);
  bool dst = false, dst2 = false;
  EXPECT_EQ(x.deserialize("true", 1, false, &dst), 1);
  EXPECT_EQ(dst, true);
  EXPECT_EQ(x.deserialize("true", 2, false, &dst, &dst2), -1);
  EXPECT_TRUE(x.fromSchema("{\"type\": \"boolean\"}"));
  EXPECT_EQ(x.deserialize("{invalid:}", va), -1);
  EXPECT_EQ(x.deserialize("\"string\"", va), -1);
  EXPECT_EQ(x.deserialize("true", va), -1);
  bool* dst_ptr = &dst;
  EXPECT_EQ(x.deserialize("true", 1, false, dst_ptr), 1);
  EXPECT_EQ(dst, true);
}

TEST(Metadata, serialize_errors) {
  rapidjson::VarArgList va;
  YggInterface::utils::Metadata x;
  char* buf = (char*)(x.GetAllocator().Malloc(sizeof(char)));
  size_t len = 1;
  EXPECT_EQ(x.serialize(&buf, &len, va), -1);
  EXPECT_TRUE(x.fromSchema("{\"type\": \"boolean\"}"));
  EXPECT_EQ(x.serialize(&buf, &len, va), -1);
  EXPECT_EQ(x.serialize(&buf, &len, 2, true, false), -1);
  x.GetAllocator().Free(buf);
  buf = NULL;
  len = 0;
  EXPECT_EQ(x.serialize(&buf, &len, 1, true), 4);
  EXPECT_EQ(strcmp(buf, "true"), 0);
  EXPECT_EQ(len, 5);
  x.GetAllocator().Free(buf);
  buf = NULL;
  len = 0;
  // Cannot isolate this use of realloc as rapidjson also uses
  // realloc in writing to a string buffer
// #ifdef ELF_AVAILABLE
//   ELF_BEGIN;
//   ELF_BEGIN_F(realloc);
//   EXPECT_EQ(x.serialize(&buf, &len, 1, true), -1);
//   ELF_END_F(realloc);
//   ELF_END;
// #endif // ELF_AVAILABLE
}

TEST(Header, Utilities) {
  YggInterface::utils::Header header;
  rapidjson::StringBuffer buf;
  EXPECT_TRUE(header.formatBuffer(buf));
  EXPECT_EQ(buf.GetLength(), 0);
  EXPECT_EQ(header.format(), 0);
  EXPECT_TRUE(header == header);
  EXPECT_FALSE(header != header);
  YggInterface::utils::Header header2;
  EXPECT_TRUE(header2.fromType("integer", false, false));
  EXPECT_FALSE(header == header2);
  EXPECT_TRUE(header != header2);
  EXPECT_TRUE(header.fromType("integer", false, false));
  EXPECT_FALSE(header == header2);
  EXPECT_TRUE(header != header2);
  header.reset();
}

TEST(Header, for_send) {
  YggInterface::utils::Metadata schema;
  EXPECT_TRUE(schema.fromSchema("{\"type\": \"string\"}"));
  std::string msg = "This is a test message";
  setenv("YGG_MODEL_NAME", "model", 1);
  setenv("YGG_MODEL_COPY", "1", 1);
  YggInterface::utils::Header header_send;
  EXPECT_TRUE(header_send.for_send(&schema, msg.c_str(), msg.size(), 0));
  EXPECT_TRUE(header_send.on_send());
  YggInterface::utils::Header header_recv;
  EXPECT_EQ(header_recv.on_recv(header_send.data[0],
				header_send.size_curr),
	    -static_cast<long>(header_send.size_curr));
  header_recv.flags |= static_cast<uint16_t>(HEAD_BUFFER_MASK);
  header_recv.data = &(header_recv.data_);
  EXPECT_EQ(header_recv.on_recv(header_send.data[0],
				header_send.size_curr),
	    header_send.size_curr);
  EXPECT_EQ(header_recv.size_data, msg.size());
  EXPECT_EQ(strcmp(header_recv.data[0], msg.c_str()), 0);
  // Header equality
  YggInterface::utils::Header header_send2;
  EXPECT_TRUE(header_send2.for_send(&schema, msg.c_str(), msg.size(), 0));
  EXPECT_TRUE(header_send2.fromMetadata(header_send));
  EXPECT_TRUE(header_send2.on_send());
  YggInterface::utils::Header header_recv2(true);
  EXPECT_EQ(header_recv2.on_recv(header_send2.data[0],
				 header_send2.size_curr),
	    header_send2.size_curr);
  EXPECT_EQ(header_send, header_send2);
  EXPECT_EQ(header_recv, header_recv2);
  EXPECT_NE(header_recv, header_send);
  // Received message larger than buffer
  std::string msg3(2048, 'a');
  YggInterface::utils::Header header_send3;
  EXPECT_TRUE(header_send3.for_send(&schema, msg3.c_str(), msg3.size(), 0));
  EXPECT_TRUE(header_send3.on_send());
  size_t size_buf3 = header_send3.size_curr - 1000;
  char* buf3 = (char*)malloc(size_buf3);
  YggInterface::utils::Header header_recv3(buf3, size_buf3, false);
  EXPECT_EQ(header_recv3.on_recv(header_send3.data[0], size_buf3 - 1), -1);
  free(buf3);
  buf3 = NULL;
  unsetenv("YGG_MODEL_NAME");
  unsetenv("YGG_MODEL_COPY");
}

