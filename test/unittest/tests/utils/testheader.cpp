#include "../../unittest.hpp"
#include "utils/serialization.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"
// #include <sstream>
// #include <algorithm>
// #include <cstdlib>

TEST(Metadata, Utilities) {
  communication::utils::Metadata metadata;
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
  communication::utils::Metadata metadata;
  EXPECT_THROW(metadata.getMeta(), std::exception);
  EXPECT_THROW(metadata.getSchema(), std::exception);
  EXPECT_THROW(metadata.SetSchemaMetadata("invalid", metadata), std::exception);
  // Get errors on missing parent
  EXPECT_THROW(metadata.GetMetaInt("test_int"), std::exception);
  EXPECT_THROW(metadata.GetMetaUint("test_uint"), std::exception);
  EXPECT_THROW(metadata.GetMetaBool("test_bool"), std::exception);
  EXPECT_THROW(metadata.GetMetaString("test_string"), std::exception);
  EXPECT_THROW(metadata.GetSchemaInt("test_int"), std::exception);
  EXPECT_THROW(metadata.GetSchemaUint("test_uint"), std::exception);
  EXPECT_THROW(metadata.GetSchemaBool("test_bool"), std::exception);
  EXPECT_THROW(metadata.GetSchemaString("test_string"), std::exception);
  // Set errors
  rapidjson::Value a(true);
  rapidjson::Value b(true);
  EXPECT_THROW(metadata.SetMetaInt("test_int", 1), std::exception);
  EXPECT_THROW(metadata.SetMetaUint("test_uint", 1u), std::exception);
  EXPECT_THROW(metadata.SetMetaBool("test_bool", true), std::exception);
  EXPECT_THROW(metadata.SetMetaString("test_string", "a"), std::exception);
  EXPECT_THROW(metadata.SetMetaValue("test_value", a), std::exception);
  EXPECT_THROW(metadata.SetSchemaInt("test_int", 1), std::exception);
  EXPECT_THROW(metadata.SetSchemaUint("test_uint", 1u), std::exception);
  EXPECT_THROW(metadata.SetSchemaBool("test_bool", true), std::exception);
  EXPECT_THROW(metadata.SetSchemaString("test_string", "a"), std::exception);
  EXPECT_THROW(metadata.SetSchemaValue("test_value", b), std::exception);
  // Init
  metadata.initMeta();
  metadata.initSchema();
  // Set
#define TEST_SET_METHOD(base, name, type, value)		\
  {								\
    type x = value;						\
    metadata.Set ## base ## name("test_" #name, x);		\
    EXPECT_EQ(metadata.Get ## base ## name("test_" #name), x);	\
  }
#define TEST_SET_METHOD_STRING(base)					\
  {									\
    std::string x = "a";						\
    metadata.Set ## base ## String("test_String", x);			\
    EXPECT_EQ(strcmp(metadata.Get ## base ## String("test_String"), x.c_str()), 0); \
  }
#define TEST_SET_METHOD_VALUE(base)			\
  {							\
    metadata.Set ## base ## Value("test_value", b);	\
  }
  TEST_SET_METHOD(Meta, Int, int, 1);
  TEST_SET_METHOD(Meta, Uint, unsigned, 1u);
  TEST_SET_METHOD(Meta, Bool, bool, true);
  TEST_SET_METHOD_STRING(Meta);
  TEST_SET_METHOD_VALUE(Meta);
  const char* id1 = NULL;
  std::string id2 = "";
  metadata.SetMetaID("test_id1", &id1);
  metadata.SetMetaID("test_id2", id2);
  EXPECT_EQ(strcmp(metadata.GetMetaString("test_id1"), id1), 0);
  EXPECT_EQ(strcmp(metadata.GetMetaString("test_id2"), id2.c_str()), 0);
  TEST_SET_METHOD(Schema, Int, int, 1);
  TEST_SET_METHOD(Schema, Uint, unsigned, 1u);
  TEST_SET_METHOD(Schema, Bool, bool, true);
  TEST_SET_METHOD_STRING(Schema);
  TEST_SET_METHOD_VALUE(Schema);
  rapidjson::Value* x_root = &metadata.getSchema();
  metadata.SetSchemaValue("test_value2", b, x_root);
  // Get errors on missing var
  EXPECT_THROW(metadata.GetMetaInt("invalid"), std::exception);
  EXPECT_THROW(metadata.GetMetaUint("invalid"), std::exception);
  EXPECT_THROW(metadata.GetMetaBool("invalid"), std::exception);
  EXPECT_THROW(metadata.GetMetaString("invalid"), std::exception);
  EXPECT_THROW(metadata.GetSchemaInt("invalid"), std::exception);
  EXPECT_THROW(metadata.GetSchemaUint("invalid"), std::exception);
  EXPECT_THROW(metadata.GetSchemaBool("invalid"), std::exception);
  EXPECT_THROW(metadata.GetSchemaString("invalid"), std::exception);
  // Get error on wrong type
  EXPECT_THROW(metadata.GetMetaInt("test_String"), std::exception);
  EXPECT_THROW(metadata.GetMetaUint("test_String"), std::exception);
  EXPECT_THROW(metadata.GetMetaBool("test_String"), std::exception);
  EXPECT_THROW(metadata.GetMetaString("test_Bool"), std::exception);
  EXPECT_THROW(metadata.GetSchemaInt("test_String"), std::exception);
  EXPECT_THROW(metadata.GetSchemaUint("test_String"), std::exception);
  EXPECT_THROW(metadata.GetSchemaBool("test_String"), std::exception);
  EXPECT_THROW(metadata.GetSchemaString("test_Bool"), std::exception);
  // Get error on wrong type with optional
  EXPECT_THROW(metadata.GetMetaIntOptional("test_String", 1),
	       std::exception);
  EXPECT_THROW(metadata.GetMetaUintOptional("test_String", 1u),
	       std::exception);
  EXPECT_THROW(metadata.GetMetaBoolOptional("test_String", true),
	       std::exception);
  EXPECT_THROW(metadata.GetMetaStringOptional("test_Bool", "a"),
	       std::exception);
  EXPECT_THROW(metadata.GetSchemaIntOptional("test_String", 1),
	       std::exception);
  EXPECT_THROW(metadata.GetSchemaUintOptional("test_String", 1u),
	       std::exception);
  EXPECT_THROW(metadata.GetSchemaBoolOptional("test_String", true),
	       std::exception);
  EXPECT_THROW(metadata.GetSchemaStringOptional("test_Bool", "a"),
	       std::exception);
  // Get optional
  EXPECT_EQ(metadata.GetMetaIntOptional("invalid", 1), 1);
  EXPECT_EQ(metadata.GetMetaUintOptional("invalid", 1u), 1u);
  EXPECT_EQ(metadata.GetMetaBoolOptional("invalid", true), true);
  EXPECT_EQ(strcmp(metadata.GetMetaStringOptional("invalid", "a"), "a"), 0);
  EXPECT_EQ(metadata.GetSchemaIntOptional("invalid", 1), 1);
  EXPECT_EQ(metadata.GetSchemaUintOptional("invalid", 1u), 1u);
  EXPECT_EQ(metadata.GetSchemaBoolOptional("invalid", true), true);
  EXPECT_EQ(strcmp(metadata.GetSchemaStringOptional("invalid", "a"), "a"), 0);
  EXPECT_THROW(metadata.SetValue("test_value", a,
				 metadata.getMeta()["test_id1"]),
	       std::exception);
  EXPECT_TRUE(metadata.GetMetaBool("test_value"));
  rapidjson::Value c(false);
  metadata.SetValue("test_value", c, metadata.getMeta());
  EXPECT_FALSE(metadata.GetMetaBool("test_value"));
}

TEST(Metadata, components) {
  communication::utils::Metadata x;
  communication::utils::Metadata y;
  communication::utils::Metadata z;
  // Array
  z.fromSchema("{\"type\": \"array\", "
	       "\"items\": [{\"type\": \"integer\"}]}", true);
  EXPECT_THROW(x.addItem(y), std::exception);
  y.fromSchema("{\"type\": \"integer\"}");
  EXPECT_THROW(x.addItem(y), std::exception);
  x.fromSchema("{\"type\": \"array\"}");
  x.addItem(y);
  x.Display();
  z.Display();
  EXPECT_EQ(x, z);
  x.reset();
  x.fromSchema("{\"type\": \"array\", "
	       "\"items\": {\"type\": \"integer\"}}");
  EXPECT_THROW(x.addItem(y), std::exception);
  // Object
  x.reset();
  y.reset();
  z.reset();
  z.fromSchema("{\"type\": \"object\", "
	       "\"properties\": {\"a\": {\"type\": \"integer\"}}}");
  EXPECT_THROW(x.addMember("a", y), std::exception);
  y.fromSchema("{\"type\": \"integer\"}");
  EXPECT_THROW(x.addMember("a", y), std::exception);
  x.fromSchema("{\"type\": \"object\"}");
  x.addMember("a", y);
  EXPECT_EQ(x, z);
  x.addMember("a", y);
  EXPECT_EQ(x, z);
  // x.reset();
  // x.fromSchema("{\"type\": \"object\", "
  // 	       "\"properties\": [{\"a\": {\"type\": \"integer\"}}]}");
  // EXPECT_THROW(x.addMember("a", y), std::exception);
}

TEST(Metadata, fromSchema) {
  communication::utils::Metadata x;
  x.setGeneric();
  EXPECT_TRUE(x.isGeneric());
  x.fromSchema("{\"type\": \"integer\"}");
  EXPECT_TRUE(x.isGeneric());
  communication::utils::Metadata y;
  y.fromSchema(x.metadata, true);
  EXPECT_EQ(x.metadata, y.metadata);
  y.fromSchema(x.getSchema());
  communication::utils::Metadata z;
  z.fromSchema("{\"type\": \"string\"}");
  EXPECT_THROW(y.fromSchema(z.getSchema()), std::exception);
  communication::utils::Metadata q;
  q.fromType("string");
  EXPECT_EQ(z.metadata, q.metadata);
}

TEST(Metadata, fromScalar) {
  communication::utils::Metadata x;
  x.fromSchema("{"
	       "  \"type\": \"scalar\","
	       "  \"subtype\": \"float\","
	       "  \"precision\": 4,"
	       "  \"units\": \"cm\""
	       "}");
  communication::utils::Metadata y;
  y.fromScalar("float", 4, "cm");
  EXPECT_EQ(x.metadata, y.metadata);
  std::cerr << x.metadata << std::endl;
  EXPECT_TRUE(y.hasType());
  EXPECT_TRUE(y.hasSubtype());
  EXPECT_EQ(strcmp(y.typeName(), "scalar"), 0);
  EXPECT_EQ(strcmp(y.subtypeName(), "float"), 0);
}
TEST(Metadata, fromNDArray) {
  communication::utils::Metadata x;
  x.fromSchema("{"
	       "  \"type\": \"ndarray\","
	       "  \"subtype\": \"float\","
	       "  \"precision\": 4,"
	       "  \"shape\": [2, 3],"
	       "  \"units\": \"cm\""
	       "}");
  communication::utils::Metadata y;
  size_t shape[2] = { 2, 3 };
  y.fromNDArray("float", 4, 2, shape, "cm");
  EXPECT_EQ(x.metadata, y.metadata);
  EXPECT_TRUE(y.hasType());
  EXPECT_TRUE(y.hasSubtype());
  EXPECT_EQ(strcmp(y.typeName(), "ndarray"), 0);
  EXPECT_EQ(strcmp(y.subtypeName(), "float"), 0);
  // No shape
  x.reset();
  y.reset();
  x.fromSchema("{"
	       "  \"type\": \"ndarray\","
	       "  \"subtype\": \"float\","
	       "  \"precision\": 4,"
	       "  \"ndim\": 2,"
	       "  \"units\": \"cm\""
	       "}");
  y.fromNDArray("float", 4, 2, NULL, "cm");
  EXPECT_EQ(x, y);
  // Bytes
  x.reset();
  y.reset();
  x.fromSchema("{"
	       "  \"type\": \"ndarray\","
	       "  \"subtype\": \"string\","
	       "  \"precision\": 4,"
	       "  \"shape\": [2, 3],"
	       "  \"units\": \"cm\""
	       "}");
  y.fromNDArray("bytes", 4, 2, shape, "cm");
  EXPECT_EQ(x, y);
  // Unicode
  x.reset();
  y.reset();
  x.fromSchema("{"
	       "  \"type\": \"ndarray\","
	       "  \"subtype\": \"string\","
	       "  \"precision\": 4,"
	       "  \"shape\": [2, 3],"
	       "  \"units\": \"cm\","
	       "  \"encoding\": \"UTF8\""
	       "}");
  y.fromNDArray("unicode", 4, 2, shape, "cm");
  EXPECT_EQ(x, y);
}

TEST(Metadata, fromFormat) {
  std::string formatStr = "%f\t%d\t%5s\n";
  {
    // Scalar
    communication::utils::Metadata x;
    x.fromSchema("{"
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
		 "}");
    x.SetString("format_str", formatStr, x.metadata["serializer"]);
    communication::utils::Metadata y;
    y.fromFormat(formatStr);
    EXPECT_EQ(x.metadata, y.metadata);
  }
  {
    // Arrays
    communication::utils::Metadata x;
    x.fromSchema("{"
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
		 "}");
    x.SetString("format_str", formatStr, x.metadata["serializer"]);
    communication::utils::Metadata y;
    y.fromFormat(formatStr, true);
    EXPECT_EQ(x.metadata, y.metadata);
    EXPECT_TRUE(y.isFormatArray());
  }
  {
    // Additional types
    std::string fmt = "%hhi\t%hi\t%lli\t%l64i\t%li\t%i\t%hhu\t%hu\t%llu\t%l64u\t%lu\t%u\t%f%+fj\n";
    communication::utils::Metadata x;
#ifdef _MSC_VER
    x.fromSchema("{"
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
		 "}");
#else // _MSC_VER
    x.fromSchema("{"
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
		 "}");
#endif // _MSC_VER
    x.SetString("format_str", fmt, x.metadata["serializer"]);
    communication::utils::Metadata y;
    y.fromFormat(fmt);
    EXPECT_EQ(x.metadata, y.metadata);
  }
  {
    // Singular
    std::string fmt = "%d\n";
    communication::utils::Metadata x;
    x.fromSchema("{"
		 "  \"type\": \"array\","
		 "  \"allowSingular\": true,"
		 "  \"items\": ["
		 "    {"
		 "      \"type\": \"scalar\","
		 "      \"subtype\": \"int\","
		 "      \"precision\": 4"
		 "    }"
		 "  ]"
		 "}");
    x.SetString("format_str", fmt, x.metadata["serializer"]);
    communication::utils::Metadata y;
    y.fromFormat(fmt);
    EXPECT_EQ(x.metadata, y.metadata);
  }
  {
    // Error
    communication::utils::Metadata x;
    EXPECT_THROW(x.fromFormat("%m"), std::exception);
  }
}

TEST(Metadata, fromMetadata) {
  communication::utils::Metadata x;
  EXPECT_THROW(x.fromMetadata("{"), std::exception);
  EXPECT_THROW(x.fromMetadata("\"hello\""), std::exception);
  EXPECT_THROW(x.fromMetadata("{}"), std::exception);
  EXPECT_THROW(x.fromMetadata("{\"__meta__\": \"hello\"}"),
	       std::exception);
  x.fromMetadata("{\"__meta__\": {}}");
  communication::utils::Metadata y;
  y.fromMetadata(x);
  EXPECT_EQ(x.metadata, y.metadata);
}

TEST(Metadata, fromEncode) {
  rapidjson::Value v(true);
  communication::utils::Metadata x;
  x.fromSchema("{\"type\": \"boolean\"}");
  communication::utils::Metadata y;
  y.fromEncode(v);
  EXPECT_EQ(x.metadata, y.metadata);
}

TEST(Metadata, deserialize_errors) {
  rapidjson::VarArgList va;
  communication::utils::Metadata x;
  EXPECT_EQ(x.deserialize("", va), -1);
  bool dst = false, dst2 = false;
  EXPECT_EQ(x.deserialize("true", 1, false, &dst), 1);
  EXPECT_EQ(dst, true);
  EXPECT_EQ(x.deserialize("true", 2, false, &dst, &dst2), -1);
  x.fromSchema("{\"type\": \"boolean\"}");
  EXPECT_EQ(x.deserialize("{invalid:}", va), -1);
  EXPECT_EQ(x.deserialize("\"string\"", va), -1);
  EXPECT_EQ(x.deserialize("true", va), -1);
  bool* dst_ptr = &dst;
  EXPECT_EQ(x.deserialize("true", 1, false, dst_ptr), 1);
  EXPECT_EQ(dst, true);
}

TEST(Metadata, serialize_errors) {
  rapidjson::VarArgList va;
  communication::utils::Metadata x;
  char* buf = (char*)(x.GetAllocator().Malloc(sizeof(char)));
  size_t len = 1;
  EXPECT_EQ(x.serialize(&buf, &len, va), -1);
  x.fromSchema("{\"type\": \"boolean\"}");
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
//   EXPECT_THROW(x.serialize(&buf, &len, 1, true), std::exception);
//   ELF_END_F(realloc);
//   ELF_END;
// #endif // ELF_AVAILABLE
}

TEST(Header, Utilities) {
  communication::utils::Header header;
  rapidjson::StringBuffer buf;
  header.formatBuffer(buf);
  EXPECT_EQ(buf.GetLength(), 0);
  EXPECT_EQ(header.format(), 0);
  EXPECT_TRUE(header == header);
  EXPECT_FALSE(header != header);
  communication::utils::Header header2;
  header2.fromType("integer", false, false);
  EXPECT_FALSE(header == header2);
  EXPECT_TRUE(header != header2);
  header.fromType("integer", false, false);
  EXPECT_FALSE(header == header2);
  EXPECT_TRUE(header != header2);
  header.reset();
}

TEST(Header, for_send) {
  communication::utils::Metadata schema;
  schema.fromSchema("{\"type\": \"string\"}");
  std::string msg = "This is a test message";
  setenv("YGG_MODEL_NAME", "model", 1);
  setenv("YGG_MODEL_COPY", "1", 1);
  communication::utils::Header header_send;
  header_send.for_send(&schema, msg.c_str(), msg.size(), 0);
  header_send.on_send();
  communication::utils::Header header_recv;
  EXPECT_EQ(header_recv.on_recv(header_send.data[0],
				header_send.size_curr),
	    -static_cast<long>(header_send.size_curr));
  header_recv.flags |= HEAD_BUFFER_MASK;
  header_recv.data = &(header_recv.data_);
  EXPECT_EQ(header_recv.on_recv(header_send.data[0],
				header_send.size_curr),
	    header_send.size_curr);
  EXPECT_EQ(header_recv.size_data, msg.size());
  EXPECT_EQ(strcmp(header_recv.data[0], msg.c_str()), 0);
  // Header equality
  communication::utils::Header header_send2;
  header_send2.for_send(&schema, msg.c_str(), msg.size(), 0);
  header_send2.fromMetadata(header_send);
  header_send2.on_send();
  communication::utils::Header header_recv2(true);
  EXPECT_EQ(header_recv2.on_recv(header_send2.data[0],
				 header_send2.size_curr),
	    header_send2.size_curr);
  EXPECT_EQ(header_send, header_send2);
  EXPECT_EQ(header_recv, header_recv2);
  EXPECT_NE(header_recv, header_send);
  // Received message larger than buffer
  std::string msg3(2048, 'a');
  communication::utils::Header header_send3;
  header_send3.for_send(&schema, msg3.c_str(), msg3.size(), 0);
  header_send3.on_send();
  size_t size_buf3 = header_send3.size_curr - 1000;
  char* buf3 = (char*)malloc(size_buf3);
  communication::utils::Header header_recv3(buf3, size_buf3, false);
  EXPECT_EQ(header_recv3.on_recv(header_send3.data[0], size_buf3 - 1), -1);
  free(buf3);
  buf3 = NULL;
  unsetenv("YGG_MODEL_NAME");
  unsetenv("YGG_MODEL_COPY");
}

