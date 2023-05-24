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
}

TEST(Metadata, SetAndGet) {
    communication::utils::Metadata metadata;
  EXPECT_THROW(metadata.getMeta(), std::exception);
  EXPECT_THROW(metadata.getSchema(), std::exception);
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
;
  // Init
  metadata.initMeta();
  metadata.initSchema();
  // Set
  EXPECT_TRUE(metadata.SetMetaInt("test_int", 1));
  EXPECT_TRUE(metadata.SetMetaUint("test_uint", 1u));
  EXPECT_TRUE(metadata.SetMetaBool("test_bool", true));
  EXPECT_TRUE(metadata.SetMetaString("test_string", "a"));
  EXPECT_TRUE(metadata.SetMetaValue("test_value", a));
  const char* id1 = NULL;
  std::string id2 = "";
  EXPECT_TRUE(metadata.SetMetaID("test_id1", &id1));
  EXPECT_TRUE(metadata.SetMetaID("test_id2", id2));
  EXPECT_TRUE(metadata.SetSchemaInt("test_int", 1));
  EXPECT_TRUE(metadata.SetSchemaUint("test_uint", 1u));
  EXPECT_TRUE(metadata.SetSchemaBool("test_bool", true));
  EXPECT_TRUE(metadata.SetSchemaString("test_string", "a"));
  EXPECT_TRUE(metadata.SetSchemaValue("test_value", b));
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
  EXPECT_THROW(metadata.GetMetaInt("test_string"), std::exception);
  EXPECT_THROW(metadata.GetMetaUint("test_string"), std::exception);
  EXPECT_THROW(metadata.GetMetaBool("test_string"), std::exception);
  EXPECT_THROW(metadata.GetMetaString("test_bool"), std::exception);
  EXPECT_THROW(metadata.GetSchemaInt("test_string"), std::exception);
  EXPECT_THROW(metadata.GetSchemaUint("test_string"), std::exception);
  EXPECT_THROW(metadata.GetSchemaBool("test_string"), std::exception);
  EXPECT_THROW(metadata.GetSchemaString("test_bool"), std::exception);
  // Get error on wrong type with optional
  EXPECT_THROW(metadata.GetMetaIntOptional("test_string", 1),
	       std::exception);
  EXPECT_THROW(metadata.GetMetaUintOptional("test_string", 1u),
	       std::exception);
  EXPECT_THROW(metadata.GetMetaBoolOptional("test_string", true),
	       std::exception);
  EXPECT_THROW(metadata.GetMetaStringOptional("test_bool", "a"),
	       std::exception);
  EXPECT_THROW(metadata.GetSchemaIntOptional("test_string", 1),
	       std::exception);
  EXPECT_THROW(metadata.GetSchemaUintOptional("test_string", 1u),
	       std::exception);
  EXPECT_THROW(metadata.GetSchemaBoolOptional("test_string", true),
	       std::exception);
  EXPECT_THROW(metadata.GetSchemaStringOptional("test_bool", "a"),
	       std::exception);
  // Get
  EXPECT_EQ(metadata.GetMetaInt("test_int"), 1);
  EXPECT_EQ(metadata.GetMetaUint("test_uint"), 1u);
  EXPECT_EQ(metadata.GetMetaBool("test_bool"), true);
  EXPECT_EQ(strcmp(metadata.GetMetaString("test_string"), "a"), 0);
  EXPECT_EQ(strcmp(metadata.GetMetaString("test_id1"), id1), 0);
  EXPECT_EQ(strcmp(metadata.GetMetaString("test_id2"), id2.c_str()), 0);
  EXPECT_EQ(metadata.GetSchemaInt("test_int"), 1);
  EXPECT_EQ(metadata.GetSchemaUint("test_uint"), 1u);
  EXPECT_EQ(metadata.GetSchemaBool("test_bool"), true);
  EXPECT_EQ(strcmp(metadata.GetSchemaString("test_string"), "a"), 0);
  // Get optional
  EXPECT_EQ(metadata.GetMetaIntOptional("invalid", 1), 1);
  EXPECT_EQ(metadata.GetMetaUintOptional("invalid", 1u), 1u);
  EXPECT_EQ(metadata.GetMetaBoolOptional("invalid", true), true);
  EXPECT_EQ(strcmp(metadata.GetMetaStringOptional("invalid", "a"), "a"), 0);
  EXPECT_EQ(metadata.GetSchemaIntOptional("invalid", 1), 1);
  EXPECT_EQ(metadata.GetSchemaUintOptional("invalid", 1u), 1u);
  EXPECT_EQ(metadata.GetSchemaBoolOptional("invalid", true), true);
  EXPECT_EQ(strcmp(metadata.GetSchemaStringOptional("invalid", "a"), "a"), 0);
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
  }
  {
    // Additional types
    std::string fmt = "%hhi\t%hi\t%lli\t%l64i\t%li\t%i\t%hhu\t%hu\t%llu\t%l64u\t%lu\t%u\n";
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
  EXPECT_THROW(x.deserialize("", va), std::exception);
  x.fromSchema("{\"type\": \"boolean\"}");
  EXPECT_THROW(x.deserialize("{invalid:}", va), std::exception);
  EXPECT_THROW(x.deserialize("\"string\"", va), std::exception);
  EXPECT_THROW(x.deserialize("true", va), std::exception);
  bool dst = false;
  bool* dst_ptr = &dst;
  EXPECT_EQ(x.deserialize("true", 1, false, dst_ptr), 1);
  EXPECT_EQ(dst, true);
}

TEST(Metadata, serialize_errors) {
  rapidjson::VarArgList va;
  communication::utils::Metadata x;
  char* buf = (char*)malloc(sizeof(char));
  size_t len = 1;
  EXPECT_THROW(x.serialize(&buf, &len, va), std::exception);
  x.fromSchema("{\"type\": \"boolean\"}");
  EXPECT_THROW(x.serialize(&buf, &len, va), std::exception);
  EXPECT_EQ(x.serialize(&buf, &len, 1, true), 4);
  EXPECT_EQ(strcmp(buf, "true"), 0);
  EXPECT_EQ(len, 5);
  free(buf);
  free(buf);
  buf = NULL;
  len = 0;
#ifdef ELF_AVAILABLE
  ELF_BEGIN;
  ELF_BEGIN_F(realloc);
  EXPECT_THROW(x.serialize(&buf, &len, 1, true), std::exception);
  std::cerr << "After realloc test" << std::endl;
  ELF_END_F(realloc);
  ELF_END;
  std::cerr << "After realloc restored" << std::endl;
#endif // ELF_AVAILABLE
}

TEST(Header, for_send) {
  communication::utils::Metadata schema;
  schema.fromSchema("{\"type\": \"string\"}");
  std::string msg = "This is a test message";
    communication::utils::Header header_send;
  header_send.for_send(&schema, msg.c_str(), msg.size());
  header_send.format(msg.c_str(), msg.size(), 2048);
    communication::utils::Header header_recv;
  EXPECT_THROW(header_recv.for_recv(header_send.data,
				    header_send.size_curr,
				    1, false), std::exception);
  header_recv.for_recv(header_send.data, header_send.size_curr,
		       header_send.size_curr, true);
  EXPECT_EQ(header_recv.size_data, msg.size());
  EXPECT_EQ(strcmp(header_recv.data[0], msg.c_str()), 0);
}

