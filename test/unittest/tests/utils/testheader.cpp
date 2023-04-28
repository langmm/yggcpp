#include <sstream>
#include <algorithm>
#include <cstdlib>
#include "../../unittest.hpp"
#include "utils/serialization.hpp"

TEST(Metadata, Utilities) {
  Metadata metadata;
  EXPECT_TRUE(metadata.empty());
  EXPECT_FALSE(metadata.hasType());
  EXPECT_FALSE(metadata.hasSubtype());
  metadata.setGeneric();
  EXPECT_TRUE(metadata.isGeneric());
  EXPECT_FALSE(metadata.empty());
}

TEST(Metadata, SetAndGet) {
  Metadata metadata;
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
  Metadata x;
  x.setGeneric();
  EXPECT_TRUE(x.isGeneric());
  x.fromSchema("{\"type\": \"integer\"}");
  EXPECT_TRUE(x.isGeneric());
  Metadata y;
  y.fromSchema(x.metadata, true);
  EXPECT_EQ(x.metadata, y.metadata);
  y.fromSchema(x.getSchema());
  Metadata z;
  z.fromSchema("{\"type\": \"string\"}");
  EXPECT_THROW(y.fromSchema(z.getSchema()), std::exception);
  Metadata q;
  q.fromType("string");
  EXPECT_EQ(z.metadata, q.metadata);
}

TEST(Metadata, fromScalar) {
  Metadata x;
  x.fromSchema("{"
	       "  \"type\": \"scalar\","
	       "  \"subtype\": \"float\","
	       "  \"precision\": 4,"
	       "  \"units\": \"cm\""
	       "}");
  Metadata y;
  y.fromScalar("float", 4, "cm");
  EXPECT_EQ(x.metadata, y.metadata);
  std::cerr << x.metadata << std::endl;
  EXPECT_TRUE(y.hasType());
  EXPECT_TRUE(y.hasSubtype());
  EXPECT_EQ(strcmp(y.typeName(), "scalar"), 0);
  EXPECT_EQ(strcmp(y.subtypeName(), "float"), 0);
}
TEST(Metadata, fromNDArray) {
  Metadata x;
  x.fromSchema("{"
	       "  \"type\": \"ndarray\","
	       "  \"subtype\": \"float\","
	       "  \"precision\": 4,"
	       "  \"shape\": [2, 3],"
	       "  \"units\": \"cm\""
	       "}");
  Metadata y;
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
    Metadata x;
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
    Metadata y;
    y.fromFormat(formatStr);
    EXPECT_EQ(x.metadata, y.metadata);
  }
  {
    // Arrays
    Metadata x;
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
    Metadata y;
    y.fromFormat(formatStr, true);
    EXPECT_EQ(x.metadata, y.metadata);
  }
}

TEST(Metadata, fromMetadata) {
  Metadata x;
  EXPECT_THROW(x.fromMetadata("{"), std::exception);
  EXPECT_THROW(x.fromMetadata("\"hello\""), std::exception);
  EXPECT_THROW(x.fromMetadata("{}"), std::exception);
  EXPECT_THROW(x.fromMetadata("{\"__meta__\": \"hello\"}"),
	       std::exception);
  x.fromMetadata("{\"__meta__\": {}}");
  Metadata y;
  y.fromMetadata(x);
  EXPECT_EQ(x.metadata, y.metadata);
}

TEST(Metadata, fromEncode) {
  rapidjson::Value v(true);
  Metadata x;
  x.fromSchema("{\"type\": \"boolean\"}");
  Metadata y;
  y.fromEncode(v);
  EXPECT_EQ(x.metadata, y.metadata);
}

TEST(Header, for_send) {
  Metadata schema;
  schema.fromSchema("{\"type\": \"string\"}");
  std::string msg = "This is a test message";
  Header header_send;
  header_send.for_send(&schema, msg.c_str(), msg.size());
  header_send.format(msg.c_str(), msg.size(), 2048);
  Header header_recv;
  EXPECT_THROW(header_recv.for_recv(header_send.data,
				    header_send.size_curr,
				    1, false), std::exception);
  header_recv.for_recv(header_send.data, header_send.size_curr,
		       header_send.size_curr, true);
  EXPECT_EQ(header_recv.size_data, msg.size());
  EXPECT_EQ(strcmp(header_recv.data[0], msg.c_str()), 0);
}
