#include "../../unittest.hpp"
#include "YggInterface.hpp"
#include "commtest.hpp"

#define INIT_INPUT_BASE(cls, cls_args, alt, alt_args)	\
  std::cerr << "before sComm" << std::endl;		\
  alt sComm alt_args;					\
  std::cerr << "after sComm" << std::endl;		\
  setenv("input_IN", sComm.getAddress().c_str(), 1);	\
  std::cerr << "before rComm" << std::endl;		\
  cls rComm cls_args;					\
  std::cerr << "after rComm" << std::endl;		\
  unsetenv("input_IN")
#define INIT_OUTPUT_BASE(cls, cls_args, alt, alt_args)	\
  alt rComm alt_args;					\
  setenv("output_OUT", rComm.getAddress().c_str(), 1);	\
  cls sComm cls_args;					\
  unsetenv("output_OUT")
#define INIT_INPUT_NOARGS(cls)				\
  INIT_INPUT_BASE(cls ## Input, ("input"), COMM_BASE,	\
		  ("", nullptr, SEND))
#define INIT_OUTPUT_NOARGS(cls)					\
  INIT_OUTPUT_BASE(cls ## Output, ("output"), COMM_BASE,	\
		   ("", nullptr, RECV))
#define INIT_INPUT(cls, ...)						\
  INIT_INPUT_BASE(cls ## Input, ("input", __VA_ARGS__), COMM_BASE,	\
		  ("", nullptr, SEND))
#define INIT_OUTPUT(cls, ...)						\
  INIT_OUTPUT_BASE(cls ## Output, ("output", __VA_ARGS__), COMM_BASE,	\
		   ("", nullptr, RECV))
#define INIT_INPUT_RPC_NOARGS(cls)				\
  INIT_INPUT_BASE(cls, ("input"), ClientComm, ("", nullptr))
#define INIT_OUTPUT_RPC_NOARGS(cls)				\
  INIT_OUTPUT_BASE(cls, ("output"), ServerComm, ("", nullptr))
#define INIT_INPUT_RPC(cls, ...)			   \
  INIT_INPUT_BASE(cls, ("input", __VA_ARGS__), ClientComm, \
		  ("", nullptr))
#define INIT_OUTPUT_RPC(cls, ...)				\
  INIT_OUTPUT_BASE(cls, ("output", __VA_ARGS__), ServerComm,	\
		   ("", nullptr))
#define TRANSFER_INPUT_TYPE(init)		\
  sComm.copySchema(&rComm);			\
  init
#define INIT_SCHEMA(schema, init)		\
  rapidjson::Document s;			\
  s.Parse(schema);				\
  init;						\
  TRANSFER_INPUT_TYPE(EXPECT_TRUE(s.IsObject()))
#define INIT_FORMAT(init, format_str, as_array)	\
  init;						\
  sComm.addFormat(format_str, as_array)
#define INIT_SCHEMA_RPC(init, schema1, schema2)	\
  rapidjson::Document s1;			\
  rapidjson::Document s2;			\
  s1.Parse(schema1);				\
  s2.Parse(schema2);				\
  init;						\
  TRANSFER_INPUT_TYPE(EXPECT_TRUE(s1.IsObject()))

#define INTERFACE_TEST(name, cls, init_data, comp_data, send_method, send_args, recv_method, recv_args, ...) \
  TEST_SEND_RECV(YggInterface, name ## _input,				\
		 INIT_INPUT(cls, __VA_ARGS__),				\
		 init_data, comp_data, send_method, send_args,		\
		 recv_method, recv_args)				\
  TEST_SEND_RECV(YggInterface, name ## _output,				\
		 INIT_OUTPUT(cls, __VA_ARGS__),				\
		 init_data, comp_data, send_method, send_args,		\
		 recv_method, recv_args)
#define INTERFACE_TEST_NOARGS(name, cls, init_data, comp_data, send_method, send_args, recv_method, recv_args) \
  TEST_SEND_RECV(YggInterface, name ## _input,				\
		 INIT_INPUT_NOARGS(cls),				\
		 init_data, comp_data, send_method, send_args,		\
		 recv_method, recv_args)				\
  TEST_SEND_RECV(YggInterface, name ## _output,				\
		 INIT_OUTPUT_NOARGS(cls),				\
		 init_data, comp_data, send_method, send_args,		\
		 recv_method, recv_args)
#define INTERFACE_TEST_SCHEMA(name, cls, init_data, comp_data, send_method, send_args, recv_method, recv_args, schema) \
  TEST_SEND_RECV(YggInterface, name ## _input,				\
		 INIT_SCHEMA(schema, INIT_INPUT(cls, s)),		\
		 init_data, comp_data, send_method, send_args,		\
		 recv_method, recv_args)				\
  TEST_SEND_RECV(YggInterface, name ## _output,				\
		 INIT_SCHEMA(schema, INIT_OUTPUT(cls, s)),		\
		 init_data, comp_data, send_method, send_args,		\
		 recv_method, recv_args)
  

INTERFACE_TEST_NOARGS(String, Ygg,
		      INIT_DATA_SINGLE(std::string,
				       "This is a test message"),
		      COMP_DATA_SINGLE,
		      send, (data_send), recv, (data_recv))
INTERFACE_TEST_NOARGS(VarString, Ygg,
		      INIT_DATA_SINGLE(std::string,
				       "This is a test message"),
		      COMP_DATA_SINGLE,
		      sendVar, (data_send), recvVar, (data_recv))
INTERFACE_TEST_NOARGS(VarDouble, Ygg,
		      INIT_DATA_SINGLE(double, 1.5),
		      COMP_DATA_SINGLE,
		      sendVar, (data_send), recvVar, (data_recv))
INTERFACE_TEST(Format, Ygg,
	       TRANSFER_INPUT_TYPE(INIT_DATA_TRIPLE), COMP_DATA_TRIPLE,
	       send, SEND_NARGS_TRIPLE, recv, RECV_NARGS_TRIPLE,
	       "%d\t%lf\t%5s")
INTERFACE_TEST(FormatSingle, Ygg,
	       INIT_DATA_SINGLE(double, 1.5), COMP_DATA_SINGLE,
	       sendVar, (data_send), recvVar, (data_recv), "%lf")
INTERFACE_TEST(FormatArray, Ygg,
	       TRANSFER_INPUT_TYPE(INIT_DATA_TABLE), COMP_DATA_TABLE,
	       send, SEND_NARGS_TABLE, recv, RECV_NARGS_TABLE,
	       "%d\t%lf\t%10s", true)
INTERFACE_TEST(FormatArrayRealloc, Ygg,
	       TRANSFER_INPUT_TYPE(INIT_DATA_TABLE_REALLOC),
	       COMP_DATA_TABLE_REALLOC,
	       send, SEND_NARGS_TABLE, recvRealloc,
	       RECV_NARGS_TABLE_REALLOC,
	       "%d\t%lf\t%10s", true)

INTERFACE_TEST_SCHEMA(Schema, Ygg,
		      INIT_DATA_SINGLE(double, 1.5),
		      COMP_DATA_SINGLE,
		      sendVar, (data_send), recvVar, (data_recv),
		      "{\"type\": \"number\"}")

// Client
TEST_SEND_RECV_RPC(YggInterface, YggRpcServer,
		   INIT_INPUT_RPC_NOARGS(YggRpcServer),
		   INIT_DATA_SINGLE(std::string,
				    "This is a test message"),
		   COMP_DATA_SINGLE,
		   send, (data_send), recv, (data_recv))
TEST_SEND_RECV_RPC(YggInterface, YggRpcClient,
		   INIT_OUTPUT_RPC_NOARGS(YggRpcClient),
		   INIT_DATA_SINGLE(std::string,
				    "This is a test message"),
		   COMP_DATA_SINGLE,
		   send, (data_send), recv, (data_recv))
TEST_SEND_RECV_RPC(YggInterface, YggRpcServer_format,
		   INIT_INPUT_RPC(YggRpcServer, "%lf", "%lf"),
		   INIT_DATA_SINGLE(double, 1.5),
		   COMP_DATA_SINGLE,
		   sendVar, (data_send), recvVar, (data_recv))
TEST_SEND_RECV_RPC(YggInterface, YggRpcClient_format,
		   INIT_OUTPUT_RPC(YggRpcClient, "%lf", "%lf"),
		   INIT_DATA_SINGLE(double, 1.5),
		   COMP_DATA_SINGLE,
		   sendVar, (data_send), recvVar, (data_recv))
TEST_SEND_RECV_RPC(YggInterface, YggRpcServer_schema,
                   INIT_SCHEMA_RPC(INIT_INPUT_RPC(YggRpcServer, s1, s2),
                                   "{\"type\": \"integer\"}",
                                   "{\"type\": \"integer\"}"),
                   INIT_DATA_SINGLE(int, 5),
                   COMP_DATA_SINGLE,
                   sendVar, (data_send), recvVar, (data_recv))
TEST_SEND_RECV_RPC(YggInterface, YggRpcClient_schema,
                   INIT_SCHEMA_RPC(INIT_OUTPUT_RPC(YggRpcClient, s1, s2),
                                   "{\"type\": \"integer\"}",
                                   "{\"type\": \"integer\"}"),
                   INIT_DATA_SINGLE(int, 5),
                   COMP_DATA_SINGLE,
                   sendVar, (data_send), recvVar, (data_recv))
TEST_SEND_RECV_RPC_GLOBAL(
  YggInterface, YggRpcClient_schema,
  INIT_SCHEMA_RPC(INIT_OUTPUT_RPC(YggRpcClient, s1, s2),
		  "{\"type\": \"integer\"}",
		  "{\"type\": \"integer\"}"),
  INIT_DATA_SINGLE(int, 5),
  COMP_DATA_SINGLE,
  sendVar, (data_send), recvVar, (data_recv))

// Timesync
TEST(YggInterface, YggTimesync) {
  {
    global_scope_comm_on();
    INIT_OUTPUT_RPC_NOARGS(YggTimesync);
    global_scope_comm_off();
    rComm.addResponseSchema("{\"type\": \"object\"}");
    DO_RPC_SIGNON;
    {
      double t_send = 1.5;
      double t_recv;
      DO_SEND_RECV_REQUEST(INIT_DATA_SCHEMA("{\"type\": \"object\", "
					    "\"properties\": {\"a\": "
					    "{\"type\": \"integer\"}}}"),
			   COMP_DATA_SINGLE,
			   send, (2, t_send, &data_send),
			   recv, (2, &t_recv, &data_recv));
      EXPECT_EQ(t_send, t_recv);
    }
    {
      DO_SEND_RECV_RESPONSE(INIT_DATA_SCHEMA("{\"type\": \"object\", "
					     "\"properties\": {\"a\": "
					     "{\"type\": \"integer\"}}}"),
			    COMP_DATA_SINGLE,
			    send, (1, &data_send),
			    recv, (1, &data_recv));
    }
  }
  Comm_t::_ygg_cleanup();
}
TEST(YggInterface, YggTimesync_units) {
  INIT_OUTPUT_RPC(YggTimesync, "s");
  rComm.addResponseSchema("{\"type\": \"object\"}");
  DO_RPC_SIGNON;
  {
    double t_send = 1.5;
    double t_recv;
    DO_SEND_RECV_REQUEST(INIT_DATA_SCHEMA("{\"type\": \"object\", "
					  "\"properties\": {\"a\": "
					  "{\"type\": \"integer\"}}}"),
			 COMP_DATA_SINGLE,
			 send, (2, t_send, &data_send),
			 recv, (2, &t_recv, &data_recv));
    EXPECT_EQ(t_send, t_recv);
  }
  {
    DO_SEND_RECV_RESPONSE(INIT_DATA_SCHEMA("{\"type\": \"object\", "
					   "\"properties\": {\"a\": "
					   "{\"type\": \"integer\"}}}"),
			  COMP_DATA_SINGLE,
			  send, (1, &data_send),
			  recv, (1, &data_recv));
  }
}

// AsciiFile
TEST_SEND_RECV(YggInterface, AsciiFile_input,
	       INIT_INPUT_NOARGS(YggAsciiFile),
	       INIT_DATA_CHAR, COMP_DATA_CHAR,
	       send, (data_send, n_send),
	       recv_line, (data_recv, n_recv))
TEST_SEND_RECV(YggInterface, AsciiFile_output,
	       INIT_OUTPUT_NOARGS(YggAsciiFile),
	       INIT_DATA_CHAR, COMP_DATA_CHAR,
	       send_line, (data_send),
	       recv, (data_recv))

// AsciiTable
TEST_SEND_RECV(YggInterface, AsciiTable_input,
	       INIT_FORMAT(INIT_INPUT_NOARGS(YggAsciiTable),
			   "%d\t%lf\t%5s", false),
	       INIT_DATA_TRIPLE, COMP_DATA_TRIPLE,
	       send, SEND_NARGS_TRIPLE, recv, RECV_NARGS_TRIPLE)
TEST_SEND_RECV(YggInterface, AsciiTable_output,
	       INIT_OUTPUT(YggAsciiTable, "%d\t%lf\t%5s"),
	       INIT_DATA_TRIPLE, COMP_DATA_TRIPLE,
	       send, SEND_NARGS_TRIPLE, recv, RECV_NARGS_TRIPLE)

// AsciiArray
TEST_SEND_RECV(YggInterface, AsciiArray_input,
	       INIT_FORMAT(INIT_INPUT_NOARGS(YggAsciiArray),
			   "%d\t%lf\t%10s", true),
	       INIT_DATA_TABLE, COMP_DATA_TABLE,
	       send, SEND_NARGS_TABLE, recv, RECV_NARGS_TABLE)
TEST_SEND_RECV(YggInterface, AsciiArray_output,
	       INIT_OUTPUT(YggAsciiArray, "%d\t%lf\t%10s"),
	       INIT_DATA_TABLE, COMP_DATA_TABLE,
	       send, SEND_NARGS_TABLE, recv, RECV_NARGS_TABLE)

// Ply
INTERFACE_TEST_NOARGS(Ply, YggPly,
		      INIT_DATA_PLY, COMP_DATA_SINGLE,
		      sendVar, (data_send), recvVar, (data_recv))

// Obj
INTERFACE_TEST_NOARGS(Obj, YggObj,
		      INIT_DATA_OBJ, COMP_DATA_SINGLE,
		      sendVar, (data_send), recvVar, (data_recv))

// Generic
INTERFACE_TEST_NOARGS(Generic, YggGeneric,
		      TRANSFER_INPUT_TYPE(
			 INIT_DATA_SCHEMA("{\"type\": \"array\", "
					  "\"items\": "
					  "{\"type\": \"integer\"}}")),
		      COMP_DATA_SINGLE,
		      sendVar, (data_send), recvVar, (data_recv))
INTERFACE_TEST_NOARGS(Any, YggAny,
		      TRANSFER_INPUT_TYPE(
			 INIT_DATA_SCHEMA("{\"type\": \"array\", "
					  "\"items\": "
					  "{\"type\": \"integer\"}}")),
		      COMP_DATA_SINGLE,
		      sendVar, (data_send), recvVar, (data_recv))

// JSONArray
INTERFACE_TEST_NOARGS(JSONArray, YggJSONArray,
		      TRANSFER_INPUT_TYPE(
			 INIT_DATA_SCHEMA("{\"type\": \"array\", "
					  "\"items\": "
					  "{\"type\": \"integer\"}}")),
		      COMP_DATA_SINGLE,
		      sendVar, (data_send), recvVar, (data_recv))

// JSONObject
INTERFACE_TEST_NOARGS(JSONObject, YggJSONObject,
		      TRANSFER_INPUT_TYPE(
			 INIT_DATA_SCHEMA("{\"type\": \"object\", "
					  "\"properties\": {\"a\": "
					  "{\"type\": \"integer\"}}}")),
		      COMP_DATA_SINGLE,
		      sendVar, (data_send), recvVar, (data_recv))

TEST(YggInterface, GlobalServerPiecemeal) {
  {
    std::string name = "test_name";
    ClientComm sComm(name, nullptr);
    sComm.set_timeout_recv(1000);
    std::string name_req = name + "_input";
    std::string name_res = name + "_output";
    std::string ikey_env = name_req + "_IN";
    std::string ival_env = sComm.getAddress();
    setenv(ikey_env.c_str(), ival_env.c_str(), 1);
    setenv("YGG_SERVER_INPUT", name_req.c_str(), 1);
    setenv("YGG_SERVER_OUTPUT", name_res.c_str(), 1);
    {
      YggInput rComm_req(name_req);
      YggOutput rComm_res(name_res);
      ServerComm& rComm = *(dynamic_cast<ServerComm*>(rComm_req.getGlobalComm()));
      rComm_req.set_timeout_recv(1000);
      DO_RPC_SIGNON;
      // Request
      int req_send = 1, req_recv = 0;
      EXPECT_GE(sComm.sendVar(req_send), 0);
      EXPECT_GE(rComm_req.recvVar(req_recv), 0);
      EXPECT_EQ(req_recv, req_send);
      EXPECT_TRUE(sComm.afterSendRecv(&sComm, &rComm));
      // Response
      int res_send = 2, res_recv = 0;
      EXPECT_GE(rComm_res.sendVar(res_send), 0);
      EXPECT_GE(sComm.recvVar(res_recv), 0);
      EXPECT_EQ(res_recv, res_send);
      EXPECT_TRUE(rComm.afterSendRecv(&rComm, &sComm));
    }
    {
      YggInput rComm_req(name_req);
      YggOutput rComm_res(name_res);
      ServerComm& rComm = *(dynamic_cast<ServerComm*>(rComm_req.getGlobalComm()));
      // Request
      int req_send = 1, req_recv = 0;
      EXPECT_GE(sComm.sendVar(req_send), 0);
      EXPECT_GE(rComm_req.recvVar(req_recv), 0);
      EXPECT_EQ(req_recv, req_send);
      EXPECT_TRUE(sComm.afterSendRecv(&sComm, &rComm));
      // Response
      int res_send = 2, res_recv = 0;
      EXPECT_GE(rComm_res.sendVar(res_send), 0);
      EXPECT_GE(sComm.recvVar(res_recv), 0);
      EXPECT_EQ(res_recv, res_send);
      EXPECT_TRUE(rComm.afterSendRecv(&rComm, &sComm));
    }
    unsetenv(ikey_env.c_str());
    unsetenv("YGG_SERVER_INPUT");
    unsetenv("YGG_SERVER_OUTPUT");
  }
  Comm_t::_ygg_cleanup();
}

#undef INTERFACE_TEST_SCHEMA
#undef INTERFACE_TEST_NOARGS
#undef INTERFACE_TEST
#undef INIT_SCHEMA_RPC
#undef INIT_FORMAT
#undef INIT_SCHEMA
#undef TRANSFER_INPUT_TYPE
#undef INIT_OUTPUT_RPC
#undef INIT_INPUT_RPC
#undef INIT_OUTPUT_RPC_NOARGS
#undef INIT_INPUT_RPC_NOARGS
#undef INIT_OUTPUT
#undef INIT_INPUT
#undef INIT_OUTPUT_NOARGS
#undef INIT_INPUT_NOARGS
#undef INIT_OUTPUT_BASE
#undef INIT_INPUT_BASE
