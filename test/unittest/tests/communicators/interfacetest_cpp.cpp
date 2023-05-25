#include "../../unittest.hpp"
#include "YggInterface.hpp"
#include "commtest.hpp"

#define INIT_INPUT_NOARGS(cls)				\
  COMM_BASE sComm("", nullptr, SEND);			\
  setenv("input_IN", sComm.getAddress().c_str(), 1);	\
  cls ## Input rComm("input")
#define INIT_OUTPUT_NOARGS(cls)				\
  COMM_BASE rComm("", nullptr, RECV);			\
  setenv("output_OUT", rComm.getAddress().c_str(), 1);	\
  cls ## Output sComm("output")
#define INIT_INPUT(cls, ...)				\
  COMM_BASE sComm("", nullptr, SEND);			\
  setenv("input_IN", sComm.getAddress().c_str(), 1);	\
  cls ## Input rComm("input", __VA_ARGS__)
#define INIT_OUTPUT(cls, ...)				\
  COMM_BASE rComm("", nullptr, RECV);			\
  setenv("output_OUT", rComm.getAddress().c_str(), 1);	\
  cls ## Output sComm("output", __VA_ARGS__)
#define TRANSFER_INPUT_TYPE(init)		\
  sComm.copySchema(&rComm);			\
  init
#define INIT_SCHEMA(schema, init)		\
  rapidjson::Document s;			\
  s.Parse(schema);				\
  init;						\
  TRANSFER_INPUT_TYPE()
#define INIT_FORMAT(init, format_str, as_array)	\
  init;						\
  sComm.addFormat(format_str, as_array)

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

// TODO:
//  - test client/server
//  - test timesync

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
