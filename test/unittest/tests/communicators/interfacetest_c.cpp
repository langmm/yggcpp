#include "../../unittest.hpp"
#include "YggInterface.h"
#include "communicators/DefaultComm.hpp"
#include "commtest.hpp"

using namespace communication::communicator;

#define INIT_INPUT_BASE(cls, cls_args, alt, alt_args)	\
  alt sComm alt_args;					\
  setenv("input_IN", sComm.getAddress().c_str(), 1);	\
  comm_t rComm = cls cls_args
#define INIT_OUTPUT_BASE(cls, cls_args, alt, alt_args)		\
  alt rComm alt_args;						\
  setenv("output_OUT", rComm.getAddress().c_str(), 1);		\
  comm_t sComm = cls cls_args
#define INIT_INPUT_NOARGS(cls)					\
  INIT_INPUT_BASE(ygg ## cls ## Input, ("input"), COMM_BASE,	\
		  ("", nullptr, SEND))
#define INIT_OUTPUT_NOARGS(cls)					\
  INIT_OUTPUT_BASE(ygg ## cls ## Output, ("output"), COMM_BASE,	\
		   ("", nullptr, RECV))
#define INIT_INPUT(cls, ...)						\
  INIT_INPUT_BASE(ygg ## cls ## Input, ("input", __VA_ARGS__),		\
		  COMM_BASE,						\
		  ("", nullptr, SEND))
#define INIT_OUTPUT(cls, ...)						\
  INIT_OUTPUT_BASE(ygg ## cls ## Output, ("output", __VA_ARGS__),	\
		   COMM_BASE,						\
		   ("", nullptr, RECV))
#define DO_SEND_RECV_BASE_C(init_data, comp_data, send_method, recv_method, send_eof, afterSendRecv, finally) \
  init_data;								\
  EXPECT_GE(send_method, 0);						\
  EXPECT_GE(recv_method, 0);						\
  comp_data;								\
  EXPECT_TRUE(afterSendRecv);						\
  EXPECT_GE(send_eof, 0);						\
  EXPECT_EQ(recv_method, -2);						\
  EXPECT_TRUE(afterSendRecv);						\
  finally
#define DO_SEND_RECV_INPUT(init_data, comp_data, send_method, send_args, recv_method, recv_args) \
  DO_SEND_RECV_BASE_C(init_data, comp_data, sComm.send_method send_args, recv_method(rComm, UNPACK_MACRO recv_args), sComm.send_eof(), sComm.afterSendRecv(&sComm, (Comm_t*)(rComm.comm)), ygg_free(&rComm))
#define DO_SEND_RECV_OUTPUT(init_data, comp_data, send_method, send_args, recv_method, recv_args) \
  DO_SEND_RECV_BASE_C(init_data, comp_data, send_method(sComm, UNPACK_MACRO send_args), rComm.recv_method recv_args, ygg_send_eof(sComm), rComm.afterSendRecv((Comm_t*)(sComm.comm), &rComm), ygg_free(&sComm))
#define INTERFACE_TEST_BASE(name, init_cls_in, init_cls_out, init_data, comp_data, send_method, send_args, send_method_cpp, send_args_cpp, recv_method, recv_args, recv_method_cpp, recv_args_cpp) \
  TEST(YggInterface_C, name ## _input) {				\
    init_cls_in;							\
    DO_SEND_RECV_INPUT(init_data, comp_data,				\
		       send_method_cpp, send_args_cpp,			\
		       recv_method, recv_args);				\
  }									\
  TEST(YggInterface_C, name ## _output) {				\
    init_cls_out;							\
    DO_SEND_RECV_OUTPUT(init_data, comp_data,				\
			send_method, send_args,				\
			recv_method_cpp, recv_args_cpp);		\
  }
#define INTERFACE_TEST(name, init_cls_in, init_cls_out, init_data, comp_data, send_method, send_args, send_args_cpp, recv_method, recv_args, recv_args_cpp) \
  INTERFACE_TEST_BASE(name, init_cls_in, init_cls_out, init_data, comp_data, send_method, send_args, send, send_args_cpp, recv_method, recv_args, recv, recv_args_cpp)
#define INTERFACE_TEST_REALLOC(name, init_cls_in, init_cls_out, init_data, comp_data, send_method, send_args, send_args_cpp, recv_method, recv_args, recv_args_cpp) \
  INTERFACE_TEST_BASE(name, init_cls_in, init_cls_out, init_data, comp_data, send_method, send_args, send, send_args_cpp, recv_method, recv_args, recvRealloc, recv_args_cpp)
#define INTERFACE_TEST_GEOM(name, cpp_name, c_name)			\
  INTERFACE_TEST_BASE(name,						\
		      INIT_INPUT_NOARGS(name),				\
		      INIT_OUTPUT_NOARGS(name),				\
		      INIT_DATA_GEOM_C(cpp_name, c_name),		\
		      COMP_DATA_GEOM_C(cpp_name),			\
		      yggSend, (data_send),				\
		      sendVar, (*((rapidjson::cpp_name*)(data_send.obj))), \
		      yggRecv, (&data_recv),				\
		      recvVar, (*((rapidjson::cpp_name*)(data_recv.obj))))
#define INTERFACE_TEST_SCHEMA(name, schema)				\
  INTERFACE_TEST_BASE(name,						\
		      INIT_INPUT_NOARGS(name),				\
		      INIT_OUTPUT_NOARGS(name),				\
		      INIT_DATA_SCHEMA_C(schema),			\
		      compare_generic(data_send, data_recv),		\
		      /* delete data_send_doc; delete data_recv_doc, */	\
		      yggSend, (data_send), sendVar, (*data_send_doc),	\
		      yggRecv, (&data_recv), recvVar, (*data_recv_doc))

INTERFACE_TEST(Base,
	       INIT_INPUT_BASE(yggInput, ("input"), COMM_BASE,
			       ("", nullptr, SEND)),
	       INIT_OUTPUT_BASE(yggOutput, ("output"), COMM_BASE,
				("", nullptr, RECV)),
	       INIT_DATA_CHAR, COMP_DATA_CHAR,
	       ygg_send, (data_send, n_send), (data_send),
	       ygg_recv, (data_recv, n_recv), (data_recv))

INTERFACE_TEST(
  Type,
  INIT_INPUT_BASE(
    yggInputType,
    ("input", create_dtype_from_schema("{\"type\": \"number\"}", false)),
    COMM_BASE,
    ("", nullptr, SEND)); sComm.addSchema("{\"type\": \"number\"}"),
  INIT_OUTPUT_BASE(
    yggOutputType,
    ("output", create_dtype_from_schema("{\"type\": \"number\"}", false)),
    COMM_BASE,
    ("", nullptr, RECV)),
  INIT_DATA_SINGLE(double, 1.5), COMP_DATA_SINGLE,
  yggSend, (data_send), (1, data_send),
  yggRecv, (&data_recv), (1, &data_recv))

INTERFACE_TEST(
  Format,
  INIT_INPUT_BASE(
    yggInputFmt,
    ("input", "%d\t%lf\t%5s"),
    COMM_BASE,
    ("", nullptr, SEND)); sComm.addFormat("%d\t%lf\t%5s"),
  INIT_OUTPUT_BASE(
    yggOutputFmt,
    ("output", "%d\t%lf\t%5s"),
    COMM_BASE,
    ("", nullptr, RECV)),
  INIT_DATA_TRIPLE, COMP_DATA_TRIPLE,
  yggSend, (a_send, b_send, c_send, nc_send), SEND_NARGS_TRIPLE,
  yggRecv, (&a_recv, &b_recv, &c_recv, &nc_recv), RECV_NARGS_TRIPLE)
  
INTERFACE_TEST(
  AsciiTable,
  INIT_INPUT_NOARGS(AsciiTable); sComm.addFormat("%d\t%lf\t%5s"),
  INIT_OUTPUT(AsciiTable, "%d\t%lf\t%5s"),
  INIT_DATA_TRIPLE, COMP_DATA_TRIPLE,
  yggSend, (a_send, b_send, c_send, nc_send), SEND_NARGS_TRIPLE,
  yggRecv, (&a_recv, &b_recv, &c_recv, &nc_recv), RECV_NARGS_TRIPLE)

INTERFACE_TEST(
  AsciiArray,
  INIT_INPUT_NOARGS(AsciiArray); sComm.addFormat("%d\t%lf\t%10s", true),
  INIT_OUTPUT(AsciiArray, "%d\t%lf\t%10s"),
  INIT_DATA_TABLE, COMP_DATA_TABLE,
  yggSend, (n_send, a_send, b_send, c_send), SEND_NARGS_TABLE,
  yggRecv, (&n_recv, &a_recv, &b_recv, &c_recv), RECV_NARGS_TABLE)

INTERFACE_TEST_REALLOC(
  AsciiArrayRealloc,
  INIT_INPUT_NOARGS(AsciiArray); sComm.addFormat("%d\t%lf\t%10s", true),
  INIT_OUTPUT(AsciiArray, "%d\t%lf\t%10s"),
  INIT_DATA_TABLE_REALLOC, COMP_DATA_TABLE_REALLOC,
  yggSend, (n_send, a_send, b_send, c_send), SEND_NARGS_TABLE,
  yggRecvRealloc, (&n_recv, &a_recv, &b_recv, &c_recv), RECV_NARGS_TABLE_REALLOC)

INTERFACE_TEST_GEOM(Ply, Ply, ply)
INTERFACE_TEST_GEOM(Obj, ObjWavefront, obj)
INTERFACE_TEST_SCHEMA(Generic, "{\"type\": \"ply\"}")
INTERFACE_TEST_SCHEMA(Any, "{\"type\": \"ply\"}")
INTERFACE_TEST_SCHEMA(JSONArray, "{\"type\": \"array\", \"items\": [{\"type\": \"integer\"}, {\"type\": \"string\"}]}")
INTERFACE_TEST_SCHEMA(JSONObject, "{\"type\": \"object\", \"properties\": {\"a\": {\"type\": \"integer\"}, \"b\": {\"type\": \"string\"}}}")


// TODO: RPC


#undef INTERFACE_TEST_SCHEMA
#undef INTERFACE_TEST_GEOM
#undef INTERFACE_TEST_REALLOC
#undef INTERFACE_TEST
#undef INTERFACE_TEST_BASE
#undef DO_SEND_RECV_OUTPUT
#undef DO_SEND_RECV_INPUT
#undef DO_SEND_RECV_BASE_C
#undef INIT_OUTPUT
#undef INIT_INPUT
#undef INIT_OUTPUT_NOARGS
#undef INIT_INPUT_NOARGS
#undef INIT_OUTPUT_BASE
#undef INIT_INPUT_BASE
