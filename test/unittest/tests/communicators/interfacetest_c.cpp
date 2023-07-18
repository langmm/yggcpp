#include "../../unittest.hpp"
#include "YggInterface.h"
#include "communicators/comms.hpp"
#include "commtest.hpp"

using namespace communication::communicator;

#define INIT_INPUT_BASE(cls, cls_args, alt, alt_args)	\
  alt sComm alt_args;					\
  setenv("input_IN", sComm.getAddress().c_str(), 1);	\
  comm_t rComm = cls cls_args;				\
  EXPECT_EQ(set_response_format(rComm, "%s"), 0);	\
  unsetenv("input_IN")
#define INIT_OUTPUT_BASE(cls, cls_args, alt, alt_args)		\
  alt rComm alt_args;						\
  setenv("output_OUT", rComm.getAddress().c_str(), 1);		\
  comm_t sComm = cls cls_args;					\
  unsetenv("output_OUT")
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
#define INTERFACE_TEST_BASE(name, init_cls_in, init_cls_out, init_data, comp_data, send_method, send_args, send_method_cpp, send_args_cpp, recv_method, recv_args, recv_method_cpp, recv_args_cpp, cleanup) \
  TEST(YggInterface_C, name ## _input) {				\
    init_cls_in;							\
    DO_SEND_RECV_INPUT(init_data, comp_data,				\
		       send_method_cpp, send_args_cpp,			\
		       recv_method, recv_args);				\
    cleanup;								\
  }									\
  TEST(YggInterface_C, name ## _output) {				\
    init_cls_out;							\
    DO_SEND_RECV_OUTPUT(init_data, comp_data,				\
			send_method, send_args,				\
			recv_method_cpp, recv_args_cpp);		\
    cleanup;								\
  }
#define INTERFACE_TEST(name, init_cls_in, init_cls_out, init_data, comp_data, send_method, send_args, send_args_cpp, recv_method, recv_args, recv_args_cpp) \
  INTERFACE_TEST_BASE(name, init_cls_in, init_cls_out, init_data, comp_data, send_method, send_args, send, send_args_cpp, recv_method, recv_args, recv, recv_args_cpp, )
#define INTERFACE_TEST_REALLOC(name, init_cls_in, init_cls_out, init_data, comp_data, send_method, send_args, send_args_cpp, recv_method, recv_args, recv_args_cpp) \
  INTERFACE_TEST_BASE(name, init_cls_in, init_cls_out, init_data, comp_data, send_method, send_args, send, send_args_cpp, recv_method, recv_args, recvRealloc, recv_args_cpp, )
#define INTERFACE_TEST_GEOM(name, cpp_name, c_name)			\
  INTERFACE_TEST_BASE(name,						\
		      INIT_INPUT_NOARGS(name),				\
		      INIT_OUTPUT_NOARGS(name),				\
		      INIT_DATA_GEOM_C(cpp_name, c_name),		\
		      COMP_DATA_GEOM_C(cpp_name),			\
		      yggSend, (data_send),				\
		      sendVar, (*((rapidjson::cpp_name*)(data_send.obj))), \
		      yggRecv, (&data_recv),				\
		      recvVar, (*((rapidjson::cpp_name*)(data_recv.obj))), \
		      free_ ## c_name(&data_send); free_ ## c_name(&data_recv))
#define INTERFACE_TEST_SCHEMA(name, schema)				\
  INTERFACE_TEST_BASE(name,						\
		      INIT_INPUT_NOARGS(name),				\
		      INIT_OUTPUT_NOARGS(name),				\
		      INIT_DATA_SCHEMA_C(schema),			\
		      compare_generic(data_send, data_recv),		\
		      yggSend, (data_send), sendVar, (*data_send_doc),	\
		      yggRecv, (&data_recv), recvVar, (*data_recv_doc),	\
		      destroy_generic(&data_send); destroy_generic(&data_recv))

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


TEST(YggInterface_C, Server) {
  ClientComm sComm("", nullptr);
  setenv("input_IN", sComm.getAddress().c_str(), 1);
  comm_t rComm_c = yggRpcServer("input", "%s", "%s");
  unsetenv("input_IN");
  ServerComm& rComm = *((ServerComm*)(rComm_c.comm));
  DO_RPC_SIGNON;
  // Request
  const char* req_send = "This is a request message";
  char* req_recv = NULL;
  size_t req_len = strlen(req_send);
  EXPECT_GE(sComm.send(req_send, req_len), 0);
  EXPECT_GT(comm_nmsg(rComm_c), 0);
  EXPECT_EQ(ygg_recv_nolimit(rComm_c, &req_recv, 0), req_len);
  EXPECT_TRUE(req_recv);
  EXPECT_EQ(strcmp(req_send, req_recv), 0);
  EXPECT_TRUE(sComm.afterSendRecv(&sComm, &rComm));
  free(req_recv);
  req_recv = NULL;
  // Response
  const char* res_send = "This is a response message";
  char* res_recv = NULL;
  size_t res_len = strlen(res_send);
  EXPECT_GE(ygg_send(rComm_c, res_send, res_len), 0);
  EXPECT_EQ(sComm.recv(res_recv, 0, true), res_len);
  EXPECT_TRUE(res_recv);
  EXPECT_EQ(strcmp(res_send, res_recv), 0);
  EXPECT_TRUE(rComm.afterSendRecv(&rComm, &sComm));
  free(res_recv);
  res_recv = NULL;
  // EOF
  EXPECT_GE(sComm.send_eof(), 0);
  EXPECT_EQ(ygg_recv_nolimit(rComm_c, &req_recv, 0), -2);
  EXPECT_TRUE(sComm.afterSendRecv(&sComm, &rComm));
  close_comm(&rComm_c);
  free(req_recv);
  req_recv = NULL;
}

TEST(YggInterface_C, Client) {
  ServerComm rComm("", nullptr);
  setenv("output_OUT", rComm.getAddress().c_str(), 1);
  comm_t sComm_c = yggRpcClient("output", "%s", "%s");
  unsetenv("output_OUT");
  ClientComm& sComm = *((ClientComm*)(sComm_c.comm));
  DO_RPC_SIGNON;
  // Stash request
  std::string req_send = "This is a request message";
  std::string req_recv = "";
  std::string msg = "\"" + req_send + "\"";
  {
    Header header;
    EXPECT_TRUE(sComm.create_header_send(header, msg.c_str(), msg.size()));
    size_t len = header.format(msg.c_str(), msg.size(), 0);
    msg.assign(header.data[0], len);
    EXPECT_GE(rComm.getRequests().addRequestServer(header), 0);
    sComm.getRequests().stashRequest();
  }
  // Pre-send response
  std::string res_send = "This is a response message";
  char* res_recv = NULL;
  size_t res_recv_len = 0;
  EXPECT_GE(rComm.sendVar(res_send), 0);
  // Call
  EXPECT_EQ(rpcCallRealloc(sComm_c,
			   req_send.c_str(), req_send.size(),
			   &res_recv, &res_recv_len), 2);
  EXPECT_EQ(res_send.size(), res_recv_len);
  EXPECT_EQ(strcmp(res_send.c_str(), res_recv), 0);
  // Server request
  EXPECT_GE(rComm.recvVar(req_recv), 0);
  EXPECT_EQ(req_recv, req_send);
  // Cleanup
  free(res_recv);
  close_comm(&sComm_c);
}

TEST(YggInterface_C, ServerAny) {
  dtype_t dtype_req = {0};
  dtype_t dtype_res = {0};
  INIT_DATA_SCHEMA_C("{\"type\": \"array\", \"items\": [{\"type\": \"integer\"}]}");
  ClientComm sComm("", nullptr);
  setenv("input_IN", sComm.getAddress().c_str(), 1);
  comm_t rComm_c = yggRpcServerType("input", dtype_req, dtype_res);
  unsetenv("input_IN");
  ServerComm& rComm = *((ServerComm*)(rComm_c.comm));
  DO_RPC_SIGNON;
  // Request
  EXPECT_GE(sComm.sendVar(*data_send_doc), 0);
  EXPECT_GE(yggRecv(rComm_c, &data_recv), 0);
  EXPECT_TRUE(compare_generic(data_recv, data_send));
  EXPECT_TRUE(sComm.afterSendRecv(&sComm, &rComm));
  data_recv_doc->SetNull();
  // Response
  EXPECT_GE(yggSend(rComm_c, data_send), 0);
  EXPECT_GE(sComm.recvVar(*data_recv_doc), 0);
  EXPECT_TRUE(compare_generic(data_recv, data_send));
  EXPECT_TRUE(rComm.afterSendRecv(&rComm, &sComm));
  data_recv_doc->SetNull();
  // EOF
  EXPECT_GE(sComm.send_eof(), 0);
  EXPECT_EQ(yggRecv(rComm_c, &data_recv), -2);
  EXPECT_TRUE(sComm.afterSendRecv(&sComm, &rComm));
  // Cleanup
  close_comm(&rComm_c);
  destroy_generic(&data_send);
  destroy_generic(&data_recv);
}

TEST(YggInterface_C, ClientAny) {
  dtype_t dtype_req = {0};
  dtype_t dtype_res = {0};
  INIT_DATA_SCHEMA_C("{\"type\": \"array\", \"items\": [{\"type\": \"integer\"}]}");
  ServerComm rComm("", nullptr);
  setenv("output_OUT", rComm.getAddress().c_str(), 1);
  comm_t sComm_c = yggRpcClientType("output", dtype_req, dtype_res);
  unsetenv("output_OUT");
  ClientComm& sComm = *((ClientComm*)(sComm_c.comm));
  DO_RPC_SIGNON;
  // Request
  EXPECT_GE(yggSend(sComm_c, data_send), 0);
  EXPECT_GE(rComm.recvVar(*data_recv_doc), 0);
  EXPECT_TRUE(compare_generic(data_recv, data_send));
  EXPECT_TRUE(sComm.afterSendRecv(&sComm, &rComm));
  data_recv_doc->SetNull();
  // Response
  EXPECT_GE(rComm.sendVar(*data_send_doc), 0);
  EXPECT_GE(yggRecv(sComm_c, &data_recv), 0);
  EXPECT_TRUE(compare_generic(data_recv, data_send));
  EXPECT_TRUE(rComm.afterSendRecv(&rComm, &sComm));
  data_recv_doc->SetNull();
  // EOF
  EXPECT_GE(ygg_send_eof(sComm_c), 0);
  EXPECT_EQ(rComm.recvVar(*data_recv_doc), -2);
  EXPECT_TRUE(sComm.afterSendRecv(&sComm, &rComm));
  // Cleanup
  close_comm(&sComm_c);
  destroy_generic(&data_send);
  destroy_generic(&data_recv);
}

TEST(YggInterface_C, ClientPointers) {
  ServerComm rComm("", nullptr);
  setenv("output_OUT", rComm.getAddress().c_str(), 1);
  comm_t sComm_c = yggRpcClient("output", "%s", "%s");
  EXPECT_EQ(set_response_format(sComm_c, "%s"), 1);
  unsetenv("output_OUT");
  ClientComm& sComm = *((ClientComm*)(sComm_c.comm));
  DO_RPC_SIGNON;
  // Stash request
  std::string req_send = "This is a request message";
  std::string req_recv = "";
  std::string msg = "\"" + req_send + "\"";
  {
    Header header;
    EXPECT_TRUE(sComm.create_header_send(header, msg.c_str(), msg.size()));
    size_t len = header.format(msg.c_str(), msg.size(), 0);
    msg.assign(header.data[0], len);
    EXPECT_GE(rComm.getRequests().addRequestServer(header), 0);
    sComm.getRequests().stashRequest();
  }
  // Pre-send response
  std::string res_send = "This is a response message";
  size_t req_send_len = req_send.size();
  size_t res_recv_len = 0;
  EXPECT_GE(rComm.sendVar(res_send), 0);
  // Call
  void** ptrs = (void**)malloc(4 * sizeof(void*));
  ptrs[0] = (void*)(req_send.c_str());
  ptrs[1] = (void*)(&req_send_len);
  ptrs[2] = NULL;
  ptrs[3] = (void*)(&res_recv_len);
  EXPECT_EQ(pcommCall(sComm_c, 1, 4, ptrs, 0), 2);
  EXPECT_EQ(res_send.size(), res_recv_len);
  EXPECT_EQ(strcmp(res_send.c_str(), (char*)(ptrs[2])), 0);
  // Server request
  EXPECT_GE(rComm.recvVar(req_recv), 0);
  EXPECT_EQ(req_recv, req_send);
  // Cleanup
  free(ptrs[2]);
  free(ptrs);
  close_comm(&sComm_c);
}

TEST(comm_t, Errors) {
  comm_t tmp;
  tmp.comm = NULL;
  EXPECT_EQ(set_response_format(tmp, "%s"), 0);
  dtype_t tmp_dtype;
  tmp_dtype.metadata = NULL;
  EXPECT_EQ(set_response_datatype(tmp, tmp_dtype), 0);
  EXPECT_EQ(comm_send(tmp, NULL, 0), -1);
  EXPECT_EQ(comm_send_eof(tmp), -1);
  EXPECT_EQ(comm_recv(tmp, NULL, 0), -1);
  EXPECT_EQ(comm_recv_realloc(tmp, NULL, 0), -1);
  EXPECT_EQ(ncommSend(tmp, 1, 1), -1);
  EXPECT_EQ(ncommRecv(tmp, 0, 1, NULL), -1);
  EXPECT_EQ(ncommCall(tmp, 0, 2, 1, NULL), -1);
  EXPECT_EQ(pcommSend(tmp, 0, NULL, 0), -1);
  EXPECT_EQ(pcommRecv(tmp, 0, 0, NULL, 0), -1);
  EXPECT_EQ(pcommCall(tmp, 0, 0, NULL, 0), -1);
  EXPECT_EQ(comm_nmsg(tmp), -1);
  tmp = init_comm("", SEND, NULL_COMM, tmp_dtype);
  EXPECT_FALSE(tmp.comm);
  {
    COMM_BASE alt("", nullptr, RECV);
    setenv("output_OUT", alt.getAddress().c_str(), 1);
    tmp = yggOutputFmt("output", "%j");
    EXPECT_FALSE(tmp.comm);
    unsetenv("output_OUT");
  }
  {
    COMM_BASE alt("", nullptr, SEND);
    setenv("input_IN", alt.getAddress().c_str(), 1);
    tmp = yggInputFmt("input", "%j");
    EXPECT_FALSE(tmp.comm);
    unsetenv("input_IN");
  }
  {
    COMM_BASE alt("", nullptr, RECV);
    setenv("output_OUT", alt.getAddress().c_str(), 1);
    tmp = yggAsciiArrayOutput("output", "%j");
    EXPECT_FALSE(tmp.comm);
    unsetenv("output_OUT");
  }
}

#define INIT_DATA_PTRS				\
  INIT_DATA_SINGLE(double, 1.5);		\
  void* p_send = &data_send;			\
  void* p_recv = &data_recv;			\
  void** pp_send = &p_send;			\
  void** pp_recv = &p_recv

INTERFACE_TEST_BASE(
  Pointers,
  INIT_INPUT_BASE(yggInput, ("input"), COMM_BASE,
		  ("", nullptr, SEND)),
  INIT_OUTPUT_BASE(yggOutputFmt, ("output", "%lf"), COMM_BASE,
		   ("", nullptr, RECV)),
  INIT_DATA_PTRS, COMP_DATA_SINGLE,
  pcommSend, (1, pp_send, 0), sendVar, (data_send),
  pcommRecv, (0, 1, pp_recv, 0), recvVar, (data_recv),
  pp_send = NULL; pp_recv = NULL; free(pp_send); free(pp_recv))

TEST(YggInterface_C, GlobalServer) {
  {
    std::string name = "test_name";
    ClientComm sComm(name, nullptr);
    sComm.set_timeout_recv(1000);
    std::string key_env = name + "_IN";
    std::string val_env = sComm.getAddress();
    setenv(key_env.c_str(), val_env.c_str(), 1);
    {
      dtype_t dtype_req = create_dtype_from_schema("{\"type\": \"integer\"}", false);
      dtype_t dtype_res = create_dtype_from_schema("{\"type\": \"integer\"}", false);
      comm_t rComm_c = yggRpcServerType_global("test_name", dtype_req, dtype_res);
      ServerComm& rComm = *((ServerComm*)(rComm_c.comm));
      rComm.set_timeout_recv(1000);
      DO_RPC_SIGNON;
      // Request
      int req_send = 1, req_recv = 0;
      EXPECT_GE(sComm.sendVar(req_send), 0);
      EXPECT_GE(yggRecv(rComm_c, &req_recv), 0);
      EXPECT_EQ(req_recv, req_send);
      EXPECT_TRUE(sComm.afterSendRecv(&sComm, &rComm));
      // Response
      int res_send = 2, res_recv = 0;
      EXPECT_GE(yggSend(rComm_c, res_send), 0);
      EXPECT_GE(sComm.recvVar(res_recv), 0);
      EXPECT_EQ(res_recv, res_send);
      EXPECT_TRUE(rComm.afterSendRecv(&rComm, &sComm));
      close_comm(&rComm_c);
    }
    {
      dtype_t dtype_req = create_dtype_from_schema("{\"type\": \"integer\"}", false);
      dtype_t dtype_res = create_dtype_from_schema("{\"type\": \"integer\"}", false);
      comm_t rComm_c = yggRpcServerType_global(name.c_str(), dtype_req, dtype_res);
      ServerComm& rComm = *((ServerComm*)(rComm_c.comm));
      // Request
      std::cout << "Client ";
      sComm.getRequests().Display();
      std::cout << "Server ";
      rComm.getRequests().Display();
      int req_send = 1, req_recv = 0;
      EXPECT_GE(sComm.sendVar(req_send), 0);
      EXPECT_GE(yggRecv(rComm_c, &req_recv), 0);
      EXPECT_EQ(req_recv, req_send);
      EXPECT_TRUE(sComm.afterSendRecv(&sComm, &rComm));
      // Response
      int res_send = 2, res_recv = 0;
      EXPECT_GE(yggSend(rComm_c, res_send), 0);
      EXPECT_GE(sComm.recvVar(res_recv), 0);
      EXPECT_EQ(res_recv, res_send);
      EXPECT_TRUE(rComm.afterSendRecv(&rComm, &sComm));
      close_comm(&rComm_c);
    }
    unsetenv(key_env.c_str());
  }
  Comm_t::_ygg_cleanup();
}

// TODO: piecemeal server

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
