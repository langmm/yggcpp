#include "../../unittest.hpp"
#include "utils/Address.hpp"
#include "communicators/FunctionComm.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"
#include "commtest.hpp"


using namespace YggInterface;
using namespace YggInterface::communicator;
using namespace YggInterface::mock;


bool example_model_function(const rapidjson::Document& data_send,
		       rapidjson::Document& data_recv) {
  std::cerr << "IN example_model_function: " << data_send << std::endl;
  if (!data_send.IsString())
    return false;
  data_recv.SetInt(static_cast<int>(data_send.GetStringLength()));
  return true;
}

#define DO_FUNCTION_CALL					\
  FunctionComm sComm("test", addr, SEND);			\
  FunctionComm rComm("test", addr, RECV);			\
  rapidjson::Document data_send, data_recv, data_exp;		\
  data_send.SetString("alpha", 5, data_send.GetAllocator());	\
  data_exp.SetInt(5);						\
  EXPECT_EQ(rComm.nmsg(), 0);					\
  EXPECT_EQ(sComm.nmsg(), 0);					\
  EXPECT_GE(sComm.send(data_send), 0);				\
  EXPECT_EQ(rComm.nmsg(), 1);					\
  EXPECT_EQ(sComm.nmsg(), 0);					\
  EXPECT_GE(rComm.recv(data_recv), 0);				\
  EXPECT_EQ(data_recv, data_exp)


#ifdef YGGTEST_DYNAMIC_example_c
TEST(FunctionComm, call_dynamic_c) {
  std::string library = "libexample_c";
#ifdef YGGTEST_DYNAMIC_DIR
  std::string directory = YGGTEST_DYNAMIC_DIR;
  library = directory + "/" + library;
#endif
  utils::Address addr("c::" + library + "::example_model_function");
  DO_FUNCTION_CALL;
}
#endif


#ifdef YGGTEST_DYNAMIC_example_fortran
TEST(FunctionComm, call_dynamic_fortran) {
  std::string library = "libexample_fortran";
#ifdef YGGTEST_DYNAMIC_DIR
  std::string directory = YGGTEST_DYNAMIC_DIR;
  library = directory + "/" + library;
#endif
  utils::Address addr("fortran::" + library + "::example_model_function");
  DO_FUNCTION_CALL;
}
#endif


TEST(FunctionComm, call_c) {
  register_function("example_model_function", example_model_function);
  utils::Address addr("cxx::example_model_function");
  DO_FUNCTION_CALL;
}

TEST(FunctionComm, call_c_async) {
  register_function("example_model_function", example_model_function);
  AsyncComm sComm("test_async", SEND,
		  COMM_FLAG_ASYNC | COMM_FLAG_SET_OPP_ENV,
		  FUNCTION_COMM);
  AsyncComm rComm("test_async", RECV, COMM_FLAG_ASYNC, DEFAULT_COMM);
  rapidjson::Document data_send, data_recv, data_exp;
  data_send.SetString("alpha", 5, data_send.GetAllocator());
  data_exp.SetInt(5);
  EXPECT_EQ(rComm.nmsg(), 0);
  EXPECT_EQ(sComm.nmsg(), 0);
  EXPECT_GE(sComm.send(data_send), 0);
  EXPECT_GT(rComm.wait_for_recv(1000000), 0);
  EXPECT_EQ(rComm.nmsg(), 1);
  EXPECT_EQ(sComm.nmsg(), 0);
  EXPECT_GE(rComm.recv(data_recv), 0);
  EXPECT_EQ(data_recv, data_exp);
}

#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
TEST(FunctionComm, call_python) {
  utils::Address addr("python::example_python:example_model_function");
  FunctionComm sComm("test", addr, SEND);
  FunctionComm rComm("test", addr, RECV);
  rapidjson::Document data_send, data_recv, data_exp;
  data_send.SetString("alpha", 5, data_send.GetAllocator());
  data_exp.SetInt(5);
  EXPECT_EQ(rComm.nmsg(), 0);
  EXPECT_EQ(sComm.nmsg(), 0);
  EXPECT_GE(sComm.send(data_send), 0);
  EXPECT_EQ(rComm.nmsg(), 1);
  EXPECT_EQ(sComm.nmsg(), 0);
  EXPECT_GE(rComm.recv(data_recv), 0);
  EXPECT_EQ(data_recv, data_exp);
}
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
