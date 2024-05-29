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


TEST(FunctionComm, call_c) {
  register_function("example_model_function", example_model_function);
  utils::Address addr("cxx::example_model_function");
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
