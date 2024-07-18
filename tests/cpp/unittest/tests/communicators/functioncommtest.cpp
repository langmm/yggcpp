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
  DO_FUNCTION_CALL;
}

TEST(FunctionComm, call_c_async) {
  register_function("example_model_function", example_model_function);
  utils::Address addr("cxx::example_model_function");
  DO_FUNCTION_CALL_ASYNC;
}
