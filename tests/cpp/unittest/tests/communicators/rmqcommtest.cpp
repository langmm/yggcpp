#include "../../unittest.hpp"
#include "utils/Address.hpp"
#include "communicators/RMQComm.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"
#include "commtest.hpp"


using namespace YggInterface;
using namespace YggInterface::communicator;
using namespace YggInterface::mock;

class RMQComm_tester : public RMQComm {
public:
  TESTER_METHODS(RMQComm)
};

#ifdef RMQINSTALLED

COMM_SERI_TEST(RMQComm)

// TEST(RMQComm, exchange) {
//   std::string addr = "amqp://guest:guest@localhost:5672//_RMQPARAM_testex_RMQPARAM_testq";
//   RMQComm sComm("test", addr, SEND, COMM_FLAG_SET_OPP_ENV);
//   RMQComm rComm("test", RECV);
//   double sData = 5.0, rData = 0.0;
//   EXPECT_GE(sComm.sendVar(sData), 0);
//   EXPECT_GE(rComm.recvVar(rData), 0);
//   EXPECT_EQ(rData, sData);
// }

TEST(RMQComm, constructor_error) {
  utils::Address addr("invalid_RMQPARAM_invalid");
  EXPECT_THROW(RMQComm("test", addr), std::exception);
}

#ifdef ELF_AVAILABLE
TEST(RMQComm, nmsg) {
  
}
#endif // ELF_AVAILABLE

#else // RMQINSTALLED

TEST(RMQComm, constructor) {
  EXPECT_THROW(RMQComm_tester ipc(""), std::exception);
  std::string name = "";
  EXPECT_THROW(RMQComm_tester ipc2(name, SEND), std::exception);
}

#endif // RMQINSTALLED
