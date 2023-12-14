#include "../../unittest.hpp"
#include "utils/Address.hpp"
#include "communicators/RMQComm.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"
#include "commtest.hpp"


using namespace communication;
using namespace communication::communicator;
using namespace communication::mock;

class RMQComm_tester : public RMQComm {
public:
  TESTER_METHODS(RMQComm)
};

#ifdef RMQINSTALLED

COMM_SERI_TEST(RMQComm)

#else // RMQINSTALLED

TEST(RMQComm, constructor) {
    EXPECT_THROW(RMQComm_tester ipc(""), std::exception);
    std::string name = "";
    EXPECT_THROW(RMQComm_tester ipc2(name, SEND), std::exception);
}

#endif // RMQINSTALLED
