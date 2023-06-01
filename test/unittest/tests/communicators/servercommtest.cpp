#include "../../unittest.hpp"
#include "utils/Address.hpp"
#include "communicators/ServerComm.hpp"
#include "communicators/ClientComm.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"

#ifdef COMM_BASE

using namespace communication;
using namespace communication::communicator;
using namespace communication::mock;

namespace communication {
namespace testing {
class ServerComm_tester : public ServerComm {
public:
    ServerComm_tester(const std::string &name = "", utils::Address *address = nullptr) :
      ServerComm(name, address), client_requests(RECV) {}
    bool addRequest() {
      utils::Header header;
      header.for_send(NULL, NULL, 0);
      if (client_requests.addRequestClient(header) < 0)
	return false;
      if (this->requests.addRequestServer(header) < 0)
	return false;
      return true;
    }
    RequestList client_requests;
};
}
}

#define SERVER_SERI_TEST_TYPE(type, value, schema)	\
  TEST(ServerComm, type) {				\
    ServerComm_tester client("client_server");		\
  }

TEST(ServerComm, constructor) {
    std::string name = "MyComm";
    ServerComm sc(name, nullptr);
    ServerComm sc1("", nullptr);
}

TEST(ServerComm, send) {
    std::string msg = "my message";
    std::string name = "MyComm";
    communication::testing::ServerComm_tester sc(name, nullptr);
    EXPECT_EQ(sc.send(msg.c_str(), msg.size()), -1);
    sc.addRequest();
    EXPECT_GE(sc.send(msg.c_str(), msg.size()), 0);
#ifdef ELF_SEND
    // Failure due to absence of request
    ELF_BEGIN;
    ELF_SEND(5);
    EXPECT_EQ(sc.send(msg.c_str(), msg.size()), -1);
    ELF_SEND_REVERT;
    ELF_END;
#endif // ELF_SEND
}

TEST(ServerComm, recv) {
#ifdef ELF_RECV
    std::string name = "MyComm";
    char* data = (char*)malloc(sizeof(char));
    size_t len = 1;

    communication::testing::ServerComm_tester sc(name, nullptr);

    ELF_BEGIN;
    ELF_RECV(0);
    EXPECT_LT(sc.recv(data, len, false), -1);
    ELF_RECV_REVERT;
    ELF_END;
    free(data);
#endif // ELF_RECV
}

#endif
