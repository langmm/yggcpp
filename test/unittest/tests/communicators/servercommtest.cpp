#include "../../unittest.hpp"
#include "utils/Address.hpp"
#include "communicators/ServerComm.hpp"
#include "communicators/ClientComm.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"
#include "commtest.hpp"

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
      Header header;
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
    RETMSG_META = "\"request_id\": \"12345\"";
    ELF_META(sc);
    EXPECT_EQ(sc.recv(data, len, false), -RETMSG.size());
    EXPECT_EQ(sc.recv(data, len, true), RETMSG.size());
    EXPECT_EQ(strcmp(data, RETMSG.c_str()), 0);
    ELF_RECV_REVERT;
    ELF_END;
    free(data);
#endif // ELF_RECV
}

TEST(ServerComm, signon) {
  std::string name = "MyComm";
  ServerComm sc(name, nullptr);
  ClientComm cc(name, new utils::Address(sc.getAddress()));
  // Send signon then message
  std::string msg_send = "Hello world";
  std::string msg_recv;
  EXPECT_GE(cc.getRequests().initClientResponse(), 0);
  EXPECT_GE(cc.send(YGG_CLIENT_SIGNON, YGG_CLIENT_SIGNON_LEN), 0);
  sc.wait_for_recv(1000);
  sc.set_timeout_recv(1000);
  EXPECT_EQ(sc.recvVar(msg_recv), -1);
  EXPECT_GE(cc.sendVar(msg_send), 0);
  EXPECT_GE(sc.recvVar(msg_recv), 0);
}

#endif
