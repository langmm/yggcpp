#include "../../unittest.hpp"
#include "utils/Address.hpp"
#include "communicators/ServerComm.hpp"
#include "communicators/ClientComm.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"
#include <dlfcn.h>

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
    setenv("YGG_MODEL_INDEX", "1", 1);			\
    ServerComm_tester client("client_server");		\
    unsetenv("YGG_MODEL_INDEX");			\
  }

TEST(ServerComm, constructor) {
    std::string name = "MyComm";
    setenv("YGG_MODEL_INDEX", "1", 1);
    ServerComm sc(name, nullptr);
    unsetenv("YGG_MODEL_INDEX");
    ServerComm sc1("", nullptr);
    // ServerComm sc2("", new utils::Address("12345"));
}

// TODO: migrate to tests for RequestList class

// TEST(ServerComm, requests) {
//     const std::string rq = "Cx159",
//             rq1 = "Ml229";
//     std::string name = "MyComm";
//     ServerComm sc(name, nullptr);
//     EXPECT_EQ(sc.has_request(rq), -1);
//     sc.add_request(rq, new utils::Address("1.2.3.4"));
//     sc.add_request(rq1, new utils::Address("1.2.3.5"));
//     int rqnum = sc.has_request(rq1);
//     EXPECT_EQ(rqnum, 1);
//     EXPECT_EQ(sc.remove_request(rqnum), 0);
//     rqnum = sc.has_request(rq1);
//     EXPECT_EQ(rqnum, -1);
//     EXPECT_EQ(sc.remove_request(rqnum), -1);
//     rqnum = sc.has_request(rq);
//     EXPECT_EQ(rqnum, 0);
//     EXPECT_EQ(sc.remove_request(rqnum), 0);
// }

// TEST(ServerComm, comm) {
//      const std::string rq = "Cx159",
//             rq1 = "Ml229";
//     std::string name = "MyComm";
//     ServerComm sc(name, nullptr);
//     auto* adr = new utils::Address("12345");
//     EXPECT_EQ(sc.has_comm(adr->address()), -1);
//     EXPECT_EQ(sc.has_comm(adr), -1);
//     sc.add_comm(adr);
//     std::string newadr = "23456";
//     sc.add_comm(newadr);
//     EXPECT_GE(sc.has_comm(adr), 0);
//     EXPECT_GE(sc.has_comm(newadr), 0);

//     EXPECT_EQ(sc.get_comm(0), nullptr);
//     sc.add_request(rq, new utils::Address("1.2.3.4"));
//     sc.add_request(rq1, new utils::Address("1.2.3.5"));
//     EXPECT_NE(sc.get_comm(1), nullptr);
//     EXPECT_EQ(sc.get_comm(5), nullptr);
//     EXPECT_NE(sc.get_comm(-1), nullptr);
// }

TEST(ServerComm, send) {
    std::string msg = "my message";
    std::string name = "MyComm";
    setenv("YGG_MODEL_INDEX", "1", 1);
    communication::testing::ServerComm_tester sc(name, nullptr);
    unsetenv("YGG_MODEL_INDEX");
    EXPECT_EQ(sc.send(msg.c_str(), msg.size()), -1);
    sc.addRequest();
    EXPECT_GE(sc.send(msg.c_str(), msg.size()), 0);
#ifdef ELF_SEND
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

    setenv("YGG_MODEL_INDEX", "1", 1);
    communication::testing::ServerComm_tester sc(name, nullptr);
    unsetenv("YGG_MODEL_INDEX");

    ELF_BEGIN;
    ELF_RECV(0);
    EXPECT_LT(sc.recv(data, len, false), -1);
    ELF_RECV_REVERT;
    ELF_END;
    free(data);
#endif // ELF_RECV
}

#endif
