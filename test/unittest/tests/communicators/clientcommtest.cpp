#include "../../unittest.hpp"
#include "utils/Address.hpp"
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
class ClientComm_tester : public ClientComm {
public:
    ClientComm_tester(const std::string &name = "", utils::Address *address = nullptr) :
      ClientComm(name, address), server_requests(SEND) {}
  bool addRequest(std::string& msg) {
    Header header;
    if (!this->create_header_send(header, msg.c_str(), msg.size()))
      return false;
    size_t len = header.format(msg.c_str(), msg.size(), 0);
    msg.assign(header.data[0], len);
    if (server_requests.addRequestServer(header) < 0)
      return false;
    return true;
  }
  bool addResponse(std::string& msg) {
    Header header;
    header.for_send(NULL);
    if (server_requests.addResponseServer(header, msg.c_str(), msg.size()) < 0)
      return false;
    if (this->requests.addResponseClient(header, msg.c_str(), msg.size()) < 0)
      return false;
    return true;
  }
  RequestList server_requests;
};
}
}

TEST(ClientComm, constructor) {
    std::string name = "MyComm";
    setenv("YGG_MODEL_INDEX", "1", 1);
    ClientComm cc(name, nullptr);
    unsetenv("YGG_MODEL_INDEX");
    ClientComm cc1("", nullptr);
    // ClientComm cc2("", new utils::Address("12345"));
}

// TODO: migrate to tests for RequestList class

// TEST(ClientComm, requests) {
//     const std::string rq = "Cx159",
//                       rq1 = "Ml229";
//     std::string name = "MyComm";
//     ClientComm cc(name, nullptr);
//     EXPECT_EQ(cc.has_request(rq), -1);
//     cc.add_request(rq);
//     cc.add_request(rq1);
//     EXPECT_EQ(cc.has_request(rq1), 1);
//     EXPECT_EQ(cc.remove_request(rq), 0);
//     EXPECT_EQ(cc.has_request(rq1), 0);
//     EXPECT_EQ(cc.remove_request(rq), 0);
//     EXPECT_EQ(cc.has_request(rq1), 0);
// }

// TEST(ClientComm, responses) {
//     const std::string rq = "Cx159",
//                       rq1 = "Ml229";
//     const std::string rs = "Cx159",
//                       rs1 = "Ml229";
//     std::string name = "MyComm";
//     ClientComm cc(name, nullptr);
//     EXPECT_EQ(cc.has_response(rs), -1);
//     std::string res1 = "This is a response";
//     cc.add_response(rs, res1.c_str(), res1.size());
//     EXPECT_EQ(cc.has_response(rs), -1);
//     cc.add_request(rq);
//     cc.add_response(rs, res1.c_str(), res1.size());
//     EXPECT_EQ(cc.has_response(rs), 0);
//     cc.remove_request(rq);
//     char* data = (char*)malloc(sizeof(char));
//     size_t mlen = 1;
//     EXPECT_EQ(cc.pop_response(rs, data, mlen, false), -1);
//     cc.add_request(rq);
//     cc.add_request(rq1);
//     cc.add_response(rs1, res1.c_str(), res1.size());
//     EXPECT_EQ(cc.pop_response(rs1, data, mlen, false), -res1.size());
//     EXPECT_EQ(cc.pop_response(rs1, data, mlen, true), res1.size());
// }

TEST(ClientComm, recv) {
    std::string name = "MyComm";
    setenv("YGG_MODEL_INDEX", "1", 1);
    communication::testing::ClientComm_tester cc(name, nullptr);
    unsetenv("YGG_MODEL_INDEX");
    char* data = (char*)malloc(sizeof(char));
    size_t len = 1;
    EXPECT_EQ(cc.recv(data, len, false), -1);
    std::string req1 = "This is a response";
    std::string res1 = req1;
    cc.addRequest(res1);
    cc.addResponse(res1);
    EXPECT_EQ(cc.recv(data, len, false), -1);
    EXPECT_GE(cc.recv(data, len, true), req1.size());
    EXPECT_EQ(req1, std::string(data));
#ifdef ELF_RECV
    ELF_BEGIN;
    ELF_RECV(-2);
    EXPECT_EQ(cc.recv(data, len, false), -res1.size());
    EXPECT_EQ(cc.recv(data, len, true), res1.size());
    ELF_RECV_REVERT;
    ELF_END;
#endif // ELF_RECV
    free(data);
}

#endif
