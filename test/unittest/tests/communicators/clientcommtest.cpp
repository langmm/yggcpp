#include "../../unittest.hpp"
#include "utils/Address.hpp"
#include "communicators/ClientComm.hpp"
#include "communicators/ServerComm.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"

#ifdef COMM_BASE

using namespace communication;
using namespace communication::communicator;
using namespace communication::mock;

namespace communication {
namespace testing {
class ClientComm_tester : public ClientComm {
private:
  ClientComm_tester(const ClientComm_tester&) = delete;
  ClientComm_tester& operator=(const ClientComm_tester&) = delete;
public:
  ClientComm_tester(const std::string &name = "", utils::Address *address = nullptr) :
    ClientComm(name, address), server_comm(NULL) {
    server_comm = new ServerComm("", new utils::Address(this->address->address()));
  }
  ~ClientComm_tester() override {
    delete server_comm;
    server_comm = NULL;
  }
  bool addSignon() {
    std::string msg_cli = YGG_CLIENT_SIGNON;
    if (!addRequest(msg_cli, true))
      return false;
    std::string msg_srv = YGG_SERVER_SIGNON;
    if (server_comm->send(msg_srv.c_str(), msg_srv.size()) < 0)
      return false;
    return true;
  }
  bool addRequest(std::string& msg, bool skip_signon=false) {
    if (!skip_signon) {
      if (!addSignon())
	return false;
    }
    Header header;
    if (!this->create_header_send(header, msg.c_str(), msg.size()))
      return false;
    size_t len = header.format(msg.c_str(), msg.size(), 0);
    msg.assign(header.data[0], len);
    if (server_comm->requests.addRequestServer(header) < 0)
      return false;
    return true;
  }
  bool sendResponse(std::string& msg) {
    return (server_comm->send(msg) >= 0);
  }
  bool addResponse(std::string& msg, bool skip_client=false) {
    Header header;
    if (!server_comm->create_header_send(header, msg.c_str(), msg.size()))
      return false;
    if (!skip_client) {
      char* data = const_cast<char*>(msg.c_str());
      if (!this->create_header_recv(header, data,
				    msg.size(), msg.size(), false, true))
	return false;
    }
    return true;
  }
  void addResponseWorkers() {
    Comm_t* worker = server_comm->getWorkers().get(server_comm, SEND);
    this->getWorkers().get(this, RECV, new utils::Address(worker->getAddress()));
  }
  void addWorkers() {
    Comm_t* worker = this->getWorkers().get(this, SEND);
    server_comm->getWorkers().get(server_comm, RECV, new utils::Address(worker->getAddress()));
  }
  ServerComm* server_comm;
};
}
}

TEST(ClientComm, constructor) {
    std::string name = "MyComm";
    communication::testing::ClientComm_tester cc(name, nullptr);
    communication::testing::ClientComm_tester cc1("", nullptr);
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

TEST(ClientComm, send) {
    std::string name = "MyComm";
    communication::testing::ClientComm_tester cc(name, nullptr);
    std::string msg = "This is a test message";
    EXPECT_TRUE(cc.addSignon());
    EXPECT_GE(cc.send(msg.c_str(), msg.size()), 0);
}

TEST(ClientComm, sendLarge) {
    std::string name = "MyComm";
    communication::testing::ClientComm_tester cc(name, nullptr);
    cc.addWorkers();
    std::string msg(cc.getMaxMsgSize(), 'A');
    EXPECT_TRUE(cc.addSignon());
    EXPECT_GE(cc.send(msg.c_str(), msg.size()), 0);
    EXPECT_FALSE(cc.getWorkers().workers[0].request.empty());
}

TEST(ClientComm, recv) {
    std::string name = "MyComm";
    communication::testing::ClientComm_tester cc(name, nullptr);
    std::string req_send = "REQUEST";
    std::string res_send = "RESPONSE";
    std::string req_recv;
    std::string res_recv;
    EXPECT_TRUE(cc.addSignon());
    EXPECT_GE(cc.send(req_send), 0);
    EXPECT_EQ(cc.server_comm->recv(req_recv), req_send.size());
    EXPECT_EQ(req_recv, req_send);
    EXPECT_GE(cc.server_comm->send(res_send), 0);
    EXPECT_EQ(cc.recv(res_recv), res_send.size());
    EXPECT_EQ(res_recv, res_send);
#ifdef ELF_RECV
    std::string req = "Hello world";
    std::string res = req;
    char* data = NULL;
    size_t len = 0;
    ELF_BEGIN;
    ELF_RECV(0);
    RETVAL = 0;
    RETVAL_INC_POLL = 0;
    RETVAL_INC_RECV = 0;
    cc.addRequest(res);
    cc.addResponse(res);
    EXPECT_EQ(cc.recv(data, len, false), -res.size());
    EXPECT_EQ(cc.recv(data, len, true), res.size());
    EXPECT_EQ(strcmp(data, req.c_str()), 0);
    ELF_RECV_REVERT;
    ELF_END;
    free(data);
#endif // ELF_RECV
}

TEST(ClientComm, recvLarge) {
    std::string name = "MyComm";
    communication::testing::ClientComm_tester cc(name, nullptr);
    std::string bigMsg(cc.getMaxMsgSize(), 'A');
    std::string req_send = "REQUEST" + bigMsg;
    std::string res_send = "RESPONSE" + bigMsg;
    std::string req_recv;
    std::string res_recv;
    EXPECT_TRUE(cc.addSignon());
    cc.addWorkers();
    EXPECT_GE(cc.send(req_send), 0);
    EXPECT_FALSE(cc.getWorkers().workers[0].request.empty());
    EXPECT_EQ(cc.server_comm->recv(req_recv), req_send.size());
    EXPECT_EQ(req_recv, req_send);
    cc.addResponseWorkers();
    EXPECT_GE(cc.server_comm->send(res_send), 0);
    EXPECT_EQ(cc.recv(res_recv), res_send.size());
    EXPECT_EQ(res_recv, res_send);
    EXPECT_TRUE(cc.getWorkers().workers[0].request.empty());
}

#endif
