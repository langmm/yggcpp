#include "../../unittest.hpp"
#include "utils/Address.hpp"
#include "communicators/ClientComm.hpp"
#include "communicators/ServerComm.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"
#include "commtest.hpp"

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
  bool addStashedRequest(std::string& msg, bool skip_signon=false) {
    while (server_comm->requests.requests.size() > 0) {
      server_comm->requests.stashRequest();
    }
    if (!addRequest(msg, skip_signon))
      return false;
    this->requests.stashRequest();
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
}

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
    ELF_BEGIN;
    ELF_RECV(0);
    std::string req = "REQUEST";
    char* data = NULL;
    size_t len = 0;
    cc.addRequest(req);
    // cc.addResponse(req);
    RETMSG_META = "\"request_id\": \"" +
      cc.getRequests().requests[0].request_id + "\"";
    ELF_META(cc);
    EXPECT_EQ(cc.recv(data, len, false), -RETMSG.size());
    EXPECT_EQ(cc.recv(data, len, true), RETMSG.size());
    EXPECT_EQ(strcmp(data, RETMSG.c_str()), 0);
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

TEST(ClientComm, call) {
  std::string name = "MyComm";
  communication::testing::ClientComm_tester cc(name, nullptr);
  cc.addSchema("{\"type\": \"string\"}");
  std::string req_send = "REQUEST1";
  std::string res_send = "RESPONSE1";
  std::string req_recv = req_send;
  char* res_recv = NULL;
  size_t res_recv_len = 0;
  // Error before response
  EXPECT_EQ(cc.recvRealloc(2, &res_recv, &res_recv_len), -1);
  // First message
  cc.set_timeout_recv(1000);
  std::string req_recv_fmt = "\"" + req_recv + "\"";
  cc.addStashedRequest(req_recv_fmt);
  EXPECT_GE(cc.server_comm->sendVar(res_send), 0);
  EXPECT_EQ(cc.callRealloc(4, req_send.c_str(), req_send.size(),
			   &res_recv, &res_recv_len), 2);
  EXPECT_EQ(res_send.size(), res_recv_len);
  EXPECT_EQ(strcmp(res_send.c_str(), res_recv), 0);
  EXPECT_EQ(cc.server_comm->recvVar(req_recv), 2);
  EXPECT_EQ(req_recv, req_send);
  // Second message
  req_send = "REQUEST2";
  res_send = "RESPONSE2";
  req_recv = req_send;
  req_recv_fmt = "\"" + req_recv + "\"";
  res_recv_len = res_send.size() + 1;
  cc.addStashedRequest(req_recv_fmt, true);
  EXPECT_GE(cc.server_comm->sendVar(res_send), 0);
  EXPECT_EQ(cc.call(4, req_send.c_str(), req_send.size(),
		    res_recv, &res_recv_len), 2);
  EXPECT_EQ(res_send.size(), res_recv_len);
  EXPECT_EQ(strcmp(res_send.c_str(), res_recv), 0);
  EXPECT_EQ(cc.server_comm->recvVar(req_recv), 2);
  EXPECT_EQ(req_recv, req_send);
  // Failed message due to incorrect number of arguments
  EXPECT_EQ(cc.call(1, req_send.c_str()), -1);
  // Failed message due to failed send
#ifdef ELF_AVAILABLE
  ELF_BEGIN;
  ELF_REPLACE_SEND;
  EXPECT_EQ(cc.call(4, req_send.c_str(), req_send.size(),
		    res_recv, &res_recv_len), -1);
  ELF_RESTORE_SEND;
  ELF_END;
#endif // ELF_AVAILABLE
  free(res_recv);
}

TEST(ClientComm, global) {
  std::string name = "test_name";
  {
    ServerComm rComm(name, nullptr);
    rComm.set_timeout_recv(1000);
    std::string key_env = name + "_OUT";
    std::string val_env = rComm.getAddress();
    setenv(key_env.c_str(), val_env.c_str(), 1);
    {
      global_scope_comm_on();
      ClientComm sComm(name, nullptr);
      sComm.set_timeout_recv(1000);
      global_scope_comm_off();
      sComm.addResponseFormat("%s");
      {
	std::string msg_cli = YGG_CLIENT_SIGNON;
	Header header;
	EXPECT_TRUE(sComm.create_header_send(header, msg_cli.c_str(), msg_cli.size()));
	size_t len = header.format(msg_cli.c_str(), msg_cli.size(), 0);
	msg_cli.assign(header.data[0], len);
	EXPECT_GE(rComm.getRequests().addRequestServer(header), 0);
	std::string msg_srv = YGG_SERVER_SIGNON;
	EXPECT_GE(rComm.send(msg_srv.c_str(), msg_srv.size()), 0);
      }
      std::string req_send = "REQUEST";
      std::string res_send = "RESPONSE";
      std::string req_recv;
      std::string res_recv;
      EXPECT_GE(sComm.send(req_send), 0);
      EXPECT_GT(rComm.comm_nmsg(), 0);
      EXPECT_EQ(rComm.recv(req_recv), req_send.size());
      EXPECT_EQ(req_recv, req_send);
      EXPECT_GE(rComm.send(res_send), 0);
      EXPECT_EQ(sComm.recv(res_recv), res_send.size());
      EXPECT_EQ(res_recv, res_send);
    }
    {
      global_scope_comm_on();
      ClientComm sComm(name, nullptr);
      global_scope_comm_off();
      std::string req_send = "REQUEST";
      std::string res_send = "RESPONSE";
      std::string req_recv;
      std::string res_recv;
      EXPECT_GE(sComm.send(req_send), 0);
      EXPECT_GT(rComm.comm_nmsg(), 0);
      EXPECT_EQ(rComm.recv(req_recv), req_send.size());
      EXPECT_EQ(req_recv, req_send);
      EXPECT_GE(rComm.send(res_send), 0);
      EXPECT_EQ(sComm.recv(res_recv), res_send.size());
      EXPECT_EQ(res_recv, res_send);
    }
    unsetenv(key_env.c_str());
  }
  Comm_t::_ygg_cleanup();
}

#endif
