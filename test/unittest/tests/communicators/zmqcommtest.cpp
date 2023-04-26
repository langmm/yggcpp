#include "../../unittest.hpp"
#include "utils/Address.hpp"
#include "utils/tools.hpp"
#include "communicators/ZMQComm.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"
#include "commtest.hpp"

using namespace communication;
using namespace communication::communicator;
using namespace communication::mock;

namespace communication {
namespace testing {
class ZMQSocket_tester : public ZMQSocket {
public:
    ZMQSocket_tester(int type) : ZMQSocket(type) {}

    ZMQSocket_tester() = delete;

    // static bool getValid() { return ctx_valid;}

    // static size_t activeCount() {return activeSockets.size();}
};
}
}

class ZMQComm_tester : public ZMQComm {
public:
    TESTER_METHODS(ZMQComm)

    std::string getAdr() { return address->address();}

    void setReply() {
        // this->getReply()->addresses.push_back(new communication::utils::Address("ABCDE"));
      std::string adr;
      this->getReply().create(adr);
    }
};

#ifdef ZMQINSTALLED

COMM_SERI_TEST(ZMQComm)

// TODO: Socket tests?

// TEST(yggSockT, yggSockTest) {
//     ZMQSocket ygs(ZMQ_REQ);
//     EXPECT_EQ(communication::testing::ZMQSocket_tester::activeCount(), 1);
//     ygs.close();
//     EXPECT_EQ(communication::testing::ZMQSocket_tester::activeCount(), 0);
//     ZMQSocket::ctx_shutdown();
//     EXPECT_FALSE(communication::testing::ZMQSocket_tester::getValid());
//     ZMQSocket::ctx_shutdown();

//     ZMQSocket::get_context();
//     //int x = 2;
//     ZMQSocket ygs1(ZMQ_REQ);
//     ZMQSocket ygs2(ZMQ_REQ);
//     EXPECT_EQ(communication::testing::ZMQSocket_tester::activeCount(), 2);
//     ZMQSocket::ctx_shutdown();
//     EXPECT_EQ(communication::testing::ZMQSocket_tester::activeCount(), 0);

//     //communication::testing::ZMQSocket_tester ygs2(ZMQ_REQ);
//     //ygs2.setValid(false);


// }

TEST(ZMQComm, constructor) {
    std::string name = "TestZMQComm";
    ZMQSocket::resetPort();
    unsetenv("YGG_MODEL_INDEX");
    EXPECT_THROW(ZMQComm zmqc(name, nullptr, SEND), std::runtime_error);
    name = "";
    setenv("YGG_MODEL_INDEX", "123", 1);
    ZMQComm_tester zmqc(name, nullptr, SEND);
    EXPECT_EQ(zmqc.comm_nmsg(), 0);
    auto *adrs = new utils::Address();
    ZMQComm_tester zmqr(name, adrs, RECV);
    EXPECT_EQ(zmqr.comm_nmsg(), 0);
#ifdef ELF_AVAILABLE
    name = "";
    ELF_BEGIN;
    // Failure to create socket
    {
      ELF_BEGIN_F(zmq_socket);
      EXPECT_THROW(ZMQComm zmqc(name, nullptr, SEND), std::exception);
      ELF_END_F(zmq_socket);
    }
    // // Failure to set socket options
    // {
    //   ELF_BEGIN_F(zmq_setsockopt);
    //   EXPECT_THROW(ZMQComm zmqc(name, nullptr, SEND), std::runtime_error);
    //   ELF_END_F(zmq_setsockopt);
    // }
    // // Failure to connect
    // {
    //   ELF_BEGIN_F(zmq_connect);
    //   EXPECT_THROW(ZMQComm zmqc("", new utils::Address("1.2.3.4"), RECV),
    // 		   std::runtime_error);
    //   ELF_END_F(zmq_connect);
    // }
    // {
    //   // Advance port on bind to existing
    //   RETVAL = EADDRINUSE;
    //   ELF_BEGIN_F(zmq_bind);
    //   ELF_BEGIN_F(zmq_errno);
    //   ZMQComm_tester zmqc2(name, nullptr, SEND);
    //   // Failure to bind
    //   RETVAL = EADDRINUSE + 1;
    //   EXPECT_THROW(ZMQComm zmqc(name, nullptr, SEND), std::runtime_error);
    //   ELF_END_F(zmq_bind);
    //   ELF_END_F(zmq_errno);
    // }
    // {
    //   // Failure to get socket options
    //   ELF_BEGIN_F(zmq_getsockopt);
    //   RETMSG = "";
    //   EXPECT_THROW(ZMQComm zmqc(name, nullptr, SEND), std::runtime_error);
    //   // Failure to get endpoint
    //   RETMSG = "invalid";
    //   EXPECT_THROW(ZMQComm zmqc(name, nullptr, SEND), std::runtime_error);
    //   ELF_END_F(zmq_getsockopt);
    // }
    ELF_END;
#endif // ELF_AVAILABLE
}

// TEST(ZMQComm, send) {
// #ifdef ELF_AVAILABLE
//     setenv("YGG_MODEL_INDEX", "123", 1);
//     std::string name = "TestZMQSend";
//     std::string mmsg = "This is a test message";
//     ZMQComm_tester zmq(name, nullptr, SEND);
//     zmq.setReply();
//     ELF_BEGIN;
//     // Failure to create message
//     {
//       ELF_BEGIN_F(zmq_msg_init_size);
//       EXPECT_EQ(zmq.send(mmsg.c_str(), mmsg.size()), -1);
//       ELF_END_F(zmq_msg_init_size);
//     }
//     // Failure to send
//     {
//       RETVAL = -1;
//       ELF_BEGIN_F(zmq_sendmsg);
//       EXPECT_EQ(zmq.send(mmsg.c_str(), mmsg.size()), -1);
//       ELF_END_F(zmq_sendmsg);
//     }
//     // Successful send
//     {
//       RETVAL = 0;
//       ELF_BEGIN_F(zmq_sendmsg);
//       EXPECT_GT(zmq.send(mmsg.c_str(), mmsg.size()), 0);
//       std::string long_msg(YGG_MSG_MAX * 3 + 20, 'A');
//       EXPECT_GT(zmq.send(long_msg.c_str(), long_msg.size()), 0);
//       ELF_END_F(zmq_sendmsg);
//     }
//     // These are only needed if the tester dosn't by pass do_reply
//     // ELF_BEGIN_F(zmq_recvmsg);
//     // ELF_BEGIN_F(zmq_poller_wait_all);
//     // ELF_END_F(zmq_recvmsg);
//     // ELF_END_F(zmq_poller_wait_all);
//     ELF_END;
// #endif // ELF_AVAILABLE
// }

// TEST(ZMQComm, recv) {
// #ifdef ELF_AVAILABLE
//     setenv("YGG_MODEL_INDEX", "123", 1);
//     std::string name = "TestZMQSend";
//     ZMQComm_tester zmq(name, nullptr, RECV);
//     ELF_BEGIN;
//     char* data = NULL;
//     size_t len = 0;
//     // Successful recv
//     RETVAL = 1;
// #ifdef ZMQ_HAVE_POLLER
//     ELF_BEGIN_F(zmq_poller_wait_all);
// #else // ZMQ_HAVE_POLLER
//     ELF_BEGIN_F(zmq_poll);
// #endif // ZMQ_HAVE_POLLER
//     ELF_BEGIN_F(zmq_recvmsg);
//     EXPECT_GE(zmq.recv(data, len, true), 0);
//     EXPECT_EQ(strcmp(data, "Hello world"), 0);
//     free(data);
//     // Fail receive on poll
//     RETVAL = -1;
//     EXPECT_EQ(zmq.recv(data, len, true), -1);
//     // Fail receive on receiving message
//     RETVAL = 0;
//     EXPECT_EQ(zmq.recv(data, len, true), -1);
// #ifdef ZMQ_HAVE_POLLER
//     ELF_END_F(zmq_poller_wait_all);
// #else // ZMQ_HAVE_POLLER
//     ELF_END_F(zmq_poll);
// #endif // ZMQ_HAVE_POLLER
//     ELF_END_F(zmq_recvmsg);
//     ELF_END;
// #endif // ELF_AVAILABLE
// }

#else // ZMQINSTALLED

TEST(ZMQComm, constructor) {
    EXPECT_THROW(ZMQComm_tester zmq, std::exception);
    std::string name = "";
    EXPECT_THROW(ZMQComm_tester zmq2(name, nullptr, SEND), std::exception);
}

#endif // ZMQINSTALLED
