#include "../../unittest.hpp"
#include "utils/Address.hpp"
#include "utils/tools.hpp"
#include "communicators/ZMQComm.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"
#include <dlfcn.h>

using namespace communication;
using namespace communication::communicator;
using namespace communication::mock;
namespace communication {
namespace testing {
class ygg_sock_tester : public ygg_sock_t {
public:
    ygg_sock_tester(int type) : ygg_sock_t(type) {}

    ygg_sock_tester() = delete;

    static bool getValid() { return ctx_valid;}

    static size_t activeCount() {return activeSockets.size();}
};

class ZMQComm_tester : public ZMQComm {
public:
    explicit ZMQComm_tester(const std::string name = "", utils::Address *address = new utils::Address(),
                            DIRECTION direction = NONE) : ZMQComm(name, address, direction) {}
    ZMQComm_tester() = delete;

    std::string getAdr() { return address->address();}

    bool getNewAddress() {return new_address();}

    void setReply() {
        reply->addresses.push_back(new communication::utils::Address("ABCDE"));
        reply->sockets.push_back(new ygg_sock_t(0));
    }
};
}
}
TEST(yggSockT, yggSockTest) {
    ygg_sock_t ygs(ZMQ_REQ);
    EXPECT_EQ(ygs.tag, 0xcafe0004);
    EXPECT_EQ(communication::testing::ygg_sock_tester::activeCount(), 1);
    ygs.close();
    EXPECT_EQ(communication::testing::ygg_sock_tester::activeCount(), 0);
    ygg_sock_t::ctx_shutdown();
    EXPECT_FALSE(communication::testing::ygg_sock_tester::getValid());
    ygg_sock_t::ctx_shutdown();

    ygg_sock_t::get_context();
    //int x = 2;
    ygg_sock_t ygs1(ZMQ_REQ);
    ygg_sock_t ygs2(ZMQ_REQ);
    EXPECT_EQ(communication::testing::ygg_sock_tester::activeCount(), 2);
    ygg_sock_t::ctx_shutdown();
    EXPECT_EQ(communication::testing::ygg_sock_tester::activeCount(), 0);

    //communication::testing::ygg_sock_tester ygs2(ZMQ_REQ);
    //ygs2.setValid(false);


}

TEST(ZMQCOMM, cunstructor) {
    std::string name = "TestZMQComm";
    EXPECT_THROW(ZMQComm zmqc(name, nullptr, SEND), std::runtime_error);
    name = "";
    setenv("YGG_MODEL_INDEX", "123", 1);
    communication::testing::ZMQComm_tester zmqc(name, nullptr, SEND);
    std::string adr = zmqc.getAdr();
    EXPECT_TRUE(zmqc.getNewAddress());
    std::string adr2 = zmqc.getAdr();
    EXPECT_NE(adr, adr2);
    EXPECT_EQ(zmqc.comm_nmsg(), 0);
    auto *adrs = new utils::Address();
    ZMQComm zmqr(name, adrs, RECV);
    EXPECT_EQ(zmqr.comm_nmsg(), 0);
}

TEST(ZMQCOMM, sendTest) {
#ifdef ELF_AVAILABLE
    setenv("YGG_MODEL_INDEX", "123", 1);
    void* handle = dlopen(SUBLIB, RTLD_LAZY);
    void* original_func = nullptr;
    void* original_func2 = nullptr;
    void* original_func3 = nullptr;
    void* original_func4 = nullptr;
    void* original_func5 = nullptr;
    void* original_func6 = nullptr;
    if (!handle)
        EXPECT_TRUE(false);
    original_func = ELFHOOK(zmq::detail::socket_base::send);
    original_func2 = ELFHOOK_PARAM(zmq::poller_t<no_user_data>::wait_all, zmq::poller_t::wait_all);
    original_func3 = ELFHOOK(zmq::detail::socket_base::recv);
    original_func4 = ELFHOOK(zmq::message_tD);
    original_func5 = ELFHOOK(zmq::message_t::to_string);
    //original_func6 = ELFHOOK_ARGS(zmq::detail::socket_base::get, sockopt|mutable_buffer);
    EXPECT_NE(original_func, ((void*)0));
    EXPECT_NE(original_func2, ((void*)0));
    EXPECT_NE(original_func3, ((void*)0));
    EXPECT_NE(original_func4, ((void*)0));
    EXPECT_NE(original_func5, ((void*)0));
    std::string name = "TestZMQSend";
    communication::testing::ZMQComm_tester zmq(name, nullptr, SEND);
    zmq.setReply();
    //zmq.set_reply_send();
    std::string mmsg = "This is a test message";
    EXPECT_EQ(zmq.send(mmsg.c_str(), mmsg.size()), 0);

    std::string long_msg(YGG_MSG_MAX * 3 + 20, 'A');
    EXPECT_EQ(zmq.send(long_msg.c_str(), long_msg.size()), 0);
    ELFREVERT(zmq::detail::socket_base::send, original_func);
    ELFREVERT(zmq::poller_t<no_user_data>::wait_all, original_func2);
    ELFREVERT(zmq::detail::socket_base::recv, original_func3);
    ELFREVERT(zmq::message_tD, original_func4);
    ELFREVERT(zmq::message_t::to_string, original_func5);
    dlclose(handle);
#endif // ELF_AVAILABLE
}

/*
TEST(ZMQCOMM, recv) {
#ifdef ELF_AVAILABLE
    setenv("YGG_MODEL_INDEX", "123", 1);
    void* handle = dlopen(SUBLIB, RTLD_LAZY);
    void* original_func = nullptr;
    void* original_func2 = nullptr;
    std::string name = "TestZMQSend";
    communication::testing::ZMQComm_tester zmq(name, nullptr, SEND);
    char* data;
    size_t len=0;
    original_func = ELFHOOK_PARAM(zmq::poller_t<no_user_data>::wait_all, zmq::poller_t::wait_all);
    original_func2 = ELFHOOK(zmq::detail::socket_base::recv);
    zmq.recv(data, len, true);

    dlclose(handle);
#endif // ELF_AVAILABLE
}*/
