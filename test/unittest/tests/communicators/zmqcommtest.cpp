#include "../../unittest.hpp"
#include "utils/Address.hpp"
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
    ZMQComm_tester(const std::string name = "", utils::Address *address = new utils::Address(),
                   DIRECTION direction = NONE) : ZMQComm(name, address, direction) {}
    ZMQComm_tester() = delete;

    std::string getAdr() { return address->address();}

    bool getNewAddress() {return new_address();}
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
    utils::Address *adrs = new utils::Address();
    ZMQComm zmqr(name, adrs, RECV);
    EXPECT_EQ(zmqr.comm_nmsg(), 0);
}

TEST(ZMQCOMM, sendTest) {

}