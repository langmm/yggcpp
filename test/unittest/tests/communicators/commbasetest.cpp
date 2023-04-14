#include "../../unittest.hpp"
#include "utils/tools.hpp"
#include "communicators/CommBase.hpp"
#include "rapidjson/document.h"

using namespace communication;
using namespace communication::communicator;

class Comm_tTest: public Comm_t {
public:
    Comm_tTest(utils::Address *address, DIRECTION dirn, const COMM_TYPE &t, int flgs = 0) :
            Comm_t(address, dirn, t, flgs) {}
    Comm_tTest(const std::string &name, DIRECTION direction, const COMM_TYPE &t) :
            Comm_t(name, direction, t) {}
    int comm_nmsg() const override {return 1;}
    int get_flags() const {return flags;}
    bool check(const size_t &len) const {return check_size(len);}
    using Comm_t::send;
    using Comm_t::recv;
protected:
    int send_single(const char*, const size_t &) override {return 0;}
    long recv_single(char*& data, const size_t &, bool) override {
        const std::string msg = "{ \"hello\" : \"world\" }";
        data = const_cast<char*>(msg.c_str());
        return static_cast<long>(msg.size());
    }
    void init() override {}
    void reset() override {}
};

TEST(Commt, Constructors) {
    utils::Address *adr = new utils::Address("this.is.a.test");
    Comm_tTest *ctest = new Comm_tTest(adr, SEND, NULL_COMM);
    EXPECT_EQ(ctest->getType(), NULL_COMM);
    EXPECT_TRUE(ctest->valid());
    EXPECT_FALSE(ctest->get_flags() & COMM_ALLOW_MULTIPLE_COMMS);
    std::cout << ctest << std::endl;
    delete ctest;
    adr = new utils::Address("this.is.a.test");
    ctest = new Comm_tTest(adr, NONE, NULL_COMM);
    EXPECT_EQ(ctest->getType(), NULL_COMM);
    EXPECT_FALSE(ctest->valid());
    EXPECT_FALSE(ctest->get_flags() & COMM_ALLOW_MULTIPLE_COMMS);
    delete ctest;
    const char* ygg = getenv("YGG_THREADING");
    setenv("YGG_THREADING", "TRUE", true);
    adr = new utils::Address("this.is.a.test");
    ctest = new Comm_tTest(adr, RECV, NULL_COMM);
    EXPECT_EQ(ctest->getType(), NULL_COMM);
    EXPECT_TRUE(ctest->valid());
    EXPECT_TRUE(ctest->get_flags() & COMM_ALLOW_MULTIPLE_COMMS);
    delete ctest;
    if (ygg != nullptr) {
        setenv("YGG_THREADING", ygg, true);
    } else {
        unsetenv("YGG_THREADING");
    }

    ctest = new Comm_tTest("Tester", SEND, NULL_COMM);
    EXPECT_FALSE(ctest->valid());
    delete ctest;
    std::string name = "abcdefghijklmnopqrstuvwxzyABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxzyABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
    ctest = new Comm_tTest(name, RECV, NULL_COMM);
    EXPECT_FALSE(ctest->valid());
    delete ctest;
    std::string testname = "tester101";
    setenv(testname.c_str(), "this.is.a.test", true);
    ctest = new Comm_tTest(testname, NONE, NULL_COMM);
    EXPECT_TRUE(ctest->valid());
    delete ctest;
    unsetenv(testname.c_str());
    setenv("YGG_MODEL_NAME", testname.c_str(), true);

    ctest = new Comm_tTest(testname, NONE, NULL_COMM);
    EXPECT_FALSE(ctest->valid());
    delete ctest;
    unsetenv("YGG_MODEL_NAME");
    ctest = new Comm_tTest("", NONE, NULL_COMM);
    EXPECT_FALSE(ctest->valid());
    delete ctest;
}

TEST(Commt, checksize) {
    Comm_tTest ctest("testname", SEND, NULL_COMM);
    EXPECT_TRUE(ctest.check(10));
    EXPECT_TRUE(ctest.check(YGG_MSG_MAX));
    EXPECT_FALSE(ctest.check(YGG_MSG_MAX + 1));
}
