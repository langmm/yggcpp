#include "../../unittest.hpp"
#include "utils/tools.hpp"
#include "communicators/CommBase.hpp"
#include "rapidjson/document.h"

using namespace communication;
using namespace communication::communicator;

class Comm_tTest: public Comm_t {
public:
    Comm_tTest(utils::Address& address, DIRECTION dirn, const COMM_TYPE &t, int flgs = 0) :
      Comm_t("", address, dirn, t, flgs), _closed(false) {}
    Comm_tTest(const std::string &name, DIRECTION direction, const COMM_TYPE &t) :
      Comm_t(name, direction, t, COMM_FLAG_INTERFACE), _closed(false) {}
    int comm_nmsg(DIRECTION=NONE) const override {return 1;}
    int get_flags() const {return flags;}
    void close() override { _closed = true; }
    bool is_closed() const override { return _closed; }
    bool check(const size_t &len) const {return check_size(len);}
    using Comm_t::send;
    using Comm_t::recv;
protected:
    int send_single(utils::Header&) override {
      return 0;
    }
    long recv_single(utils::Header&) override {
      // const std::string msg = "{ \"hello\" : \"world\" }";
      // data = const_cast<char*>(msg.c_str());
      // return static_cast<long>(msg.size());
      return 0;
    }
    Comm_t* create_worker(utils::Address& adr,
                          const DIRECTION& dir, int flgs) override {
      return new Comm_tTest(adr, dir, this->type, flgs);
    }

    bool _closed;
};

class EmptyComm : public CommBase<int> {
public:
  EmptyComm() :
    CommBase("", SEND, ZMQ_COMM, 0), nmsg_(-1) {
    handle = new int();
    updateMaxMsgSize(1000);
  }
  int wait_for_recv(const int&) override { return 0; }
  int comm_nmsg(DIRECTION dir=NONE) const override {
    if (nmsg_ >= 0) return nmsg_;
    return CommBase::comm_nmsg(dir);
  }
  int nmsg_;
};

TEST(Commt, Constructors) {
    unsetenv("YGG_MODEL_NAME");
    utils::Address adr("this.is.a.test");
    Comm_tTest *ctest = new Comm_tTest(adr, SEND, NULL_COMM);
    EXPECT_EQ(ctest->getType(), NULL_COMM);
    EXPECT_TRUE(ctest->valid());
    EXPECT_FALSE(ctest->get_flags() & COMM_ALLOW_MULTIPLE_COMMS);
    std::cout << ctest << std::endl;
    delete ctest;
    //adr = new utils::Address("this.is.a.test");
    ctest = new Comm_tTest(adr, NONE, NULL_COMM);
    EXPECT_EQ(ctest->getType(), NULL_COMM);
    EXPECT_FALSE(ctest->valid());
    EXPECT_FALSE(ctest->get_flags() & COMM_ALLOW_MULTIPLE_COMMS);
    delete ctest;
    const char* ygg = getenv("YGG_THREADING");
    setenv("YGG_THREADING", "TRUE", true);
    //adr = new utils::Address("this.is.a.test");
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
    std::string testname_env = testname + "_IN";
    setenv(testname_env.c_str(), "this.is.a.test", true);
    ctest = new Comm_tTest(testname, RECV, NULL_COMM);
    EXPECT_TRUE(ctest->valid());
    delete ctest;
    unsetenv(testname_env.c_str());
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

TEST(CommBase, MissingOverrides) {
  EmptyComm x;
  x.addSchema("{\"type\": \"any\"}");
  EXPECT_EQ(x.comm_nmsg(), -1);
  EXPECT_EQ(x.sendVar(0), -1);
  int var = 0;
  EXPECT_EQ(x.recvVar(var), -1);
  x.nmsg_ = 1;
  EXPECT_EQ(x.recvVar(var), -1);
  std::string msg(1050, 'a');
  EXPECT_EQ(x.sendVar(msg), -1);
}
