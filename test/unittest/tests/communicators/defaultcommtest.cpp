#include "../../unittest.hpp"
#include "utils/Address.hpp"
#include "utils/tools.hpp"
#include "communicators/DefaultComm.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"
#include "commtest.hpp"

using namespace communication;
using namespace communication::communicator;
using namespace communication::mock;

TEST(DefaultCommu, checkTypeErrors) {
  DefaultComm x("", nullptr, SEND);
  x.addSchema("{\"type\": \"array\"}");
  {
    double data = 5.0;
    EXPECT_EQ(x.sendVar(data), -1);
    EXPECT_EQ(x.recvVar(data), -1);
  }
  {
    std::string data;
    EXPECT_EQ(x.sendVar(data), -1);
    EXPECT_EQ(x.recvVar(data), -1);
  }
  {
    rapidjson::Document data;
    data.Set(5.0);
    EXPECT_EQ(x.sendVar(data), -1);
    EXPECT_EQ(x.recvVar(data), -1);
  }
  {
    rapidjson::Ply data;
    EXPECT_EQ(x.sendVar(data), -1);
    EXPECT_EQ(x.recvVar(data), -1);
  }
  {
    rapidjson::ObjWavefront data;
    EXPECT_EQ(x.sendVar(data), -1);
    EXPECT_EQ(x.recvVar(data), -1);
  }
}

TEST(DefaultCommu, seriErrors) {
  DefaultComm sComm("", nullptr, SEND);
  DefaultComm rComm("", new utils::Address(sComm.getAddress().c_str()), RECV);
  int a, b;
  EXPECT_EQ(sComm.send(2, 1, 1), -1);
  // EXPECT_EQ(rComm.recv(2, &a, &b), -1);
  EXPECT_GE(sComm.send("hello", 5), 0);
  EXPECT_EQ(rComm.recv(2, &a, &b), -1);
}

TEST(DefaultCommu, workerErrors) {
  DefaultComm sComm("", nullptr, SEND);
  EXPECT_FALSE(sComm.getWorkers().setRequest(nullptr, "invalid"));
  EXPECT_FALSE(sComm.getWorkers().setResponse("invalid"));
  EXPECT_EQ(sComm.getWorkers().get(nullptr, RECV, new utils::Address(sComm.getAddress().c_str())), nullptr);
}
