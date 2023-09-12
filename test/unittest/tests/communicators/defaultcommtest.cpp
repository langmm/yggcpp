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
  EXPECT_TRUE(x.addSchema("{\"type\": \"boolean\"}"));
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
  int a = 0, b = 0;
  EXPECT_EQ(sComm.send(2, 1, 1), -1); // No schema to parse variable arguments
  EXPECT_GE(sComm.send("hello", 5), 0);
  EXPECT_EQ(rComm.recvVar(a, b), -1); // Type mismatch
}

TEST(DefaultCommu, workerErrors) {
  DefaultComm sComm("", nullptr, SEND);
  EXPECT_FALSE(sComm.getWorkers().setRequest(nullptr, "invalid"));
  EXPECT_FALSE(sComm.getWorkers().setResponse("invalid"));
  utils::Address* addr = new utils::Address(sComm.getAddress().c_str());
  EXPECT_EQ(sComm.getWorkers().get(nullptr, RECV, addr), nullptr);
  delete addr;
}

TEST(DefaultCommu, filter_recv) {
  DefaultComm sComm("", nullptr, SEND);
  DefaultComm rComm("", new utils::Address(sComm.getAddress()), RECV);
  rComm.getMetadata().addFilter(example_filter);
  EXPECT_GT(sComm.sendVar(0), 0);
  EXPECT_GT(sComm.sendVar(1), 0);
  EXPECT_GT(sComm.sendVar(2), 0);
  EXPECT_GT(sComm.send_eof(), 0);
  int result = -1;
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, 0);
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, 2);
  EXPECT_EQ(rComm.recvVar(result), -2);
}
TEST(DefaultCommu, filter_send) {
  DefaultComm sComm("", nullptr, SEND);
  DefaultComm rComm("", new utils::Address(sComm.getAddress()), RECV);
  sComm.getMetadata().addFilter(example_filter);
  EXPECT_GT(sComm.sendVar(0), 0);
  EXPECT_EQ(sComm.sendVar(1), 0);
  EXPECT_GT(sComm.sendVar(2), 0);
  EXPECT_GT(sComm.send_eof(), 0);
  int result = -1;
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, 0);
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, 2);
  EXPECT_EQ(rComm.recvVar(result), -2);
}

TEST(DefaultCommu, transform_recv) {
  DefaultComm sComm("", nullptr, SEND);
  DefaultComm rComm("", new utils::Address(sComm.getAddress()), RECV);
  rComm.getMetadata().addTransform(&example_transform);
  EXPECT_GT(sComm.sendVar(0), 0);
  EXPECT_GT(sComm.sendVar(1), 0);
  EXPECT_GT(sComm.sendVar(2), 0);
  EXPECT_GT(sComm.send_eof(), 0);
  std::string result = "";
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, "0");
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, "1");
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, "2");
  EXPECT_EQ(rComm.recvVar(result), -2);
}
TEST(DefaultCommu, transform_send) {
  DefaultComm sComm("", nullptr, SEND);
  DefaultComm rComm("", new utils::Address(sComm.getAddress()), RECV);
  sComm.getMetadata().addTransform(&example_transform);
  EXPECT_GT(sComm.sendVar(0), 0);
  EXPECT_GT(sComm.sendVar(1), 0);
  EXPECT_GT(sComm.sendVar(2), 0);
  EXPECT_GT(sComm.send_eof(), 0);
  std::string result = "";
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, "0");
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, "1");
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, "2");
  EXPECT_EQ(rComm.recvVar(result), -2);
}
