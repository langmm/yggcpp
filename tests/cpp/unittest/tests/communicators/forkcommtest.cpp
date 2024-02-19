#include "../../unittest.hpp"
#include "utils/Address.hpp"
#include "utils/tools.hpp"
#include "communicators/ForkComm.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"
#include "commtest.hpp"
#include "YggInterface.hpp"

using namespace YggInterface;
using namespace YggInterface::communicator;
using namespace YggInterface::mock;

TEST(ForkComm, default_pattern) {
  ForkComm sComm("fork", SEND, COMM_FLAG_SET_OPP_ENV, DEFAULT_COMM, 2);
  // ForkComm rComm("fork", RECV, COMM_FLAG_INTERFACE, DEFAULT_COMM);
  YggInput rComm("fork");
  double sData = 5.0, rData = 0.0;
  EXPECT_GE(sComm.sendVar(sData), 0);
  EXPECT_EQ(rComm.comm_nmsg(), 2);
  EXPECT_EQ(rComm.comm_nmsg(SEND), 0);
  EXPECT_EQ(sComm.comm_nmsg(RECV), 0);
  EXPECT_GE(rComm.recvVar(rData), 0);
  EXPECT_EQ(rData, sData);
  EXPECT_EQ(rComm.comm_nmsg(), 1);
  EXPECT_GE(rComm.recvVar(rData), 0);
  EXPECT_EQ(rData, sData);
  EXPECT_EQ(rComm.comm_nmsg(), 0);
  EXPECT_GE(sComm.send_eof(), 0);
  EXPECT_EQ(rComm.recvVar(rData), -2);
}

TEST(ForkComm, composite_pattern) {
  ForkComm sComm("fork", SEND,
		 COMM_FLAG_SET_OPP_ENV | COMM_FLAG_FORK_COMPOSITE,
		 DEFAULT_COMM, 2);
  YggInput rComm("fork", COMM_FLAG_FORK_COMPOSITE);
  rapidjson::Document invalidData;
  invalidData.SetInt(5);
  rapidjson::Document sData(rapidjson::kArrayType);
  sData.PushBack(rapidjson::Value("hello", 5, sData.GetAllocator()).Move(),
		 sData.GetAllocator());
  EXPECT_EQ(sComm.sendVar(invalidData), -1);
  // TODO: Check type when updating metadata
  sComm.getMetadata().reset();
  EXPECT_EQ(sComm.sendVar(sData), -1);
  sComm.getMetadata().reset();
  sData.PushBack(rapidjson::Value(5.0).Move(),
		 sData.GetAllocator());
  rapidjson::Document rData;
  EXPECT_GE(sComm.sendVar(sData), 0);
  EXPECT_EQ(rComm.comm_nmsg(), 1);
  EXPECT_EQ(rComm.comm_nmsg(SEND), 0);
  EXPECT_EQ(sComm.comm_nmsg(RECV), 0);
  EXPECT_GE(rComm.recvVar(rData), 0);
  EXPECT_EQ(rData, sData);
  EXPECT_EQ(rComm.comm_nmsg(), 0);
  EXPECT_GE(sComm.send_eof(), 0);
  EXPECT_EQ(rComm.recvVar(rData), -2);
}


TEST(ForkComm, broadcast_cycle_pattern) {
  ForkComm sComm("fork", SEND,
		 COMM_FLAG_SET_OPP_ENV | COMM_FLAG_FORK_BROADCAST,
		 DEFAULT_COMM, 2);
  YggInput rComm("fork", COMM_FLAG_FORK_CYCLE);
  double sData = 5.0, rData = 0.0;
  EXPECT_GE(sComm.sendVar(sData), 0);
  EXPECT_EQ(rComm.comm_nmsg(), 2);
  EXPECT_EQ(rComm.comm_nmsg(SEND), 0);
  EXPECT_EQ(sComm.comm_nmsg(RECV), 0);
  EXPECT_GE(rComm.recvVar(rData), 0);
  EXPECT_EQ(rData, sData);
  EXPECT_EQ(rComm.comm_nmsg(), 1);
  EXPECT_GE(rComm.recvVar(rData), 0);
  EXPECT_EQ(rData, sData);
  EXPECT_EQ(rComm.comm_nmsg(), 0);
  EXPECT_GE(sComm.send_eof(), 0);
  EXPECT_EQ(rComm.recvVar(rData), -2);
}

TEST(ForkComm, cycle_cycle_pattern) {
  ForkComm sComm("fork", SEND,
		 COMM_FLAG_SET_OPP_ENV | COMM_FLAG_FORK_CYCLE,
		 DEFAULT_COMM, 2);
  YggInput rComm("fork", COMM_FLAG_FORK_CYCLE);
  double sData = 5.0, rData = 0.0;
  EXPECT_GE(sComm.sendVar(sData), 0);
  EXPECT_EQ(rComm.comm_nmsg(), 1);
  EXPECT_EQ(rComm.comm_nmsg(SEND), 0);
  EXPECT_EQ(sComm.comm_nmsg(RECV), 0);
  EXPECT_GE(rComm.recvVar(rData), 0);
  EXPECT_EQ(rData, sData);
  EXPECT_GE(sComm.sendVar(sData), 0);
  EXPECT_EQ(rComm.comm_nmsg(), 1);
  EXPECT_GE(rComm.recvVar(rData), 0);
  EXPECT_EQ(rData, sData);
  EXPECT_EQ(rComm.comm_nmsg(), 0);
  EXPECT_GE(sComm.send_eof(), 0);
  EXPECT_EQ(rComm.recvVar(rData), -2);
}


// TODO
// TEST(ForkComm, transform) {
// }
