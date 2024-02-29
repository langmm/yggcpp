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

TEST(ForkComm, constructor_errors) {
  EXPECT_THROW(ForkComm("", SEND, 0, DEFAULT_COMM, 0), std::exception);
  EXPECT_THROW(ForkComm("n1,n2,n3", "a1,a2", SEND, 0, DEFAULT_COMM, 0),
	       std::exception);
  EXPECT_THROW(ForkComm("n1,n2", "a1,a2,a3", SEND, 0, DEFAULT_COMM, 0),
	       std::exception);
#ifdef ZMQINSTALLED
  EXPECT_THROW(ForkComm("fork", "a1,a2,a3", SEND, 0, ZMQ_COMM, 0),
	       std::exception);
#endif // ZMQINSTALLED
}

#ifdef ELF_AVAILABLE
TEST(ForkComm, elf_errors_default) {
  ForkComm sComm("fork", SEND, COMM_FLAG_SET_OPP_ENV, DEFAULT_COMM, 2);
  YggInput rComm("fork");
  double sData = 5.0, rData = 0.0;
  EXPECT_GE(sComm.sendVar(sData), 0);
  EXPECT_EQ(rComm.nmsg(), 2);
  ELF_BEGIN;
  RETVAL = -1;
  ELF_REPLACE_NMSG;
  EXPECT_EQ(rComm.nmsg(), -1);
  ELF_RESTORE_NMSG;
  EXPECT_EQ(rComm.nmsg(), 2);
  ELF_REPLACE_SEND;
  EXPECT_EQ(sComm.sendVar(sData), -1);
  ELF_RESTORE_SEND;
  ELF_REPLACE_RECV;
  EXPECT_EQ(rComm.recvVar(rData), -1);
  ELF_RESTORE_RECV;
  ELF_END;
}
TEST(ForkComm, elf_errors_composite) {
  ForkComm sComm("fork", SEND,
		 COMM_FLAG_SET_OPP_ENV | COMM_FLAG_FORK_COMPOSITE,
		 DEFAULT_COMM, 2);
  YggInput rComm("fork", COMM_FLAG_FORK_COMPOSITE);
  rapidjson::Document sData(rapidjson::kArrayType), rData;
  sData.PushBack(rapidjson::Value("hello", 5,
				  sData.GetAllocator()).Move(),
		 sData.GetAllocator());
  sData.PushBack(rapidjson::Value(5.0).Move(),
		 sData.GetAllocator());
  EXPECT_GE(sComm.sendVar(sData), 0);
  EXPECT_EQ(rComm.nmsg(), 1);
  ELF_BEGIN;
  RETVAL = -1;
  ELF_REPLACE_NMSG;
  EXPECT_EQ(rComm.nmsg(), -1);
  ELF_RESTORE_NMSG;
  EXPECT_EQ(rComm.nmsg(), 1);
  ELF_REPLACE_SEND;
  EXPECT_EQ(sComm.sendVar(sData), -1);
  ELF_RESTORE_SEND;
  ELF_REPLACE_RECV;
  EXPECT_EQ(rComm.recvVar(rData), -1);
  ELF_RESTORE_RECV;
  ELF_END;
}
#endif // ELF_AVAILABLE

TEST(ForkComm, default_pattern) {
  ForkComm sComm("fork", SEND, COMM_FLAG_SET_OPP_ENV, DEFAULT_COMM, 2);
  // ForkComm rComm("fork", RECV, COMM_FLAG_INTERFACE, DEFAULT_COMM);
  YggInput rComm("fork");
  double sData = 5.0, rData = 0.0;
  EXPECT_EQ(sComm.recvVar(rData), -1);
  EXPECT_GE(sComm.sendVar(sData), 0);
  EXPECT_EQ(rComm.nmsg(), 2);
  EXPECT_EQ(rComm.nmsg(SEND), 0);
  EXPECT_EQ(sComm.nmsg(RECV), 0);
  EXPECT_GE(rComm.recvVar(rData), 0);
  EXPECT_EQ(rData, sData);
  EXPECT_EQ(rComm.nmsg(), 1);
  EXPECT_GE(rComm.recvVar(rData), 0);
  EXPECT_EQ(rData, sData);
  EXPECT_EQ(rComm.nmsg(), 0);
  EXPECT_GE(sComm.send_eof(), 0);
  EXPECT_EQ(rComm.recvVar(rData), -2);
}

TEST(ForkComm, global) {
  double sData = 5.0, rData = 0.0;
  std::string name = "global_fork";
  global_scope_comm_on();
  {
    ForkComm sComm(name, SEND, COMM_FLAG_SET_OPP_ENV, DEFAULT_COMM, 2);
    YggInput rComm(name);
    EXPECT_GE(sComm.sendVar(sData), 0);
    EXPECT_EQ(rComm.nmsg(), 2);
    EXPECT_EQ(rComm.nmsg(SEND), 0);
    EXPECT_EQ(sComm.nmsg(RECV), 0);
    EXPECT_GE(rComm.recvVar(rData), 0);
    EXPECT_EQ(rData, sData);
    EXPECT_EQ(rComm.nmsg(), 1);
  }
  {
    ForkComm sComm(name, SEND, COMM_FLAG_SET_OPP_ENV, DEFAULT_COMM, 2);
    YggInput rComm(name);
    EXPECT_EQ(rComm.nmsg(), 1);
    EXPECT_GE(rComm.recvVar(rData), 0);
    EXPECT_EQ(rData, sData);
    EXPECT_EQ(rComm.nmsg(), 0);
    EXPECT_GE(sComm.send_eof(), 0);
    EXPECT_EQ(rComm.recvVar(rData), -2);
  }
  global_scope_comm_off();
  ygg_cleanup(CLEANUP_COMMS);
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
  EXPECT_EQ(rComm.nmsg(), 1);
  EXPECT_EQ(rComm.nmsg(SEND), 0);
  EXPECT_EQ(sComm.nmsg(RECV), 0);
  EXPECT_GE(rComm.recvVar(rData), 0);
  EXPECT_EQ(rData, sData);
  EXPECT_EQ(rComm.nmsg(), 0);
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
  EXPECT_EQ(rComm.nmsg(), 2);
  EXPECT_EQ(rComm.nmsg(SEND), 0);
  EXPECT_EQ(sComm.nmsg(RECV), 0);
  EXPECT_GE(rComm.recvVar(rData), 0);
  EXPECT_EQ(rData, sData);
  EXPECT_EQ(rComm.nmsg(), 1);
  EXPECT_GE(rComm.recvVar(rData), 0);
  EXPECT_EQ(rData, sData);
  EXPECT_EQ(rComm.nmsg(), 0);
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
  EXPECT_EQ(rComm.nmsg(), 1);
  EXPECT_EQ(rComm.nmsg(SEND), 0);
  EXPECT_EQ(sComm.nmsg(RECV), 0);
  EXPECT_GE(rComm.recvVar(rData), 0);
  EXPECT_EQ(rData, sData);
  EXPECT_GE(sComm.sendVar(sData), 0);
  EXPECT_EQ(rComm.nmsg(), 1);
  EXPECT_GE(rComm.recvVar(rData), 0);
  EXPECT_EQ(rData, sData);
  EXPECT_EQ(rComm.nmsg(), 0);
  EXPECT_GE(sComm.send_eof(), 0);
  EXPECT_EQ(rComm.recvVar(rData), -2);
}

TEST(ForkComm, filter_send) {
  ForkComm sComm("fork", SEND,
		 COMM_FLAG_SET_OPP_ENV | COMM_FLAG_FORK_CYCLE,
		 DEFAULT_COMM, 2);
  YggInput rComm("fork", COMM_FLAG_FORK_CYCLE);
  EXPECT_TRUE(sComm.getMetadata().addFilter(&example_filter));
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

TEST(ForkComm, transform_send) {
  ForkComm sComm("fork", SEND,
		 COMM_FLAG_SET_OPP_ENV | COMM_FLAG_FORK_CYCLE,
		 DEFAULT_COMM, 2);
  YggInput rComm("fork", COMM_FLAG_FORK_CYCLE);
  EXPECT_TRUE(sComm.getMetadata().addTransform(&example_transform));
  EXPECT_GT(sComm.sendVar(0), 0);
  EXPECT_LT(sComm.sendVar(5), 0);
  EXPECT_GT(sComm.sendVar(2), 0);
  EXPECT_GT(sComm.send_eof(), 0);
  std::string result = "";
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, "0");
  EXPECT_GT(rComm.recvVar(result), 0);
  EXPECT_EQ(result, "2");
  EXPECT_EQ(rComm.recvVar(result), -2);
}

TEST(ForkComm, filter_recv) {
  ForkComm sComm("fork", SEND,
		 COMM_FLAG_SET_OPP_ENV | COMM_FLAG_FORK_COMPOSITE,
		 DEFAULT_COMM, 2);
  YggInput rComm("fork", COMM_FLAG_FORK_CYCLE);
  EXPECT_TRUE(rComm.getMetadata().addFilter(example_filter));
  rapidjson::Document sData(rapidjson::kArrayType), rData, eData;
  eData.SetInt(2);
  sData.PushBack(rapidjson::Value(1).Move(), sData.GetAllocator());
  sData.PushBack(rapidjson::Value(2).Move(), sData.GetAllocator());
  EXPECT_GE(sComm.sendVar(sData), 0);
  EXPECT_EQ(rComm.nmsg(), 2);
  EXPECT_GT(rComm.recvVar(rData), 0);
  EXPECT_EQ(rData, eData);
}

TEST(ForkComm, coerce_composite) {
  ForkComm sComm("fork", SEND,
		 COMM_FLAG_SET_OPP_ENV | COMM_FLAG_FORK_COMPOSITE,
		 DEFAULT_COMM, 2);
  YggInput rComm("fork", COMM_FLAG_FORK_COMPOSITE);
  rapidjson::Document sData, rData;
  sData.Parse("{\"a\": \"a\", \"b\": 1}");
  EXPECT_GE(sComm.sendVar(sData), 0);
  EXPECT_EQ(rComm.nmsg(), 1);
  EXPECT_GE(rComm.recv_dict(rData), 0);
  EXPECT_EQ(rData, sData);
  EXPECT_EQ(rComm.nmsg(), 0);
  EXPECT_GE(sComm.send_eof(), 0);
  EXPECT_EQ(rComm.recv_dict(rData), -2);
}

TEST(ForkComm, coerce_composite_error) {
  ForkComm sComm("fork", SEND,
		 COMM_FLAG_SET_OPP_ENV | COMM_FLAG_FORK_COMPOSITE,
		 DEFAULT_COMM, 2);
  YggInput rComm("fork", COMM_FLAG_FORK_COMPOSITE);
  rapidjson::Document sData, rData;
  sData.Parse("{\"a\": \"a\", \"b\": 1, \"c\": 5.0}");
  EXPECT_LT(sComm.sendVar(sData), 0);
  EXPECT_GE(sComm.send_eof(), 0);
  EXPECT_EQ(rComm.recv_dict(rData), -2);
}
