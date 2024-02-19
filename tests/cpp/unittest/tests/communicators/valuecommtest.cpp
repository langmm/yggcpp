#include "../../unittest.hpp"
#include "utils/Address.hpp"
#include "communicators/ValueComm.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"
#include "commtest.hpp"


using namespace YggInterface;
using namespace YggInterface::communicator;
using namespace YggInterface::mock;

#define VALUE_TEST_TYPE(type, value)		\
  TEST(ValueComm, type) {			\
    ValueComm rComm("test", RECV);		\
    INIT_DATA_SINGLE(type, value);		\
    EXPECT_EQ(rComm.comm_nmsg(), -1);		\
    EXPECT_EQ(rComm.comm_nmsg(SEND), 0);	\
    EXPECT_EQ(rComm.recvVar(data_recv), -1);	\
    rComm.setValue(data_send, 2);		\
    EXPECT_EQ(rComm.comm_nmsg(), 2);		\
    EXPECT_GE(rComm.recvVar(data_recv), 0);	\
    COMP_DATA_SINGLE;				\
    EXPECT_EQ(rComm.comm_nmsg(), 1);		\
    EXPECT_GE(rComm.recvVar(data_recv), 0);	\
    COMP_DATA_SINGLE;				\
    EXPECT_EQ(rComm.comm_nmsg(), 1);		\
    EXPECT_EQ(rComm.recvVar(data_recv), -2);	\
    EXPECT_EQ(rComm.comm_nmsg(), 0);		\
    rComm.close();				\
    EXPECT_EQ(rComm.recvVar(data_recv), -1);	\
  }

TEST(ValueComm, is_installed) {
  EXPECT_TRUE(is_commtype_installed(ValueComm::defaultCommType()));
}

TEST(ValueComm, global) {
  std::string name = "test_value";
  double data_send = 1.5, data_recv = 0.0;
  global_scope_comm_on();
  {
    ValueComm rComm(name, RECV);
    EXPECT_EQ(rComm.comm_nmsg(), -1);
    EXPECT_EQ(rComm.comm_nmsg(SEND), 0);
    EXPECT_EQ(rComm.recvVar(data_recv), -1);
    rComm.setValue(data_send, 2);
    EXPECT_EQ(rComm.comm_nmsg(), 2);
  }
  {
    ValueComm rComm(name, RECV);
    EXPECT_EQ(rComm.comm_nmsg(), 2);
    EXPECT_GE(rComm.recvVar(data_recv), 0);
    EXPECT_EQ(data_recv, data_send);
  }
  {
    ValueComm rComm(name, RECV);
    EXPECT_EQ(rComm.comm_nmsg(), 1);
    EXPECT_GE(rComm.recvVar(data_recv), 0);
    EXPECT_EQ(data_recv, data_send);
    EXPECT_EQ(rComm.recvVar(data_recv), -2);
    EXPECT_EQ(rComm.comm_nmsg(), 0);
  }
  global_scope_comm_off();
  ygg_cleanup(CLEANUP_COMMS);
}

TEST(ValueComm, send) {
  ValueComm sComm("test", SEND);
  sComm.setValue(1, 2);
  EXPECT_EQ(sComm.comm_nmsg(SEND), 0);
  EXPECT_EQ(sComm.sendVar(1), -1);
}

VALUE_TEST_TYPE(double, 1.5)
VALUE_TEST_TYPE(int, 32)
VALUE_TEST_TYPE(uint8_t, 3u)
VALUE_TEST_TYPE(bool, true)
// VALUE_TEST_GEOM(Ply, INIT_DATA_PLY)
// VALUE_TEST_GEOM(ObjWavefront, INIT_DATA_OBJ)
