#include <thread>
#include <cstdlib>

#define TESTER_METHODS(cls)						\
  cls ## _tester(const std::string name = "",				\
		 utils::Address *address = new utils::Address(),	\
		 const DIRECTION direction = NONE) :			\
  cls(name, address, direction) {}					\
  cls ## _tester(DIRECTION dir) :					\
  cls("", nullptr, dir) {}						\
  cls ## _tester(const std::string name, DIRECTION dir) :		\
  cls(name, dir) {}

#define COMM_SERI_TEST_TYPE(cls, type, value, schema)			\
  TEST(cls, type) {							\
    setenv("YGG_MODEL_INDEX", "1", 1);					\
    cls ## _tester sComm(SEND);						\
    sComm.addSchema(schema);						\
    std::string name = "test_name";					\
    std::string name_env = name + "_IN=" + sComm.getAddress();		\
    putenv(const_cast<char*>(name_env.c_str()));			\
    cls ## _tester rComm(name, RECV);					\
    type data_send = value;						\
    type data_recv;							\
    EXPECT_GE(sComm.sendVar(data_send), 0);				\
    EXPECT_GE(rComm.recvVar(data_recv), 0);				\
    EXPECT_TRUE(sComm.afterSendRecv(&sComm, &rComm));			\
    EXPECT_EQ(data_send, data_recv);					\
    unsetenv("YGG_MODEL_INDEX");					\
  }
#define COMM_SERI_TEST(cls)						\
  COMM_SERI_TEST_TYPE(cls, double, 1.5, "{\"type\": \"number\"}")	\
  COMM_SERI_TEST_TYPE(cls, int, 32, "{\"type\": \"integer\"}")		\
  COMM_SERI_TEST_TYPE(cls, uint8_t, 3u,					\
		      "{\"type\": \"scalar\","				\
		      " \"subtype\": \"uint\","				\
		      " \"precision\": 1}")

#define ELF_BEGIN					\
  void *handle = dlopen(SUBLIB, RTLD_LAZY);		\
  if (!handle)						\
    EXPECT_TRUE(false)
#define ELF_END					\
  dlclose(handle)
  
#define ELF_BEGIN_F(func)				\
  void *original_func_ ## func = nullptr;		\
  original_func_ ## func = ELFHOOK(func);		\
  EXPECT_NE(original_func_ ## func, ((void*)0))
#define ELF_BEGIN_F_RET(func, ret)			\
  RETVAL = ret;						\
  ELF_BEGIN_F(func)
#define ELF_END_F(func)				\
  ELFREVERT(func, original_func_ ## func)

#ifdef ELF_AVAILABLE
#if COMM_BASE == IPC_COMM
#define ELF_SEND(ret)				\
  ELF_BEGIN_F_RET(msgsnd, ret)
#define ELF_SEND_REVERT				\
  ELF_END_F(msgsnd)
#define ELF_RECV(ret)				\
  ELF_BEGIN_F_RET(msgrcv, ret)
#define ELF_RECV_REVERT				\
  ELF_END_F(msgrcv)
#else if COMM_BASE == ZMQ_COMM
#define ELF_SEND(ret)				\
  ELF_BEGIN_F_RET(zmq_sendmsg, ret)
#define ELF_SEND_REVERT				\
  ELF_END_F(zmq_sendmsg)
#define ELF_RECV(ret)				\
  ELF_BEGIN_F_RET(zmq_recvmsg, ret)
#define ELF_RECV_REVERT				\
  ELF_END_F(zmq_recvmsg)
#endif
#endif // ELF_AVAILABLE
