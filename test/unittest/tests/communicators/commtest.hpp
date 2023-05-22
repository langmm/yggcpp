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
    cls ## _tester sComm(SEND);						\
    sComm.addSchema(schema);						\
    std::string name = "test_name";					\
    std::string key_env = name + "_IN";					\
    std::string val_env = sComm.getAddress();				\
    setenv(key_env.c_str(), val_env.c_str(), 1);			\
    cls ## _tester rComm(name, RECV);					\
    type data_send = value;						\
    type data_recv;							\
    EXPECT_GE(sComm.sendVar(data_send), 0);				\
    EXPECT_GE(rComm.recvVar(data_recv), 0);				\
    EXPECT_TRUE(sComm.afterSendRecv(&sComm, &rComm));			\
    EXPECT_EQ(data_send, data_recv);					\
    EXPECT_GE(sComm.send_eof(), 0);					\
    EXPECT_EQ(rComm.recvVar(data_recv), -2);				\
    EXPECT_TRUE(sComm.afterSendRecv(&sComm, &rComm));			\
  }
#define COMM_SERI_TEST(cls)						\
  COMM_SERI_TEST_TYPE(cls, double, 1.5, "{\"type\": \"number\"}")	\
  COMM_SERI_TEST_TYPE(cls, int, 32, "{\"type\": \"integer\"}")		\
  COMM_SERI_TEST_TYPE(cls, uint8_t, 3u,					\
		      "{\"type\": \"scalar\","				\
		      " \"subtype\": \"uint\","				\
		      " \"precision\": 1}")				\
  COMM_SERI_TEST_TYPE(cls, bool, true, "{\"type\": \"boolean\"}")	\
  TEST(cls, large) {							\
    cls ## _tester sComm(SEND);						\
    std::string name = "test_name";					\
    std::string key_env = name + "_IN";					\
    std::string val_env = sComm.getAddress();				\
    setenv(key_env.c_str(), val_env.c_str(), 1);			\
    cls ## _tester rComm(name, RECV);					\
    if (sComm.getMaxMsgSize() > 0) {					\
      /* Add worker in advance so that send is successful */		\
      Comm_t* sComm_worker = sComm.getWorkers().get(&sComm, SEND);	\
      rComm.getWorkers().get(&rComm, RECV, new utils::Address(sComm_worker->getAddress())); \
      sComm_worker = nullptr;						\
      std::string bigMsg(sComm.getMaxMsgSize(), 'A');			\
      std::string data_send = "\"Test message\"";			\
      std::string data_recv;						\
      std::string schema("{\"serializer\": {\"datatype\": {\"type\": \"string\"}}, \"userData\": \""); \
      schema += bigMsg;							\
      schema += "\"}";							\
      sComm.addSchema(schema, true);					\
      EXPECT_GE(sComm.send(data_send), 0);				\
      EXPECT_GE(rComm.recv(data_recv), 0);				\
      EXPECT_TRUE(sComm.afterSendRecv(&sComm, &rComm));			\
      EXPECT_EQ(data_send, data_recv);					\
      /* Error when sending message that can't fit in buffer */		\
      sComm.getFlags() |= COMM_ALWAYS_SEND_HEADER;			\
      utils::Metadata& metadata = sComm.getMetadata();				\
      metadata.initMeta();						\
      metadata.SetMetaString("invalid", bigMsg);			\
      EXPECT_THROW(sComm.send(data_send), std::exception);		\
    }									\
  }


#ifdef ELF_AVAILABLE
#if COMM_BASE == IPC_COMM
#define ELF_SEND(ret)				\
  ELF_BEGIN_F_RET(msgsnd, ret);			\
  RETVAL_INC_SEND = 0;				\
  RETVAL_INC_POLL = 0
#define ELF_SEND_REVERT				\
  ELF_END_F(msgsnd)
#define ELF_RECV(ret)				\
  ELF_BEGIN_F_RET(msgrcv, ret);			\
  RETVAL_INC_RECV = 0;				\
  RETVAL_INC_POLL = 0
#define ELF_RECV_REVERT				\
  ELF_END_F(msgrcv)
#elif COMM_BASE == ZMQ_COMM
#define ELF_SEND(ret)				\
  ELF_BEGIN_F_RET(zmq_sendmsg, ret);		\
  RETVAL_INC_SEND = 0;				\
  RETVAL_INC_POLL = 0
#define ELF_SEND_REVERT				\
  ELF_END_F(zmq_sendmsg)

#ifdef ZMQ_HAVE_POLLER
#define ELF_RECV(ret)				\
  ELF_BEGIN_F_RET(zmq_recvmsg, ret);		\
  ELF_BEGIN_F(zmq_poller_wait_all);		\
  RETVAL_INC_RECV = 0;				\
  RETVAL_INC_POLL = 0
#define ELF_RECV_REVERT				\
  ELF_END_F(zmq_recvmsg);			\
  ELF_END_F(zmq_poller_wait_all)
#else // ZMQ_HAVE_POLLER
#define ELF_RECV(ret)				\
  ELF_BEGIN_F_RET(zmq_recvmsg, ret);		\
  ELF_BEGIN_F(zmq_poll);			\
  RETVAL_INC_RECV = 0;				\
  RETVAL_INC_POLL = 0
#define ELF_RECV_REVERT				\
  ELF_END_F(zmq_recvmsg);			\
  ELF_END_F(zmq_poll)
#endif // ZMQ_HAVE_POLLER

#endif
#endif // ELF_AVAILABLE
