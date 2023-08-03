#include <thread>
#include <cstdlib>

#define DO_SEND_RECV_EXCHANGE(init_data, comp_data, send_method, send_args, recv_method, recv_args) \
  init_data;								\
  EXPECT_GE(sComm.send_method send_args, 0);				\
  EXPECT_GT(rComm.wait_for_recv(10000), 0);				\
  EXPECT_GT(rComm.comm_nmsg(), 0);					\
  rComm.set_timeout_recv(10000);					\
  EXPECT_GE(rComm.recv_method recv_args, 0);				\
  EXPECT_TRUE(sComm.afterSendRecv(&sComm, &rComm));			\
  comp_data
#define DO_SEND_RECV_EOF(recv_method, recv_args)			\
  EXPECT_GE(sComm.send_eof(), 0);					\
  EXPECT_GE(rComm.recv_method recv_args, -2);				\
  EXPECT_TRUE(sComm.afterSendRecv(&sComm, &rComm))

#define DO_SEND_RECV_BASE(init_data, comp_data, send_method, recv_method, send_args, recv_args) \
  DO_SEND_RECV_EXCHANGE(init_data, comp_data, send_method, send_args, recv_method, recv_args); \
  DO_SEND_RECV_EOF(recv_method, recv_args)

#define DO_SEND_RECV_REQUEST(init_data, comp_data, send_method, send_args, recv_method, recv_args) \
  DO_SEND_RECV_EXCHANGE(init_data, comp_data, send_method, send_args, recv_method, recv_args)
  
#define DO_SEND_RECV_RESPONSE(init_data, comp_data, send_method, send_args, recv_method, recv_args) \
  init_data;								\
  EXPECT_GE(rComm.send_method send_args, 0);				\
  EXPECT_GE(sComm.recv_method recv_args, 0);				\
  EXPECT_TRUE(rComm.afterSendRecv(&rComm, &sComm));			\
  comp_data;								\
  EXPECT_GE(sComm.send_eof(), 0);					\
  EXPECT_GE(rComm.recv_method recv_args, -2);				\
  EXPECT_TRUE(sComm.afterSendRecv(&sComm, &rComm))

#define DO_RPC_SIGNON							\
  {									\
    std::string msg_cli = YGG_CLIENT_SIGNON;				\
    communication::utils::Header header;				\
    EXPECT_TRUE(sComm.create_header_send(header, msg_cli.c_str(), msg_cli.size())); \
    size_t len = header.format(msg_cli.c_str(), msg_cli.size(), 0);	\
    msg_cli.assign(header.data[0], len);				\
    EXPECT_GE(rComm.getRequests().addRequestServer(header), 0);		\
    std::string msg_srv = YGG_SERVER_SIGNON;				\
    EXPECT_GE(rComm.send(msg_srv.c_str(), msg_srv.size()), 0);		\
  }


#define INIT_DATA_SINGLE(type, value)		\
  type data_send = value;			\
  type data_recv

#define INIT_DATA_CHAR				\
  const char data_send[10] = "hello";		\
  char data_recv[10];				\
  size_t n_send = 5;				\
  size_t n_recv = 10

#define INIT_DATA_DOUBLE			\
  int a_send = 1;				\
  int a_recv;					\
  double b_send = 5.5;				\
  double b_recv

#define INIT_DATA_TRIPLE			\
  INIT_DATA_DOUBLE;				\
  const char c_send[10] = "hello";		\
  char c_recv[10];				\
  size_t nc_send = 5;				\
  size_t nc_recv = 10

#define INIT_DATA_TRIPLE_REALLOC		\
  INIT_DATA_DOUBLE;				\
  const char* c_send = "hello";			\
  char* c_recv = NULL;				\
  size_t nc_send = 5;				\
  size_t nc_recv = 0

#define INIT_DATA_TABLE_SEND				\
  size_t n_send = 3;					\
  int a_send[3];					\
  a_send[0] = 0;					\
  a_send[1] = 1;					\
  a_send[2] = 2;					\
  double b_send[3];					\
  b_send[0] = 0.0;					\
  b_send[1] = 1.0;					\
  b_send[2] = 2.0;					\
  char c_send[30];					\
  memset(c_send, 0, 30);				\
  strcpy(&(c_send[0]), "one");				\
  strcpy(&(c_send[10]), "two");				\
  strcpy(&(c_send[20]), "three");			\
  size_t nc_send = 10
#define INIT_DATA_TABLE				\
  INIT_DATA_TABLE_SEND;				\
  size_t n_recv = 3;				\
  int a_recv[3];				\
  double b_recv[3];				\
  char c_recv[30]
#define INIT_DATA_TABLE_REALLOC			\
  INIT_DATA_TABLE_SEND;				\
  size_t n_recv = 0;				\
  int* a_recv = NULL;				\
  double* b_recv = NULL;			\
  char* c_recv = NULL

#define INIT_DATA_PLY						\
  rapidjson::Document sd;					\
  rapidjson::Document result;					\
  sd.Parse("{\"type\": \"ply\"}");				\
  rapidjson::SchemaDocument s(sd);				\
  rapidjson::SchemaValidator validator(s);			\
  validator.GenerateData(result);				\
  rapidjson::Ply data_send = result.GetPly();			\
  rapidjson::Ply data_recv
#define INIT_DATA_OBJ						\
  rapidjson::Document sd;					\
  rapidjson::Document result;					\
  sd.Parse("{\"type\": \"obj\"}");				\
  rapidjson::SchemaDocument s(sd);				\
  rapidjson::SchemaValidator validator(s);			\
  validator.GenerateData(result);				\
  rapidjson::ObjWavefront data_send = result.GetObjWavefront();	\
  rapidjson::ObjWavefront data_recv
#define INIT_DATA_GEOM_C(name, c_name)				\
  rapidjson::Document sd;					\
  rapidjson::Document result;					\
  sd.Parse("{\"type\": \"" #c_name "\"}");			\
  rapidjson::SchemaDocument s(sd);				\
  rapidjson::SchemaValidator validator(s);			\
  validator.GenerateData(result);				\
  c_name ## _t data_send = init_ ## c_name();			\
  c_name ## _t data_recv = init_ ## c_name();			\
  rapidjson::name* data_send_x = new rapidjson::name();		\
  rapidjson::name* data_recv_x = new rapidjson::name();		\
  result.Get(*data_send_x);					\
  data_send.obj = (void*)data_send_x;				\
  data_recv.obj = (void*)data_recv_x


#define INIT_DATA_SCHEMA(schema)				\
  rapidjson::Document sd;					\
  rapidjson::Document data_send;				\
  sd.Parse(schema);						\
  rapidjson::SchemaDocument s(sd);				\
  rapidjson::SchemaValidator validator(s);			\
  validator.GenerateData(data_send);				\
  rapidjson::Document data_recv

#define INIT_DATA_SCHEMA_C(schema)					\
  rapidjson::Document sd;						\
  rapidjson::Document* data_send_doc = new rapidjson::Document();	\
  rapidjson::Document* data_recv_doc = new rapidjson::Document();	\
  sd.Parse(schema);							\
  rapidjson::SchemaDocument s(sd);					\
  rapidjson::SchemaValidator validator(s);				\
  validator.GenerateData(*data_send_doc);				\
  generic_t data_send = init_generic();					\
  data_send.obj = (void*)data_send_doc;					\
  generic_t data_recv = init_generic();					\
  data_recv.obj = (void*)data_recv_doc

#define COMP_DATA_SINGLE			\
  EXPECT_EQ(data_send, data_recv)

#define COMP_DATA_CHAR				\
  n_recv = n_send;				\
  EXPECT_EQ(n_send, n_recv);			\
  EXPECT_EQ(strcmp(data_send, data_recv), 0)

#define COMP_DATA_TRIPLE			\
  EXPECT_EQ(a_send, a_recv);			\
  EXPECT_EQ(b_send, b_recv);			\
  EXPECT_EQ(strcmp(c_send, c_recv), 0)

#define COMP_DATA_TABLE_BASE(cmp_c)		\
  EXPECT_EQ(n_send, n_recv);			\
  if (n_send == n_recv) {			\
    for (size_t i = 0; i < n_send; i++) {	\
      EXPECT_EQ(a_send[i], a_recv[i]);		\
      EXPECT_EQ(b_send[i], b_recv[i]);		\
      cmp_c					\
    }						\
  }
#define COMP_TABLE_C				\
  EXPECT_EQ(strncmp(&(c_send[i * nc_send]), &(c_recv[i * nc_send]), nc_send), 0);
#define COMP_TABLE_C_REALLOC						\
  if (c_recv != NULL) {							\
    EXPECT_EQ(strncmp(&(c_send[i * nc_send]), c_recv + (i * nc_send), nc_send), 0); \
  }
#define COMP_DATA_TABLE				\
  COMP_DATA_TABLE_BASE(COMP_TABLE_C)
#define COMP_DATA_TABLE_REALLOC			\
  COMP_DATA_TABLE_BASE(COMP_TABLE_C_REALLOC);	\
  free(a_recv);					\
  free(b_recv);					\
  free(c_recv)

#define COMP_DATA_GEOM_C(name)				\
  EXPECT_EQ((*((rapidjson::name*)(data_send.obj))),	\
	    (*((rapidjson::name*)(data_recv.obj))))

#define SEND_NARGS_TRIPLE			\
  (4, a_send, b_send, c_send, nc_send)
#define RECV_NARGS_TRIPLE			\
  (4, &a_recv, &b_recv, &c_recv, &nc_recv)
#define RECV_NARGS_TRIPLE_REALLOC		\
  (4, &a_recv, &b_recv, &c_recv, &nc_recv)

#define SEND_NARGS_TABLE			\
  (4, n_send, a_send, b_send, c_send)
#define RECV_NARGS_TABLE			\
  (4, &n_recv, &a_recv, &b_recv, &c_recv)
#define RECV_NARGS_TABLE_REALLOC			\
  (4, &n_recv, &a_recv, &b_recv, &c_recv)

#define DO_SEND_RECV(send_method, recv_method, type, value)		\
  DO_SEND_RECV_BASE(INIT_DATA_SINGLE(type, value), COMP_DATA_SINGLE,	\
		    send_method, recv_method, (data_send), (data_recv))

#define TEST_SEND_RECV(group, name, init_cls, init_data, comp_data, send_method, send_args, recv_method, recv_args) \
  TEST(group, name) {							\
    init_cls;								\
    DO_SEND_RECV_BASE(init_data, comp_data, send_method, recv_method,	\
		      send_args, recv_args);				\
  }
#define TEST_SEND_RECV_RPC(group, name, init_cls, init_data, comp_data, send_method, send_args, recv_method, recv_args)	\
  TEST(group, name) {							\
    init_cls;								\
    DO_RPC_SIGNON;							\
    {									\
      DO_SEND_RECV_REQUEST(init_data, comp_data, send_method, send_args, recv_method, recv_args); \
    }									\
    {									\
      DO_SEND_RECV_RESPONSE(init_data, comp_data, send_method, send_args, recv_method, recv_args); \
    }									\
  }
#define TEST_SEND_RECV_RPC_GLOBAL(group, name, init_cls, init_data, comp_data, send_method, send_args, recv_method, recv_args) \
  TEST(group, name ## Global) {						\
    {									\
      global_scope_comm_on();						\
      init_cls;								\
      global_scope_comm_off();						\
      DO_RPC_SIGNON;							\
      {									\
	DO_SEND_RECV_REQUEST(init_data, comp_data, send_method, send_args, recv_method, recv_args); \
      }									\
      {									\
	DO_SEND_RECV_RESPONSE(init_data, comp_data, send_method, send_args, recv_method, recv_args); \
      }									\
    }									\
    Comm_t::_ygg_cleanup();						\
  }

#define TESTER_METHODS(cls)						\
  cls ## _tester(const std::string name = "",				\
		 utils::Address *address = new utils::Address(),	\
		 const DIRECTION direction = NONE) :			\
  cls(name, address, direction) {}					\
  cls ## _tester(DIRECTION dir) :					\
  cls("", nullptr, dir) {}						\
  cls ## _tester(const std::string name, DIRECTION dir) :		\
  cls(name, dir) {}

#define COMM_SERI_TEST_TYPE(cls, type, value, schema, init)		\
  TEST(cls, type) {							\
    init;								\
    cls ## _tester sComm(SEND);						\
    sComm.addSchema(schema);						\
    std::string name = "test_name";					\
    std::string key_env = name + "_IN";					\
    std::string val_env = sComm.getAddress();				\
    setenv(key_env.c_str(), val_env.c_str(), 1);			\
    cls ## _tester rComm(name, RECV);					\
    unsetenv(key_env.c_str());						\
    DO_SEND_RECV(sendVar, recvVar, type, value);			\
    rComm.close();							\
    EXPECT_EQ(rComm.recvVar(data_recv), -1);				\
  }									\
  TEST(cls, type ## _AsGeneric) {					\
    init;								\
    cls ## _tester sComm(SEND);						\
    sComm.addSchema(schema);						\
    std::string name = "test_name";					\
    std::string key_env = name + "_IN";					\
    std::string val_env = sComm.getAddress();				\
    setenv(key_env.c_str(), val_env.c_str(), 1);			\
    cls ## _tester rComm(name, RECV);					\
    unsetenv(key_env.c_str());						\
    sComm.getMetadata().setGeneric();					\
    rComm.getMetadata().setGeneric();					\
    DO_SEND_RECV(sendVar, recvVar, type, value);			\
    rComm.close();							\
    EXPECT_EQ(rComm.recvVar(data_recv), -1);				\
  }
#define COMM_SERI_TEST_GEOM(cls, type, init_data, schema, init)		\
  TEST(cls, type) {							\
    init;								\
    cls ## _tester sComm(SEND);						\
    sComm.addSchema(schema);						\
    std::string name = "test_name";					\
    std::string key_env = name + "_IN";					\
    std::string val_env = sComm.getAddress();				\
    setenv(key_env.c_str(), val_env.c_str(), 1);			\
    cls ## _tester rComm(name, RECV);					\
    unsetenv(key_env.c_str());						\
    sComm.getMetadata().setGeneric();					\
    rComm.getMetadata().setGeneric();					\
    DO_SEND_RECV_EXCHANGE(init_data, COMP_DATA_SINGLE,			\
			  sendVar, (data_send),				\
			  recvVar, (data_recv));			\
    rComm.close();							\
    EXPECT_EQ(rComm.recvVar(data_recv), -1);				\
  }

#define COMM_SERI_TEST_BASE(cls, init)					\
  COMM_SERI_TEST_TYPE(cls, double, 1.5, "{\"type\": \"number\"}", init)	\
  COMM_SERI_TEST_TYPE(cls, int, 32, "{\"type\": \"integer\"}", init)	\
  COMM_SERI_TEST_TYPE(cls, uint8_t, 3u,					\
		      "{\"type\": \"scalar\","				\
		      " \"subtype\": \"uint\","				\
		      " \"precision\": 1}", init)			\
  COMM_SERI_TEST_TYPE(cls, bool, true, "{\"type\": \"boolean\"}", init)	\
  COMM_SERI_TEST_GEOM(cls, Ply, INIT_DATA_PLY,				\
		      "{\"type\": \"ply\"}", init)			\
  COMM_SERI_TEST_GEOM(cls, ObjWavefront, INIT_DATA_OBJ,			\
		      "{\"type\": \"obj\"}", init)			\
  TEST(cls, generic) {							\
    init;								\
    cls ## _tester sComm(SEND);						\
    sComm.addSchema("{\"type\": \"any\"}");				\
    std::string name = "test_name";					\
    std::string key_env = name + "_IN";					\
    std::string val_env = sComm.getAddress();				\
    setenv(key_env.c_str(), val_env.c_str(), 1);			\
    cls ## _tester rComm(name, RECV);					\
    rComm.addSchema("{\"type\": \"any\"}");				\
    unsetenv(key_env.c_str());						\
    DO_SEND_RECV(sendVarAsGeneric, recvVarAsGeneric, int, 32);		\
    rComm.close();							\
    EXPECT_EQ(rComm.recvVarAsGeneric(data_recv), -1);			\
  }									\
  TEST(cls, incompatible) {						\
    init;								\
    cls ## _tester sComm(SEND);						\
    std::string name = "test_name";					\
    std::string key_env = name + "_IN";					\
    std::string val_env = sComm.getAddress();				\
    setenv(key_env.c_str(), val_env.c_str(), 1);			\
    cls ## _tester rComm(name, RECV);					\
    unsetenv(key_env.c_str());						\
    sComm.addSchema("{\"type\": \"boolean\"}");				\
    rComm.addSchema("{\"type\": \"integer\"}");				\
    /* Invalid send */							\
    EXPECT_EQ(sComm.sendVar(1), -1);					\
    /* Invalid recv */							\
    bool dst = false;							\
    EXPECT_GT(sComm.sendVar(true), 0);					\
    EXPECT_EQ(rComm.recvVar(dst), -1);					\
  }									\
  TEST(cls, global) {							\
    init;								\
    std::string name = "test_name";					\
    global_scope_comm_on();						\
    {									\
      cls ## _tester sComm(name, nullptr, SEND);			\
      sComm.addSchema("{\"type\": \"number\"}");			\
      std::string key_env = name + "_IN";				\
      std::string val_env = sComm.getAddress();				\
      setenv(key_env.c_str(), val_env.c_str(), 1);			\
      cls ## _tester rComm(name, RECV);					\
      unsetenv(key_env.c_str());					\
      DO_SEND_RECV_EXCHANGE(INIT_DATA_SINGLE(double, 1.5),		\
			    COMP_DATA_SINGLE,				\
			    sendVar, (data_send),			\
			    recvVar, (data_recv));			\
    }									\
    {									\
      cls ## _tester sComm(name, nullptr, SEND);			\
      cls ## _tester rComm(name, RECV);					\
      DO_SEND_RECV_EXCHANGE(INIT_DATA_SINGLE(double, 1.5),		\
			    COMP_DATA_SINGLE,				\
			    sendVar, (data_send),			\
			    recvVar, (data_recv));			\
      DO_SEND_RECV_EOF(recvVar, (data_recv));				\
    }									\
    global_scope_comm_off();						\
    Comm_t::_ygg_cleanup();						\
  }
#define COMM_SERI_TEST(cls)						\
  COMM_SERI_TEST_BASE(cls,)						\
  TEST(cls, large) {							\
    cls ## _tester sComm(SEND);						\
    std::string name = "test_name";					\
    std::string key_env = name + "_IN";					\
    std::string val_env = sComm.getAddress();				\
    setenv(key_env.c_str(), val_env.c_str(), 1);			\
    cls ## _tester rComm(name, RECV);					\
    unsetenv(key_env.c_str());						\
    if (sComm.getMaxMsgSize() > 0) {					\
      /* Add worker in advance so that send is successful */		\
      Comm_t* sComm_worker = sComm.getWorkers().get(&sComm, SEND);	\
      rComm.getWorkers().get(&rComm, RECV, new utils::Address(sComm_worker->getAddress())); \
      EXPECT_EQ(rComm.getWorkers().find_worker(sComm_worker), -1);	\
      rComm.getWorkers().remove_worker(sComm_worker);			\
      sComm_worker = nullptr;						\
      std::string bigMsg(sComm.getMaxMsgSize(), 'A');			\
      std::string data_send = "\"Test message\"";			\
      std::string data_recv;						\
      std::string schema("{\"serializer\": {\"datatype\": {\"type\": \"string\"}}, \"userData\": \""); \
      schema += bigMsg;							\
      schema += "\"}";							\
      sComm.addSchema(schema, true);					\
      EXPECT_EQ(rComm.wait_for_recv(100), 0);				\
      EXPECT_GE(sComm.send(data_send), 0);				\
      EXPECT_GE(rComm.recv(data_recv), 0);				\
      EXPECT_TRUE(sComm.afterSendRecv(&sComm, &rComm));			\
      EXPECT_EQ(data_send, data_recv);					\
      EXPECT_EQ(rComm.call(2, data_send.c_str(), data_recv.c_str()), -1); \
      /* Error when sending message that can't fit in buffer */		\
      sComm.getFlags() |= COMM_ALWAYS_SEND_HEADER;			\
      utils::Metadata& metadata = sComm.getMetadata();				\
      metadata.initMeta();						\
      metadata.SetMetaString("invalid", bigMsg);			\
      EXPECT_THROW(sComm.send(data_send), std::exception);		\
    }									\
  }

#ifdef ELF_AVAILABLE

#define ELF_SEND_T(type, ret)			\
  ELF_REPLACE_SEND_ ## type;			\
  RETVAL = ret
#define ELF_SEND_REVERT_T(type)			\
  ELF_RESTORE_SEND_ ## type
#define ELF_RECV_T(type, ret)			\
  ELF_REPLACE_NMSG_ ## type;			\
  ELF_REPLACE_RECV_ ## type;			\
  RETMSG = "Hello world";			\
  RETMSG_META = "";				\
  RETMSG_META_DEFAULT = "";			\
  RETVAL = ret
#define ELF_RECV_REVERT_T(type)			\
  ELF_RESTORE_NMSG_ ## type;			\
  ELF_RESTORE_RECV_ ## type
#define ELF_CREATE_T(type, ret)			\
  ELF_REPLACE_CREATE_ ## type;			\
  RETVAL_CREATE = ret
#define ELF_CREATE_REVERT_T(type)		\
  ELF_RESTORE_CREATE_ ## type

#ifdef IPCDEF
#define ELF_REPLACE_RECV ELF_REPLACE_RECV_IPC
#define ELF_RESTORE_RECV ELF_RESTORE_RECV_IPC
#define ELF_REPLACE_SEND ELF_REPLACE_SEND_IPC
#define ELF_RESTORE_SEND ELF_RESTORE_SEND_IPC
#define ELF_REPLACE_NMSG ELF_REPLACE_NMSG_IPC
#define ELF_RESTORE_NMSG ELF_RESTORE_NMSG_IPC
#define ELF_SEND(ret) ELF_SEND_T(IPC, ret)
#define ELF_SEND_REVERT ELF_SEND_REVERT_T(IPC)
#define ELF_RECV(ret) ELF_RECV_T(IPC, ret)
#define ELF_RECV_REVERT ELF_RECV_REVERT_T(IPC)
#define ELF_META(comm) ELF_META_IPC(comm)
#define ELF_CREATE(ret) ELF_CREATE_T(IPC, ret)
#define ELF_CREATE_REVERT ELF_CREATE_REVERT_T(IPC)
#else
#define ELF_REPLACE_RECV ELF_REPLACE_RECV_ZMQ
#define ELF_RESTORE_RECV ELF_RESTORE_RECV_ZMQ
#define ELF_REPLACE_SEND ELF_REPLACE_SEND_ZMQ
#define ELF_RESTORE_SEND ELF_RESTORE_SEND_ZMQ
#define ELF_REPLACE_NMSG ELF_REPLACE_NMSG_ZMQ
#define ELF_RESTORE_NMSG ELF_RESTORE_NMSG_ZMQ
#define ELF_SEND(ret) ELF_SEND_T(ZMQ, ret)
#define ELF_SEND_REVERT ELF_SEND_REVERT_T(ZMQ)
#define ELF_RECV(ret) ELF_RECV_T(ZMQ, ret)
#define ELF_RECV_REVERT ELF_RECV_REVERT_T(ZMQ)
#define ELF_META(comm) ELF_META_ZMQ(comm)
#define ELF_CREATE(ret) ELF_CREATE_T(ZMQ, ret)
#define ELF_CREATE_REVERT ELF_CREATE_REVERT_T(ZMQ)
#endif

#endif // ELF_AVAILABLE
