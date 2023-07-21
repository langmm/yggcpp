#pragma once

#include "CommBase.hpp"

#ifdef ZMQINSTALLED
#include <zmq.h>
#endif

#include <vector>
namespace communication {
#ifdef YGG_TEST
namespace testing {
class ZMQSocket_tester;
}
#endif
namespace communicator {
//class ClientComm;
/* extern double _wait_send_t; */
#if defined(_MSC_VER) && defined(_OPENMP)
extern __declspec(thread) char _reply_msg[100];
extern __declspec(thread) char _purge_msg[100];
extern __declspec(thread) int _zmq_sleeptime;
#else // _MSC_VER
extern char _reply_msg[100];
extern char _purge_msg[100];
extern int _zmq_sleeptime;
#ifdef _OPENMP
#pragma omp threadprivate(_reply_msg, _purge_msg, _zmq_sleeptime)
#endif
#endif // _MSC_VER

class ZMQContext {
public:
  ZMQContext();
private:
  ZMQContext(const ZMQContext& rhs) = delete;
  ZMQContext& operator=(const ZMQContext& rhs) = delete;
public:
#ifdef ZMQINSTALLED
  void init();
  static void destroy();
#else
  void init() { UNINSTALLED_ERROR(ZMQ); }
#endif
  void* ctx;
  static void* ygg_s_process_ctx;
};
  

class ZMQSocket {
private:
  ZMQSocket& operator=(const ZMQSocket& rhs) = delete;
public:
  ZMQSocket();
  ZMQSocket(const ZMQSocket& rhs);
  ZMQSocket(int type0, utils::Address* address = NULL,
	    int linger = 0, int immediate = 1,
	    int sndtimeo = -1);
  void init(int type0, std::string address,
	    int linger = 0, int immediate = 1,
	    int sndtimeo = -1);
#ifdef ZMQINSTALLED
  void init(int type0, utils::Address* address = NULL,
	    int linger = 0, int immediate = 1,
	    int sndtimeo = -1);
  int poll(int method, int tout);
  int send(const std::string msg);
  template<typename T>
  int set(int member, const T& data);
  int recv(std::string& msg, bool for_identity=false);
  void destroy();
#else
  void init(int, utils::Address* = NULL, int = 0, int = 1, int = -1) {
    UNINSTALLED_ERROR(ZMQ);
  }
  void destroy() {}
#endif
  ~ZMQSocket();

  void *handle;               //  The libzmq socket handle
  std::string endpoint;       //  Last bound endpoint, if any
  int type;                   //  Socket type
  ZMQContext ctx;             //  Context used to create the socket
private:
  static int _last_port;
  static int _last_port_set;
#ifdef YGG_TEST
public:
  friend class testing::ZMQSocket_tester;
  static void resetPort();
#endif
};

/*!
  @brief Struct to store info for reply.
*/
class ZMQReply {
public:
  ZMQReply(DIRECTION dir);
#ifdef ZMQINSTALLED
  void clear();
  int create(std::string& endpoint);
  int find(std::string endpoint);
  int set(std::string endpoint = "");
  bool recv(std::string msg_send="");
  bool recv_stage1(std::string msg_send="");
  bool recv_stage2(std::string msg_send="");
  bool send();
  bool send_stage1(std::string& msg_data);
  bool send_stage2(const std::string msg_data);
#endif // ZMQINSTALLED

  std::vector<ZMQSocket> sockets;
  int n_msg;
  int n_rep;
  DIRECTION direction;
  int last_idx;
#ifdef YGG_TEST
  static bool return_val;
  static void set_return_val(bool new_val);
#endif // YGG_TEST
};

class ZMQComm : public CommBase<ZMQSocket> {
public:
    /**
     * Constructor
     * @param name The name of the communicator
     * @param address The address to associate with the communicator, if address is nullptr
     *                then an address will be created.
     * @param direction Enumerated direction for communicator
     * @param flgs Bitwise flags describing the communicator
     */
    explicit ZMQComm(const std::string name = "",
		     utils::Address *address = new utils::Address(),
                     const DIRECTION direction = NONE,
		     int flgs = 0, const COMM_TYPE type = ZMQ_COMM);
    ADD_CONSTRUCTORS(ZMQComm, ZMQ_COMM)

#ifdef ZMQINSTALLED
    /**
     * Destructor
     */
    ~ZMQComm() override;

    /**
     * The number of messages in the queue
     * @return The number of messages
     */
    int comm_nmsg() const override;
    using Comm_t::send;
    using Comm_t::recv;

#ifndef YGG_TEST
protected:
#endif
    void init();
    bool init_handle();
    int send_single(const char *data, const size_t &len, const utils::Header& header) override;
    long recv_single(char*& data, const size_t &len, bool allow_realloc) override;
    virtual bool do_reply_recv(const utils::Header& header);
    virtual bool do_reply_send(const utils::Header& header);
    bool create_header_send(utils::Header& header, const char* data, const size_t &len) override;
    bool create_header_recv(utils::Header& header, char*& data, const size_t &len,
			    size_t msg_len, int allow_realloc,
			    int temp) override;
    WORKER_METHOD_DECS(ZMQComm);
    Comm_t* create_worker_send(utils::Header& head) override;
    Comm_t* create_worker_recv(utils::Header& head) override;
#ifdef YGG_TEST
    bool afterSendRecv(Comm_t* sComm, Comm_t* rComm) override;
#endif
#else
    void init() { UNINSTALLED_ERROR(ZMQ); }
#endif

#ifdef YGG_TEST
public:
    ZMQReply& getReply() { return reply; }
#endif
  
private:
    friend class ClientComm;
    friend class ServerComm;

    ZMQReply reply;

};

}
}
