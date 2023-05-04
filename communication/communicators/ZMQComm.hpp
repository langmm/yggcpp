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
  ZMQContext(const ZMQContext& rhs);
  ZMQContext& operator=(const ZMQContext& rhs);
  static void destroy();
  void* ctx;
  static void* ygg_s_process_ctx;
};
  

class ZMQSocket {
private:
  ZMQSocket& operator=(const ZMQSocket& rhs) = delete;
public:
  ZMQSocket();
  ZMQSocket(int type0, utils::Address* address = NULL,
	    int linger = 0, int immediate = 1,
	    int sndtimeo = -1);
  ZMQSocket(int type0, std::string address,
	    int linger = 0, int immediate = 1,
	    int sndtimeo = -1);
  ZMQSocket(const ZMQSocket& rhs);
  void init(int type0, utils::Address* address = NULL,
	    int linger = 0, int immediate = 1,
	    int sndtimeo = -1);
  void init(int type0, std::string address,
	    int linger = 0, int immediate = 1,
	    int sndtimeo = -1);
  int poll(int method, int tout);
  int send(const std::string msg);
  template<typename T>
  int set(int member, const T& data);
  int recv(std::string& msg, bool for_identity=false);
  void destroy();
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

  std::vector<ZMQSocket> sockets;
  int n_msg;
  int n_rep;
  DIRECTION direction;
  int last_idx;
};

class ZMQComm : public CommBase<ZMQSocket> {
public:
    /**
     * Constructor
     * @param name The name of the communicator
     * @param address The address to associate with the communicator, if address is nullptr
     *                then an address will be created.
     * @param direction Enumerated direction for communicator
     */
    explicit ZMQComm(const std::string name = "", utils::Address *address = new utils::Address(),
                     const DIRECTION direction = NONE,
		     int flgs = 0);
    explicit ZMQComm(const std::string name,
		     const DIRECTION direction, int flgs = 0);

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

#ifdef YGG_TEST
    ZMQReply& getReply() { return reply; }
    bool afterSendRecv(Comm_t* sComm, Comm_t* rComm) override;
#else
protected:
#endif
    void init() override;
    bool init_handle();
    int send_single(const char *data, const size_t &len, const Header& header) override;
    long recv_single(char*& data, const size_t &len, bool allow_realloc) override;
    virtual bool do_reply_recv(const Header& header);
    virtual bool do_reply_send(const Header& header);
    bool create_header_send(Header& header, const char* data, const size_t &len) override;
    bool create_header_recv(Header& header, char*& data, const size_t &len,
			    size_t msg_len, int allow_realloc,
			    int temp) override;
    WORKER_METHOD_DECS(ZMQComm);
    Comm_t* create_worker_recv(Header& head) override;

private:
    friend class ClientComm;
    friend class ServerComm;

    ZMQReply reply;
    void destroy();

};

}
}