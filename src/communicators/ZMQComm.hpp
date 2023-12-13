#pragma once

#include "CommBase.hpp"

#ifdef ZMQINSTALLED
#include <zmq.h>
#endif

#include <vector>
namespace YggInterface {
#ifdef YGG_TEST
namespace testing {
class ZMQSocket_tester;
}
#endif
namespace communicator {
  
YGG_THREAD_GLOBAL_VAR(char, _reply_msg, [100])
YGG_THREAD_GLOBAL_VAR(char, _purge_msg, [100])
YGG_THREAD_GLOBAL_VAR(int, _zmq_sleeptime, )

/*!
 * @brief Class for ZMQ communication context
 */
class ZMQContext {
public:
    /*!
     * @brief Constructor
     */
    ZMQContext();
    ZMQContext(const ZMQContext& rhs) = delete;
    ZMQContext& operator=(const ZMQContext& rhs) = delete;
#ifdef ZMQINSTALLED
    /*!
     * @brief Initialize the context
     */
  void init();
  /*!
   * @brief Destroy the context
   */
  static void destroy();
#else
  void init() { UNINSTALLED_ERROR(ZMQ); }
#endif
  void* ctx;
  static void* ygg_s_process_ctx;
};
  

/*!
 * @brief Class for ZMQ sockets
 */
class ZMQSocket {
public:
    ZMQSocket& operator=(const ZMQSocket& rhs) = delete;
    /*!
     * @brief Constructor
     */
  ZMQSocket();
  /*!
   * @brief Copy constructor
   * @param[in] rhs The socket to copy
   */
  ZMQSocket(const ZMQSocket& rhs);
  /*!
   * @brief Constructor
   * @param[in] type0 The type of zmq socket
   * @param[in] linger How long to allow queued messages to linger after a socket closes. A value of -1 means infinite wait.
   * @param[in] immediate If set to 1 then messages will only be queued on completed connections
   * @param[in] sndtimeo Sending timeout in ms. A value of -1 will block until the message is sent.
   */
  explicit ZMQSocket(int type0, int linger = 0, int immediate = 1,
                     int sndtimeo = -1);
  /*!
   * @brief Constructor
   * @param[in] type0 The type of zmq socket
   * @param[in] address The address for this socket to connect to
   * @param[in] linger How long to allow queued messages to linger after a socket closes. A value of -1 means infinite wait.
   * @param[in] immediate If set to 1 then messages will only be queued on completed connections
   * @param[in] sndtimeo Sending timeout in ms. A value of -1 will block until the message is sent.
   * @see utils::Address
   */
  ZMQSocket(int type0, utils::Address& address,
	    int linger = 0, int immediate = 1,
	    int sndtimeo = -1);
  /*!
   * @brief Initialize the socket
   * @param[in] type0 The type of zmq socket
   * @param[in] address The string address for this socket to connect to
   * @param[in] linger How long to allow queued messages to linger after a socket closes. A value of -1 means infinite wait.
   * @param[in] immediate If set to 1 then messages will only be queued on completed connections
   * @param[in] sndtimeo Sending timeout in ms. A value of -1 will block until the message is sent.
   */
  void init(int type0, const std::string& address,
	    int linger = 0, int immediate = 1,
	    int sndtimeo = -1);
#ifdef ZMQINSTALLED
  /*!
   * @brief Initialize the socket
   * @param[in] type0 The type of zmq socket
   * @param[in] address The  address for this socket to connect to
   * @param[in] linger How long to allow queued messages to linger after a socket closes. A value of -1 means infinite wait.
   * @param[in] immediate If set to 1 then messages will only be queued on completed connections
   * @param[in] sndtimeo Sending timeout in ms. A value of -1 will block until the message is sent.
   * @see utils::Address
   */
  void init(int type0, utils::Address& address,
            int linger = 0, int immediate = 1,
            int sndtimeo = -1);
  /*!
   * @brief Interface to ZMQ poller
   * @param[in] method The polling method to use.
   * @param[in] tout The timeout to wait
   * @return 0 on success
   */
  int poll(int method, int tout);
  /*!
   * @brief Send the given message
   * @param[in] msg The message to send
   * @return The size of the message, or -1 on error.
   */
  int send(const std::string& msg);
  /*!
   * @brief Set a socket option
   * @tparam T The data type to set
   * @param[in] member Enum of the item to be set
   * @param[in] data The value to use
   * @return 1 on success, -1 on error
   */
  template<typename T>
  int set(int member, const T& data);
  /*!
   * @brief Receive a message
   * @param[out] msg
   * @param[in] for_identity If true then expect an identity message
   * @return The message size, -1 on error.
   */
  int recv(std::string& msg, bool for_identity=false);
  /*!
   * @brief Destroy the socket
   */
  void destroy();
#else
  void init(int, utils::Address&, int = 0, int = 1, int = -1) {
    UNINSTALLED_ERROR(ZMQ);
  }
  void destroy() {}
#endif
  ~ZMQSocket();

  void *handle;               //!<  The libzmq socket handle
  std::string endpoint;       //!<  Last bound endpoint, if any
  int type;                   //!<  Socket type
  ZMQContext ctx;             //!<  Context used to create the socket
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
    /*!
     * @brief Constructor
     * @param dir The direction for the reply
     */
  explicit ZMQReply(DIRECTION dir);
#ifdef ZMQINSTALLED
  /*!
   * @brief Clear the socket
   */
  void clear();
  /*!
   * @brief Create an endpoint
   * @param[in] endpoint The name of the endpoint
   * @return The index of the endpoint
   */
  int create(std::string& endpoint);
  /*!
   * @brief Find an endpoint
   * @param[in] endpoint The endpoint to find
   * @return The index of the endpoint, or -1 on error
   */
  int find(std::string endpoint);
  /*!
   * @brief Set which endpoint to use
   * @param[in] endpoint The name of the endpoint to use
   * @return The index of the enpoint
   */
  int set(std::string endpoint = "");
  /*!
   * Receive a message
   * @param[out] msg_send Where to put the message
   * @return true on success
   */
  bool recv(std::string msg_send="");
  /*!
   * @brief Receive the message
   * @param[in] msg_send where to put the message
   * @return true on success
   */
  bool recv_stage1(std::string msg_send="");
  /*!
   * @brief Receive the message
   * @param[in] msg_send where to put the message
   * @return true on success
   */
  bool recv_stage2(std::string msg_send="");
  /*!
   * @brief Send a message
   * @return true on success
   */
  bool send();
  /*!
   * @brief Send a message
   * @param[out] msg_data Reply from endpoint
   * @return true on success
   */
  bool send_stage1(std::string& msg_data);
  /*!
   * @brief Send a message
   * @param[out] msg_data Reply from endpoint
   * @return true on success
   */
  bool send_stage2(const std::string& msg_data);
#endif // ZMQINSTALLED

  std::vector<ZMQSocket> sockets;   //!< vector of sockets to use
  int n_msg;                        //!< number of messages queued
  int n_rep;                        //!< number of responses
  DIRECTION direction;              //!< communicator direction
  int last_idx;                     //!< index of last used socket
#ifdef YGG_TEST
  static bool return_val;
  static void set_return_val(bool new_val);
#endif // YGG_TEST
};

/*!
 * @brief zeroMQ based communicator
 */
class ZMQComm : public CommBase<ZMQSocket> {
public:
    /**
     * @brief Constructor
     * @param[in] name The name of the communicator
     * @param[in] address The address to associate with the communicator, if address is nullptr
     *                then an address will be created.
     * @param[in] direction Enumerated direction for communicator
     * @param[in] flgs Bitwise flags describing the communicator
     * @param[in] type THe communicator type
     * @see utils::Address
     */
    explicit ZMQComm(const std::string& name,
                     utils::Address& address,
                     const DIRECTION direction = NONE,
                     int flgs = 0, const COMM_TYPE type = ZMQ_COMM);
    ADD_CONSTRUCTORS(ZMQ)

#ifdef ZMQINSTALLED
    /*! \copydoc YggInterface::communicator::Comm_t::close */
    void close() override;

    /*! \copydoc YggInterface::communicator::Comm_t::comm_nmsg */
    int comm_nmsg(DIRECTION dir=NONE) const override;
  
    using Comm_t::send;
    using Comm_t::recv;

#ifndef YGG_TEST
protected:
#endif
    /*!
     * @brief Initialize the communicator
     */
    void init();
    /*! \copydoc YggInterface::communicator::CommBase::send_single */
    int send_single(utils::Header& msg) override;
    /*! \copydoc YggInterface::communicator::CommBase::recv_single */
    long recv_single(utils::Header& msg) override;
    /*!
     * @brief Wait for the message was received signal
     * @param[in] header The header to use
     * @return True on success
     * @see utils::Header
     */
    virtual bool do_reply_recv(const utils::Header& header);
    /*!
     * @brief Send a reply that the message was received
     * @param header The header to use
     * @return True on success
     * @see utils::Header
     */
    virtual bool do_reply_send(const utils::Header& header);
    /*! \copydoc YggInterface::communicator::CommBase::create_header_send */
    bool create_header_send(utils::Header& header) override;
    /*! \copydoc YggInterface::communicator::CommBase::create_worker */
    WORKER_METHOD_DECS(ZMQComm);
    /*! \copydoc YggInterface::communicator::CommBase::create_worker_send */
    Comm_t* create_worker_send(utils::Header& head) override;
    /*! \copydoc YggInterface::communicator::CommBase::create_worker_recv */
    Comm_t* create_worker_recv(utils::Header& head) override;
#ifdef YGG_TEST
    bool afterSendRecv(Comm_t* sComm, Comm_t* rComm) override;
#endif
#else
    void init() { UNINSTALLED_ERROR(ZMQ); }
#endif

#ifdef YGG_TEST
public:
    /*!
     * @brief Get the reply
     * @return The reply
     */
    ZMQReply& getReply() { return reply; }
#endif
  
private:
    friend class ClientComm;   //!< @see ClientComm
    friend class ServerComm;   //!< @see ServerComm

    ZMQReply reply;   //!< the most recent reply

};

}
}
