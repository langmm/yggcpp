#pragma once

#include "CommBase.hpp"

#ifdef ZMQINSTALLED
#include <zmq.hpp>
#include <zmq_addon.hpp>
#else

#endif
#include <vector>
namespace communication {
#ifdef YGG_TEST
namespace testing {
class ygg_sock_tester;
class ZMQComm_tester;
}
#endif
namespace communicator {
//class ClientComm;
static unsigned _zmq_rand_seeded = 0;
static unsigned _last_port_set = 0;
static int _last_port = 49152;
/* static double _wait_send_t = 0;  // 0.0001; */
static char _reply_msg[100] = "YGG_REPLY";
static char _purge_msg[100] = "YGG_PURGE";
static int _zmq_sleeptime = 10000;
#ifdef _OPENMP
#pragma omp threadprivate(_reply_msg, _purge_msg, _zmq_sleeptime)
#endif

#ifdef ZMQINSTALLED

/**
 * Wrapper for zeroMQ socket
 */
class ygg_sock_t : public zmq::socket_t {
#else
    class ygg_sock_t {
#endif
public:
    explicit ygg_sock_t(int type);

#ifdef ZMQINSTALLED

    static zmq::context_t &get_context();

    static void ctx_shutdown();

#else

#endif /*ZMQINSTALLED*/
    void close();
    ~ygg_sock_t();

#ifdef _OPENMP
    uint32_t tag;
    int type;
private:
#ifdef YGG_TEST
    friend class testing::ygg_sock_tester;
#endif
    static zmq::context_t ygg_s_process_ctx;
    static bool ctx_valid;
    static std::vector<ygg_sock_t*> activeSockets;
#endif
};

/*void ygg_zsock_destroy(zsock_t **self_p) {
    // Recreation of czmq zsock_destroy that is OMP aware
    if (*self_p) {
        auto *self = (ygg_zsock_t*)(*self_p);
        self->tag = 0xDeadBeef;
        zmq_close (self->handle);
        freen (self->endpoint);
        freen (self->cache);
        freen (self);
        *self_p = nullptr;
    }
};
#else
#define ygg_zsock_destroy zsock_destroy
#endif
*/

/*!
  @brief Struct to store info for reply.
*/
typedef struct zmq_reply_t {
    ~zmq_reply_t() {
        clear();
    }

    size_t nsockets() const {
        return sockets.size();
    }

    void clear() {
        for (auto it: sockets) {
            if (it != nullptr)
                delete it;
        }
        for (auto it: addresses) {
            if (it != nullptr)
                delete it;
        }
        sockets.clear();
        addresses.clear();
        n_msg = 0;
        n_rep = 0;
    }

    std::vector<ygg_sock_t *> sockets;
    std::vector<utils::Address *> addresses;
    int n_msg = 0;
    int n_rep = 0;
} zmq_reply_t;

/**
 * Class utilising zeroMQ for communication
 */
class ZMQComm : public CommBase<ygg_sock_t, zmq_reply_t> {
public:
    /**
     * Constructor
     * @param name The name of the communicator
     * @param address The address to associate with the communicator, if address is nullptr
     *                then an address will be created.
     * @param direction Enumerated direction for communicator
     */
    explicit ZMQComm(const std::string name = "", utils::Address *address = new utils::Address(),
                     DIRECTION direction = NONE);

    //explicit ZMQComm(Comm_t* comm);
    /**
     * Destructor
     */
    ~ZMQComm() override;


    //void open() override;
    //void close() override;
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
    /**
     * Create a new address for the communciator
     * @return Success/failure
     */
    virtual bool new_address();

    /**
     * Initialize the communicator
     */
    void init() override;

    /**
     * Sending function
     * @param data The message to send
     * @param len The length of data
     * @return THe status
     */
    int send(const char *data, const size_t &len) override;

    /**
     * Receiving function
     * @param data The contents of the message withh be placed here
     * @param len The initial length of data
     * @param allow_realloc Whether data can be reallocated if it is too small to hold the message.
     * @return The length of data after the message was copied.
     */
    long recv(char *data, const size_t &len, bool allow_realloc) override;

private:
    friend class ClientComm;
    friend class ServerComm;
#ifdef YGG_TEST
    friend class testing::ZMQComm_tester;
#endif

    ygg_sock_t *sock;        //!< socket to use

    /**
     * Add reply socket information to a send comm.
     * @return Reply socket address as a string
     */
    std::string set_reply_send();

    /**
     * Locate matching reply socket.
     * @param address Address that should be matched against.
     * @return int Index of matched socket, -1 if no match, -2 if error.
     */
    int find_reply_socket(utils::Address *address = nullptr);

    /**
     * Get reply information from message.
     * @param data char* Received message containing reply info that will be
     *                 removed on return.
     * @param len size_t Length of received message.
     * @return int Length of message without the reply info. -1 if there is an error.
     */
    int check_reply_recv(const char *data, const size_t &len);

    /**
     * Add information about reply socket to outgoing message.
     * @param data char* Message that reply info should be added to.
     */
    static void check_reply_send(const char *data);

    /**
     * Send confirmation to sending socket.
     * @param isock int Index of socket that reply should be done for.
     * @param msg char* Mesage to send/recv.
     * @return int 0 if successfule, -1 otherwise.
     */
    int do_reply_recv(const int &isock, const char *msg);

    void init_reply();

    /**
     * Add reply socket information to a recv comm.
     * @param adr Address that confirmation is for.
     * @returns int Index of the reply socket.
     */
    int set_reply_recv(utils::Address *adr);

    /**
     * Request confirmation from receiving socket.
     * @returns int 0 if successful, -2 on EOF, -1 otherwise.
     */
    int do_reply_send();

    /**
     * Add empty reply structure information to comm.
     */
    void init_zmq_reply();

    void destroy();

    //bool create_new();
    bool connect_to_existing();

    int recv_time_limit(zmq::multipart_t &msg);

    int send(zmq::multipart_t &msgs);
};

}
}