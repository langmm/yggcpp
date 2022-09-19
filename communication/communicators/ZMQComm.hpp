#pragma once

#include "CommBase.hpp"

#ifdef ZMQINSTALLED
#include <zmq.hpp>
#include <zmq_addon.hpp>
#else

#endif
#include <vector>
namespace communication {
namespace communicator {
class ClientComm;
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

class ygg_sock_t : public zmq::socket_t {
#else
    class ygg_sock_t {
#endif
public:
    explicit ygg_sock_t(int type);

#ifdef ZMQINSTALLED

    static zmq::context_t &get_context();

    static void shutdown();

#else

#endif /*ZMQINSTALLED*/

    ~ygg_sock_t();

#ifdef _OPENMP
    uint32_t tag;
    int type;
private:
    static zmq::context_t ygg_s_process_ctx;
    static bool ctx_valid;
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

class ZMQComm : public CommBase<ygg_sock_t, zmq_reply_t> {
public:
    explicit ZMQComm(const std::string &name = "", utils::Address *address = new utils::Address(), Direction direction = NONE,
                     datatypes::DataType *datatype = nullptr);

    //explicit ZMQComm(Comm_t* comm);
    ~ZMQComm();

    int send(const char *data, const size_t &len) override;

    long recv(char **data, const size_t &len, bool allow_realloc) override;

    //void open() override;
    //void close() override;
    int comm_nmsg() const override;

protected:
    int new_address();
    void init() override;

private:
    friend ClientComm;
    ygg_sock_t *sock;

    std::string set_reply_send();

    int find_reply_socket(utils::Address *address = nullptr);

    int check_reply_recv(const char *data, const size_t &len);

    static void check_reply_send(const char *data);

    int do_reply_recv(const int &isock, const char *msg);

    void init_reply();

    int set_reply_recv(utils::Address *adr);

    int do_reply_send();

    void init_zmq_reply();

    void destroy();

    //bool create_new();
    bool connect_to_existing();

    int recv_time_limit(zmq::multipart_t &msg);

    int send(zmq::multipart_t &msgs);
};

}
}