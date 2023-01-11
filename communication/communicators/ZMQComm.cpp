#include "ZMQComm.hpp"

#ifdef ZMQINSTALLED
using namespace communication::communicator;
using namespace communication::utils;
//using namespace communication::datatypes;
#include <zmq.hpp>
#include <zmq_addon.hpp>
#include <boost/algorithm/string.hpp>
//#include "datatypes/datatypes.hpp"
#include "utils/tools.hpp"
#include "utils/logging.hpp"
#include "CommHead.hpp"

const std::chrono::milliseconds timeout{1000};
const std::chrono::milliseconds short_timeout{10};
#ifdef _OPENMP
zmq::context_t ygg_sock_t::ygg_s_process_ctx = zmq::context_t();
bool ygg_sock_t::ctx_valid = true;

void ygg_sock_t::shutdown() {
#pragma omp critical (zmq)
    {
        if (ctx_valid) {
            ygg_s_process_ctx.shutdown();
            ygg_s_process_ctx.close();
            ctx_valid = false;
        }
    }
}

zmq::context_t &ygg_sock_t::get_context() {
    if (!ctx_valid) {
        ygg_s_process_ctx = zmq::context_t();
        ctx_valid = true;
    }
    return ygg_s_process_ctx;
}

// TODO??  #pragma omp critical (zmq)

ygg_sock_t::ygg_sock_t(int type) : zmq::socket_t(get_context(), type), tag(0xcafe0004), type(type) {
#else // _OPENMP
    zmq::context_t &ygg_sock_t::get_context() {
        zqm::context ctx;
        return ctx;
    }

    void ygg_sock_t::shutdown() {

    }
    ygg_sock_t::ygg_sock_t(int type) : zmq::socket_t(get_context(), type) {
#endif // _OPENMP
    set(zmq::sockopt::linger, 0);
    set(zmq::sockopt::immediate, 1);
}

//#ifdef _OPENMP
//}
//#endif

ygg_sock_t::~ygg_sock_t() {
#ifdef _OPENMP
    // Recreation of czmq zsock_destroy that is OMP aware
    tag = 0xDeadBeef;
#else
#endif
}

void ZMQComm::init() {
    sock = nullptr;
    if (!(flags & COMM_FLAG_VALID))
        return;
    if (address == nullptr || !address->valid()) {
        if (new_address()) {
            flags |= COMM_FLAG_VALID;
        } else {
            flags &= ~COMM_FLAG_VALID;
        }
    } else {
        if (connect_to_existing()) {
            flags |= COMM_FLAG_VALID;
        } else {
            flags &= ~COMM_FLAG_VALID;
        }
    }
}

/*!
  @brief Initialize a ZeroMQ communication.
  @param[in] comm comm_t * Comm structure initialized with init_comm_base.
  @returns int -1 if the comm could not be initialized.
 */
ZMQComm::ZMQComm(const std::string &name, Address *address, const DIRECTION direction) :
        CommBase(address, direction, ZMQ_COMM) {
    init();
}

/*ZMQComm::ZMQComm(communication::Comm_t *comm) : CommBase(comm, ZMQ_COMM) {
    if (comm->getType() == ZMQ_COMM)

}*/

void ZMQComm::init_reply() {
    if (reply != nullptr)
        delete reply;
    reply = new zmq_reply_t;
    reply->n_msg = 0;
    reply->n_rep = 0;
}

/*!
  @brief Add empty reply structure information to comm.
 */

void ZMQComm::init_zmq_reply() {
    if (reply == nullptr)
        reply = new zmq_reply_t();
    else {
        reply->clear();
    }
}

/*!
  @brief Locate matching reply socket.
  @param[in] comm comm_t* Comm that should be checked for matching reply socket.
  @param[in] address char* Address that should be matched against.
  @returns int Index of matched socket, -1 if no match, -2 if error.
 */
int ZMQComm::find_reply_socket(Address *address) {
    Address* adr;
    if (address == nullptr)
        adr = this->address;
    else
        adr = address;
    // Get reply
    if (reply == nullptr) {
        ygglog_error << "find_reply_socket(" << name << "): Reply structure not initialized.";
        return -2;
    }
    for (int i = 0; i < reply->nsockets(); i++) {
        if (reply->addresses[i] == adr) {
            return i;
        }
    }
    return -1;
}

/*!
  @brief Request confirmation from receiving socket.
  @param[in] comm comm_t* Comm structure to do reply for.
  @returns int 0 if successful, -2 on EOF, -1 otherwise.
 */

int ZMQComm::do_reply_send() {
    // Get reply
    if (reply == nullptr) {
        ygglog_error << "do_reply_send(" << name << ") : Reply structure not initialized.";
        return -1;
    }
    reply->n_msg++;
    if (reply->nsockets() == 0 || reply->sockets[0] == nullptr) {
        ygglog_error << "do_reply_send(" << name << ") : Socket is nullptr.";
        return -1;
    }
    sock = reply->sockets[0];
    // Poll
    ygglog_debug << "do_reply_send(" << name << ") : address=" << reply->addresses[0]->address()
                 << ", begin";

//#if defined(__cplusplus) && defined(_WIN32)
    // TODO: There seems to be an error in the poller when using it in C++
//#else
    zmq::poller_t<> in_poller;

    in_poller.add(*sock, zmq::event_flags::pollin);
    std::vector<zmq::poller_event<>> in_events(1);

    ygglog_debug << "do_reply_send(" << name << ") : waiting on poller...";
    size_t nin = 0;
    size_t nout = 0;

    while (nin == 0) {
        nin = in_poller.wait_all(in_events, timeout);
    }
//#endif
    // Receive
    zmq::message_t zmsg;
    auto rres = in_events[0].socket.recv(zmsg);
    std::string msg_data = zmsg.to_string();

    // Check for EOF
    bool is_purge = false;
    if (msg_data == YGG_MSG_EOF) {
        ygglog_debug << "do_reply_send(" << name << ") : EOF received";
        reply->n_msg = 0;
        reply->n_rep = 0;
        return -2;
    } else if (msg_data == _purge_msg) {
        is_purge = true;
    }
    // Send
    // zsock_set_linger(s, _zmq_sleeptime);
    zmq::poller_t<> out_poller;
    out_poller.add(*sock, zmq::event_flags::pollout);
    std::vector<zmq::poller_event<>> out_events(1);
    while (nout == 0) {
        nout = out_poller.wait_all(out_events, timeout);
    }
    int ret = -1;
    auto sres = out_events[0].socket.send(zmsg, zmq::send_flags::none);
    // Check for purge or EOF
    if (!sres.has_value()) {
        ygglog_error << "do_reply_send(" << name << ") : Error sending reply frame.";
    } else {
        if (is_purge) {
            ygglog_debug << "do_reply_send(" << name << ") : PURGE received";
            reply->n_msg = 0;
            reply->n_rep = 0;
            ret = do_reply_send();
        } else {
            reply->n_rep++;
        }
    }
    ygglog_debug << "do_reply_send(" << name << ") : address=" << reply->addresses[0]->address()
                 << ", end";
//#if defined(__cplusplus) && defined(_WIN32)
    // TODO: There seems to be an error in the poller when using it in C++
//#else
    if (sres.has_value()) {
        ygglog_debug << "do_reply_send(" << name << ") : waiting on poller...";
        nout = out_poller.wait_all(out_events, short_timeout);
        ygglog_debug << "do_reply_send(" << name << ") : poller returned";
        if (is_purge)
            return ret;
        return static_cast<int>(sres.value());
    }
//#endif
    return ret;
}

/*!
  @brief Send confirmation to sending socket.
  @param[in] comm comm_t* Comm structure to do reply for.
  @param[in] isock int Index of socket that reply should be done for.
  @param[in] msg char* Mesage to send/recv.
  @returns int 0 if successfule, -1 otherwise.
 */
int ZMQComm::do_reply_recv(const int &isock, const char* msg) {
    // Get reply
    if (reply->sockets.size() <= isock) {
        ygglog_error << "do_reply_recv(" << name << ") : Socket does not exist";
    } else if (reply->sockets.at(isock) == nullptr) {
        ygglog_error << "do_reply_recv(" << name << ") : Socket is nullptr.";
        return -1;
    }
    sock = reply->sockets[isock];
    ygglog_debug << "do_reply_recv(" << name << ") : address=" << reply->addresses[isock]->address()
                 << ", begin";
    zmq::message_t msg_send(msg);

    // Send
    auto ret = sock->send(msg_send, zmq::send_flags::none);
    if (!ret.has_value()) {
        ygglog_error << "do_reply_recv(" << name << ") : Error sending confirmation.";
        return -1;
    }
    if (strcmp(msg, YGG_MSG_EOF) == 0) {
        ygglog_info << "do_reply_recv(" << name << ") : EOF confirmation.";
        reply->n_msg = 0;
        reply->n_rep = 0;
        sock->set(zmq::sockopt::linger, _zmq_sleeptime);
        return -2;
    }
    // Poll to prevent block
    ygglog_debug << "do_reply_recv(" << name << ") : address=" << reply->addresses[isock]->address()
                 << ", polling for reply";
//#if defined(__cplusplus) && defined(_WIN32)
    // TODO: There seems to be an error in the poller when using it in C++
//#else
    zmq::poller_t<> poller;
    poller.add(*sock, zmq::event_flags::pollin);
    std::vector<zmq::poller_event<>> events(1);
    ygglog_debug << "do_reply_recv(" << name << ") : waiting on poller...";

    size_t nin = poller.wait_all(events, timeout);

    ygglog_debug << "do_reply_recv(" << name << ") : poller returned";
    if (nin == 0) {
        ygglog_error << "do_reply_recv(" << name << ") : Poller failed";
        return -1;
    }
//#endif
    // Receive
    zmq::message_t msg_recv;
    ret = sock->recv(msg_recv);
    if (msg_recv.empty()) {
        ygglog_error << "do_reply_recv(" << name << ") : did not receive";
        return -1;
    }
    reply->n_rep++;
    ygglog_debug << "do_reply_recv(" << name << ") : address=" << reply->addresses[isock]->address() << ", end";
    return 0;
}

/*!
  @brief Add reply socket information to a send comm.
  @param[in] comm comm_t* Comm that confirmation is for.
  @returns char* Reply socket address.
*/
std::string ZMQComm::set_reply_send() {
    std::string out;

    if (reply == nullptr) {
        ygglog_error << "set_reply_send(" << name << ") : Reply structure not initialized.";
        return out;
    }
    // Create socket
    if (reply->nsockets() == 0) {
        reply->sockets.push_back(new ygg_sock_t(ZMQ_REP));
        if (reply->sockets[0] == nullptr) {
            ygglog_error << "set_reply_send(" << name << ") : Could not initialize empty socket.";
            return out;
        }
        const std::string protocol = "tcp";
        std::string host = "localhost";
        if (host == "localhost")
            host = "127.0.0.1";
        std::string address;
#ifdef _OPENMP
#pragma omp critical (zmqport)
        {
#endif
            if (_last_port_set == 0) {
                ygglog_debug << "model_index = " << getenv("YGG_MODEL_INDEX");
                _last_port = 49152 + 1000 * static_cast<int>(strtol(getenv("YGG_MODEL_INDEX"), nullptr, 0));
                _last_port_set = 1;
                ygglog_debug << "_last_port = " << _last_port;
            }
            address = protocol + "://" +  host + ":*[" + std::to_string(_last_port + 1) + "]";
            try {
                reply->sockets[0]->bind(address);
            } catch(zmq::error_t &err) {
                ygglog_error << "set_reply_send(" << name << ") : Could not bind socket to address = "
                             << address << " : " << err.what();
            }
            address = reply->sockets[0]->get(zmq::sockopt::last_endpoint);
            std::vector<std::string> parts;
            boost::split(parts, address, boost::is_any_of(":"));
            _last_port = std::stoi(parts.back());
#ifdef _OPENMP
        }
#endif
        //sprintf(address, "%s://%s:%d", protocol, host, port);
        auto *adr = new Address(address);

        reply->addresses.push_back(adr);
        ygglog_debug << "set_reply_send(" << name << ") : New reply socket: " << address;
    }
    return reply->addresses[0]->address();
}

/*!
  @brief Add reply socket information to a recv comm.
  @param[in] comm comm_t* Comm that confirmation is for.
  @returns int Index of the reply socket.
*/
int ZMQComm::set_reply_recv(Address* adr) {
    int out = -1;
    // Get reply
    if (reply == nullptr) {
        ygglog_error << "set_reply_recv(" << name << ") : Reply structure not initialized.";
        return out;
    }
    // Match address and create if it doesn't exist
    int isock = find_reply_socket(adr);
    if (isock < 0) {
        if (isock == -2) {
            ygglog_error << "set_reply_recv(" << name << ") : Error locating socket.";
            return out;
        }
        // Create new socket
        isock = static_cast<int>(reply->nsockets());
        reply->sockets.push_back(new ygg_sock_t(ZMQ_REQ));
        if (reply->sockets[isock] == nullptr) {
            ygglog_error << "set_reply_recv(" << name << ") : Could not initialize empty socket.";
            return out;
        }
        reply->addresses.push_back(adr);
        try {
            reply->sockets[0]->connect(adr->address());
        } catch(zmq::error_t &err) {
            ygglog_error << "set_reply_recv(" << name << ") : Could not connect to socket. "
                         << err.what();
            return out;
        }
        ygglog_debug << "set_reply_recv(" << name << ") : New recv socket: " <<  adr->address();
    }
    return isock;
}

/*!
  @brief Add information about reply socket to outgoing message.
  @param[in] comm comm_t* Comm that confirmation is for.
  @param[in] data char* Message that reply info should be added to.
  @param[in] len int Length of the outgoing message.
  @returns char* Message with reply information added.
 */
void ZMQComm::check_reply_send(const char* data) {
    //return data;
    return;
}


/*!
  @brief Get reply information from message.
  @param[in] comm comm_* Comm structure for incoming message.
  @param[in, out] data char* Received message containing reply info that will be
  removed on return.
  @param[in] len size_t Length of received message.
  @returns int Length of message without the reply info. -1 if there is an error.
 */
int ZMQComm::check_reply_recv(const char* data, const size_t &len) {
    int new_len = (int)len;
    int ret;
    // Get reply
    if (reply == nullptr) {
        ygglog_error << "check_reply_recv(" << name << ") : Reply structure not initialized.";
        return -1;
    }
    reply->n_msg++;
    // Extract address
    datatypes::CommHead head(data, len);
    if (!(head.flags & HEAD_FLAG_VALID)) {
        ygglog_error << "check_reply_recv(" << name << ") : Invalid header.";
        return -1;
    }
    Address *adr;
    if ((flags & COMM_FLAG_WORKER) && (reply->nsockets() == 1)) {
        adr = reply->addresses[0];
    } else if (head.zmq_reply != nullptr) {
        adr = new Address(*head.zmq_reply);
    } else {
        ygglog_error << "check_reply_recv(" << name << ") : Error parsing reply header in '" << data << "'";
        return -1;
    }

    // Match address and create if it dosn't exist
    int isock = set_reply_recv(address);
    if (isock < 0) {
        ygglog_error << "check_reply_recv(" << name << ") : Error setting reply socket.";
        return -1;
    }
    // Confirm message receipt
    ret = do_reply_recv(isock, _reply_msg);
    if (ret < 0) {
        ygglog_error << "check_reply_recv(" << name << ") : Error during reply.";
        return -1;
    }
    return new_len;
}

/*!
  @brief Create a new socket.
  @param[in] comm comm_t * Comm structure initialized with new_comm_base.
  @returns int -1 if the address could not be created.
*/
bool ZMQComm::new_address() {
    // TODO: Get protocol/host from input
    std::string protocol = "tcp";
    std::string host = "localhost";
    auto *adr = new Address();
    msgBufSize = 100;
    if (host == "localhost")
        host = "127.0.0.1";
    if (protocol == "inproc" || protocol == "ipc") {
        // TODO: small chance of reusing same number
        int key = 0;
#ifdef _OPENMP
#pragma omp critical (zmqport)
        {
#endif
            if (!(_zmq_rand_seeded)) {
                srand(ptr2seed(this));
                _zmq_rand_seeded = 1;
            }
#ifdef _OPENMP
        }
#endif
        while (key == 0) key = rand();
        if (name.empty())
            name = "tempnewZMQ-" + std::to_string(key);
        adr->address(protocol + "://" + name);
    } else {
#ifdef _OPENMP
#pragma omp critical (zmqport)
        {
#endif
            if (_last_port_set == 0) {
                ygglog_debug << "model_index = %s" << getenv("YGG_MODEL_INDEX");
                _last_port = 49152 + 1000 * atoi(getenv("YGG_MODEL_INDEX"));
                _last_port_set = 1;
                ygglog_debug << "_last_port = " << _last_port;
            }
            adr->address( protocol + "://" + host + ":" +std::to_string(_last_port + 1));
#ifdef _OPENMP
        }
#endif
        /* strcat(address, ":!"); // For random port */
    }
    // Bind
    if (handle != nullptr) {
        delete handle;
        handle = nullptr;
    }

    if (flags & COMM_FLAG_CLIENT_RESPONSE) {
        handle = new ygg_sock_t(ZMQ_ROUTER);
    } else if (flags & COMM_ALLOW_MULTIPLE_COMMS) {
        handle = new ygg_sock_t(ZMQ_DEALER);
    } else {
        handle = new ygg_sock_t(ZMQ_PAIR);
    }
    if (handle == nullptr) {
        ygglog_error << "create_new: Could not initialize empty socket.";
        return false;
    }
    //void* libzmq_socket = handle.handle();
    try {
        handle->bind(adr->address().c_str());
    } catch (zmq::error_t &err) {
        ygglog_error << "create_new: Could not bind socket to address = " << adr->address() << " : "
                     << err.what();
        return false;
    }
    // Add port to address
    int port = -1;
#ifdef _OPENMP
#pragma omp critical (zmqport)
    {
#endif
        std::string addr = handle->get(zmq::sockopt::last_endpoint);
        std::vector<std::string> parts;
        boost::split(parts, addr, boost::is_any_of(":"));
        port = std::stoi(parts.back());
        adr->address(addr);

        if (protocol != "inproc" && protocol != "ipc") {
            _last_port = port;
            //adr->address(protocol + "://" +  host + ":" + std::to_string(port));
        }
#ifdef _OPENMP
    }
#endif
    if (address != nullptr)
        delete address;
    address = adr;
    ygglog_debug << "create_new: Bound socket to " << address->address();
    if (name.empty())
        name = "tempnewZMQ-" + std::to_string(port);

    // Init reply
    init_zmq_reply();
    return true;
}


ZMQComm::~ZMQComm() {
    destroy();
}

void ZMQComm::destroy() {
    // Drain input
    if (direction == RECV && flags & COMM_FLAG_VALID
        && (!(const_flags & COMM_EOF_RECV))) {
        if (utils::_ygg_error_flag == 0) {
            size_t data_len = 100;
            char *data = (char*)malloc(data_len);
            datatypes::CommHead head;
            bool is_eof_flag = false;
            while (comm_nmsg() > 0) {
                if (long ret = recv(data, data_len, true) >= 0) {
                    head = datatypes::CommHead(data, ret);
                    if (strncmp(YGG_MSG_EOF, data + head.bodybeg, strlen(YGG_MSG_EOF)) == 0)
                        is_eof_flag = true;

                    if ((head.flags & HEAD_FLAG_VALID) && is_eof_flag) {
                        const_flags |= COMM_EOF_RECV;
                        break;
                    }
                }
            }
            free(data);
        }
    }
    // Free reply
    if (reply != nullptr) {
        delete reply;
        reply = nullptr;
    }
    if (handle != nullptr) {
        delete handle;
        ygglog_debug << "Destroying socket: " << address->address();
        handle = nullptr;
    }
    ygglog_debug << "free_zmq_comm: finished";

    //TODO: THERE IS MORE TO DELETE?
}

/*!
  @brief Get number of messages in the comm.
  @returns int Number of messages. -1 indicates an error.
 */
int ZMQComm::comm_nmsg() const {
    int out = 0;
    if (direction == RECV) {
        if (handle != nullptr) {
            zmq::poller_t<> poller;
            poller.add(*handle, zmq::event_flags::pollin);
            std::vector<zmq::poller_event<>> events(1);
            return static_cast<int>(poller.wait_all(events, short_timeout));
        }
    } else {
        /* if (x->last_send[0] != 0) { */
        /*   time_t now; */
        /*   time(&now); */
        /*   double elapsed = difftime(now, x->last_send[0]); */
        /*   if (elapsed > _wait_send_t) */
        /* 	out = 0; */
        /*   else */
        /* 	out = 1; */
        /* } */
        if (reply != nullptr) {
            ygglog_debug << "zmq_comm_nmsg(" << name << ") : nmsg = " << reply->n_msg << ", nrep = "
                         << reply->n_rep;
            out = reply->n_msg - reply->n_rep;
        }
    }
    return out;
}

/*!
  @brief Send a message to the comm.
  Send a message smaller than YGG_MSG_MAX bytes to an output comm. If the
  message is larger, it will not be sent.
  @param[in] data character pointer to message that should be sent.
  @param[in] len size_t length of message to be sent.
  @returns int 0 if send succesfull, -1 if send unsuccessful.
 */
int ZMQComm::send(const char* data, const size_t &len) {
    int ret;
    size_t msgsiz;
    size_t prev = 0;
    while (prev < len) {
        if ((len - prev) > YGG_MSG_MAX)
            msgsiz = YGG_MSG_MAX;
        else
            msgsiz = len - prev;
        zmq::message_t msg(data + prev, msgsiz);

        prev += msgsiz;
        auto res = handle->send(msg, ((prev < len) ? zmq::send_flags::sndmore : zmq::send_flags::none));

        if (!res.has_value()) {
            ygglog_error << "zmq_comm_send(" << name << ") : Error in zframe_send";
            break;
        }
    }
    // Reply
    ret = do_reply_send();
    if (ret < 0) {
        if (ret == -2) {
            ygglog_error << "zmq_comm_send(" << name << ") : EOF received";
        } else {
            ygglog_error << "zmq_comm_send(" << name << ") : Error in do_reply_send";
        }
    }


    ygglog_debug << "zmq_comm_send(" << name << ") : returning " << ret;
    return ret;
}


/*zframe_t* ZMQComm::recv_zframe() {
    ygglog_debug << "zmq_comm_recv_zframe(" << name << ") ";
    if (handle == nullptr) {
        ygglog_error << "zmq_comm_recv_zframe(" << name << ") : socket handle is nullptr";
        return nullptr;
    }
    clock_t start = clock();
    while ((((double)(clock() - start))/CLOCKS_PER_SEC) < 180) {
        int nmsg = comm_nmsg();
        if (nmsg < 0)
            return nullptr;
        else if (nmsg > 0)
            break;
        else {
            ygglog_debug << "zmq_comm_recv_zframe(" << name << ") : no messages, sleep %d",
                         YGG_SLEEP_TIME);
            usleep(YGG_SLEEP_TIME);
        }
    }
    ygglog_debug << "zmq_comm_recv_zframe(" << name << ") : receiving";
    zframe_t *out = nullptr;
    if (flags & COMM_FLAG_CLIENT_RESPONSE) {
        out = zframe_recv(handle);
        if (out == nullptr) {
            ygglog_debug << "zmq_comm_recv_zframe(" << name << ") : did not receive identity";
            return nullptr;
        }
        zframe_destroy(&out);
        out = nullptr;
    }
    out = zframe_recv(handle);
    if (out == nullptr) {
        ygglog_debug << "zmq_comm_recv_zframe(" << name << ") : did not receive";
        return nullptr;
    }
    return out;
}*/

int ZMQComm::recv_time_limit(zmq::multipart_t &msgs) {
    clock_t start = clock();
    while ((((double)(clock() - start))/CLOCKS_PER_SEC) < 180) {
        const int nmsg = comm_nmsg();
        if (nmsg < 0) {
            ygglog_debug << "zmq::recv(" << name << ") : did not receive";
            return -1;
        } else if (nmsg > 0) {
            break;
        } else {
            ygglog_debug << "zmq_comm_recv_zframe(" << name << ") : no messages, sleep "
                         << YGG_SLEEP_TIME;
            usleep(YGG_SLEEP_TIME);
        }
    }

    ygglog_debug << "zmq_comm_recv_zframe(" << name << ") : receiving";

    if (flags & COMM_FLAG_CLIENT_RESPONSE) {
        zmq::message_t msg;
        auto resp = handle->recv(msg, zmq::recv_flags::none);
        if (!resp.has_value()) {
            ygglog_debug << "zmq_comm_recv_zframe(" << name << ") : did not receive identity";
            return -1;
        }
    }
    auto resp = zmq::recv_multipart(*handle, std::back_inserter(msgs));
    if (!resp.has_value()) {
        ygglog_debug << "zmq_comm_recv_zframe(" << name << ") : did not receive";
        return -1;
    }

    return 0;
}
/*!
  @brief Receive a message from an input comm.
  Receive a message smaller than YGG_MSG_MAX bytes from an input comm.
  @param[in] x comm_t* structure that message should be sent to.
  @param[out] data char ** pointer to allocated buffer where the message
  should be saved. This should be a malloc'd buffer if allow_realloc is 1.
  @param[in] len const size_t length of the allocated message buffer in bytes.
  @param[in] allow_realloc const int If 1, the buffer will be realloced if it
  is not large enought. Otherwise an error will be returned.
  @returns int -1 if message could not be received. Length of the received
  message if message was received.
 */
long ZMQComm::recv(char* data, const size_t &len, bool allow_realloc) {
    long ret = -1;
    zmq::multipart_t msgs;
    ygglog_debug << "zmq_comm_recv(" << name << ") ";
    if (handle == nullptr) {
        ygglog_error << "zmq_comm_recv(" << name << ") : socket handle is nullptr";
        return ret;
    }

    if ((ret = recv_time_limit(msgs)) < 0)
        return ret;

    // Check for server signon and respond
    while (true) {
        char* temp = msgs[0].data<char>();
        if (strncmp(temp, "ZMQ_SERVER_SIGNING_ON::", 23) == 0) {
            ygglog_debug << "zmq_comm_recv(" << name << ") : Received sign-on";
            std::string client_address = temp+23;

            // create a DEALER socket and connect to address
            ygg_sock_t* client_socket;
            try {
                client_socket = new ygg_sock_t(ZMQ_DEALER);
            } catch (...) {
                ygglog_error << "zmq_comm_recv(" << name
                             << ") : Could not initalize the client side of the proxy socket to confirm signon";
                return ret;
            }

            client_socket->set(zmq::sockopt::sndtimeo, _zmq_sleeptime);
            client_socket->set(zmq::sockopt::immediate, 1);
            client_socket->set(zmq::sockopt::linger, _zmq_sleeptime);
            client_socket->connect(client_address);
            zmq::send_result_t resp;
            if (msgs.size() == 1) {
                zmq::message_t response(std::move(msgs[0]));
                resp = client_socket->send(response, zmq::send_flags::none);
            } else {
                zmq::multipart_t tmp(std::move(msgs));
                resp = zmq::send_multipart(*client_socket, tmp);
            }
            if (!resp.has_value()) {
                ygglog_error << "zmq_comm_recv(" << name << ") : Error sending response message.";
                delete client_socket;
                return ret;
            }
            delete client_socket;

            if ((ret = recv_time_limit(msgs)) < 0) {
                ygglog_debug << "zmq_comm_recv(" << name << ") : did not receive";
                return ret;
            }
        } else {
            break;
        }
    }

    size_t len_recv = 0;
    for (auto& msg : msgs) {
        len_recv += msg.size();
    }
    len_recv += 1;
    // Realloc and copy data
    if (len_recv > len) {
        if (allow_realloc) {
            ygglog_debug << "zmq_comm_recv(" << name << ") : reallocating buffer from " << len << " to "
                         << len_recv << " bytes.";
            data = (char*)realloc(data, len_recv);
            if (data == nullptr) {
                ygglog_error << "zmq_comm_recv(" << name << ") : failed to realloc buffer.";
                return -1;
            }
        } else {
            ygglog_error << "zmq_comm_recv(" << name << ") : buffer (" << len
                         << " bytes) is not large enough for message (" << len_recv << " bytes)";
            return -((int)(len_recv - 1));
        }
    }
    strcpy(data, "");
    for (auto& m: msgs) {
        strncat(data, m.data<char>(), m.size());
    }

    data[len_recv-1] = '\0';
    ret = (int)len_recv - 1;
    /*
    if (strlen(*data) != ret) {
      ygglog_error << "zmq_comm_recv(" << name << ") : Size of string (%d) doesn't match expected (%d)",
           name.c_str(), strlen(*data), ret);
      return -1;
    }
    */
    // Check reply
    ret = check_reply_recv(data, ret);
    if (ret < 0) {
        ygglog_error << "zmq_comm_recv(" << name << ") : failed to check for reply socket.";
        return ret;
    }
    ygglog_debug << "zmq_comm_recv(" << name << ") : returning " << ret;
    return ret;
}

bool ZMQComm::connect_to_existing() {
    msgBufSize = 100;
    if (flags & (COMM_FLAG_SERVER | COMM_ALLOW_MULTIPLE_COMMS)) {
        handle = new ygg_sock_t(ZMQ_DEALER);
    } else {
        handle = new ygg_sock_t(ZMQ_PAIR);
    }
    if (handle == nullptr) {
        ygglog_error << "init_zmq_address: Could not initialize empty socket.";
        flags &= ~COMM_FLAG_VALID;
        return false;
    }
    try {
        handle->connect(address->address());
    } catch (zmq::error_t &err) {
        ygglog_error << "init_zmq_address: Could not connect socket to address = "
                     << this->address->address() << " : " << err.what();
        delete handle;

        flags &= ~COMM_FLAG_VALID;
        return false;
    }
    ygglog_debug << "init_zmq_address: Connected socket to " << this->address->address();
    if (this->name.empty()) {
        if (name.empty()) {
            this->name = "tempinitZMQ-" + this->address->address();
        }
        else {
            this->name = name;
        }
    }
    // Asign to void pointer
    init_reply();
    flags |= COMM_ALWAYS_SEND_HEADER;
    return true;
}
// Definitions in the case where ZMQ libraries not installed
#else /*ZMQINSTALLED*/

/*!
  @brief Print error message about ZMQ library not being installed.
 */
static inline
void ygg_zsys_shutdown() {
  ygglog_error << "Compiler flag 'ZMQINSTALLED' not defined so ZMQ bindings are disabled.");
};

/*!
  @brief Print error message about ZMQ library not being installed.
 */
static inline
void* ygg_zsys_init() {
  ygglog_error << "Compiler flag 'ZMQINSTALLED' not defined so ZMQ bindings are disabled.");
  return nullptr;
};

/*!
  @brief Print error message about ZMQ library not being installed.
 */
static inline
void zmq_install_error() {
  ygglog_error << "Compiler flag 'ZMQINSTALLED' not defined so ZMQ bindings are disabled.");
};

/*!
  @brief Perform deallocation for ZMQ communication.
  @param[in] x comm_t Pointer to communication to deallocate.
  @returns int 1 if there is and error, 0 otherwise.
*/
static inline
int free_zmq_comm(comm_t *x) {
  zmq_install_error();
  return 1;
};

/*!
  @brief Create a new socket.
  @param[in] comm comm_t * Comm structure initialized with new_comm_base.
  @returns int -1 if the address could not be created.
*/
static inline
int new_zmq_address(comm_t *comm) {
  zmq_install_error();
  return -1;
};

/*!
  @brief Initialize a ZeroMQ communication.
  @param[in] comm comm_t * Comm structure initialized with init_comm_base.
  @returns int -1 if the comm could not be initialized.
 */
static inline
int init_zmq_comm(comm_t *comm) {
  zmq_install_error();
  return -1;
};

/*!
  @brief Get number of messages in the comm.
  @param[in] x comm_t* Communicator to check.
  @returns int Number of messages. -1 indicates an error.
 */
static inline
int zmq_comm_nmsg(const comm_t* x) {
  zmq_install_error();
  return -1;
};

/*!
  @brief Send a message to the comm.
  Send a message smaller than YGG_MSG_MAX bytes to an output comm. If the
  message is larger, it will not be sent.
  @param[in] x comm_t* structure that comm should be sent to.
  @param[in] data character pointer to message that should be sent.
  @param[in] len size_t length of message to be sent.
  @returns int 0 if send succesfull, -1 if send unsuccessful.
 */
static inline
int zmq_comm_send(const comm_t* x, const char *data, const size_t len) {
  zmq_install_error();
  return -1;
};

/*!
  @brief Receive a message from an input comm.
  Receive a message smaller than YGG_MSG_MAX bytes from an input comm.
  @param[in] x comm_t* structure that message should be sent to.
  @param[out] data char ** pointer to allocated buffer where the message
  should be saved. This should be a malloc'd buffer if allow_realloc is 1.
  @param[in] len const size_t length of the allocated message buffer in bytes.
  @param[in] allow_realloc const int If 1, the buffer will be realloced if it
  is not large enought. Otherwise an error will be returned.
  @returns int -1 if message could not be received. Length of the received
  message if message was received.
 */
static inline
int zmq_comm_recv(const comm_t* x, char **data, const size_t len,
		  const int allow_realloc) {
  zmq_install_error();
  return -1;
};

/*!
  @brief Add reply socket information to a send comm.
  @param[in] comm comm_t* Comm that confirmation is for.
  @returns char* Reply socket address.
*/
static inline
char *set_reply_send(const comm_t *comm) {
  zmq_install_error();
  return nullptr;
};

/*!
  @brief Add reply socket information to a recv comm.
  @param[in] comm comm_t* Comm that confirmation is for.
  @param[in] address const char* Comm address.
  @returns int Index of the reply socket.
*/
static inline
int set_reply_recv(const comm_t *comm, const char* address) {
  zmq_install_error();
  return -1;
};

#endif /*ZMQINSTALLED*/