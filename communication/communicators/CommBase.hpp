#pragma once

#include "utils/tools.hpp"
#include "datatypes/datatypes.hpp"

//#define __cplusplus

/*! @brief Bit flags. */
#define COMM_FLAG_VALID   0x00000001  //!< Set if the comm is initialized
#define COMM_FLAG_GLOBAL  0x00000002  //!< Set if the comm is global
#define COMM_FLAG_FILE    0x00000004  //!< Set if the comm connects to a file
#define COMM_FLAG_WORKER  0x00000008  //!< Set if the comm is a work comm
#define COMM_FLAG_CLIENT  0x00000010  //!< Set if the comm is a client
#define COMM_FLAG_SERVER  0x00000020  //!< Set if the comm is a server
#define COMM_FLAG_CLIENT_RESPONSE 0x00000040 //!< Set if the comm is a client response comm
#define COMM_ALWAYS_SEND_HEADER   0x00000080 //!< Set if the comm should always include a header in messages
#define COMM_ALLOW_MULTIPLE_COMMS 0x00000100 //!< Set if the comm should connect in a way that allow multiple connections

/*! @brief Bit flags that can be set for const comm */
#define COMM_FLAGS_USED   0x00000001  //!< Set if the comm has been used
#define COMM_EOF_SENT     0x00000002  //!< Set if EOF has been sent
#define COMM_EOF_RECV     0x00000004  //!< Set if EOF has been received

/*! @brief Set if the comm is the receiving comm for a client/server request connection */
#define COMM_FLAG_RPC     COMM_FLAG_SERVER | COMM_FLAG_CLIENT
#define COMM_NAME_SIZE 100
#define COMM_DIR_SIZE 100

namespace communication {
namespace communicator {
class ServerComm;

class ClientComm;

/*! @brief Communicator types. */
enum comm_enum {
    NULL_COMM, IPC_COMM, ZMQ_COMM,
    SERVER_COMM, CLIENT_COMM,
    ASCII_FILE_COMM, ASCII_TABLE_COMM, ASCII_TABLE_ARRAY_COMM,
    MPI_COMM
};
enum Direction {
    SEND, NONE, RECV
};
typedef enum comm_enum comm_type;

class Comm_t {
public:
    ~Comm_t();

    virtual int send(const char *data, const size_t &len) = 0;

    virtual long recv(char **data, const size_t &len, bool allow_realloc) = 0;

    virtual int comm_nmsg() const = 0;

    comm_type getType() const { return type; }

    bool valid() const { return _valid; }

protected:
    friend ServerComm;
    friend ClientComm;

    //Comm_t(const Comm_t* comm, comm_type type);
    Comm_t(utils::Address *address, Direction direction, const comm_type &t, datatypes::DataType *datatype, int flgs = 0);

    explicit Comm_t(const std::string &name, Direction direction = NONE, const comm_type &t = NULL_COMM,
                    datatypes::DataType *datatype = nullptr);

    bool check_size(const size_t &len) const;

    virtual void init() = 0;

    virtual void reset() = 0;

    comm_type type; //!< Comm type.
    //void *other; //!< Pointer to additional information for the comm.
    std::string name; //!< Comm name.
    utils::Address *address; //!< Comm address.
    Direction direction; //!< send or recv for direction messages will go.
    int flags; //!< Flags describing the status of the comm.
    int const_flags;  //!< Flags describing the status of the comm that can be est for const.
    ServerComm *info; //!< Pointer to any extra info comm requires.
    datatypes::DataType *datatype; //!< Data type for comm messages.
    size_t maxMsgSize; //!< The maximum message size.
    size_t msgBufSize; //!< The size that should be reserved in messages.
    int index_in_register; //!< Index of the comm in the comm register.
    time_t *last_send; //!< Clock output at time of last send.
    int thread_id; //!< ID for the thread that created the comm.
    bool _valid;
};

/*!
      @brief Communication structure.
     */
template<typename H, typename R>
class CommBase : public Comm_t {
public:
    CommBase() = delete;

    virtual int send(const char *data, const size_t &len) = 0;

    virtual long recv(char **data, const size_t &len, bool allow_realloc) = 0;

    virtual int comm_nmsg() const = 0;

protected:
    CommBase(utils::Address *address, Direction direction, const comm_type &t, datatypes::DataType *datatype);

    //CommBase(Comm_t* comm, const comm_type &type);
    explicit CommBase(const std::string &name, Direction direction = NONE, const comm_type &t = NULL_COMM,
                      datatypes::DataType *datatype = nullptr);

    virtual void init() = 0;

    void reset() override;

    ~CommBase();

    H *handle; //!< Pointer to handle for comm.
    R *reply; //!< Reply information.
};

/*template<typename H, typename R>
CommBase<H, R>::CommBase(Comm_t *comm, const comm_type &type) : Comm_t(comm, type) {
    if (comm->getType() == type) {
        const auto *temp = reinterpret_cast<CommBase<H,R>* const>(comm);
        *handle = *(temp->handle);
        *reply = *(temp->reply);
    } else {
        handle = nullptr;
        reply = nullptr;
    }
}*/

template<typename H, typename R>
CommBase<H, R>::CommBase(utils::Address *address, Direction direction, const comm_type &t, datatypes::DataType *datatype) :
        Comm_t(address, direction, t, datatype, 0) {
    //other = NULL_ptr;
    handle = nullptr;
    reply = nullptr;
}

template<typename H, typename R>
CommBase<H, R>::CommBase(const std::string &name, Direction direction, const comm_type &t,
                         datatypes::DataType *datatype) :
        Comm_t(name, direction, t, datatype) {
}

template<typename H, typename R>
void CommBase<H, R>::reset() {
    if (handle != nullptr)
        delete handle;
    if (reply != nullptr)
        delete reply;
}

template<typename H, typename R>
CommBase<H, R>::~CommBase() {
    utils::ygglog_debug("~CommBase: Started");
    if (handle != nullptr)
        delete handle;
    if (reply != nullptr)
        delete reply;
    utils::ygglog_debug("~CommBase: Finished");
}

}

}
