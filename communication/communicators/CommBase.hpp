#pragma once


#include "utils/enums.hpp"
#include "utils/Address.hpp"
#include "utils/logging.hpp"
//#include "datatypes/datatypes.hpp"
#include "datatypes/dtype_t.hpp"
#include "utils/logging.hpp"
//#include "datatypes/YggPly.hpp"
//#include "datatypes/YggObj.hpp"
#include "rapidjson/document.h"
#include "rapidjson/writer.h"

/*! @brief Bit flags. */
enum CommFlags {
    COMM_FLAG_VALID = 0x00000001,  //!< Set if the comm is initialized
    COMM_FLAG_GLOBAL = 0x00000002,  //!< Set if the comm is global
    COMM_FLAG_FILE  = 0x00000004,  //!< Set if the comm connects to a file
    COMM_FLAG_WORKER = 0x00000008,  //!< Set if the comm is a work comm
    COMM_FLAG_CLIENT = 0x00000010,  //!< Set if the comm is a client
    COMM_FLAG_SERVER = 0x00000020,  //!< Set if the comm is a server
    COMM_FLAG_CLIENT_RESPONSE= 0x00000040, //!< Set if the comm is a client response comm
    COMM_ALWAYS_SEND_HEADER = 0x00000080, //!< Set if the comm should always include a header in messages
    COMM_ALLOW_MULTIPLE_COMMS = 0x00000100 //!< Set if the comm should connect in a way that allow multiple connections
};

/*! @brief Bit flags that can be set for const comm */
enum CommConstFlags {
    COMM_FLAGS_USED = 0x00000001,  //!< Set if the comm has been used
    COMM_EOF_SENT   = 0x00000002,  //!< Set if EOF has been sent
    COMM_EOF_RECV   = 0x00000004  //!< Set if EOF has been received
};
/*! @brief Set if the comm is the receiving comm for a client/server request connection */
const int COMM_FLAG_RPC = COMM_FLAG_SERVER | COMM_FLAG_CLIENT;
#define COMM_NAME_SIZE 100
#define COMM_DIR_SIZE 100


namespace communication {

namespace communicator {
class ServerComm;

class ClientComm;

/**
 * Abstract base class for all communicators. Cannot be instantiated directly, but is used as a generalized hook
 * for passing communicators around. Should only be instantiated by the CommBase<> class.
 */
class Comm_t {
public:
    virtual ~Comm_t();

    /**
     * General send function for transmissing a message with a communicator. The function calls the specialized
     * (virtual) send functions of the specialized child classes.
     * @param dtype The data to be sent
     * @return int detailing the status
     */
    int send(const dtype_t* dtype);

    /**
     * General receiving function for getting a message with a communicator. The function calls the specialized
     * (virtual) recv functions of the specialized child classes.
     * @param dtype The data that were received
     * @return int detailing the status
     */
    long recv(dtype_t* dtype);

    /**
     * Returns the number of messages in the queue
     * @return The number of messages in the queue
     */
    virtual int comm_nmsg() const = 0;

    /**
     * Gets the type of communicator
     * @return The enumerated type
     */
    COMM_TYPE getType() const { return type; }

    /**
     * Returns the validity of the communicator (i.e. properly initialized with valid address)
     * @return bool
     */
    bool valid() {return flags & COMM_FLAG_VALID;}

#ifdef YGG_TEST
    std::string getName() {return name;}
#endif
protected:
    friend ServerComm;
    friend ClientComm;

    int send(const std::string message) {
        return send(message.c_str(), message.size());
    }

    /**
     * Virtual function for the specialized sending function for each communicator type.
     * @param data The data to send as a char*
     * @param len The length of data
     * @return The status
     */
    virtual int send(const char *data, const size_t &len) = 0;

    /**
     * Virtual function for the specialized receiving function for each communicator type.
     * @param data The data received as a char*
     * @param len The length of data
     * @return The status
     */
    virtual long recv(char *data, const size_t &len, bool allow_realloc) = 0;

    //Comm_t(const Comm_t* comm, COMM_TYPE type);
    /**
     * Constructor, which can only be instantiated by a child class
     * @param address The address to associate with this communicator.
     * @param direction Whether this instance is a sender or receiver
     * @param t Enumerated communicator type
     * @param flgs Initial bitwise flags
     * @see utils::Address()
     */
    Comm_t(utils::Address *address, DIRECTION direction, const COMM_TYPE &t, int flgs = 0);

    /**
     * Constructor, which can only be instantiated by a child class
     * @param name The name of communicator
     * @param direction Whether this instance is a sender or receiver
     * @param t Enumerated communicator type
     */
    explicit Comm_t(const std::string &name, DIRECTION direction = NONE, const COMM_TYPE &t = NULL_COMM);

    /**
     * Checks the size of the message to see if it exceeds the maximum allowable size as define by YGG_MSG_MAX
     * @param len The length of the message to check
     * @return bool Whether the message is smaller than YGG_MSG_MAX (true), false otherwise
     */
    bool check_size(const size_t &len) const;

    /**
     * Virtualized initialization function
     */
    virtual void init() = 0;

    /**
     * Virtualized reset function
     */
    virtual void reset() = 0;

    COMM_TYPE type; //!< Comm type.
    //void *other; //!< Pointer to additional information for the comm.
    std::string name; //!< Comm name.
    utils::Address *address; //!< Comm address.
    DIRECTION direction; //!< send or recv for direction messages will go.
    int flags; //!< Flags describing the status of the comm.
    int const_flags;  //!< Flags describing the status of the comm that can be est for const.
    ServerComm *info; //!< Pointer to any extra info comm requires.
    size_t maxMsgSize; //!< The maximum message size.
    size_t msgBufSize; //!< The size that should be reserved in messages.
    int index_in_register; //!< Index of the comm in the comm register.
    time_t *last_send; //!< Clock output at time of last send.
    int thread_id; //!< ID for the thread that created the comm.
};

/**
 * Creates a new communicator of the specified type
 * @param dir The direction for the communicator
 * @param type The enumerated type of communicator to create
 * @param name The name of the communicator
 * @param address The initial address of the communicator.
 * @return
 */
Comm_t* new_Comm_t(const DIRECTION dir, const COMM_TYPE type, const std::string &name="", char* address=nullptr);

/**
 * Templated base class for all communicators
 * @tparam H Handle type for the communicator
 * @tparam R Reply type for the communicator
 */
template<typename H, typename R>
class CommBase : public Comm_t {
public:
    CommBase() = delete;

    /**
     * Returns the number of messages in the queue
     * @return The number of messages in the queue
     */
    int comm_nmsg() const override {
        utils::ygglog_throw_error("Comm_nmsg of base class called, must be overridden");
        return -1;
    }
    using Comm_t::send;
    using Comm_t::recv;

protected:
    /**
     * Not used, must be overloaded by a child class
     */
    int send(const char *data, const size_t &len) override {
        utils::ygglog_throw_error("Send of base class called, must be overridden");
        return -1;
    }

    /**
     * Not used, must be overloaded by child class
     */
    long recv(char *data, const size_t &len, bool allow_realloc) override {
        utils::ygglog_throw_error("Recv of base class called, must be overridden");
        return -1;
    }

    /**
     * Constructor
     * @param address The initial address for the communicator
     * @param direction The enumerated direction of the communicator
     * @param t The enumerated type of the communicator
     */
    CommBase(utils::Address *address, DIRECTION direction, const COMM_TYPE &t);

    //CommBase(Comm_t* comm, const COMM_TYPE &type);
    /**
     * Constructor
     * @param name The name of the communicator
     * @param direction The enumerated direction of the communicator
     * @param t The enumerated type of the communicator
     */
    explicit CommBase(const std::string &name, DIRECTION direction = NONE, const COMM_TYPE &t = NULL_COMM);

    /**
     * Not used, must be overridden by child class
     */
    void init() override {
        utils::ygglog_throw_error("init of base class called, must be overridden");
    }

    /**
     * reset the communicator
     */
    void reset() override;

    /**
     * Destructor
     */
    ~CommBase() override;

    H *handle; //!< Pointer to handle for comm.
    R *reply; //!< Reply information.
};

template<typename H, typename R>
CommBase<H, R>::CommBase(utils::Address *address, DIRECTION direction, const COMM_TYPE &t) :
        Comm_t(address, direction, t, 0) {
    //other = NULL_ptr;
    handle = nullptr;
    reply = nullptr;
}

template<typename H, typename R>
CommBase<H, R>::CommBase(const std::string &name, DIRECTION direction, const COMM_TYPE &t) :
        Comm_t(name, direction, t) {
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
    ygglog_debug << "~CommBase: Started";
    if (handle != nullptr)
        delete handle;
    if (reply != nullptr)
        delete reply;
    ygglog_debug << "~CommBase: Finished";
}


}
}
