#pragma once


#include "utils/tools.hpp"
#include "utils/enums.hpp"
#include "utils/Address.hpp"
#include "utils/logging.hpp"
#include "utils/logging.hpp"
#include "utils/serialization.hpp"
#include "rapidjson/document.h"
#include "rapidjson/writer.h"

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
#define COMM_FLAGS_USED   0x00000200  //!< Set if the comm has been used
#define COMM_EOF_SENT     0x00000400  //!< Set if EOF has been sent
#define COMM_EOF_RECV     0x00000800  //!< Set if EOF has been received

/*! @brief Set if the comm is the receiving comm for a client/server request connection */
#define COMM_FLAG_RPC     COMM_FLAG_SERVER | COMM_FLAG_CLIENT
#define COMM_NAME_SIZE 100
#define COMM_DIR_SIZE 100


namespace communication {

namespace communicator {
class ServerComm;

class ClientComm;

class ZMQComm;

class IPCComm;

class RequestList;

typedef struct comm_t comm_t;

class Comm_t {
private:
  Comm_t(const Comm_t& other) = delete;
  Comm_t& operator=(const Comm_t&) = delete;
public:
    virtual ~Comm_t();

    /*!
      @brief Send a string message through the communicator.
      @param[in] data Message.
      @returns int Values >= 0 indicate success.
     */
    int send(const std::string data) {
        return send(data.c_str(), data.size());
    }
    /*!
      @brief Receive a string message from the communicator.
      @param[out] data String to store message in.
      @returns int -1 if message could not be received. Length of the
        received message if message was received.
     */
    long recv(std::string& data) {
      char* str = NULL;
      size_t len = 0;
      long out = recv(str, len, true);
      if (out >= 0)
	data.assign(str, len);
      return out;
    }
    /*!
      @brief Send a string message through the communicator.
      @param[in] data Message.
      @param[in] len Size of data in bytes.
      @returns int Values >= 0 indicate success.
     */
    virtual int send(const char *data, const size_t &len);
    /*!
      @brief Receive a string message from the communicator.
      @param[out] data Allocated buffer where the message should be saved.
      @param[in] len Length of the allocated message buffer in bytes.
      @param[in] allow_realloc If true, data will be reallocated if the
        message is larger than len. If false, an error will be raised if
	the message is larger than len.
      @returns int -1 if message could not be received. Length of the
        received message if message was received.
    */
    virtual long recv(char*& data, const size_t &len, bool allow_realloc);

    /*!
      @brief Send an object through the communicator.
      @tparam T1 Type of object being sent.
      @param[in] data T1 object.
      @return Integer specifying if the receive was succesful. Values >= 0
        indicate success.
    */
    template<typename T1>
    int sendVar(const T1 data) {
      ygglog_debug << "CommBase(" << name << ")::send(const T1& data)" << std::endl;
      return send(1, data);
    }
    /*!
      @brief Receive an object from the communicator.
      @tparam T1 Type of object being received.
      @param[out] data T1 bject to receive message into.
      @return Integer specifying if the receive was succesful. Values >= 0
        indicate success.
    */
    template<typename T1>
    long recvVar(T1& data) {
      ygglog_debug << "CommBase(" << name << ")::recv(T1& data)" << std::endl;
      return recv(1, &data);
    }
  
    /*!
      @brief Send a rapidjson document through the communicator.
      @param[in] data Message.
      @returns int Values >= 0 indicate success.
     */
    // template<>
    int sendVar(const rapidjson::Document& data) {
      return send(1, &data);
    }
    /*!
      @brief Send a Ply object through the communicator.
      @param[in] data Ply object.
      @return Integer specifying if the receive was succesful. Values >= 0
        indicate success.
    */
    // template<>
    int sendVar(const rapidjson::Ply& data) {
      return send(1, &data);
    }
    /*!
      @brief Send a ObjWavefront object through the communicator.
      @param[in] data ObjWavefront object.
      @return Integer specifying if the receive was succesful. Values >= 0
      indicate success.
    */
    // template<>
    int sendVar(const rapidjson::ObjWavefront& data) {
      return send(1, &data);
    }

    /*!
      @brief Receive and parse a message into the provided arguments.
      @param[in] nargs Number of arguments being passed.
      @param[out] ... mixed arguments that should be assigned parameters
        extracted from the incoming message. Since these will be assigned,
	they should be pointers to memory that has already been allocated.
      @return Integer specifying if the receive was succesful. Values >= 0
        indicate success.
    */
    long recv(const int nargs, ...);
    /*!
      @brief Receive and parse a message into the provided arguments,
        reallocating the memory pointed to by those arguments as necessary.
      @param[in] nargs Number of arguments being passed.
      @param[out] ... mixed arguments that should be assigned parameters
        extracted from the incoming message. Since these will be assigned,
	and possibly reallocated, they should be pointers to memory that
	can be reallocated.
      @return Integer specifying if the receive was succesful. Values >= 0
        indicate success.
    */
    long recvRealloc(const int nargs, ...);
    /*!
      @brief Send a message created by serializing the provided arugments.
      @param[in] nargs Number of arguments being passed.
      @param[in] ... mixed arguments that should be serialized into a
        message based on the datatype associated with this communicator.
      @returns 0 if send succesfull, -1 if send unsuccessful.
    */
    int send(const int nargs, ...);

    /*!
      @brief Receive a message into a list of variable arguments.
      @param[in,out] ap Variable argument list that message will be
          received into.
      @return Integer specifying if the receive was succesful.
        Values >= 0 indicate success.
     */
    long vRecv(rapidjson::VarArgList& ap);
    /*!
      @brief Send a message containing a list of variable arguments.
      @param[in] ap Variable argument list that message will be
          constructed from.
      @return Integer specifying if the send was succesful.
        Values >= 0 indicate success.
     */
    int vSend(rapidjson::VarArgList& ap);
  
    /*!
      @brief Get the number of messages in the communicator.
      @return Number of messages.
     */
    virtual int comm_nmsg() const = 0;

    /*!
      @brief Get the type code for the communicator.
      @returns Type code.
     */
    COMM_TYPE getType() const { return type; }
    /*!
      @brief Determine if the communicator is valid.
      @return true if it is valid, false otherwise.
     */
    bool valid() {return flags & COMM_FLAG_VALID;}

#ifdef YGG_TEST
    std::string getName() {return name;}
    std::string getAddress() {return address->address();}
    DIRECTION getDirection() {return direction;}
    virtual bool afterSendRecv(Comm_t*, Comm_t*) { return true; }
#endif
    void addSchema(const Metadata& s);
    void addSchema(const rapidjson::Value& s, bool isMetadata = false);
    void addSchema(const std::string schemaStr);
    void addFormat(const std::string format_str, bool as_array = false);

private:
    int deserialize(const char* buf, rapidjson::VarArgList& ap);
    int serialize(char*& buf, size_t& buf_siz,
		  rapidjson::VarArgList& ap);

protected:
    friend ServerComm;
    friend ClientComm;
    friend IPCComm;
    friend ZMQComm;
    friend RequestList;

    long copyData(char*& dst, const size_t dst_len,
		  const char* src, const size_t src_len,
		  bool allow_realloc) {
      if ((src_len + 1) > dst_len) {
	if (!allow_realloc) {
	  ygglog_error << "CommBase(" << name << ")::copyData: Size of message (" << src_len << " + 1 bytes) exceeds buffer size (" << dst_len << " bytes) and the buffer cannot be reallocated." << std::endl;
	  return -((long)src_len);
	}
	dst = (char*)realloc(dst, src_len + 1);
	if (dst == nullptr) {
	  ygglog_error << "CommBase(" << name << ")::copyData: Error reallocating buffer" << std::endl;
	  return -1;
	}
      }
      if (src) {
	memcpy(dst, src, src_len);
	dst[src_len] = '\0';
      }
      return (long)src_len;
    }

    virtual int update_datatype(const rapidjson::Value& new_schema,
				const DIRECTION dir=NONE);
    virtual bool create_header_send(Header& header, const char* data, const size_t &len);
    virtual bool create_header_recv(Header& header, char*& data, const size_t &len,
				    size_t msg_len, int allow_realloc,
				    int temp);
    virtual Comm_t* create_worker(utils::Address* address, const DIRECTION);
    // Comm_t* create_worker_wrap(utils::Address* address, const DIRECTION);
    virtual Comm_t* create_worker_send(Header& head);
    virtual Comm_t* create_worker_recv(Header& head);
    virtual int send_single(const char *data, const size_t &len) = 0;
    virtual long recv_single(char*& data, const size_t &len, bool allow_realloc) = 0;

    //Comm_t(const Comm_t* comm, COMM_TYPE type);
    Comm_t(utils::Address *address, DIRECTION direction, const COMM_TYPE &t, int flgs = 0);

    explicit Comm_t(const std::string &name, DIRECTION direction = NONE, const COMM_TYPE &t = NULL_COMM, int flgs = 0);

    bool check_size(const size_t &len) const;

    virtual void init() = 0;

    virtual void reset() = 0;

    COMM_TYPE type; //!< Comm type.
    //void *other; //!< Pointer to additional information for the comm.
    std::string name; //!< Comm name.
    utils::Address *address; //!< Comm address.
    DIRECTION direction; //!< send or recv for direction messages will go.
    int flags; //!< Flags describing the status of the comm.
    size_t maxMsgSize; //!< The maximum message size.
    size_t msgBufSize; //!< The size that should be reserved in messages.
    int index_in_register; //!< Index of the comm in the comm register.
    int thread_id; //!< ID for the thread that created the comm.
    Metadata metadata;

    static std::vector<Comm_t*> registry;
    static void register_comm(Comm_t* x);
};

Comm_t* new_Comm_t(const DIRECTION dir, const COMM_TYPE type, const std::string &name="", char* address=nullptr);
/*!
      @brief Communication structure.
     */
template<typename H>
class CommBase : public Comm_t {
private:
  CommBase(const CommBase& other) = delete;
  CommBase& operator=(const CommBase&) = delete;
public:
    CommBase() = delete;

    int comm_nmsg() const override {
        utils::ygglog_throw_error("Comm_nmsg of base class called, must be overridden");
        return -1;
    }
    using Comm_t::send;
    using Comm_t::recv;

protected:
    int send_single(const char *, const size_t &) override {
        utils::ygglog_throw_error("Send of base class called, must be overridden");
        return -1;
    }

    long recv_single(char*&, const size_t &, bool) override {
        utils::ygglog_throw_error("Recv of base class called, must be overridden");
        return -1;
    }

    CommBase(utils::Address *address, DIRECTION direction, const COMM_TYPE &t, int flags = 0);

    explicit CommBase(const std::string &name, DIRECTION direction = NONE, const COMM_TYPE &t = NULL_COMM, int flags = 0);

    void init() override {
        utils::ygglog_throw_error("init of base class called, must be overridden");
    }

    void reset() override;

    ~CommBase() override;

    H *handle; //!< Pointer to handle for comm.
};

template<typename H>
CommBase<H>::CommBase(utils::Address *address, DIRECTION direction, const COMM_TYPE &t, int flags) :
  Comm_t(address, direction, t, flags), handle(nullptr) {}

template<typename H>
CommBase<H>::CommBase(const std::string &name, DIRECTION direction, const COMM_TYPE &t, int flags) :
  Comm_t(name, direction, t, flags), handle(nullptr) {}

template<typename H>
void CommBase<H>::reset() {
    if (handle != nullptr)
        delete handle;
}

template<typename H>
CommBase<H>::~CommBase() {
    ygglog_debug << "~CommBase: Started" << std::endl;
    if (handle != nullptr)
        delete handle;
    ygglog_debug << "~CommBase: Finished" << std::endl;
}


}
}
