#pragma once


#include "utils/tools.hpp"
#include "utils/enums.hpp"
#include "utils/Address.hpp"
#include "utils/logging.hpp"
#include "utils/serialization.hpp"
#include "Workers.hpp"
#include "rapidjson/document.h"
#include "rapidjson/writer.h"
#include "rapidjson/schema.h"

/*! @brief Bit flags. */
#define COMM_FLAG_VALID   0x00000001  //!< Set if the comm is initialized
#define COMM_FLAG_GLOBAL  0x00000002  //!< Set if the comm is global
#define COMM_FLAG_FILE    0x00000004  //!< Set if the comm connects to a file
#define COMM_FLAG_WORKER  0x00000008  //!< Set if the comm is a work comm
#define COMM_FLAG_CLIENT  0x00000010  //!< Set if the comm is a client
#define COMM_FLAG_SERVER  0x00000020  //!< Set if the comm is a server
#define COMM_FLAG_CLIENT_RESPONSE 0x00000040 //!< Set if the comm is a client response comm
#define COMM_FLAG_SERVER_RESPONSE 0x00000080 //!< Set if the comm is a server response comm
#define COMM_ALWAYS_SEND_HEADER   0x00000100 //!< Set if the comm should always include a header in messages
#define COMM_ALLOW_MULTIPLE_COMMS 0x00000200 //!< Set if the comm should connect in a way that allow multiple connections
#define COMM_FLAGS_USED   0x00000400  //!< Set if the comm has been used
#define COMM_EOF_SENT     0x00000800  //!< Set if EOF has been sent
#define COMM_EOF_RECV     0x00001000  //!< Set if EOF has been received
#define COMM_FLAG_INTERFACE 0x00002000 //!< Set if communicator is an interface communicator

/*! @brief Set if the comm is the receiving comm for a client/server request connection */
#define COMM_FLAG_RPC     COMM_FLAG_SERVER | COMM_FLAG_CLIENT
#define COMM_NAME_SIZE 100
#define COMM_DIR_SIZE 100

// Bug in gnu std::regex that dosn't allow for matching large messages
// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=86164
#ifdef __GNUC__
#define COMM_BASE_MAX_MSG_SIZE 2048
#else
#define COMM_BASE_MAX_MSG_SIZE
#endif

#define WORKER_METHOD_DECS(cls)					\
  Comm_t* create_worker(utils::Address* address,		\
			const DIRECTION, int flgs) override
#define WORKER_METHOD_DEFS(cls)					\
  Comm_t* cls::create_worker(utils::Address* address,		\
			     const DIRECTION dir, int flgs) {	\
    return new cls("", address, dir, flgs | COMM_FLAG_WORKER);	\
  }
#define WORKER_METHOD_DUMMY(cls, abbr)				\
  Comm_t* cls::create_worker(utils::Address*,			\
			     const DIRECTION, int) {		\
    abbr ## _install_error();					\
    return NULL;						\
  }

#define ADD_CONSTRUCTORS(cls)			\
  explicit cls(const std::string nme,		\
	       const DIRECTION dirn,		\
	       int flgs = 0) :			\
    cls(nme, nullptr, dirn, flgs) {}		\
  explicit cls(utils::Address *addr,		\
	       const DIRECTION dirn,		\
	       int flgs = 0) :			\
    cls("", addr, dirn, flgs) {}
#define ADD_CONSTRUCTORS_RPC(cls)		\
  explicit cls(const std::string nme,		\
	       int flgs = 0) :			\
    cls(nme, nullptr, flgs) {}			\
  explicit cls(utils::Address *addr,		\
	       int flgs = 0) :			\
    cls("", addr, flgs) {}


using namespace rapidjson;

namespace communication {

namespace communicator {

#if defined(_MSC_VER) && defined(_OPENMP)
extern __declspec(thread) int global_scope_comm;
#else // _MSC_VER
extern int global_scope_comm;
#ifdef _OPENMP
#pragma omp threadprivate(global_scope_comm)
#endif
#endif // _MSC_VER

void global_scope_comm_on();
void global_scope_comm_off();

class RPCComm;

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
      @brief Send a message indicating that the communicator is closing.
      @returns int Values >= 0 indicate success.
     */
     int send_eof() {
       return send(YGG_MSG_EOF);
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
      if (out >= 0 || out == -2) {
	if (out >= 0)
	  data.assign(str, static_cast<size_t>(out));
	free(str);
      }
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
      @brief Set the time limit for receiving messages.
      @param[in] new_timeout New time limit in micro-seconds. -1 will
        cause receive calls to block until a message is available.
     */
    virtual void set_timeout_recv(int new_timeout);
    /*!
      @brief Wait until a message is available to be received or a time
        limit is reached.
      @param[in] tout Time (in micro-seconds) that should be waited. If -1
        the process will wait forever.
      @return Number of messages available for receive. -1 if an error
        occurred.
     */
    virtual int wait_for_recv(const int tout);
    /*!
      @brief Receive a string message from the communicator.
      @param[out] data Allocated buffer where the message should be saved.
      @returns int -1 if message could not be received. Length of the
        received message if message was received.
    */
    template<size_t N>
    long recv(char(& data)[N]) {
      size_t len = N;
      char* ptr = &(data[0]);
      return recv(ptr, len, false);
    }
  
    /*!
      @brief Receive a raw string message from the communicator.
      @param[out] data Allocated buffer where the message should be saved.
      @param[in] len Length of the allocated message buffer in bytes.
      @param[in] allow_realloc If true, data will be reallocated if the
        message is larger than len. If false, an error will be raised if
	the message is larger than len.
      @returns int -1 if message could not be received. Length of the
        received message if message was received.
    */
    virtual long recv(char*& data, const size_t &len,
		      bool allow_realloc=false);

    /*!
      @brief Send an object through the communicator.
      @tparam T Type of object being sent.
      @param[in] data Object to send.
      @return Integer specifying if the receive was succesful. Values >= 0
        indicate success.
    */
    template<typename T>
    int sendVar(const T data) {
      ygglog_debug << "CommBase(" << name << ")::sendVar(const T& data)" << std::endl;
      if (!checkType(data, SEND))
	return -1;
      return send(1, data);
    }
    /*!
      @brief Send a C++ string through the communicator.
      @param[in] data Message.
      @returns int Values >= 0 indicate success.
     */
    int sendVar(const std::string& data);
    /*!
      @brief Send a rapidjson document through the communicator.
      @param[in] data Message.
      @returns int Values >= 0 indicate success.
     */
    int sendVar(const rapidjson::Document& data);
    /*!
      @brief Send a Ply object through the communicator.
      @param[in] data Ply object.
      @return Integer specifying if the receive was succesful. Values >= 0
        indicate success.
    */
    int sendVar(const rapidjson::Ply& data);
    /*!
      @brief Send a ObjWavefront object through the communicator.
      @param[in] data ObjWavefront object.
      @return Integer specifying if the receive was succesful.
        Values >= 0 indicate success.
    */
    int sendVar(const rapidjson::ObjWavefront& data);

    /*!
      @brief Receive an object from the communicator.
      @tparam T Type of object being received.
      @param[out] data Object to receive message into.
      @return Integer specifying if the receive was succesful.
        Values >= 0 indicate success.
    */
    template<typename T>
    long recvVar(T& data) {
      ygglog_debug << "CommBase(" << name << ")::recvVar(T& data)" << std::endl;
      if (!checkType(data, RECV))
	return -1;
      return recv(1, &data);
    }
    /*!
      @brief Receive a rapidjson::Document object from the communicator.
      @param[out] data Object to receive message into.
      @return Integer specifying if the receive was succesful.
        Values >= 0 indicate success.
    */
    long recvVar(rapidjson::Document& data) {
      ygglog_debug << "CommBase(" << name << ")::recvVar(rapidjson::Document& data)" << std::endl;
      if ((!data.IsNull()) && (!checkType(data, RECV)))
	return -1;
      return recv(1, &data);
    }
    /*!
      @brief Receive a string object from the communicator.
      @param[out] data Object to receive message into.
      @return Integer specifying if the receive was succesful. Values >= 0
        indicate success.
    */
    long recvVar(std::string& data) {
      ygglog_debug << "CommBase(" << name << ")::recvVar(std::string& data)" << std::endl;
      if (!checkType(data, RECV))
	return -1;
      char* str = NULL;
      size_t len = 0;
      long out = recvRealloc(2, &str, &len);
      if (out >= 0 || out == -2) {
	if (out >= 0)
	  data.assign(str, static_cast<size_t>(len));
	free(str);
      }
      return out;
    }
  
    // TODO: Versions of sendVar/recvVar with multiples?

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
      @brief Send a request and receive a response from the provided
        arguments containing data for both the request and response,
	reallocating variables for the response as necessary.
      @param[in] nargs Number of arguments being passed.
      @param[in] ... mixed arguments that request will be constructed
        from and response will be received into based on the datatypes
	associated with the communicator. Arguments that will be
	populated with data from the response should be pointers to
	memory addresses that can be reallocated.
      @return Integer specifying if the receive was succesful.
        Values >= 0 indicate success.
    */
    long call(const int nargs, ...);
    /*!
      @brief Send a request and receive a response from the provided
        arguments containing data for both the request and response.
      @param[in] nargs Number of arguments being passed.
      @param[in] ... mixed arguments that request will be constructed
        from and response will be received into based on the datatypes
	associated with the communicator. Arguments that will be
	populated with data from the response should be pointers to
	memory that has already been allocated.
      @return Integer specifying if the receive was succesful.
        Values >= 0 indicate success.
    */
    long callRealloc(const int nargs, ...);
  
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
      @brief Send a request and receive a response from a list of
        variable arguments containing data for both the request and
        response.
      @param[in] ap Variable argument list that request will be
        constructed from and response will be receieved into.
      @return Integer specifying if the receive was succesful.
        Values >= 0 indicate success.
    */
    virtual long vCall(rapidjson::VarArgList& ap);
      
    /*!
      @brief Get the number of messages in the communicator.
      @return Number of messages.
     */
    virtual int comm_nmsg() const = 0;

    /*!
      @brief Close the communicator.
     */
    virtual void close() = 0;

    /*!
      @brief Check if the communicator is closed.
      @return true if the communicator is closed, false otherwise.
     */
    virtual bool is_closed() const = 0;

    /*!
      @brief Check if the communicator is open.
      @return true if the communicator is open, false otherwise.
     */
    virtual bool is_open() const {
      return (!is_closed());
    }

    /*!
      @brief Get the type code for the communicator.
      @returns Type code.
     */
    COMM_TYPE getType() const { return type; }
    /*!
      @brief Determine if the communicator is valid.
      @return true if it is valid, false otherwise.
     */
    bool valid() { return flags & COMM_FLAG_VALID; }
    /*!
      @brief Determine if the communicator is global.
      @return true if it is global, false otherwise.
     */
    bool global() { return flags & COMM_FLAG_GLOBAL; }

#ifdef YGG_TEST
    std::string getName() { return name; }
    std::string getAddress() {
      if (address)
	return address->address();
      return "";
    }
    DIRECTION getDirection() { return direction; }
    WorkerList& getWorkers() { return workers; }
    Metadata& getMetadata() { return metadata; }
    int& getFlags() { return flags; }
    virtual bool afterSendRecv(Comm_t*, Comm_t*) { return true; }
    size_t getMaxMsgSize() { return maxMsgSize; }
#endif
    void addSchema(const Metadata& s);
    void addSchema(const rapidjson::Value& s, bool isMetadata = false);
    void addSchema(const std::string schemaStr, bool isMetadata = false);
    void addFormat(const std::string format_str, bool as_array = false);
    void copySchema(const Comm_t* other);

    static void _ygg_cleanup();

private:
    int deserialize(const char* buf, rapidjson::VarArgList& ap);
    int serialize(char*& buf, size_t& buf_siz,
		  rapidjson::VarArgList& ap);

protected:
    friend RPCComm;
    friend ServerComm;
    friend ClientComm;
    friend IPCComm;
    friend ZMQComm;
    friend RequestList;
    friend Worker;
    friend WorkerList;

    static int _ygg_initialized;
    static int _ygg_finalized;
    static void _ygg_init();
  
    void updateMaxMsgSize(size_t new_size) {
      if (global_comm) {
	global_comm->updateMaxMsgSize(new_size);
	return;
      }
      if (maxMsgSize == 0 || new_size < maxMsgSize)
	maxMsgSize = new_size;
    }

    void setFlags(const Header& head, DIRECTION dir) {
      if (global_comm) {
	global_comm->setFlags(head, dir);
	return;
      }
      flags |= COMM_FLAGS_USED;
      if (head.flags & HEAD_FLAG_EOF) {
	if (dir == SEND)
	  flags |= COMM_EOF_SENT;
	else
	  flags |= COMM_EOF_RECV;
      }
    }

    static utils::Address* addressFromEnv(const std::string& name,
					  DIRECTION direction) {
      utils::Address* out = new utils::Address();
      if (name.empty())
	return out;
      std::string full_name;
      full_name = name;
      if (full_name.size() > COMM_NAME_SIZE)
	full_name.resize(COMM_NAME_SIZE);
      if (direction != NONE) {
	if (direction == SEND) {
	  full_name += "_OUT";
	} else if (direction == RECV) {
	  full_name += "_IN";
	}
      }
      char *model_name = getenv("YGG_MODEL_NAME");
      char *addr = std::getenv(full_name.c_str());
      if ((!addr) && model_name) {
	std::string prefix(model_name);
	prefix += ":";
	if (prefix.size() > COMM_NAME_SIZE)
	  prefix.resize(COMM_NAME_SIZE);
	if (full_name.rfind(prefix, 0) != 0) {
	  prefix += full_name;
	  full_name = prefix;
	  addr = std::getenv(full_name.c_str());
	}
      }
      if (!addr) {
	std::string temp_name(full_name);
	size_t loc;
	while ((loc = temp_name.find(":")) != std::string::npos) {
	  temp_name.replace(loc, 1, "__COLON__");
	}
	addr = getenv(temp_name.c_str());
      }
      std::string model_name_str = "null";
      std::string addr_str = "null";
      if (model_name)
	model_name_str.assign(model_name);
      if (addr)
	addr_str.assign(addr_str);
      ygglog_debug << "CommBase::addressFromEnv: model_name = " <<
	model_name_str << ", address = " << addr_str << std::endl;
      ygglog_debug << std::endl;
      if (addr)
	out->address(addr);
      return out;
    }

    long copyData(char*& dst, const size_t dst_len,
		  const char* src, const size_t src_len,
		  bool allow_realloc) {
      if ((src_len + 1) > dst_len) {
	if (!allow_realloc) {
	  ygglog_error << "CommBase(" << name << ")::copyData: Size of message (" << src_len << " + 1 bytes) exceeds buffer size (" << dst_len << " bytes) and the buffer cannot be reallocated." << std::endl;
	  return -((long)src_len);
	}
	char* tmp = (char*)realloc(dst, src_len + 1);
	if (tmp == NULL) {
	  ygglog_error << "CommBase(" << name << ")::copyData: Error reallocating buffer" << std::endl;
	  return -1;
	}
	dst = tmp;
      }
      if (src) {
	memcpy(dst, src, src_len);
	dst[src_len] = '\0';
      }
      return (long)src_len;
    }

    virtual Metadata& get_metadata(const DIRECTION dir=NONE);
    int update_datatype(const rapidjson::Value& new_schema,
			const DIRECTION dir);
    template<typename T>
    void zeroData(const T* data,
		  RAPIDJSON_ENABLEIF((internal::OrExpr<YGGDRASIL_IS_ANY_SCALAR(T), internal::IsSame<T, bool> >))) {
      memset(const_cast<T*>(data), 0, sizeof(T));
    }
    template<typename T>
    void zeroData(const T*,
		  RAPIDJSON_DISABLEIF((internal::OrExpr<YGGDRASIL_IS_ANY_SCALAR(T), internal::IsSame<T, bool> >))) {}
    template<typename T>
    bool checkType(const T& data, const DIRECTION dir) {
      Metadata& meta = get_metadata(dir);
      if (dir == RECV)
	zeroData(&data);
      try {
	meta.fromData(data);
      } catch (...) {
	return false;
      }
      return true;
    }
    rapidjson::Value& getSchema(const DIRECTION dir=NONE) {
      return get_metadata(dir).getSchema();
    }
    virtual bool create_header_send(Header& header, const char* data, const size_t &len);
    virtual bool create_header_recv(Header& header, char*& data, const size_t &len,
				    size_t msg_len, int allow_realloc,
				    int temp);
    virtual Comm_t* create_worker(utils::Address* address,
				  const DIRECTION, int flgs) = 0;
    virtual Comm_t* create_worker_send(Header& head);
    virtual Comm_t* create_worker_recv(Header& head);
    virtual int send_single(const char *data, const size_t &len, const Header& header) = 0;
    virtual long recv_single(char*& data, const size_t &len, bool allow_realloc) = 0;

    explicit Comm_t(const std::string &name,
		    utils::Address *address = nullptr,
		    DIRECTION direction = NONE,
		    const COMM_TYPE &t = NULL_COMM, int flgs = 0);

    bool check_size(const size_t &len) const;

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
    int timeout_recv; //!< Time to wait for messages during receive.
    WorkerList workers; //!< Communicator to use for sending large messages.
    Comm_t* global_comm; // !< Pointer to global comm that this comm shadows.

    static std::vector<Comm_t*> registry;
    bool get_global_scope_comm();
public:
    static void register_comm(Comm_t* x);
    static Comm_t* find_registered_comm(const std::string& name,
					const DIRECTION dir,
					const COMM_TYPE type);
};

  Comm_t* new_Comm_t(const DIRECTION dir, const COMM_TYPE type, const std::string &name="", char* address=nullptr, int flags=0);
  Comm_t* new_Comm_t(const DIRECTION dir, const COMM_TYPE type, const std::string &name, utils::Address* address, int flags=0);
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
    void close() override;
    bool is_closed() const override;
    using Comm_t::send;
    using Comm_t::recv;

protected:
    int send_single(const char *, const size_t &, const Header&) override {
        utils::ygglog_throw_error("Send of base class called, must be overridden");
        return -1;
    }

    long recv_single(char*&, const size_t &, bool) override {
        utils::ygglog_throw_error("Recv of base class called, must be overridden");
        return -1;
    }

    explicit CommBase(const std::string &name, utils::Address *address = nullptr, DIRECTION direction = NONE, const COMM_TYPE &t = NULL_COMM, int flags = 0);

    Comm_t* create_worker(utils::Address*, const DIRECTION,
			  int) override {
      utils::ygglog_throw_error("create_worker of base class called, must be overridden");
      return NULL;
    }

    void reset() override;

    ~CommBase() override;

    H *handle; //!< Pointer to handle for comm.
};

template<typename H>
CommBase<H>::CommBase(const std::string &nme, utils::Address *addr,
		      DIRECTION dirn, const COMM_TYPE &t, int flgs) :
  Comm_t(nme, addr, dirn, t, flgs), handle(nullptr) {
  if (global_comm)
    handle = dynamic_cast<CommBase<H>*>(global_comm)->handle;
}

template<typename H>
void CommBase<H>::close() {
  if (handle) {
    if (!global_comm)
      delete handle;
    handle = nullptr;
  }
}

template<typename H>
bool CommBase<H>::is_closed() const {
  return ((!handle) || !(flags & COMM_FLAG_VALID));
}

template<typename H>
void CommBase<H>::reset() {
  close();
}

template<typename H>
CommBase<H>::~CommBase() {
    ygglog_debug << "~CommBase: Started" << std::endl;
    close();
    ygglog_debug << "~CommBase: Finished" << std::endl;
}


}
}
