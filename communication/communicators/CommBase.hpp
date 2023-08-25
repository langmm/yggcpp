#pragma once
#define VIRT_END = 0

#include "utils/tools.hpp"
#include "utils/enums.hpp"
#include "utils/Address.hpp"
#include "utils/logging.hpp"
#include "utils/serialization.hpp"
#include "Workers.hpp"
#include "rapidjson/document.h"
#include "rapidjson/writer.h"
#include "rapidjson/schema.h"

/*! @brief Set if the comm is the receiving comm for a client/server request connection */
const int COMM_FLAG_RPC = COMM_FLAG_SERVER | COMM_FLAG_CLIENT;
#define COMM_NAME_SIZE 100
#define COMM_DIR_SIZE 100

// Bug in gnu std::regex that doesn't allow for matching large messages
// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=86164
#ifdef __GNUC__
#define COMM_BASE_MAX_MSG_SIZE 2048
#else
#define COMM_BASE_MAX_MSG_SIZE
#endif

#define IPC_INSTALLED_FLAG 0
#define ZMQ_INSTALLED_FLAG 0
#define MPI_INSTALLED_FLAG 0

#ifdef IPCINSTALLED
#undef IPC_INSTALLED_FLAG
#define IPC_INSTALLED_FLAG 1
#endif
#ifdef ZMQINSTALLED
#undef ZMQ_INSTALLED_FLAG
#define ZMQ_INSTALLED_FLAG 1
#endif
#if defined(MPIINSTALLED) && defined(MPI_COMM_WORLD)
#undef MPI_INSTALLED_FLAG
#define MPI_INSTALLED_FLAG 1
#endif

#define UNINSTALLED_ERROR(name)					\
  utils::ygglog_throw_error("Compiler flag '" #name "INSTALLED' not defined so " #name " bindings are disabled")
#define ADD_CONSTRUCTORS(T)						\
  explicit T ## Comm(const std::string nme,				\
		     const DIRECTION dirn,				\
		     int flgs = 0, const COMM_TYPE type = T ## _COMM);	\
  explicit T ## Comm(utils::Address *addr,				\
		     const DIRECTION dirn,				\
		     int flgs = 0, const COMM_TYPE type = T ## _COMM);	\
  static bool isInstalled() { return T ## _INSTALLED_FLAG; }		\
  static COMM_TYPE defaultCommType() { return T ## _COMM; }		\
  ~T ## Comm() {							\
    ygglog_debug << "~" #T "Comm: Started" << std::endl;		\
    if (!is_closed()) {							\
      close();								\
    }									\
    ygglog_debug << "~" #T "Comm: Finished" << std::endl;		\
  }

#define ADD_CONSTRUCTORS_DEF(cls)		\
  cls::cls(const std::string nme,		\
	   const DIRECTION dirn,		\
	   int flgs, const COMM_TYPE type) :	\
    cls(nme, nullptr, dirn, flgs, type) {}	\
  cls::cls(utils::Address *addr,		\
	   const DIRECTION dirn,		\
	   int flgs, const COMM_TYPE type) :	\
    cls("", addr, dirn, flgs, type) {}
#define ADD_CONSTRUCTORS_RPC(cls, defT)				\
  explicit cls(const std::string nme,				\
	       int flgs = 0, const COMM_TYPE type = defT);	\
  explicit cls(utils::Address *addr,				\
	       int flgs = 0, const COMM_TYPE type = defT);
#define ADD_CONSTRUCTORS_RPC_DEF(cls)		\
  cls::cls(const std::string nme,		\
	   int flgs, const COMM_TYPE type) :	\
    cls(nme, nullptr, flgs, type) {}		\
  cls::cls(utils::Address *addr,		\
	   int flgs, const COMM_TYPE type) :	\
    cls("", addr, flgs, type) {}
#define WORKER_METHOD_DECS(cls)					\
  Comm_t* create_worker(utils::Address* address,		\
			const DIRECTION&, int flgs) override
#define WORKER_METHOD_DEFS(cls)					\
  Comm_t* cls::create_worker(utils::Address* address,		\
			     const DIRECTION& dir, int flgs) {	\
    return new cls("", address, dir, flgs | COMM_FLAG_WORKER);	\
  }
#define WORKER_METHOD_DUMMY(cls, abbr)				\
  Comm_t* cls::create_worker(utils::Address*,			\
			     const DIRECTION&, int) {		\
    abbr ## _install_error();					\
    return NULL;						\
  }

using namespace rapidjson;

namespace communication {

namespace communicator {

YGG_THREAD_GLOBAL_VAR(int, global_scope_comm, )

void global_scope_comm_on();
void global_scope_comm_off();

class AsyncComm;
class AsyncBacklog;

class RPCComm;

class ServerComm;

class ClientComm;

class ZMQComm;

class IPCComm;

class RequestList;

typedef struct comm_t comm_t;

/**
 * Abstract base class for all communicators. Cannot be instantiated directly, but is used as a generalized hook
 * for passing communicators around. Should only be instantiated by the CommBase<> class.
 */
class Comm_t {
public:
    Comm_t(const Comm_t& other) = delete;
    Comm_t& operator=(const Comm_t&) = delete;
    virtual ~Comm_t();

    /*!
      @brief Send a string message through the communicator.
      @param[in] data Message.
      @returns int Values >= 0 indicate success.
     */
    int send(const std::string& data) {
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
      @brief Get the time limit for receiving messages.
      @returns Timeout in micro-seconds.
     */
    virtual int get_timeout_recv();
    /*!
      @brief Wait until a message is available to be received or a time
        limit is reached.
      @param[in] tout Time (in micro-seconds) that should be waited. If -1
        the process will wait forever.
      @return Number of messages available for receive. -1 if an error
        occurred.
     */
    virtual int wait_for_recv(const int& tout);
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
      @return Integer specifying if the receive was succesful.
        Values >= 0 indicate success.
    */
    template<typename T>
    int sendVar(const T data) {
      ygglog_debug << "CommBase(" << name <<
	")::sendVar(const T& data)" << std::endl;
      if (!checkType(data, SEND))
	return -1;
      if (getMetadata(SEND).isGeneric())
	return sendVarAsGeneric(data);
      return send(1, data);
    }
    /*!
      @brief Send an object through the communicator as a rapidjson
        Document.
      @tparam T Type of object being sent.
      @param[in] data Object to send.
      @return Integer specifying if the receive was succesful.
        Values >= 0 indicate success.
    */
    template<typename T>
    int sendVarAsGeneric(const T data) {
      ygglog_debug << "CommBase(" << name <<
	")::sendVarAsGeneric" << std::endl;
      rapidjson::Document doc;
      doc.Set(data, doc.GetAllocator());
      return sendVar(doc);
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
      if (getMetadata(RECV).isGeneric())
	return recvVarAsGeneric(data);
      return recv(1, &data);
    }
    /*!
      @brief Receive an object from the communicator that expects generic
        objects.
      @tparam T Type of object being received.
      @param[out] data Object to receive message into.
      @return Integer specifying if the receive was succesful.
        Values >= 0 indicate success.
    */
    template<typename T>
    long recvVarAsGeneric(T& data) {
      ygglog_debug << "CommBase(" << name << ")::recvVarAsGeneric" << std::endl;
      rapidjson::Document doc;
      long out = recvVar(doc);
      if (out >= 0) {
	data = doc.Get<T>();
      }
      return out;
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
      @brief Send a request and receive a response in the forms of
        rapidjson Documents.
      @param[in] sendData rapidjson document containing the request data.
      @param[out] recvData rapidjson document that the response data
        should be stored in.
      @return Integer specifying if the send and receive were succesful.
        Values >= 0 indicate success.
    */
    long callVar(const rapidjson::Document& sendData,
		 rapidjson::Document& recvData);
  
    /*!
      @brief Send a request and receive a response from a list of
        variable arguments containing data for both the request and
        response.
      @param[in] ap Variable argument list that request will be
        constructed from and response will be receieved into.
      @return Integer specifying if the send and receive were succesful.
        Values >= 0 indicate success.
    */
    virtual long vCall(rapidjson::VarArgList& ap);
      
    /*!
      @brief Get the number of messages in the communicator.
      @param[in] dir Direction to check for messages in.
      @return Number of messages.
     */
    virtual int comm_nmsg(DIRECTION dir=NONE) const VIRT_END;

    /*!
      @brief Close the communicator.
     */
    virtual void close() VIRT_END;

    /*!
      @brief Check if the communicator is closed.
      @return true if the communicator is closed, false otherwise.
     */
    virtual bool is_closed() const VIRT_END;

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
    /*!
      @brief Determine if the communicator is async.
      @return true if it is async, false otherwise.
     */
    bool async() { return flags & COMM_FLAG_ASYNC; }
    /*!
      @brief Get the Metadata object containing header information about
        the comm including datatype.
      @return Metadata.
     */
    virtual communication::utils::Metadata& getMetadata(const DIRECTION dir=NONE);
    /*!
      @brief Get the bitwise flags associated with the communicator.
      @returns flags.
     */
    int& getFlags() { return flags; }
    /*!
      @brief Get the communicator's name.
      @returns name.
     */
    const std::string& getName() const { return name; }
    /*!
      @brief Get the communicator's address.
      @returns Address.
     */
    std::string getAddress() const {
      if (address)
	return address->address();
      return "";
    }
    /*!
      @brief Get the communicator's direction.
      @returns Direction.
     */
    DIRECTION getDirection() const { return direction; }
    /*!
      @brief Get the communicator's type.
      @returns Communicator type.
     */
    COMM_TYPE getCommType() const { return type; }
    /*!
      @brief Get the maximum size (in bytes) for individual messages.
        Messages larger than this size will be split into multiple parts.
      @returns Maximum message size.
    */
    size_t getMaxMsgSize() const { return maxMsgSize; }
    /*!
      @brief Get the buffer size that should be reserved in messages.
      @returns Reserved message buffer size.
    */
    size_t getMsgBufSize() const { return msgBufSize; }
    /*!
      @brief Determine if the communicator is fully installed.
      @returns true if it is installed, false otherwise.
    */
    static bool isInstalled() { return false; }
  
#ifdef YGG_TEST
    WorkerList& getWorkers() { return workers; }
    virtual bool afterSendRecv(Comm_t*, Comm_t*) { return true; }
    Comm_t* getGlobalComm() { return global_comm; }
#endif
    void addSchema(const utils::Metadata& s);
    void addSchema(const rapidjson::Value& s, bool isMetadata = false);
    void addSchema(const std::string& schemaStr, bool isMetadata = false);
    void addFormat(const std::string& format_str, bool as_array = false);
    void copySchema(const Comm_t* other);

    static void _ygg_cleanup();

private:
    int deserialize(const char* buf, rapidjson::VarArgList& ap);
    int serialize(char*& buf, size_t& buf_siz,
		  rapidjson::VarArgList& ap);

protected:
    friend AsyncComm;
    friend AsyncBacklog;
    friend RPCComm;
    friend ServerComm;
    friend ClientComm;
    friend IPCComm;
    friend ZMQComm;
    friend RequestList;
    friend Worker;
    friend WorkerList;

public:
    static int _ygg_initialized;
    static int _ygg_finalized;
    static int _ygg_init();

protected:
    void updateMaxMsgSize(size_t new_size) {
      if (maxMsgSize == 0 || new_size < maxMsgSize)
	maxMsgSize = new_size;
    }
    void updateMsgBufSize(size_t new_size) {
      msgBufSize = new_size;
    }

    void setFlags(const utils::Header& head, DIRECTION dir) {
      if (dir == SEND)
	flags |= COMM_FLAGS_USED_SENT;
      else
	flags |= COMM_FLAGS_USED_RECV;
      if (head.flags & HEAD_FLAG_EOF) {
	if (dir == SEND)
	  flags |= COMM_EOF_SENT;
	else
	  flags |= COMM_EOF_RECV;
      }
    }

    void init() {
      if (flags & COMM_FLAG_SET_OPP_ENV)
	setOppEnv();
    }
    static std::string envName(const std::string& name,
			       DIRECTION dir, bool opp=false) {
      std::string out = name;
      if (out.size() > COMM_NAME_SIZE)
	out.resize(COMM_NAME_SIZE);
      if (((!opp) && dir == SEND) || (opp && dir == RECV))
	out += "_OUT";
      else if (((!opp) && dir == RECV) || (opp && dir == SEND))
	out += "_IN";
      return out;
    }
    void setOppEnv() {
      if (address) {
	std::string opp_name = envName(name, direction, true);
	ygglog_debug << "CommBase(" << name << ")::setOppEnv: " << opp_name << " = " << getAddress() << std::endl;
	setenv(opp_name.c_str(), getAddress().c_str(), 1);
      }
    }
    void unsetOppEnv() {
      ygglog_debug << "CommBase(" << name << ")::unsetOppEnv" << std::endl;
      std::string opp_name = envName(name, direction, true);
      unsetenv(opp_name.c_str());
    }

    static utils::Address* addressFromEnv(const std::string& name,
					  DIRECTION direction) {
      utils::Address* out = new utils::Address();
      if (name.empty())
	return out;
      std::string full_name = envName(name, direction);
      char *addr = std::getenv(full_name.c_str());
      if (!addr) {
	std::string temp_name(full_name);
	size_t loc;
	while ((loc = temp_name.find(":")) != std::string::npos) {
	  temp_name.replace(loc, 1, "__COLON__");
	}
	addr = getenv(temp_name.c_str());
      }
      std::string addr_str = "null";
      if (addr)
	addr_str.assign(addr_str);
      ygglog_debug << "CommBase::addressFromEnv: full_name = " <<
	full_name << ", address = " << addr_str << std::endl;
      ygglog_debug << std::endl;
      if (addr)
	out->address(addr);
      return out;
    }

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
      communication::utils::Metadata& meta = getMetadata(dir);
      if (dir == RECV)
	zeroData(&data);
      try {
	meta.fromData(data);
      } catch (...) {
	return false;
      }
      return true;
    }
    virtual bool create_header_send(utils::Header&) { return true; }
    rapidjson::Value& getSchema(const DIRECTION dir=NONE) {
      return getMetadata(dir).getSchema();
    }
    virtual Comm_t* create_worker(utils::Address* address,
				  const DIRECTION&, int flgs) VIRT_END;
    virtual Comm_t* create_worker_send(utils::Header& head);
    virtual Comm_t* create_worker_recv(utils::Header& head);
    /**
     * Sending function
     * @param header Instance containing message and header.
     * @return The length of data sent.
     */
    virtual int send_single(utils::Header& header) VIRT_END;
    /**
     * Receiving function
     * @param header Instance to store message and header in.
     * @return The length of data received.
     */
    virtual long recv_single(utils::Header& header) VIRT_END;

    /**
     * Constructor, which can only be instantiated by a child class
     * @param name The name of communicator
     * @param address The address to associate with this communicator.
     * @param direction Whether this instance is a sender or receiver
     * @param t Enumerated communicator type
     * @param flgs Initial bitwise flags
     * @see utils::Address()
     */
    explicit Comm_t(const std::string &name,
		    utils::Address *address = nullptr,
		    DIRECTION direction = NONE,
		    const COMM_TYPE &t = NULL_COMM, int flgs = 0);

    /**
     * Checks the size of the message to see if it exceeds the maximum allowable size as define by YGG_MSG_MAX
     * @param len The length of the message to check
     * @return bool Whether the message is smaller than YGG_MSG_MAX (true), false otherwise
     */
    bool check_size(const size_t &len) const;

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
    utils::Metadata metadata;
    int timeout_recv; //!< Time to wait for messages during receive.
    WorkerList workers; //!< Communicator to use for sending large messages.
    Comm_t* global_comm; // !< Pointer to global comm that this comm shadows.
    std::vector<std::string> cache; // !< Cache of messages received.

    static std::vector<Comm_t*> registry;
    bool get_global_scope_comm();
public:
    static void register_comm(Comm_t* x);
    static Comm_t* find_registered_comm(const std::string& name,
					const DIRECTION dir,
					const COMM_TYPE type);
};

/**
 * Creates a new communicator of the specified type
 * @param dir The direction for the communicator
 * @param type The enumerated type of communicator to create
 * @param name The name of the communicator
 * @param address The initial address of the communicator.
 * @param flags Bitwise flags describing the communicator
 * @return
 */
Comm_t* new_Comm_t(const DIRECTION dir, const COMM_TYPE type, const std::string &name="", char* address=nullptr, int flags=0);
Comm_t* new_Comm_t(const DIRECTION dir, const COMM_TYPE type, const std::string &name, utils::Address* address, int flags=0);

/**
 * Determine if a communicator type is installed.
 * @param commtype The communicator type to check.
 * @return true if the communicator type is installed, false otherwise.
 */
bool is_commtype_installed(const COMM_TYPE type);

/**
 * Templated base class for all communicators
 * @tparam H Handle type for the communicator
 */
template<typename H>
class CommBase : public Comm_t {
public:
    CommBase(const CommBase& other) = delete;
    CommBase& operator=(const CommBase&) = delete;
    CommBase() = delete;

    /*! \copydoc Comm_t::comm_nmsg */
    int comm_nmsg(DIRECTION=NONE) const override {
      ygglog_error << "Comm_nmsg of base class called, must be overridden" << std::endl;
      return -1;
    }
    /*! \copydoc Comm_t::close */
    void close() override;
    /*! \copydoc Comm_t::is_closed */
    bool is_closed() const override;
    using Comm_t::send;
    using Comm_t::recv;

protected:
    /**
     * Not used, must be overloaded by a child class
     */
    int send_single(utils::Header&) override {
      ygglog_error << "Send of base class called, must be overridden" << std::endl;
      return -1;
    }

    /**
     * Not used, must be overloaded by child class
     */
    long recv_single(utils::Header&) override {
      ygglog_error << "Recv of base class called, must be overridden" << std::endl;
      return -1;
    }

    /**
     * Constructor
     * @param name The name of the communicator
     * @param address The initial address for the communicator
     * @param direction The enumerated direction of the communicator
     * @param t The enumerated type of the communicator
     * @param flags Bitwise flags describing the communicator
     */
    explicit CommBase(const std::string &name, utils::Address *address = nullptr, DIRECTION direction = NONE, const COMM_TYPE &t = NULL_COMM, int flags = 0);

    Comm_t* create_worker(utils::Address*, const DIRECTION&,
			  int) override {
      utils::ygglog_throw_error("create_worker of base class called, must be overridden");
      return NULL; // GCOVR_EXCL_LINE
    }

    /**
     * Destructor
     */
    ~CommBase() override;

    H *handle; //!< Pointer to handle for comm.

#ifdef YGG_TEST
public:
    H* getHandle() {
      if (global_comm)
	return dynamic_cast<CommBase<H>*>(global_comm)->getHandle();
      return handle;
    }
    void setHandle(H* h) {
      if (global_comm)
	dynamic_cast<CommBase<H>*>(global_comm)->setHandle(h);
      handle = h;
    }
#endif // YGG_TEST
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
  workers.workers.clear();
}

template<typename H>
bool CommBase<H>::is_closed() const {
  return ((!((bool)handle)) || !(flags & COMM_FLAG_VALID));
}

template<typename H>
CommBase<H>::~CommBase() {
    ygglog_debug << "~CommBase: Started" << std::endl;
    close();
    ygglog_debug << "~CommBase: Finished" << std::endl;
}


}
}
