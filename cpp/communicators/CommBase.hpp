#pragma once
#define VIRT_END = 0
#include "utils/tools.hpp"
#include "utils/enums.hpp"
#include "utils/Address.hpp"
#include "utils/logging.hpp"
#include "utils/serialization.hpp"
#include "Workers.hpp"
#include "utils/rapidjson_wrapper.hpp"
#include "CommContext.hpp"

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
#define RMQ_INSTALLED_FLAG 0

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
#ifdef RMQINSTALLED // TODO: Check for RMQ server
#undef RMQ_INSTALLED_FLAG
#define RMQ_INSTALLED_FLAG 1
#endif

#define UNINSTALLED_ERROR(name)					\
  utils::YggLogThrowError("Compiler flag '" #name "INSTALLED' not defined so " #name " bindings are disabled")

#define ADD_DESTRUCTOR(cls, base)					\
  protected:								\
  /** \copydoc YggInterface::communicator::Comm_t::_close */		\
  void _close(bool call_base);						\
public:									\
 /** @brief Close the communicator */					\
 void close() override;							\
 /** @brief Destructor */						\
 ~cls() override;
#define ADD_DESTRUCTOR_DEF(cls, base, tempT, temp)			\
  tempT									\
  void cls temp::close() {						\
    log_debug() << #cls "::close: Started" << std::endl;		\
    _close(true);							\
    log_debug() << #cls "::close: Finished" << std::endl;		\
  }									\
  tempT									\
  cls temp::~cls() {							\
    YggLogDestructor << "~" #cls ": Started (closed = " << this->_is_closed() << ")" << std::endl; \
    if (this->handle) {							\
      _close(false);							\
    }									\
    YggLogDestructor << "~" #cls ": Finished" << std::endl;		\
  }

#define ADD_METHODS_BASE(cls, typ, flag)				\
  /** \copydoc YggInterface::communicator::Comm_t::isInstalled */	\
  static bool isInstalled() { return flag; }				\
  /** \copydoc YggInterface::communicator::Comm_t::defaultCommType */	\
  static COMM_TYPE defaultCommType() { return typ; }			\
  ADD_DESTRUCTOR(cls, CommBase)
#define ADD_CONSTRUCTORS_BASE_NOLOG(cls, typ, flag)			\
  /** @brief Constructor */						\
  /** @param[in] nme Communicator name. If empty, one will be generated */ \
  /** @param[in] dirn Enumerated communicator direction */		\
  /** @param[in] flgs Bitwise communicator flags */			\
  /** @param[in] type Enumerated communicator type */			\
  explicit cls(const std::string& nme,					\
	       const DIRECTION dirn,					\
	       int flgs = 0, const COMM_TYPE type = typ);		\
  /** @brief Constructor */						\
  /** @param[in] addr Communicator address. If empty, one will be generated */ \
  /** @param[in] dirn Enumerated communicator direction */		\
  /** @param[in] flgs Bitwise communicator flags */			\
  /** @param[in] type Enumerated communicator type */			\
  explicit cls(utils::Address &addr,					\
	       const DIRECTION dirn,					\
	       int flgs = 0, const COMM_TYPE type = typ);		\
  ADD_METHODS_BASE(cls, typ, flag)
#define ADD_CONSTRUCTORS_BASE(cls, typ, flag)				\
  ADD_CONSTRUCTORS_BASE_NOLOG(cls, typ, flag)				\
  /** \copydoc YggInterface::utils::LogBase::logClass */		\
  std::string logClass() const override { return #cls; }
#define ADD_CONSTRUCTORS(T)			\
  ADD_CONSTRUCTORS_BASE(T ## Comm, T ## _COMM, T ## _INSTALLED_FLAG)

#define ADD_CONSTRUCTORS_DEF(cls)				\
  cls::cls(const std::string& nme,				\
	   const DIRECTION dirn,				\
	   int flgs, const COMM_TYPE type) :			\
    cls(nme, utils::blankAddress, dirn, flgs, type) {}		\
  cls::cls(utils::Address &addr,				\
	   const DIRECTION dirn,				\
	   int flgs, const COMM_TYPE type) :			\
    cls("", addr, dirn, flgs, type) {}				\
  ADD_DESTRUCTOR_DEF(cls, CommBase, , )
#define ADD_CONSTRUCTORS_RPC(cls, defT)					\
  /** @brief Constructor */						\
  /** @param[in] nme Communicator name. If empty, one will be generated */ \
  /** @param[in] flgs Bitwise communicator flags */			\
  /** @param[in] type Enumerated communicator type */			\
  /** @param[in] ncomm Number of forked communicators (fork only) */	\
  /** @param[in] reqtype Enumerated type of request comm (rpc only) */	\
  /** @param[in] restype Enumerated type of response comm (rpc only) */	\
  /** @param[in] reqflags Bitwise flags for request comm (rpc only) */	\
  /** @param[in] resflags Bitwise flags for response comm (rpc only) */	\
  explicit cls(const std::string& nme,					\
	       int flgs = 0, const COMM_TYPE type = defT,		\
	       size_t ncomm = 0,					\
	       const COMM_TYPE reqtype = DEFAULT_COMM,			\
	       const COMM_TYPE restype = DEFAULT_COMM,			\
	       int reqflags = 0, int resflags = 0);			\
  /** @brief Constructor */						\
  /** @param[in] addr Communicator address. If empty, one will be generated */ \
  /** @param[in] flgs Bitwise communicator flags */			\
  /** @param[in] type Enumerated communicator type */			\
  /** @param[in] ncomm Number of forked communicators (fork only) */	\
  /** @param[in] reqtype Enumerated type of request comm (rpc only) */	\
  /** @param[in] restype Enumerated type of response comm (rpc only) */	\
  /** @param[in] reqflags Bitwise flags for request comm (rpc only) */	\
  /** @param[in] resflags Bitwise flags for response comm (rpc only) */	\
  /** @see utils::Address */						\
  explicit cls(utils::Address &addr,					\
	       int flgs = 0, const COMM_TYPE type = defT,		\
	       size_t ncomm = 0,					\
	       const COMM_TYPE reqtype = DEFAULT_COMM,			\
	       const COMM_TYPE restype = DEFAULT_COMM,			\
	       int reqflags = 0, int resflags = 0);			\
  ADD_DESTRUCTOR(cls, RPCComm)
#define ADD_CONSTRUCTORS_RPC_DEF(cls)			\
  cls::cls(const std::string& nme,			\
	   int flgs, const COMM_TYPE type,		\
	   size_t ncomm,				\
	   const COMM_TYPE reqtype,			\
	   const COMM_TYPE restype,			\
	   int reqflags, int resflags) :		\
    cls(nme, utils::blankAddress, flgs, type, ncomm,	\
	reqtype, restype, reqflags, resflags) {}	\
  cls::cls(utils::Address &addr,			\
	   int flgs, const COMM_TYPE type,		\
	   size_t ncomm,				\
	   const COMM_TYPE reqtype,			\
	   const COMM_TYPE restype,			\
	   int reqflags, int resflags) :		\
    cls("", addr, flgs, type, ncomm,			\
	reqtype, restype, reqflags, resflags) {}	\
  ADD_DESTRUCTOR_DEF(cls, RPCComm, , )			\
  void cls::_close(bool call_base) {			\
    if (call_base) {					\
      RPCComm::_close(true);				\
    }							\
  }
#define WORKER_METHOD_DECS(cls)						\
  /** \copydoc YggInterface::communicator::Comm_t::create_worker */	\
  Comm_t* create_worker(utils::Address& address,			\
			const DIRECTION dir, int flgs) override
#define WORKER_METHOD_DEFS(cls)					\
  Comm_t* cls::create_worker(utils::Address& address,		\
			     const DIRECTION dir, int flgs) {	\
    return new cls("", address, dir, flgs | COMM_FLAG_WORKER);	\
  }
#define WORKER_METHOD_DUMMY(cls, abbr)				\
  Comm_t* cls::create_worker(utils::Address&,			\
			     const DIRECTION direction, int) {	\
    abbr ## _install_error();					\
    return NULL;						\
  }
#define ADD_KEY_TRACKER_DECS						\
  private:								\
  /** @brief Check if a key is already registered */			\
  /** @param[in] key Key to check */					\
  /** @return Negative index+1 of key if present, 0 otherwise */	\
  int check_key(int key);						\
  /** @brief Add a key to the set of registered keys */			\
  /** @param[in] key Key to add */					\
  /** @return 0 on success, -1 on error */				\
  int track_key(int key);						\
  /** @brief Remove a key from those registered */			\
  /** @param[in] key Key to remove from registry */			\
  /** @return 0 on success, -1 on error */				\
  int untrack_key(int key);						\
  static int _keysUsed[MAX_KEYS_ALLOWED]; /**< Registry of keys */	\
  static unsigned _NkeysUsed; /**< Number of registered keys */		\
  static bool _rand_seeded /**< true if rand has been seeded for keys */
#define ADD_KEY_TRACKER_DEFS(cls)					\
  int cls::_keysUsed[MAX_KEYS_ALLOWED];					\
  unsigned cls::_NkeysUsed = 0;						\
  bool cls::_rand_seeded = false;					\
  int cls::check_key(int key) {						\
    unsigned i;								\
    int error_code = 0;							\
    YGG_THREAD_SAFE_BEGIN(cls) {					\
      for (i = 0; i < cls::_NkeysUsed; i++ ) {				\
	if (cls::_keysUsed[i] == key) {					\
	  error_code = -(static_cast<int>(i) + 1);			\
	  break;							\
	}								\
      }									\
      /* Fail if > _yggTrackChannels channels used */			\
    } YGG_THREAD_SAFE_END;						\
    return error_code;							\
  }									\
  int cls::track_key(int key) {						\
    int ret = 0;							\
    YGG_THREAD_SAFE_BEGIN(cls) {					\
      if (cls::_NkeysUsed++ >= MAX_KEYS_ALLOWED) {			\
	log_error() << "Too many channels in use, max: " << MAX_KEYS_ALLOWED << std::endl; \
	ret = -1;							\
      } else {								\
	cls::_keysUsed[cls::_NkeysUsed] = key;				\
      }									\
    } YGG_THREAD_SAFE_END;						\
    return ret;								\
  }									\
  int cls::untrack_key(int key) {					\
    int ret = 0; /* -1; */						\
    unsigned i;								\
    YGG_THREAD_SAFE_BEGIN(cls) {					\
      for (i = 0; i < cls::_NkeysUsed; i++) {				\
	if (key == cls::_keysUsed[i]) {					\
	  memmove(cls::_keysUsed + i, cls::_keysUsed + i + 1,		\
		  (MAX_KEYS_ALLOWED - (i + 1))*sizeof(int));		\
	  cls::_NkeysUsed--;						\
	  ret = 0;							\
	  break;							\
	}								\
      }									\
    } YGG_THREAD_SAFE_END;						\
    return ret;								\
  }
#define CREATE_KEY(cls)				\
  YGG_THREAD_SAFE_BEGIN(cls) {			\
    if (!cls::_rand_seeded) {			\
      std::srand(ptr2seed(this));		\
      cls::_rand_seeded = true;			\
    }						\
  } YGG_THREAD_SAFE_END;			\
  while (key == 0 || check_key(key) < 0) {	\
    key = std::rand();				\
  }


using namespace rapidjson;

namespace YggInterface {

namespace communicator {

class AsyncComm;
class AsyncBacklog;
class RPCComm;
class ServerComm;
class ClientComm;
class ZMQComm;
class IPCComm;
class WrapComm;
class RequestList;

  // typedef struct comm_t comm_t;


/**
 * @brief Abstract base class for all communicators. Cannot be instantiated directly, but is used as a generalized hook
 * for passing communicators around. Should only be instantiated by the CommBase<> class.
 */
class YGG_API Comm_t : public YggInterface::utils::LogBase {
public:
    Comm_t(const Comm_t& other) = delete;
    Comm_t& operator=(const Comm_t&) = delete;
    Comm_t() = delete;
    virtual ~Comm_t();

    //////////////////
    // SEND METHODS //
    //////////////////
  
    /*!
      @brief Send a message indicating that the communicator is closing.
      @returns int Values >= 0 indicate success.
     */
     int send_eof() {
       return send_raw(YGG_MSG_EOF, YGG_MSG_EOF_LEN);
     }
    /*!
      @brief Send a string message through the communicator as raw bytes
        without setting the datatype, performing normalization,
	transformations, filtering, or calling the rapidjson serializer.
      @param[in] data Message.
      @param[in] len Size of data in bytes.
      @returns int Values >= 0 indicate success.
     */
    virtual int send_raw(const char *data, const size_t &len);
    /*!
      @brief Send a rapidjson document through the communicator.
      @param[in] data Message.
      @param[in] not_generic If true, the datatype will not be updated to
        expect a generic object in all future send calls.
      @returns int Values >= 0 indicate success.
     */
    int send(const rapidjson::Document& data, bool not_generic=false);
    /*!
      @brief Send a rapidjson value through the communicator.
      @param[in] data Message.
      @param[in] not_generic If true, the datatype will not be updated to
        expect a generic object in all future send calls.
      @returns int Values >= 0 indicate success.
     */
    int send(const rapidjson::Value& data, bool not_generic=false);
    /*!
      @brief Send a set of objects through the communicator.
      @tparam T Type of first object being sent in message.
      @param[in] data First object to send in message.
      @param[in] args Additional objects to send in the message.
      @return Integer specifying if the receive was succesful.
        Values >= 0 indicate success.
    */
    template<typename T, typename... Args>
    int sendVar(const T& data, Args... args) {
      rapidjson::Document doc(rapidjson::kArrayType);
      return _sendVA(0, doc, data, args...);
    }
private:
#ifndef DOXYGEN_SHOULD_SKIP_THIS
#define HANDLE_SEND_BEFORE_			\
  UNUSED(i);					\
  rapidjson::Value v
#define HANDLE_SEND_AFTER_			\
  doc.PushBack(v, doc.GetAllocator());		\
  i++
#define HANDLE_SEND_NEXT_			\
  return _sendVA(i, doc, args...)
#define HANDLE_SEND_LAST_(single)		\
  if (doc.Size() == 1) {			\
    rapidjson::Document tmp;			\
    tmp.Swap(doc[0]);			\
    return send(tmp, single);			\
  }						\
  return send(doc, single)
#define HANDLE_SEND_(set, adv, single, ...)			\
  int _sendVA(int i, rapidjson::Document& doc, __VA_ARGS__) {	\
    HANDLE_SEND_BEFORE_;					\
    set;							\
    HANDLE_SEND_AFTER_;						\
    adv;							\
    HANDLE_SEND_LAST_(single);					\
  }								\
  template<typename... Args>					\
  int _sendVA(int i, rapidjson::Document& doc, __VA_ARGS__,	\
	      Args... args) {					\
    HANDLE_SEND_BEFORE_;					\
    set;							\
    HANDLE_SEND_AFTER_;						\
    adv;							\
    HANDLE_SEND_NEXT_;						\
  }
#define HANDLE_SEND_TMP_(tmp, set, adv, single, ...)	\
  template<tmp>						\
  int _sendVA(int i, rapidjson::Document& doc, __VA_ARGS__) {	\
    HANDLE_SEND_BEFORE_;					\
    set;							\
    HANDLE_SEND_AFTER_;						\
    adv;							\
    HANDLE_SEND_LAST_(single);					\
  }								\
  template<tmp, typename... Args>				\
  int _sendVA(int i, rapidjson::Document& doc, __VA_ARGS__,	\
	      Args... args) {					\
    HANDLE_SEND_BEFORE_;					\
    set;							\
    HANDLE_SEND_AFTER_;						\
    adv;							\
    HANDLE_SEND_NEXT_;						\
  }
#define HANDLE_SEND_TMP_COND_(tmp, set, cond, adv, single, ...)	\
  template<tmp>						\
  RAPIDJSON_ENABLEIF_RETURN(cond, (int))			\
  _sendVA(int i, rapidjson::Document& doc, __VA_ARGS__) {	\
    HANDLE_SEND_BEFORE_;					\
    set;							\
    HANDLE_SEND_AFTER_;						\
    adv;							\
    HANDLE_SEND_LAST_(single);					\
  }								\
  template<tmp, typename... Args>				\
  RAPIDJSON_ENABLEIF_RETURN(cond, (int))			\
  _sendVA(int i, rapidjson::Document& doc, __VA_ARGS__,	\
	  Args... args) {					\
    HANDLE_SEND_BEFORE_;					\
    set;							\
    HANDLE_SEND_AFTER_;						\
    adv;							\
    HANDLE_SEND_NEXT_;						\
  }
  HANDLE_SEND_TMP_COND_(
    typename T, v.Set(data, doc.GetAllocator()),
    (internal::NotExpr<
     internal::OrExpr<internal::IsSame<T, char*>,
     internal::OrExpr<internal::IsSame<T, char[]>, 
     internal::OrExpr<internal::IsSame<T, rapidjson::Document>,
     internal::IsPointer<T> > > > >),
    , true, const T& data)
  HANDLE_SEND_(
    v.SetString(data, static_cast<rapidjson::SizeType>(len),
		doc.GetAllocator()),
    i++, true, const char*& data, const size_t& len)
  HANDLE_SEND_(
    v.SetString(data, static_cast<rapidjson::SizeType>(strlen(data)),
		doc.GetAllocator()),
    , false, const char*& data)
  HANDLE_SEND_TMP_(
    size_t N,
    v.SetString(data, static_cast<rapidjson::SizeType>(N),
		doc.GetAllocator()),
    , false, const char (&data)[N])
  HANDLE_SEND_(
    v.CopyFrom(data, doc.GetAllocator(), true),
    , false, const rapidjson::Document& data)
  HANDLE_SEND_TMP_COND_(
    typename T, v.Set1DArray(data,
			     static_cast<rapidjson::SizeType>(len),
			     doc.GetAllocator()),
    (internal::AndExpr<internal::IsPointer<T>,
     internal::NotExpr<internal::IsSame<T, char*> > >),
    i++, true, const T& data, const size_t& len)
  HANDLE_SEND_TMP_COND_(
    typename T, v.SetNDArray(data, shape,
			     static_cast<rapidjson::SizeType>(ndim),
			     doc.GetAllocator()),
    (internal::AndExpr<internal::IsPointer<T>,
     internal::NotExpr<internal::IsSame<T, char*> > >),
    i += 2, true, const T& data,
    const rapidjson::SizeType& ndim,
    const size_t*& shape)
#undef HANDLE_SEND_BEFORE_
#undef HANDLE_SEND_AFTER_
#undef HANDLE_SEND_NEXT_
#undef HANDLE_SEND_LAST_
#undef HANDLE_SEND_
#undef HANDLE_SEND_TMP_
#undef HANDLE_SEND_TMP_COND_
#endif // DOXYGEN_SHOULD_SKIP_THIS
public:
    /*!
      @brief Send a string message through the communicator.
      @param[in] data Message.
      @param[in] len Size of data in bytes.
      @returns int Values >= 0 indicate success.
     */
    int send(const char *data, const size_t &len);
    /*!
      @brief Send a string message through the communicator.
      @param[in] data Message.
      @returns int Values >= 0 indicate success.
     */
    int send(const std::string& data) {
      return sendVar(data);
    }

    //////////////////
    // RECV METHODS //
    //////////////////
  
    /*!
      @brief Set the time limit for receiving messages.
      @param[in] new_timeout New time limit in micro-seconds. -1 will
        cause receive calls to block until a message is available.
     */
    virtual void set_timeout_recv(int64_t new_timeout);
    /*!
      @brief Get the time limit for receiving messages.
      @returns Timeout in micro-seconds.
     */
    virtual int64_t get_timeout_recv();
    /*!
      @brief Wait until a message is available to be received or a time
        limit is reached.
      @param[in] tout Time (in micro-seconds) that should be waited. If -1
        the process will wait forever.
      @return Number of messages available for receive. -1 if an error
        occurred.
     */
    virtual int wait_for_recv(const int64_t& tout);
    /*!
      @brief Receive a raw string message from the communicator without
        performing transformations, normalization, filtering, setting the,
	datatype, or calling the rapidjson deserializer.
      @param[out] data Heap buffer where the message should be saved.
      @param[in] len Length of the allocated message buffer in bytes.
      @returns -1 if message could not be received. Length of the
        received message if message was received.
    */
    virtual long recv_raw(char*& data, const size_t &len);
    /*!
      @brief Receive a message as a rapidjson::Document.
      @param[out] data rapidjson document to populate with received data.
      @param[in] not_generic If true, the datatype will not be updated to
        expect a generic object in all future recv calls.
      @returns -1 if message could not be received. Length of the
        received message if message was received.
    */
    long recv(rapidjson::Document& data, bool not_generic=false);
    /*!
      @brief Receive a series of objects from the communicator.
      @tparam T Type of first object being received.
      @param[out] data First object to receive message into.
      @param[out] args Additional objects that message should be stored
        in during recursive calls.
      @return Integer specifying if the receive was succesful.
        Values >= 0 indicate success.
    */
    template<typename T, typename... Args>
    long recvVar(T& data, Args... args) {
      rapidjson::Document doc;
      long out = recv(doc, true);
      if (out < 0) return out;
      return _recvVA(0, false, doc, data, args...);
    }
    /*!
      @brief Receive a series of objects from the communicator, allowing
        for reallocation of variable size objects.
      @tparam T Type of first object being received.
      @param[out] data First object to receive message into.
      @param[out] args Additional objects that message should be stored
        in during recursive calls.
      @return Integer specifying if the receive was succesful.
        Values >= 0 indicate success.
    */
    template<typename T, typename... Args>
    long recvVarRealloc(T& data, Args... args) {
      rapidjson::Document doc;
      long out = recv(doc, true);
      if (out < 0) return out;
      return _recvVA(0, true, doc, data, args...);
    }
private:
#ifndef DOXYGEN_SHOULD_SKIP_THIS
#define HANDLE_RECV_BEFORE_						\
  UNUSED(allow_realloc);						\
  bool was_array = doc.IsArray();					\
  UNUSED(was_array);							\
  if (!doc.IsArray()) {							\
    rapidjson::Value tmp;						\
    tmp.Swap(doc);							\
    doc.SetArray();							\
    doc.PushBack(tmp, doc.GetAllocator());				\
  }									\
  if ((!doc.IsArray()) || doc.Size() == 0) {				\
    log_error() <<							\
      "recvVar(T& data): Received document does not have enough "	\
      "arguments to fill all passed variables: " << doc << std::endl;	\
    return -1;								\
  }									\
  zeroData(&data)
#define HANDLE_RECV_AFTER_						\
  doc.Erase(doc.Begin());						\
  i++
#define HANDLE_RECV_CHECK_(check, Tname)				\
  if (!check) {								\
    log_error() <<							\
      "recvVar(T& data): " <<						\
      "Element" << i << " in received document is not the "		\
      "expected type. type = " << Tname <<				\
      ", document = " << doc[0] << std::endl;				\
    return -1;								\
  }
  
#define HANDLE_RECV_NEXT_			\
  return _recvVA(i, allow_realloc, doc, args...)
#define HANDLE_RECV_LAST_						\
  if (doc.Size() > 0) {							\
    log_error() <<							\
      "recvVar(T& data): Received document has more members than "	\
      "the number of passed arguments (" << i << "), remaining "	\
      "elements: " << doc << std::endl;					\
    return -1;								\
  }									\
  return i
#define HANDLE_RECV_(check, set, adv, Tname, after, ...)		\
  long _recvVA(int i, bool allow_realloc, rapidjson::Document& doc,	\
	       __VA_ARGS__) {						\
    HANDLE_RECV_BEFORE_;						\
    HANDLE_RECV_CHECK_(check, Tname);					\
    set;								\
    HANDLE_RECV_AFTER_;							\
    adv;								\
    after;								\
    HANDLE_RECV_LAST_;							\
    return i;								\
  }									\
  template<typename... Args>						\
  long _recvVA(int i, bool allow_realloc, rapidjson::Document& doc,	\
	       __VA_ARGS__, Args... args) {				\
    HANDLE_RECV_BEFORE_;						\
    HANDLE_RECV_CHECK_(check, Tname);					\
    set;								\
    HANDLE_RECV_AFTER_;							\
    adv;								\
    HANDLE_RECV_NEXT_;							\
    return i;								\
  }
#define HANDLE_RECV_TMP_(cond, check, set, adv, after, ...)		\
  template<typename T>							\
  RAPIDJSON_ENABLEIF_RETURN(cond, (long))				\
    _recvVA(int i, bool allow_realloc, rapidjson::Document& doc,	\
	    __VA_ARGS__) {						\
    HANDLE_RECV_BEFORE_;						\
    HANDLE_RECV_CHECK_(check, typeid(T).name());			\
    set;								\
    HANDLE_RECV_AFTER_;							\
    adv;								\
    after;								\
    HANDLE_RECV_LAST_;							\
    return i;								\
  }									\
  template<typename T, typename... Args>				\
  RAPIDJSON_ENABLEIF_RETURN(cond, (long))				\
    _recvVA(int i, bool allow_realloc, rapidjson::Document& doc,	\
	    __VA_ARGS__, Args... args) {				\
    HANDLE_RECV_BEFORE_;						\
    HANDLE_RECV_CHECK_(check, typeid(T).name());			\
    set;								\
    HANDLE_RECV_AFTER_;							\
    adv;								\
    HANDLE_RECV_NEXT_;							\
    return i;								\
  }
  // TODO: Handle case of table arrays

  HANDLE_RECV_(doc[0].IsString(),
	       long ret = utils::copyData(data, len, doc[0].GetString(),
					  static_cast<size_t>(doc[0].GetStringLength()),
					  allow_realloc);
	       if (ret < 0) { return -1; } len = static_cast<size_t>(ret),
	       i++, "char*",
	       if (i == 2 && doc.Size() == 0) { return ret; },
	       char*& data, size_t& len)
  HANDLE_RECV_((i >= 0),
	       data.CopyFrom(doc[0], data.GetAllocator(), true),
	       , "rapidjson::Document",
	       if (i == 1 && (doc.Size() > 0 || was_array)) {
		 rapidjson::Value tmp;
		 data.Swap(tmp);
		 data.SetArray();
		 data.Reserve(doc.Size() + 1, data.GetAllocator());
		 data.PushBack(tmp, data.GetAllocator());
		 while (doc.Size() > 0) {
		   tmp.CopyFrom(doc[0], data.GetAllocator(), true);
		   data.PushBack(tmp, data.GetAllocator());
		   doc.Erase(doc.Begin());
		 }
	       }
	       ,rapidjson::Document& data)
// #endif // WRAP_RAPIDJSON_FOR_DLL
  HANDLE_RECV_TMP_((internal::NotExpr<
		    internal::OrExpr<YGGDRASIL_IS_ANY_SCALAR(T),
		    internal::OrExpr<internal::IsSame<T, char*>,
		    internal::OrExpr<internal::IsSame<T, char[]>, 
		    internal::OrExpr<internal::IsSame<T, rapidjson::Document>,
		    internal::IsPointer<T> > > > > >),
		   doc[0].Is<T>(),
		   doc[0].Get(data);
		   if (i == 0 && doc.Size() == 1 &&
		       doc[0].IsString()) {
		     return static_cast<long>(doc[0].GetStringLength());
		   }, , ,
		   T& data)
  HANDLE_RECV_TMP_((internal::AndExpr<internal::IsPointer<T>,
		    internal::NotExpr<internal::IsSame<T, char*> > >),
		   doc[0].Is1DArray<std::remove_pointer<T>::type >(),
		   long ret = utils::copyData(data, len,
					      doc[0].GetString(),
					      static_cast<size_t>(doc[0].GetStringLength()),
					      allow_realloc);
		   if (ret < 0) { return -1; } len = static_cast<size_t>(doc[0].GetNElements()),
		   i++, , T& data, size_t& len)
  HANDLE_RECV_TMP_((internal::AndExpr<internal::IsPointer<T>,
		    internal::NotExpr<internal::IsSame<T, char*> > >),
		   doc[0].IsNDArray<std::remove_pointer<T>::type >(),
		   size_t len = 0;
		   if (ndim > 0 && shape) {
		     len = 1;
		     for (size_t ii = 0; ii < ndim; ii++) {
		       len *= shape[ii];
		     }
		   }
		   if (utils::copyData(data, len,
				       doc[0].GetString(),
				       static_cast<size_t>(doc[0].GetStringLength()),
				       allow_realloc) < 0) { return -1; }
		   const rapidjson::Value& doc_shape = doc[0].GetShape();
		   if (utils::copyData(shape, ndim * sizeof(size_t),
				       (size_t*)NULL,
				       static_cast<size_t>(doc_shape.Size()) * sizeof(size_t),
				       allow_realloc) < 0) { return -1; }
		   ndim = static_cast<size_t>(doc_shape.Size());
		   for (size_t ii = 0; ii < ndim; ii++) {
		     shape[ii] = static_cast<size_t>(doc_shape[static_cast<rapidjson::SizeType>(ii)].GetUint());
		   },
		   i++, , T& data, size_t& ndim, size_t*& shape)
  HANDLE_RECV_TMP_((YGGDRASIL_IS_ANY_SCALAR(T)),
		   doc[0].IsScalar<T>(),
		   doc[0].GetScalarValue(data),
		   , , T& data)
    
					      
#undef HANDLE_RECV_BEFORE_
#undef HANDLE_RECV_AFTER_
#undef HANDLE_RECV_CHECK_
#undef HANDLE_RECV_NEXT_
#undef HANDLE_RECV_LAST_
#undef HANDLE_RECV_
#undef HANDLE_RECV_TMP_
#endif // DOXYGEN_SHOULD_SKIP_THIS
public:
    /*!
      @brief Receive a raw string message from the communicator.
      @param[out] data Allocated buffer where the message should be saved.
      @param[in] len Length of the allocated message buffer in bytes.
      @param[in] allow_realloc If true, data will be reallocated if the
        message is larger than len. If false, an error will be raised if
	the message is larger than len.
      @returns -1 if message could not be received. Length of the
        received message if message was received.
    */
    long recv(char*& data, const size_t &len,
	      bool allow_realloc=false);
    /*!
      @brief Receive a string message from the communicator.
      @tparam N The size of the message.
      @param[out] data Allocated buffer where the message should be saved.
      @returns -1 if message could not be received. Length of the
        received message if message was received.
    */
    template<size_t N>
    long recv(char(& data)[N]) {
      size_t len = N;
      char* ptr = &(data[0]);
      return recv(ptr, len, false);
    }
    /*!
      @brief Receive a string message from the communicator.
      @param[out] data String to store message in.
      @returns -1 if message could not be received. Length of the
        received message if message was received.
     */
    long recv(std::string& data) {
      return recvVar(data);
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
    long call(const rapidjson::Document& sendData,
		      rapidjson::Document& recvData);
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
			 rapidjson::Document& recvData) {
      return call(sendData, recvData);
    }
  
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
      @brief Set the communicator type code.
      @param[in] new_type New communicator type code.
     */
    void setType(COMM_TYPE new_type) { type = new_type; }
    /*!
      @brief Determine if the communicator is valid.
      @return true if it is valid, false otherwise.
     */
    bool valid() const { return flags & COMM_FLAG_VALID; }
    /*!
      @brief Determine if the communicator is global.
      @return true if it is global, false otherwise.
     */
    bool global() const { return flags & COMM_FLAG_GLOBAL; }
    /*!
      @brief Determine if the communicator is async.
      @return true if it is async, false otherwise.
     */
    bool async() const { return flags & COMM_FLAG_ASYNC; }
    /*!
      @brief Get the Metadata object containing header information about
        the comm including datatype.
      @param[in] dir Direction to get metadata for.
      @return Metadata.
     */
    virtual YggInterface::utils::Metadata& getMetadata(const DIRECTION dir=NONE);
    /*!
      @brief Get a constant reference to the Metadata object containing
        header information about the comm including datatype.
      @param[in] dir Direction to get metadata for.
      @return Metadata.
     */
    const YggInterface::utils::Metadata&  getMetadata(const DIRECTION dir=NONE) const {
      return const_cast<Comm_t*>(this)->getMetadata(dir);
    }
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
    //! \copydoc YggInterface::utils::LogBase::logClass
    std::string logClass() const override {
      return COMM_TYPE_cls_map.find(getCommType())->second;
    }
    //! \copydoc YggInterface::utils::LogBase::logInst
    std::string logInst() const override {
      std::string out = name + "-" +
	DIRECTION_map.find(getDirection())->second;
      if (flags & COMM_FLAG_CLIENT_RESPONSE)
	out += "-CLIRES";
      else if (flags & COMM_FLAG_SERVER_RESPONSE)
	out += "-SRVRES";
      return out;
    }
    /*!
      @brief Get the communicator's address.
      @returns Address.
     */
    std::string getAddress() const {
        if (address.valid())
            return address.address();
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
      @brief Get the language associated with the communicator.
      @returns Language code.
    */
    LANGUAGE getLanguage() const { return language; }
    /*!
      @brief Set the communicator language code.
      @param[in] new_lang New communicator language code.
      @returns false if there is an error, true otherwise.
     */
    bool setLanguage(LANGUAGE new_lang=NO_LANGUAGE) {
      if (new_lang == NO_LANGUAGE) {
	char* model_language = std::getenv("YGG_MODEL_LANGUAGE");
	if (model_language) {
	  if (!enum_value_search(LANGUAGE_map,
				 std::string(model_language),
				 new_lang))
	    return false;
	} else {
	  new_lang = CXX_LANGUAGE;
	}
      }
      if (new_lang != NO_LANGUAGE)
	language = new_lang;
      return true;
    }
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
    /*!
      @brief Get the default communicator type for this class
      @returns Enumerated communicator type
    */
    static COMM_TYPE defaultCommType() { return DEFAULT_COMM; }

    /*!
      @brief Get the list of worker comms used for large messages.
      @returns Workers.
     */
    virtual WorkerList& getWorkers() { return workers; }
    /*!
     * Add a schema from the given metadata
     * @param s The Metadata object to use
     * @param dir Direction of comm to set the schema for (RPC only).
     * @return true if successful, false otherwise.
     */
    bool addSchema(const utils::Metadata& s, const DIRECTION dir=NONE);
    /*!
     * Add a schema based on the Value given
     * @param s The value to use
     * @param isMetadata If true, then the data is for the metadata section
     * @param dir Direction of comm to set the schema for (RPC only).
     * @return true if successful, false otherwise.
     */
    bool addSchema(const rapidjson::Value& s, bool isMetadata = false,
		   const DIRECTION dir=NONE);
    /*!
     * Add a schema based on the given string
     * @param schemaStr String representation of schema to use
     * @param isMetadata If true, then the data is for the metadata section
     * @param dir Direction of comm to set the schema for (RPC only).
     * @return true if successful, false otherwise.
     */
    bool addSchema(const std::string& schemaStr, bool isMetadata = false,
		   const DIRECTION dir=NONE);
    /*!
     * Get metadata based on the format string
     * @param format_str The format to use
     * @param as_array If true, then set the internal type to an array
     * @param field_names Names of fields described in format_str.
     * @param field_units Units of fields described in format_str.
     * @param dir Direction of comm to set the schema for (RPC only).
     * @return true if successful, false otherwise.
     */
    bool addFormat(const std::string& format_str, bool as_array = false,
		   const std::vector<std::string>& field_names = {},
		   const std::vector<std::string>& field_units = {},
		   const DIRECTION dir=NONE);
    /*!
     * @brief Copy a schema from another communicator
     * @param other THe communicator to use
     * @param dir Direction of comm to set the schema for (RPC only).
     * @return true if successful, false otherwise.
     */
    bool copySchema(const Comm_t* other, const DIRECTION dir=NONE);
    /*!
     * @brief Set the filters used to select messages. This removes any
     *   existing filters
     * @tparam T Type of filters
     * @param[in] new_filters Set of filters to use
     * @param[in] dir Direction of messages to set filters for (RPC only)
     * @return true if successful, false otherwise
     */
    template<typename T>
    bool setFilters(const std::vector<T>& new_filters,
		    const DIRECTION dir=NONE) {
      return getMetadata(dir).setFilters(new_filters);
    }
    /*!
     * @brief Set the transforms used to select messages. This removes
     *   any existing transforms
     * @tparam T Type of transforms
     * @param[in] new_transforms Set of transforms to use
     * @param[in] dir Direction of messages to set transforms for
     *   (RPC only)
     * @return true if successful, false otherwise
     */
    template<typename T>
    bool setTransforms(const std::vector<T>& new_transforms,
		       const DIRECTION dir=NONE) {
      return getMetadata(dir).setTransforms(new_transforms);
    }

private:
    // /*!
    //  * @brief Deserialize the given buffer into the argument list
    //  * @param[out] buf The buffer to deserialize
    //  * @param[in] ap The list to deserialize in to
    //  * @return 0 on success
    //  */
    // int deserialize(const char* buf, rapidjson::VarArgList& ap);
    // /*!
    //  * @brief Serialize the arg list into the buffer
    //  * @param[out] buf The buffer to fill
    //  * @param[in, out] buf_siz The initial size of buf
    //  * @param[in] ap The argument list to serialize
    //  * @return The size of buf
    //  */
    // int serialize(char*& buf, size_t& buf_siz,
    // 		  rapidjson::VarArgList& ap);
    
protected:

    /*!
     * @brief Initialize the base class for this instance
     */
    void init_base();
    /*!
     * @brief Initialize this instance
     */
    void init() {
      if (flags & COMM_FLAG_SET_OPP_ENV)
	setOppEnv();
    }

    friend CommContext;  //!< @see CommContext
    friend AsyncComm;    //!< @see AsyncComm
    friend AsyncBacklog; //!< @see AsyncBacklog
    friend RPCComm;      //!< @see RPCComm
    friend ServerComm;   //!< @see ServerComm
    friend ClientComm;   //!< @see ClientComm
    friend IPCComm;      //!< @see IPCComm
    friend ZMQComm;      //!< @see ZMQComm
    friend WrapComm;     //!< @see WrapComm
    friend RequestList;  //!< @see RequestList
    friend Worker;       //!< @see Worker
    friend WorkerList;   //!< @see WorkerList

    /**
     * @brief Perform class specific close operations
     * @param[in] call_base If true, the base class's _close method will
     *   be called
     */
    void _close(bool call_base) {
      UNUSED(call_base);
    }
    /**
     * @brief Class specific check for if the comm is closed
     * @return true if the comm is closed, false otherwise
     */
    bool _is_closed() const { return true; };
    
    /**
     * @brief Change the maximum message size
     * @param[in] new_size The new maximum message size
     */
    void updateMaxMsgSize(size_t new_size) {
        if (maxMsgSize == 0 || new_size < maxMsgSize)
            maxMsgSize = new_size;
    }
    /*!
     * @brief Change the message buffer size
     * @param[in] new_size The new buffer size
     */
    void updateMsgBufSize(size_t new_size) {
      msgBufSize = new_size;
    }

    /*!
     * @brief Set the flags for the givern header
     * @param[in] head The header to set the flags on
     * @param[in] dir The communication direction
     * @see utils::Header
     */
    void setFlags(const utils::Header& head, DIRECTION dir) {
      if (dir == SEND)
	flags |= COMM_FLAG_USED_SENT;
      else
	flags |= COMM_FLAG_USED_RECV;
      if (head.flags & HEAD_FLAG_EOF) {
	if (dir == SEND) {
	  flags |= COMM_FLAG_EOF_SENT;
	  if (flags & COMM_FLAG_CLOSE_ON_EOF_SEND)
	    close();
	} else {
	  flags |= COMM_FLAG_EOF_RECV;
	  if (flags & COMM_FLAG_CLOSE_ON_EOF_RECV)
	    close();
	}
      }
    }

    /*!
     * @brief Get the name of the environment variable that would be used
     *   to store an address for an interface comm during an integration.
     * @param[in] name Communicator name.
     * @param[in] dir Direction of the communicator.
     * @param[in] opp If true, get the environment variable for the
     *   opposing communicator.
     * @return Environment variable name.
     */
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
    /*!
     * @brief Set the environment variables for the opposing comm.
     */
    void setOppEnv() {
      if (address.valid()) {
	std::string opp_name = envName(name, direction, true);
	log_debug() << "setOppEnv: " << opp_name << " = " << getAddress() << std::endl;
	setenv(opp_name.c_str(), getAddress().c_str(), 1);
      }
    }
    /*!
     * @brief Unset the environment variables for the opposing comm.
     */
    void unsetOppEnv() {
      log_debug() << "unsetOppEnv" << std::endl;
      std::string opp_name = envName(name, direction, true);
      unsetenv(opp_name.c_str());
    }

  public:
    /*!
     * @brief Get the address string from the environment based on the given name
     *
     * addressFromEnv("YGG", SEND) will look for YGG_OUT in the environment
     * addressFromEnf("YGG", RECV) will look for YGG_IN in the environment
     * @param[in] name The base name to search for in the environment
     * @param[in] direction The communication direction
     * @return The new address
     * @see utils::Address
     */
    static utils::Address addressFromEnv(const std::string& name,
					 DIRECTION direction) {
      utils::Address out;
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
	addr = std::getenv(temp_name.c_str());
      }
      std::string addr_str = "null";
      if (addr)
	addr_str.assign(addr);
      YggLogDebug << "CommBase::addressFromEnv: full_name = " <<
	full_name << ", address = " << addr_str << std::endl;
      YggLogDebug << std::endl;
      if (addr)
	out.address(addr);
      return out;
    }
  protected:

    // /*!
    //  * @brief Update the metadata from the given schems
    //  * @param[in] new_schema The schema to update from
    //  * @param[in] dir The communication direction
    //  * @return Always returns 1
    //  */
    // int update_datatype(const rapidjson::Value& new_schema,
    //                     const DIRECTION dir);
    /*!
     * @brief Clear the given data
     * @tparam T Template data type
     * @param[in] data The data to clear
     */
    template<typename T>
    void zeroData(const T* data,
		  RAPIDJSON_ENABLEIF((internal::OrExpr<YGGDRASIL_IS_ANY_SCALAR(T), internal::IsSame<T, bool> >))) {
      memset(const_cast<T*>(data), 0, sizeof(T));
    }
    /*!
     * @brief Stub for constant data, which cannot be cleared.
     * @tparam T
     */
    template<typename T>
    void zeroData(const T*,
		  RAPIDJSON_DISABLEIF((internal::OrExpr<YGGDRASIL_IS_ANY_SCALAR(T),
                  internal::IsSame<T, bool> >))) {}
    /*
     * @brief If dir is RECV, then clear the data, if dir is SEND then copy the data into the document
     * @tparam T The template data type
     * @param[in] data The data to work with
     * @param[in] dir The communication direction
     * @return true if successful
     */
    // template<typename T>
    // bool checkType(const T& data, const DIRECTION dir) {
    //   YggInterface::utils::Metadata& meta = getMetadata(dir);
    //   if (dir == RECV)
    // 	zeroData(&data);
    //   if (dir == SEND && meta.transforms.size() > 0)
    // 	return true;
    //   return meta.fromData(data);
    // }
    /*!
     * @brief Create a header for a reply
     * @param[out] header Variable that new header should be stored in
     * @return true if successful
     * @see utils::Header
     */
    virtual bool create_header_send(utils::Header& header) {
      UNUSED(header);
      return true;
    }
    /*!
     * @brief Get the schema for the given communications direction
     * @param[in] dir The communications direction
     * @return The schema
     */
    rapidjson::Value* getSchema(const DIRECTION dir=NONE) {
      return getMetadata(dir).getSchema();
    }
    /*!
     * @brief Create a new communications worker
     * @param[in] address The address for the communicator
     * @param[in] dir The communications direction
     * @param[in] flgs Flags for the worker settings
     * @return The new worker
     * @see utils::Address
     */
    virtual Comm_t* create_worker(utils::Address& address,
                                  const DIRECTION dir, int flgs) VIRT_END;
    /*!
     * @brief Create a worker for sending
     * @param[in] head The header to use
     * @return The worker
     * @see utils::Header
     */
    virtual Comm_t* create_worker_send(utils::Header& head);
    /*!
     * @brief Create a worker for receiving
     * @param[in] head The header to use
     * @return The worker
     * @see utils::Header
     */
    virtual Comm_t* create_worker_recv(utils::Header& head);
    /**
     * @brief Sending function
     * @param[in] header Instance containing message and header.
     * @return The length of data sent.
     * @see utils::Header
     */
    virtual int send_single(utils::Header& header) VIRT_END;
    /**
     * @brief Receiving function
     * @param[in] header Instance to store message and header in.
     * @return The length of data received.
     * @see utils::Header
     */
    virtual long recv_single(utils::Header& header) VIRT_END;

    /**
     * @brief Constructor, which can only be instantiated by a child class
     * @param[in] name The name of communicator
     * @param[in] address The address to associate with this communicator.
     * @param[in] direction Whether this instance is a sender or receiver
     * @param[in] t Enumerated communicator type
     * @param[in] flgs Initial bitwise flags
     * @see utils::Address
     */
    explicit Comm_t(const std::string &name,
		    const utils::Address &address,
		    const DIRECTION direction = NONE,
		    const COMM_TYPE &t = NULL_COMM, int flgs = 0);

    /*!
     * @brief Constructor
     * @param[in] name The name of communicator
     * @param[in] dir Whether this instance is a sender or receiver
     * @param[in] t Enumerated communicator type
     * @param[in] flgs Initial bitwise flags
     */
    explicit Comm_t(const std::string& name, const DIRECTION dir = NONE,
                    const COMM_TYPE &t = NULL_COMM, int flgs = 0);
    /**
     * @brief Checks the size of the message to see if it exceeds the maximum allowable size as define by YGG_MSG_MAX
     * @param[in] len The length of the message to check
     * @return bool Whether the message is smaller than YGG_MSG_MAX (true), false otherwise
     */
    bool check_size(const size_t &len) const;

    std::shared_ptr<CommContext> ctx; //!< Context.
    COMM_TYPE type;                //!< Comm type.
    //void *other; //!< Pointer to additional information for the comm.
    std::string name;              //!< Comm name.
    utils::Address address;        //!< Comm address.
    DIRECTION direction;           //!< send or recv for direction messages will go.
    int flags;                     //!< Flags describing the status of the comm.
    size_t maxMsgSize;             //!< The maximum message size.
    size_t msgBufSize;             //!< The size that should be reserved in messages.
    int index_in_register;         //!< Index of the comm in the comm register.
    std::string thread_id;         //!< ID for the thread that created the comm.
    utils::Metadata metadata;      //!< Metadata for the communcator
    int64_t timeout_recv;          //!< Time to wait for messages during receive.
    WorkerList workers;            //!< Communicator to use for sending large messages.
    Comm_t* global_comm;           //!< Pointer to global comm that this comm shadows.
    LANGUAGE language;             //!< Language that model is written in
    std::vector<std::string> cache; //!< Cache of messages received.
    
    /*!
     * @brief Create a global communicator based on environment
     *   variables: YGG_SERVER_INPUT, YGG_SERVER_OUTPUT, YGG_MODEL_NAME
     * @return true on success
     */
    bool create_global_scope_comm();
    
public:

    // Methods for testing
    /*!
     * @brief Perform post send/recv tasks for testing.
     * @param[in,out] sComm Send communicator.
     * @param[in,out] rComm Receive communicator.
     * @return true on success.
     */
    virtual bool afterSendRecv(Comm_t* sComm, Comm_t* rComm) {
      UNUSED(sComm);
      UNUSED(rComm);
      return true;
    }
    /*!
     * @brief Generate metadata for a test message.
     * @param[out] out String that metadata should be stored in.
     * @return true on success.
     */
    virtual bool genMetadata(std::string& out) {
      UNUSED(out);
      return true;
    }
    /*!
     * @brief Get the global communicator.
     * @return Global comm.
     */
    Comm_t* getGlobalComm() { return global_comm; }
    /*!
     * @brief Create a test header.
     * @param[out] header Destination header.
     * @return true on success
     */
    bool create_header_test(utils::Header& header) {
      return create_header_send(header);
    }

};

/**
 * @brief Creates a new communicator of the specified type
 * @param[in] dir The direction for the communicator
 * @param[in] type The enumerated type of communicator to create
 * @param[in] name The name of the communicator
 * @param[in] address The initial address of the communicator.
 * @param[in] flags Bitwise flags describing the communicator
 * @param[in] ncomm Number of communicators to create.
 * @param[in] request_commtype Communicator type for RPC request comm.
 * @param[in] response_commtype Communicator type for RPC response comm.
 * @param[in] request_flags Communicator flags for RPC request comm.
 * @param[in] response_flags Communicator flags for RPC response comm.
 * @return The new communicator instance
 */
YGG_API Comm_t* new_Comm_t(const DIRECTION dir, const COMM_TYPE type,
			   const std::string &name="",
			   char* address=nullptr, int flags=0,
			   size_t ncomm=0,
			   const COMM_TYPE request_commtype=DEFAULT_COMM,
			   const COMM_TYPE response_commtype=DEFAULT_COMM,
			   int request_flags=0, int response_flags=0);
/**
 * @brief Creates a new communicator of the specified type
 * @param[in] dir The direction for the communicator
 * @param[in] type The enumerated type of communicator to create
 * @param[in] name The name of the communicator
 * @param[in] address The initial address of the communicator.
 * @param[in] flags Bitwise flags describing the communicator
 * @param[in] ncomm Number of communicators to create.
 * @param[in] request_commtype Communicator type for RPC request comm.
 * @param[in] response_commtype Communicator type for RPC response comm.
 * @param[in] request_flags Communicator flags for RPC request comm.
 * @param[in] response_flags Communicator flags for RPC response comm.
 * @return The new communicator instance
 * @see utils::Address
 */
YGG_API Comm_t* new_Comm_t(const DIRECTION dir, const COMM_TYPE type,
			   const std::string &name,
			   const utils::Address& address, int flags=0,
			   size_t ncomm=0,
			   const COMM_TYPE request_commtype=DEFAULT_COMM,
			   const COMM_TYPE response_commtype=DEFAULT_COMM,
			   int request_flags=0, int response_flags=0);
/**
 * @brief Creates a new communicator of the specified type
 * @param[in] dir The direction for the communicator
 * @param[in] type The enumerated type of communicator to create
 * @param[in] name The name of the communicator
 * @param[in] flags Bitwise flags describing the communicator
 * @param[in] ncomm Number of communicators to create.
 * @param[in] request_commtype Communicator type for RPC request comm.
 * @param[in] response_commtype Communicator type for RPC response comm.
 * @param[in] request_flags Communicator flags for RPC request comm.
 * @param[in] response_flags Communicator flags for RPC response comm.
 * @return The new communicator instance
 */
YGG_API Comm_t* new_Comm_t(const DIRECTION dir, const COMM_TYPE type,
			   const std::string &name, int flags=0,
			   size_t ncomm=0,
			   const COMM_TYPE request_commtype=DEFAULT_COMM,
			   const COMM_TYPE response_commtype=DEFAULT_COMM,
			   int request_flags=0, int response_flags=0);
  
/**
 * @brief Determine if a communicator type is installed.
 * @param[in] type The communicator type to check.
 * @return true if the communicator type is installed, false otherwise.
 */
YGG_API bool is_commtype_installed(const COMM_TYPE type);

/**
 * @brief Templated base class for all communicators
 * @tparam H Handle type for the communicator
 */
template<typename H>
class CommBase : public Comm_t {
public:
    CommBase(const CommBase& other) = delete;
    CommBase& operator=(const CommBase&) = delete;
    CommBase() = delete;

    /*! \copydoc YggInterface::communicator::Comm_t::comm_nmsg */
    int comm_nmsg(DIRECTION dir=NONE) const override {
      UNUSED(dir);
      log_error() << "Comm_nmsg of base class called, must be overridden" << std::endl;
      return -1;
    }
    ADD_DESTRUCTOR(CommBase, Comm_t);

    /*! \copydoc YggInterface::communicator::Comm_t::is_closed */
    bool is_closed() const override;

    using Comm_t::send;
    using Comm_t::recv;

protected:

    /** \copydoc YggInterface::communicator::Comm_t::_is_closed */
    bool _is_closed() const;
  
    /**
     * @brief Send a single message with this communicator
     * @param[in] header The header to use
     * @return The size of the sent message
     * @see utils::Header
     */
    int send_single(utils::Header& header) override {
      UNUSED(header);
      log_error() << "Send of base class called, must be overridden" << std::endl;
      return -1;
    }

    /**
     * @brief Receive a single message
     * @param[in] header The header to put the message in.
     * @return The size of the message
     * @see utils::Header
     */
    long recv_single(utils::Header& header) override {
      UNUSED(header);
      log_error() << "Recv of base class called, must be overridden" << std::endl;
      return -1;
    }

    /**
     * @brief Constructor
     * @param[in] name The name of the communicator
     * @param[in] address The initial address for the communicator
     * @param[in] direction The enumerated direction of the communicator
     * @param[in] t The enumerated type of the communicator
     * @param[in] flags Bitwise flags describing the communicator
     * @see utils::Address
     */
    explicit CommBase(const std::string &name, const utils::Address& address,
		      const DIRECTION direction = NONE,
		      const COMM_TYPE &t = NULL_COMM, int flags = 0);

    /*!
     * @brief Constructor
     * @param[in] name The name of the communicator
     * @param[in] dir The enumerated direction of the communicator
     * @param[in] t The enumerated type of the communicator
     * @param[in] flgs Bitwise flags describing the communicator
     */
    explicit CommBase(const std::string &name, const DIRECTION dir = NONE,
                      const COMM_TYPE &t = NULL_COMM, int flgs = 0);

    /*!
     * @brief Create a worker using the inputs
     * @param[in] addr The address to use for the worker
     * @param[in] dir The communication direction
     * @param[in] flgs Any flags for the worker
     * @return The communications worker
     * @see utils::Address
     */
    Comm_t* create_worker(utils::Address& addr, const DIRECTION dir,
                          int flgs) override {
  
        UNUSED(dir);
	UNUSED(addr);
	UNUSED(flgs);
        throw_error("create_worker of base class called, must be overridden");
        return nullptr; // GCOVR_EXCL_LINE
    }

    H *handle; //!< Pointer to handle for comm.

    // Test methods
public:
    /*!
     * @brief Get the handle
     * @return The handle
     */
    H* getHandle() {
        if (global_comm)
            return dynamic_cast<CommBase<H>*>(global_comm)->getHandle();
        return handle;
    }
    /*!
     * @brief Set the handle
     * @param[in] h The handle to use
     */
    void setHandle(H* h) {
        if (global_comm)
            dynamic_cast<CommBase<H>*>(global_comm)->setHandle(h);
        handle = h;
    }
};

template<typename H>
CommBase<H>::CommBase(const std::string &nme, const utils::Address &addr,
		      const DIRECTION dirn, const COMM_TYPE &t, int flgs) :
  Comm_t(nme, addr, dirn, t, flgs), handle(nullptr) {
  if (global_comm)
    handle = dynamic_cast<CommBase<H>*>(global_comm)->handle;
}

template<typename H>
CommBase<H>::CommBase(const std::string &nme, const DIRECTION dirn,
                      const COMM_TYPE &t, int flgs) :
  CommBase(nme, utils::blankAddress, dirn, t, flgs) {}

template<typename H>
bool CommBase<H>::is_closed() const {
  return _is_closed();
}

template<typename H>
bool CommBase<H>::_is_closed() const {
  return ((!((bool)(handle))) || !(flags & COMM_FLAG_VALID));
}

template<typename H>
void CommBase<H>::_close(bool call_base) {
  if (handle) {
    if (!global_comm)
      delete handle;
    handle = nullptr;
  }
  workers.workers.clear();
  if (call_base)
    Comm_t::_close(true);
}

ADD_DESTRUCTOR_DEF(CommBase, Comm_t, template<typename H>, <H>)

}
}
