#pragma once

#ifdef __cplusplus
#include <map>
#include <string>
#include <istream>
#include <iostream>
#include <algorithm>
extern "C" {
#endif
  
// If any of these are updated they must be also be updated in
//   fortran/YggInterface.F90
/**
 * Enum for communicator types
 */
enum COMM_TYPE {
    NULL_COMM,   //!< No type
    DEFAULT_COMM,//!< Default communicator
    IPC_COMM,    //!< IPC based communicator
    ZMQ_COMM,    //!< ZeroMQ based communicator
    MPI_COMM,    //!< MPI based communicator
    SERVER_COMM, //!< Server communicator
    CLIENT_COMM, //!< Client communicator
    FILE_COMM,   //!< File communicator
    RMQ_COMM,    //!< RabbitMQ communicator
    VALUE_COMM,  //!< Value communicator
};

/**
 * Communicator direction
 */
enum DIRECTION {
    SEND,  //!< Sending communicator
    NONE,  //!< No direction
    RECV   //!< Receiving communicator
};

/**
 * Cleanup mode
 */
enum CLEANUP_MODE {
  CLEANUP_DEFAULT, //!< Cleanup everything including ZMQ/Python
  CLEANUP_ATEXIT,  //!< Cleanup during atexit call
  CLEANUP_COMMS    //!< Only cleanup comms, not ZMQ or Python
};

// If any of these are updated they must be also be updated in
//   fortran/YggInterface.F90
/*! @brief Bit flags. */
enum COMM_FLAG {
  COMM_FLAG_VALID           = 0x00000001, //!< Comm is initialized
  COMM_FLAG_GLOBAL          = 0x00000002, //!< Comm is global
  COMM_FLAG_WORKER          = 0x00000004, //!< Comm is a worker
  COMM_FLAG_CLIENT          = 0x00000008, //!< Comm is a client
  COMM_FLAG_SERVER          = 0x00000010, //!< Comm is a server
  COMM_FLAG_CLIENT_RESPONSE = 0x00000020, //!< Comm is a client response
  COMM_FLAG_SERVER_RESPONSE = 0x00000040, //!< Comm is a server response
  COMM_FLAG_ALWAYS_SEND_HEADER   = 0x00000080, //!< Comm should always include a header in messages
  COMM_FLAG_ALLOW_MULTIPLE_COMMS = 0x00000100, //!< Comm should connect in a way that allow multiple connections
  COMM_FLAG_USED_SENT       = 0x00000200, //!< Comm has sent messages
  COMM_FLAG_USED_RECV       = 0x00000400, //!< Comm has received messages
  COMM_FLAG_EOF_SENT        = 0x00000800, //!< EOF has been sent
  COMM_FLAG_EOF_RECV        = 0x00001000, //!< EOF has been received
  COMM_FLAG_CLOSE_ON_EOF_RECV    = 0x00002000, //!< Comm will close on EOF recv
  COMM_FLAG_CLOSE_ON_EOF_SEND    = 0x00004000, //!< Comm will close on EOF recv
  COMM_FLAG_INTERFACE       = 0x00008000, //!< Comm is an interface comm
  COMM_FLAG_DELETE          = 0x00010000, //!< Comm needs to be deleted
  COMM_FLAG_ASYNC           = 0x00020000, //!< Comm is asynchronous
  COMM_FLAG_ASYNC_WRAPPED   = 0x00040000, //!< Comm is wrapped by an asynchronous comm
  COMM_FLAG_SET_OPP_ENV     = 0x00080000, //!< Set environment variables for opposite communicator
  COMM_FLAG_WRAPPER         = 0x00100000, //!< Communicator is a wrapper
  COMM_FLAG_FORK_CYCLE      = 0x00200000, //!< Forked communicator cycle
  COMM_FLAG_FORK_BROADCAST  = 0x00400000, //!< Forked communicator broadcast
  COMM_FLAG_FORK_COMPOSITE  = 0x00800000, //!< Forked communicator composite
  COMM_FLAG_FORK_TINE       = 0x01000000, //!< Forked communicator tine.
  FILE_FLAG_APPEND          = 0x02000000, //!< Append sent messages to the end of the file
  FILE_FLAG_BINARY          = 0x04000000, //!< Open file in binary mode
  FILE_FLAG_READLINE        = 0x08000000  //!< Read file contents line by line
};

enum LANGUAGE {
  NO_LANGUAGE,       //!< No explicit language interface
  CXX_LANGUAGE,      //!< Interfaced being accesed from CXX
  C_LANGUAGE,        //!< Interfaced being accesed from C
  FORTRAN_LANGUAGE,  //!< Interfaced being accesed from Fortran
  PYTHON_LANGUAGE,   //!< Interfaced being accesed from Python
  MATLAB_LANGUAGE,   //!< Interfaced being accesed from MATLAB
  R_LANGUAGE,        //!< Interfaced being accesed from R
  JULIA_LANGUAGE,    //!< Interfaced being accesed from Julia
  JAVA_LANGUAGE,     //!< Interfaced being accesed from Java
};
  
/*! @brief Bit flags describing message state. */
enum HeadFlags {
  HEAD_FLAG_VALID           = 0x00000001, //!< Message is valid
  HEAD_FLAG_MULTIPART       = 0x00000002, //!< Message is multipart
  HEAD_META_IN_DATA         = 0x00000004, //!< Type in message body
  HEAD_AS_ARRAY             = 0x00000008, //!< Message is an array
  HEAD_FLAG_OWNSDATA        = 0x00000010, //!< Header frees the buffer
  HEAD_FLAG_ALLOW_REALLOC   = 0x00000020, //!< Buffer can be reallocated
  HEAD_TEMPORARY            = 0x00000040, //!< Message is temporary
  HEAD_FLAG_EOF             = 0x00000080, //!< Message is EOF
  HEAD_FLAG_CLIENT_EOF      = 0x00000100, //!< Message is client EOF
  HEAD_FLAG_CLIENT_SIGNON   = 0x00000200, //!< Message is client signon
  HEAD_FLAG_SERVER_SIGNON   = 0x00000400, //!< Message is server signon
  HEAD_FLAG_REPEAT          = 0x00000800, //!< Message will be discarded
  HEAD_FLAG_FORMATTED       = 0x00001000, //!< Message is formatted
  HEAD_FLAG_NO_TYPE         = 0x00002000, //!< No type info in message
  HEAD_FLAG_NO_HEAD         = 0x00004000, //!< No header in message
  HEAD_FLAG_ASYNC           = 0x00008000  //!< Header formated by async
};
const int HEAD_BUFFER_MASK = (HEAD_FLAG_ALLOW_REALLOC |
			      HEAD_FLAG_OWNSDATA);

enum HEAD_RESET_MODE {
  HEAD_RESET_COMPLETE,
  HEAD_RESET_KEEP_BUFFER,
  HEAD_RESET_OWN_DATA,
  HEAD_RESET_DROP_DATA
};

enum SIGNON_STATUS {
  SIGNON_NOT_SENT,
  SIGNON_ERROR,
  SIGNON_NOT_WAITING,
  SIGNON_WAITING,
  SIGNON_COMPLETE
};

enum THREAD_STATUS {
  THREAD_INACTIVE     = 0,
  THREAD_INIT         = 0x00000001,
  THREAD_STARTED      = 0x00000002,
  THREAD_COMPLETE     = 0x00000004,
  THREAD_ERROR        = 0x00000008,
  THREAD_SIGNON_SENT  = 0x00000010,
  THREAD_SIGNON_RECV  = 0x00000020,
  THREAD_HAS_RESPONSE = 0x00000040,
  THREAD_IS_CLIENT    = 0x00000080,
  THREAD_CLOSING      = 0x00000100
};

enum FORK_TYPE {
  FORK_DEFAULT,   //!< Default set based on direction
  FORK_CYCLE,     //!< Cycle through comms on each message
  FORK_BROADCAST, //!< [SEND ONLY] Send the same message to each comm
  FORK_COMPOSITE  //!< Part of message is sent/received to/from each comm
};

// enum DTYPE {
//     T_NULL, T_OBJECT, T_1DARRAY,
//     T_DIRECT, T_NDARRAY, T_SCALAR,
//     T_PLY, T_OBJ, T_GROUP, T_FORMATTED,
//     T_CLASS, T_FUNCTION, T_INSTANCE, T_SCHEMA, T_ANY, T_PLY_T, T_OBJ_T
// };

// enum SUBTYPE {
//     T_BOOLEAN, T_STRING,
//     T_FLOAT, T_UINT, T_INT, T_COMPLEX, T_BYTES, T_UNICODE
// };

// enum VTYPE {
//     T_SCALABLE, T_ARRAY1D
// };

#ifdef __cplusplus
}

const std::map<const DIRECTION, const std::string> DIRECTION_map {
  {SEND, "SEND"},
  {NONE, "NONE"},
  {RECV, "RECV"}};

const std::map<const COMM_TYPE, const std::string> COMM_TYPE_map {
  {NULL_COMM, "NULL"},
  {DEFAULT_COMM, "DEFAULT"},
  {IPC_COMM, "IPC"},
  {ZMQ_COMM, "ZMQ"},
  {MPI_COMM, "MPI"},
  {SERVER_COMM, "SERVER"},
  {CLIENT_COMM, "CLIENT"},
  {FILE_COMM, "FILE"},
  {RMQ_COMM, "RMQ"},
  {VALUE_COMM, "VALUE"}};

const std::map<const COMM_TYPE, const std::string> COMM_TYPE_cls_map {
  {NULL_COMM, "NullComm"},
  {DEFAULT_COMM, "DefaultComm"},
  {IPC_COMM, "IPCComm"},
  {ZMQ_COMM, "ZMQComm"},
  {MPI_COMM, "MPIComm"},
  {SERVER_COMM, "ServerComm"},
  {CLIENT_COMM, "ClientComm"},
  {FILE_COMM, "FileComm"},
  {RMQ_COMM, "RMQComm"},
  {VALUE_COMM, "ValueComm"}};

const std::map<const LANGUAGE, const std::string> LANGUAGE_map {
  {NO_LANGUAGE, ""},
  {CXX_LANGUAGE, "c"},
  {C_LANGUAGE, "cxx"},
  {FORTRAN_LANGUAGE, "fortran"},
  {PYTHON_LANGUAGE, "python"},
  {MATLAB_LANGUAGE, "matlab"},
  {R_LANGUAGE, "r"},
  {JULIA_LANGUAGE, "julia"},
  {JAVA_LANGUAGE, "java"}};

const std::map<const COMM_FLAG, const std::string> COMM_FLAG_map {
  {COMM_FLAG_VALID           , "VALID"},
  {COMM_FLAG_GLOBAL          , "GLOBAL"},
  {COMM_FLAG_WORKER          , "WORKER"},
  {COMM_FLAG_CLIENT          , "CLIENT"},
  {COMM_FLAG_SERVER          , "SERVER"},
  {COMM_FLAG_CLIENT_RESPONSE , "CLIENT_RESPONSE"},
  {COMM_FLAG_SERVER_RESPONSE , "SERVER_RESPONSE"},
  {COMM_FLAG_ALWAYS_SEND_HEADER   , "ALWAYS_SEND_HEADER"},
  {COMM_FLAG_ALLOW_MULTIPLE_COMMS , "ALLOW_MULTIPLE_COMMS"},
  {COMM_FLAG_USED_SENT       , "USED_SENT"},
  {COMM_FLAG_USED_RECV       , "USED_RECV"},
  {COMM_FLAG_EOF_SENT        , "EOF_SENT"},
  {COMM_FLAG_EOF_RECV        , "EOF_RECV"},
  {COMM_FLAG_CLOSE_ON_EOF_RECV    , "CLOSE_ON_EOF_RECV"},
  {COMM_FLAG_CLOSE_ON_EOF_SEND    , "CLOSE_ON_EOF_SEND"},
  {COMM_FLAG_INTERFACE       , "INTERFACE"},
  {COMM_FLAG_DELETE          , "DELETE"},
  {COMM_FLAG_ASYNC           , "ASYNC"},
  {COMM_FLAG_ASYNC_WRAPPED   , "ASYNC_WRAPPED"},
  {COMM_FLAG_SET_OPP_ENV     , "SET_OPP_ENV"},
  {COMM_FLAG_WRAPPER         , "WRAPPER"},
  {COMM_FLAG_FORK_CYCLE      , "FORK_CYCLE"},
  {COMM_FLAG_FORK_BROADCAST  , "FORK_BROADCAST"},
  {COMM_FLAG_FORK_COMPOSITE  , "FORK_COMPOSITE"},
  {COMM_FLAG_FORK_TINE       , "FORK_TINE"}};
const std::map<const COMM_FLAG, const std::string> FILE_FLAG_map {
  {FILE_FLAG_APPEND          , "APPEND"},
  {FILE_FLAG_BINARY          , "BINARY"},
  {FILE_FLAG_READLINE        , "READLINE"}};

static inline std::string str_toupper(const std::string& inStr) {
  std::string outStr(inStr);
  std::transform(inStr.begin(), inStr.end(), outStr.begin(),
		 [](unsigned char c) { return std::toupper(c); });
  return outStr;
}
static inline std::string str_tolower(const std::string& inStr) {
  std::string outStr(inStr);
  std::transform(inStr.begin(), inStr.end(), outStr.begin(),
		 [](unsigned char c) { return std::tolower(c); });
  return outStr;
}

template<typename T1>
static bool enum_value_search(const std::map<const T1, const std::string> map,
			      const std::string& val,
			      T1& key, bool allow_anycase=false,
			      std::string prefix="",
			      std::string suffix="") {
  for (typename std::map<const T1, const std::string>::const_iterator it = map.cbegin();
       it != map.cend(); it++) {
    if ((it->second == val) ||
	(allow_anycase && (val == str_toupper(it->second) ||
			   val == str_tolower(it->second))) ||
	((!prefix.empty()) && (val == (prefix + it->second))) ||
	((!suffix.empty()) && (val == (it->second + suffix)))) {
      key = it->first;
      return true;
    }
  }
  return false;
}

template<typename T1, typename T2>
static T1 max_enum_value(const std::map<const T1, const T2> map) {
  return map.crbegin()->first;
}

// const std::map<const std::string, const SUBTYPE> submap {{"int", T_INT},
//                                                          {"int8_t", T_INT},
//                                                          {"int16_t", T_INT},
//                                                          {"int32_t", T_INT},
//                                                          {"int64_t", T_INT},
//                                                          {"float", T_FLOAT},
//                                                          {"double", T_FLOAT},
//                                                          {"long double", T_FLOAT},
//                                                          {"ldouble", T_FLOAT},
//                                                          {"bool", T_BOOLEAN},
//                                                          {"string", T_STRING},
//                                                          {"complex_float_t", T_COMPLEX},
//                                                          {"complex_double_t", T_COMPLEX},
//                                                          {"complex_long_double_t", T_COMPLEX},
//                                                          {"uint", T_UINT},
//                                                          {"uint8_t", T_UINT},
//                                                          {"uint16_t", T_UINT},
//                                                          {"uint32_t", T_UINT},
//                                                          {"uint64_t", T_UINT},
//                                                          {"bytes", T_BYTES},
//                                                          {"unicode", T_UNICODE},
//                                                          {"uchar", T_UINT},
//                                                          {"char", T_INT},
//                                                          {"short", T_INT},
//                                                          {"ushort", T_UINT},
//                                                          {"long", T_INT},
//                                                          {"ulong", T_UINT}};

// const std::map<const SUBTYPE, const std::string> mapsub {{T_INT, "int"},
//                                                          {T_FLOAT, "float"},
//                                                          {T_BOOLEAN, "bool"},
//                                                          {T_STRING, "string"},
//                                                          {T_COMPLEX, "complex_float_t"},
//                                                          {T_UINT, "uint"},
//                                                          {T_BYTES, "bytes"},
//                                                          {T_UNICODE, "unicode"}};

// Currently unused
// static
// std::istream& operator>>(std::istream& in, SUBTYPE& type) {
//     int t;
//     in >> t;
//     type = static_cast<SUBTYPE>(t);
//     return in;
// }
// static
// std::istream& operator>>(std::istream& in, VTYPE& type) {
//     int t;
//     in >> t;
//     type = static_cast<VTYPE>(t);
//     return in;
// }

#endif
