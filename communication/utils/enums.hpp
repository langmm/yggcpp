#pragma once

#ifdef __cplusplus
#include <map>
#include <string>
#include <istream>
extern "C" {
#endif
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
    CLIENT_COMM  //!< Client communicator
};

/**
 * Communicator direction
 */
enum DIRECTION {
    SEND,  //!< Sending communicator
    NONE,  //!< No direction
    RECV   //!< Receiving communicator
};

/*! @brief Bit flags. */
enum CommFlags {
  COMM_FLAG_VALID           = 0x00000001, //!< Comm is initialized
  COMM_FLAG_GLOBAL          = 0x00000002, //!< Comm is global
  COMM_FLAG_WORKER          = 0x00000004, //!< Comm is a worker
  COMM_FLAG_CLIENT          = 0x00000008, //!< Comm is a client
  COMM_FLAG_SERVER          = 0x00000010, //!< Comm is a server
  COMM_FLAG_CLIENT_RESPONSE = 0x00000020, //!< Comm is a client response
  COMM_FLAG_SERVER_RESPONSE = 0x00000040, //!< Comm is a server response
  COMM_ALWAYS_SEND_HEADER   = 0x00000080, //!< Comm should always include a header in messages
  COMM_ALLOW_MULTIPLE_COMMS = 0x00000100, //!< Comm should connect in a way that allow multiple connections
  COMM_FLAGS_USED_SENT      = 0x00000200, //!< Comm has sent messages
  COMM_FLAGS_USED_RECV      = 0x00000400, //!< Comm has received messages
  COMM_EOF_SENT             = 0x00000800, //!< EOF has been sent
  COMM_EOF_RECV             = 0x00001000, //!< EOF has been received
  COMM_FLAG_INTERFACE       = 0x00002000, //!< Comm is an interface comm
  COMM_FLAG_DELETE          = 0x00004000, //!< Comm needs to be deleted
  COMM_FLAG_ASYNC           = 0x00008000, //!< Comm is asynchronous
  COMM_FLAG_ASYNC_WRAPPED   = 0x00010000  //!< Comm is wrapped by an asynchronous comm
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
