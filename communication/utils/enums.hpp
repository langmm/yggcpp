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
    SERVER_COMM, //!< Server communicator
    CLIENT_COMM, //!< Client communicator
    MPI_COMM     //!< MPI based communicator
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
    COMM_FLAG_VALID  = 0x00000001,  //!< Set if the comm is initialized
    COMM_FLAG_GLOBAL = 0x00000002,  //!< Set if the comm is global
    COMM_FLAG_WORKER = 0x00000004,  //!< Set if the comm is a work comm
    COMM_FLAG_CLIENT = 0x00000008,  //!< Set if the comm is a client
    COMM_FLAG_SERVER = 0x00000010,  //!< Set if the comm is a server
    COMM_FLAG_CLIENT_RESPONSE = 0x00000020, //!< Set if the comm is a client response comm
    COMM_FLAG_SERVER_RESPONSE = 0x00000040, //!< Set if the comm is a server response comm
    COMM_ALWAYS_SEND_HEADER   = 0x00000080, //!< Set if the comm should always include a header in messages
    COMM_ALLOW_MULTIPLE_COMMS = 0x00000100, //!< Set if the comm should connect in a way that allow multiple connections
    COMM_FLAGS_USED_SENT = 0x00000200,  //!< Set if the comm has been used
    COMM_FLAGS_USED_RECV = 0x00000400,  //!< Set if the comm has been used
    COMM_EOF_SENT   = 0x00000800,  //!< Set if EOF has been sent
    COMM_EOF_RECV   = 0x00001000,  //!< Set if EOF has been received
    COMM_FLAG_INTERFACE = 0x00002000,       //!< Set if communicator is an interface communicator
    COMM_FLAG_DELETE    = 0x00004000,       //!< Set if the communicator needs to be deleted
    COMM_FLAG_ASYNC     = 0x00008000        //!< Set if the communicator is asynchronous
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
