#ifndef __YGG_ENUMS_HPP
#define __YGG_ENUMS_HPP

#ifdef __cplusplus
extern "C" {
#include <cstdint>
#else
#include <stdint.h>
#endif

#define FLAG_TYPE int64_t
  
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
    REST_COMM,   //!< REST communicator
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

/*!
  @brief Bit flags describing the communicator.
  
  The maximum int64_t is 9223372036854775807 (0x7fffffffffffffff) so
  the maximum flag is 0x4000000000000000 (1 << 62)
  
  Generic flags can go up to 0x0000080000000000 (1 << 43)
  Type specific flags start at 0x0000100000000000 (1 << 44) and can go
    up to the maximum. Duplicates are allowed between types that are
    exclusive, but should not be allowed for aggregate classes

  Any time these values are updated, the corresponding values in
    fortran/YggInterface.F90 should be updated as well

*/
enum COMM_FLAG
#ifdef __cplusplus
: FLAG_TYPE
#endif // __cplusplus
  {
  COMM_FLAG_VALID           = 0x00000001LL, //!< Comm is initialized
  COMM_FLAG_GLOBAL          = 0x00000002LL, //!< Comm is global
  COMM_FLAG_WORKER          = 0x00000004LL, //!< Comm is a worker
  COMM_FLAG_DELAYED_OPEN    = 0x00000008LL, //!< Comm will not be opened when created
  COMM_FLAG_CLIENT          = 0x00000010LL, //!< Comm is a client
  COMM_FLAG_SERVER          = 0x00000020LL, //!< Comm is a server
  COMM_FLAG_CLIENT_RESPONSE = 0x00000040LL, //!< Comm is a client response
  COMM_FLAG_SERVER_RESPONSE = 0x00000080LL, //!< Comm is a server response
  COMM_FLAG_ALWAYS_SEND_HEADER   = 0x00000100LL, //!< Comm should always include a header in messages
  COMM_FLAG_ALLOW_MULTIPLE_COMMS = 0x00000200LL, //!< Comm should connect in a way that allow multiple connections
  COMM_FLAG_USED_SENT       = 0x00000400LL, //!< Comm has sent messages
  COMM_FLAG_USED_RECV       = 0x00000800LL, //!< Comm has received messages
  COMM_FLAG_EOF_SENT        = 0x00001000LL, //!< EOF has been sent
  COMM_FLAG_EOF_RECV        = 0x00002000LL, //!< EOF has been received
  COMM_FLAG_CLOSE_ON_EOF_RECV    = 0x00004000LL, //!< Comm will close on EOF recv
  COMM_FLAG_CLOSE_ON_EOF_SEND    = 0x00008000LL, //!< Comm will close on EOF recv
  COMM_FLAG_INTERFACE       = 0x00010000LL, //!< Comm is an interface comm
  COMM_FLAG_DELETE          = 0x00020000LL, //!< Comm needs to be deleted
  COMM_FLAG_ASYNC           = 0x00040000LL, //!< Comm is asynchronous
  COMM_FLAG_ASYNC_WRAPPED   = 0x00080000LL, //!< Comm is wrapped by an asynchronous comm
  COMM_FLAG_SET_OPP_ENV     = 0x00100000LL, //!< Set environment variables for opposite communicator
  COMM_FLAG_WRAPPER         = 0x00200000LL, //!< Communicator is a wrapper
  COMM_FLAG_FORK_CYCLE      = 0x00400000LL, //!< Forked communicator cycle
  COMM_FLAG_FORK_BROADCAST  = 0x00800000LL, //!< Forked communicator broadcast
  COMM_FLAG_FORK_COMPOSITE  = 0x01000000LL, //!< Forked communicator composite
  COMM_FLAG_FORK_TINE       = 0x02000000LL, //!< Forked communicator tine.
  // Type specific flags
  // File flags
  FILE_FLAG_APPEND          = 0x0000100000000000LL, //!< Append sent messages to the end of the file
  FILE_FLAG_BINARY          = 0x0000200000000000LL, //!< Open file in binary mode
  FILE_FLAG_READLINE        = 0x0000400000000000LL, //!< Read file contents line by line

  COMM_FLAG_MAX             = 0x4000000000000000LL
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

#ifdef __cplusplus
}
#endif

#endif // __YGG_ENUMS_HPP
