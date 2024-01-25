#pragma once
#include "utils/tools.hpp"
#ifdef __cplusplus
#include <cstdlib>
#else
#include <stdbool.h>
#endif
#include "utils/enums.hpp"
#include "datatypes/dtype_t.h"

/*! @brief Define macros to allow counts of variables. */
// https://codecraft.co/2014/11/25/variadic-macros-tricks/
#ifdef _MSC_VER
// https://stackoverflow.com/questions/48710758/how-to-fix-variadic-macro-related-issues-with-macro-overloading-in-msvc-mic
#define MSVC_BUG(MACRO, ARGS) MACRO ARGS  // name to remind that bug fix is due to MSVC :-)
#define _GET_NTH_ARG_2(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, N, ...) N
#define _GET_NTH_ARG(...) MSVC_BUG(_GET_NTH_ARG_2, (__VA_ARGS__))
#define COUNT_VARARGS(...) _GET_NTH_ARG("ignored", ##__VA_ARGS__, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
#define VA_MACRO(MACRO, ...) MSVC_BUG(CONCATE, (MACRO, COUNT_VARARGS(__VA_ARGS__)))(__VA_ARGS__)
#else
#define _GET_NTH_ARG(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, N, ...) N
#define COUNT_VARARGS(...) _GET_NTH_ARG("ignored", ##__VA_ARGS__, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
#endif

#define CSafe(x, err)						\
  try								\
    {								\
      x;							\
    }								\
  catch(...)							\
    {								\
      ygglog_error << "C++ exception thrown." << std::endl;	\
    }

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Struct for holding a C++ class communicator as a void* for use in C
 */
typedef struct comm_t {
  void* comm; /**< Pointer to C++ communicator */
} comm_t;

  
// TODO: Allow use of ygglog_error as function in C
void ygglog_error(const char* fmt, ...);
void ygglog_debug(const char* fmt, ...);
void ygglog_info(const char* fmt, ...);

/**
 * Initialize yggdrasil interface.
 */
int ygg_init();

/**
 * Cleanup yggdrasil interface prior to exit.
 */
void ygg_exit();

/**
 * @brief Delete the underlying communicator
 * @param comm The communicator to delete
 */
void free_comm(comm_t* comm);

/**
 * @brief Close the underlying communicator
 * @param comm The communicator to close
 */
void close_comm(comm_t* comm);

//**
// * Open a new communicator of the given type
// * @param address The initial address of the communicator
// * @param dir The enumerated direction for the communicator
// * @param t The enumerated communicator type to create
// * @return comm_t struct containing the requested communicator
// */
// comm_t open_comm(char* address, DIRECTION dir, const COMM_TYPE &t);

/**
 * @brief Initialize a new communicator without interface flag set
 * @param name The name for the communicator
 * @param dir The enumerated direction of the communicator
 * @param t The enumerated communicator type to create
 * @param datatype Pointer to datatype that will be moved into the
 *   created communicator.
 * @param flags Bitwise flags describing properties the communicator
 *   should have.
 * @param ncomm Number of communicators in a forked comm.
 * @return comm_t struct containing the requested communicator
 */
comm_t _init_comm(const char* name, const enum DIRECTION dir,
		  const enum COMM_TYPE t,
		  dtype_t* datatype, const int flags,
		  const size_t ncomm);
  
/**
 * @brief Initialize a new communicator
 * @param name The name for the communicator
 * @param dir The enumerated direction of the communicator
 * @param t The enumerated communicator type to create
 * @param datatype Pointer to datatype that will be moved into the
 *   created communicator.
 * @return comm_t struct containing the requested communicator
 */
comm_t init_comm(const char* name, const enum DIRECTION dir,
		 const enum COMM_TYPE t,
		 dtype_t* datatype);

/**
 * @brief Initialize a new communicator with flags
 * @param name The name for the communicator
 * @param dir The enumerated direction of the communicator
 * @param t The enumerated communicator type to create
 * @param flags Bitwise flags describing the communicator.
 * @return comm_t struct containing the requested communicator
 */
comm_t init_comm_flags(const char* name, const enum DIRECTION dir,
		       const enum COMM_TYPE t, int flags);

/**
 * @brief Set a communicators language.
 * @param x Communicator
 * @param lang Language.
 * @return 1 if successful, 0 otherwise.
 */
int set_comm_language(comm_t x, const enum LANGUAGE lang);

  
/**
 * Set a communicators datatype based on a C-style format string.
 * @param comm Communicator
 * @param fmt C-style format string
 * @return 1 if successful, 0 otherwise.
 */
int set_response_format(comm_t comm, const char *fmt);
  
/**
 * @brief Set a communicators datatype.
 * @param x Communicator
 * @param datatype Pointer to datatype. The underlying data will be
 *   consumed.
 * @return 1 if successful, 0 otherwise.
 */
int set_response_datatype(comm_t x, dtype_t* datatype);

/**
 * @brief Get the datatype associated with a communicator.
 * @param x Communicator
 * @return The datatype
 */
dtype_t comm_get_datatype(comm_t x);

/**
 * @brief Set the datatype associated with a communicator.
 * @param x Communicator
 * @param datatype The datatype
 * @return 1 if successful, 0 otherwise.
 */
int comm_set_datatype(comm_t x, dtype_t* datatype);
  
/**
 * @brief Send a message with the given communicator
 * @param comm The communicator to use
 * @param data The message to send
 * @param len The size of data in bytes
 * @return Any value greater than 0 indicates success
 */
int comm_send(comm_t comm, const char *data, const size_t len);

/**
 * @brief Send an end-of-file notification on the given communicator
 * @param comm The communicator to use
 * @return Any value greater than 0 indicates success
 */
int comm_send_eof(comm_t comm);

/**
 * @brief Determine if a communicator's datatype indicates an table of
 *   arrays
 * @param x The communicator to check.
 * @return 1 if true, 0 otherwise.
 */
int is_comm_format_array_type(comm_t x);
  
/**
 * @brief Receive a message with the given communicator
 * @param comm The communicator to use
 * @param data An allocated buffer to put the received message into
 * @param len The size of the allocated buffer in data
 * @return On success, the size of the received message will be returned.
 *   Negative values indicate there was an error.
 */
long comm_recv(comm_t comm, char *data, const size_t len);
/**
 * @brief Receive a message with the given communicator into a buffer
 *   allocated on heap that can be reallocated.
 * @param comm The communicator to use
 * @param data Pointer to a buffer on heap to put the received message
 *   into that can be reallocated.
 * @param len The size of the allocated buffer in data
 * @return On success, the size of the received message will be returned.
 *   Negative values indicate there was an error.
 */
long comm_recv_realloc(comm_t comm, char **data, const size_t len);
  
/**
 * @brief The number of messages in the communicators queue
 * @param comm The communicator to query
 * @return The number of messages in the queue
 */
int comm_nmsg(comm_t comm);
/*!
 * @brief Send the variable argument list with the given communicator
 * @param x The communicator to use
 * @param nargs The number of arguments
 * @param ... The arguments to send
 * @return Any value greater than 0 indicates success
 */
int ncommSend(const comm_t x, size_t nargs, ...);
/*!
 * @brief Receive a message into a variable list of arguments
 * @param x The communicator to use
 * @param allow_realloc If true then the number of arguments can be changed based on the message
 * @param nargs The current number of arguments
 * @param ... The arguments
 * @return Any value greater than 0 indicates success
 */
long ncommRecv(comm_t x, const int allow_realloc, size_t nargs, ...);
/*!
 * @brief Send a request and receive a response from a list of variable arguments containing data for both the request and response.
 * @param x The communicator to use
 * @param allow_realloc If true then the number of arguments can change based on the messages
 * @param nargs The number of arguments
 * @param ... The arguments
 * @return Any value greater than 0 indicates success
 */
long ncommCall(comm_t x, const int allow_realloc, size_t nargs, ...);
/*!
 * @brief Send a message from a list of pointers
 * @param comm The communicator to use
 * @param nargs The number of arguments
 * @param ptrs Pointer array of pointers of the data to send
 * @param for_fortran If set to true then the list is of explicit fortran pointers
 * @return Any value greater than 0 indicates success
 */
int pcommSend(const comm_t comm, size_t nargs, void** ptrs, int for_fortran);
/*!
 * @brief Receive a messag into a list of pointers
 * @param comm The communciator to use
 * @param allow_realloc If true then the number of pointers may change based on the message contents
 * @param nargs The number of pointers
 * @param ptrs Pointer array of pointers to hold the message
 * @param for_fortran If true then the list is of explicit fortran pointers
 * @return Any value greater than 0 indicates success
 */
long pcommRecv(comm_t comm, const int allow_realloc, size_t nargs, void** ptrs, int for_fortran);
/*!
 * @brief Send a request and receive a response from a list of pointers containing data for both the request and response.
 * @param comm The communciator to use
 * @param allow_realloc If true then the number of pointers may change based on the message contents
 * @param nargs The number of pointers
 * @param ptrs Pointer array of pointers to hold the message
 * @param for_fortran If true then the list is of explicit fortran pointers
 * @return Any value greater than 0 indicates success
 */
long pcommCall(comm_t comm, const int allow_realloc, size_t nargs, void** ptrs, int for_fortran);
#define commSend(x, ...) ncommSend(x, COUNT_VARARGS(__VA_ARGS__), __VA_ARGS__)
#define commRecvStack(x, ...) ncommRecv(x, 0, COUNT_VARARGS(__VA_ARGS__), __VA_ARGS__)
#define commRecvHeap(x, ...) ncommRecv(x, 1, COUNT_VARARGS(__VA_ARGS__), __VA_ARGS__)
#define commCallStack(x, ...) ncommCall(x, 0, COUNT_VARARGS(__VA_ARGS__), __VA_ARGS__)
#define commCallHeap(x, ...) ncommCall(x, 1, COUNT_VARARGS(__VA_ARGS__), __VA_ARGS__)
  
#define commRecv commRecvStack
#define commRecvRealloc commRecvHeap
#define commCall commCallStack
#define commCallRealloc commCallHeap

void global_scope_comm_on_c();
void global_scope_comm_off_c();

#ifdef __cplusplus
}
#endif
