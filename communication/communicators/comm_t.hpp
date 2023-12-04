#pragma once
#ifdef __cplusplus
#include <cstdlib>
#else
#include <stdbool.h>
#endif
#include "utils/enums.hpp"
#include "datatypes/dtype_t.h"

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
     * Struct for holding a C++ class communicator as a void* for use in C
     */
typedef struct comm_t {
    void* comm;
}comm_t;

  
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
 * Delete the underlying communicator
 * @param comm The communicator to delete
 */
void free_comm(comm_t* comm);

/**
 * Close the underlying communicator
 * @param comm The communicator to close
 */
void close_comm(comm_t* comm);

/**
 * Open a new communicator of the given type
 * @param address The initial address of the communicator
 * @param dir The enumerated direction for the communicator
 * @param t The enumerated communicator type to create
 * @return comm_t struct containing the requested communicator
 */
// comm_t open_comm(char* address, DIRECTION dir, const COMM_TYPE &t);

/**
 * Initialize a new communicator without interface flag set
 * @param name The name for the communicator
 * @param dir The enumerated direction of the communicator
 * @param t The enumerated communicator type to create
 * @param datatype Pointer to datatype that will be moved into the
 *   created communicator.
 * @param flags Bitwise flags describing properties the communicator
 *   should have.
 * @return comm_t struct containing the requested communicator
 */
comm_t _init_comm(const char* name, const enum DIRECTION dir,
		  const enum COMM_TYPE t,
		  dtype_t* datatype, const int flags);
  
/**
 * Initialize a new communicator
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
 * Set a communicators language.
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
 * Set a communicators datatype.
 * @param x Communicator
 * @param datatype Pointer to datatype. The underlying data will be
 *   consumed.
 * @return 1 if successful, 0 otherwise.
 */
int set_response_datatype(comm_t x, dtype_t* datatype);

/**
 * Get the datatype associated with a communicator.
 * @param x Communicator
 * @return The datatype
 */
dtype_t comm_get_datatype(comm_t x);
  
/**
 * Send a message with the given communicator
 * @param comm The communicator to use
 * @param dtype The message to send
 * @return The status, negative values indicate errors
 */
int comm_send(comm_t comm, const char *data, const size_t len);
/**
 * Send a message with the given communicator indicating that no more
 * messages will be sent.
 * @param comm The communicator to use
 * @return The status, negative values indicate errors
 */
int comm_send_eof(comm_t comm);

/**
 * Determine if a communicator's datatype indicates an table of arrays
 * @param x The communicator to check.
 * @return 1 if true, 0 otherwise.
 */
int is_comm_format_array_type(comm_t x);
  
/**
 * Receive a message with the given communicator
 * @param comm The communicator to use
 * @param data An allocated buffer to put the received message into
 * @param len The size of the allocated buffer in data
 * @return On success, the size of the received message will be returned.
 *   Negative values indicate there was an error.
 */
long comm_recv(comm_t comm, char *data, const size_t len);
/**
 * Receive a message with the given communicator into a buffer allocated
 * on heap that can be reallocated.
 * @param comm The communicator to use
 * @param data Pointer to a buffer on heap to put the received message
 *   into that can be reallocated.
 * @param len The size of the allocated buffer in data
 * @return On success, the size of the received message will be returned.
 *   Negative values indicate there was an error.
 */
long comm_recv_realloc(comm_t comm, char **data, const size_t len);
  
/**
 * The number of messages in the communicators queue
 * @param comm The communicator to query
 * @return The number of messages in the queue
 */
int comm_nmsg(comm_t comm);
int ncommSend(const comm_t x, size_t nargs, ...);
long ncommRecv(comm_t x, const int allow_realloc, size_t nargs, ...);
long ncommCall(comm_t x, const int allow_realloc, size_t nargs, ...);
int pcommSend(const comm_t comm, size_t nargs, void** ptrs, int for_fortran);
long pcommRecv(comm_t comm, const int allow_realloc, size_t nargs, void** ptrs, int for_fortran);
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
