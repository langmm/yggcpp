#pragma once
#include <cstdlib>
#include "utils/enums.hpp"
#include "datatypes/dtype_t.hpp"

#define CSafe(x, err)						\
  try								\
    {								\
      x;							\
    }								\
  catch(...)							\
    {								\
      ygglog_error << "C++ exception thrown.") << std::endl;	\
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
comm_t open_comm(char* address, DIRECTION dir, const COMM_TYPE &t);
  
/**
 * Initialize a new communicator
 * @param name The name for the communicator
 * @param dir The enumerated direction of the communicator
 * @param t The enumerated communicator type to create
 * @return comm_t struct containing the requested communicator
 */
comm_t init_comm(const char* name, DIRECTION dir, const COMM_TYPE &t,
		 dtype_t datatype);

int set_response_format(comm_t comm, const char *fmt);
int set_response_datatype(comm_t x, dtype_t datatype);

/**
 * Send a message with the given communicator
 * @param comm The communicator to use
 * @param dtype The message to send
 * @return The status
 */
int comm_send(comm_t comm, const char *data, const size_t len);
int comm_send_eof(comm_t comm);
  
/**
 * Receive a message with the given communicator
 * @param comm The communicator to use
 * @param dtype The structure to put the received message into
 * @return The status
 */
long comm_recv(comm_t comm, char *data, const size_t len);
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

comm_t get_global_scope_comm(const char *name, const DIRECTION dir,
			     const COMM_TYPE &t);
void global_scope_comm_on_c();
void global_scope_comm_off_c();

#ifdef __cplusplus
}
#endif
