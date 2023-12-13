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
     * @brief Struct for holding a C++ class communicator as a void* for use in C
     */
typedef struct comm_t {
    void* comm;
}comm_t;

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
 * @brief Initialize a new communicator
 * @param name The name for the communicator
 * @param dir The enumerated direction of the communicator
 * @param t The enumerated communicator type to create
 * @param datatype Enumerated data type for the communicator
 * @return comm_t struct containing the requested communicator
 */
comm_t init_comm(const char* name, DIRECTION dir, const COMM_TYPE t,
		 dtype_t datatype);

/**
 * @brief Set the format for a response for the given communicator
 * @param comm The communicator to set the response format for
 * @param fmt The format to use
 * @return 1 on success, 0 otherwise
 */
int set_response_format(comm_t comm, const char *fmt);

/**
 * @brief Set the response data type for the given communicator
 * @param x The communicator to set the data type for
 * @param datatype The data type to use
 * @return 1 on success, 0 otherwise
 */
int set_response_datatype(comm_t x, dtype_t datatype);

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
 * @brief Receive a message with the given communicator
 * @param comm The communicator to use
 * @param data The buffer to put the received message into
 * @param len The size of the buffer
 * @return Any value greater than 0 indicates success
 */
long comm_recv(comm_t comm, char *data, const size_t len);

/**
 * @brief Receive a message with the given communicator, resizing the buffer as needed.
 * @param comm The communicator to use
 * @param data The buffer to put the received message into
 * @param len The size of the buffer
 * @return Any value greater than 0 indicates success
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
