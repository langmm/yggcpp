#pragma once
#include <cstdlib>
#include "utils/enums.hpp"
#include "datatypes/dtype_t.hpp"
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
comm_t* open_comm(char* address, DIRECTION dir, const COMM_TYPE &t);

/**
 * Initialize a new communicator
 * @param name The name for the communicator
 * @param dir The enumerated direction of the communicator
 * @param t The enumerated communicator type to create
 * @return comm_t struct containing the requested communicator
 */
comm_t* ini_comm(const char* name, DIRECTION dir, const COMM_TYPE &t);
//int comm_send(comm_t* comm, const char *data, const size_t &len);
//int comm_recv(comm_t* comm, char **data, const size_t &len, bool allow_realloc);

/**
 * Send a message with the given communicator
 * @param comm The communicator to use
 * @param dtype The message to send
 * @return The status
 */
int comm_send(comm_t* comm, const dtype_t* dtype);

/**
 * Receive a message with the given communicator
 * @param comm The communicator to use
 * @param dtype The structure to put the received message into
 * @return The status
 */
long comm_recv(comm_t* comm, dtype_t* dtype);

/**
 * The number of messages in the communicators queue
 * @param comm The communicator to query
 * @return The number of messages in the queue
 */
int comm_nmsg(comm_t* comm);
#ifdef __cplusplus
}
#endif