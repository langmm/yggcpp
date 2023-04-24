#pragma once
#include <cstdlib>
#include "utils/enums.hpp"
#include "datatypes/dtype_t.hpp"
#ifdef __cplusplus
extern "C" {
#endif

typedef struct comm_t {
    void* comm;
}comm_t;

void free_comm(comm_t* comm);

void close_comm(comm_t* comm);

comm_t* open_comm(char* address, DIRECTION dir, const COMM_TYPE &t);
comm_t* ini_comm(const char* name, DIRECTION dir, const COMM_TYPE &t);
int comm_send(comm_t* comm, const char *data, const size_t len);
long comm_recv(comm_t* comm, char *data, const size_t len);
long comm_recv_realloc(comm_t* comm, char **data, const size_t len);
int comm_nmsg(comm_t* comm);
int ncommSend(const comm_t *x, size_t nargs, ...);
long ncommRecv(comm_t *x, const int allow_realloc, size_t nargs, ...);
#define commSend(x, ...) ncommSend(x, COUNT_VARARGS(__VA_ARGS__), __VA_ARGS__)
#define commRecvStack(x, ...) ncommRecv(x, 0, COUNT_VARARGS(__VA_ARGS__), __VA_ARGS__)
#define commRecvHeap(x, ...) ncommRecv(x, 1, COUNT_VARARGS(__VA_ARGS__), __VA_ARGS__)
#define commRecv commRecvStack
#define commRecvRealloc commRecvHeap

#ifdef __cplusplus
}
#endif
