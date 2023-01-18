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
//int comm_send(comm_t* comm, const char *data, const size_t &len);
//int comm_recv(comm_t* comm, char **data, const size_t &len, bool allow_realloc);
int comm_send(comm_t* comm, const dtype_t* dtype);
long comm_recv(comm_t* comm, dtype_t* dtype);
int comm_nmsg(comm_t* comm);
#ifdef __cplusplus
}
#endif