#pragma once
#include "enums.hpp"
#include "datatypes/dtype_t.hpp"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct comm_t {
    void* comm;
};

void free_comm(comm_t* comm);

void close_comm(comm_t* comm);

comm_t* open_comm(const char* address, Direction dir, const comm_type &t, const dtype_t* datatype);
comm_t* ini_comm(const char* name, Direction dir, const comm_type &t, const dtype_t* datatype);
int comm_send(comm_t* comm, const char *data, const size_t &len);
int comm_recv(comm_t* comm, char **data, const size_t &len, bool allow_realloc);
int comm_nmsg(comm_t* comm);
#ifdef __cplusplus
}
#endif