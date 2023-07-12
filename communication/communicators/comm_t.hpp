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

typedef struct comm_t {
    void* comm;
}comm_t;

void free_comm(comm_t* comm);

void close_comm(comm_t* comm);

comm_t open_comm(char* address, DIRECTION dir, const COMM_TYPE &t);
comm_t init_comm(const char* name, DIRECTION dir, const COMM_TYPE &t,
		 dtype_t datatype);
int set_response_format(comm_t comm, const char *fmt);
int set_response_datatype(comm_t x, dtype_t datatype);
int comm_send(comm_t comm, const char *data, const size_t len);
int comm_send_eof(comm_t comm);
long comm_recv(comm_t comm, char *data, const size_t len);
long comm_recv_realloc(comm_t comm, char **data, const size_t len);
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
