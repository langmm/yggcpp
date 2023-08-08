#pragma once
#ifndef PY_SSIZE_T_CLEAN
#define PY_SSIZE_T_CLEAN
#endif
#include <Python.h>
#include "communicators/IPCComm.hpp"
#include "pyYggCommBase.hpp"
#include "utils/enums.hpp"

typedef struct {
    pyComm_t super;
} pyIPCComm;

void IOPComm_dealloc(pyIPCComm* self);
int IPCComm_init(pyIPCComm* self, PyObject* args, PyObject* kwds);
PyObject* IPCComm_new(PyTypeObject* type, PyObject* args, PyObject* kwds);


IPC & MPI
comm_nmsg
send_single
recv_single

Client
set_timeout_recv
wait_for_recv
update_datatype
create_header_send
create_header_recv
create_worker_send
create_worker_recv
recv_single

Server
update_datatype
create_header_send
create_header_recv
send_single

zmq
comm_nmsg
afterSendRecv
send_single
recv_single
create_header_send
create_header_recv
create_worker_send
create_worker_recv