#pragma once

#ifdef IPCDEF
#ifdef IPCINSTALLED
#include "communicators/IPCComm.hpp"
#define COMM_BASE IPCComm
#define COMM_BASE_TYPE IPC_COMM
#define DefaultComm IPCComm
#else
#error "IPC requested, but not installed"
#endif
#else
#ifdef ZMQINSTALLED
#include "communicators/ZMQComm.hpp"
#define COMM_BASE ZMQComm
#define COMM_BASE_TYPE ZMQ_COMM
#define DefaultComm ZMQComm
#endif
#endif

#ifndef COMM_BASE
// #error "Neither IPC nor ZMQ were found, at least one is needed"
#include "communicators/FileComm.hpp"
#define COMM_BASE FileComm
#define COMM_BASE_TYPE FILE_COMM
#define DefaultComm FileComm
#endif
