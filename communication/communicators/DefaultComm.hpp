#pragma once

#ifdef IPCDEF
#ifdef IPCINSTALLED
#include "IPCComm.hpp"
#define COMM_BASE IPCComm
#define COMM_BASE_TYPE IPC_COMM
#else
#error "IPC requested, but not installed"
#endif
#else
#ifdef ZMQINSTALLED
#include "ZMQComm.hpp"
#define COMM_BASE ZMQComm
#define COMM_BASE_TYPE ZMQ_COMM
#endif
#endif

#ifndef COMM_BASE
#error "Neither IPC nor ZMQ were found, at least one is needed"
#endif
