#pragma once

#ifdef _YGGIPC
#include "IPCComm.hpp"
namespace communication {
namespace communicator {
static COMM_TYPE _default_comm = IPC_COMM;
}
}
#define COMM_BASE IPCComm
#else
#ifdef ZMQINSTALLED
#include "ZMQComm.hpp"
namespace communication {
namespace communicator {
static COMM_TYPE _default_comm = ZMQ_COMM;
}
}
#define COMM_BASE ZMQComm
#else
#error "Neither boost.interprocess not ZMQ were found, at least one is needed"
#endif
#endif
