#pragma once

#ifdef IPCDEF
#include "IPCComm.hpp"
namespace communication {
namespace communicator {
static comm_type _default_comm = IPC_COMM;
}
}
#define COMM_BASE IPPComm
#else
#include "ZMQComm.hpp"
namespace communication {
namespace communicator {
static comm_type _default_comm = ZMQ_COMM;
}
}
#define COMM_BASE ZMQComm
#endif
