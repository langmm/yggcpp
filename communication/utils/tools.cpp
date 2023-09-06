//
// Created by friedel on 8/22/22.
//

#include "tools.hpp"

#define YGG_THREAD_MUTEX(name)			\
  std::mutex communication::utils::name ## _mutex;
  YGG_THREAD_MUTEX(init)
  YGG_THREAD_MUTEX(clean)
  YGG_THREAD_MUTEX(client)
  YGG_THREAD_MUTEX(comms)
  YGG_THREAD_MUTEX(IPCComm)
  YGG_THREAD_MUTEX(zmq)
  YGG_THREAD_MUTEX(zmqport)
#undef YGG_THREAD_MUTEX
