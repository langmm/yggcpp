#pragma once

#define STRINGIFY_MACRO(x) STR(x)
#define STR(x) #x
#define EXPAND(x) x
#define CONCAT3_STR(n1, n2, n3) STRINGIFY_MACRO(EXPAND(n1)EXPAND(n2)EXPAND(n3))
#define CONCAT_AGAIN(n1, n2) n1##n2
#define CONCAT(n1, n2) CONCAT_AGAIN(n1, n2)

#ifdef DEFAULT_COMM_PREFIX
#define COMM_BASE CONCAT(DEFAULT_COMM_PREFIX, Comm)
#define COMM_BASE_TYPE CONCAT(DEFAULT_COMM_PREFIX, _COMM)
#define DefaultComm CONCAT(DEFAULT_COMM_PREFIX, Comm)
#include CONCAT3_STR(communicators/,DEFAULT_COMM_PREFIX,Comm.hpp)
#endif

// #undef CONCAT
// #undef CONCAT_AGAIN
// #undef CONCAT3_STR
// #undef EXPAND
// #undef STR
// #undef STRINGIFY_MACRO

#ifndef COMM_BASE
// #error "Neither IPC nor ZMQ were found, at least one is needed"
#include "communicators/FileComm.hpp"
#define COMM_BASE FileComm
#define COMM_BASE_TYPE FILE_COMM
#define DefaultComm FileComm
#endif
