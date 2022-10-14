#pragma once

#ifdef __cplusplus
#include <map>
#include <string>
extern "C" {
#endif

enum COMM_TYPE {
    NULL_COMM, IPC_COMM, ZMQ_COMM,
    SERVER_COMM, CLIENT_COMM,
    ASCII_FILE_COMM, ASCII_TABLE_COMM, ASCII_TABLE_ARRAY_COMM,
    MPI_COMM
};
enum DIRECTION {
    SEND, NONE, RECV
};

enum DTYPE {
    T_NULL, T_OBJECT,
    T_DIRECT, T_NDARRAY, T_SCALAR,
    T_PLY, T_OBJ, T_1DARRAY, T_2DARRAY, T_GROUP,
    T_CLASS, T_FUNCTION, T_INSTANCE, T_SCHEMA, T_ANY, T_PLY_T, T_OBJ_T
};

enum SUBTYPE {
    T_BOOLEAN, T_STRING,
    T_FLOAT, T_UINT, T_INT, T_COMPLEX, T_BYTES, T_UNICODE
};

#ifdef __cplusplus
const std::map<std::string, SUBTYPE> submap {{"int", T_INT},
                                             {"float", T_FLOAT},
                                             {"bool", T_BOOLEAN},
                                             {"std::string", T_STRING},
                                             {"complex_float_t", T_COMPLEX},
                                             {"uint", T_UINT}};
}
#endif