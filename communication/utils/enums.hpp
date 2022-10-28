#pragma once

#ifdef __cplusplus
#include <map>
#include <string>
#include <istream>
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
    T_NULL, T_OBJECT, T_1DARRAY,
    T_DIRECT, T_NDARRAY, T_SCALAR,
    T_PLY, T_OBJ, T_GROUP, T_FORMATTED,
    T_CLASS, T_FUNCTION, T_INSTANCE, T_SCHEMA, T_ANY, T_PLY_T, T_OBJ_T
};

enum SUBTYPE {
    T_BOOLEAN, T_STRING,
    T_FLOAT, T_UINT, T_INT, T_COMPLEX, T_BYTES, T_UNICODE
};

enum VTYPE {
    T_SCALABLE, T_ARRAY1D
};

#ifdef __cplusplus
}
const std::map<const std::string, const SUBTYPE> submap {{"int", T_INT},
                                             {"float", T_FLOAT},
                                             {"bool", T_BOOLEAN},
                                             {"std::string", T_STRING},
                                             {"complex_float_t", T_COMPLEX},
                                             {"uint", T_UINT},
                                             {"bytes", T_BYTES},
                                             {"unicode", T_UNICODE}};

const std::map<const SUBTYPE, const std::string> mapsub {{T_INT, "int"},
                                                         {T_FLOAT, "float"},
                                                         {T_BOOLEAN, "bool"},
                                                         {T_STRING, "string"},
                                                         {T_COMPLEX, "complex_float_t"},
                                                         {T_UINT, "uint"},
                                                         {T_BYTES, "bytes"},
                                                         {T_UNICODE, "unicode"}};

static
std::istream& operator>>(std::istream& in, SUBTYPE& type) {
    int t;
    in >> t;
    type = static_cast<SUBTYPE>(t);
    return in;
}
static
std::istream& operator>>(std::istream& in, VTYPE& type) {
    int t;
    in >> t;
    type = static_cast<VTYPE>(t);
    return in;
}

#endif