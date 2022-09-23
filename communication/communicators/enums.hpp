#pragma once

enum comm_type {
    NULL_COMM, IPC_COMM, ZMQ_COMM,
    SERVER_COMM, CLIENT_COMM,
    ASCII_FILE_COMM, ASCII_TABLE_COMM, ASCII_TABLE_ARRAY_COMM,
    MPI_COMM
};
enum Direction {
    SEND, NONE, RECV
};
