#ifndef DOXYGEN_SHOULD_SKIP_THIS

  enum, bind( C )
     enumerator :: &
        NULL_COMM = 0, &
        DEFAULT_COMM = 1, &
        IPC_COMM = 2, &
        ZMQ_COMM = 3, &
        MPI_COMM = 4, &
        SERVER_COMM = 5, &
        CLIENT_COMM = 6, &
        FILE_COMM = 7, &
        RMQ_COMM = 8, &
        VALUE_COMM = 9, &
        REST_COMM = 10, &
        BUFFER_COMM = 11
  end enum


  enum, bind( C )
     enumerator :: &
        SEND = 0, &
        NONE = 1, &
        RECV = 2
  end enum


  enum, bind( C )
     enumerator :: &
        CLEANUP_DEFAULT = 0, &
        CLEANUP_ATEXIT = 1, &
        CLEANUP_COMMS = 2
  end enum


  integer(kind=int64), parameter :: COMM_FLAG_VALID = 1_int64
  integer(kind=int64), parameter :: COMM_FLAG_GLOBAL = 2_int64
  integer(kind=int64), parameter :: COMM_FLAG_WORKER = 4_int64
  integer(kind=int64), parameter :: COMM_FLAG_DELAYED_OPEN = 8_int64
  integer(kind=int64), parameter :: COMM_FLAG_CLIENT = 16_int64
  integer(kind=int64), parameter :: COMM_FLAG_SERVER = 32_int64
  integer(kind=int64), parameter :: COMM_FLAG_CLIENT_RESPONSE = 64_int64
  integer(kind=int64), parameter :: COMM_FLAG_SERVER_RESPONSE = 128_int64
  integer(kind=int64), parameter :: COMM_FLAG_ALWAYS_SEND_HEADER = 256_int64
  integer(kind=int64), parameter :: COMM_FLAG_ALLOW_MULTIPLE_COMMS = 512_int64
  integer(kind=int64), parameter :: COMM_FLAG_USED_SENT = 1024_int64
  integer(kind=int64), parameter :: COMM_FLAG_USED_RECV = 2048_int64
  integer(kind=int64), parameter :: COMM_FLAG_EOF_SENT = 4096_int64
  integer(kind=int64), parameter :: COMM_FLAG_EOF_RECV = 8192_int64
  integer(kind=int64), parameter :: COMM_FLAG_CLOSE_ON_EOF_RECV = 16384_int64
  integer(kind=int64), parameter :: COMM_FLAG_CLOSE_ON_EOF_SEND = 32768_int64
  integer(kind=int64), parameter :: COMM_FLAG_INTERFACE = 65536_int64
  integer(kind=int64), parameter :: COMM_FLAG_DELETE = 131072_int64
  integer(kind=int64), parameter :: COMM_FLAG_ASYNC = 262144_int64
  integer(kind=int64), parameter :: COMM_FLAG_ASYNC_WRAPPED = 524288_int64
  integer(kind=int64), parameter :: COMM_FLAG_SET_OPP_ENV = 1048576_int64
  integer(kind=int64), parameter :: COMM_FLAG_WRAPPER = 2097152_int64
  integer(kind=int64), parameter :: COMM_FLAG_FORK = 4194304_int64
  integer(kind=int64), parameter :: COMM_FLAG_FORK_CYCLE = 8388608_int64
  integer(kind=int64), parameter :: COMM_FLAG_FORK_BROADCAST = 16777216_int64
  integer(kind=int64), parameter :: COMM_FLAG_FORK_COMPOSITE = 33554432_int64
  integer(kind=int64), parameter :: COMM_FLAG_FORK_TINE = 67108864_int64
  integer(kind=int64), parameter :: FILE_FLAG_APPEND = 17592186044416_int64
  integer(kind=int64), parameter :: FILE_FLAG_BINARY = 35184372088832_int64
  integer(kind=int64), parameter :: FILE_FLAG_READLINE = 70368744177664_int64


  enum, bind( C )
     enumerator :: &
        NO_LANGUAGE = 0, &
        CXX_LANGUAGE = 1, &
        C_LANGUAGE = 2, &
        FORTRAN_LANGUAGE = 3, &
        PYTHON_LANGUAGE = 4, &
        MATLAB_LANGUAGE = 5, &
        R_LANGUAGE = 6, &
        JULIA_LANGUAGE = 7, &
        JAVA_LANGUAGE = 8
  end enum


  enum, bind( C )
     enumerator :: &
        HEAD_FLAG_VALID = 1, &
        HEAD_FLAG_MULTIPART = 2, &
        HEAD_META_IN_DATA = 4, &
        HEAD_AS_ARRAY = 8, &
        HEAD_FLAG_OWNSDATA = 16, &
        HEAD_FLAG_ALLOW_REALLOC = 32, &
        HEAD_TEMPORARY = 64, &
        HEAD_FLAG_EOF = 128, &
        HEAD_FLAG_CLIENT_SIGNON = 512, &
        HEAD_FLAG_SERVER_SIGNON = 1024, &
        HEAD_FLAG_REPEAT = 2048, &
        HEAD_FLAG_FORMATTED = 4096, &
        HEAD_FLAG_NO_TYPE = 8192, &
        HEAD_FLAG_NO_HEAD = 16384, &
        HEAD_FLAG_ASYNC = 32768
  end enum


  enum, bind( C )
     enumerator :: &
        HEAD_RESET_COMPLETE = 0, &
        HEAD_RESET_KEEP_BUFFER = 1, &
        HEAD_RESET_OWN_DATA = 2, &
        HEAD_RESET_DROP_DATA = 3
  end enum


  enum, bind( C )
     enumerator :: &
        SIGNON_NOT_SENT = 0, &
        SIGNON_ERROR = 1, &
        SIGNON_NOT_WAITING = 2, &
        SIGNON_WAITING = 3, &
        SIGNON_COMPLETE = 4
  end enum


  enum, bind( C )
     enumerator :: &
        THREAD_INACTIVE = 0, &
        THREAD_INIT = 1, &
        THREAD_STARTED = 2, &
        THREAD_COMPLETE = 4, &
        THREAD_ERROR = 8, &
        THREAD_SIGNON_SENT = 16, &
        THREAD_SIGNON_RECV = 32, &
        THREAD_HAS_RESPONSE = 64, &
        THREAD_IS_CLIENT = 128, &
        THREAD_CLOSING = 256
  end enum


  enum, bind( C )
     enumerator :: &
        FORK_DEFAULT = 0, &
        FORK_CYCLE = 1, &
        FORK_BROADCAST = 2, &
        FORK_COMPOSITE = 3
  end enum

#endif
