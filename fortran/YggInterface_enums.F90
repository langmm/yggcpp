#ifndef DOXYGEN_SHOULD_SKIP_THIS

  enum, bind( C )
     enumerator :: &
        NULL_COMM, &
        DEFAULT_COMM, &
        IPC_COMM, &
        ZMQ_COMM, &
        MPI_COMM, &
        SERVER_COMM, &
        CLIENT_COMM, &
        FILE_COMM, &
        RMQ_COMM, &
        VALUE_COMM, &
        REST_COMM
  end enum


  enum, bind( C )
     enumerator :: &
        SEND, &
        NONE, &
        RECV
  end enum


  enum, bind( C )
     enumerator :: &
        CLEANUP_DEFAULT, &
        CLEANUP_ATEXIT, &
        CLEANUP_COMMS
  end enum


  integer(kind=c_int64_t), protected, bind(c, name="COMM_FLAG_VALID_F") :: COMM_FLAG_VALID
  integer(kind=c_int64_t), protected, bind(c, name="COMM_FLAG_GLOBAL_F") :: COMM_FLAG_GLOBAL
  integer(kind=c_int64_t), protected, bind(c, name="COMM_FLAG_WORKER_F") :: COMM_FLAG_WORKER
  integer(kind=c_int64_t), protected, bind(c, name="COMM_FLAG_DELAYED_OPEN_F") :: COMM_FLAG_DELAYED_OPEN
  integer(kind=c_int64_t), protected, bind(c, name="COMM_FLAG_CLIENT_F") :: COMM_FLAG_CLIENT
  integer(kind=c_int64_t), protected, bind(c, name="COMM_FLAG_SERVER_F") :: COMM_FLAG_SERVER
  integer(kind=c_int64_t), protected, bind(c, name="COMM_FLAG_CLIENT_RESPONSE_F") :: COMM_FLAG_CLIENT_RESPONSE
  integer(kind=c_int64_t), protected, bind(c, name="COMM_FLAG_SERVER_RESPONSE_F") :: COMM_FLAG_SERVER_RESPONSE
  integer(kind=c_int64_t), protected, bind(c, name="COMM_FLAG_ALWAYS_SEND_HEADER_F") :: COMM_FLAG_ALWAYS_SEND_HEADER
  integer(kind=c_int64_t), protected, bind(c, name="COMM_FLAG_ALLOW_MULTIPLE_COMMS_F") :: COMM_FLAG_ALLOW_MULTIPLE_COMMS
  integer(kind=c_int64_t), protected, bind(c, name="COMM_FLAG_USED_SENT_F") :: COMM_FLAG_USED_SENT
  integer(kind=c_int64_t), protected, bind(c, name="COMM_FLAG_USED_RECV_F") :: COMM_FLAG_USED_RECV
  integer(kind=c_int64_t), protected, bind(c, name="COMM_FLAG_EOF_SENT_F") :: COMM_FLAG_EOF_SENT
  integer(kind=c_int64_t), protected, bind(c, name="COMM_FLAG_EOF_RECV_F") :: COMM_FLAG_EOF_RECV
  integer(kind=c_int64_t), protected, bind(c, name="COMM_FLAG_CLOSE_ON_EOF_RECV_F") :: COMM_FLAG_CLOSE_ON_EOF_RECV
  integer(kind=c_int64_t), protected, bind(c, name="COMM_FLAG_CLOSE_ON_EOF_SEND_F") :: COMM_FLAG_CLOSE_ON_EOF_SEND
  integer(kind=c_int64_t), protected, bind(c, name="COMM_FLAG_INTERFACE_F") :: COMM_FLAG_INTERFACE
  integer(kind=c_int64_t), protected, bind(c, name="COMM_FLAG_DELETE_F") :: COMM_FLAG_DELETE
  integer(kind=c_int64_t), protected, bind(c, name="COMM_FLAG_ASYNC_F") :: COMM_FLAG_ASYNC
  integer(kind=c_int64_t), protected, bind(c, name="COMM_FLAG_ASYNC_WRAPPED_F") :: COMM_FLAG_ASYNC_WRAPPED
  integer(kind=c_int64_t), protected, bind(c, name="COMM_FLAG_SET_OPP_ENV_F") :: COMM_FLAG_SET_OPP_ENV
  integer(kind=c_int64_t), protected, bind(c, name="COMM_FLAG_WRAPPER_F") :: COMM_FLAG_WRAPPER
  integer(kind=c_int64_t), protected, bind(c, name="COMM_FLAG_FORK_CYCLE_F") :: COMM_FLAG_FORK_CYCLE
  integer(kind=c_int64_t), protected, bind(c, name="COMM_FLAG_FORK_BROADCAST_F") :: COMM_FLAG_FORK_BROADCAST
  integer(kind=c_int64_t), protected, bind(c, name="COMM_FLAG_FORK_COMPOSITE_F") :: COMM_FLAG_FORK_COMPOSITE
  integer(kind=c_int64_t), protected, bind(c, name="COMM_FLAG_FORK_TINE_F") :: COMM_FLAG_FORK_TINE
  integer(kind=c_int64_t), protected, bind(c, name="FILE_FLAG_APPEND_F") :: FILE_FLAG_APPEND
  integer(kind=c_int64_t), protected, bind(c, name="FILE_FLAG_BINARY_F") :: FILE_FLAG_BINARY
  integer(kind=c_int64_t), protected, bind(c, name="FILE_FLAG_READLINE_F") :: FILE_FLAG_READLINE
  integer(kind=c_int64_t), protected, bind(c, name="COMM_FLAG_MAX_F") :: COMM_FLAG_MAX


  enum, bind( C )
     enumerator :: &
        NO_LANGUAGE, &
        CXX_LANGUAGE, &
        C_LANGUAGE, &
        FORTRAN_LANGUAGE, &
        PYTHON_LANGUAGE, &
        MATLAB_LANGUAGE, &
        R_LANGUAGE, &
        JULIA_LANGUAGE, &
        JAVA_LANGUAGE
  end enum


  enum, bind( C )
     enumerator :: &
        HEAD_FLAG_VALID, &
        HEAD_FLAG_MULTIPART, &
        HEAD_META_IN_DATA, &
        HEAD_AS_ARRAY, &
        HEAD_FLAG_OWNSDATA, &
        HEAD_FLAG_ALLOW_REALLOC, &
        HEAD_TEMPORARY, &
        HEAD_FLAG_EOF, &
        HEAD_FLAG_CLIENT_EOF, &
        HEAD_FLAG_CLIENT_SIGNON, &
        HEAD_FLAG_SERVER_SIGNON, &
        HEAD_FLAG_REPEAT, &
        HEAD_FLAG_FORMATTED, &
        HEAD_FLAG_NO_TYPE, &
        HEAD_FLAG_NO_HEAD, &
        HEAD_FLAG_ASYNC
  end enum


  enum, bind( C )
     enumerator :: &
        HEAD_RESET_COMPLETE, &
        HEAD_RESET_KEEP_BUFFER, &
        HEAD_RESET_OWN_DATA, &
        HEAD_RESET_DROP_DATA
  end enum


  enum, bind( C )
     enumerator :: &
        SIGNON_NOT_SENT, &
        SIGNON_ERROR, &
        SIGNON_NOT_WAITING, &
        SIGNON_WAITING, &
        SIGNON_COMPLETE
  end enum


  enum, bind( C )
     enumerator :: &
        THREAD_INACTIVE, &
        THREAD_INIT, &
        THREAD_STARTED, &
        THREAD_COMPLETE, &
        THREAD_ERROR, &
        THREAD_SIGNON_SENT, &
        THREAD_SIGNON_RECV, &
        THREAD_HAS_RESPONSE, &
        THREAD_IS_CLIENT, &
        THREAD_CLOSING
  end enum


  enum, bind( C )
     enumerator :: &
        FORK_DEFAULT, &
        FORK_CYCLE, &
        FORK_BROADCAST, &
        FORK_COMPOSITE
  end enum

#endif
