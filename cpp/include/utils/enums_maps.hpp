#pragma once

#include <map>
#include <string>
#include "utils/enums.hpp"

namespace YggInterface {
  namespace utils {
    
    const std::map<const COMM_TYPE, const std::string> COMM_TYPE_map {
      {NULL_COMM   , "NULL"   },
      {DEFAULT_COMM, "DEFAULT"},
      {IPC_COMM    , "IPC"    },
      {ZMQ_COMM    , "ZMQ"    },
      {MPI_COMM    , "MPI"    },
      {SERVER_COMM , "SERVER" },
      {CLIENT_COMM , "CLIENT" },
      {FILE_COMM   , "FILE"   },
      {RMQ_COMM    , "RMQ"    },
      {VALUE_COMM  , "VALUE"  },
      {REST_COMM   , "REST"   },
    };
    
    
    const std::map<const COMM_TYPE, const std::string> COMM_TYPE_cls_map {
      {NULL_COMM   , "NullComm"   },
      {DEFAULT_COMM, "DefaultComm"},
      {IPC_COMM    , "IPCComm"    },
      {ZMQ_COMM    , "ZMQComm"    },
      {MPI_COMM    , "MPIComm"    },
      {SERVER_COMM , "ServerComm" },
      {CLIENT_COMM , "ClientComm" },
      {FILE_COMM   , "FileComm"   },
      {RMQ_COMM    , "RMQComm"    },
      {VALUE_COMM  , "ValueComm"  },
      {REST_COMM   , "RESTComm"   },
    };
    
    
    const std::map<const DIRECTION, const std::string> DIRECTION_map {
      {SEND, "SEND"},
      {NONE, "NONE"},
      {RECV, "RECV"},
    };
    
    
    const std::map<const CLEANUP_MODE, const std::string> CLEANUP_MODE_map {
      {CLEANUP_DEFAULT, "CLEANUP_DEFAULT"},
      {CLEANUP_ATEXIT , "CLEANUP_ATEXIT" },
      {CLEANUP_COMMS  , "CLEANUP_COMMS"  },
    };
    
    
    const std::map<const COMM_FLAG, const std::string> COMM_FLAG_map {
      {COMM_FLAG_VALID               , "VALID"               },
      {COMM_FLAG_GLOBAL              , "GLOBAL"              },
      {COMM_FLAG_WORKER              , "WORKER"              },
      {COMM_FLAG_DELAYED_OPEN        , "DELAYED_OPEN"        },
      {COMM_FLAG_CLIENT              , "CLIENT"              },
      {COMM_FLAG_SERVER              , "SERVER"              },
      {COMM_FLAG_CLIENT_RESPONSE     , "CLIENT_RESPONSE"     },
      {COMM_FLAG_SERVER_RESPONSE     , "SERVER_RESPONSE"     },
      {COMM_FLAG_ALWAYS_SEND_HEADER  , "ALWAYS_SEND_HEADER"  },
      {COMM_FLAG_ALLOW_MULTIPLE_COMMS, "ALLOW_MULTIPLE_COMMS"},
      {COMM_FLAG_USED_SENT           , "USED_SENT"           },
      {COMM_FLAG_USED_RECV           , "USED_RECV"           },
      {COMM_FLAG_EOF_SENT            , "EOF_SENT"            },
      {COMM_FLAG_EOF_RECV            , "EOF_RECV"            },
      {COMM_FLAG_CLOSE_ON_EOF_RECV   , "CLOSE_ON_EOF_RECV"   },
      {COMM_FLAG_CLOSE_ON_EOF_SEND   , "CLOSE_ON_EOF_SEND"   },
      {COMM_FLAG_INTERFACE           , "INTERFACE"           },
      {COMM_FLAG_DELETE              , "DELETE"              },
      {COMM_FLAG_ASYNC               , "ASYNC"               },
      {COMM_FLAG_ASYNC_WRAPPED       , "ASYNC_WRAPPED"       },
      {COMM_FLAG_SET_OPP_ENV         , "SET_OPP_ENV"         },
      {COMM_FLAG_WRAPPER             , "WRAPPER"             },
      {COMM_FLAG_FORK_CYCLE          , "FORK_CYCLE"          },
      {COMM_FLAG_FORK_BROADCAST      , "FORK_BROADCAST"      },
      {COMM_FLAG_FORK_COMPOSITE      , "FORK_COMPOSITE"      },
      {COMM_FLAG_FORK_TINE           , "FORK_TINE"           },
      {FILE_FLAG_APPEND              , "FILE_FLAG_APPEND"    },
      {FILE_FLAG_BINARY              , "FILE_FLAG_BINARY"    },
      {FILE_FLAG_READLINE            , "FILE_FLAG_READLINE"  },
      {COMM_FLAG_MAX                 , "MAX"                 },
    };
    
    
    const std::map<const COMM_FLAG, const std::string> FILE_FLAG_map {
      {FILE_FLAG_APPEND  , "APPEND"  },
      {FILE_FLAG_BINARY  , "BINARY"  },
      {FILE_FLAG_READLINE, "READLINE"},
    };
    
    
    const std::map<const LANGUAGE, const std::string> LANGUAGE_map {
      {NO_LANGUAGE     , ""       },
      {CXX_LANGUAGE    , "cxx"    },
      {C_LANGUAGE      , "c"      },
      {FORTRAN_LANGUAGE, "fortran"},
      {PYTHON_LANGUAGE , "python" },
      {MATLAB_LANGUAGE , "matlab" },
      {R_LANGUAGE      , "r"      },
      {JULIA_LANGUAGE  , "julia"  },
      {JAVA_LANGUAGE   , "java"   },
    };
    
    
    const std::map<const HeadFlags, const std::string> HeadFlags_map {
      {HEAD_FLAG_VALID        , "HEAD_FLAG_VALID"        },
      {HEAD_FLAG_MULTIPART    , "HEAD_FLAG_MULTIPART"    },
      {HEAD_META_IN_DATA      , "HEAD_META_IN_DATA"      },
      {HEAD_AS_ARRAY          , "HEAD_AS_ARRAY"          },
      {HEAD_FLAG_OWNSDATA     , "HEAD_FLAG_OWNSDATA"     },
      {HEAD_FLAG_ALLOW_REALLOC, "HEAD_FLAG_ALLOW_REALLOC"},
      {HEAD_TEMPORARY         , "HEAD_TEMPORARY"         },
      {HEAD_FLAG_EOF          , "HEAD_FLAG_EOF"          },
      {HEAD_FLAG_CLIENT_EOF   , "HEAD_FLAG_CLIENT_EOF"   },
      {HEAD_FLAG_CLIENT_SIGNON, "HEAD_FLAG_CLIENT_SIGNON"},
      {HEAD_FLAG_SERVER_SIGNON, "HEAD_FLAG_SERVER_SIGNON"},
      {HEAD_FLAG_REPEAT       , "HEAD_FLAG_REPEAT"       },
      {HEAD_FLAG_FORMATTED    , "HEAD_FLAG_FORMATTED"    },
      {HEAD_FLAG_NO_TYPE      , "HEAD_FLAG_NO_TYPE"      },
      {HEAD_FLAG_NO_HEAD      , "HEAD_FLAG_NO_HEAD"      },
      {HEAD_FLAG_ASYNC        , "HEAD_FLAG_ASYNC"        },
    };
    
    
    const std::map<const HEAD_RESET_MODE, const std::string> HEAD_RESET_MODE_map {
      {HEAD_RESET_COMPLETE   , "HEAD_RESET_COMPLETE"   },
      {HEAD_RESET_KEEP_BUFFER, "HEAD_RESET_KEEP_BUFFER"},
      {HEAD_RESET_OWN_DATA   , "HEAD_RESET_OWN_DATA"   },
      {HEAD_RESET_DROP_DATA  , "HEAD_RESET_DROP_DATA"  },
    };
    
    
    const std::map<const SIGNON_STATUS, const std::string> SIGNON_STATUS_map {
      {SIGNON_NOT_SENT   , "SIGNON_NOT_SENT"   },
      {SIGNON_ERROR      , "SIGNON_ERROR"      },
      {SIGNON_NOT_WAITING, "SIGNON_NOT_WAITING"},
      {SIGNON_WAITING    , "SIGNON_WAITING"    },
      {SIGNON_COMPLETE   , "SIGNON_COMPLETE"   },
    };
    
    
    const std::map<const THREAD_STATUS, const std::string> THREAD_STATUS_map {
      {THREAD_INACTIVE    , "THREAD_INACTIVE"    },
      {THREAD_INIT        , "THREAD_INIT"        },
      {THREAD_STARTED     , "THREAD_STARTED"     },
      {THREAD_COMPLETE    , "THREAD_COMPLETE"    },
      {THREAD_ERROR       , "THREAD_ERROR"       },
      {THREAD_SIGNON_SENT , "THREAD_SIGNON_SENT" },
      {THREAD_SIGNON_RECV , "THREAD_SIGNON_RECV" },
      {THREAD_HAS_RESPONSE, "THREAD_HAS_RESPONSE"},
      {THREAD_IS_CLIENT   , "THREAD_IS_CLIENT"   },
      {THREAD_CLOSING     , "THREAD_CLOSING"     },
    };
    
    
    const std::map<const FORK_TYPE, const std::string> FORK_TYPE_map {
      {FORK_DEFAULT  , "FORK_DEFAULT"  },
      {FORK_CYCLE    , "FORK_CYCLE"    },
      {FORK_BROADCAST, "FORK_BROADCAST"},
      {FORK_COMPOSITE, "FORK_COMPOSITE"},
    };
    
  }
}