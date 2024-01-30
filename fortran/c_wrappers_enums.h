#ifndef YGG_FC_ENUM_WRAPPERS_H_
#define YGG_FC_ENUM_WRAPPERS_H_

#ifndef DOXYGEN_SHOULD_SKIP_THIS

#include "YggInterface_fortran_export.h"

#ifdef __cplusplus /* If this is a C++ compiler, use C linkage */
#include <cstdint>
extern "C" {
#else
#include "stdint.h"
#endif


  extern FYGG_API int64_t COMM_FLAG_VALID_F;
  extern FYGG_API int64_t COMM_FLAG_GLOBAL_F;
  extern FYGG_API int64_t COMM_FLAG_WORKER_F;
  extern FYGG_API int64_t COMM_FLAG_DELAYED_OPEN_F;
  extern FYGG_API int64_t COMM_FLAG_CLIENT_F;
  extern FYGG_API int64_t COMM_FLAG_SERVER_F;
  extern FYGG_API int64_t COMM_FLAG_CLIENT_RESPONSE_F;
  extern FYGG_API int64_t COMM_FLAG_SERVER_RESPONSE_F;
  extern FYGG_API int64_t COMM_FLAG_ALWAYS_SEND_HEADER_F;
  extern FYGG_API int64_t COMM_FLAG_ALLOW_MULTIPLE_COMMS_F;
  extern FYGG_API int64_t COMM_FLAG_USED_SENT_F;
  extern FYGG_API int64_t COMM_FLAG_USED_RECV_F;
  extern FYGG_API int64_t COMM_FLAG_EOF_SENT_F;
  extern FYGG_API int64_t COMM_FLAG_EOF_RECV_F;
  extern FYGG_API int64_t COMM_FLAG_CLOSE_ON_EOF_RECV_F;
  extern FYGG_API int64_t COMM_FLAG_CLOSE_ON_EOF_SEND_F;
  extern FYGG_API int64_t COMM_FLAG_INTERFACE_F;
  extern FYGG_API int64_t COMM_FLAG_DELETE_F;
  extern FYGG_API int64_t COMM_FLAG_ASYNC_F;
  extern FYGG_API int64_t COMM_FLAG_ASYNC_WRAPPED_F;
  extern FYGG_API int64_t COMM_FLAG_SET_OPP_ENV_F;
  extern FYGG_API int64_t COMM_FLAG_WRAPPER_F;
  extern FYGG_API int64_t COMM_FLAG_FORK_CYCLE_F;
  extern FYGG_API int64_t COMM_FLAG_FORK_BROADCAST_F;
  extern FYGG_API int64_t COMM_FLAG_FORK_COMPOSITE_F;
  extern FYGG_API int64_t COMM_FLAG_FORK_TINE_F;
  extern FYGG_API int64_t FILE_FLAG_APPEND_F;
  extern FYGG_API int64_t FILE_FLAG_BINARY_F;
  extern FYGG_API int64_t FILE_FLAG_READLINE_F;
  extern FYGG_API int64_t COMM_FLAG_MAX_F;


#ifdef __cplusplus
}
#endif

#endif // DOXYGEN_SHOULD_SKIP_THIS

#endif // YGG_FC_ENUM_WRAPPERS_H_
