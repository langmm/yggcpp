  interface
     ! Utilities
     subroutine c_free(x) bind(c, name="ygg_c_free")
       use, intrinsic :: iso_c_binding, only: c_ptr
       implicit none
       type(c_ptr) :: x
     end subroutine c_free

     subroutine fsleep(seconds) bind(c, name="sleep")
       use, intrinsic :: iso_c_binding, only: c_int
       integer(kind=c_int), intent(in), value :: seconds
     end subroutine fsleep

     subroutine register_function_wrapper_c(name, func) &
          bind(c, name="register_function_wrapper_f")
       use, intrinsic :: iso_c_binding, only: c_funptr, c_bool, c_char
       implicit none
       character(kind=c_char), dimension(*), intent(in) :: name
       type(c_funptr), value, intent(in) :: func
     end subroutine register_function_wrapper_c

     ! Methods for initializing channels
     function init_comm_c(name, dir, t, datatype, flags, ncomm) &
          result(channel) bind(c, name="_init_comm_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, &
            c_ptr, c_size_t, c_int64_t
       import :: yggcomm
       implicit none
       character(kind=c_char), dimension(*), intent(in) :: name
       integer(kind=c_int), value, intent(in) :: dir, t
       integer(kind=c_int64_t), value, intent(in) :: flags
       integer(kind=c_size_t), value, intent(in) :: ncomm
       type(c_ptr), value, intent(in) :: datatype
       type(yggcomm) :: channel
     end function init_comm_c
     
     ! Method for constructing data types
     subroutine display_dtype_c(datatype) &
          bind(c, name="display_dtype_f")
       import :: yggdtype
       implicit none
       type(yggdtype), value, intent(in) :: datatype
     end subroutine display_dtype_c
     
     ! Methods for sending/receiving
     function ygg_send_var_c(ygg_q, nargs, args) result (flag) &
          bind(c, name="ygg_send_var_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_int
       import :: yggcomm
       implicit none
       type(yggcomm), value, intent(in) :: ygg_q
       integer(kind=c_int), value :: nargs
       type(c_ptr), value :: args
       integer(kind=c_int) :: flag
     end function ygg_send_var_c

     function ygg_recv_var_c(ygg_q, nargs, args) result (flag) &
          bind(c, name="ygg_recv_var_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_int
       import :: yggcomm
       implicit none
       type(yggcomm), value :: ygg_q
       integer(kind=c_int), value :: nargs
       type(c_ptr), value :: args
       integer(kind=c_int) :: flag
     end function ygg_recv_var_c

     function ygg_recv_var_realloc_c(ygg_q, nargs, args) &
          result (flag) bind(c, name="ygg_recv_var_realloc_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_int
       import :: yggcomm
       implicit none
       type(yggcomm), value :: ygg_q
       integer(kind=c_int), value :: nargs
       type(c_ptr), value :: args
       integer(kind=c_int) :: flag
     end function ygg_recv_var_realloc_c

     function ygg_rpc_call_c(ygg_q, nargs, args) &
          result (flag) bind(c, name="rpc_call_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_int
       import :: yggcomm
       implicit none
       type(yggcomm), value :: ygg_q
       integer(kind=c_int), value :: nargs
       type(c_ptr), value :: args
       integer(kind=c_int) :: flag
     end function ygg_rpc_call_c
     
     function ygg_rpc_call_realloc_c(ygg_q, nargs, args) &
          result (flag) bind(c, name="rpc_call_realloc_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_int
       import :: yggcomm
       implicit none
       type(yggcomm), value :: ygg_q
       integer(kind=c_int), value :: nargs
       type(c_ptr), value :: args
       integer(kind=c_int) :: flag
     end function ygg_rpc_call_realloc_c

     ! Interface for getting/setting generic map elements
     ! Get
     function generic_map_get_keys_c(x, n_keys_f, key_size_f) &
          result(out) bind(c, name="generic_map_get_keys_f")
       use, intrinsic :: iso_c_binding, only: c_size_t, c_ptr
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value :: n_keys_f
       type(c_ptr), value :: key_size_f
       type(c_ptr) :: out
     end function generic_map_get_keys_c

     ! subroutine dummy_function_c(message) &
     !      bind(c, name="dummy_function_f")
     !   use, intrinsic :: iso_c_binding, only: c_char, c_int64_t
     !   implicit none
     !   integer(kind=c_int64_t), value, intent(in) :: message
     !   ! character(kind=c_char), dimension(*), intent(in) :: message
     ! end subroutine dummy_function_c

     ! LINES AFTER THIS WERE GENERATED AND SHOULD NOT BE MODIFIED DIRECTLY
     !====================================================================
     !> @brief Constructor for comm_t structure with format.
     !>   Create a comm_t structure based on a provided name that is used to
     !>   locate a particular comm address stored in the environment variable name
     !>   and a format string that can be used to format arguments into outgoing
     !>   messages for the queue.
     !> @param[in] name constant character pointer to name of queue.
     !> @param[in] fmtString character pointer to format string.
     !> @returns comm_t output queue structure.
     function ygg_output_fmt_c(name, fmtString) &
          result(out) &
          bind(c, name="ygg_output_fmt_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: yggcomm
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: name
       character(kind = c_char), dimension(*), intent(in) :: fmtString
       type(yggcomm) :: out
     end function ygg_output_fmt_c
     !> @brief Constructor for comm_t structure with format.
     !>   Create a comm_t structure based on a provided name that is used to
     !>   locate a particular comm address stored in the environment variable name
     !>   and a format stirng that can be used to extract arguments from received
     !>   messages.
     !> @param[in] name constant character pointer to name of queue.
     !> @param[in] fmtString character pointer to format string.
     !> @returns comm_t input queue structure.
     function ygg_input_fmt_c(name, fmtString) &
          result(out) &
          bind(c, name="ygg_input_fmt_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: yggcomm
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: name
       character(kind = c_char), dimension(*), intent(in) :: fmtString
       type(yggcomm) :: out
     end function ygg_input_fmt_c
     !> @brief Send a message to an output queue.
     !>   Send a message smaller than YGG_MSG_MAX bytes to an output queue. If the
     !>   message is larger, it will not be sent.
     !> @param[in] yggQ comm_t structure that queue should be sent to.
     !> @param[in] data character pointer to message that should be sent.
     !> @param[in] len size_t length of message to be sent.
     !> @returns int 0 if send succesfull, -1 if send unsuccessful.
     function ygg_send_c(yggQ, data, len) &
          result(out) &
          bind(c, name="ygg_send_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_size_t
       import :: yggcomm
       implicit none
       type(yggcomm), value, intent(in) :: yggQ
       character(kind = c_char), dimension(*), intent(in) :: data
       integer(kind = c_size_t), value, intent(in) :: len
       integer(kind = c_int) :: out
     end function ygg_send_c
     !> @brief Send EOF message to the output queue.
     !> @param[in] yggQ comm_t structure that message should be sent to.
     !> @returns int 0 if send successfull, -1 if unsuccessful.
     function ygg_send_eof_c(yggQ) &
          result(out) &
          bind(c, name="ygg_send_eof_f")
       use, intrinsic :: iso_c_binding, only: c_int
       import :: yggcomm
       implicit none
       type(yggcomm), value, intent(in) :: yggQ
       integer(kind = c_int) :: out
     end function ygg_send_eof_c
     !> @brief Receive a message from an input queue.
     !>   Receive a message smaller than YGG_MSG_MAX bytes from an input queue.
     !> @param[in] yggQ Communicator that message should be sent to.
     !> @param[out] data Pointer to allocated buffer where the message
     !>   should be saved.
     !> @param[in] len Length of the allocated message buffer in bytes.
     !> @returns -1 if message could not be received. Length of the received
     !>   message if message was received.
     function ygg_recv_c(yggQ, data, len) &
          result(out) &
          bind(c, name="ygg_recv_f")
       use, intrinsic :: iso_c_binding, only: c_long, c_ptr, c_size_t
       import :: yggcomm
       implicit none
       type(yggcomm), value :: yggQ
       type(c_ptr), value :: data
       integer(kind = c_size_t), value, intent(in) :: len
       integer(kind = c_long) :: out
     end function ygg_recv_c
     !> @brief Constructor for client side RPC structure with explicit type info.
     !>   Creates an instance of yggRpc_t with provided information.
     !> @param[in] name constant character pointer to name for queues.
     !> @param[in] outType Pointer to a dtype_t data structure containing type info
     !>   for data that will be sent by the client.
     !> @param[in] inType Pointer to a dtype_t data structure containing type info
     !>   for data that will be received by the client.
     !> @return yggRpc_t structure with provided info.
     function ygg_rpc_client_type_c(name, outType, inType) &
          result(out) &
          bind(c, name="ygg_rpc_client_type_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr
       import :: yggcomm
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: name
       type(c_ptr), value :: outType
       type(c_ptr), value :: inType
       type(yggcomm) :: out
     end function ygg_rpc_client_type_c
     !> @brief Constructor for server side RPC structure with explicit type info.
     !>   Creates an instance of yggRpc_t with provided information.
     !> @param[in] name constant character pointer to name for queues.
     !> @param[in] inType Pointer to a dtype_t data structure containing type info
     !>   for data that will be received by the server.
     !> @param[in] outType Pointer to a dtype_t data structure containing type info
     !>   for data that will be sent by the server.
     !> @return yggRpc_t structure with provided info.
     function ygg_rpc_server_type_c(name, inType, outType) &
          result(out) &
          bind(c, name="ygg_rpc_server_type_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr
       import :: yggcomm
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: name
       type(c_ptr), value :: inType
       type(c_ptr), value :: outType
       type(yggcomm) :: out
     end function ygg_rpc_server_type_c
     !> @brief Constructor for client side RPC structure.
     !>   Creates an instance of yggRpc_t with provided information.
     !> @param[in] name constant character pointer to name for queues.
     !> @param[in] outFormat character pointer to format that should be used for
     !>   formatting output.
     !> @param[in] inFormat character pointer to format that should be used for
     !>   parsing input.
     !> @return yggRpc_t structure with provided info.
     function ygg_rpc_client_c(name, outFormat, inFormat) &
          result(out) &
          bind(c, name="ygg_rpc_client_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: yggcomm
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: name
       character(kind = c_char), dimension(*), intent(in) :: outFormat
       character(kind = c_char), dimension(*), intent(in) :: inFormat
       type(yggcomm) :: out
     end function ygg_rpc_client_c
     !> @brief Constructor for server side RPC structure.
     !>   Creates an instance of yggRpc_t with provided information.
     !> @param[in] name constant character pointer to name for queues.
     !> @param[in] inFormat character pointer to format that should be used for
     !>   parsing input.
     !> @param[in] outFormat character pointer to format that should be used for
     !>   formatting output.
     !> @return yggRpc_t structure with provided info.
     function ygg_rpc_server_c(name, inFormat, outFormat) &
          result(out) &
          bind(c, name="ygg_rpc_server_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: yggcomm
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: name
       character(kind = c_char), dimension(*), intent(in) :: inFormat
       character(kind = c_char), dimension(*), intent(in) :: outFormat
       type(yggcomm) :: out
     end function ygg_rpc_server_c
     !> @brief Constructor for client side timestep synchronization calls.
     !>   Creates an instance of comm_t with provided information.
     !> @param[in] name constant character pointer to name for queues.
     !> @param[in] t_units const char* Units that should be used for the
     !>   timestep. "" indicates no units.
     !> @return comm_t structure with provided info.
     function ygg_timesync_c(name, t_units) &
          result(out) &
          bind(c, name="ygg_timesync_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: yggcomm
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: name
       character(kind = c_char), dimension(*), intent(in) :: t_units
       type(yggcomm) :: out
     end function ygg_timesync_c
     !> @brief Constructor for table output comm to an output channel.
     !> @param[in] name constant character pointer to output channel name.
     !> @param[in] format_str character pointer to format string that should be used
     !>   to format rows into table lines.
     !> @returns comm_t output structure.
     function ygg_ascii_table_output_c(name, format_str) &
          result(out) &
          bind(c, name="ygg_ascii_table_output_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: yggcomm
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: name
       character(kind = c_char), dimension(*), intent(in) :: format_str
       type(yggcomm) :: out
     end function ygg_ascii_table_output_c
     !> @brief Constructor for AsciiTable input comm from an input channel.
     !> @param[in] name constant character pointer to input channel name.
     !> @returns comm_t input structure.
     function ygg_ascii_table_input_c(name) &
          result(out) &
          bind(c, name="ygg_ascii_table_input_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: yggcomm
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: name
       type(yggcomm) :: out
     end function ygg_ascii_table_input_c
     !> @brief Constructor for table output comm with array output.
     !> @param[in] name constant character pointer to an output channel name.
     !> @param[in] format_str character pointer to format string that should be
     !>   used to format rows into table lines.
     !> @returns comm_t output structure.
     function ygg_ascii_array_output_c(name, format_str) &
          result(out) &
          bind(c, name="ygg_ascii_array_output_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: yggcomm
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: name
       character(kind = c_char), dimension(*), intent(in) :: format_str
       type(yggcomm) :: out
     end function ygg_ascii_array_output_c
     !> @brief Constructor for AsciiTable input comm with array input.
     !> @param[in] name constant character pointer to an input channel name.
     !> @returns comm_t input structure.
     function ygg_ascii_array_input_c(name) &
          result(out) &
          bind(c, name="ygg_ascii_array_input_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: yggcomm
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: name
       type(yggcomm) :: out
     end function ygg_ascii_array_input_c
     !> @brief Constructor for ply output comm to an output channel.
     !> @param[in] name constant character pointer to output channel name.
     !> @returns comm_t output structure.
     function ygg_ply_output_c(name) &
          result(out) &
          bind(c, name="ygg_ply_output_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: yggcomm
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: name
       type(yggcomm) :: out
     end function ygg_ply_output_c
     !> @brief Constructor for ply input comm from an input channel.
     !> @param[in] name constant character pointer to input channel name.
     !> @returns comm_t input structure.
     function ygg_ply_input_c(name) &
          result(out) &
          bind(c, name="ygg_ply_input_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: yggcomm
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: name
       type(yggcomm) :: out
     end function ygg_ply_input_c
     !> @brief Constructor for obj output comm to an output channel.
     !> @param[in] name constant character pointer to output channel name.
     !> @returns comm_t output structure.
     function ygg_obj_output_c(name) &
          result(out) &
          bind(c, name="ygg_obj_output_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: yggcomm
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: name
       type(yggcomm) :: out
     end function ygg_obj_output_c
     !> @brief Constructor for obj input comm from an input channel.
     !> @param[in] name constant character pointer to input channel name.
     !> @returns comm_t input structure.
     function ygg_obj_input_c(name) &
          result(out) &
          bind(c, name="ygg_obj_input_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: yggcomm
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: name
       type(yggcomm) :: out
     end function ygg_obj_input_c
     !> @brief Constructor for generic output comm to an output channel.
     !> @param[in] name constant character pointer to output channel name.
     !> @returns comm_t output structure.
     function ygg_generic_output_c(name) &
          result(out) &
          bind(c, name="ygg_generic_output_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: yggcomm
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: name
       type(yggcomm) :: out
     end function ygg_generic_output_c
     !> @brief Constructor for generic input comm from an input channel.
     !> @param[in] name constant character pointer to input channel name.
     !> @returns comm_t input structure.
     function ygg_generic_input_c(name) &
          result(out) &
          bind(c, name="ygg_generic_input_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: yggcomm
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: name
       type(yggcomm) :: out
     end function ygg_generic_input_c
     !> @brief Constructor for generic output comm to an output channel.
     !> @param[in] name constant character pointer to output channel name.
     !> @returns comm_t output structure.
     function ygg_any_output_c(name) &
          result(out) &
          bind(c, name="ygg_any_output_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: yggcomm
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: name
       type(yggcomm) :: out
     end function ygg_any_output_c
     !> @brief Constructor for generic input comm from an input channel.
     !> @param[in] name constant character pointer to input channel name.
     !> @returns comm_t input structure.
     function ygg_any_input_c(name) &
          result(out) &
          bind(c, name="ygg_any_input_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: yggcomm
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: name
       type(yggcomm) :: out
     end function ygg_any_input_c
     !> @brief Constructor for vector output comm to an output channel.
     !> @param[in] name constant character pointer to output channel name.
     !> @returns comm_t output structure.
     function ygg_json_array_output_c(name) &
          result(out) &
          bind(c, name="ygg_json_array_output_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: yggcomm
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: name
       type(yggcomm) :: out
     end function ygg_json_array_output_c
     !> @brief Constructor for vector input comm from an input channel.
     !> @param[in] name constant character pointer to input channel name.
     !> @returns comm_t input structure.
     function ygg_json_array_input_c(name) &
          result(out) &
          bind(c, name="ygg_json_array_input_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: yggcomm
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: name
       type(yggcomm) :: out
     end function ygg_json_array_input_c
     !> @brief Constructor for map output comm to an output channel.
     !> @param[in] name constant character pointer to output channel name.
     !> @returns comm_t output structure.
     function ygg_json_object_output_c(name) &
          result(out) &
          bind(c, name="ygg_json_object_output_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: yggcomm
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: name
       type(yggcomm) :: out
     end function ygg_json_object_output_c
     !> @brief Constructor for map input comm from an input channel.
     !> @param[in] name constant character pointer to input channel name.
     !> @returns comm_t input structure.
     function ygg_json_object_input_c(name) &
          result(out) &
          bind(c, name="ygg_json_object_input_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: yggcomm
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: name
       type(yggcomm) :: out
     end function ygg_json_object_input_c
     !> @brief Write a log message at the ERROR level. This will also cause
     !>   the calling model to return an error code on exit.
     !> @param[in] fmt Log message.
     !> @param[in] ... Additional arguments are formated into the log message
     !>   via sprintf.
     subroutine ygglog_error_c(fmt) &
          bind(c, name="ygglog_error_f")
       use, intrinsic :: iso_c_binding, only: c_char
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: fmt
     end subroutine ygglog_error_c
     !> @brief Write a log message at the DEBUG level.
     !> @param[in] fmt Log message.
     !> @param[in] ... Additional arguments are formated into the log message
     !>   via sprintf.
     subroutine ygglog_debug_c(fmt) &
          bind(c, name="ygglog_debug_f")
       use, intrinsic :: iso_c_binding, only: c_char
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: fmt
     end subroutine ygglog_debug_c
     !> @brief Write a log message at the INFO level.
     !> @param[in] fmt Log message.
     !> @param[in] ... Additional arguments are formated into the log message
     !>   via sprintf.
     subroutine ygglog_info_c(fmt) &
          bind(c, name="ygglog_info_f")
       use, intrinsic :: iso_c_binding, only: c_char
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: fmt
     end subroutine ygglog_info_c
     !>   Initialize yggdrasil interface.
     function ygg_init_c() &
          result(out) &
          bind(c, name="ygg_init_f")
       use, intrinsic :: iso_c_binding, only: c_int
       implicit none
       integer(kind = c_int) :: out
     end function ygg_init_c
     !>   Cleanup yggdrasil interface prior to exit.
     subroutine ygg_exit_c() &
          bind(c, name="ygg_exit_f")
       implicit none
     end subroutine ygg_exit_c
     !> @brief Delete the underlying communicator
     !> @param comm The communicator to delete
     subroutine free_comm_c(comm) &
          bind(c, name="free_comm_f")
       use, intrinsic :: iso_c_binding, only: c_ptr
       implicit none
       type(c_ptr), value :: comm
     end subroutine free_comm_c
     !> @brief Close the underlying communicator
     !> @param comm The communicator to close
     subroutine close_comm_c(comm) &
          bind(c, name="close_comm_f")
       use, intrinsic :: iso_c_binding, only: c_ptr
       implicit none
       type(c_ptr), value :: comm
     end subroutine close_comm_c
     !>   Set a communicators datatype based on a C-style format string.
     !> @param comm Communicator
     !> @param fmt C-style format string
     !> @return 1 if successful, 0 otherwise.
     function set_response_format_c(comm, fmt) &
          result(out) &
          bind(c, name="set_response_format_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int
       import :: yggcomm
       implicit none
       type(yggcomm), value :: comm
       character(kind = c_char), dimension(*), intent(in) :: fmt
       integer(kind = c_int) :: out
     end function set_response_format_c
     !> @brief Set a communicators datatype.
     !> @param x Communicator
     !> @param datatype Pointer to datatype. The underlying data will be
     !>   consumed.
     !> @return 1 if successful, 0 otherwise.
     function set_response_datatype_c(x, datatype) &
          result(out) &
          bind(c, name="set_response_datatype_f")
       use, intrinsic :: iso_c_binding, only: c_int, c_ptr
       import :: yggcomm
       implicit none
       type(yggcomm), value :: x
       type(c_ptr), value :: datatype
       integer(kind = c_int) :: out
     end function set_response_datatype_c
     !> @brief Get the datatype associated with a communicator.
     !> @param x Communicator
     !> @return The datatype
     function comm_get_datatype_c(x) &
          result(out) &
          bind(c, name="comm_get_datatype_f")
       import :: yggcomm, yggdtype
       implicit none
       type(yggcomm), value :: x
       type(yggdtype) :: out
     end function comm_get_datatype_c
     !> @brief Set the datatype associated with a communicator.
     !> @param x Communicator
     !> @param datatype The datatype
     !> @return 1 if successful, 0 otherwise.
     function comm_set_datatype_c(x, datatype) &
          result(out) &
          bind(c, name="comm_set_datatype_f")
       use, intrinsic :: iso_c_binding, only: c_int, c_ptr
       import :: yggcomm
       implicit none
       type(yggcomm), value :: x
       type(c_ptr), value :: datatype
       integer(kind = c_int) :: out
     end function comm_set_datatype_c
     !> @brief Send a message with the given communicator
     !> @param comm The communicator to use
     !> @param data The message to send
     !> @param len The size of data in bytes
     !> @return Any value greater than 0 indicates success
     function comm_send_c(comm, data, len) &
          result(out) &
          bind(c, name="comm_send_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_size_t
       import :: yggcomm
       implicit none
       type(yggcomm), value :: comm
       character(kind = c_char), dimension(*), intent(in) :: data
       integer(kind = c_size_t), value, intent(in) :: len
       integer(kind = c_int) :: out
     end function comm_send_c
     !> @brief Send an end-of-file notification on the given communicator
     !> @param comm The communicator to use
     !> @return Any value greater than 0 indicates success
     function comm_send_eof_c(comm) &
          result(out) &
          bind(c, name="comm_send_eof_f")
       use, intrinsic :: iso_c_binding, only: c_int
       import :: yggcomm
       implicit none
       type(yggcomm), value :: comm
       integer(kind = c_int) :: out
     end function comm_send_eof_c
     !> @brief Determine if a communicator's datatype indicates an table of
     !>   arrays
     !> @param x The communicator to check.
     !> @return 1 if true, 0 otherwise.
     function is_comm_format_array_type_c(x) &
          result(out) &
          bind(c, name="is_comm_format_array_type_f")
       use, intrinsic :: iso_c_binding, only: c_int
       import :: yggcomm
       implicit none
       type(yggcomm), value, intent(in) :: x
       integer(kind = c_int) :: out
     end function is_comm_format_array_type_c
     !> @brief Receive a message with the given communicator
     !> @param comm The communicator to use
     !> @param data An allocated buffer to put the received message into
     !> @param len The size of the allocated buffer in data
     !> @return On success, the size of the received message will be returned.
     !>   Negative values indicate there was an error.
     function comm_recv_c(comm, data, len) &
          result(out) &
          bind(c, name="comm_recv_f")
       use, intrinsic :: iso_c_binding, only: c_long, c_ptr, c_size_t
       import :: yggcomm
       implicit none
       type(yggcomm), value :: comm
       type(c_ptr), value :: data
       integer(kind = c_size_t), value, intent(in) :: len
       integer(kind = c_long) :: out
     end function comm_recv_c
     !> @brief The number of messages in the communicators queue
     !> @param comm The communicator to query
     !> @return The number of messages in the queue
     function comm_nmsg_c(comm) &
          result(out) &
          bind(c, name="comm_nmsg_f")
       use, intrinsic :: iso_c_binding, only: c_int
       import :: yggcomm
       implicit none
       type(yggcomm), value :: comm
       integer(kind = c_int) :: out
     end function comm_nmsg_c
     !> @brief Send a message from a list of pointers
     !> @param comm The communicator to use
     !> @param nargs The number of arguments
     !> @param ptrs Pointer array of pointers of the data to send
     !> @param for_fortran If set to true then the list is of explicit fortran pointers
     !> @return Any value greater than 0 indicates success
     function pcomm_send_c(comm, nargs, ptrs, for_fortran) &
          result(out) &
          bind(c, name="pcomm_send_f")
       use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_size_t
       import :: yggcomm
       implicit none
       type(yggcomm), value, intent(in) :: comm
       integer(kind = c_size_t), value :: nargs
       type(c_ptr), value :: ptrs
       integer(kind = c_int), value :: for_fortran
       integer(kind = c_int) :: out
     end function pcomm_send_c
     !> @brief Receive a messag into a list of pointers
     !> @param comm The communciator to use
     !> @param allow_realloc If true then the number of pointers may change based on the message contents
     !> @param nargs The number of pointers
     !> @param ptrs Pointer array of pointers to hold the message
     !> @param for_fortran If true then the list is of explicit fortran pointers
     !> @return Any value greater than 0 indicates success
     function pcomm_recv_c(comm, allow_realloc, nargs, ptrs, for_fortran) &
          result(out) &
          bind(c, name="pcomm_recv_f")
       use, intrinsic :: iso_c_binding, only: c_int, c_long, c_ptr, c_size_t
       import :: yggcomm
       implicit none
       type(yggcomm), value :: comm
       integer(kind = c_int), value, intent(in) :: allow_realloc
       integer(kind = c_size_t), value :: nargs
       type(c_ptr), value :: ptrs
       integer(kind = c_int), value :: for_fortran
       integer(kind = c_long) :: out
     end function pcomm_recv_c
     !> @brief Send a request and receive a response from a list of pointers containing data for both the request and response.
     !> @param comm The communciator to use
     !> @param allow_realloc If true then the number of pointers may change based on the message contents
     !> @param nargs The number of pointers
     !> @param ptrs Pointer array of pointers to hold the message
     !> @param for_fortran If true then the list is of explicit fortran pointers
     !> @return Any value greater than 0 indicates success
     function pcomm_call_c(comm, allow_realloc, nargs, ptrs, for_fortran) &
          result(out) &
          bind(c, name="pcomm_call_f")
       use, intrinsic :: iso_c_binding, only: c_int, c_long, c_ptr, c_size_t
       import :: yggcomm
       implicit none
       type(yggcomm), value :: comm
       integer(kind = c_int), value, intent(in) :: allow_realloc
       integer(kind = c_size_t), value :: nargs
       type(c_ptr), value :: ptrs
       integer(kind = c_int), value :: for_fortran
       integer(kind = c_long) :: out
     end function pcomm_call_c
     subroutine set_global_comm_c() &
          bind(c, name="set_global_comm_f")
       implicit none
     end subroutine set_global_comm_c
     subroutine unset_global_comm_c() &
          bind(c, name="unset_global_comm_f")
       implicit none
     end subroutine unset_global_comm_c
     !> @brief Get the length of a C string stored in a pointer.
     !> @param[in] x String pointer.
     !> @returns Length of the string.
     function pointer_strlen_c(x) &
          result(out) &
          bind(c, name="pointer_strlen_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       implicit none
       type(c_ptr), value, intent(in) :: x
       integer(kind = c_size_t) :: out
     end function pointer_strlen_c
     !> @brief Initialize an empty generic object.
     !> @returns generic_t New generic object structure.
     function init_generic_c() &
          result(out) &
          bind(c, name="init_generic_f")
       import :: ygggeneric
       implicit none
       type(ygggeneric) :: out
     end function init_generic_c
     !> @brief Initialize an empty generic reference object.
     !> @param[in] parent Generic object that will be the parent of the
     !>   returned reference object.
     !> @returns New generic reference object structure.
     function init_generic_ref_c(parent) &
          result(out) &
          bind(c, name="init_generic_ref_f")
       import :: ygggeneric, ygggenericref
       implicit none
       type(ygggeneric), value :: parent
       type(ygggenericref) :: out
     end function init_generic_ref_c
     !> @brief Initialize an empty generic object with a null JSON document
     !> @returns generic_t New generic object structure.
     function init_generic_null_c() &
          result(out) &
          bind(c, name="init_generic_null_f")
       import :: ygggeneric
       implicit none
       type(ygggeneric) :: out
     end function init_generic_null_c
     !> @brief Initialize an empty array of mixed types with generic wrappers.
     !> @returns generic_t New generic object structure containing an empty
     !>   array.
     function init_generic_array_c() &
          result(out) &
          bind(c, name="init_generic_array_f")
       import :: ygggeneric
       implicit none
       type(ygggeneric) :: out
     end function init_generic_array_c
     !> @brief Initialize an empty map (JSON object) of mixed types with
     !>   generic wrappers.
     !> @returns New generic object structure contaiing an empty map (JSON
     !>   object).
     function init_generic_map_c() &
          result(out) &
          bind(c, name="init_generic_map_f")
       import :: ygggeneric
       implicit none
       type(ygggeneric) :: out
     end function init_generic_map_c
     !> @brief Initialize a generic object from a JSON string.
     !> @param[in] json JSON encoded string.
     !> @returns New generic object structure wrapping a rapidjson::Document
     !>   instance.
     function init_generic_json_c(json) &
          result(out) &
          bind(c, name="init_generic_json_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: ygggeneric
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: json
       type(ygggeneric) :: out
     end function init_generic_json_c
     !> @brief Initialize a generic object from a JSON string.
     !> @param[in] schema JSON encoded schema describing object to generate.
     !> @returns New generic object structure wrapping a rapidjson::Document
     !>   instance.
     function init_generic_generate_c(schema) &
          result(out) &
          bind(c, name="init_generic_generate_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: ygggeneric
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: schema
       type(ygggeneric) :: out
     end function init_generic_generate_c
     !> @brief Determine if a generic structure is initialized.
     !> @param[in] x Generic structure to test.
     !> @returns 1 if the structure is initialized, 0 otherwise.
     function is_generic_init_c(x) &
          result(out) &
          bind(c, name="is_generic_init_f")
       use, intrinsic :: iso_c_binding, only: c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value, intent(in) :: x
       integer(kind = c_int) :: out
     end function is_generic_init_c
     !> @brief Determine if a generic reference structure is initialized.
     !> @param[in] x Generic reference structure to test.
     !> @returns 1 if the structure is initialized, 0 otherwise.
     function is_generic_ref_init_c(x) &
          result(out) &
          bind(c, name="is_generic_ref_init_f")
       use, intrinsic :: iso_c_binding, only: c_int
       import :: ygggenericref
       implicit none
       type(ygggenericref), value, intent(in) :: x
       integer(kind = c_int) :: out
     end function is_generic_ref_init_c
     !> @brief Destroy a generic object.
     !> @param[in] x generic_t* Pointer to generic object structure to destory.
     !> @returns int -1 if unsuccessful, 0 otherwise.
     function free_generic_c(x) &
          result(out) &
          bind(c, name="free_generic_f")
       use, intrinsic :: iso_c_binding, only: c_int, c_ptr
       implicit none
       type(c_ptr), value :: x
       integer(kind = c_int) :: out
     end function free_generic_c
     !> @brief Copy data from one generic object into another.
     !> @param[in,out] dst Pointer to destination object.
     !> @param[in] src Source object.
     !> @returns int -1 if unsuccessful, 0 otherwise.
     function copy_generic_into_c(dst, src) &
          result(out) &
          bind(c, name="copy_generic_into_f")
       use, intrinsic :: iso_c_binding, only: c_int, c_ptr
       import :: ygggeneric
       implicit none
       type(c_ptr), value :: dst
       type(ygggeneric), value, intent(in) :: src
       integer(kind = c_int) :: out
     end function copy_generic_into_c
     !> @brief Copy data from one generic object to the other.
     !> @param[in] src generic_t Generic structure that data should be copied from.
     !> @returns generic_t Copied structure.
     function copy_generic_c(src) &
          result(out) &
          bind(c, name="copy_generic_f")
       import :: ygggeneric
       implicit none
       type(ygggeneric), value, intent(in) :: src
       type(ygggeneric) :: out
     end function copy_generic_c
     !> @brief Compare two generic objects.
     !> @param[in] a First object for comparison.
     !> @param[in] b Second object for comparison.
     !> @returns true if the two objects are equivalent, false otherwise.
     function compare_generic_c(a, b) &
          result(out) &
          bind(c, name="compare_generic_f")
       use, intrinsic :: iso_c_binding, only: c_bool
       import :: ygggeneric
       implicit none
       type(ygggeneric), value, intent(in) :: a
       type(ygggeneric), value, intent(in) :: b
       logical(kind = c_bool) :: out
     end function compare_generic_c
     !> @brief Display information about the generic type.
     !> @param[in] x generic_t* Wrapper for generic object.
     subroutine display_generic_c(x) &
          bind(c, name="display_generic_f")
       import :: ygggeneric
       implicit none
       type(ygggeneric), value, intent(in) :: x
     end subroutine display_generic_c
     !> @brief Add an element to the end of an array of generic elements.
     !> @param[in] arr generic_t Array to add element to.
     !> @param[in] x generic_t Element to add.
     !> @returns int Flag that is 1 if there is an error and 0 otherwise.
     function add_generic_array_c(arr, x) &
          result(out) &
          bind(c, name="add_generic_array_f")
       use, intrinsic :: iso_c_binding, only: c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: arr
       type(ygggeneric), value, intent(in) :: x
       integer(kind = c_int) :: out
     end function add_generic_array_c
     !> @brief Set an element in the array at a given index to a new value.
     !> @param[in] arr generic_t Array to add element to.
     !> @param[in] i size_t Index where element should be added.
     !> @param[in] x generic_t Element to add.
     !> @returns int Flag that is 1 if there is an error and 0 otherwise.
     function set_generic_array_c(arr, i, x) &
          result(out) &
          bind(c, name="set_generic_array_f")
       use, intrinsic :: iso_c_binding, only: c_int, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: arr
       integer(kind = c_size_t), value, intent(in) :: i
       type(ygggeneric), value, intent(in) :: x
       integer(kind = c_int) :: out
     end function set_generic_array_c
     !> @brief Get an element from an array.
     !> @param[in] arr generic_t Array to get element from.
     !> @param[in] i size_t Index of element to get.
     !> @param[out] x generic_t* Pointer to address where element should be
     !>   stored.
     !> @returns int Flag that is 1 if there is an error and 0 otherwise.
     function get_generic_array_c(arr, i, x) &
          result(out) &
          bind(c, name="get_generic_array_f")
       use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value, intent(in) :: arr
       integer(kind = c_size_t), value, intent(in) :: i
       type(c_ptr), value :: x
       integer(kind = c_int) :: out
     end function get_generic_array_c
     function get_generic_array_ref_c(arr, i, x) &
          result(out) &
          bind(c, name="get_generic_array_ref_f")
       use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value, intent(in) :: arr
       integer(kind = c_size_t), value, intent(in) :: i
       type(c_ptr), value :: x
       integer(kind = c_int) :: out
     end function get_generic_array_ref_c
     !> @brief Set an element in the object at for a given key to a new value.
     !> @param[in] arr generic_t Object to add element to.
     !> @param[in] k const char* Key where element should be added.
     !> @param[in] x generic_t Element to add.
     !> @returns int Flag that is 1 if there is an error and 0 otherwise.
     function set_generic_object_c(arr, k, x) &
          result(out) &
          bind(c, name="set_generic_object_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: arr
       character(kind = c_char), dimension(*), intent(in) :: k
       type(ygggeneric), value, intent(in) :: x
       integer(kind = c_int) :: out
     end function set_generic_object_c
     !> @brief Get an element from an object.
     !> @param[in] arr generic_t Object to get element from.
     !> @param[in] k const char* Key of element to return.
     !> @param[out] x generic_t* Pointer to address where element should be
     !>   stored.
     !> @returns int Flag that is 1 if there is an error and 0 otherwise.
     function get_generic_object_c(arr, k, x) &
          result(out) &
          bind(c, name="get_generic_object_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr
       import :: ygggeneric
       implicit none
       type(ygggeneric), value, intent(in) :: arr
       character(kind = c_char), dimension(*), intent(in) :: k
       type(c_ptr), value :: x
       integer(kind = c_int) :: out
     end function get_generic_object_c
     function get_generic_object_ref_c(arr, k, x) &
          result(out) &
          bind(c, name="get_generic_object_ref_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr
       import :: ygggeneric
       implicit none
       type(ygggeneric), value, intent(in) :: arr
       character(kind = c_char), dimension(*), intent(in) :: k
       type(c_ptr), value :: x
       integer(kind = c_int) :: out
     end function get_generic_object_ref_c
     !> @brief Determine if a map object has a certain key.
     !> @param[in] x generic_t Generic object that is presumed to contain a map.
     !> @param[in] key char* Key to check for.
     !> @returns int 1 if the key is present, 0 otherwise.
     function generic_map_has_key_c(x, key) &
          result(out) &
          bind(c, name="generic_map_has_key_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value, intent(in) :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       integer(kind = c_int) :: out
     end function generic_map_has_key_c
     !> @brief Set the data in the given item to the value given by the json character string
     !> @param[in, out] x The item to set
     !> @param[in] json The value to set x to
     !> @return 0 on success, 1 on error
     function generic_set_json_c(x, json) &
          result(out) &
          bind(c, name="generic_set_json_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: json
       integer(kind = c_int) :: out
     end function generic_set_json_c
     !> @brief Initialize Python if it is not initialized.
     !> @returns int 0 if successful, other values indicate errors.
     function init_python_api_c() &
          result(out) &
          bind(c, name="init_python_api_f")
       use, intrinsic :: iso_c_binding, only: c_int
       implicit none
       integer(kind = c_int) :: out
     end function init_python_api_c
     !> @brief Initialize a Python wrapper object.
     !> @returns Initialized object.
     function init_python_c() &
          result(out) &
          bind(c, name="init_python_f")
       import :: yggpython
       implicit none
       type(yggpython) :: out
     end function init_python_c
     !> @brief Destroy a structure containing a Python object.
     !> @param[in] x Pointer to Python object structure that should be freed.
     subroutine free_python_c(x) &
          bind(c, name="free_python_f")
       use, intrinsic :: iso_c_binding, only: c_ptr
       implicit none
       type(c_ptr), value :: x
     end subroutine free_python_c
     !> @brief Copy a Python object structure (NOTE: this dosn't copy the
     !>   underlying Python object but does increment the reference count).
     !> @param[in] x Structure containing Python object to copy.
     !> @returns python_t Copy of x.
     function copy_python_c(x) &
          result(out) &
          bind(c, name="copy_python_f")
       import :: yggpython
       implicit none
       type(yggpython), value :: x
       type(yggpython) :: out
     end function copy_python_c
     !> @brief Display a Python object structure.
     !> @param[in] x Structure containing Python object to display.
     subroutine display_python_c(x) &
          bind(c, name="display_python_f")
       import :: yggpython
       implicit none
       type(yggpython), value :: x
     end subroutine display_python_c
     !> @brief Determine if a datatype is empty.
     !> @param[in] dtype structure to test.
     !> @returns int 1 if dtype is empty, 0 otherwise.
     function is_empty_dtype_c(dtype) &
          result(out) &
          bind(c, name="is_empty_dtype_f")
       use, intrinsic :: iso_c_binding, only: c_int
       import :: yggdtype
       implicit none
       type(yggdtype), value, intent(in) :: dtype
       integer(kind = c_int) :: out
     end function is_empty_dtype_c
     !> @brief Determine if a datatype was created from a format.
     !> @param[in] type_struct Datatype structure.
     !> @returns 1 if the datatype was created from a format, 0 if it was not,
     !>   -1 if there is an error.
     function is_dtype_format_array_c(type_struct) &
          result(out) &
          bind(c, name="is_dtype_format_array_f")
       use, intrinsic :: iso_c_binding, only: c_int
       import :: yggdtype
       implicit none
       type(yggdtype), value, intent(in) :: type_struct
       integer(kind = c_int) :: out
     end function is_dtype_format_array_c
     !> @brief Get the name of the type from the class.
     !> @param[in] type_class Type structure..
     !> @returns Type name.
     function dtype_name_c(type_class) &
          result(out) &
          bind(c, name="dtype_name_f")
       use, intrinsic :: iso_c_binding, only: c_ptr
       import :: yggdtype
       implicit none
       type(yggdtype), value, intent(in) :: type_class
       type(c_ptr) :: out
     end function dtype_name_c
     !> @brief Get the subtype of the type.
     !> @param[in] type_class Type structure..
     !> @returns The subtype of the class, "" if there is an error.
     function dtype_subtype_c(type_class) &
          result(out) &
          bind(c, name="dtype_subtype_f")
       use, intrinsic :: iso_c_binding, only: c_ptr
       import :: yggdtype
       implicit none
       type(yggdtype), value, intent(in) :: type_class
       type(c_ptr) :: out
     end function dtype_subtype_c
     !> @brief Compare two datatypes structures.
     !> @param[in] a First datatype for comparison
     !> @param[in] b Second datatype for comparison
     function compare_dtype_c(a, b) &
          result(out) &
          bind(c, name="compare_dtype_f")
       use, intrinsic :: iso_c_binding, only: c_bool
       import :: yggdtype
       implicit none
       type(yggdtype), value, intent(in) :: a
       type(yggdtype), value, intent(in) :: b
       logical(kind = c_bool) :: out
     end function compare_dtype_c
     !> @brief Get the precision of the type.
     !> @param[in] type_class Type structure..
     !> @returns The precision of the class, 0 if there is an error.
     function dtype_precision_c(type_class) &
          result(out) &
          bind(c, name="dtype_precision_f")
       use, intrinsic :: iso_c_binding, only: c_size_t
       import :: yggdtype
       implicit none
       type(yggdtype), value, intent(in) :: type_class
       integer(kind = c_size_t) :: out
     end function dtype_precision_c
     !> @brief Set the type name in the datatype structure.
     !> @param[in,out] dtype Datatype structure to update. It must have
     !>   been initialized.
     !> @param[in] name Type name to set in dtype.
     !> @returns 0 on success, -1 if there is an error.
     function set_dtype_name_c(dtype, name) &
          result(out) &
          bind(c, name="set_dtype_name_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int
       import :: yggdtype
       implicit none
       type(yggdtype), value :: dtype
       character(kind = c_char), dimension(*), intent(in) :: name
       integer(kind = c_int) :: out
     end function set_dtype_name_c
     !> @brief Initialize a datatype structure including setting the type string.
     !> @param[in] dtype Type structure.
     !> @param[in] use_generic If true, serialized or deserialized
     !>   objects will be expected to be generic_t instances.
     !> @returns Initialized type structure.
     function complete_dtype_c(dtype, use_generic) &
          result(out) &
          bind(c, name="complete_dtype_f")
       use, intrinsic :: iso_c_binding, only: c_bool
       import :: yggdtype
       implicit none
       type(yggdtype), value :: dtype
       logical(kind = c_bool), value, intent(in) :: use_generic
       type(yggdtype) :: out
     end function complete_dtype_c
     !> @brief Construct a type object from a JSON schema.
     !> @param[in] schema Serialized JSON schema.
     !> @param[in] use_generic If true, serialized or deserialized objects will
     !>   be expected to be generic_t instances.
     !> @returns Type structure.
     function create_dtype_from_schema_c(schema, use_generic) &
          result(out) &
          bind(c, name="create_dtype_from_schema_f")
       use, intrinsic :: iso_c_binding, only: c_bool, c_char
       import :: yggdtype
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: schema
       logical(kind = c_bool), value, intent(in) :: use_generic
       type(yggdtype) :: out
     end function create_dtype_from_schema_c
     !> @brief Construct and empty type object.
     !> @param[in] use_generic If true, serialized or deserialized
     !>   objects will be expected to be generic_t instances.
     !> @returns Type structure.
     function create_dtype_empty_c(use_generic) &
          result(out) &
          bind(c, name="create_dtype_empty_f")
       use, intrinsic :: iso_c_binding, only: c_bool
       import :: yggdtype
       implicit none
       logical(kind = c_bool), value, intent(in) :: use_generic
       type(yggdtype) :: out
     end function create_dtype_empty_c
     !> @brief Create a datatype based on a Python dictionary.
     !> @param[in] pyobj Python dictionary.
     !> @param[in] use_generic If true, serialized or deserialized
     !>   objects will be expected to be generic_t instances.
     !> @returns Type structure.
     function create_dtype_python_c(pyobj, use_generic) &
          result(out) &
          bind(c, name="create_dtype_python_f")
       use, intrinsic :: iso_c_binding, only: c_bool, c_ptr
       import :: yggdtype
       implicit none
       type(c_ptr), value :: pyobj
       logical(kind = c_bool), value, intent(in) :: use_generic
       type(yggdtype) :: out
     end function create_dtype_python_c
     !> @brief Construct a Direct type object.
     !> @param[in] use_generic If true, serialized or deserialized
     !>   objects will be expected to be generic_t instances.
     !> @returns Type structure.
     function create_dtype_direct_c(use_generic) &
          result(out) &
          bind(c, name="create_dtype_direct_f")
       use, intrinsic :: iso_c_binding, only: c_bool
       import :: yggdtype
       implicit none
       logical(kind = c_bool), value, intent(in) :: use_generic
       type(yggdtype) :: out
     end function create_dtype_direct_c
     !> @brief Construct a type object for one of the default JSON types.
     !> @param[in] type Name of the type.
     !> @param[in] use_generic If true, serialized or deserialized
     !>   objects will be expected to be generic_t instances.
     !> @returns Type structure.
     function create_dtype_default_c(type, use_generic) &
          result(out) &
          bind(c, name="create_dtype_default_f")
       use, intrinsic :: iso_c_binding, only: c_bool, c_char
       import :: yggdtype
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: type
       logical(kind = c_bool), value, intent(in) :: use_generic
       type(yggdtype) :: out
     end function create_dtype_default_c
     !> @brief Construct a Scalar type object.
     !> @param[in] subtype Name of the scalar subtype (e.g. int, uint, float,
     !>   bytes).
     !> @param[in] precision Precision of the scalar in bits.
     !> @param[in] units Units for scalar. (e.g. "cm", "g", "" for unitless)
     !> @param[in] use_generic If true, serialized or deserialized
     !>   objects will be expected to be generic_t instances.
     !> @returns Type structure.
     function create_dtype_scalar_c(subtype, precision, units, use_generic) &
          result(out) &
          bind(c, name="create_dtype_scalar_f")
       use, intrinsic :: iso_c_binding, only: c_bool, c_char, c_size_t
       import :: yggdtype
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: subtype
       integer(kind = c_size_t), value, intent(in) :: precision
       character(kind = c_char), dimension(*), intent(in) :: units
       logical(kind = c_bool), value, intent(in) :: use_generic
       type(yggdtype) :: out
     end function create_dtype_scalar_c
     !> @brief Construct a 1D array type object.
     !> @param[in] subtype Name of the array subtype (e.g. int, uint, float,
     !>   bytes).
     !> @param[in] precision Precision of the array in bits.
     !> @param[in] length Number of elements in the array.
     !> @param[in] units Units for array elements. (e.g. "cm", "g", "" for
     !>   unitless)
     !> @param[in] use_generic If true, serialized or deserialized
     !>   objects will be expected to be generic_t instances.
     !> @returns Type structure.
     function create_dtype_1darray_c(subtype, precision, length, units, use_generic) &
          result(out) &
          bind(c, name="create_dtype_1darray_f")
       use, intrinsic :: iso_c_binding, only: c_bool, c_char, c_size_t
       import :: yggdtype
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: subtype
       integer(kind = c_size_t), value, intent(in) :: precision
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       logical(kind = c_bool), value, intent(in) :: use_generic
       type(yggdtype) :: out
     end function create_dtype_1darray_c
     !> @brief Construct a ND array type object.
     !> @param[in] subtype Name of the array subtype (e.g. int, uint, float,
     !>   bytes).
     !> @param[in] precision Precision of the array in bits.
     !> @param[in] ndim Number of dimensions in the array (and therefore also
     !>   the number of elements in shape).
     !> @param[in] shape Pointer to array where each element is the
     !>   size of the array in that dimension.
     !> @param[in] units Units for array elements. (e.g. "cm", "g", "" for
     !>   unitless)
     !> @param[in] use_generic If true, serialized or deserialized
     !>   objects will be expected to be generic_t instances.
     !> @returns Type structure.
     function create_dtype_ndarray_c(subtype, precision, ndim, shape, units, use_generic) &
          result(out) &
          bind(c, name="create_dtype_ndarray_f")
       use, intrinsic :: iso_c_binding, only: c_bool, c_char, c_ptr, c_size_t
       import :: yggdtype
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: subtype
       integer(kind = c_size_t), value, intent(in) :: precision
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       logical(kind = c_bool), value, intent(in) :: use_generic
       type(yggdtype) :: out
     end function create_dtype_ndarray_c
     !> @brief Construct a JSON array type object.
     !> @param[in] nitems Number of types in items.
     !> @param[in] items Pointer to array of types describing the array
     !>   elements.
     !> @param[in] use_generic If true, serialized or deserialized
     !>   objects will be expected to be generic_t instances.
     !> @returns Type structure.
     function create_dtype_json_array_c(nitems, items, use_generic) &
          result(out) &
          bind(c, name="create_dtype_json_array_f")
       use, intrinsic :: iso_c_binding, only: c_bool, c_ptr, c_size_t
       import :: yggdtype
       implicit none
       integer(kind = c_size_t), value, intent(in) :: nitems
       type(c_ptr), value :: items
       logical(kind = c_bool), value, intent(in) :: use_generic
       type(yggdtype) :: out
     end function create_dtype_json_array_c
     !> @brief Construct a JSON object type object.
     !> @param[in] nitems Number of items in keys and values.
     !> @param[in] keys Pointer to array of keys for each type.
     !> @param[in] values Pointer to array of types describing the values
     !>   for each key.
     !> @param[in] use_generic If true, serialized or deserialized
     !>   objects will be expected to be generic_t instances.
     !> @returns Type structure.
     function create_dtype_json_object_c(nitems, keys, values, use_generic) &
          result(out) &
          bind(c, name="create_dtype_json_object_f")
       use, intrinsic :: iso_c_binding, only: c_bool, c_ptr, c_size_t
       import :: yggdtype
       implicit none
       integer(kind = c_size_t), value, intent(in) :: nitems
       type(c_ptr), value, intent(in) :: keys
       type(c_ptr), value :: values
       logical(kind = c_bool), value, intent(in) :: use_generic
       type(yggdtype) :: out
     end function create_dtype_json_object_c
     !> @brief Construct a Ply type object.
     !> @param[in] use_generic If true, serialized or deserialized
     !>   objects will be expected to be generic_t instances.
     !> @returns Type structure.
     function create_dtype_ply_c(use_generic) &
          result(out) &
          bind(c, name="create_dtype_ply_f")
       use, intrinsic :: iso_c_binding, only: c_bool
       import :: yggdtype
       implicit none
       logical(kind = c_bool), value, intent(in) :: use_generic
       type(yggdtype) :: out
     end function create_dtype_ply_c
     !> @brief Construct a Obj type object.
     !> @param[in] use_generic If true, serialized or deserialized
     !>   objects will be expected to be generic_t instances.
     !> @returns Type structure.
     function create_dtype_obj_c(use_generic) &
          result(out) &
          bind(c, name="create_dtype_obj_f")
       use, intrinsic :: iso_c_binding, only: c_bool
       import :: yggdtype
       implicit none
       logical(kind = c_bool), value, intent(in) :: use_generic
       type(yggdtype) :: out
     end function create_dtype_obj_c
     !> @brief Construct an AsciiTable type object.
     !> @param[in] format_str C-style format string that will be used to
     !>   determine the type of elements in arrays that will be
     !>   serialized or deserialized using the resulting type.
     !> @param[in] as_array If true, the types will be arrays. Otherwise they
     !>   will be scalars.
     !> @param[in] use_generic If true, serialized or deserialized
     !>   objects will be expected to be generic_t instances.
     !> @returns Type structure.
     function create_dtype_ascii_table_c(format_str, as_array, use_generic) &
          result(out) &
          bind(c, name="create_dtype_ascii_table_f")
       use, intrinsic :: iso_c_binding, only: c_bool, c_char
       import :: yggdtype
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: format_str
       logical(kind = c_bool), value, intent(in) :: as_array
       logical(kind = c_bool), value, intent(in) :: use_generic
       type(yggdtype) :: out
     end function create_dtype_ascii_table_c
     !> @brief Construct a type object based on the provided format string.
     !> @param[in] format_str C-style format string that will be used to
     !>   determine the type of elements in arrays that will be
     !>   serialized or deserialized using the resulting type.
     !> @param[in] as_array If true, the types will be arrays. Otherwise they
     !>   will be scalars.
     !> @param[in] use_generic If true, serialized or deserialized
     !>   objects will be expected to be generic_t instances.
     !> @returns Type structure.
     function create_dtype_format_c(format_str, as_array, use_generic) &
          result(out) &
          bind(c, name="create_dtype_format_f")
       use, intrinsic :: iso_c_binding, only: c_bool, c_char
       import :: yggdtype
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: format_str
       logical(kind = c_bool), value, intent(in) :: as_array
       logical(kind = c_bool), value, intent(in) :: use_generic
       type(yggdtype) :: out
     end function create_dtype_format_c
     !> @brief Construct a type object for Python objects.
     !> @param[in] type Type string.
     !> @param[in] use_generic If true, serialized or deserialized
     !>   objects will be expected to be generic_t instances.
     !> @returns Type structure.
     function create_dtype_pyobj_c(type, use_generic) &
          result(out) &
          bind(c, name="create_dtype_pyobj_f")
       use, intrinsic :: iso_c_binding, only: c_bool, c_char
       import :: yggdtype
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: type
       logical(kind = c_bool), value, intent(in) :: use_generic
       type(yggdtype) :: out
     end function create_dtype_pyobj_c
     !> @brief Construct a type object for Python object instances.
     !> @param[in] class_name Python class name that instance should be a
     !>   subclass of. If NULL or an empty string, no class constraints will
     !>   be placed on the instance.
     !> @param[in] args_dtype Datatype describing the arguments creating the
     !>   instance. The datatype will be consumed and does not need to be freed.
     !> @param[in] kwargs_dtype Datatype describing the keyword arguments
     !>   creating the instance. The datatype will be consumed and does not
     !>   need to be freed.
     !> @param[in] use_generic If true, serialized or deserialized
     !>   objects will be expected to be generic_t instances.
     !> @returns Type structure.
     function create_dtype_pyinst_c(class_name, args_dtype, kwargs_dtype, use_generic) &
          result(out) &
          bind(c, name="create_dtype_pyinst_f")
       use, intrinsic :: iso_c_binding, only: c_bool, c_char, c_ptr
       import :: yggdtype
       implicit none
       character(kind = c_char), dimension(*), intent(in) :: class_name
       type(c_ptr), value :: args_dtype
       type(c_ptr), value :: kwargs_dtype
       logical(kind = c_bool), value, intent(in) :: use_generic
       type(yggdtype) :: out
     end function create_dtype_pyinst_c
     !> @brief Construct a type object for a schema.
     !> @param[in] use_generic If true, serialized or deserialized
     !>   objects will be expected to be generic_t instances.
     !> @returns Type structure.
     function create_dtype_schema_c(use_generic) &
          result(out) &
          bind(c, name="create_dtype_schema_f")
       use, intrinsic :: iso_c_binding, only: c_bool
       import :: yggdtype
       implicit none
       logical(kind = c_bool), value, intent(in) :: use_generic
       type(yggdtype) :: out
     end function create_dtype_schema_c
     !> @brief Construct a type object for receiving any type.
     !> @param[in] use_generic If true, serialized or deserialized
     !>   objects will be expected to be generic_t instances.
     !> @returns Type structure.
     function create_dtype_any_c(use_generic) &
          result(out) &
          bind(c, name="create_dtype_any_f")
       use, intrinsic :: iso_c_binding, only: c_bool
       import :: yggdtype
       implicit none
       logical(kind = c_bool), value, intent(in) :: use_generic
       type(yggdtype) :: out
     end function create_dtype_any_c
     !> @brief Wrapper for freeing rapidjson::Document class wrapper struct.
     !> @param[in] dtype Wrapper struct for C++ Metadata.
     !> @returns int 0 if free was successfull, -1 if there was an error.
     function destroy_dtype_c(dtype) &
          result(out) &
          bind(c, name="destroy_dtype_f")
       use, intrinsic :: iso_c_binding, only: c_int, c_ptr
       implicit none
       type(c_ptr), value :: dtype
       integer(kind = c_int) :: out
     end function destroy_dtype_c
     !> @brief Get a copy of a type structure.
     !> @param[in] dtype Wrapper struct for C++ Metadata.
     !> @returns: Type class.
     function copy_dtype_c(dtype) &
          result(out) &
          bind(c, name="copy_dtype_f")
       import :: yggdtype
       implicit none
       type(yggdtype), value, intent(in) :: dtype
       type(yggdtype) :: out
     end function copy_dtype_c
     !> @brief Determine if a type structure indicates that generic objects
     !>   should be used.
     !> @param[in] dtype Wrapper struct for C++ Metadata.
     !> @returns 1 if generic objects will be used, 0 if not, -1 for errors.
     function dtype_uses_generic_c(dtype) &
          result(out) &
          bind(c, name="dtype_uses_generic_f")
       use, intrinsic :: iso_c_binding, only: c_int
       import :: yggdtype
       implicit none
       type(yggdtype), value :: dtype
       integer(kind = c_int) :: out
     end function dtype_uses_generic_c
     !> @brief Initialize empty obj structure.
     !> @returns obj_t Obj structure.
     function init_obj_c() &
          result(out) &
          bind(c, name="init_obj_f")
       import :: yggobj
       implicit none
       type(yggobj) :: out
     end function init_obj_c
     !> @brief Create a obj structure with generated data.
     !> @returns obj_t Obj structure.
     function generate_obj_c() &
          result(out) &
          bind(c, name="generate_obj_f")
       import :: yggobj
       implicit none
       type(yggobj) :: out
     end function generate_obj_c
     !> @brief Free obj structure.
     !> @param[in] p *obj_t Pointer to obj structure.
     subroutine free_obj_c(p) &
          bind(c, name="free_obj_f")
       use, intrinsic :: iso_c_binding, only: c_ptr
       implicit none
       type(c_ptr), value :: p
     end subroutine free_obj_c
     !> @brief Set parameters from a rapidjson::ObjWavefront object.
     !> @param[in,out] x Structure to modify.
     !> @param[in] obj rapidjson::ObjWavefront object to copy.
     !> @param[in] copy If 1, the provided object will be copied, otherwise
     !>   the pointer will be added to the structured directly and it will
     !>   be freed on destruction.
     subroutine set_obj_c(x, obj, copy) &
          bind(c, name="set_obj_f")
       use, intrinsic :: iso_c_binding, only: c_int, c_ptr
       implicit none
       type(c_ptr), value :: x
       type(c_ptr), value :: obj
       integer(kind = c_int), value :: copy
     end subroutine set_obj_c
     !> @brief Copy an obj structure.
     !> @param[in] src obj_t Obj structure that should be copied.
     !> @returns Copy of obj structure.
     function copy_obj_c(src) &
          result(out) &
          bind(c, name="copy_obj_f")
       import :: yggobj
       implicit none
       type(yggobj), value :: src
       type(yggobj) :: out
     end function copy_obj_c
     !> @brief Display the information contained by an Obj struct.
     !> @param[in] p obj_t Obj structure.
     !> @param[in] indent const char* Indentation that should be added to
     !>   each line.
     subroutine display_obj_indent_c(p, indent) &
          bind(c, name="display_obj_indent_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: yggobj
       implicit none
       type(yggobj), value :: p
       character(kind = c_char), dimension(*), intent(in) :: indent
     end subroutine display_obj_indent_c
     !> @brief Display the information contained by an Obj struct.
     !> @param[in] p obj_t Obj structure.
     subroutine display_obj_c(p) &
          bind(c, name="display_obj_f")
       import :: yggobj
       implicit none
       type(yggobj), value :: p
     end subroutine display_obj_c
     !> @brief Get the number of elements of a certain type in the structure.
     !> @param[in] p obj_t ObjWavefront structure.
     !> @param[in] name Name of element type to count.
     !> @returns Number of elements of the specified type.
     function nelements_obj_c(p, name) &
          result(out) &
          bind(c, name="nelements_obj_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int
       import :: yggobj
       implicit none
       type(yggobj), value :: p
       character(kind = c_char), dimension(*), intent(in) :: name
       integer(kind = c_int) :: out
     end function nelements_obj_c
     !> @brief Compare two obj structures for equality.
     !> @param[in] a First structure for comparison.
     !> @param[in] b Second structure for comparison.
     !> @returns true if a and b are equal, false otherwise.
     function compare_obj_c(a, b) &
          result(out) &
          bind(c, name="compare_obj_f")
       use, intrinsic :: iso_c_binding, only: c_bool
       import :: yggobj
       implicit none
       type(yggobj), value, intent(in) :: a
       type(yggobj), value, intent(in) :: b
       logical(kind = c_bool) :: out
     end function compare_obj_c
     !> @brief Initialize empty ply structure.
     !> @returns ply_t Ply structure.
     function init_ply_c() &
          result(out) &
          bind(c, name="init_ply_f")
       import :: yggply
       implicit none
       type(yggply) :: out
     end function init_ply_c
     !> @brief Create a ply structure with generated data.
     !> @returns ply_t Ply structure.
     function generate_ply_c() &
          result(out) &
          bind(c, name="generate_ply_f")
       import :: yggply
       implicit none
       type(yggply) :: out
     end function generate_ply_c
     !> @brief Free ply structure.
     !> @param[in] p *ply_t Pointer to ply structure.
     subroutine free_ply_c(p) &
          bind(c, name="free_ply_f")
       use, intrinsic :: iso_c_binding, only: c_ptr
       implicit none
       type(c_ptr), value :: p
     end subroutine free_ply_c
     !> @brief Set parameters from a rapidjson::Ply object.
     !> @param[in,out] x Structure to modify.
     !> @param[in] ply rapidjson::Ply object to copy.
     !> @param[in] copy If 1, the provided object will be copied, otherwise
     !>   the pointer will be added to the structured directly and it will
     !>   be freed on destruction.
     subroutine set_ply_c(x, ply, copy) &
          bind(c, name="set_ply_f")
       use, intrinsic :: iso_c_binding, only: c_int, c_ptr
       implicit none
       type(c_ptr), value :: x
       type(c_ptr), value :: ply
       integer(kind = c_int), value :: copy
     end subroutine set_ply_c
     !> @brief Copy an ply structure.
     !> @param[in] src ply_t Ply structure that should be copied.
     !> @returns Copy of ply structure.
     function copy_ply_c(src) &
          result(out) &
          bind(c, name="copy_ply_f")
       import :: yggply
       implicit none
       type(yggply), value :: src
       type(yggply) :: out
     end function copy_ply_c
     !> @brief Display the information contained by an Ply struct.
     !> @param[in] p ply_t Ply structure.
     !> @param[in] indent const char* Indentation that should be added to
     !>   each line.
     subroutine display_ply_indent_c(p, indent) &
          bind(c, name="display_ply_indent_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: yggply
       implicit none
       type(yggply), value :: p
       character(kind = c_char), dimension(*), intent(in) :: indent
     end subroutine display_ply_indent_c
     !> @brief Display the information contained by an Ply struct.
     !> @param[in] p ply_t Ply structure.
     subroutine display_ply_c(p) &
          bind(c, name="display_ply_f")
       import :: yggply
       implicit none
       type(yggply), value :: p
     end subroutine display_ply_c
     !> @brief Get the number of elements of a certain type in the structure.
     !> @param[in] p ply_t Ply structure.
     !> @param[in] name Name of element type to count.
     !> @returns Number of elements of the specified type.
     function nelements_ply_c(p, name) &
          result(out) &
          bind(c, name="nelements_ply_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int
       import :: yggply
       implicit none
       type(yggply), value :: p
       character(kind = c_char), dimension(*), intent(in) :: name
       integer(kind = c_int) :: out
     end function nelements_ply_c
     !> @brief Compare two ply structures for equality.
     !> @param[in] a First structure for comparison.
     !> @param[in] b Second structure for comparison.
     !> @returns true if a and b are equal, false otherwise.
     function compare_ply_c(a, b) &
          result(out) &
          bind(c, name="compare_ply_f")
       use, intrinsic :: iso_c_binding, only: c_bool
       import :: yggply
       implicit none
       type(yggply), value, intent(in) :: a
       type(yggply), value, intent(in) :: b
       logical(kind = c_bool) :: out
     end function compare_ply_c
     !> @brief Get the number of elements in a array
     !> @param[in] x generic_t Generic object that is presumed to contain a
     !>   array
     !> @returns size_t Number of elements in array
     function generic_array_get_size_c(x) &
          result(out) &
          bind(c, name="generic_array_get_size_f")
       use, intrinsic :: iso_c_binding, only: c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t) :: out
     end function generic_array_get_size_c
     !> @brief Get the number of elements in a object
     !> @param[in] x generic_t Generic object that is presumed to contain a
     !>   object
     !> @returns size_t Number of elements in object
     function generic_object_get_size_c(x) &
          result(out) &
          bind(c, name="generic_object_get_size_f")
       use, intrinsic :: iso_c_binding, only: c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t) :: out
     end function generic_object_get_size_c
     !> @brief Set a given generic item to a null
     !> @param[in] x The generic item to set
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_null_c(x, value) &
          result(out) &
          bind(c, name="generic_set_null_f")
       use, intrinsic :: iso_c_binding, only: c_int, c_ptr
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_set_null_c
     !> @brief Set a given generic item to a boolean
     !> @param[in] x The generic item to set
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_bool_c(x, value) &
          result(out) &
          bind(c, name="generic_set_bool_f")
       use, intrinsic :: iso_c_binding, only: c_bool, c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       logical(kind = c_bool), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_set_bool_c
     !> @brief Set a given generic item to a integer
     !> @param[in] x The generic item to set
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_integer_c(x, value) &
          result(out) &
          bind(c, name="generic_set_integer_f")
       use, intrinsic :: iso_c_binding, only: c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_int), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_set_integer_c
     !> @brief Set a given generic item to a number
     !> @param[in] x The generic item to set
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_number_c(x, value) &
          result(out) &
          bind(c, name="generic_set_number_f")
       use, intrinsic :: iso_c_binding, only: c_double, c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       real(kind = c_double), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_set_number_c
     !> @brief Set a given generic item to a string
     !> @param[in] x The generic item to set
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_string_c(x, value) &
          result(out) &
          bind(c, name="generic_set_string_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_set_string_c
     !> @brief Set a given generic item to a item
     !> @param[in] x The generic item to set
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_item_c(x, type, value) &
          result(out) &
          bind(c, name="generic_set_item_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: type
       type(c_ptr), value :: value
       integer(kind = c_int) :: out
     end function generic_set_item_c
     !> @brief Set a given generic item to a array
     !> @param[in] x The generic item to set
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_array_c(x, value) &
          result(out) &
          bind(c, name="generic_set_array_f")
       use, intrinsic :: iso_c_binding, only: c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(ygggeneric), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_set_array_c
     !> @brief Set a given generic item to a object
     !> @param[in] x The generic item to set
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_object_c(x, value) &
          result(out) &
          bind(c, name="generic_set_object_f")
       use, intrinsic :: iso_c_binding, only: c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(ygggeneric), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_set_object_c
     !> @brief Set a given generic item to a ply
     !> @param[in] x The generic item to set
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_ply_c(x, value) &
          result(out) &
          bind(c, name="generic_set_ply_f")
       use, intrinsic :: iso_c_binding, only: c_int
       import :: ygggeneric, yggply
       implicit none
       type(ygggeneric), value :: x
       type(yggply), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_set_ply_c
     !> @brief Set a given generic item to a obj
     !> @param[in] x The generic item to set
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_obj_c(x, value) &
          result(out) &
          bind(c, name="generic_set_obj_f")
       use, intrinsic :: iso_c_binding, only: c_int
       import :: ygggeneric, yggobj
       implicit none
       type(ygggeneric), value :: x
       type(yggobj), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_set_obj_c
     !> @brief Set a given generic item to a class
     !> @param[in] x The generic item to set
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_python_class_c(x, value) &
          result(out) &
          bind(c, name="generic_set_python_class_f")
       use, intrinsic :: iso_c_binding, only: c_int
       import :: ygggeneric, yggpython
       implicit none
       type(ygggeneric), value :: x
       type(yggpython), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_set_python_class_c
     !> @brief Set a given generic item to a function
     !> @param[in] x The generic item to set
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_python_function_c(x, value) &
          result(out) &
          bind(c, name="generic_set_python_function_f")
       use, intrinsic :: iso_c_binding, only: c_int
       import :: ygggeneric, yggpython
       implicit none
       type(ygggeneric), value :: x
       type(yggpython), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_set_python_function_c
     !> @brief Set a given generic item to a instance
     !> @param[in] x The generic item to set
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_python_instance_c(x, value) &
          result(out) &
          bind(c, name="generic_set_python_instance_f")
       use, intrinsic :: iso_c_binding, only: c_int
       import :: ygggeneric, yggpython
       implicit none
       type(ygggeneric), value :: x
       type(yggpython), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_set_python_instance_c
     !> @brief Set a given generic item to a scalar
     !> @param[in] x The generic item to set
     !> @param[in] value Pointer to the memory containing the value to assign to x
     !> @param[in] subtype Subtype of data contained in value
     !> @param[in] precision The precision of the data in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_scalar_c(x, value, subtype, precision, units) &
          result(out) &
          bind(c, name="generic_set_scalar_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: subtype
       integer(kind = c_size_t), value, intent(in) :: precision
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_scalar_c
     !> @brief Set a given generic item to a int scalar
     !> @param[in] x The generic item to set
     !> @param[in] value The value to assign to x
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_int16_c(x, value, units) &
          result(out) &
          bind(c, name="generic_set_int16_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_int16_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_int16_t), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_int16_c
     !> @brief Set a given generic item to a int scalar
     !> @param[in] x The generic item to set
     !> @param[in] value The value to assign to x
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_int32_c(x, value, units) &
          result(out) &
          bind(c, name="generic_set_int32_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_int32_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_int32_t), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_int32_c
     !> @brief Set a given generic item to a int scalar
     !> @param[in] x The generic item to set
     !> @param[in] value The value to assign to x
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_int64_c(x, value, units) &
          result(out) &
          bind(c, name="generic_set_int64_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_int64_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_int64_t), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_int64_c
     !> @brief Set a given generic item to a float scalar
     !> @param[in] x The generic item to set
     !> @param[in] value The value to assign to x
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_float_c(x, value, units) &
          result(out) &
          bind(c, name="generic_set_float_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_float, c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       real(kind = c_float), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_float_c
     !> @brief Set a given generic item to a float scalar
     !> @param[in] x The generic item to set
     !> @param[in] value The value to assign to x
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_double_c(x, value, units) &
          result(out) &
          bind(c, name="generic_set_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_double, c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       real(kind = c_double), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_double_c
     !> @brief Set a given generic item to a complex scalar
     !> @param[in] x The generic item to set
     !> @param[in] value The value to assign to x
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_complex_float_c(x, value, units) &
          result(out) &
          bind(c, name="generic_set_complex_float_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_float_complex, c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       complex(kind = c_float_complex), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_complex_float_c
     !> @brief Set a given generic item to a complex scalar
     !> @param[in] x The generic item to set
     !> @param[in] value The value to assign to x
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_complex_double_c(x, value, units) &
          result(out) &
          bind(c, name="generic_set_complex_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_double_complex, c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       complex(kind = c_double_complex), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_complex_double_c
     !> @brief Set a given generic item to a 1darray
     !> @param[in] x The generic item to set
     !> @param[in] value Pointer to the memory containing the array to assign
     !>   to x
     !> @param[in] subtype Subtype of data contained in value
     !> @param[in] precision The precision of the elements in value
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_1darray_c(x, value, subtype, precision, length, units) &
          result(out) &
          bind(c, name="generic_set_1darray_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: subtype
       integer(kind = c_size_t), value, intent(in) :: precision
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_1darray_c
     !> @brief Set a given generic item to a int 1darray
     !> @param[in] x The generic item to set
     !> @param[in] value The array of values to assign to x
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_1darray_int16_c(x, value, length, units) &
          result(out) &
          bind(c, name="generic_set_1darray_int16_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_1darray_int16_c
     !> @brief Set a given generic item to a int 1darray
     !> @param[in] x The generic item to set
     !> @param[in] value The array of values to assign to x
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_1darray_int32_c(x, value, length, units) &
          result(out) &
          bind(c, name="generic_set_1darray_int32_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_1darray_int32_c
     !> @brief Set a given generic item to a int 1darray
     !> @param[in] x The generic item to set
     !> @param[in] value The array of values to assign to x
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_1darray_int64_c(x, value, length, units) &
          result(out) &
          bind(c, name="generic_set_1darray_int64_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_1darray_int64_c
     !> @brief Set a given generic item to a float 1darray
     !> @param[in] x The generic item to set
     !> @param[in] value The array of values to assign to x
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_1darray_float_c(x, value, length, units) &
          result(out) &
          bind(c, name="generic_set_1darray_float_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_1darray_float_c
     !> @brief Set a given generic item to a float 1darray
     !> @param[in] x The generic item to set
     !> @param[in] value The array of values to assign to x
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_1darray_double_c(x, value, length, units) &
          result(out) &
          bind(c, name="generic_set_1darray_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_1darray_double_c
     !> @brief Set a given generic item to a complex 1darray
     !> @param[in] x The generic item to set
     !> @param[in] value The array of values to assign to x
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_1darray_complex_float_c(x, value, length, units) &
          result(out) &
          bind(c, name="generic_set_1darray_complex_float_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_1darray_complex_float_c
     !> @brief Set a given generic item to a complex 1darray
     !> @param[in] x The generic item to set
     !> @param[in] value The array of values to assign to x
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_1darray_complex_double_c(x, value, length, units) &
          result(out) &
          bind(c, name="generic_set_1darray_complex_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_1darray_complex_double_c
     !> @brief Set a given generic item to a ndarray
     !> @param[in] x The generic item to set
     !> @param[in] value Pointer to the memory containing the array to assign
     !>   to x
     !> @param[in] subtype Subtype of data contained in value
     !> @param[in] precision The precision of the elements in value
     !> @param[in] ndim The number of dimensions in value
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_ndarray_c(x, value, subtype, precision, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_set_ndarray_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: subtype
       integer(kind = c_size_t), value, intent(in) :: precision
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_ndarray_c
     !> @brief Set a given generic item to a int ndarray
     !> @param[in] x The generic item to set
     !> @param[in] value The array of values to assign to x
     !>   in row-major order
     !> @param[in] ndim The number of dimensions in value, or 0 on error
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_ndarray_int16_c(x, value, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_set_ndarray_int16_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_ndarray_int16_c
     !> @brief Set a given generic item to a int ndarray
     !> @param[in] x The generic item to set
     !> @param[in] value The array of values to assign to x
     !>   in row-major order
     !> @param[in] ndim The number of dimensions in value, or 0 on error
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_ndarray_int32_c(x, value, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_set_ndarray_int32_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_ndarray_int32_c
     !> @brief Set a given generic item to a int ndarray
     !> @param[in] x The generic item to set
     !> @param[in] value The array of values to assign to x
     !>   in row-major order
     !> @param[in] ndim The number of dimensions in value, or 0 on error
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_ndarray_int64_c(x, value, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_set_ndarray_int64_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_ndarray_int64_c
     !> @brief Set a given generic item to a float ndarray
     !> @param[in] x The generic item to set
     !> @param[in] value The array of values to assign to x
     !>   in row-major order
     !> @param[in] ndim The number of dimensions in value, or 0 on error
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_ndarray_float_c(x, value, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_set_ndarray_float_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_ndarray_float_c
     !> @brief Set a given generic item to a float ndarray
     !> @param[in] x The generic item to set
     !> @param[in] value The array of values to assign to x
     !>   in row-major order
     !> @param[in] ndim The number of dimensions in value, or 0 on error
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_ndarray_double_c(x, value, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_set_ndarray_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_ndarray_double_c
     !> @brief Set a given generic item to a complex ndarray
     !> @param[in] x The generic item to set
     !> @param[in] value The array of values to assign to x
     !>   in row-major order
     !> @param[in] ndim The number of dimensions in value, or 0 on error
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_ndarray_complex_float_c(x, value, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_set_ndarray_complex_float_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_ndarray_complex_float_c
     !> @brief Set a given generic item to a complex ndarray
     !> @param[in] x The generic item to set
     !> @param[in] value The array of values to assign to x
     !>   in row-major order
     !> @param[in] ndim The number of dimensions in value, or 0 on error
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_ndarray_complex_double_c(x, value, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_set_ndarray_complex_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_ndarray_complex_double_c
     !> @brief Set a given generic item to a schema
     !> @param[in] x The generic item to set
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_schema_c(x, value) &
          result(out) &
          bind(c, name="generic_set_schema_f")
       use, intrinsic :: iso_c_binding, only: c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(ygggeneric), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_set_schema_c
     !> @brief Set a given generic item to a any
     !> @param[in] x The generic item to set
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_any_c(x, value) &
          result(out) &
          bind(c, name="generic_set_any_f")
       use, intrinsic :: iso_c_binding, only: c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(ygggeneric), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_set_any_c
     !> @brief Get a null from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @returns Value from x
     function generic_get_null_c(x) &
          result(out) &
          bind(c, name="generic_get_null_f")
       use, intrinsic :: iso_c_binding, only: c_ptr
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr) :: out
     end function generic_get_null_c
     !> @brief Get a boolean from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @returns Value from x
     function generic_get_bool_c(x) &
          result(out) &
          bind(c, name="generic_get_bool_f")
       use, intrinsic :: iso_c_binding, only: c_bool
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       logical(kind = c_bool) :: out
     end function generic_get_bool_c
     !> @brief Get a integer from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @returns Value from x
     function generic_get_integer_c(x) &
          result(out) &
          bind(c, name="generic_get_integer_f")
       use, intrinsic :: iso_c_binding, only: c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_int) :: out
     end function generic_get_integer_c
     !> @brief Get a number from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @returns Value from x
     function generic_get_number_c(x) &
          result(out) &
          bind(c, name="generic_get_number_f")
       use, intrinsic :: iso_c_binding, only: c_double
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       real(kind = c_double) :: out
     end function generic_get_number_c
     !> @brief Get a string from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @returns Value from x
     function generic_get_string_c(x) &
          result(out) &
          bind(c, name="generic_get_string_f")
       use, intrinsic :: iso_c_binding, only: c_ptr
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr) :: out
     end function generic_get_string_c
     !> @brief Get the raw item data
     !> @param[in] x Generic item to retrieve data from
     !> @param[in] type Type of item to retrieve
     !> @returns Pointer to data containing raw item data, NULL on error
     function generic_get_item_c(x, type) &
          result(out) &
          bind(c, name="generic_get_item_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: type
       type(c_ptr) :: out
     end function generic_get_item_c
     !> @brief Get the size of the raw item data
     !> @param[in] x Generic item to retrieve data size from
     !> @param[in] type Type of item to retrieve
     !> @returns Number of bytes in raw item data, 0 on error
     function generic_get_item_nbytes_c(x, type) &
          result(out) &
          bind(c, name="generic_get_item_nbytes_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: type
       integer(kind = c_int) :: out
     end function generic_get_item_nbytes_c
     !> @brief Get a array from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @returns Value from x
     function generic_get_array_c(x) &
          result(out) &
          bind(c, name="generic_get_array_f")
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(ygggeneric) :: out
     end function generic_get_array_c
     !> @brief Get a object from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @returns Value from x
     function generic_get_object_c(x) &
          result(out) &
          bind(c, name="generic_get_object_f")
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(ygggeneric) :: out
     end function generic_get_object_c
     !> @brief Get a ply from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @returns Value from x
     function generic_get_ply_c(x) &
          result(out) &
          bind(c, name="generic_get_ply_f")
       import :: ygggeneric, yggply
       implicit none
       type(ygggeneric), value :: x
       type(yggply) :: out
     end function generic_get_ply_c
     !> @brief Get a obj from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @returns Value from x
     function generic_get_obj_c(x) &
          result(out) &
          bind(c, name="generic_get_obj_f")
       import :: ygggeneric, yggobj
       implicit none
       type(ygggeneric), value :: x
       type(yggobj) :: out
     end function generic_get_obj_c
     !> @brief Get a class from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @returns Value from x
     function generic_get_python_class_c(x) &
          result(out) &
          bind(c, name="generic_get_python_class_f")
       import :: ygggeneric, yggpython
       implicit none
       type(ygggeneric), value :: x
       type(yggpython) :: out
     end function generic_get_python_class_c
     !> @brief Get a function from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @returns Value from x
     function generic_get_python_function_c(x) &
          result(out) &
          bind(c, name="generic_get_python_function_f")
       import :: ygggeneric, yggpython
       implicit none
       type(ygggeneric), value :: x
       type(yggpython) :: out
     end function generic_get_python_function_c
     !> @brief Get a instance from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @returns Value from x
     function generic_get_python_instance_c(x) &
          result(out) &
          bind(c, name="generic_get_python_instance_f")
       import :: ygggeneric, yggpython
       implicit none
       type(ygggeneric), value :: x
       type(yggpython) :: out
     end function generic_get_python_instance_c
     !> @brief Get a scalar from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @param[in] subtype Subtype of data to return
     !> @param[in] precision Precision of the data to return
     !> @returns Pointer to value in x
     function generic_get_scalar_c(x, subtype, precision) &
          result(out) &
          bind(c, name="generic_get_scalar_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: subtype
       integer(kind = c_size_t), value, intent(in) :: precision
       type(c_ptr) :: out
     end function generic_get_scalar_c
     !> @brief Get a int scalar from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @returns Value from x
     function generic_get_int16_c(x) &
          result(out) &
          bind(c, name="generic_get_int16_f")
       use, intrinsic :: iso_c_binding, only: c_int16_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_int16_t) :: out
     end function generic_get_int16_c
     !> @brief Get a int scalar from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @returns Value from x
     function generic_get_int32_c(x) &
          result(out) &
          bind(c, name="generic_get_int32_f")
       use, intrinsic :: iso_c_binding, only: c_int32_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_int32_t) :: out
     end function generic_get_int32_c
     !> @brief Get a int scalar from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @returns Value from x
     function generic_get_int64_c(x) &
          result(out) &
          bind(c, name="generic_get_int64_f")
       use, intrinsic :: iso_c_binding, only: c_int64_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_int64_t) :: out
     end function generic_get_int64_c
     !> @brief Get a float scalar from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @returns Value from x
     function generic_get_float_c(x) &
          result(out) &
          bind(c, name="generic_get_float_f")
       use, intrinsic :: iso_c_binding, only: c_float
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       real(kind = c_float) :: out
     end function generic_get_float_c
     !> @brief Get a float scalar from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @returns Value from x
     function generic_get_double_c(x) &
          result(out) &
          bind(c, name="generic_get_double_f")
       use, intrinsic :: iso_c_binding, only: c_double
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       real(kind = c_double) :: out
     end function generic_get_double_c
     !> @brief Get a complex scalar from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @returns Value from x
     function generic_get_complex_float_c(x) &
          result(out) &
          bind(c, name="generic_get_complex_float_f")
       use, intrinsic :: iso_c_binding, only: c_float_complex
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       complex(kind = c_float_complex) :: out
     end function generic_get_complex_float_c
     !> @brief Get a complex scalar from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @returns Value from x
     function generic_get_complex_double_c(x) &
          result(out) &
          bind(c, name="generic_get_complex_double_f")
       use, intrinsic :: iso_c_binding, only: c_double_complex
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       complex(kind = c_double_complex) :: out
     end function generic_get_complex_double_c
     !> @brief Get a 1darray from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @param[in] subtype Subtype of data to return
     !> @param[in] precision Precision of the data to return
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_get_1darray_c(x, subtype, precision, value) &
          result(out) &
          bind(c, name="generic_get_1darray_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: subtype
       integer(kind = c_size_t), value, intent(in) :: precision
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_get_1darray_c
     !> @brief Get a int 1darray from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_get_1darray_int16_c(x, value) &
          result(out) &
          bind(c, name="generic_get_1darray_int16_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_get_1darray_int16_c
     !> @brief Get a int 1darray from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_get_1darray_int32_c(x, value) &
          result(out) &
          bind(c, name="generic_get_1darray_int32_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_get_1darray_int32_c
     !> @brief Get a int 1darray from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_get_1darray_int64_c(x, value) &
          result(out) &
          bind(c, name="generic_get_1darray_int64_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_get_1darray_int64_c
     !> @brief Get a float 1darray from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_get_1darray_float_c(x, value) &
          result(out) &
          bind(c, name="generic_get_1darray_float_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_get_1darray_float_c
     !> @brief Get a float 1darray from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_get_1darray_double_c(x, value) &
          result(out) &
          bind(c, name="generic_get_1darray_double_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_get_1darray_double_c
     !> @brief Get a complex 1darray from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_get_1darray_complex_float_c(x, value) &
          result(out) &
          bind(c, name="generic_get_1darray_complex_float_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_get_1darray_complex_float_c
     !> @brief Get a complex 1darray from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_get_1darray_complex_double_c(x, value) &
          result(out) &
          bind(c, name="generic_get_1darray_complex_double_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_get_1darray_complex_double_c
     !> @brief Get a ndarray from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @param[in] subtype Subtype of data to return
     !> @param[in] precision Precision of the data to return
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_get_ndarray_c(x, subtype, precision, value, shape) &
          result(out) &
          bind(c, name="generic_get_ndarray_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: subtype
       integer(kind = c_size_t), value, intent(in) :: precision
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_get_ndarray_c
     !> @brief Get a int ndarray from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_get_ndarray_int16_c(x, value, shape) &
          result(out) &
          bind(c, name="generic_get_ndarray_int16_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_get_ndarray_int16_c
     !> @brief Get a int ndarray from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_get_ndarray_int32_c(x, value, shape) &
          result(out) &
          bind(c, name="generic_get_ndarray_int32_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_get_ndarray_int32_c
     !> @brief Get a int ndarray from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_get_ndarray_int64_c(x, value, shape) &
          result(out) &
          bind(c, name="generic_get_ndarray_int64_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_get_ndarray_int64_c
     !> @brief Get a float ndarray from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_get_ndarray_float_c(x, value, shape) &
          result(out) &
          bind(c, name="generic_get_ndarray_float_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_get_ndarray_float_c
     !> @brief Get a float ndarray from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_get_ndarray_double_c(x, value, shape) &
          result(out) &
          bind(c, name="generic_get_ndarray_double_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_get_ndarray_double_c
     !> @brief Get a complex ndarray from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_get_ndarray_complex_float_c(x, value, shape) &
          result(out) &
          bind(c, name="generic_get_ndarray_complex_float_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_get_ndarray_complex_float_c
     !> @brief Get a complex ndarray from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_get_ndarray_complex_double_c(x, value, shape) &
          result(out) &
          bind(c, name="generic_get_ndarray_complex_double_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_get_ndarray_complex_double_c
     !> @brief Get a schema from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @returns Value from x
     function generic_get_schema_c(x) &
          result(out) &
          bind(c, name="generic_get_schema_f")
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(ygggeneric) :: out
     end function generic_get_schema_c
     !> @brief Get a any from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @returns Value from x
     function generic_get_any_c(x) &
          result(out) &
          bind(c, name="generic_get_any_f")
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(ygggeneric) :: out
     end function generic_get_any_c
     !> @brief Get a null from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @returns Value from x
     function generic_ref_get_null_c(x) &
          result(out) &
          bind(c, name="generic_ref_get_null_f")
       use, intrinsic :: iso_c_binding, only: c_ptr
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       type(c_ptr) :: out
     end function generic_ref_get_null_c
     !> @brief Get a boolean from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @returns Value from x
     function generic_ref_get_bool_c(x) &
          result(out) &
          bind(c, name="generic_ref_get_bool_f")
       use, intrinsic :: iso_c_binding, only: c_bool
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       logical(kind = c_bool) :: out
     end function generic_ref_get_bool_c
     !> @brief Get a integer from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @returns Value from x
     function generic_ref_get_integer_c(x) &
          result(out) &
          bind(c, name="generic_ref_get_integer_f")
       use, intrinsic :: iso_c_binding, only: c_int
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       integer(kind = c_int) :: out
     end function generic_ref_get_integer_c
     !> @brief Get a number from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @returns Value from x
     function generic_ref_get_number_c(x) &
          result(out) &
          bind(c, name="generic_ref_get_number_f")
       use, intrinsic :: iso_c_binding, only: c_double
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       real(kind = c_double) :: out
     end function generic_ref_get_number_c
     !> @brief Get a string from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @returns Value from x
     function generic_ref_get_string_c(x) &
          result(out) &
          bind(c, name="generic_ref_get_string_f")
       use, intrinsic :: iso_c_binding, only: c_ptr
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       type(c_ptr) :: out
     end function generic_ref_get_string_c
     !> @brief Get the raw item data
     !> @param[in] x Generic item to retrieve data from
     !> @param[in] type Type of item to retrieve
     !> @returns Pointer to data containing raw item data, NULL on error
     function generic_ref_get_item_c(x, type) &
          result(out) &
          bind(c, name="generic_ref_get_item_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       character(kind = c_char), dimension(*), intent(in) :: type
       type(c_ptr) :: out
     end function generic_ref_get_item_c
     !> @brief Get the size of the raw item data
     !> @param[in] x Generic item to retrieve data size from
     !> @param[in] type Type of item to retrieve
     !> @returns Number of bytes in raw item data, 0 on error
     function generic_ref_get_item_nbytes_c(x, type) &
          result(out) &
          bind(c, name="generic_ref_get_item_nbytes_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       character(kind = c_char), dimension(*), intent(in) :: type
       integer(kind = c_int) :: out
     end function generic_ref_get_item_nbytes_c
     !> @brief Get a array from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @returns Value from x
     function generic_ref_get_array_c(x) &
          result(out) &
          bind(c, name="generic_ref_get_array_f")
       import :: ygggeneric, ygggenericref
       implicit none
       type(ygggenericref), value :: x
       type(ygggeneric) :: out
     end function generic_ref_get_array_c
     !> @brief Get a object from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @returns Value from x
     function generic_ref_get_object_c(x) &
          result(out) &
          bind(c, name="generic_ref_get_object_f")
       import :: ygggeneric, ygggenericref
       implicit none
       type(ygggenericref), value :: x
       type(ygggeneric) :: out
     end function generic_ref_get_object_c
     !> @brief Get a ply from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @returns Value from x
     function generic_ref_get_ply_c(x) &
          result(out) &
          bind(c, name="generic_ref_get_ply_f")
       import :: ygggenericref, yggply
       implicit none
       type(ygggenericref), value :: x
       type(yggply) :: out
     end function generic_ref_get_ply_c
     !> @brief Get a obj from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @returns Value from x
     function generic_ref_get_obj_c(x) &
          result(out) &
          bind(c, name="generic_ref_get_obj_f")
       import :: ygggenericref, yggobj
       implicit none
       type(ygggenericref), value :: x
       type(yggobj) :: out
     end function generic_ref_get_obj_c
     !> @brief Get a class from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @returns Value from x
     function generic_ref_get_python_class_c(x) &
          result(out) &
          bind(c, name="generic_ref_get_python_class_f")
       import :: ygggenericref, yggpython
       implicit none
       type(ygggenericref), value :: x
       type(yggpython) :: out
     end function generic_ref_get_python_class_c
     !> @brief Get a function from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @returns Value from x
     function generic_ref_get_python_function_c(x) &
          result(out) &
          bind(c, name="generic_ref_get_python_function_f")
       import :: ygggenericref, yggpython
       implicit none
       type(ygggenericref), value :: x
       type(yggpython) :: out
     end function generic_ref_get_python_function_c
     !> @brief Get a instance from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @returns Value from x
     function generic_ref_get_python_instance_c(x) &
          result(out) &
          bind(c, name="generic_ref_get_python_instance_f")
       import :: ygggenericref, yggpython
       implicit none
       type(ygggenericref), value :: x
       type(yggpython) :: out
     end function generic_ref_get_python_instance_c
     !> @brief Get a scalar from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @param[in] subtype Subtype of data to return
     !> @param[in] precision Precision of the data to return
     !> @returns Pointer to value in x
     function generic_ref_get_scalar_c(x, subtype, precision) &
          result(out) &
          bind(c, name="generic_ref_get_scalar_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       character(kind = c_char), dimension(*), intent(in) :: subtype
       integer(kind = c_size_t), value, intent(in) :: precision
       type(c_ptr) :: out
     end function generic_ref_get_scalar_c
     !> @brief Get a int scalar from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @returns Value from x
     function generic_ref_get_int16_c(x) &
          result(out) &
          bind(c, name="generic_ref_get_int16_f")
       use, intrinsic :: iso_c_binding, only: c_int16_t
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       integer(kind = c_int16_t) :: out
     end function generic_ref_get_int16_c
     !> @brief Get a int scalar from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @returns Value from x
     function generic_ref_get_int32_c(x) &
          result(out) &
          bind(c, name="generic_ref_get_int32_f")
       use, intrinsic :: iso_c_binding, only: c_int32_t
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       integer(kind = c_int32_t) :: out
     end function generic_ref_get_int32_c
     !> @brief Get a int scalar from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @returns Value from x
     function generic_ref_get_int64_c(x) &
          result(out) &
          bind(c, name="generic_ref_get_int64_f")
       use, intrinsic :: iso_c_binding, only: c_int64_t
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       integer(kind = c_int64_t) :: out
     end function generic_ref_get_int64_c
     !> @brief Get a float scalar from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @returns Value from x
     function generic_ref_get_float_c(x) &
          result(out) &
          bind(c, name="generic_ref_get_float_f")
       use, intrinsic :: iso_c_binding, only: c_float
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       real(kind = c_float) :: out
     end function generic_ref_get_float_c
     !> @brief Get a float scalar from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @returns Value from x
     function generic_ref_get_double_c(x) &
          result(out) &
          bind(c, name="generic_ref_get_double_f")
       use, intrinsic :: iso_c_binding, only: c_double
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       real(kind = c_double) :: out
     end function generic_ref_get_double_c
     !> @brief Get a complex scalar from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @returns Value from x
     function generic_ref_get_complex_float_c(x) &
          result(out) &
          bind(c, name="generic_ref_get_complex_float_f")
       use, intrinsic :: iso_c_binding, only: c_float_complex
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       complex(kind = c_float_complex) :: out
     end function generic_ref_get_complex_float_c
     !> @brief Get a complex scalar from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @returns Value from x
     function generic_ref_get_complex_double_c(x) &
          result(out) &
          bind(c, name="generic_ref_get_complex_double_f")
       use, intrinsic :: iso_c_binding, only: c_double_complex
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       complex(kind = c_double_complex) :: out
     end function generic_ref_get_complex_double_c
     !> @brief Get a 1darray from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @param[in] subtype Subtype of data to return
     !> @param[in] precision Precision of the data to return
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_ref_get_1darray_c(x, subtype, precision, value) &
          result(out) &
          bind(c, name="generic_ref_get_1darray_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       character(kind = c_char), dimension(*), intent(in) :: subtype
       integer(kind = c_size_t), value, intent(in) :: precision
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_ref_get_1darray_c
     !> @brief Get a int 1darray from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_ref_get_1darray_int16_c(x, value) &
          result(out) &
          bind(c, name="generic_ref_get_1darray_int16_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_ref_get_1darray_int16_c
     !> @brief Get a int 1darray from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_ref_get_1darray_int32_c(x, value) &
          result(out) &
          bind(c, name="generic_ref_get_1darray_int32_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_ref_get_1darray_int32_c
     !> @brief Get a int 1darray from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_ref_get_1darray_int64_c(x, value) &
          result(out) &
          bind(c, name="generic_ref_get_1darray_int64_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_ref_get_1darray_int64_c
     !> @brief Get a float 1darray from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_ref_get_1darray_float_c(x, value) &
          result(out) &
          bind(c, name="generic_ref_get_1darray_float_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_ref_get_1darray_float_c
     !> @brief Get a float 1darray from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_ref_get_1darray_double_c(x, value) &
          result(out) &
          bind(c, name="generic_ref_get_1darray_double_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_ref_get_1darray_double_c
     !> @brief Get a complex 1darray from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_ref_get_1darray_complex_float_c(x, value) &
          result(out) &
          bind(c, name="generic_ref_get_1darray_complex_float_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_ref_get_1darray_complex_float_c
     !> @brief Get a complex 1darray from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_ref_get_1darray_complex_double_c(x, value) &
          result(out) &
          bind(c, name="generic_ref_get_1darray_complex_double_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_ref_get_1darray_complex_double_c
     !> @brief Get a ndarray from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @param[in] subtype Subtype of data to return
     !> @param[in] precision Precision of the data to return
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_ref_get_ndarray_c(x, subtype, precision, value, shape) &
          result(out) &
          bind(c, name="generic_ref_get_ndarray_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       character(kind = c_char), dimension(*), intent(in) :: subtype
       integer(kind = c_size_t), value, intent(in) :: precision
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_ref_get_ndarray_c
     !> @brief Get a int ndarray from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_ref_get_ndarray_int16_c(x, value, shape) &
          result(out) &
          bind(c, name="generic_ref_get_ndarray_int16_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_ref_get_ndarray_int16_c
     !> @brief Get a int ndarray from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_ref_get_ndarray_int32_c(x, value, shape) &
          result(out) &
          bind(c, name="generic_ref_get_ndarray_int32_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_ref_get_ndarray_int32_c
     !> @brief Get a int ndarray from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_ref_get_ndarray_int64_c(x, value, shape) &
          result(out) &
          bind(c, name="generic_ref_get_ndarray_int64_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_ref_get_ndarray_int64_c
     !> @brief Get a float ndarray from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_ref_get_ndarray_float_c(x, value, shape) &
          result(out) &
          bind(c, name="generic_ref_get_ndarray_float_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_ref_get_ndarray_float_c
     !> @brief Get a float ndarray from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_ref_get_ndarray_double_c(x, value, shape) &
          result(out) &
          bind(c, name="generic_ref_get_ndarray_double_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_ref_get_ndarray_double_c
     !> @brief Get a complex ndarray from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_ref_get_ndarray_complex_float_c(x, value, shape) &
          result(out) &
          bind(c, name="generic_ref_get_ndarray_complex_float_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_ref_get_ndarray_complex_float_c
     !> @brief Get a complex ndarray from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_ref_get_ndarray_complex_double_c(x, value, shape) &
          result(out) &
          bind(c, name="generic_ref_get_ndarray_complex_double_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_ref_get_ndarray_complex_double_c
     !> @brief Get a schema from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @returns Value from x
     function generic_ref_get_schema_c(x) &
          result(out) &
          bind(c, name="generic_ref_get_schema_f")
       import :: ygggeneric, ygggenericref
       implicit none
       type(ygggenericref), value :: x
       type(ygggeneric) :: out
     end function generic_ref_get_schema_c
     !> @brief Get a any from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @returns Value from x
     function generic_ref_get_any_c(x) &
          result(out) &
          bind(c, name="generic_ref_get_any_f")
       import :: ygggeneric, ygggenericref
       implicit none
       type(ygggenericref), value :: x
       type(ygggeneric) :: out
     end function generic_ref_get_any_c
     !> @brief Set an element in a array to a null
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_null_c(x, index, value) &
          result(out) &
          bind(c, name="generic_array_set_null_f")
       use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_array_set_null_c
     !> @brief Set an element in a array to a boolean
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_bool_c(x, index, value) &
          result(out) &
          bind(c, name="generic_array_set_bool_f")
       use, intrinsic :: iso_c_binding, only: c_bool, c_int, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       logical(kind = c_bool), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_array_set_bool_c
     !> @brief Set an element in a array to a integer
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_integer_c(x, index, value) &
          result(out) &
          bind(c, name="generic_array_set_integer_f")
       use, intrinsic :: iso_c_binding, only: c_int, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       integer(kind = c_int), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_array_set_integer_c
     !> @brief Set an element in a array to a number
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_number_c(x, index, value) &
          result(out) &
          bind(c, name="generic_array_set_number_f")
       use, intrinsic :: iso_c_binding, only: c_double, c_int, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       real(kind = c_double), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_array_set_number_c
     !> @brief Set an element in a array to a string
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_string_c(x, index, value) &
          result(out) &
          bind(c, name="generic_array_set_string_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       character(kind = c_char), dimension(*), intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_array_set_string_c
     !> @brief Set an element in a array to a item
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_item_c(x, index, type, value) &
          result(out) &
          bind(c, name="generic_array_set_item_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       character(kind = c_char), dimension(*), intent(in) :: type
       type(c_ptr), value :: value
       integer(kind = c_int) :: out
     end function generic_array_set_item_c
     !> @brief Set an element in a array to a array
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_array_c(x, index, value) &
          result(out) &
          bind(c, name="generic_array_set_array_f")
       use, intrinsic :: iso_c_binding, only: c_int, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(ygggeneric), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_array_set_array_c
     !> @brief Set an element in a array to a object
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_object_c(x, index, value) &
          result(out) &
          bind(c, name="generic_array_set_object_f")
       use, intrinsic :: iso_c_binding, only: c_int, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(ygggeneric), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_array_set_object_c
     !> @brief Set an element in a array to a ply
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_ply_c(x, index, value) &
          result(out) &
          bind(c, name="generic_array_set_ply_f")
       use, intrinsic :: iso_c_binding, only: c_int, c_size_t
       import :: ygggeneric, yggply
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(yggply), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_array_set_ply_c
     !> @brief Set an element in a array to a obj
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_obj_c(x, index, value) &
          result(out) &
          bind(c, name="generic_array_set_obj_f")
       use, intrinsic :: iso_c_binding, only: c_int, c_size_t
       import :: ygggeneric, yggobj
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(yggobj), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_array_set_obj_c
     !> @brief Set an element in a array to a class
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_python_class_c(x, index, value) &
          result(out) &
          bind(c, name="generic_array_set_python_class_f")
       use, intrinsic :: iso_c_binding, only: c_int, c_size_t
       import :: ygggeneric, yggpython
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(yggpython), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_array_set_python_class_c
     !> @brief Set an element in a array to a function
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_python_function_c(x, index, value) &
          result(out) &
          bind(c, name="generic_array_set_python_function_f")
       use, intrinsic :: iso_c_binding, only: c_int, c_size_t
       import :: ygggeneric, yggpython
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(yggpython), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_array_set_python_function_c
     !> @brief Set an element in a array to a instance
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_python_instance_c(x, index, value) &
          result(out) &
          bind(c, name="generic_array_set_python_instance_f")
       use, intrinsic :: iso_c_binding, only: c_int, c_size_t
       import :: ygggeneric, yggpython
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(yggpython), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_array_set_python_instance_c
     !> @brief Set an element in a array to a scalar
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value Pointer to the memory containing the value to assign to x
     !> @param[in] subtype Subtype of data contained in value
     !> @param[in] precision The precision of the data in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_scalar_c(x, index, value, subtype, precision, units) &
          result(out) &
          bind(c, name="generic_array_set_scalar_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: subtype
       integer(kind = c_size_t), value, intent(in) :: precision
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_scalar_c
     !> @brief Set an element in a array to a int scalar
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_int16_c(x, index, value, units) &
          result(out) &
          bind(c, name="generic_array_set_int16_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_int16_t, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       integer(kind = c_int16_t), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_int16_c
     !> @brief Set an element in a array to a int scalar
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_int32_c(x, index, value, units) &
          result(out) &
          bind(c, name="generic_array_set_int32_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_int32_t, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       integer(kind = c_int32_t), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_int32_c
     !> @brief Set an element in a array to a int scalar
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_int64_c(x, index, value, units) &
          result(out) &
          bind(c, name="generic_array_set_int64_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_int64_t, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       integer(kind = c_int64_t), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_int64_c
     !> @brief Set an element in a array to a float scalar
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_float_c(x, index, value, units) &
          result(out) &
          bind(c, name="generic_array_set_float_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_float, c_int, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       real(kind = c_float), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_float_c
     !> @brief Set an element in a array to a float scalar
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_double_c(x, index, value, units) &
          result(out) &
          bind(c, name="generic_array_set_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_double, c_int, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       real(kind = c_double), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_double_c
     !> @brief Set an element in a array to a complex scalar
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_complex_float_c(x, index, value, units) &
          result(out) &
          bind(c, name="generic_array_set_complex_float_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_float_complex, c_int, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       complex(kind = c_float_complex), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_complex_float_c
     !> @brief Set an element in a array to a complex scalar
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_complex_double_c(x, index, value, units) &
          result(out) &
          bind(c, name="generic_array_set_complex_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_double_complex, c_int, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       complex(kind = c_double_complex), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_complex_double_c
     !> @brief Set an element in a array to a 1darray
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value Pointer to the memory containing the array to assign
     !>   to x
     !> @param[in] subtype Subtype of data contained in value
     !> @param[in] precision The precision of the elements in value
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_1darray_c(x, index, value, subtype, precision, length, units) &
          result(out) &
          bind(c, name="generic_array_set_1darray_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: subtype
       integer(kind = c_size_t), value, intent(in) :: precision
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_1darray_c
     !> @brief Set an element in a array to a int 1darray
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_1darray_int16_c(x, index, value, length, units) &
          result(out) &
          bind(c, name="generic_array_set_1darray_int16_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_1darray_int16_c
     !> @brief Set an element in a array to a int 1darray
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_1darray_int32_c(x, index, value, length, units) &
          result(out) &
          bind(c, name="generic_array_set_1darray_int32_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_1darray_int32_c
     !> @brief Set an element in a array to a int 1darray
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_1darray_int64_c(x, index, value, length, units) &
          result(out) &
          bind(c, name="generic_array_set_1darray_int64_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_1darray_int64_c
     !> @brief Set an element in a array to a float 1darray
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_1darray_float_c(x, index, value, length, units) &
          result(out) &
          bind(c, name="generic_array_set_1darray_float_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_1darray_float_c
     !> @brief Set an element in a array to a float 1darray
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_1darray_double_c(x, index, value, length, units) &
          result(out) &
          bind(c, name="generic_array_set_1darray_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_1darray_double_c
     !> @brief Set an element in a array to a complex 1darray
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_1darray_complex_float_c(x, index, value, length, units) &
          result(out) &
          bind(c, name="generic_array_set_1darray_complex_float_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_1darray_complex_float_c
     !> @brief Set an element in a array to a complex 1darray
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_1darray_complex_double_c(x, index, value, length, units) &
          result(out) &
          bind(c, name="generic_array_set_1darray_complex_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_1darray_complex_double_c
     !> @brief Set an element in a array to a ndarray
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value Pointer to the memory containing the array to assign
     !>   to x
     !> @param[in] subtype Subtype of data contained in value
     !> @param[in] precision The precision of the elements in value
     !> @param[in] ndim The number of dimensions in value
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_ndarray_c(x, index, value, subtype, precision, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_array_set_ndarray_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: subtype
       integer(kind = c_size_t), value, intent(in) :: precision
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_ndarray_c
     !> @brief Set an element in a array to a int ndarray
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !>   in row-major order
     !> @param[in] ndim The number of dimensions in value, or 0 on error
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_ndarray_int16_c(x, index, value, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_array_set_ndarray_int16_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_ndarray_int16_c
     !> @brief Set an element in a array to a int ndarray
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !>   in row-major order
     !> @param[in] ndim The number of dimensions in value, or 0 on error
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_ndarray_int32_c(x, index, value, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_array_set_ndarray_int32_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_ndarray_int32_c
     !> @brief Set an element in a array to a int ndarray
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !>   in row-major order
     !> @param[in] ndim The number of dimensions in value, or 0 on error
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_ndarray_int64_c(x, index, value, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_array_set_ndarray_int64_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_ndarray_int64_c
     !> @brief Set an element in a array to a float ndarray
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !>   in row-major order
     !> @param[in] ndim The number of dimensions in value, or 0 on error
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_ndarray_float_c(x, index, value, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_array_set_ndarray_float_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_ndarray_float_c
     !> @brief Set an element in a array to a float ndarray
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !>   in row-major order
     !> @param[in] ndim The number of dimensions in value, or 0 on error
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_ndarray_double_c(x, index, value, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_array_set_ndarray_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_ndarray_double_c
     !> @brief Set an element in a array to a complex ndarray
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !>   in row-major order
     !> @param[in] ndim The number of dimensions in value, or 0 on error
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_ndarray_complex_float_c(x, index, value, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_array_set_ndarray_complex_float_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_ndarray_complex_float_c
     !> @brief Set an element in a array to a complex ndarray
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !>   in row-major order
     !> @param[in] ndim The number of dimensions in value, or 0 on error
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_ndarray_complex_double_c(x, index, value, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_array_set_ndarray_complex_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_ndarray_complex_double_c
     !> @brief Set an element in a array to a schema
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_schema_c(x, index, value) &
          result(out) &
          bind(c, name="generic_array_set_schema_f")
       use, intrinsic :: iso_c_binding, only: c_int, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(ygggeneric), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_array_set_schema_c
     !> @brief Set an element in a array to a any
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_any_c(x, index, value) &
          result(out) &
          bind(c, name="generic_array_set_any_f")
       use, intrinsic :: iso_c_binding, only: c_int, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(ygggeneric), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_array_set_any_c
     !> @brief Get a null from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_array_get_null_c(x, index) &
          result(out) &
          bind(c, name="generic_array_get_null_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr) :: out
     end function generic_array_get_null_c
     !> @brief Get a boolean from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_array_get_bool_c(x, index) &
          result(out) &
          bind(c, name="generic_array_get_bool_f")
       use, intrinsic :: iso_c_binding, only: c_bool, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       logical(kind = c_bool) :: out
     end function generic_array_get_bool_c
     !> @brief Get a integer from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_array_get_integer_c(x, index) &
          result(out) &
          bind(c, name="generic_array_get_integer_f")
       use, intrinsic :: iso_c_binding, only: c_int, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       integer(kind = c_int) :: out
     end function generic_array_get_integer_c
     !> @brief Get a number from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_array_get_number_c(x, index) &
          result(out) &
          bind(c, name="generic_array_get_number_f")
       use, intrinsic :: iso_c_binding, only: c_double, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       real(kind = c_double) :: out
     end function generic_array_get_number_c
     !> @brief Get a string from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_array_get_string_c(x, index) &
          result(out) &
          bind(c, name="generic_array_get_string_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr) :: out
     end function generic_array_get_string_c
     !> @brief Get a item from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] type Type of item to retrieve
     !> @returns Pointer to data containing raw item data, NULL on error
     function generic_array_get_item_c(x, index, type) &
          result(out) &
          bind(c, name="generic_array_get_item_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       character(kind = c_char), dimension(*), intent(in) :: type
       type(c_ptr) :: out
     end function generic_array_get_item_c
     !> @brief Get a item_nbytes from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] type Type of item to retrieve
     !> @returns Number of bytes in raw item data, 0 on error
     function generic_array_get_item_nbytes_c(x, index, type) &
          result(out) &
          bind(c, name="generic_array_get_item_nbytes_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       character(kind = c_char), dimension(*), intent(in) :: type
       integer(kind = c_int) :: out
     end function generic_array_get_item_nbytes_c
     !> @brief Get a array from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_array_get_array_c(x, index) &
          result(out) &
          bind(c, name="generic_array_get_array_f")
       use, intrinsic :: iso_c_binding, only: c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(ygggeneric) :: out
     end function generic_array_get_array_c
     !> @brief Get a object from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_array_get_object_c(x, index) &
          result(out) &
          bind(c, name="generic_array_get_object_f")
       use, intrinsic :: iso_c_binding, only: c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(ygggeneric) :: out
     end function generic_array_get_object_c
     !> @brief Get a ply from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_array_get_ply_c(x, index) &
          result(out) &
          bind(c, name="generic_array_get_ply_f")
       use, intrinsic :: iso_c_binding, only: c_size_t
       import :: ygggeneric, yggply
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(yggply) :: out
     end function generic_array_get_ply_c
     !> @brief Get a obj from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_array_get_obj_c(x, index) &
          result(out) &
          bind(c, name="generic_array_get_obj_f")
       use, intrinsic :: iso_c_binding, only: c_size_t
       import :: ygggeneric, yggobj
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(yggobj) :: out
     end function generic_array_get_obj_c
     !> @brief Get a class from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_array_get_python_class_c(x, index) &
          result(out) &
          bind(c, name="generic_array_get_python_class_f")
       use, intrinsic :: iso_c_binding, only: c_size_t
       import :: ygggeneric, yggpython
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(yggpython) :: out
     end function generic_array_get_python_class_c
     !> @brief Get a function from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_array_get_python_function_c(x, index) &
          result(out) &
          bind(c, name="generic_array_get_python_function_f")
       use, intrinsic :: iso_c_binding, only: c_size_t
       import :: ygggeneric, yggpython
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(yggpython) :: out
     end function generic_array_get_python_function_c
     !> @brief Get a instance from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_array_get_python_instance_c(x, index) &
          result(out) &
          bind(c, name="generic_array_get_python_instance_f")
       use, intrinsic :: iso_c_binding, only: c_size_t
       import :: ygggeneric, yggpython
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(yggpython) :: out
     end function generic_array_get_python_instance_c
     !> @brief Get a scalar from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] subtype Subtype of data to return
     !> @param[in] precision Precision of the data to return
     !> @returns Pointer to value in x
     function generic_array_get_scalar_c(x, index, subtype, precision) &
          result(out) &
          bind(c, name="generic_array_get_scalar_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       character(kind = c_char), dimension(*), intent(in) :: subtype
       integer(kind = c_size_t), value, intent(in) :: precision
       type(c_ptr) :: out
     end function generic_array_get_scalar_c
     !> @brief Get a int scalar from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_array_get_int16_c(x, index) &
          result(out) &
          bind(c, name="generic_array_get_int16_f")
       use, intrinsic :: iso_c_binding, only: c_int16_t, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       integer(kind = c_int16_t) :: out
     end function generic_array_get_int16_c
     !> @brief Get a int scalar from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_array_get_int32_c(x, index) &
          result(out) &
          bind(c, name="generic_array_get_int32_f")
       use, intrinsic :: iso_c_binding, only: c_int32_t, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       integer(kind = c_int32_t) :: out
     end function generic_array_get_int32_c
     !> @brief Get a int scalar from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_array_get_int64_c(x, index) &
          result(out) &
          bind(c, name="generic_array_get_int64_f")
       use, intrinsic :: iso_c_binding, only: c_int64_t, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       integer(kind = c_int64_t) :: out
     end function generic_array_get_int64_c
     !> @brief Get a float scalar from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_array_get_float_c(x, index) &
          result(out) &
          bind(c, name="generic_array_get_float_f")
       use, intrinsic :: iso_c_binding, only: c_float, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       real(kind = c_float) :: out
     end function generic_array_get_float_c
     !> @brief Get a float scalar from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_array_get_double_c(x, index) &
          result(out) &
          bind(c, name="generic_array_get_double_f")
       use, intrinsic :: iso_c_binding, only: c_double, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       real(kind = c_double) :: out
     end function generic_array_get_double_c
     !> @brief Get a complex scalar from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_array_get_complex_float_c(x, index) &
          result(out) &
          bind(c, name="generic_array_get_complex_float_f")
       use, intrinsic :: iso_c_binding, only: c_float_complex, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       complex(kind = c_float_complex) :: out
     end function generic_array_get_complex_float_c
     !> @brief Get a complex scalar from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_array_get_complex_double_c(x, index) &
          result(out) &
          bind(c, name="generic_array_get_complex_double_f")
       use, intrinsic :: iso_c_binding, only: c_double_complex, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       complex(kind = c_double_complex) :: out
     end function generic_array_get_complex_double_c
     !> @brief Get a 1darray from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] subtype Subtype of data to return
     !> @param[in] precision Precision of the data to return
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_array_get_1darray_c(x, index, subtype, precision, value) &
          result(out) &
          bind(c, name="generic_array_get_1darray_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       character(kind = c_char), dimension(*), intent(in) :: subtype
       integer(kind = c_size_t), value, intent(in) :: precision
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_array_get_1darray_c
     !> @brief Get a int 1darray from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_array_get_1darray_int16_c(x, index, value) &
          result(out) &
          bind(c, name="generic_array_get_1darray_int16_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_array_get_1darray_int16_c
     !> @brief Get a int 1darray from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_array_get_1darray_int32_c(x, index, value) &
          result(out) &
          bind(c, name="generic_array_get_1darray_int32_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_array_get_1darray_int32_c
     !> @brief Get a int 1darray from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_array_get_1darray_int64_c(x, index, value) &
          result(out) &
          bind(c, name="generic_array_get_1darray_int64_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_array_get_1darray_int64_c
     !> @brief Get a float 1darray from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_array_get_1darray_float_c(x, index, value) &
          result(out) &
          bind(c, name="generic_array_get_1darray_float_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_array_get_1darray_float_c
     !> @brief Get a float 1darray from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_array_get_1darray_double_c(x, index, value) &
          result(out) &
          bind(c, name="generic_array_get_1darray_double_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_array_get_1darray_double_c
     !> @brief Get a complex 1darray from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_array_get_1darray_complex_float_c(x, index, value) &
          result(out) &
          bind(c, name="generic_array_get_1darray_complex_float_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_array_get_1darray_complex_float_c
     !> @brief Get a complex 1darray from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_array_get_1darray_complex_double_c(x, index, value) &
          result(out) &
          bind(c, name="generic_array_get_1darray_complex_double_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_array_get_1darray_complex_double_c
     !> @brief Get a ndarray from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] subtype Subtype of data to return
     !> @param[in] precision Precision of the data to return
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_array_get_ndarray_c(x, index, subtype, precision, value, shape) &
          result(out) &
          bind(c, name="generic_array_get_ndarray_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       character(kind = c_char), dimension(*), intent(in) :: subtype
       integer(kind = c_size_t), value, intent(in) :: precision
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_array_get_ndarray_c
     !> @brief Get a int ndarray from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_array_get_ndarray_int16_c(x, index, value, shape) &
          result(out) &
          bind(c, name="generic_array_get_ndarray_int16_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_array_get_ndarray_int16_c
     !> @brief Get a int ndarray from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_array_get_ndarray_int32_c(x, index, value, shape) &
          result(out) &
          bind(c, name="generic_array_get_ndarray_int32_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_array_get_ndarray_int32_c
     !> @brief Get a int ndarray from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_array_get_ndarray_int64_c(x, index, value, shape) &
          result(out) &
          bind(c, name="generic_array_get_ndarray_int64_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_array_get_ndarray_int64_c
     !> @brief Get a float ndarray from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_array_get_ndarray_float_c(x, index, value, shape) &
          result(out) &
          bind(c, name="generic_array_get_ndarray_float_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_array_get_ndarray_float_c
     !> @brief Get a float ndarray from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_array_get_ndarray_double_c(x, index, value, shape) &
          result(out) &
          bind(c, name="generic_array_get_ndarray_double_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_array_get_ndarray_double_c
     !> @brief Get a complex ndarray from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_array_get_ndarray_complex_float_c(x, index, value, shape) &
          result(out) &
          bind(c, name="generic_array_get_ndarray_complex_float_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_array_get_ndarray_complex_float_c
     !> @brief Get a complex ndarray from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_array_get_ndarray_complex_double_c(x, index, value, shape) &
          result(out) &
          bind(c, name="generic_array_get_ndarray_complex_double_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_array_get_ndarray_complex_double_c
     !> @brief Get a schema from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_array_get_schema_c(x, index) &
          result(out) &
          bind(c, name="generic_array_get_schema_f")
       use, intrinsic :: iso_c_binding, only: c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(ygggeneric) :: out
     end function generic_array_get_schema_c
     !> @brief Get a any from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_array_get_any_c(x, index) &
          result(out) &
          bind(c, name="generic_array_get_any_f")
       use, intrinsic :: iso_c_binding, only: c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(ygggeneric) :: out
     end function generic_array_get_any_c
     !> @brief Set an element in a object to a null
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_null_c(x, key, value) &
          result(out) &
          bind(c, name="generic_object_set_null_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_object_set_null_c
     !> @brief Set an element in a object to a boolean
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_bool_c(x, key, value) &
          result(out) &
          bind(c, name="generic_object_set_bool_f")
       use, intrinsic :: iso_c_binding, only: c_bool, c_char, c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       logical(kind = c_bool), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_object_set_bool_c
     !> @brief Set an element in a object to a integer
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_integer_c(x, key, value) &
          result(out) &
          bind(c, name="generic_object_set_integer_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       integer(kind = c_int), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_object_set_integer_c
     !> @brief Set an element in a object to a number
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_number_c(x, key, value) &
          result(out) &
          bind(c, name="generic_object_set_number_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_double, c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       real(kind = c_double), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_object_set_number_c
     !> @brief Set an element in a object to a string
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_string_c(x, key, value) &
          result(out) &
          bind(c, name="generic_object_set_string_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       character(kind = c_char), dimension(*), intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_object_set_string_c
     !> @brief Set an element in a object to a item
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_item_c(x, key, type, value) &
          result(out) &
          bind(c, name="generic_object_set_item_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       character(kind = c_char), dimension(*), intent(in) :: type
       type(c_ptr), value :: value
       integer(kind = c_int) :: out
     end function generic_object_set_item_c
     !> @brief Set an element in a object to a array
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_array_c(x, key, value) &
          result(out) &
          bind(c, name="generic_object_set_array_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(ygggeneric), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_object_set_array_c
     !> @brief Set an element in a object to a object
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_object_c(x, key, value) &
          result(out) &
          bind(c, name="generic_object_set_object_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(ygggeneric), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_object_set_object_c
     !> @brief Set an element in a object to a ply
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_ply_c(x, key, value) &
          result(out) &
          bind(c, name="generic_object_set_ply_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int
       import :: ygggeneric, yggply
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(yggply), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_object_set_ply_c
     !> @brief Set an element in a object to a obj
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_obj_c(x, key, value) &
          result(out) &
          bind(c, name="generic_object_set_obj_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int
       import :: ygggeneric, yggobj
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(yggobj), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_object_set_obj_c
     !> @brief Set an element in a object to a class
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_python_class_c(x, key, value) &
          result(out) &
          bind(c, name="generic_object_set_python_class_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int
       import :: ygggeneric, yggpython
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(yggpython), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_object_set_python_class_c
     !> @brief Set an element in a object to a function
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_python_function_c(x, key, value) &
          result(out) &
          bind(c, name="generic_object_set_python_function_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int
       import :: ygggeneric, yggpython
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(yggpython), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_object_set_python_function_c
     !> @brief Set an element in a object to a instance
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_python_instance_c(x, key, value) &
          result(out) &
          bind(c, name="generic_object_set_python_instance_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int
       import :: ygggeneric, yggpython
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(yggpython), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_object_set_python_instance_c
     !> @brief Set an element in a object to a scalar
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value Pointer to the memory containing the value to assign to x
     !> @param[in] subtype Subtype of data contained in value
     !> @param[in] precision The precision of the data in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_scalar_c(x, key, value, subtype, precision, units) &
          result(out) &
          bind(c, name="generic_object_set_scalar_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: subtype
       integer(kind = c_size_t), value, intent(in) :: precision
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_scalar_c
     !> @brief Set an element in a object to a int scalar
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_int16_c(x, key, value, units) &
          result(out) &
          bind(c, name="generic_object_set_int16_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_int16_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       integer(kind = c_int16_t), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_int16_c
     !> @brief Set an element in a object to a int scalar
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_int32_c(x, key, value, units) &
          result(out) &
          bind(c, name="generic_object_set_int32_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_int32_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       integer(kind = c_int32_t), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_int32_c
     !> @brief Set an element in a object to a int scalar
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_int64_c(x, key, value, units) &
          result(out) &
          bind(c, name="generic_object_set_int64_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_int64_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       integer(kind = c_int64_t), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_int64_c
     !> @brief Set an element in a object to a float scalar
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_float_c(x, key, value, units) &
          result(out) &
          bind(c, name="generic_object_set_float_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_float, c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       real(kind = c_float), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_float_c
     !> @brief Set an element in a object to a float scalar
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_double_c(x, key, value, units) &
          result(out) &
          bind(c, name="generic_object_set_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_double, c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       real(kind = c_double), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_double_c
     !> @brief Set an element in a object to a complex scalar
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_complex_float_c(x, key, value, units) &
          result(out) &
          bind(c, name="generic_object_set_complex_float_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_float_complex, c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       complex(kind = c_float_complex), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_complex_float_c
     !> @brief Set an element in a object to a complex scalar
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_complex_double_c(x, key, value, units) &
          result(out) &
          bind(c, name="generic_object_set_complex_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_double_complex, c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       complex(kind = c_double_complex), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_complex_double_c
     !> @brief Set an element in a object to a 1darray
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value Pointer to the memory containing the array to assign
     !>   to x
     !> @param[in] subtype Subtype of data contained in value
     !> @param[in] precision The precision of the elements in value
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_1darray_c(x, key, value, subtype, precision, length, units) &
          result(out) &
          bind(c, name="generic_object_set_1darray_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: subtype
       integer(kind = c_size_t), value, intent(in) :: precision
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_1darray_c
     !> @brief Set an element in a object to a int 1darray
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_1darray_int16_c(x, key, value, length, units) &
          result(out) &
          bind(c, name="generic_object_set_1darray_int16_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_1darray_int16_c
     !> @brief Set an element in a object to a int 1darray
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_1darray_int32_c(x, key, value, length, units) &
          result(out) &
          bind(c, name="generic_object_set_1darray_int32_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_1darray_int32_c
     !> @brief Set an element in a object to a int 1darray
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_1darray_int64_c(x, key, value, length, units) &
          result(out) &
          bind(c, name="generic_object_set_1darray_int64_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_1darray_int64_c
     !> @brief Set an element in a object to a float 1darray
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_1darray_float_c(x, key, value, length, units) &
          result(out) &
          bind(c, name="generic_object_set_1darray_float_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_1darray_float_c
     !> @brief Set an element in a object to a float 1darray
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_1darray_double_c(x, key, value, length, units) &
          result(out) &
          bind(c, name="generic_object_set_1darray_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_1darray_double_c
     !> @brief Set an element in a object to a complex 1darray
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_1darray_complex_float_c(x, key, value, length, units) &
          result(out) &
          bind(c, name="generic_object_set_1darray_complex_float_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_1darray_complex_float_c
     !> @brief Set an element in a object to a complex 1darray
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_1darray_complex_double_c(x, key, value, length, units) &
          result(out) &
          bind(c, name="generic_object_set_1darray_complex_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_1darray_complex_double_c
     !> @brief Set an element in a object to a ndarray
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value Pointer to the memory containing the array to assign
     !>   to x
     !> @param[in] subtype Subtype of data contained in value
     !> @param[in] precision The precision of the elements in value
     !> @param[in] ndim The number of dimensions in value
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_ndarray_c(x, key, value, subtype, precision, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_object_set_ndarray_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: subtype
       integer(kind = c_size_t), value, intent(in) :: precision
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_ndarray_c
     !> @brief Set an element in a object to a int ndarray
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !>   in row-major order
     !> @param[in] ndim The number of dimensions in value, or 0 on error
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_ndarray_int16_c(x, key, value, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_object_set_ndarray_int16_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_ndarray_int16_c
     !> @brief Set an element in a object to a int ndarray
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !>   in row-major order
     !> @param[in] ndim The number of dimensions in value, or 0 on error
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_ndarray_int32_c(x, key, value, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_object_set_ndarray_int32_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_ndarray_int32_c
     !> @brief Set an element in a object to a int ndarray
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !>   in row-major order
     !> @param[in] ndim The number of dimensions in value, or 0 on error
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_ndarray_int64_c(x, key, value, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_object_set_ndarray_int64_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_ndarray_int64_c
     !> @brief Set an element in a object to a float ndarray
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !>   in row-major order
     !> @param[in] ndim The number of dimensions in value, or 0 on error
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_ndarray_float_c(x, key, value, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_object_set_ndarray_float_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_ndarray_float_c
     !> @brief Set an element in a object to a float ndarray
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !>   in row-major order
     !> @param[in] ndim The number of dimensions in value, or 0 on error
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_ndarray_double_c(x, key, value, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_object_set_ndarray_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_ndarray_double_c
     !> @brief Set an element in a object to a complex ndarray
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !>   in row-major order
     !> @param[in] ndim The number of dimensions in value, or 0 on error
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_ndarray_complex_float_c(x, key, value, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_object_set_ndarray_complex_float_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_ndarray_complex_float_c
     !> @brief Set an element in a object to a complex ndarray
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !>   in row-major order
     !> @param[in] ndim The number of dimensions in value, or 0 on error
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_ndarray_complex_double_c(x, key, value, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_object_set_ndarray_complex_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_ndarray_complex_double_c
     !> @brief Set an element in a object to a schema
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_schema_c(x, key, value) &
          result(out) &
          bind(c, name="generic_object_set_schema_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(ygggeneric), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_object_set_schema_c
     !> @brief Set an element in a object to a any
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_any_c(x, key, value) &
          result(out) &
          bind(c, name="generic_object_set_any_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(ygggeneric), value, intent(in) :: value
       integer(kind = c_int) :: out
     end function generic_object_set_any_c
     !> @brief Get a null from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_object_get_null_c(x, key) &
          result(out) &
          bind(c, name="generic_object_get_null_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr) :: out
     end function generic_object_get_null_c
     !> @brief Get a boolean from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_object_get_bool_c(x, key) &
          result(out) &
          bind(c, name="generic_object_get_bool_f")
       use, intrinsic :: iso_c_binding, only: c_bool, c_char
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       logical(kind = c_bool) :: out
     end function generic_object_get_bool_c
     !> @brief Get a integer from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_object_get_integer_c(x, key) &
          result(out) &
          bind(c, name="generic_object_get_integer_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       integer(kind = c_int) :: out
     end function generic_object_get_integer_c
     !> @brief Get a number from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_object_get_number_c(x, key) &
          result(out) &
          bind(c, name="generic_object_get_number_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_double
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       real(kind = c_double) :: out
     end function generic_object_get_number_c
     !> @brief Get a string from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_object_get_string_c(x, key) &
          result(out) &
          bind(c, name="generic_object_get_string_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr) :: out
     end function generic_object_get_string_c
     !> @brief Get a item from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] type Type of item to retrieve
     !> @returns Pointer to data containing raw item data, NULL on error
     function generic_object_get_item_c(x, key, type) &
          result(out) &
          bind(c, name="generic_object_get_item_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       character(kind = c_char), dimension(*), intent(in) :: type
       type(c_ptr) :: out
     end function generic_object_get_item_c
     !> @brief Get a item_nbytes from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] type Type of item to retrieve
     !> @returns Number of bytes in raw item data, 0 on error
     function generic_object_get_item_nbytes_c(x, key, type) &
          result(out) &
          bind(c, name="generic_object_get_item_nbytes_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       character(kind = c_char), dimension(*), intent(in) :: type
       integer(kind = c_int) :: out
     end function generic_object_get_item_nbytes_c
     !> @brief Get a array from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_object_get_array_c(x, key) &
          result(out) &
          bind(c, name="generic_object_get_array_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(ygggeneric) :: out
     end function generic_object_get_array_c
     !> @brief Get a object from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_object_get_object_c(x, key) &
          result(out) &
          bind(c, name="generic_object_get_object_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(ygggeneric) :: out
     end function generic_object_get_object_c
     !> @brief Get a ply from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_object_get_ply_c(x, key) &
          result(out) &
          bind(c, name="generic_object_get_ply_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: ygggeneric, yggply
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(yggply) :: out
     end function generic_object_get_ply_c
     !> @brief Get a obj from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_object_get_obj_c(x, key) &
          result(out) &
          bind(c, name="generic_object_get_obj_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: ygggeneric, yggobj
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(yggobj) :: out
     end function generic_object_get_obj_c
     !> @brief Get a class from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_object_get_python_class_c(x, key) &
          result(out) &
          bind(c, name="generic_object_get_python_class_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: ygggeneric, yggpython
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(yggpython) :: out
     end function generic_object_get_python_class_c
     !> @brief Get a function from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_object_get_python_function_c(x, key) &
          result(out) &
          bind(c, name="generic_object_get_python_function_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: ygggeneric, yggpython
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(yggpython) :: out
     end function generic_object_get_python_function_c
     !> @brief Get a instance from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_object_get_python_instance_c(x, key) &
          result(out) &
          bind(c, name="generic_object_get_python_instance_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: ygggeneric, yggpython
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(yggpython) :: out
     end function generic_object_get_python_instance_c
     !> @brief Get a scalar from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] subtype Subtype of data to return
     !> @param[in] precision Precision of the data to return
     !> @returns Pointer to value in x
     function generic_object_get_scalar_c(x, key, subtype, precision) &
          result(out) &
          bind(c, name="generic_object_get_scalar_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       character(kind = c_char), dimension(*), intent(in) :: subtype
       integer(kind = c_size_t), value, intent(in) :: precision
       type(c_ptr) :: out
     end function generic_object_get_scalar_c
     !> @brief Get a int scalar from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_object_get_int16_c(x, key) &
          result(out) &
          bind(c, name="generic_object_get_int16_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int16_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       integer(kind = c_int16_t) :: out
     end function generic_object_get_int16_c
     !> @brief Get a int scalar from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_object_get_int32_c(x, key) &
          result(out) &
          bind(c, name="generic_object_get_int32_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int32_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       integer(kind = c_int32_t) :: out
     end function generic_object_get_int32_c
     !> @brief Get a int scalar from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_object_get_int64_c(x, key) &
          result(out) &
          bind(c, name="generic_object_get_int64_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int64_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       integer(kind = c_int64_t) :: out
     end function generic_object_get_int64_c
     !> @brief Get a float scalar from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_object_get_float_c(x, key) &
          result(out) &
          bind(c, name="generic_object_get_float_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_float
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       real(kind = c_float) :: out
     end function generic_object_get_float_c
     !> @brief Get a float scalar from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_object_get_double_c(x, key) &
          result(out) &
          bind(c, name="generic_object_get_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_double
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       real(kind = c_double) :: out
     end function generic_object_get_double_c
     !> @brief Get a complex scalar from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_object_get_complex_float_c(x, key) &
          result(out) &
          bind(c, name="generic_object_get_complex_float_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_float_complex
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       complex(kind = c_float_complex) :: out
     end function generic_object_get_complex_float_c
     !> @brief Get a complex scalar from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_object_get_complex_double_c(x, key) &
          result(out) &
          bind(c, name="generic_object_get_complex_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_double_complex
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       complex(kind = c_double_complex) :: out
     end function generic_object_get_complex_double_c
     !> @brief Get a 1darray from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] subtype Subtype of data to return
     !> @param[in] precision Precision of the data to return
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_object_get_1darray_c(x, key, subtype, precision, value) &
          result(out) &
          bind(c, name="generic_object_get_1darray_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       character(kind = c_char), dimension(*), intent(in) :: subtype
       integer(kind = c_size_t), value, intent(in) :: precision
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_object_get_1darray_c
     !> @brief Get a int 1darray from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_object_get_1darray_int16_c(x, key, value) &
          result(out) &
          bind(c, name="generic_object_get_1darray_int16_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_object_get_1darray_int16_c
     !> @brief Get a int 1darray from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_object_get_1darray_int32_c(x, key, value) &
          result(out) &
          bind(c, name="generic_object_get_1darray_int32_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_object_get_1darray_int32_c
     !> @brief Get a int 1darray from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_object_get_1darray_int64_c(x, key, value) &
          result(out) &
          bind(c, name="generic_object_get_1darray_int64_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_object_get_1darray_int64_c
     !> @brief Get a float 1darray from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_object_get_1darray_float_c(x, key, value) &
          result(out) &
          bind(c, name="generic_object_get_1darray_float_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_object_get_1darray_float_c
     !> @brief Get a float 1darray from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_object_get_1darray_double_c(x, key, value) &
          result(out) &
          bind(c, name="generic_object_get_1darray_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_object_get_1darray_double_c
     !> @brief Get a complex 1darray from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_object_get_1darray_complex_float_c(x, key, value) &
          result(out) &
          bind(c, name="generic_object_get_1darray_complex_float_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_object_get_1darray_complex_float_c
     !> @brief Get a complex 1darray from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_object_get_1darray_complex_double_c(x, key, value) &
          result(out) &
          bind(c, name="generic_object_get_1darray_complex_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_object_get_1darray_complex_double_c
     !> @brief Get a ndarray from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] subtype Subtype of data to return
     !> @param[in] precision Precision of the data to return
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_object_get_ndarray_c(x, key, subtype, precision, value, shape) &
          result(out) &
          bind(c, name="generic_object_get_ndarray_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       character(kind = c_char), dimension(*), intent(in) :: subtype
       integer(kind = c_size_t), value, intent(in) :: precision
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_object_get_ndarray_c
     !> @brief Get a int ndarray from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_object_get_ndarray_int16_c(x, key, value, shape) &
          result(out) &
          bind(c, name="generic_object_get_ndarray_int16_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_object_get_ndarray_int16_c
     !> @brief Get a int ndarray from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_object_get_ndarray_int32_c(x, key, value, shape) &
          result(out) &
          bind(c, name="generic_object_get_ndarray_int32_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_object_get_ndarray_int32_c
     !> @brief Get a int ndarray from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_object_get_ndarray_int64_c(x, key, value, shape) &
          result(out) &
          bind(c, name="generic_object_get_ndarray_int64_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_object_get_ndarray_int64_c
     !> @brief Get a float ndarray from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_object_get_ndarray_float_c(x, key, value, shape) &
          result(out) &
          bind(c, name="generic_object_get_ndarray_float_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_object_get_ndarray_float_c
     !> @brief Get a float ndarray from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_object_get_ndarray_double_c(x, key, value, shape) &
          result(out) &
          bind(c, name="generic_object_get_ndarray_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_object_get_ndarray_double_c
     !> @brief Get a complex ndarray from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_object_get_ndarray_complex_float_c(x, key, value, shape) &
          result(out) &
          bind(c, name="generic_object_get_ndarray_complex_float_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_object_get_ndarray_complex_float_c
     !> @brief Get a complex ndarray from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_object_get_ndarray_complex_double_c(x, key, value, shape) &
          result(out) &
          bind(c, name="generic_object_get_ndarray_complex_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_object_get_ndarray_complex_double_c
     !> @brief Get a schema from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_object_get_schema_c(x, key) &
          result(out) &
          bind(c, name="generic_object_get_schema_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(ygggeneric) :: out
     end function generic_object_get_schema_c
     !> @brief Get a any from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_object_get_any_c(x, key) &
          result(out) &
          bind(c, name="generic_object_get_any_f")
       use, intrinsic :: iso_c_binding, only: c_char
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(ygggeneric) :: out
     end function generic_object_get_any_c
#ifdef YGGDRASIL_LONG_DOUBLE_AVAILABLE
     !> @brief Set a given generic item to a float scalar
     !> @param[in] x The generic item to set
     !> @param[in] value The value to assign to x
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_long_double_c(x, value, units) &
          result(out) &
          bind(c, name="generic_set_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_double
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       real(kind = c_long_double), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_long_double_c
     !> @brief Set a given generic item to a complex scalar
     !> @param[in] x The generic item to set
     !> @param[in] value The value to assign to x
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_complex_long_double_c(x, value, units) &
          result(out) &
          bind(c, name="generic_set_complex_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_double_complex
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       complex(kind = c_long_double_complex), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_complex_long_double_c
     !> @brief Set a given generic item to a float 1darray
     !> @param[in] x The generic item to set
     !> @param[in] value The array of values to assign to x
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_1darray_long_double_c(x, value, length, units) &
          result(out) &
          bind(c, name="generic_set_1darray_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_1darray_long_double_c
     !> @brief Set a given generic item to a complex 1darray
     !> @param[in] x The generic item to set
     !> @param[in] value The array of values to assign to x
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_1darray_complex_long_double_c(x, value, length, units) &
          result(out) &
          bind(c, name="generic_set_1darray_complex_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_1darray_complex_long_double_c
     !> @brief Set a given generic item to a float ndarray
     !> @param[in] x The generic item to set
     !> @param[in] value The array of values to assign to x
     !>   in row-major order
     !> @param[in] ndim The number of dimensions in value, or 0 on error
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_ndarray_long_double_c(x, value, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_set_ndarray_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_ndarray_long_double_c
     !> @brief Set a given generic item to a complex ndarray
     !> @param[in] x The generic item to set
     !> @param[in] value The array of values to assign to x
     !>   in row-major order
     !> @param[in] ndim The number of dimensions in value, or 0 on error
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_set_ndarray_complex_long_double_c(x, value, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_set_ndarray_complex_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_set_ndarray_complex_long_double_c
     !> @brief Get a float scalar from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @returns Value from x
     function generic_get_long_double_c(x) &
          result(out) &
          bind(c, name="generic_get_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_long_double
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       real(kind = c_long_double) :: out
     end function generic_get_long_double_c
     !> @brief Get a complex scalar from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @returns Value from x
     function generic_get_complex_long_double_c(x) &
          result(out) &
          bind(c, name="generic_get_complex_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_long_double_complex
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       complex(kind = c_long_double_complex) :: out
     end function generic_get_complex_long_double_c
     !> @brief Get a float 1darray from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_get_1darray_long_double_c(x, value) &
          result(out) &
          bind(c, name="generic_get_1darray_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_get_1darray_long_double_c
     !> @brief Get a complex 1darray from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_get_1darray_complex_long_double_c(x, value) &
          result(out) &
          bind(c, name="generic_get_1darray_complex_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_get_1darray_complex_long_double_c
     !> @brief Get a float ndarray from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_get_ndarray_long_double_c(x, value, shape) &
          result(out) &
          bind(c, name="generic_get_ndarray_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_get_ndarray_long_double_c
     !> @brief Get a complex ndarray from a generic item
     !> @param[in] x Generic item to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_get_ndarray_complex_long_double_c(x, value, shape) &
          result(out) &
          bind(c, name="generic_get_ndarray_complex_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_get_ndarray_complex_long_double_c
     !> @brief Get a float scalar from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @returns Value from x
     function generic_ref_get_long_double_c(x) &
          result(out) &
          bind(c, name="generic_ref_get_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_long_double
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       real(kind = c_long_double) :: out
     end function generic_ref_get_long_double_c
     !> @brief Get a complex scalar from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @returns Value from x
     function generic_ref_get_complex_long_double_c(x) &
          result(out) &
          bind(c, name="generic_ref_get_complex_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_long_double_complex
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       complex(kind = c_long_double_complex) :: out
     end function generic_ref_get_complex_long_double_c
     !> @brief Get a float 1darray from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_ref_get_1darray_long_double_c(x, value) &
          result(out) &
          bind(c, name="generic_ref_get_1darray_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_ref_get_1darray_long_double_c
     !> @brief Get a complex 1darray from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_ref_get_1darray_complex_long_double_c(x, value) &
          result(out) &
          bind(c, name="generic_ref_get_1darray_complex_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_ref_get_1darray_complex_long_double_c
     !> @brief Get a float ndarray from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_ref_get_ndarray_long_double_c(x, value, shape) &
          result(out) &
          bind(c, name="generic_ref_get_ndarray_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_ref_get_ndarray_long_double_c
     !> @brief Get a complex ndarray from a generic item reference
     !> @param[in] x Generic item reference to retrieve data from
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_ref_get_ndarray_complex_long_double_c(x, value, shape) &
          result(out) &
          bind(c, name="generic_ref_get_ndarray_complex_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggenericref
       implicit none
       type(ygggenericref), value :: x
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_ref_get_ndarray_complex_long_double_c
     !> @brief Set an element in a array to a float scalar
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_long_double_c(x, index, value, units) &
          result(out) &
          bind(c, name="generic_array_set_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_double, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       real(kind = c_long_double), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_long_double_c
     !> @brief Set an element in a array to a complex scalar
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_complex_long_double_c(x, index, value, units) &
          result(out) &
          bind(c, name="generic_array_set_complex_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_double_complex, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       complex(kind = c_long_double_complex), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_complex_long_double_c
     !> @brief Set an element in a array to a float 1darray
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_1darray_long_double_c(x, index, value, length, units) &
          result(out) &
          bind(c, name="generic_array_set_1darray_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_1darray_long_double_c
     !> @brief Set an element in a array to a complex 1darray
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_1darray_complex_long_double_c(x, index, value, length, units) &
          result(out) &
          bind(c, name="generic_array_set_1darray_complex_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_1darray_complex_long_double_c
     !> @brief Set an element in a array to a float ndarray
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !>   in row-major order
     !> @param[in] ndim The number of dimensions in value, or 0 on error
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_ndarray_long_double_c(x, index, value, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_array_set_ndarray_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_ndarray_long_double_c
     !> @brief Set an element in a array to a complex ndarray
     !> @param[in] x array to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !>   in row-major order
     !> @param[in] ndim The number of dimensions in value, or 0 on error
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_array_set_ndarray_complex_long_double_c(x, index, value, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_array_set_ndarray_complex_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_array_set_ndarray_complex_long_double_c
     !> @brief Get a float scalar from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_array_get_long_double_c(x, index) &
          result(out) &
          bind(c, name="generic_array_get_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_long_double, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       real(kind = c_long_double) :: out
     end function generic_array_get_long_double_c
     !> @brief Get a complex scalar from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_array_get_complex_long_double_c(x, index) &
          result(out) &
          bind(c, name="generic_array_get_complex_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_long_double_complex, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       complex(kind = c_long_double_complex) :: out
     end function generic_array_get_complex_long_double_c
     !> @brief Get a float 1darray from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_array_get_1darray_long_double_c(x, index, value) &
          result(out) &
          bind(c, name="generic_array_get_1darray_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_array_get_1darray_long_double_c
     !> @brief Get a complex 1darray from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_array_get_1darray_complex_long_double_c(x, index, value) &
          result(out) &
          bind(c, name="generic_array_get_1darray_complex_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_array_get_1darray_complex_long_double_c
     !> @brief Get a float ndarray from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_array_get_ndarray_long_double_c(x, index, value, shape) &
          result(out) &
          bind(c, name="generic_array_get_ndarray_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_array_get_ndarray_long_double_c
     !> @brief Get a complex ndarray from an element in a array
     !> @param[in] x array to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_array_get_ndarray_complex_long_double_c(x, index, value, shape) &
          result(out) &
          bind(c, name="generic_array_get_ndarray_complex_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       integer(kind = c_size_t), value, intent(in) :: index
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_array_get_ndarray_complex_long_double_c
     !> @brief Set an element in a object to a float scalar
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_long_double_c(x, key, value, units) &
          result(out) &
          bind(c, name="generic_object_set_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_double
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       real(kind = c_long_double), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_long_double_c
     !> @brief Set an element in a object to a complex scalar
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The value to assign to x
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_complex_long_double_c(x, key, value, units) &
          result(out) &
          bind(c, name="generic_object_set_complex_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_double_complex
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       complex(kind = c_long_double_complex), value, intent(in) :: value
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_complex_long_double_c
     !> @brief Set an element in a object to a float 1darray
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_1darray_long_double_c(x, key, value, length, units) &
          result(out) &
          bind(c, name="generic_object_set_1darray_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_1darray_long_double_c
     !> @brief Set an element in a object to a complex 1darray
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !> @param[in] length The number of elements in value
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_1darray_complex_long_double_c(x, key, value, length, units) &
          result(out) &
          bind(c, name="generic_object_set_1darray_complex_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: length
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_1darray_complex_long_double_c
     !> @brief Set an element in a object to a float ndarray
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !>   in row-major order
     !> @param[in] ndim The number of dimensions in value, or 0 on error
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_ndarray_long_double_c(x, key, value, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_object_set_ndarray_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_ndarray_long_double_c
     !> @brief Set an element in a object to a complex ndarray
     !> @param[in] x object to set element in
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[in] value The array of values to assign to x
     !>   in row-major order
     !> @param[in] ndim The number of dimensions in value, or 0 on error
     !> @param[in] shape The size of value in each dimension
     !> @param[in] units Units of value
     !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
     function generic_object_set_ndarray_complex_long_double_c(x, key, value, ndim, shape, units) &
          result(out) &
          bind(c, name="generic_object_set_ndarray_complex_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value, intent(in) :: value
       integer(kind = c_size_t), value, intent(in) :: ndim
       type(c_ptr), value, intent(in) :: shape
       character(kind = c_char), dimension(*), intent(in) :: units
       integer(kind = c_int) :: out
     end function generic_object_set_ndarray_complex_long_double_c
     !> @brief Get a float scalar from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_object_get_long_double_c(x, key) &
          result(out) &
          bind(c, name="generic_object_get_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_long_double
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       real(kind = c_long_double) :: out
     end function generic_object_get_long_double_c
     !> @brief Get a complex scalar from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @returns Value from x
     function generic_object_get_complex_long_double_c(x, key) &
          result(out) &
          bind(c, name="generic_object_get_complex_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_long_double_complex
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       complex(kind = c_long_double_complex) :: out
     end function generic_object_get_complex_long_double_c
     !> @brief Get a float 1darray from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_object_get_1darray_long_double_c(x, key, value) &
          result(out) &
          bind(c, name="generic_object_get_1darray_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_object_get_1darray_long_double_c
     !> @brief Get a complex 1darray from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x
     !> @returns Number of elements in the array, or 0 on error
     function generic_object_get_1darray_complex_long_double_c(x, key, value) &
          result(out) &
          bind(c, name="generic_object_get_1darray_complex_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value :: value
       integer(kind = c_size_t) :: out
     end function generic_object_get_1darray_complex_long_double_c
     !> @brief Get a float ndarray from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_object_get_ndarray_long_double_c(x, key, value, shape) &
          result(out) &
          bind(c, name="generic_object_get_ndarray_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_object_get_ndarray_long_double_c
     !> @brief Get a complex ndarray from an element in a object
     !> @param[in] x object to get element from
     !> @param[in] {idx} {idx} of element to {x}
     !> @param[out] value Pointer to memory that should be reallocated and
     !>   filled with the array contents of x in row-major order
     !> @param[out] shape Pointer to memory that should be reallocated and
     !>   filled with the size of the array in each dimension
     !> @returns Number of dimensions in the array, or 0 on error
     function generic_object_get_ndarray_complex_long_double_c(x, key, value, shape) &
          result(out) &
          bind(c, name="generic_object_get_ndarray_complex_long_double_f")
       use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t
       import :: ygggeneric
       implicit none
       type(ygggeneric), value :: x
       character(kind = c_char), dimension(*), intent(in) :: key
       type(c_ptr), value :: value
       type(c_ptr), value :: shape
       integer(kind = c_size_t) :: out
     end function generic_object_get_ndarray_complex_long_double_c
#endif // YGGDRASIL_LONG_DOUBLE_AVAILABLE

  end interface
