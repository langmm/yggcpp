module YggInterface

  ! TODO: Ensure that dynamically allocated C/C++ variables are freed.
  use iso_c_binding
  use iso_fortran_env
  implicit none

  integer, parameter :: LINE_SIZE_MAX = 2048
  integer, parameter :: YGG_MSG_BUF = 2048
  integer, parameter :: ascii = selected_char_kind ("ascii")
#ifdef _WIN32
  integer, parameter :: ucs4  = 1
#else
  integer, parameter :: ucs4  = selected_char_kind ('ISO_10646')
#endif
  integer(kind=c_int), bind(c, name="YGG_MSG_MAX_F") :: YGG_MSG_MAX
  real(8),  parameter :: PI_8  = 4 * atan (1.0_8)
  real(16), parameter :: PI_16 = 4 * atan (1.0_16)

  include "YggInterface_enums.F90"
  ! include "YggInterface_interfaces.F90"

  !> @brief Wrap a fortran variable so that yggdrasil can pass it to comm
  !>    send & receive methods.
  !> @param [in,out] x Variable to wrap.
  !> @returns y Wrapped variable.
  interface yggarg
     module procedure yggarg_scalar_unsigned1
     module procedure yggarg_scalar_unsigned2
     module procedure yggarg_scalar_unsigned4
     module procedure yggarg_scalar_unsigned8
     module procedure yggarg_scalar_integer2
     module procedure yggarg_scalar_integer4
     module procedure yggarg_scalar_integer8
     module procedure yggarg_scalar_real4
     module procedure yggarg_scalar_real8
#ifndef _WIN32
     module procedure yggarg_scalar_real16
#endif
     module procedure yggarg_scalar_complex4
     module procedure yggarg_scalar_complex8
#ifndef _WIN32
     module procedure yggarg_scalar_complex16
#endif
     module procedure yggarg_scalar_logical1
     module procedure yggarg_scalar_logical2
     module procedure yggarg_scalar_logical4
     module procedure yggarg_scalar_logical8
     module procedure yggarg_scalar_character
#ifndef _WIN32
     module procedure yggarg_scalar_unicode
#endif
     module procedure yggarg_scalar_yggchar_r
     module procedure yggarg_scalar_ply
     module procedure yggarg_scalar_obj
     module procedure yggarg_scalar_null
     module procedure yggarg_scalar_generic
     module procedure yggarg_scalar_yggarr
     module procedure yggarg_scalar_yggmap
     module procedure yggarg_scalar_yggschema
     module procedure yggarg_scalar_yggpython
     module procedure yggarg_scalar_yggpyinst
     module procedure yggarg_scalar_yggpyfunc
     module procedure yggarg_scalar_yggptr
     module procedure yggarg_realloc_1darray_unsigned1
     module procedure yggarg_realloc_1darray_unsigned2
     module procedure yggarg_realloc_1darray_unsigned4
     module procedure yggarg_realloc_1darray_unsigned8
     module procedure yggarg_realloc_1darray_c_long
     module procedure yggarg_realloc_1darray_integer
     module procedure yggarg_realloc_1darray_integer2
     module procedure yggarg_realloc_1darray_integer4
     module procedure yggarg_realloc_1darray_integer8
     module procedure yggarg_realloc_1darray_real
     module procedure yggarg_realloc_1darray_real4
     module procedure yggarg_realloc_1darray_real8
     module procedure yggarg_realloc_1darray_real16
     module procedure yggarg_realloc_1darray_complex
     module procedure yggarg_realloc_1darray_complex4
     module procedure yggarg_realloc_1darray_complex8
     module procedure yggarg_realloc_1darray_complex16
     module procedure yggarg_realloc_1darray_logical
     module procedure yggarg_realloc_1darray_logical1
     module procedure yggarg_realloc_1darray_logical2
     module procedure yggarg_realloc_1darray_logical4
     module procedure yggarg_realloc_1darray_logical8
     module procedure yggarg_realloc_1darray_character
     module procedure yggarg_1darray_unsigned1
     module procedure yggarg_1darray_unsigned2
     module procedure yggarg_1darray_unsigned4
     module procedure yggarg_1darray_unsigned8
     module procedure yggarg_1darray_integer2
     module procedure yggarg_1darray_integer4
     module procedure yggarg_1darray_integer8
     module procedure yggarg_1darray_real4
     module procedure yggarg_1darray_real8
#ifndef _WIN32
     module procedure yggarg_1darray_real16
#endif
     module procedure yggarg_1darray_complex4
     module procedure yggarg_1darray_complex8
#ifndef _WIN32
     module procedure yggarg_1darray_complex16
#endif
     module procedure yggarg_1darray_logical1
     module procedure yggarg_1darray_logical2
     module procedure yggarg_1darray_logical4
     module procedure yggarg_1darray_logical8
     module procedure yggarg_1darray_character
#ifndef _WIN32
     module procedure yggarg_1darray_unicode
#endif
     module procedure yggarg_1darray_yggchar_r
     module procedure yggarg_realloc_ndarray_unsigned1
     module procedure yggarg_realloc_ndarray_unsigned2
     module procedure yggarg_realloc_ndarray_unsigned4
     module procedure yggarg_realloc_ndarray_unsigned8
     module procedure yggarg_realloc_ndarray_c_long
     module procedure yggarg_realloc_ndarray_integer
     module procedure yggarg_realloc_ndarray_integer2
     module procedure yggarg_realloc_ndarray_integer4
     module procedure yggarg_realloc_ndarray_integer8
     module procedure yggarg_realloc_ndarray_real
     module procedure yggarg_realloc_ndarray_real4
     module procedure yggarg_realloc_ndarray_real8
     module procedure yggarg_realloc_ndarray_real16
     module procedure yggarg_realloc_ndarray_complex
     module procedure yggarg_realloc_ndarray_complex4
     module procedure yggarg_realloc_ndarray_complex8
     module procedure yggarg_realloc_ndarray_complex16
     module procedure yggarg_realloc_ndarray_logical
     module procedure yggarg_realloc_ndarray_logical1
     module procedure yggarg_realloc_ndarray_logical2
     module procedure yggarg_realloc_ndarray_logical4
     module procedure yggarg_realloc_ndarray_logical8
     module procedure yggarg_realloc_ndarray_character
     module procedure yggarg_2darray_unsigned1
     module procedure yggarg_2darray_unsigned2
     module procedure yggarg_2darray_unsigned4
     module procedure yggarg_2darray_unsigned8
     module procedure yggarg_2darray_integer2
     module procedure yggarg_2darray_integer4
     module procedure yggarg_2darray_integer8
     module procedure yggarg_2darray_real4
     module procedure yggarg_2darray_real8
#ifndef _WIN32
     module procedure yggarg_2darray_real16
#endif
     module procedure yggarg_2darray_complex4
     module procedure yggarg_2darray_complex8
#ifndef _WIN32
     module procedure yggarg_2darray_complex16
#endif
     module procedure yggarg_2darray_logical1
     module procedure yggarg_2darray_logical2
     module procedure yggarg_2darray_logical4
     module procedure yggarg_2darray_logical8
     module procedure yggarg_2darray_character
     module procedure yggarg_2darray_yggchar_r
  end interface yggarg
#ifndef DOXYGEN_SHOULD_SKIP_THIS
  !> @brief Copy values from a specialized reallocatable array class
  !>   to a classical fortran array
  !> @param[in] in Specialized reallocatable array
  !> @param[out] out Classical fortran array
  interface yggassign
     module procedure yggassign_yggchar2character
     module procedure yggassign_integer_1d_to_array
     module procedure yggassign_integer_1d_from_array
     module procedure yggassign_integer2_1d_to_array
     module procedure yggassign_integer2_1d_from_array
     module procedure yggassign_integer4_1d_to_array
     module procedure yggassign_integer4_1d_from_array
     module procedure yggassign_integer8_1d_to_array
     module procedure yggassign_integer8_1d_from_array
     module procedure yggassign_unsigned2_1d_to_array
     module procedure yggassign_unsigned2_1d_from_array
     module procedure yggassign_unsigned4_1d_to_array
     module procedure yggassign_unsigned4_1d_from_array
     module procedure yggassign_unsigned8_1d_to_array
     module procedure yggassign_unsigned8_1d_from_array
     module procedure yggassign_real_1d_to_array
     module procedure yggassign_real_1d_from_array
     module procedure yggassign_real4_1d_to_array
     module procedure yggassign_real4_1d_from_array
     module procedure yggassign_real8_1d_to_array
     module procedure yggassign_real8_1d_from_array
     module procedure yggassign_real16_1d_to_array
     module procedure yggassign_real16_1d_from_array
     module procedure yggassign_complex_1d_to_array
     module procedure yggassign_complex_1d_from_array
     module procedure yggassign_complex4_1d_to_array
     module procedure yggassign_complex4_1d_from_array
     module procedure yggassign_complex8_1d_to_array
     module procedure yggassign_complex8_1d_from_array
     module procedure yggassign_complex16_1d_to_array
     module procedure yggassign_complex16_1d_from_array
     module procedure yggassign_logical_1d_to_array
     module procedure yggassign_logical_1d_from_array
     module procedure yggassign_logical1_1d_to_array
     module procedure yggassign_logical1_1d_from_array
     module procedure yggassign_logical2_1d_to_array
     module procedure yggassign_logical2_1d_from_array
     module procedure yggassign_logical4_1d_to_array
     module procedure yggassign_logical4_1d_from_array
     module procedure yggassign_logical8_1d_to_array
     module procedure yggassign_logical8_1d_from_array
     ! TODO: ND array
  end interface yggassign
#endif
  !> @brief Convert an object to type(yggarr)
  !> @param[in] input Object to convert.
  !> @returns Converted object.
  interface yggarr
     module procedure ygggeneric2yggarr
  end interface yggarr
  !> @brief Convert an object to type(yggmap)
  !> @param[in] input Object to convert.
  !> @returns Converted object.
  interface yggmap
     module procedure ygggeneric2yggmap
  end interface yggmap
  !> @brief Convert an object to type(yggschema)
  !> @param[in] input Object to convert.
  !> @returns Converted object.
  interface yggschema
     module procedure ygggeneric2yggschema
  end interface yggschema
  !> @brief Convert an object to type(yggpyinst)
  !> @param[in] input Object to convert.
  !> @returns Converted object.
  interface yggpyinst
     module procedure ygggeneric2yggpyinst
  end interface yggpyinst
  !> @brief Convert an object to type(ygggeneric)
  !> @param[in] input Object to convert.
  !> @returns Converted object.
  interface ygggeneric
     module procedure yggarr2ygggeneric
     module procedure yggmap2ygggeneric
     module procedure yggschema2ygggeneric
     module procedure yggpyinst2ygggeneric
  end interface ygggeneric
  !> @brief Convert an object to type(yggpyfunc)
  !> @param[in] input Object to convert.
  !> @returns Converted object.
  interface yggpyfunc
     module procedure yggpython2yggpyfunc
  end interface yggpyfunc
  !> @brief Convert an object to type(yggpython)
  !> @param[in] input Object to convert.
  !> @returns Converted object.
  interface yggpython
     module procedure yggpython2yggpython
     module procedure yggpyfunc2yggpython
  end interface yggpython
  
  !> @brief Constructor for an input comm.
  !>   Create a yggcomm structure for an input channel based on a provided 
  !>   name that is used to locate a particular comm address stored in an 
  !>   environment variable.
  !> @param[in] name Name of the channel.
  !> @param[in] datatype Data structure containing type information.
  !> @returns Input comm structure.
  interface ygg_input
     module procedure ygg_input
     module procedure ygg_input_type
     module procedure ygg_input_fmt
  end interface ygg_input
  !> @brief Constructor for an output comm.
  !>   Create a yggcomm structure for an output channel based on a provided 
  !>   name that is used to locate a particular comm address stored in an
  !>   environment variable.
  !> @param[in] name Name of the channel.
  !> @param[in] datatype Data structure containing type information.
  !> @returns Output comm structure.
  interface ygg_output
     module procedure ygg_output
     module procedure ygg_output_type
     module procedure ygg_output_fmt
  end interface ygg_output
  !> @brief Send one or more variables.
  !> @param[in] ygg_q Output/RPC/Timesync comm.
  !> @param[in] args One or more variables to send.
  !> @returns flag Success (.true.) or failure (.false.) of the send.
  interface ygg_send_var
     module procedure ygg_send_var_sing
     module procedure ygg_send_var_mult
  end interface ygg_send_var
  interface ygg_send
     module procedure ygg_send
     module procedure ygg_send_var_sing
     module procedure ygg_send_var_mult
  end interface ygg_send
  !> @brief Receive data into one or more variables that cannot be resized.
  !> @param[in] ygg_q Output/RPC/Timesync comm.
  !> @param[in,out] args One or more variables to receive into.
  !> @returns flag Success (.true.) or failure (.false.) of the send.
  interface ygg_recv_var
     module procedure ygg_recv_var_sing
     module procedure ygg_recv_var_mult
  end interface ygg_recv_var
  interface ygg_recv
     module procedure ygg_recv
     module procedure ygg_recv_var_sing
     module procedure ygg_recv_var_mult
  end interface ygg_recv
  !> @brief Receive data into one or more variables that can be resized.
  !> @param[in] ygg_q Output/RPC/Timesync comm.
  !> @param[in,out] args One or more variables to receive into.
  !> @returns flag Success (.true.) or failure (.false.) of the send.
  interface ygg_recv_var_realloc
     module procedure ygg_recv_var_realloc_sing
     module procedure ygg_recv_var_realloc_mult
  end interface ygg_recv_var_realloc
  interface ygg_recv_realloc
     module procedure ygg_recv_nolimit
     module procedure ygg_recv_var_realloc_sing
     module procedure ygg_recv_var_realloc_mult
  end interface ygg_recv_realloc
  !> @brief Send a request and receive a response into one or more variables
  !>   that cannot be resized.
  !> @param[in] ygg_q RPC/Timesync comm.
  !> @param[in] oargs One or more variables to send in the request.
  !> @param[in,out] iargs One or more variables to receive the response 
  !>   into that cannot be resized.
  !> @returns flag Success (.true.) or failure (.false.) of the call.
  interface ygg_rpc_call
     module procedure ygg_rpc_call_1v1
     module procedure ygg_rpc_call_1vm
     module procedure ygg_rpc_call_mv1
     module procedure ygg_rpc_call_mult
  end interface ygg_rpc_call
  !> @brief Send a request and receive a response into one or more variables
  !>   that can be resized.
  !> @param[in] ygg_q RPC/Timesync comm.
  !> @param[in] oargs One or more variables to send in the request.
  !> @param[in,out] iargs One or more variables to receive the response 
  !>   into that can be resized.
  !> @returns flag Success (.true.) or failure (.false.) of the call.
  interface ygg_rpc_call_realloc
     module procedure ygg_rpc_call_realloc_1v1
     module procedure ygg_rpc_call_realloc_1vm
     module procedure ygg_rpc_call_realloc_mv1
     module procedure ygg_rpc_call_realloc_mult
  end interface ygg_rpc_call_realloc
  !> @brief Initialize a ygguint instance.
  !> @param[in] x Integer that unsigned integer object should be initialized
  !>   with. The kind of input integer will determine the precision of the
  !>   unsigned int object.
  !> @returns A ygguint instance.
  interface ygguint
     module procedure init_ygguint1
     module procedure init_ygguint2
     module procedure init_ygguint4
     module procedure init_ygguint8
  end interface ygguint
  !> @brief Wrapper for a C comm object.
  type, bind(c) :: yggcomm
     type(c_ptr) :: comm !< C comm object
  end type yggcomm
  !> @brief Wrapper for a C data type
  type, bind(c) :: yggdtype
     type(c_ptr) :: ptr !< C data type
  end type yggdtype
  !> @brief Wrapper for a reallocatable character array.
  type :: yggchar_r
     character(kind=c_char), dimension(:), pointer :: x => null() !< Wrapped array
  end type yggchar_r
  !> @brief Wrapper for a reallocatable 1D array of c_long.
  type :: c_long_1d
     integer(kind=c_long), dimension(:), pointer :: x => null() !< Wrapped array
  end type c_long_1d
  !> @brief Wrapper for a reallocatable 1D array of 1 byte unsigned integers.
  type :: unsigned1_1d
     integer(kind=1), dimension(:), pointer :: x => null() !< Wrapped array
  end type unsigned1_1d
  !> @brief Wrapper for a reallocatable 1D array of 2 byte unsigned integers.
  type :: unsigned2_1d
     integer(kind=2), dimension(:), pointer :: x => null() !< Wrapped array
  end type unsigned2_1d
  !> @brief Wrapper for a reallocatable 1D array of 4 byte unsigned integers.
  type :: unsigned4_1d
     integer(kind=4), dimension(:), pointer :: x => null() !< Wrapped array
  end type unsigned4_1d
  !> @brief Wrapper for a reallocatable 1D array of 8 byte unsigned integers.
  type :: unsigned8_1d
     integer(kind=8), dimension(:), pointer :: x => null() !< Wrapped array
  end type unsigned8_1d
  !> @brief Wrapper for a reallocatable 1D array of 1 byte integers.
  type :: integer_1d
     integer, dimension(:), pointer :: x => null() !< Wrapped array
  end type integer_1d
  !> @brief Wrapper for a reallocatable 1D array of 2 byte integers.
  type :: integer2_1d
     integer(kind=2), dimension(:), pointer :: x => null() !< Wrapped array
  end type integer2_1d
  !> @brief Wrapper for a reallocatable 1D array of 4 byte integers.
  type :: integer4_1d
     integer(kind=4), dimension(:), pointer :: x => null() !< Wrapped array
  end type integer4_1d
  !> @brief Wrapper for a reallocatable 1D array of 8 byte integers.
  type :: integer8_1d
     integer(kind=8), dimension(:), pointer :: x => null() !< Wrapped array
  end type integer8_1d
  !> @brief Wrapper for a reallocatable 1D array of real.
  type :: real_1d
     real, dimension(:), pointer :: x => null() !< Wrapped array
  end type real_1d
  !> @brief Wrapper for a reallocatable 1D array of 4 byte real.
  type :: real4_1d
     real(kind=4), dimension(:), pointer :: x => null() !< Wrapped array
  end type real4_1d
  !> @brief Wrapper for a reallocatable 1D array of 8 byte real.
  type :: real8_1d
     real(kind=8), dimension(:), pointer :: x => null() !< Wrapped array
  end type real8_1d
#ifdef _WIN32
  !> @brief Wrapper for a reallocatable 1D array of 16 byte real.
  type :: real16_1d
     real(kind=8), dimension(:), pointer :: x => null() !< Wrapped array
  end type real16_1d
#else
  !> @brief Wrapper for a reallocatable 1D array of 16 byte real.
  type :: real16_1d
     real(kind=16), dimension(:), pointer :: x => null() !< Wrapped array
  end type real16_1d
#endif
  !> @brief Wrapper for a reallocatable 1D array of complex.
  type :: complex_1d
     complex, dimension(:), pointer :: x => null() !< Wrapped array
  end type complex_1d
  !> @brief Wrapper for a reallocatable 1D array of 4 byte complex.
  type :: complex4_1d
     complex(kind=4), dimension(:), pointer :: x => null() !< Wrapped array
  end type complex4_1d
  !> @brief Wrapper for a reallocatable 1D array of 8 byte complex.
  type :: complex8_1d
     complex(kind=8), dimension(:), pointer :: x => null() !< Wrapped array
  end type complex8_1d
#ifdef _WIN32
  !> @brief Wrapper for a reallocatable 1D array of 16 byte complex.
  type :: complex16_1d
     complex(kind=8), dimension(:), pointer :: x => null() !< Wrapped array
  end type complex16_1d
#else
  !> @brief Wrapper for a reallocatable 1D array of 16 byte complex.
  type :: complex16_1d
     complex(kind=16), dimension(:), pointer :: x => null() !< Wrapped array
  end type complex16_1d
#endif
  !> @brief Wrapper for a reallocatable 1D array of logical.
  type :: logical_1d
     logical, dimension(:), pointer :: x => null() !< Wrapped array
  end type logical_1d
  !> @brief Wrapper for a reallocatable 1D array of 1 byte logical.
  type :: logical1_1d
     logical(kind=1), dimension(:), pointer :: x => null() !< Wrapped array
  end type logical1_1d
  !> @brief Wrapper for a reallocatable 1D array of 2 byte logical.
  type :: logical2_1d
     logical(kind=2), dimension(:), pointer :: x => null() !< Wrapped array
  end type logical2_1d
  !> @brief Wrapper for a reallocatable 1D array of 4 byte logical.
  type :: logical4_1d
     logical(kind=4), dimension(:), pointer :: x => null() !< Wrapped array
  end type logical4_1d
  !> @brief Wrapper for a reallocatable 1D array of 8 byte logical.
  type :: logical8_1d
     logical(kind=8), dimension(:), pointer :: x => null() !< Wrapped array
  end type logical8_1d
  !> @brief Wrapper for a reallocatable 1D array of character arrays.
  type :: character_1d
     type(yggchar_r), dimension(:), pointer :: x => null() !< Wrapped array
  end type character_1d
  !> @brief Wrapper for a reallocatable ND array of 1 byte unsigned integer.
  type :: unsigned1_nd
     integer(kind=1), dimension(:), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type unsigned1_nd
  !> @brief Wrapper for a reallocatable ND array of 2 byte unsigned integer.
  type :: unsigned2_nd
     integer(kind=2), dimension(:), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type unsigned2_nd
  !> @brief Wrapper for a reallocatable ND array of 4 byte unsigned integer.
  type :: unsigned4_nd
     integer(kind=4), dimension(:), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type unsigned4_nd
  !> @brief Wrapper for a reallocatable ND array of 8 byte unsigned integer.
  type :: unsigned8_nd
     integer(kind=8), dimension(:), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type unsigned8_nd
  !> @brief Wrapper for a reallocatable ND array of c_long.
  type :: c_long_nd
     integer(kind=c_long), dimension(:), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type c_long_nd
  !> @brief Wrapper for a reallocatable ND array of integer.
  type :: integer_nd
     integer, dimension(:), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type integer_nd
  !> @brief Wrapper for a reallocatable ND array of 2 byte integer.
  type :: integer2_nd
     integer(kind=2), dimension(:), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type integer2_nd
  !> @brief Wrapper for a reallocatable ND array of 4 byte integer.
  type :: integer4_nd
     integer(kind=4), dimension(:), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type integer4_nd
  !> @brief Wrapper for a reallocatable ND array of 8 byte integer.
  type :: integer8_nd
     integer(kind=8), dimension(:), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type integer8_nd
  !> @brief Wrapper for a reallocatable ND array of real.
  type :: real_nd
     real, dimension(:), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type real_nd
  !> @brief Wrapper for a reallocatable ND array of 4 byte real.
  type :: real4_nd
     real(kind=4), dimension(:), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type real4_nd
  !> @brief Wrapper for a reallocatable ND array of 8 byte real.
  type :: real8_nd
     real(kind=8), dimension(:), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type real8_nd
#ifdef _WIN32
  !> @brief Wrapper for a reallocatable ND array of 16 byte real.
  type :: real16_nd
     real(kind=8), dimension(:), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type real16_nd
#else
  !> @brief Wrapper for a reallocatable ND array of 16 byte real.
  type :: real16_nd
     real(kind=16), dimension(:), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type real16_nd
#endif
  !> @brief Wrapper for a reallocatable ND array of complex.
  type :: complex_nd
     complex, dimension(:), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type complex_nd
  !> @brief Wrapper for a reallocatable ND array of 4 byte complex.
  type :: complex4_nd
     complex(kind=4), dimension(:), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type complex4_nd
  !> @brief Wrapper for a reallocatable ND array of 8 byte complex.
  type :: complex8_nd
     complex(kind=8), dimension(:), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type complex8_nd
#ifdef _WIN32
  !> @brief Wrapper for a reallocatable ND array of 16 byte complex.
  type :: complex16_nd
     complex(kind=8), dimension(:), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type complex16_nd
#else
  !> @brief Wrapper for a reallocatable ND array of 16 byte complex.
  type :: complex16_nd
     complex(kind=16), dimension(:), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type complex16_nd
#endif
  !> @brief Wrapper for a reallocatable ND array of logical.
  type :: logical_nd
     logical, dimension(:), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type logical_nd
  !> @brief Wrapper for a reallocatable ND array of 1 byte logical.
  type :: logical1_nd
     logical(kind=1), dimension(:), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type logical1_nd
  !> @brief Wrapper for a reallocatable ND array of 2 byte logical.
  type :: logical2_nd
     logical(kind=2), dimension(:), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type logical2_nd
  !> @brief Wrapper for a reallocatable ND array of 4 byte logical.
  type :: logical4_nd
     logical(kind=4), dimension(:), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type logical4_nd
  !> @brief Wrapper for a reallocatable ND array of 8 byte logical.
  type :: logical8_nd
     logical(kind=8), dimension(:), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type logical8_nd
  !> @brief Wrapper for a reallocatable ND array of bytes.
  type :: bytes_nd
     character(len=:), dimension(:), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type bytes_nd
  !> @brief Wrapper for a reallocatable ND array of unicode.
  type :: unicode_nd
     character(kind=ucs4, len=:), dimension(:), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type unicode_nd
  !> @brief Wrapper for a reallocatable ND array of character arrays.
  type :: character_nd
     type(yggchar_r), dimension(:), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type character_nd
  !> @brief Wrapper for a reallocatable 2D array of 1 byte unsigned integer.
  type :: unsigned1_2d
     integer(kind=1), dimension(:, :), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type unsigned1_2d
  !> @brief Wrapper for a reallocatable 2D array of 2 byte unsigned integer.
  type :: unsigned2_2d
     integer(kind=2), dimension(:, :), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type unsigned2_2d
  !> @brief Wrapper for a reallocatable 2D array of 4 byte unsigned integer.
  type :: unsigned4_2d
     integer(kind=4), dimension(:, :), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type unsigned4_2d
  !> @brief Wrapper for a reallocatable 2D array of 8 byte unsigned integer.
  type :: unsigned8_2d
     integer(kind=8), dimension(:, :), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type unsigned8_2d
  !> @brief Wrapper for a reallocatable 2D array of c_long.
  type :: c_long_2d
     integer(kind=c_long), dimension(:, :), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type c_long_2d
  !> @brief Wrapper for a reallocatable 2D array of integer.
  type :: integer_2d
     integer, dimension(:, :), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type integer_2d
  !> @brief Wrapper for a reallocatable 2D array of 2 byte integer.
  type :: integer2_2d
     integer(kind=2), dimension(:, :), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type integer2_2d
  !> @brief Wrapper for a reallocatable 2D array of 4 byte integer.
  type :: integer4_2d
     integer(kind=4), dimension(:, :), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type integer4_2d
  !> @brief Wrapper for a reallocatable 2D array of 8 byte integer.
  type :: integer8_2d
     integer(kind=8), dimension(:, :), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type integer8_2d
  !> @brief Wrapper for a reallocatable 2D array of real.
  type :: real_2d
     real, dimension(:, :), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type real_2d
  !> @brief Wrapper for a reallocatable 2D array of 4 byte real.
  type :: real4_2d
     real(kind=4), dimension(:, :), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type real4_2d
  !> @brief Wrapper for a reallocatable 2D array of 8 byte real.
  type :: real8_2d
     real(kind=8), dimension(:, :), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type real8_2d
#ifdef _WIN32
  !> @brief Wrapper for a reallocatable 2D array of 16 byte real.
  type :: real16_2d
     real(kind=8), dimension(:, :), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type real16_2d
#else
  !> @brief Wrapper for a reallocatable 2D array of 16 byte real.
  type :: real16_2d
     real(kind=16), dimension(:, :), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type real16_2d
#endif
  !> @brief Wrapper for a reallocatable 2D array of complex.
  type :: complex_2d
     complex, dimension(:, :), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type complex_2d
  !> @brief Wrapper for a reallocatable 2D array of 4 byte complex.
  type :: complex4_2d
     complex(kind=4), dimension(:, :), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type complex4_2d
  !> @brief Wrapper for a reallocatable 2D array of 8 byte complex.
  type :: complex8_2d
     complex(kind=8), dimension(:, :), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type complex8_2d
#ifdef _WIN32
  !> @brief Wrapper for a reallocatable 2D array of 16 byte complex.
  type :: complex16_2d
     complex(kind=8), dimension(:, :), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type complex16_2d
#else
  !> @brief Wrapper for a reallocatable 2D array of 16 byte complex.
  type :: complex16_2d
     complex(kind=16), dimension(:, :), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type complex16_2d
#endif
  !> @brief Wrapper for a reallocatable 2D array of logical.
  type :: logical_2d
     logical, dimension(:, :), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type logical_2d
  !> @brief Wrapper for a reallocatable 2D array of 1 byte logical.
  type :: logical1_2d
     logical(kind=1), dimension(:, :), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type logical1_2d
  !> @brief Wrapper for a reallocatable 2D array of 2 byte logical.
  type :: logical2_2d
     logical(kind=2), dimension(:, :), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type logical2_2d
  !> @brief Wrapper for a reallocatable 2D array of 4 byte logical.
  type :: logical4_2d
     logical(kind=4), dimension(:, :), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type logical4_2d
  !> @brief Wrapper for a reallocatable 2D array of 8 byte logical.
  type :: logical8_2d
     logical(kind=8), dimension(:, :), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type logical8_2d
  !> @brief Wrapper for a reallocatable 2D array of character arrays.
  type :: character_2d
     type(yggchar_r), dimension(:, :), pointer :: x => null() !< Wrapped array
     integer(kind=c_size_t), dimension(:), pointer :: shape => null() !< Shape of the array
  end type character_2d
  !> @brief A wrapper for any scalar or array accessed via a C pointer.
  !>   Only some of the members will be used for each specific type.
   type :: yggptr
     character(len=15) :: type = "none" !< Type of data wrapped
     logical :: ndarray = .false. !< .true. if the data is an ND array
     logical :: array = .false. !< .true. if the data is an array
     logical :: alloc = .false. !< .true. if the memory was allocated
     integer(kind=8) :: len = 0 !< Length of an 1D array
     integer(kind=8) :: prec = 0 !< Precison of scalar/array elements
     integer(kind=8) :: ndim = 0 !< Number of dimensions in an ND array
     integer(kind=8) :: nbytes = 0 !< Number of bytes in the wrapped data
     integer(kind=8), dimension(:), pointer :: shape => null() !< Shape of an ND array
     type(c_ptr) :: ptr = c_null_ptr !< C pointer to data
     class(*), pointer :: item => null() !< Fortran pointer to scalar data
     class(*), dimension(:), pointer :: item_array => null() !< Fortran pointer to 1D array
     class(*), dimension(:, :), pointer :: item_array_2d => null() !< Fortran pointer to 2D array
     class(*), dimension(:, :, :), pointer :: item_array_3d => null() !< Fortran pointer to 3D array
     character(len=:), dimension(:), pointer :: item_array_char => null() !< Explicit pointer to 1D character array
     character, dimension(:), pointer :: data_character_unit => null() !< Fortran pointer to array of characters
     character(kind=ucs4), dimension(:), &
          pointer :: data_unicode_unit => null() !< Fortran array to array of unicode characters
     integer(kind=c_size_t), pointer :: len_c => null() !< C variable for storing array length
     integer(kind=c_size_t), pointer :: prec_c => null() !< C variable for storing precision
     integer(kind=c_size_t), pointer :: ndim_c => null() !< C variable for storing number of array dimensions
     integer(kind=c_size_t), dimension(:), pointer :: shape_c => null() !< C variable for storing the array shape
     type(c_ptr) :: len_ptr = c_null_ptr !< C pointer to len_c variable
     type(c_ptr) :: prec_ptr = c_null_ptr !< C pointer to prec_c variable
     type(c_ptr) :: ndim_ptr = c_null_ptr !< C pointer to ndim_c variable
     type(c_ptr) :: shape_ptr = c_null_ptr !< C pointer to shape_c variable
  end type yggptr
  !> @brief Wrapper to a reallocatable array of pointers.
  type :: yggptr_arr
     type(yggptr), dimension(:), pointer :: vals => null() !< Wrapped array
  end type yggptr_arr
  !> @brief Wrapper to a reallocatable map of pointers.
  type :: yggptr_map
     character(len=20), dimension(:), pointer :: keys => null() !< Keys in the wrapped object
     type(yggptr), dimension(:), pointer :: vals => null() !< Values in the wrapped object
  end type yggptr_map
  !> @brief Wrapper for a C generic object.
  type, bind(c) :: ygggeneric
     type(c_ptr) :: obj = c_null_ptr !< Pointer to C generic object
  end type ygggeneric
  !> @brief Wrapper for a C generic ref object.
  type, bind(c) :: ygggenericref
     type(c_ptr) :: obj = c_null_ptr !< Pointer to C generic object
     type(c_ptr) :: allocator = c_null_ptr !< Pointer to allocator
  end type ygggenericref
  !> @brief Wrapper for C NULL object.
  type :: yggnull
     type(c_ptr) :: ptr = c_null_ptr !< C NULL object
   ! contains
   !   procedure :: write_null
   !   generic :: write(formatted) => write_null
  end type yggnull
  !> @brief Wrapper for an array of generic objects (stored in a generic object).
  type, bind(c) :: yggarr
     type(c_ptr) :: obj = c_null_ptr !< Pointer to wrapped array.
  end type yggarr
  !> @brief Wrapper for a mapping of generic objects (stored in a generic object).
  type, bind(c) :: yggmap
     type(c_ptr) :: obj = c_null_ptr !< Pointer to wrapped mapping.
  end type yggmap
  !> @brief Wrapper for a schema (stored in a generic object).
  type, bind(c) :: yggschema
     type(c_ptr) :: obj = c_null_ptr !< Pointer to wrapped schema.
  end type yggschema
  !> @brief  Wrapper for a Python instance (stored in a generic object).
  type, bind(c) :: yggpyinst
     type(c_ptr) :: obj = c_null_ptr !< Pointer to wrapped Python instance.
  end type yggpyinst
  !> @brief Wrapper for a Python function.
  type, bind(c) :: yggpyfunc
     type(c_ptr) :: obj = c_null_ptr !< Pointer to wrapped Python instance.
  end type yggpyfunc
  !> @brief Wrapper for a Python object.
  type, bind(c) :: yggpython
     type(c_ptr) :: obj = c_null_ptr !< Pointer to wrapped Python instance.
  end type yggpython
  !> @brief Ply structure.
  type, bind(c) :: yggply
     type(c_ptr) :: obj = c_null_ptr !< Pointer to wrapped rapidjson::Ply instance.
  end type yggply
  !> @brief Obj structure.
  type, bind(c) :: yggobj
     type(c_ptr) :: obj = c_null_ptr !< Pointer to wrapped rapidjson::ObjWavefront instance.
  end type yggobj
  !> @brief Wrapper for a 1 byte unsigned integer.
  type ygguint1
     integer(kind=1) :: x !< Wrapped scalar
  end type ygguint1
  !> @brief Wrapper for a 2 byte unsigned integer.
  type ygguint2
     integer(kind=2) :: x !< Wrapped scalar
  end type ygguint2
  !> @brief Wrapper for a 4 byte unsigned integer.
  type ygguint4
     integer(kind=4) :: x !< Wrapped scalar
  end type ygguint4
  !> @brief Wrapper for a 8 byte unsigned integer.
  type ygguint8
     integer(kind=8) :: x !< Wrapped scalar
  end type ygguint8
  !> @brief Wrapper for complex number with float components
  type, bind(c) :: yggcomplex_float
     real(kind=c_float) :: re !< Real component
     real(kind=c_float) :: im !< Imaginary component
  end type yggcomplex_float
  !> @brief Wrapper for complex number with double components
  type, bind(c) :: yggcomplex_double
     real(kind=c_double) :: re !< Real component
     real(kind=c_double) :: im !< Imaginary component
  end type yggcomplex_double
! #ifdef YGGDRASIL_LONG_DOUBLE_AVAILABLE
  !> @brief Wrapper for complex number with long double components
  type, bind(c) :: yggcomplex_long_double
     real(kind=c_long_double) :: re !< Real component
     real(kind=c_long_double) :: im !< Imaginary component
  end type yggcomplex_long_double
! #endif

#ifndef DOXYGEN_SHOULD_SKIP_THIS
  interface assignment(=)
     module procedure ygguint1_assign
     module procedure ygguint2_assign
     module procedure ygguint4_assign
     module procedure ygguint8_assign
  end interface assignment(=)
#endif

  public :: yggarg, yggchar_r, yggcomm, ygggeneric, ygggenericref, &
       yggptr, yggnull, yggarr, yggmap, &
       yggschema, yggpython, yggply, yggobj, yggpyinst, yggpyfunc, &
       LINE_SIZE_MAX, yggcomplex_float, yggcomplex_double
  
#ifdef YGGDRASIL_LONG_DOUBLE_AVAILABLE
  public :: yggcomplex_long_double
#endif

  include "YggInterface_cdef.F90"

#define WITH_GLOBAL_SCOPE(COMM) call set_global_comm(); COMM; call unset_global_comm()

  ! abstract interface
  !    logical(kind=c_bool) function func_abs(data_send, data_recv)
  !      use, intrinsic :: iso_c_binding, only: c_bool
  !      import :: ygggeneric
  !      implicit none
  !      type(ygggeneric), value, intent(in) :: data_send
  !      type(ygggeneric), value :: data_recv
  !    end function func_abs
  ! end interface
  
contains

  ! include "YggInterface_copy.F90"
  include "YggInterface_realloc.F90"
  include "YggInterface_c2f.F90"
  include "YggInterface_arg.F90"
  include "YggInterface_conv.F90"
  include "YggInterface_assign.F90"
  ! include "YggInterface_array.F90"
  ! include "YggInterface_map.F90"

#ifndef DOXYGEN_SHOULD_SKIP_THIS
  subroutine ygguint1_assign(self, other)
    type(ygguint1), intent(inout) :: self
    integer, intent(in) :: other
    self%x = int(other, kind=1)
  end subroutine ygguint1_assign
  subroutine ygguint2_assign(self, other)
    type(ygguint2), intent(inout) :: self
    integer, intent(in) :: other
    self%x = int(other, kind=2)
  end subroutine ygguint2_assign
  subroutine ygguint4_assign(self, other)
    type(ygguint4), intent(inout) :: self
    integer, intent(in) :: other
    self%x = int(other, kind=4)
  end subroutine ygguint4_assign
  subroutine ygguint8_assign(self, other)
    type(ygguint8), intent(inout) :: self
    integer, intent(in) :: other
    self%x = int(other, kind=8)
  end subroutine ygguint8_assign


#ifndef _WIN32
  function yggarg_scalar_real16(x) result (y)
    type(yggptr) :: y
    real(kind=16), target :: x
    real(kind=16), pointer :: xp
    y = yggarg_scalar_init(x)
    xp => x
    y%type = "real"
    y%ptr = c_loc(xp)
    y%nbytes = 16
  end function yggarg_scalar_real16
  function yggarg_scalar_complex16(x) result (y)
    type(yggptr) :: y
    complex(kind=16), target :: x
    complex(kind=16), pointer :: xp
    y = yggarg_scalar_init(x)
    xp => x
    y%type = "complex"
    y%ptr = c_loc(xp)
    y%nbytes = 16 * 2
  end function yggarg_scalar_complex16
  function yggarg_1darray_real16(x, x_shape) result (y)
    real(kind=16), dimension(:), target :: x
    real(kind=16), dimension(:), pointer :: xp
    integer, dimension(:), optional :: x_shape
    type(yggptr) :: y
    xp => x
    if (present(x_shape)) then
       y = yggarg_ndarray_init(x, x_shape)
    else
       y = yggarg_ndarray_init(x)
    end if
    y%type = "real"
    y%ptr = c_loc(xp(1))
  end function yggarg_1darray_real16
  function yggarg_1darray_complex16(x, x_shape) result (y)
    complex(kind=16), dimension(:), target :: x
    complex(kind=16), dimension(:), pointer :: xp
    integer, dimension(:), optional :: x_shape
    type(yggptr) :: y
    xp => x
    if (present(x_shape)) then
       y = yggarg_ndarray_init(x, x_shape)
    else
       y = yggarg_ndarray_init(x)
    end if
    y%type = "complex"
    y%ptr = c_loc(xp(1))
  end function yggarg_1darray_complex16
  ! function yggarg_1darray_real16(x) result (y)
  !   real(kind=16), dimension(:), target :: x
  !   real(kind=16), dimension(:), pointer :: xp
  !   type(yggptr) :: y
  !   xp => x
  !   y = yggarg_ndarray_init(x)
  !   y%type = "real"
  !   y%ptr = c_loc(xp(1))
  ! end function yggarg_1darray_real16
  ! function yggarg_1darray_complex16(x) result (y)
  !   complex(kind=16), dimension(:), target :: x
  !   complex(kind=16), dimension(:), pointer :: xp
  !   type(yggptr) :: y
  !   xp => x
  !   y = yggarg_ndarray_init(x)
  !   y%type = "complex"
  !   y%ptr = c_loc(xp(1))
  ! end function yggarg_1darray_complex16
  function yggarg_2darray_real16(x) result (y)
    real(kind=16), dimension(:, :), target :: x
    real(kind=16), dimension(:), pointer :: xp
    type(yggptr) :: y
    allocate(xp(size(x)))
    xp = reshape(x, [size(x)])
    y = yggarg(xp, shape(x))
    call yggarg_2darray_init(y, x)
  end function yggarg_2darray_real16
  function yggarg_2darray_complex16(x) result (y)
    complex(kind=16), dimension(:, :), target :: x
    complex(kind=16), dimension(:), pointer :: xp
    type(yggptr) :: y
    allocate(xp(size(x)))
    xp = reshape(x, [size(x)])
    y = yggarg(xp, shape(x))
    call yggarg_2darray_init(y, x)
  end function yggarg_2darray_complex16
#endif
  
  ! Utilities
  function init_ygguint1(x) result(y)
    integer(kind=1) :: x
    type(ygguint1) :: y
    y%x = x
    if (y%x.lt.0) stop "Unsigned int cannot be less than 0."
  end function init_ygguint1
  function init_ygguint2(x) result(y)
    integer(kind=2) :: x
    type(ygguint2) :: y
    y%x = x
    if (y%x.lt.0) stop "Unsigned int cannot be less than 0."
  end function init_ygguint2
  function init_ygguint4(x) result(y)
    integer(kind=4) :: x
    type(ygguint4) :: y
    y%x = x
    if (y%x.lt.0) stop "Unsigned int cannot be less than 0."
  end function init_ygguint4
  function init_ygguint8(x) result(y)
    integer(kind=8) :: x
    type(ygguint8) :: y
    y%x = x
    if (y%x.lt.0) stop "Unsigned int cannot be less than 0."
  end function init_ygguint8
#endif
  !> @brief Display a null object (for completeness).
  !> @param[in] x Instance to display.
  subroutine display_null(x)
    class(yggnull), intent(in) :: x
    write (*, '("NULL")')
  end subroutine display_null
  ! subroutine write_null(dtv, unit, iotype, v_list, iostat, iomsg)
  !   ! Argument names here from the std, but you can name them differently.
  !   class(yggnull), intent(in) :: dtv   ! Object to write.
  !   integer, intent(in) :: unit         ! Internal unit to write to.
  !   character(*), intent(in) :: iotype  ! LISTDIRECTED or DTxxx
  !   integer, intent(in) :: v_list(:)    ! parameters from fmt spec.
  !   integer, intent(out) :: iostat      ! non zero on error, etc.
  !   character(*), intent(inout) :: iomsg  ! define if iostat non zero.
  !   write (unit, '("NULL")', IOSTAT=iostat, IOMSG=iomsg)
  ! end subroutine write_null
#ifndef DOXYGEN_SHOULD_SKIP_THIS
  subroutine fix_format_str(x)
    implicit none
    character(len=*) :: x
    integer :: i, length
    length = len(x)
    i = index(x, "\t")
    do while (i.ne.0)
       x(i:i) = char(9)
       x((i+1):length) = x((i+2):length)
       length = len(x)
       i = index(x, "\t")
    end do
    i = index(x, "\n")
    do while (i.ne.0)
       x(i:i) = NEW_LINE('c')
       x((i+1):length) = x((i+2):length)
       length = len(x)
       i = index(x, "\n")
    end do
  end subroutine fix_format_str
#endif

  ! YggInterface

  !> @brief Register a Fortran function
  !> @param[in] name Function name
  !> @param[in] func Function to register
  subroutine register_function(name, func)
    implicit none
    character(len=*), intent(in) :: name
    character(len=len(name)+9) :: prefixed_name
    ! procedure(func_abs), pointer :: func
    character(kind=c_char), allocatable :: c_name(:)
    type(c_funptr) :: c_func
#ifndef DOXYGEN_SHOULD_SKIP_THIS
    !> @brief Function wrapped to handle ygggeneric input and output
    !>   that can be called from C++
    !> @param[in] data_send Input data
    !> @param[out] data_recv Output data structure that will be filled
    !> @returns Success or failure of the function call
    interface
       logical(kind=c_bool) function func(data_send, data_recv) &
            bind(c)
         use, intrinsic :: iso_c_binding, only: c_bool
         import :: ygggeneric
         implicit none
         type(ygggeneric), value, intent(in) :: data_send
         type(ygggeneric), value :: data_recv
       end function func
    end interface
#endif
    prefixed_name = 'fortran::' // trim(name)
    c_name = convert_string_f2c(prefixed_name)
    c_func = c_funloc(func)
    call register_function_c(c_name, c_func)
    deallocate(c_name)
  end subroutine register_function

  ! Methods for initializing channels
#ifndef DOXYGEN_SHOULD_SKIP_THIS
  function is_comm_format_array_type(x, args) result(out)
    implicit none
    type(yggcomm), intent(in) :: x
    type(yggptr) :: args(:)
    logical :: out
    integer(c_int) :: c_out
    integer :: i
    c_out = is_comm_format_array_type_c(x)
    if (c_out.eq.0) then
       out = .false.
    else if (c_out.eq.1) then
       out = .true.
    else if (size(args).eq.1) then
       out = .false.
    else
       out = .true.
       do i = 2, size(args)
          if (.not.args(i)%array) then
             out = .false.
             exit
          end if
       end do
       if ((out).and.(.not.((args(1)%array).or.(is_size_t(args(1)))))) then
          out = .false.
       end if
       ! stop "is_comm_format_array_type: Error checking type."
    end if
  end function is_comm_format_array_type
#endif

  !> @brief Constructor for a generic comm for testing purposes.
  !> @param[in] name Name of the channel.
  !> @param[in] dir Direction for the comm.
  !> @param[in] t Communicator type to create.
  !> @param[in] datatype Data type for the communicator.
  !> @param[in] flags Bitwise flags describing the communicator.
  !> @param[in] ncomm Optional number of communicators in a forked comm.
  !> @returns Comm structure.
  function init_comm(name, dir, t, datatype, flags, ncomm) &
       result(channel)
    implicit none
    character(len=*), intent(in) :: name
    character(kind=c_char), allocatable :: c_name(:)
    integer, intent(in) :: dir, t
    integer(kind=8), value, intent(in) :: flags
    integer, optional :: ncomm
    integer(kind=c_int) :: c_dir, c_t
    integer(kind=c_int64_t) :: c_flags
    integer(kind=c_size_t) :: c_ncomm
    type(yggdtype), target :: datatype
    type(c_ptr) :: c_datatype
    type(yggcomm) :: channel
    if (present(ncomm)) then
       c_ncomm = ncomm
    else
       c_ncomm = 0
    end if
    c_name = convert_string_f2c(name)
    c_dir = dir
    c_t = t
    c_flags = flags
    c_datatype = c_loc(datatype)
    write(*, *) "flags = ", flags, ", c_flags = ", c_flags
    channel = init_comm_c(c_name, c_dir, c_t, c_datatype, &
         c_flags, c_ncomm)
    deallocate(c_name)
  end function init_comm
  
  !> @brief Constructor for an output comm.
  !>   Create a yggcomm structure for an output channel based on a provided 
  !>   name that is used to locate a particular comm address stored in an
  !>   environment variable.
  !> @param[in] name Name of the channel.
  !> @param[in] commtype Optional communicator type code.
  !> @param[in] flags Optional bitwise flags describing communicator.
  !> @returns Output comm structure.
  function ygg_output(name, commtype, flags) result(channel)
    implicit none
    character(len=*), intent(in) :: name
    integer(kind=c_int), intent(in), optional :: commtype
    integer(kind=8), intent(in), optional :: flags
    character(kind=c_char), allocatable :: c_name(:)
    integer(kind=c_int) :: c_commtype
    integer(kind=c_int64_t) :: c_flags
    integer(kind=c_size_t) :: c_ncomm
    type(c_ptr) :: c_datatype
    type(yggcomm) :: channel
    c_name = convert_string_f2c(name)
    if (present(commtype)) then
       c_commtype = commtype
    else
       c_commtype = DEFAULT_COMM
    end if
    if (present(flags)) then
       c_flags = flags
    else
       c_flags = 0
    end if
    c_ncomm = 0
    c_flags = IOR(c_flags, COMM_FLAG_INTERFACE)
    c_datatype = c_null_ptr
    channel = init_comm_c(c_name, SEND, c_commtype, c_datatype, &
         c_flags, c_ncomm)
    deallocate(c_name)
  end function ygg_output
  
  !> @brief Constructor for an input comm.
  !>   Create a yggcomm structure for an input channel based on a provided 
  !>   name that is used to locate a particular comm address stored in an 
  !>   environment variable.
  !> @param[in] name Name of the channel.
  !> @param[in] commtype Optional communicator type code.
  !> @param[in] flags Optional bitwise flags describing communicator.
  !> @returns Input comm structure.
  function ygg_input(name, commtype, flags) result(channel)
    implicit none
    character(len=*), intent(in) :: name
    integer(kind=c_int), intent(in), optional :: commtype
    integer(kind=8), intent(in), optional :: flags
    character(kind=c_char), allocatable :: c_name(:)
    integer(kind=c_int) :: c_commtype
    integer(kind=c_int64_t) :: c_flags
    integer(kind=c_size_t) :: c_ncomm
    type(c_ptr) :: c_datatype
    type(yggcomm) :: channel
    c_name = convert_string_f2c(name)
    if (present(commtype)) then
       c_commtype = commtype
    else
       c_commtype = DEFAULT_COMM
    end if
    if (present(flags)) then
       c_flags = flags
    else
       c_flags = 0
    end if
    c_flags = IOR(c_flags, COMM_FLAG_INTERFACE)
    c_ncomm = 0
    c_datatype = c_null_ptr
    channel = init_comm_c(c_name, RECV, c_commtype, c_datatype, &
         c_flags, c_ncomm)
    deallocate(c_name)
  end function ygg_input

  !> @brief Constructor for an output comm that will send a specific data 
  !>   type. Create a yggcomm structure for an output channel based on a 
  !>   provided name that is used to locate a particular comm address stored 
  !>   in an environment variable and a structure defining the datatype of
  !>   outgoing messages.
  !> @param[in] name Name of the channel.
  !> @param[in] datatype Data structure containing type information.
  !> @param[in] commtype Optional communicator type code.
  !> @param[in] flags Optional bitwise flags describing communicator.
  !> @returns Output comm structure.
  function ygg_output_type(name, datatype, commtype, flags) &
       result(channel)
    implicit none
    character(len=*), intent(in) :: name
    type(yggdtype), target :: datatype
    integer(kind=c_int), intent(in), optional :: commtype
    integer(kind=8), intent(in), optional :: flags
    type(c_ptr) :: c_datatype
    character(kind=c_char), allocatable :: c_name(:)
    integer(kind=c_int) :: c_commtype
    integer(kind=c_int64_t) :: c_flags
    integer(kind=c_size_t) :: c_ncomm
    type(yggcomm) :: channel
    c_name = convert_string_f2c(name)
    c_datatype = c_loc(datatype)
    if (present(commtype)) then
       c_commtype = commtype
    else
       c_commtype = DEFAULT_COMM
    end if
    if (present(flags)) then
       c_flags = flags
    else
       c_flags = 0
    end if
    c_flags = IOR(c_flags, COMM_FLAG_INTERFACE)
    c_ncomm = 0
    channel = init_comm_c(c_name, SEND, c_commtype, c_datatype, &
         c_flags, c_ncomm)
    deallocate(c_name)
  end function ygg_output_type
  
  !> @brief Constructor for an input comm that will receive a specific data 
  !>   type. Create a yggcomm structure for an input channel based on a 
  !>   provided name that is used to locate a particular comm address stored 
  !>   in an environment variable and a structure defining the datatype of
  !>   incoming messages.
  !> @param[in] name Name of the channel.
  !> @param[in] datatype Data structure containing type information.
  !> @param[in] commtype Optional communicator type code.
  !> @param[in] flags Optional bitwise flags describing communicator.
  !> @returns Input comm structure.
  function ygg_input_type(name, datatype, commtype, flags) &
       result(channel)
    implicit none
    character(len=*), intent(in) :: name
    type(yggdtype), target :: datatype
    integer(kind=c_int), intent(in), optional :: commtype
    integer(kind=8), intent(in), optional :: flags
    type(c_ptr) :: c_datatype
    character(kind=c_char), allocatable :: c_name(:)
    integer(kind=c_int) :: c_commtype
    integer(kind=c_int64_t) :: c_flags
    integer(kind=c_size_t) :: c_ncomm
    type(yggcomm) :: channel
    c_name = convert_string_f2c(name)
    c_datatype = c_loc(datatype)
    if (present(commtype)) then
       c_commtype = commtype
    else
       c_commtype = DEFAULT_COMM
    end if
    if (present(flags)) then
       c_flags = flags
    else
       c_flags = 0
    end if
    c_flags = IOR(c_flags, COMM_FLAG_INTERFACE)
    c_ncomm = 0
    channel = init_comm_c(c_name, RECV, c_commtype, c_datatype, &
         c_flags, c_ncomm)
    deallocate(c_name)
  end function ygg_input_type
  
  !> @brief Constructor for a client-side RPC comm with explicit datatypes.
  !>   Create a yggcomm structure for the client side of an RPC channel based
  !>   on a provided name that is used to locate a particular comm address
  !>   stored in an environment variable. Types can be specified by datatype
  !>   structures.
  !> @param[in] name Name of the channel.
  !> @param[in] out_type_in Datatype structure containing information on
  !>   the type of data that requests (outgoing messages) will contain.
  !> @param[in] in_type_in Datatype structure containing information on
  !>   the type of data that responses (incoming messages) will contain.
  !> @returns Client comm structure.
  function ygg_rpc_client_type(name, out_type_in, in_type_in) result(channel)
    implicit none
    character(len=*), intent(in) :: name
    type(yggdtype), target, optional :: out_type_in
    type(yggdtype), target, optional :: in_type_in
    type(c_ptr) :: c_out_type
    type(c_ptr) :: c_in_type
    character(kind=c_char), allocatable :: c_name(:)
    type(yggcomm) :: channel
    if (present(out_type_in)) then
       c_out_type = c_loc(out_type_in) ! %ptr
    else
       c_out_type = c_null_ptr
    end if
    if (present(in_type_in)) then
       c_in_type = c_loc(in_type_in) ! %ptr
    else
       c_in_type = c_null_ptr
    end if
    c_name = convert_string_f2c(name)
    channel = ygg_rpc_client_type_c(c_name, c_out_type, c_in_type)
    deallocate(c_name)
  end function ygg_rpc_client_type

  !> @brief Constructor for a server-side RPC comm with explicit datatypes.
  !>   Create a yggcomm structure for the server side of an RPC channel based
  !>   on a provided name that is used to locate a particular comm address
  !>   stored in an environment variable. Types can be specified by datatype
  !>   structures.
  !> @param[in] name Name of the channel.
  !> @param[in] in_type_in Datatype structure containing information on
  !>   the type of data that requests (incoming messages) will contain.
  !> @param[in] out_type_in Datatype structure containing information on
  !>   the type of data that responses (outgoing messages) will contain.
  !> @returns Server comm structure.
  function ygg_rpc_server_type(name, in_type_in, out_type_in) result(channel)
    implicit none
    character(len=*), intent(in) :: name
    type(yggdtype), target, optional :: in_type_in
    type(yggdtype), target, optional :: out_type_in
    type(c_ptr) :: c_in_type
    type(c_ptr) :: c_out_type
    character(kind=c_char), allocatable :: c_name(:)
    type(yggcomm) :: channel
    if (present(in_type_in)) then
       c_in_type = c_loc(in_type_in) ! %ptr
    else
       c_in_type = c_null_ptr
    end if
    if (present(out_type_in)) then
       c_out_type = c_loc(out_type_in) ! %ptr
    else
       c_out_type = c_null_ptr
    end if
    c_name = convert_string_f2c(name)
    channel = ygg_rpc_server_type_c(c_name, c_in_type, c_out_type)
    deallocate(c_name)
  end function ygg_rpc_server_type

  ! Methods for constructing/manipulating data types
  
  !> @brief Print a datatype to the stdout buffer.
  !> @param[in] datatype Datatype to display.
  subroutine display_dtype(datatype)
    implicit none
    type(yggdtype), intent(in) :: datatype
    call display_dtype_c(datatype)
  end subroutine display_dtype

  ! Methods for sending/receiving
  !> @brief Send raw bytes data from a character array.
  !> @param[in] ygg_q Output/RPC/Timesync comm.
  !> @param[in] data Array of bytes to send.
  !> @param[in] data_len Number of bytes from data to send.
  !> @returns flag Success (.true.) or failure (.false.) of the send.
  function ygg_send(ygg_q, data, data_len) result (flag)
    implicit none
    type(yggcomm), intent(in) :: ygg_q
    character(len=*), intent(in) :: data
    character(len=len(data)+1) :: c_data
    integer, intent(in) :: data_len
    integer(kind=c_size_t) :: c_data_len
    logical :: flag
    integer(kind=c_int) :: c_flag
    c_data = data//c_null_char
    c_data_len = data_len
    c_flag = ygg_send_c(ygg_q, c_data, c_data_len)
    if (c_flag.ge.0) then
       flag = .true.
    else
       flag = .false.
    end if
  end function ygg_send
  
  !> @brief Receive raw bytes data into a character array that cannot be
  !>   resized.
  !> @param[in] ygg_q Input/RPC/Timesync comm.
  !> @param[in] data Array to receive bytes into.
  !> @param[in] data_len Variable where the size of the received message
  !>   should be stored.
  !> @returns flag Success (.true.) or failure (.false.) of the receive.
  function ygg_recv(ygg_q, data, data_len) result (flag)
    implicit none
    type(yggcomm) :: ygg_q
    character(len=*) :: data
    character(len=len(data)+1), target :: c_data
    integer :: data_len
    integer(kind=c_size_t) :: c_data_len
    logical :: flag
    integer(kind=c_int) :: c_flag
    c_data = data//c_null_char
    c_data_len = data_len
    c_flag = ygg_recv_c(ygg_q, c_loc(c_data), c_data_len)
    if (c_flag.ge.0) then
       flag = .true.
       data = c_data(:c_flag)
       data_len = c_flag
    else
       flag = .false.
    end if
  end function ygg_recv

  !> @brief Send raw bytes data from a character array.
  !> @param[in] ygg_q Output/RPC/Timesync comm.
  !> @param[in] data Array of bytes to send.
  !> @param[in] data_len Number of bytes from data to send.
  !> @returns flag Success (.true.) or failure (.false.) of the send.
  function ygg_send_nolimit(ygg_q, data, data_len) result (flag)
    implicit none
    type(yggcomm), intent(in) :: ygg_q
    type(yggchar_r) :: data
    integer, intent(in) :: data_len
    logical :: flag
    integer(kind=c_size_t) :: len_used
    len_used = data_len
    flag = ygg_send_var(ygg_q, [yggarg(data), yggarg(len_used)])
  end function ygg_send_nolimit
  
  !> @brief Receive raw bytes data into a character array that can be resized.
  !> @param[in] ygg_q Input/RPC/Timesync comm.
  !> @param[in] data Array to receive bytes into.
  !> @param[in] data_len Variable where the size of the received message
  !>   should be stored.
  !> @returns flag Success (.true.) or failure (.false.) of the receive.
  function ygg_recv_nolimit(ygg_q, data, data_len) result (flag)
    implicit none
    type(yggcomm) :: ygg_q
    type(yggchar_r) :: data
    integer :: data_len
    logical :: flag
    integer(kind=c_size_t) :: len_used
    len_used = data_len
    flag = ygg_recv_var_realloc(ygg_q, [yggarg(data), yggarg(len_used)])
    if (flag) then
       data_len = int(len_used)
    end if
  end function ygg_recv_nolimit

#ifndef DOXYGEN_SHOULD_SKIP_THIS
  function ygg_send_var_sing(ygg_q, args) result (flag)
    implicit none
    type(yggcomm), intent(in) :: ygg_q
    type(yggptr) :: args
    logical :: flag
    flag = ygg_send_var_mult(ygg_q, [args])
  end function ygg_send_var_sing
  function ygg_send_var_mult(ygg_q, args) result (flag)
    implicit none
    type(yggcomm), intent(in) :: ygg_q
    type(yggptr) :: args(:)
    type(c_ptr), allocatable, target :: c_args(:)
    integer :: c_nargs
    logical :: flag, is_format
    integer(kind=c_int) :: c_flag
    is_format = is_comm_format_array_type(ygg_q, args)
    c_nargs = pre_send(args, c_args, is_format)
    c_flag = ygg_send_var_c(ygg_q, c_nargs, c_loc(c_args(1)))
    if (c_flag.ge.0) then
       flag = .true.
    else
       flag = .false.
    end if
    call post_send(args, c_args, flag)
  end function ygg_send_var_mult

  function is_next_size_t(args, i, req_array) result(flag)
    implicit none
    type(yggptr) :: args(:)
    integer :: i
    logical, intent(in), optional :: req_array
    logical :: flag
    if (i.ge.size(args)) then
       flag = .false.
    else
       if (present(req_array)) then
          flag = is_size_t(args(i+1), req_array)
       else
          flag = is_size_t(args(i+1))
       end if
    end if
  end function is_next_size_t

  function is_size_t(arg, req_array_in) result(flag)
    type(yggptr), intent(in) :: arg
    logical, optional :: req_array_in
    logical :: req_array
    logical :: flag
    if (present(req_array_in)) then
       req_array = req_array_in
    else
       req_array = .false.
    end if
    if (((arg%type.eq."integer").or.(arg%type.eq."size_t")).and. &
         (arg%nbytes.eq.8)) then
       flag = .true.
       if (req_array.and.(.not.arg%array)) then
          flag = .false.
       else if ((.not.req_array).and.arg%array) then
          flag = .false.
       end if
    else
       flag = .false.
    end if
  end function is_size_t

  function pre_send(args, c_args, is_format) result(nargs)
    implicit none
    type(yggptr) :: args(:)
    type(c_ptr), allocatable, target :: c_args(:)
    logical :: is_format
    integer(kind=c_size_t) :: k
    integer :: i, j
    integer :: nargs
    call ygglog_debug("pre_send: begin")
    nargs = size(args)  ! Number of arguments passed
    if (is_format) then
       if (.not.is_size_t(args(1))) then
          nargs = nargs + 1
       end if
    end if
    do i = 1, size(args)
       allocate(args(i)%len_c)
       allocate(args(i)%prec_c)
       allocate(args(i)%ndim_c)
       allocate(args(i)%shape_c(args(i)%ndim))
       args(i)%len_c = 1
       args(i)%prec_c = 1
       args(i)%ndim_c = 1
       do k = 1, args(i)%ndim
          args(i)%shape_c(k) = args(i)%shape(k)
       end do
       if (args(i)%array) then
          if (args(i)%ndim.gt.1) then
             if (is_next_size_t(args, i).and. &
                  is_next_size_t(args, i+1, req_array=.true.)) then
                ! Do nothing, vars already exist
             else if (is_next_size_t(args, i, req_array=.true.)) then
                ! if (args(i)%alloc) then
                nargs = nargs + 1  ! For ndim
                ! end if
             else
                ! else if (args(i)%alloc) then
                nargs = nargs + 2  ! For ndim and shape
             end if
          else
             if ((.not.is_format).and.(.not.is_next_size_t(args, i))) then
                ! if (args(i)%alloc) then
                nargs = nargs + 1  ! For the array size
                ! end if
             end if
          end if
          if (((args(i)%type.eq."character").or. &
               (args(i)%type.eq."unicode")).and. &
               args(i)%prec.ne.1) then
             nargs = nargs + 1  ! For the string length
          end if
       else if ((args(i)%type.eq."character").or. &
            (args(i)%type.eq."unicode")) then
          if (.not.is_next_size_t(args, i)) then
             nargs = nargs + 1  ! For the string size
          end if
       end if
    end do
    call ygglog_debug("pre_send: counted variables")
    allocate(c_args(nargs))
    j = 1
    if (is_format) then
       if (.not.is_size_t(args(1))) then
          args(1)%len_c = args(1)%len
          args(1)%len_ptr = c_loc(args(1)%len_c)
          c_args(j) = args(1)%len_ptr
          j = j + 1
       end if
    end if
    do i = 1, size(args)
       c_args(j) = args(i)%ptr
       j = j + 1
       if (args(i)%array) then
          if (args(i)%ndim.gt.1) then
             if (is_next_size_t(args, i).and.is_next_size_t(args, i+1, req_array=.true.)) then
                args(i)%ndim_ptr = args(i+1)%ptr
                args(i)%shape_ptr = args(i+2)%ptr
             else if (is_next_size_t(args, i, req_array=.true.)) then
                args(i)%shape_ptr = args(i+1)%ptr
                ! if (args(i)%alloc) then
                args(i)%ndim_c = args(i)%ndim
                args(i)%ndim_ptr = c_loc(args(i)%ndim_c)
                c_args(j) = args(i)%ndim_ptr
                j = j + 1
                ! end if
             else
                ! else if (args(i)%alloc) then
                args(i)%ndim_c = args(i)%ndim
                args(i)%ndim_ptr = c_loc(args(i)%ndim_c)
                c_args(j) = args(i)%ndim_ptr
                j = j + 1
                args(i)%shape_ptr = c_loc(args(i)%shape_c(1))
                c_args(j) = args(i)%shape_ptr
                j = j + 1
             end if
          else
             if (is_format) then
                args(i)%len_ptr = c_args(1)
             else if (is_next_size_t(args, i)) then
                args(i)%len_ptr = args(i+1)%ptr
             else
                ! else if (args(i)%alloc) then
                args(i)%len_c = args(i)%len
                args(i)%len_ptr = c_loc(args(i)%len_c)
                c_args(j) = args(i)%len_ptr
                j = j + 1
             end if
          end if
          if (((args(i)%type.eq."character").or. &
               (args(i)%type.eq."unicode")).and. &
               args(i)%prec.ne.1) then
             args(i)%prec_c = args(i)%prec
             args(i)%prec_ptr = c_loc(args(i)%prec_c)
             c_args(j) = args(i)%prec_ptr
             j = j + 1
          end if
       else if ((args(i)%type.eq."character").or. &
            (args(i)%type.eq."unicode")) then
          if (is_next_size_t(args, i)) then
             args(i)%prec_ptr = args(i+1)%ptr
          else
             args(i)%prec_c = args(i)%prec
             args(i)%prec_ptr = c_loc(args(i)%prec_c)
             c_args(j) = args(i)%prec_ptr
             j = j + 1
          end if
       end if
    end do
    call ygglog_debug("pre_send: end")
  end function pre_send

  function pre_recv(args, c_args, is_format) result(nargs)
    implicit none
    type(yggptr) :: args(:)
    type(c_ptr), allocatable, target :: c_args(:)
    logical :: is_format
    integer(kind=c_size_t) :: k
    integer :: i, j
    integer :: nargs
    call ygglog_debug("pre_recv: begin")
    nargs = size(args)  ! Number of arguments passed
    if ((is_format).and.(nargs.gt.0)) then
       if (.not.is_size_t(args(1))) then
          nargs = nargs + 1
       end if
    end if
    do i = 1, size(args)
       allocate(args(i)%len_c)
       allocate(args(i)%prec_c)
       allocate(args(i)%ndim_c)
       args(i)%len_c = 1
       args(i)%prec_c = 1
       args(i)%ndim_c = 1
       if (associated(args(i)%shape)) then
          allocate(args(i)%shape_c(args(i)%ndim))
          do k = 1, args(i)%ndim
             args(i)%shape_c(k) = args(i)%shape(k)
          end do
       end if
       if (args(i)%array) then
          if ((args(i)%ndim.gt.1).or.args(i)%ndarray) then
             if (is_next_size_t(args, i).and.is_next_size_t(args, i+1, req_array=.true.)) then
                ! Do nothing, vars already exist
             else if (is_next_size_t(args, i, req_array=.true.)) then
                nargs = nargs + 1  ! For ndim
             else
                nargs = nargs + 2  ! For ndim and shape
             end if
          else
             if ((.not.is_format).and.(.not.is_next_size_t(args, i))) then
                nargs = nargs + 1  ! For the array size
             end if
          end if
          if ((args(i)%type.eq."character").or. &
               (args(i)%type.eq."unicode")) then
             nargs = nargs + 1  ! For the string length
          end if
       else if ((args(i)%type.eq."character").or. &
            (args(i)%type.eq."unicode")) then
          if (.not.is_next_size_t(args, i)) then
             nargs = nargs + 1  ! For the string size
          end if
       end if
    end do
    allocate(c_args(nargs))
    call ygglog_debug("pre_recv: counted and allocated for arguments")
    j = 1
    if (is_format) then
       if (.not.is_size_t(args(1))) then
          args(1)%len_c = args(1)%len
          args(1)%len_ptr = c_loc(args(1)%len_c)
          c_args(j) = args(1)%len_ptr
          j = j + 1
       end if
    end if
    do i = 1, size(args)
       c_args(j) = args(i)%ptr
       j = j + 1
       if (args(i)%array) then
          ! TODO: handle case where shape is explicit and ensure
          ! that length of shape variable is not appended
          if ((args(i)%ndim.gt.1).or.args(i)%ndarray) then
             if (is_next_size_t(args, i).and.is_next_size_t(args, i+1, req_array=.true.)) then
                args(i)%ndim_ptr = args(i+1)%ptr
                args(i)%shape_ptr = args(i+2)%ptr
             else if (is_next_size_t(args, i, req_array=.true.)) then
                args(i)%shape_ptr = args(i+1)%ptr
                args(i)%ndim_c = args(i)%ndim
                args(i)%ndim_ptr = c_loc(args(i)%ndim_c)
                c_args(j) = args(i)%ndim_ptr
                j = j + 1
             else
                args(i)%ndim_c = args(i)%ndim
                args(i)%ndim_ptr = c_loc(args(i)%ndim_c)
                c_args(j) = args(i)%ndim_ptr
                j = j + 1
                if (associated(args(i)%shape_c)) then
                   args(i)%shape_ptr = c_loc(args(i)%shape_c(1))
                else
                   args(i)%shape_ptr = c_null_ptr
                end if
                c_args(j) = args(i)%shape_ptr
                j = j + 1
             end if
          else
             if (is_format) then
                args(i)%len_ptr = c_args(1)
             else if (is_next_size_t(args, i)) then
                args(i)%len_ptr = args(i+1)%ptr
             else
                args(i)%len_c = args(i)%len
                args(i)%len_ptr = c_loc(args(i)%len_c)
                c_args(j) = args(i)%len_ptr
                j = j + 1
             end if
          end if
          if ((args(i)%type.eq."character").or. &
               (args(i)%type.eq."unicode")) then
             args(i)%prec_c = args(i)%prec
             args(i)%prec_ptr = c_loc(args(i)%prec_c)
             c_args(j) = args(i)%prec_ptr
             j = j + 1
          end if
       else if ((args(i)%type.eq."character").or. &
            (args(i)%type.eq."unicode")) then
          if (is_next_size_t(args, i)) then
             args(i)%prec_ptr = args(i+1)%ptr
          else
             args(i)%prec_c = args(i)%prec
             args(i)%prec_ptr = c_loc(args(i)%prec_c)
             c_args(j) = args(i)%prec_ptr
             j = j + 1
          end if
       end if
    end do
    call ygglog_debug("pre_recv: end")
  end function pre_recv

  subroutine post_recv(args, c_args, flag, realloc, is_format)
    implicit none
    type(yggptr) :: args(:)
    type(c_ptr), allocatable, target :: c_args(:)
    logical :: flag
    integer :: i, j
    logical :: realloc, is_format
    call ygglog_debug("post_recv: begin")
    if (flag) then
       j = 1
       if (is_format) then
          if (.not.is_size_t(args(1))) then
             j = j + 1
          end if
       end if
       do i = 1, size(args)
          args(i)%ptr = c_args(j)
          if ((args(i)%ndim.gt.1).or.args(i)%ndarray) then
             args(i)%shape_ptr = c_args(j+2)
          end if
          flag = yggptr_c2f(args(i), realloc)
          if (.not.flag) then
             call ygglog_error("Error recovering fortran pointer for variable.")
             exit
          end if
          j = j + 1
          if (args(i)%array) then
             if ((args(i)%ndim.gt.1).or.args(i)%ndarray) then
                if (is_next_size_t(args, i).and.is_next_size_t(args, i+1, req_array=.true.)) then
                   ! Do nothing, process variables as normal
                else if (is_next_size_t(args, i, req_array=.true.)) then
                   j = j + 1
                else
                   j = j + 2
                end if
             else
                if ((.not.is_format).and.(.not.is_next_size_t(args, i))) then
                   j = j + 1
                end if
             end if
             if ((args(i)%type.eq."character").or. &
                  (args(i)%type.eq."unicode")) then
                j = j + 1
             end if
          else if ((args(i)%type.eq."character").or. &
               (args(i)%type.eq."unicode")) then
             if (.not.is_next_size_t(args, i)) then
                j = j + 1
             end if
          end if
       end do
    end if
    if (flag) then
       do i = 1, size(args)
          deallocate(args(i)%len_c)
          deallocate(args(i)%prec_c)
          deallocate(args(i)%ndim_c)
          if (associated(args(i)%shape_c)) then
             deallocate(args(i)%shape_c)
          end if
       end do
    end if
    if (allocated(c_args)) then
       deallocate(c_args)
    end if
    call ygglog_debug("post_recv: end")
  end subroutine post_recv

  subroutine post_send(args, c_args, flag)
    implicit none
    type(yggptr) :: args(:)
    type(c_ptr), allocatable, target :: c_args(:)
    logical :: flag
    integer :: i
    call ygglog_debug("post_send: begin")
    if (flag) then
       do i = 1, size(args)
          deallocate(args(i)%len_c)
          deallocate(args(i)%prec_c)
          deallocate(args(i)%ndim_c)
          deallocate(args(i)%shape_c)
       end do
    end if
    if (allocated(c_args)) then
       deallocate(c_args)
    end if
    call ygglog_debug("post_send: end")
  end subroutine post_send
  
  function ygg_rpc_call_1v1(ygg_q, oargs, iargs) result (flag)
    implicit none
    type(yggcomm) :: ygg_q
    type(yggptr) :: oargs
    type(yggptr) :: iargs
    logical :: flag
    flag = ygg_rpc_call_mult(ygg_q, [oargs], [iargs])
  end function ygg_rpc_call_1v1
  function ygg_rpc_call_1vm(ygg_q, oargs, iargs) result (flag)
    implicit none
    type(yggcomm) :: ygg_q
    type(yggptr) :: oargs
    type(yggptr) :: iargs(:)
    logical :: flag
    flag = ygg_rpc_call_mult(ygg_q, [oargs], iargs)
  end function ygg_rpc_call_1vm
  function ygg_rpc_call_mv1(ygg_q, oargs, iargs) result (flag)
    implicit none
    type(yggcomm) :: ygg_q
    type(yggptr) :: oargs(:)
    type(yggptr) :: iargs
    logical :: flag
    flag = ygg_rpc_call_mult(ygg_q, oargs, [iargs])
  end function ygg_rpc_call_mv1
  function ygg_rpc_call_mult(ygg_q, oargs, iargs) result (flag)
    implicit none
    type(yggcomm) :: ygg_q
    type(yggptr) :: oargs(:)
    type(yggptr) :: iargs(:)
    type(c_ptr), allocatable, target :: c_args(:)
    type(c_ptr), allocatable, target :: c_iargs(:)
    type(c_ptr), allocatable, target :: c_oargs(:)
    integer :: c_nargs
    logical :: flag
    integer :: i
    integer(kind=c_int) :: c_flag
    logical :: iis_format, ois_format
    ois_format = is_comm_format_array_type(ygg_q, oargs)
    iis_format = is_comm_format_array_type(ygg_q, iargs)
    c_nargs = 0
    c_nargs = c_nargs + pre_send(oargs, c_oargs, ois_format)
    c_nargs = c_nargs + pre_recv(iargs, c_iargs, iis_format)
    allocate(c_args(size(c_oargs) + size(c_iargs)))
    do i = 1, size(c_oargs)
       c_args(i) = c_oargs(i)
    end do
    do i = 1, size(c_iargs)
       c_args(i + size(c_oargs)) = c_iargs(i)
    end do
    c_flag = ygg_rpc_call_c(ygg_q, c_nargs, c_loc(c_args(1)))
    if (c_flag.ge.0) then
       flag = .true.
    else
       flag = .false.
    end if
    do i = 1, size(c_iargs)
       c_iargs(i) = c_args(i + size(c_oargs))
    end do
    call post_send(oargs, c_oargs, flag)
    call post_recv(iargs, c_iargs, flag, .false., iis_format)
    if (allocated(c_args)) then
       deallocate(c_args)
    end if
  end function ygg_rpc_call_mult
  
  function ygg_rpc_call_realloc_1v1(ygg_q, oargs, iargs) result (flag)
    implicit none
    type(yggcomm) :: ygg_q
    type(yggptr) :: oargs
    type(yggptr) :: iargs
    logical :: flag
    flag = ygg_rpc_call_realloc_mult(ygg_q, [oargs], [iargs])
  end function ygg_rpc_call_realloc_1v1
  function ygg_rpc_call_realloc_1vm(ygg_q, oargs, iargs) result (flag)
    implicit none
    type(yggcomm) :: ygg_q
    type(yggptr) :: oargs
    type(yggptr) :: iargs(:)
    logical :: flag
    flag = ygg_rpc_call_realloc_mult(ygg_q, [oargs], iargs)
  end function ygg_rpc_call_realloc_1vm
  function ygg_rpc_call_realloc_mv1(ygg_q, oargs, iargs) result (flag)
    implicit none
    type(yggcomm) :: ygg_q
    type(yggptr) :: oargs(:)
    type(yggptr) :: iargs
    logical :: flag
    flag = ygg_rpc_call_realloc_mult(ygg_q, oargs, [iargs])
  end function ygg_rpc_call_realloc_mv1
  function ygg_rpc_call_realloc_mult(ygg_q, oargs, iargs) result (flag)
    implicit none
    type(yggcomm) :: ygg_q
    type(yggptr) :: oargs(:)
    type(yggptr) :: iargs(:)
    type(c_ptr), allocatable, target :: c_args(:)
    type(c_ptr), allocatable, target :: c_oargs(:)
    type(c_ptr), allocatable, target :: c_iargs(:)
    integer :: c_nargs
    logical :: flag, iis_format, ois_format
    integer(kind=c_int) :: c_flag
    integer :: i
    ois_format = is_comm_format_array_type(ygg_q, oargs)
    iis_format = is_comm_format_array_type(ygg_q, iargs)
    flag = .true.
    do i = 1, size(iargs)
       if ((iargs(i)%array.or.(iargs(i)%type.eq."character").or. &
            (iargs(i)%type.eq."unicode")).and. &
            (.not.(iargs(i)%alloc))) then
          call ygglog_error("Provided array/string is not allocatable.")
          flag = .false.
       end if
    end do
    if (flag) then
       c_nargs = 0
       c_nargs = c_nargs + pre_send(oargs, c_oargs, ois_format)
       c_nargs = c_nargs + pre_recv(iargs, c_iargs, iis_format)
       allocate(c_args(size(c_oargs) + size(c_iargs)))
       do i = 1, size(c_oargs)
          c_args(i) = c_oargs(i)
       end do
       do i = 1, size(c_iargs)
          c_args(i + size(c_oargs)) = c_iargs(i)
       end do
       c_flag = ygg_rpc_call_realloc_c(ygg_q, c_nargs, c_loc(c_args(1)))
       if (c_flag.ge.0) then
          flag = .true.
       else
          flag = .false.
       end if
       do i = 1, size(c_iargs)
          c_iargs(i) = c_args(i + size(c_oargs))
       end do
    end if
    call post_send(oargs, c_oargs, flag)
    call post_recv(iargs, c_iargs, flag, .true., iis_format)
    if (allocated(c_args)) then
       deallocate(c_args)
    end if
  end function ygg_rpc_call_realloc_mult
  
  function ygg_recv_var_sing(ygg_q, args) result (flag)
    implicit none
    type(yggcomm) :: ygg_q
    type(yggptr) :: args
    logical :: flag
    flag = ygg_recv_var_mult(ygg_q, [args])
  end function ygg_recv_var_sing
  function ygg_recv_var_mult(ygg_q, args) result (flag)
    implicit none
    type(yggcomm) :: ygg_q
    type(yggptr) :: args(:)
    type(c_ptr), allocatable, target :: c_args(:)
    integer :: c_nargs
    logical :: flag, is_format
    integer(kind=c_int) :: c_flag
    is_format = is_comm_format_array_type(ygg_q, args)
    c_nargs = pre_recv(args, c_args, is_format)
    c_flag = ygg_recv_var_c(ygg_q, c_nargs, c_loc(c_args(1)))
    if (c_flag.ge.0) then
       flag = .true.
    else
       flag = .false.
    end if
    call post_recv(args, c_args, flag, .false., is_format)
  end function ygg_recv_var_mult

  function ygg_recv_var_realloc_sing(ygg_q, args) result (flag)
    implicit none
    type(yggcomm) :: ygg_q
    type(yggptr) :: args
    logical :: flag
    flag = ygg_recv_var_realloc_mult(ygg_q, [args])
  end function ygg_recv_var_realloc_sing
  function ygg_recv_var_realloc_mult(ygg_q, args) result (flag)
    implicit none
    type(yggcomm) :: ygg_q
    type(yggptr), target :: args(:)
    type(c_ptr), allocatable, target :: c_args(:)
    integer :: c_nargs
    logical :: flag, is_format
    integer :: i
    integer(kind=c_int) :: c_flag
    call ygglog_debug("ygg_recv_var_realloc: begin")
    is_format = is_comm_format_array_type(ygg_q, args)
    flag = .true.
    do i = 1, size(args)
       if ((args(i)%array.or.(args(i)%type.eq."character").or. &
            (args(i)%type.eq."unicode")).and. &
            (.not.(args(i)%alloc))) then
          call ygglog_error("Provided array/string is not allocatable.")
          flag = .false.
       end if
    end do
    call ygglog_debug("ygg_recv_var_realloc: checked variables")
    if (flag) then
       c_nargs = pre_recv(args, c_args, is_format)
       c_flag = ygg_recv_var_realloc_c(ygg_q, c_nargs, c_loc(c_args(1)))
       if (c_flag.ge.0) then
          flag = .true.
       else
          flag = .false.
       end if
    end if
    call post_recv(args, c_args, flag, .true., is_format)
    call ygglog_debug("ygg_recv_var_realloc: end")
  end function ygg_recv_var_realloc_mult
#endif

  ! Ply interface
  !> @brief Set the wrapped ply mesh instance.
  !> @param[in] p The ply mesh to modify.
  !> @param[in] obj The rapidjson::Ply instance to insert.
  !> @param[in] copy If 1, the instance will be copied, otherwise a reference
  !>   will be inserted.
  subroutine set_ply(p, obj, copy)
    implicit none
    type(yggply), target :: p
    type(c_ptr), intent(in) :: obj
    integer(kind=c_int), intent(in) :: copy
    type(yggply), pointer :: pp
    type(c_ptr) :: c_p
    pp => p
    c_p = c_loc(pp)
    ! c_p = c_loc(p)
    call set_ply_c(c_p, obj, copy)
    nullify(pp)
  end subroutine set_ply
  
  ! Obj interface
  !> @brief Set the wrapped obj mesh instance.
  !> @param[in] p The obj mesh to modify.
  !> @param[in] obj The rapidjson::ObjWavefront instance to insert.
  !> @param[in] copy If 1, the instance will be copied, otherwise a reference
  !>   will be inserted.
  subroutine set_obj(p, obj, copy)
    implicit none
    type(yggobj), target :: p
    type(c_ptr), intent(in) :: obj
    integer(kind=c_int), intent(in) :: copy
    type(yggobj), pointer :: pp
    type(c_ptr) :: c_p
    pp => p
    c_p = c_loc(pp)
    ! c_p = c_loc(p)
    call set_obj_c(c_p, obj, copy)
    nullify(pp)
  end subroutine set_obj

  ! Generic interface
  
  !> @brief Get an element from an array.
  !> @param[in] arr Array to get element from.
  !> @param[in] i Index of element to get.
  !> @param[out] x Pointer to address where element should be stored.
  !> @returns A flag that is 1 if there is an error and 0 otherwise.
  ! function get_generic_array(arr, i, x) result(out)
  !   implicit none
  !   type(ygggeneric), intent(in) :: arr
  !   integer(kind=c_size_t), intent(in) :: i
  !   type(ygggeneric), pointer :: x
  !   integer(kind=c_int) :: out
  !   type(c_ptr) :: c_x
  !   allocate(x);
  !   c_x = c_loc(x) ! Maybe use first element in type
  !   out = get_generic_array_c(arr, i-1, c_x)
  ! end function get_generic_array
  !> @brief Get a reference to an element from an array.
  !> @param[in] arr Array to get element from.
  !> @param[in] i Index of element to get.
  !> @param[out] x Pointer to address where element should be stored.
  !> @returns A flag that is 1 if there is an error and 0 otherwise.
  ! function get_generic_array_ref(arr, i, x) result(out)
  !   implicit none
  !   type(ygggeneric), intent(in) :: arr
  !   integer(kind=c_size_t), intent(in) :: i
  !   type(ygggeneric), pointer :: x
  !   integer(kind=c_int) :: out
  !   type(c_ptr) :: c_x
  !   allocate(x);
  !   c_x = c_loc(x) ! Maybe use first element in type
  !   out = get_generic_array_ref_c(arr, i-1, c_x)
  ! end function get_generic_array_ref
  !> @brief Get an element from an object.
  !> @param[in] arr Object to get element from.
  !> @param[in] k Key of element to return.
  !> @param[out] x Pointer to address where element should be stored.
  !> @returns A flag that is 1 if there is an error and 0 otherwise.
  ! function get_generic_object(arr, k, x) result(out)
  !   implicit none
  !   type(ygggeneric), intent(in) :: arr
  !   character(len=*), intent(in) :: k
  !   type(ygggeneric), pointer, intent(out) :: x
  !   integer(kind=c_int) :: out
  !   character(kind=c_char), allocatable :: c_k(:)
  !   type(c_ptr) :: c_x
  !   allocate(x);
  !   c_k = convert_string_f2c(k)
  !   c_x = c_loc(x) ! Maybe use first element in type
  !   out = get_generic_object_c(arr, c_k, c_x)
  !   deallocate(c_k)
  ! end function get_generic_object
  !> @brief Get a reference to an element from an object.
  !> @param[in] arr Object to get element from.
  !> @param[in] k Key of element to return.
  !> @param[out] x Pointer to address where reference should be stored.
  !> @returns A flag that is 1 if there is an error and 0 otherwise.
  ! function get_generic_object_ref(arr, k, x) result(out)
  !   implicit none
  !   type(ygggeneric), intent(in) :: arr
  !   character(len=*), intent(in) :: k
  !   type(ygggeneric), pointer, intent(out) :: x
  !   integer(kind=c_int) :: out
  !   character(kind=c_char), allocatable :: c_k(:)
  !   type(c_ptr) :: c_x
  !   allocate(x);
  !   c_k = convert_string_f2c(k)
  !   c_x = c_loc(x) ! Maybe use first element in type
  !   out = get_generic_object_ref_c(arr, c_k, c_x)
  !   deallocate(c_k)
  ! end function get_generic_object_ref

  ! Interface for getting/setting generic map elements
  ! Get
  !> @brief Get the keys in a map object.
  !> @param[in] x Generic object that is presumed to contain a map.
  !> @param[out] keys Pointer to memory where array of keys should be stored.
  subroutine generic_map_get_keys(x, keys)
    implicit none
    type(ygggeneric) :: x
    character(len=*), dimension(:), pointer, intent(out) :: keys
    integer(kind=c_size_t), target :: n_keys
    integer(kind=c_size_t), target :: key_size
    integer(kind=c_size_t) :: i, j
    type(c_ptr) :: c_keys
    character, dimension(:), pointer :: f_keys
    c_keys = generic_map_get_keys_c(x, c_loc(n_keys), c_loc(key_size))
    if (.not.c_associated(c_keys)) then
      stop "Error getting keys from map."
    endif
    call c_f_pointer(c_keys, f_keys, [n_keys * key_size])
    allocate(keys(n_keys))
    if (key_size.gt.len(keys(1))) then
      stop "Key size is greater than size of provided keys."
    endif
    ! Allocation of character length and array dimension in gfortran
    ! has a bug which is fixed in gfortran 9.1 and the version on
    ! conda-forge as m2w64-gcc-fortran is only 5.3 as of 2020/6/18
    ! so for now the keys pointer needs to have a defined character
    ! length.
    ! allocate(character(len=key_size) :: keys(n_keys))
    do i = 1, n_keys
       do j = 1, key_size
          keys(i)(j:j) = f_keys(((i-1)*key_size)+j)
       end do
       do j = key_size+1, len(keys(1))
          keys(i)(j:j) = ' '
       end do
    end do
    deallocate(f_keys)
  end subroutine generic_map_get_keys
  ! Set
  ! subroutine dummy_function(message)
  !   implicit none
  !   integer(kind=c_int64_t), intent(in) :: message
  !   integer(kind=c_int64_t) :: c_message
  !   c_message = message
  !   write(*,*) "c_int64_t", c_int64_t
  !   ! character(len=*), intent(in) :: message
  !   ! character(kind=c_char), allocatable :: c_message(:)
  !   ! c_message = convert_string_f2c(message)
  !   write(*, *) "dummy function (fortran)", message, c_message
  !   call dummy_function_c(c_message)
  !   ! deallocate(c_message)
  ! end subroutine dummy_function
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
  function ygg_output_fmt(name, fmtString) &
       result(out)
    implicit none
    character(len = *), intent(in) :: name
    character(len = *), intent(in) :: fmtString
    type(yggcomm) :: out
    character(kind = c_char), dimension(:), allocatable :: c_name
    character(kind = c_char), dimension(:), allocatable :: c_fmtString
    c_name = convert_string_f2c(name)
    c_fmtString = convert_format_f2c(fmtString)
    out = ygg_output_fmt_c(c_name, c_fmtString)
    deallocate(c_name)
    deallocate(c_fmtString)
  end function ygg_output_fmt
  !> @brief Constructor for comm_t structure with format.
  !>   Create a comm_t structure based on a provided name that is used to
  !>   locate a particular comm address stored in the environment variable name
  !>   and a format stirng that can be used to extract arguments from received
  !>   messages.
  !> @param[in] name constant character pointer to name of queue.
  !> @param[in] fmtString character pointer to format string.
  !> @returns comm_t input queue structure.
  function ygg_input_fmt(name, fmtString) &
       result(out)
    implicit none
    character(len = *), intent(in) :: name
    character(len = *), intent(in) :: fmtString
    type(yggcomm) :: out
    character(kind = c_char), dimension(:), allocatable :: c_name
    character(kind = c_char), dimension(:), allocatable :: c_fmtString
    c_name = convert_string_f2c(name)
    c_fmtString = convert_format_f2c(fmtString)
    out = ygg_input_fmt_c(c_name, c_fmtString)
    deallocate(c_name)
    deallocate(c_fmtString)
  end function ygg_input_fmt
  !> @brief Send EOF message to the output queue.
  !> @param[in] yggQ comm_t structure that message should be sent to.
  !> @returns int 0 if send successfull, -1 if unsuccessful.
  function ygg_send_eof(yggQ) &
       result(out)
    implicit none
    type(yggcomm), value, intent(in) :: yggQ
    logical :: out
    integer(kind = c_int) :: c_out
    c_out = ygg_send_eof_c(yggQ)
    out = (c_out.ge.0)
  end function ygg_send_eof
  !> @brief Constructor for client side RPC structure.
  !>   Creates an instance of yggRpc_t with provided information.
  !> @param[in] name constant character pointer to name for queues.
  !> @param[in] outFormat character pointer to format that should be used for
  !>   formatting output.
  !> @param[in] inFormat character pointer to format that should be used for
  !>   parsing input.
  !> @return yggRpc_t structure with provided info.
  function ygg_rpc_client(name, outFormat, inFormat) &
       result(out)
    implicit none
    character(len = *), intent(in) :: name
    character(len = *), intent(in), optional :: outFormat
    character(len = *), intent(in), optional :: inFormat
    type(yggcomm) :: out
    character(kind = c_char), dimension(:), allocatable :: c_name
    character(kind = c_char), dimension(:), allocatable :: c_outFormat
    character(kind = c_char), dimension(:), allocatable :: c_inFormat
    c_name = convert_string_f2c(name)
    if (present(outFormat)) then
       c_outFormat = convert_format_f2c(outFormat)
    else
       allocate(c_outFormat(3))
       c_outFormat(1) = '%'
       c_outFormat(2) = 's'
       c_outFormat(3) = c_null_char
    end if
    if (present(inFormat)) then
       c_inFormat = convert_format_f2c(inFormat)
    else
       allocate(c_inFormat(3))
       c_inFormat(1) = '%'
       c_inFormat(2) = 's'
       c_inFormat(3) = c_null_char
    end if
    out = ygg_rpc_client_c(c_name, c_outFormat, c_inFormat)
    deallocate(c_name)
    deallocate(c_outFormat)
    deallocate(c_inFormat)
  end function ygg_rpc_client
  !> @brief Constructor for server side RPC structure.
  !>   Creates an instance of yggRpc_t with provided information.
  !> @param[in] name constant character pointer to name for queues.
  !> @param[in] inFormat character pointer to format that should be used for
  !>   parsing input.
  !> @param[in] outFormat character pointer to format that should be used for
  !>   formatting output.
  !> @return yggRpc_t structure with provided info.
  function ygg_rpc_server(name, inFormat, outFormat) &
       result(out)
    implicit none
    character(len = *), intent(in) :: name
    character(len = *), intent(in), optional :: inFormat
    character(len = *), intent(in), optional :: outFormat
    type(yggcomm) :: out
    character(kind = c_char), dimension(:), allocatable :: c_name
    character(kind = c_char), dimension(:), allocatable :: c_inFormat
    character(kind = c_char), dimension(:), allocatable :: c_outFormat
    c_name = convert_string_f2c(name)
    if (present(inFormat)) then
       c_inFormat = convert_format_f2c(inFormat)
    else
       allocate(c_inFormat(3))
       c_inFormat(1) = '%'
       c_inFormat(2) = 's'
       c_inFormat(3) = c_null_char
    end if
    if (present(outFormat)) then
       c_outFormat = convert_format_f2c(outFormat)
    else
       allocate(c_outFormat(3))
       c_outFormat(1) = '%'
       c_outFormat(2) = 's'
       c_outFormat(3) = c_null_char
    end if
    out = ygg_rpc_server_c(c_name, c_inFormat, c_outFormat)
    deallocate(c_name)
    deallocate(c_inFormat)
    deallocate(c_outFormat)
  end function ygg_rpc_server
  !> @brief Constructor for client side timestep synchronization calls.
  !>   Creates an instance of comm_t with provided information.
  !> @param[in] name constant character pointer to name for queues.
  !> @param[in] t_units const char* Units that should be used for the
  !>   timestep. "" indicates no units.
  !> @return comm_t structure with provided info.
  function ygg_timesync(name, t_units) &
       result(out)
    implicit none
    character(len = *), intent(in) :: name
    character(len = *), intent(in) :: t_units
    type(yggcomm) :: out
    character(kind = c_char), dimension(:), allocatable :: c_name
    character(kind = c_char), dimension(:), allocatable :: c_t_units
    c_name = convert_string_f2c(name)
    c_t_units = convert_string_f2c(t_units)
    out = ygg_timesync_c(c_name, c_t_units)
    deallocate(c_name)
    deallocate(c_t_units)
  end function ygg_timesync
  !> @brief Constructor for table output comm to an output channel.
  !> @param[in] name constant character pointer to output channel name.
  !> @param[in] format_str character pointer to format string that should be used
  !>   to format rows into table lines.
  !> @returns comm_t output structure.
  function ygg_ascii_table_output(name, format_str) &
       result(out)
    implicit none
    character(len = *), intent(in) :: name
    character(len = *), intent(in) :: format_str
    type(yggcomm) :: out
    character(kind = c_char), dimension(:), allocatable :: c_name
    character(kind = c_char), dimension(:), allocatable :: c_format_str
    c_name = convert_string_f2c(name)
    c_format_str = convert_format_f2c(format_str)
    out = ygg_ascii_table_output_c(c_name, c_format_str)
    deallocate(c_name)
    deallocate(c_format_str)
  end function ygg_ascii_table_output
  !> @brief Constructor for AsciiTable input comm from an input channel.
  !> @param[in] name constant character pointer to input channel name.
  !> @returns comm_t input structure.
  function ygg_ascii_table_input(name) &
       result(out)
    implicit none
    character(len = *), intent(in) :: name
    type(yggcomm) :: out
    character(kind = c_char), dimension(:), allocatable :: c_name
    c_name = convert_string_f2c(name)
    out = ygg_ascii_table_input_c(c_name)
    deallocate(c_name)
  end function ygg_ascii_table_input
  !> @brief Constructor for table output comm with array output.
  !> @param[in] name constant character pointer to an output channel name.
  !> @param[in] format_str character pointer to format string that should be
  !>   used to format rows into table lines.
  !> @returns comm_t output structure.
  function ygg_ascii_array_output(name, format_str) &
       result(out)
    implicit none
    character(len = *), intent(in) :: name
    character(len = *), intent(in) :: format_str
    type(yggcomm) :: out
    character(kind = c_char), dimension(:), allocatable :: c_name
    character(kind = c_char), dimension(:), allocatable :: c_format_str
    c_name = convert_string_f2c(name)
    c_format_str = convert_format_f2c(format_str)
    out = ygg_ascii_array_output_c(c_name, c_format_str)
    deallocate(c_name)
    deallocate(c_format_str)
  end function ygg_ascii_array_output
  !> @brief Constructor for AsciiTable input comm with array input.
  !> @param[in] name constant character pointer to an input channel name.
  !> @returns comm_t input structure.
  function ygg_ascii_array_input(name) &
       result(out)
    implicit none
    character(len = *), intent(in) :: name
    type(yggcomm) :: out
    character(kind = c_char), dimension(:), allocatable :: c_name
    c_name = convert_string_f2c(name)
    out = ygg_ascii_array_input_c(c_name)
    deallocate(c_name)
  end function ygg_ascii_array_input
  !> @brief Constructor for ply output comm to an output channel.
  !> @param[in] name constant character pointer to output channel name.
  !> @returns comm_t output structure.
  function ygg_ply_output(name) &
       result(out)
    implicit none
    character(len = *), intent(in) :: name
    type(yggcomm) :: out
    character(kind = c_char), dimension(:), allocatable :: c_name
    c_name = convert_string_f2c(name)
    out = ygg_ply_output_c(c_name)
    deallocate(c_name)
  end function ygg_ply_output
  !> @brief Constructor for ply input comm from an input channel.
  !> @param[in] name constant character pointer to input channel name.
  !> @returns comm_t input structure.
  function ygg_ply_input(name) &
       result(out)
    implicit none
    character(len = *), intent(in) :: name
    type(yggcomm) :: out
    character(kind = c_char), dimension(:), allocatable :: c_name
    c_name = convert_string_f2c(name)
    out = ygg_ply_input_c(c_name)
    deallocate(c_name)
  end function ygg_ply_input
  !> @brief Constructor for obj output comm to an output channel.
  !> @param[in] name constant character pointer to output channel name.
  !> @returns comm_t output structure.
  function ygg_obj_output(name) &
       result(out)
    implicit none
    character(len = *), intent(in) :: name
    type(yggcomm) :: out
    character(kind = c_char), dimension(:), allocatable :: c_name
    c_name = convert_string_f2c(name)
    out = ygg_obj_output_c(c_name)
    deallocate(c_name)
  end function ygg_obj_output
  !> @brief Constructor for obj input comm from an input channel.
  !> @param[in] name constant character pointer to input channel name.
  !> @returns comm_t input structure.
  function ygg_obj_input(name) &
       result(out)
    implicit none
    character(len = *), intent(in) :: name
    type(yggcomm) :: out
    character(kind = c_char), dimension(:), allocatable :: c_name
    c_name = convert_string_f2c(name)
    out = ygg_obj_input_c(c_name)
    deallocate(c_name)
  end function ygg_obj_input
  !> @brief Constructor for generic output comm to an output channel.
  !> @param[in] name constant character pointer to output channel name.
  !> @returns comm_t output structure.
  function ygg_generic_output(name) &
       result(out)
    implicit none
    character(len = *), intent(in) :: name
    type(yggcomm) :: out
    character(kind = c_char), dimension(:), allocatable :: c_name
    c_name = convert_string_f2c(name)
    out = ygg_generic_output_c(c_name)
    deallocate(c_name)
  end function ygg_generic_output
  !> @brief Constructor for generic input comm from an input channel.
  !> @param[in] name constant character pointer to input channel name.
  !> @returns comm_t input structure.
  function ygg_generic_input(name) &
       result(out)
    implicit none
    character(len = *), intent(in) :: name
    type(yggcomm) :: out
    character(kind = c_char), dimension(:), allocatable :: c_name
    c_name = convert_string_f2c(name)
    out = ygg_generic_input_c(c_name)
    deallocate(c_name)
  end function ygg_generic_input
  !> @brief Constructor for generic output comm to an output channel.
  !> @param[in] name constant character pointer to output channel name.
  !> @returns comm_t output structure.
  function ygg_any_output(name) &
       result(out)
    implicit none
    character(len = *), intent(in) :: name
    type(yggcomm) :: out
    character(kind = c_char), dimension(:), allocatable :: c_name
    c_name = convert_string_f2c(name)
    out = ygg_any_output_c(c_name)
    deallocate(c_name)
  end function ygg_any_output
  !> @brief Constructor for generic input comm from an input channel.
  !> @param[in] name constant character pointer to input channel name.
  !> @returns comm_t input structure.
  function ygg_any_input(name) &
       result(out)
    implicit none
    character(len = *), intent(in) :: name
    type(yggcomm) :: out
    character(kind = c_char), dimension(:), allocatable :: c_name
    c_name = convert_string_f2c(name)
    out = ygg_any_input_c(c_name)
    deallocate(c_name)
  end function ygg_any_input
  !> @brief Constructor for vector output comm to an output channel.
  !> @param[in] name constant character pointer to output channel name.
  !> @returns comm_t output structure.
  function ygg_json_array_output(name) &
       result(out)
    implicit none
    character(len = *), intent(in) :: name
    type(yggcomm) :: out
    character(kind = c_char), dimension(:), allocatable :: c_name
    c_name = convert_string_f2c(name)
    out = ygg_json_array_output_c(c_name)
    deallocate(c_name)
  end function ygg_json_array_output
  !> @brief Constructor for vector input comm from an input channel.
  !> @param[in] name constant character pointer to input channel name.
  !> @returns comm_t input structure.
  function ygg_json_array_input(name) &
       result(out)
    implicit none
    character(len = *), intent(in) :: name
    type(yggcomm) :: out
    character(kind = c_char), dimension(:), allocatable :: c_name
    c_name = convert_string_f2c(name)
    out = ygg_json_array_input_c(c_name)
    deallocate(c_name)
  end function ygg_json_array_input
  !> @brief Constructor for map output comm to an output channel.
  !> @param[in] name constant character pointer to output channel name.
  !> @returns comm_t output structure.
  function ygg_json_object_output(name) &
       result(out)
    implicit none
    character(len = *), intent(in) :: name
    type(yggcomm) :: out
    character(kind = c_char), dimension(:), allocatable :: c_name
    c_name = convert_string_f2c(name)
    out = ygg_json_object_output_c(c_name)
    deallocate(c_name)
  end function ygg_json_object_output
  !> @brief Constructor for map input comm from an input channel.
  !> @param[in] name constant character pointer to input channel name.
  !> @returns comm_t input structure.
  function ygg_json_object_input(name) &
       result(out)
    implicit none
    character(len = *), intent(in) :: name
    type(yggcomm) :: out
    character(kind = c_char), dimension(:), allocatable :: c_name
    c_name = convert_string_f2c(name)
    out = ygg_json_object_input_c(c_name)
    deallocate(c_name)
  end function ygg_json_object_input
  !> @brief Write a log message at the ERROR level. This will also cause
  !>   the calling model to return an error code on exit.
  !> @param[in] fmt Log message.
  !> @param[in] ... Additional arguments are formated into the log message
  !>   via sprintf.
  subroutine ygglog_error(fmt)
    implicit none
    character(len = *), intent(in) :: fmt
    character(kind = c_char), dimension(:), allocatable :: c_fmt
    c_fmt = convert_string_f2c(fmt)
    call ygglog_error_c(c_fmt)
    deallocate(c_fmt)
  end subroutine ygglog_error
  !> @brief Write a log message at the DEBUG level.
  !> @param[in] fmt Log message.
  !> @param[in] ... Additional arguments are formated into the log message
  !>   via sprintf.
  subroutine ygglog_debug(fmt)
    implicit none
    character(len = *), intent(in) :: fmt
    character(kind = c_char), dimension(:), allocatable :: c_fmt
    c_fmt = convert_string_f2c(fmt)
    call ygglog_debug_c(c_fmt)
    deallocate(c_fmt)
  end subroutine ygglog_debug
  !> @brief Write a log message at the INFO level.
  !> @param[in] fmt Log message.
  !> @param[in] ... Additional arguments are formated into the log message
  !>   via sprintf.
  subroutine ygglog_info(fmt)
    implicit none
    character(len = *), intent(in) :: fmt
    character(kind = c_char), dimension(:), allocatable :: c_fmt
    c_fmt = convert_string_f2c(fmt)
    call ygglog_info_c(c_fmt)
    deallocate(c_fmt)
  end subroutine ygglog_info
  !>   Initialize yggdrasil interface.
  function ygg_init() &
       result(out)
    implicit none
    logical :: out
    integer(kind = c_int) :: c_out
    c_out = ygg_init_c()
    out = (c_out.eq.0)
  end function ygg_init
  !>   Cleanup yggdrasil interface prior to exit.
  subroutine ygg_exit()
    implicit none
    call ygg_exit_c()
  end subroutine ygg_exit
  !> @brief Delete the underlying communicator
  !> @param comm The communicator to delete
  subroutine free_comm(comm)
    implicit none
    type(yggcomm), target :: comm
    type(yggcomm), pointer :: comm_ptr
    type(c_ptr) :: c_comm
    comm_ptr => comm
    c_comm = c_loc(comm_ptr)
    call free_comm_c(c_comm)
    nullify(comm_ptr)
  end subroutine free_comm
  !> @brief Close the underlying communicator
  !> @param comm The communicator to close
  subroutine close_comm(comm)
    implicit none
    type(yggcomm), target :: comm
    type(yggcomm), pointer :: comm_ptr
    type(c_ptr) :: c_comm
    comm_ptr => comm
    c_comm = c_loc(comm_ptr)
    call close_comm_c(c_comm)
    nullify(comm_ptr)
  end subroutine close_comm
  !>   Set a communicators datatype based on a C-style format string.
  !> @param comm Communicator
  !> @param fmt C-style format string
  !> @return 1 if successful, 0 otherwise.
  function set_response_format(comm, fmt) &
       result(out)
    implicit none
    type(yggcomm), value :: comm
    character(len = *), intent(in) :: fmt
    integer :: out
    character(kind = c_char), dimension(:), allocatable :: c_fmt
    integer(kind = c_int) :: c_out
    c_fmt = convert_string_f2c(fmt)
    c_out = set_response_format_c(comm, c_fmt)
    deallocate(c_fmt)
    out = c_out
  end function set_response_format
  !> @brief Set a communicators datatype.
  !> @param x Communicator
  !> @param datatype Pointer to datatype. The underlying data will be
  !>   consumed.
  !> @return 1 if successful, 0 otherwise.
  function set_response_datatype(x, datatype) &
       result(out)
    implicit none
    type(yggcomm), value :: x
    type(yggdtype), target :: datatype
    integer :: out
    type(yggdtype), pointer :: datatype_ptr
    type(c_ptr) :: c_datatype
    integer(kind = c_int) :: c_out
    datatype_ptr => datatype
    c_datatype = c_loc(datatype_ptr)
    c_out = set_response_datatype_c(x, c_datatype)
    nullify(datatype_ptr)
    out = c_out
  end function set_response_datatype
  !> @brief Wait for a message to become available to receive.
  !> @param x Communicator
  !> @param tout Time (in micro-seconds) that should be waited. If -1
  !>   the process will wait forever.
  !> @return Number of messages available for receive. -1 if an error
  !>   occurred.
  function comm_wait_for_recv(x, tout) &
       result(out)
    implicit none
    type(yggcomm), value, intent(in) :: x
    integer(kind=int64), value, intent(in) :: tout
    integer :: out
    integer(kind=c_int64_t) :: c_tout
    integer(kind = c_int) :: c_out
    c_tout = tout
    c_out = comm_wait_for_recv_c(x, c_tout)
    out = c_out
  end function comm_wait_for_recv
  !> @brief Get the datatype associated with a communicator.
  !> @param x Communicator
  !> @return The datatype
  function comm_get_datatype(x) &
       result(out)
    implicit none
    type(yggcomm), value :: x
    type(yggdtype) :: out
    out = comm_get_datatype_c(x)
  end function comm_get_datatype
  !> @brief Set the datatype associated with a communicator.
  !> @param x Communicator
  !> @param datatype The datatype
  !> @return 1 if successful, 0 otherwise.
  function comm_set_datatype(x, datatype) &
       result(out)
    implicit none
    type(yggcomm), value :: x
    type(yggdtype), target :: datatype
    integer :: out
    type(yggdtype), pointer :: datatype_ptr
    type(c_ptr) :: c_datatype
    integer(kind = c_int) :: c_out
    datatype_ptr => datatype
    c_datatype = c_loc(datatype_ptr)
    c_out = comm_set_datatype_c(x, c_datatype)
    nullify(datatype_ptr)
    out = c_out
  end function comm_set_datatype
  !> @brief Send a message with the given communicator
  !> @param comm The communicator to use
  !> @param data The message to send
  !> @param len The size of data in bytes
  !> @return Any value greater than 0 indicates success
  function comm_send(comm, data, len) &
       result(out)
    implicit none
    type(yggcomm), value :: comm
    character(len = *), intent(in) :: data
    integer, value, intent(in) :: len
    integer :: out
    character(kind = c_char), dimension(:), allocatable :: c_data
    integer(kind = c_size_t) :: c_len
    integer(kind = c_int) :: c_out
    c_data = convert_string_f2c(data)
    c_len = len
    c_out = comm_send_c(comm, c_data, c_len)
    deallocate(c_data)
    out = c_out
  end function comm_send
  !> @brief Send an end-of-file notification on the given communicator
  !> @param comm The communicator to use
  !> @return Any value greater than 0 indicates success
  function comm_send_eof(comm) &
       result(out)
    implicit none
    type(yggcomm), value :: comm
    integer :: out
    integer(kind = c_int) :: c_out
    c_out = comm_send_eof_c(comm)
    out = c_out
  end function comm_send_eof
  !> @brief The number of messages in the communicators queue
  !> @param comm The communicator to query
  !> @return The number of messages in the queue
  function comm_nmsg(comm) &
       result(out)
    implicit none
    type(yggcomm), value :: comm
    integer :: out
    integer(kind = c_int) :: c_out
    c_out = comm_nmsg_c(comm)
    out = c_out
  end function comm_nmsg
  !> @brief Send a message from a list of pointers
  !> @param comm The communicator to use
  !> @param nargs The number of arguments
  !> @param ptrs Pointer array of pointers of the data to send
  !> @param for_fortran If set to true then the list is of explicit fortran pointers
  !> @return Any value greater than 0 indicates success
  function pcomm_send(comm, nargs, ptrs, for_fortran) &
       result(out)
    implicit none
    type(yggcomm), value, intent(in) :: comm
    integer, value, intent(in) :: nargs
    type(c_ptr), value :: ptrs
    integer, value, intent(in) :: for_fortran
    integer :: out
    integer(kind = c_size_t) :: c_nargs
    integer(kind = c_int) :: c_for_fortran
    integer(kind = c_int) :: c_out
    c_nargs = nargs
    c_for_fortran = for_fortran
    c_out = pcomm_send_c(comm, c_nargs, ptrs, c_for_fortran)
    out = c_out
  end function pcomm_send
  !> @brief Receive a messag into a list of pointers
  !> @param comm The communciator to use
  !> @param allow_realloc If true then the number of pointers may change based on the message contents
  !> @param nargs The number of pointers
  !> @param ptrs Pointer array of pointers to hold the message
  !> @param for_fortran If true then the list is of explicit fortran pointers
  !> @return Any value greater than 0 indicates success
  function pcomm_recv(comm, allow_realloc, nargs, ptrs, for_fortran) &
       result(out)
    implicit none
    type(yggcomm), value :: comm
    integer, value, intent(in) :: allow_realloc
    integer, value, intent(in) :: nargs
    type(c_ptr), value :: ptrs
    integer, value, intent(in) :: for_fortran
    logical :: out
    integer(kind = c_int) :: c_allow_realloc
    integer(kind = c_size_t) :: c_nargs
    integer(kind = c_int) :: c_for_fortran
    integer(kind = c_long) :: c_out
    c_allow_realloc = allow_realloc
    c_nargs = nargs
    c_for_fortran = for_fortran
    c_out = pcomm_recv_c(comm, c_allow_realloc, c_nargs, ptrs, c_for_fortran)
    out = (c_out.ge.0)
  end function pcomm_recv
  !> @brief Send a request and receive a response from a list of pointers containing data for both the request and response.
  !> @param comm The communciator to use
  !> @param allow_realloc If true then the number of pointers may change based on the message contents
  !> @param nargs The number of pointers
  !> @param ptrs Pointer array of pointers to hold the message
  !> @param for_fortran If true then the list is of explicit fortran pointers
  !> @return Any value greater than 0 indicates success
  function pcomm_call(comm, allow_realloc, nargs, ptrs, for_fortran) &
       result(out)
    implicit none
    type(yggcomm), value :: comm
    integer, value, intent(in) :: allow_realloc
    integer, value, intent(in) :: nargs
    type(c_ptr), value :: ptrs
    integer, value, intent(in) :: for_fortran
    logical :: out
    integer(kind = c_int) :: c_allow_realloc
    integer(kind = c_size_t) :: c_nargs
    integer(kind = c_int) :: c_for_fortran
    integer(kind = c_long) :: c_out
    c_allow_realloc = allow_realloc
    c_nargs = nargs
    c_for_fortran = for_fortran
    c_out = pcomm_call_c(comm, c_allow_realloc, c_nargs, ptrs, c_for_fortran)
    out = (c_out.ge.0)
  end function pcomm_call
  subroutine set_global_comm()
    implicit none
    call set_global_comm_c()
  end subroutine set_global_comm
  subroutine unset_global_comm()
    implicit none
    call unset_global_comm_c()
  end subroutine unset_global_comm
  !> @brief Get the length of a C string stored in a pointer.
  !> @param[in] x String pointer.
  !> @returns Length of the string.
  function pointer_strlen(x) &
       result(out)
    implicit none
    type(yggnull), value, intent(in) :: x
    integer :: out
    type(c_ptr) :: c_x
    integer(kind = c_size_t) :: c_out
    c_x = x%ptr
    c_out = pointer_strlen_c(c_x)
    out = c_out
  end function pointer_strlen
  !> @brief Initialize an empty generic object.
  !> @returns generic_t New generic object structure.
  function init_generic() &
       result(out)
    implicit none
    type(ygggeneric) :: out
    out = init_generic_c()
  end function init_generic
  !> @brief Initialize an empty generic reference object.
  !> @param[in] parent Generic object that will be the parent of the
  !>   returned reference object.
  !> @returns New generic reference object structure.
  function init_generic_ref(parent) &
       result(out)
    implicit none
    type(ygggeneric), value :: parent
    type(ygggenericref) :: out
    out = init_generic_ref_c(parent)
  end function init_generic_ref
  !> @brief Initialize an empty generic object with a null JSON document
  !> @returns generic_t New generic object structure.
  function init_generic_null() &
       result(out)
    implicit none
    type(ygggeneric) :: out
    out = init_generic_null_c()
  end function init_generic_null
  !> @brief Initialize an empty array of mixed types with generic wrappers.
  !> @returns generic_t New generic object structure containing an empty
  !>   array.
  function init_generic_array() &
       result(out)
    implicit none
    type(ygggeneric) :: out
    out = init_generic_array_c()
  end function init_generic_array
  !> @brief Initialize an empty map (JSON object) of mixed types with
  !>   generic wrappers.
  !> @returns New generic object structure contaiing an empty map (JSON
  !>   object).
  function init_generic_map() &
       result(out)
    implicit none
    type(ygggeneric) :: out
    out = init_generic_map_c()
  end function init_generic_map
  !> @brief Initialize a generic object from a JSON string.
  !> @param[in] json JSON encoded string.
  !> @returns New generic object structure wrapping a rapidjson::Document
  !>   instance.
  function init_generic_json(json) &
       result(out)
    implicit none
    character(len = *), intent(in) :: json
    type(ygggeneric) :: out
    character(kind = c_char), dimension(:), allocatable :: c_json
    c_json = convert_string_f2c(json)
    out = init_generic_json_c(c_json)
    deallocate(c_json)
  end function init_generic_json
  !> @brief Initialize a generic object from a JSON string.
  !> @param[in] schema JSON encoded schema describing object to generate.
  !> @returns New generic object structure wrapping a rapidjson::Document
  !>   instance.
  function init_generic_generate(schema) &
       result(out)
    implicit none
    character(len = *), intent(in) :: schema
    type(ygggeneric) :: out
    character(kind = c_char), dimension(:), allocatable :: c_schema
    c_schema = convert_string_f2c(schema)
    out = init_generic_generate_c(c_schema)
    deallocate(c_schema)
  end function init_generic_generate
  !> @brief Determine if a generic structure is initialized.
  !> @param[in] x Generic structure to test.
  !> @returns 1 if the structure is initialized, 0 otherwise.
  function is_generic_init(x) &
       result(out)
    implicit none
    type(ygggeneric), value, intent(in) :: x
    logical :: out
    integer(kind = c_int) :: c_out
    c_out = is_generic_init_c(x)
    out = (c_out.ne.0)
  end function is_generic_init
  !> @brief Determine if a generic reference structure is initialized.
  !> @param[in] x Generic reference structure to test.
  !> @returns 1 if the structure is initialized, 0 otherwise.
  function is_generic_ref_init(x) &
       result(out)
    implicit none
    type(ygggenericref), value, intent(in) :: x
    integer :: out
    integer(kind = c_int) :: c_out
    c_out = is_generic_ref_init_c(x)
    out = c_out
  end function is_generic_ref_init
  !> @brief Destroy a generic object.
  !> @param[in] x generic_t* Pointer to generic object structure to destory.
  !> @returns int -1 if unsuccessful, 0 otherwise.
  subroutine free_generic(x)
    implicit none
    type(ygggeneric), target :: x
    type(ygggeneric), pointer :: x_ptr
    type(c_ptr) :: c_x
    integer(kind = c_int) :: c_out
    x_ptr => x
    c_x = c_loc(x_ptr)
    c_out = free_generic_c(c_x)
    nullify(x_ptr)
    if (c_out.ne.0) then
       stop "free_generic: Error freeing generic object."
    end if
  end subroutine free_generic
  !> @brief Copy data from one generic object into another.
  !> @param[in,out] dst Pointer to destination object.
  !> @param[in] src Source object.
  !> @returns int -1 if unsuccessful, 0 otherwise.
  subroutine copy_generic_into(dst, src)
    implicit none
    type(ygggeneric), target :: dst
    type(ygggeneric), value, intent(in) :: src
    type(ygggeneric), pointer :: dst_ptr
    type(c_ptr) :: c_dst
    integer(kind = c_int) :: c_out
    dst_ptr => dst
    c_dst = c_loc(dst_ptr)
    c_out = copy_generic_into_c(c_dst, src)
    nullify(dst_ptr)
    if (c_out.ne.0) then
       stop "copy_generic_into: Error copying generic object."
    end if
  end subroutine copy_generic_into
  !> @brief Copy data from one generic object to the other.
  !> @param[in] src generic_t Generic structure that data should be copied from.
  !> @returns generic_t Copied structure.
  function copy_generic(src) &
       result(out)
    implicit none
    type(ygggeneric), value, intent(in) :: src
    type(ygggeneric) :: out
    out = copy_generic_c(src)
  end function copy_generic
  !> @brief Compare two generic objects.
  !> @param[in] a First object for comparison.
  !> @param[in] b Second object for comparison.
  !> @returns true if the two objects are equivalent, false otherwise.
  function compare_generic(a, b) &
       result(out)
    implicit none
    type(ygggeneric), value, intent(in) :: a
    type(ygggeneric), value, intent(in) :: b
    logical :: out
    logical(kind = c_bool) :: c_out
    c_out = compare_generic_c(a, b)
    out = c_out
  end function compare_generic
  !> @brief Display information about the generic type.
  !> @param[in] x generic_t* Wrapper for generic object.
  subroutine display_generic(x)
    implicit none
    type(ygggeneric), value, intent(in) :: x
    call display_generic_c(x)
  end subroutine display_generic
  !> @brief Add an element to the end of an array of generic elements.
  !> @param[in] arr generic_t Array to add element to.
  !> @param[in] x generic_t Element to add.
  !> @returns int Flag that is 1 if there is an error and 0 otherwise.
  function add_generic_array(arr, x) &
       result(out)
    implicit none
    type(ygggeneric), value :: arr
    type(ygggeneric), value, intent(in) :: x
    integer :: out
    integer(kind = c_int) :: c_out
    c_out = add_generic_array_c(arr, x)
    out = c_out
  end function add_generic_array
  !> @brief Set an element in the array at a given index to a new value.
  !> @param[in] arr generic_t Array to add element to.
  !> @param[in] i size_t Index where element should be added.
  !> @param[in] x generic_t Element to add.
  !> @returns int Flag that is 1 if there is an error and 0 otherwise.
  function set_generic_array(arr, i, x) &
       result(out)
    implicit none
    type(ygggeneric), value :: arr
    integer, value, intent(in) :: i
    type(ygggeneric), value, intent(in) :: x
    integer :: out
    integer(kind = c_size_t) :: c_i
    integer(kind = c_int) :: c_out
    c_i = i - 1
    c_out = set_generic_array_c(arr, c_i, x)
    out = c_out
  end function set_generic_array
  !> @brief Get an element from an array.
  !> @param[in] arr generic_t Array to get element from.
  !> @param[in] i size_t Index of element to get.
  !> @param[out] x generic_t* Pointer to address where element should be
  !>   stored.
  !> @returns int Flag that is 1 if there is an error and 0 otherwise.
  function get_generic_array(arr, i, x) &
       result(out)
    implicit none
    type(ygggeneric), value, intent(in) :: arr
    integer, value, intent(in) :: i
    type(ygggeneric), target :: x
    integer :: out
    type(ygggeneric), pointer :: x_ptr
    integer(kind = c_size_t) :: c_i
    type(c_ptr) :: c_x
    integer(kind = c_int) :: c_out
    c_i = i - 1
    x_ptr => x
    c_x = c_loc(x_ptr)
    c_out = get_generic_array_c(arr, c_i, c_x)
    nullify(x_ptr)
    out = c_out
  end function get_generic_array
  function get_generic_array_ref(arr, i, x) &
       result(out)
    implicit none
    type(ygggeneric), value, intent(in) :: arr
    integer, value, intent(in) :: i
    type(ygggenericref), target :: x
    integer :: out
    type(ygggenericref), pointer :: x_ptr
    integer(kind = c_size_t) :: c_i
    type(c_ptr) :: c_x
    integer(kind = c_int) :: c_out
    c_i = i - 1
    x_ptr => x
    c_x = c_loc(x_ptr)
    c_out = get_generic_array_ref_c(arr, c_i, c_x)
    nullify(x_ptr)
    out = c_out
  end function get_generic_array_ref
  !> @brief Set an element in the object at for a given key to a new value.
  !> @param[in] arr generic_t Object to add element to.
  !> @param[in] k const char* Key where element should be added.
  !> @param[in] x generic_t Element to add.
  !> @returns int Flag that is 1 if there is an error and 0 otherwise.
  function set_generic_object(arr, k, x) &
       result(out)
    implicit none
    type(ygggeneric), value :: arr
    character(len = *), intent(in) :: k
    type(ygggeneric), value, intent(in) :: x
    integer :: out
    character(kind = c_char), dimension(:), allocatable :: c_k
    integer(kind = c_int) :: c_out
    c_k = convert_string_f2c(k)
    c_out = set_generic_object_c(arr, c_k, x)
    deallocate(c_k)
    out = c_out
  end function set_generic_object
  !> @brief Get an element from an object.
  !> @param[in] arr generic_t Object to get element from.
  !> @param[in] k const char* Key of element to return.
  !> @param[out] x generic_t* Pointer to address where element should be
  !>   stored.
  !> @returns int Flag that is 1 if there is an error and 0 otherwise.
  function get_generic_object(arr, k, x) &
       result(out)
    implicit none
    type(ygggeneric), value, intent(in) :: arr
    character(len = *), intent(in) :: k
    type(ygggeneric), target :: x
    integer :: out
    type(ygggeneric), pointer :: x_ptr
    character(kind = c_char), dimension(:), allocatable :: c_k
    type(c_ptr) :: c_x
    integer(kind = c_int) :: c_out
    c_k = convert_string_f2c(k)
    x_ptr => x
    c_x = c_loc(x_ptr)
    c_out = get_generic_object_c(arr, c_k, c_x)
    deallocate(c_k)
    nullify(x_ptr)
    out = c_out
  end function get_generic_object
  function get_generic_object_ref(arr, k, x) &
       result(out)
    implicit none
    type(ygggeneric), value, intent(in) :: arr
    character(len = *), intent(in) :: k
    type(ygggenericref), target :: x
    integer :: out
    type(ygggenericref), pointer :: x_ptr
    character(kind = c_char), dimension(:), allocatable :: c_k
    type(c_ptr) :: c_x
    integer(kind = c_int) :: c_out
    c_k = convert_string_f2c(k)
    x_ptr => x
    c_x = c_loc(x_ptr)
    c_out = get_generic_object_ref_c(arr, c_k, c_x)
    deallocate(c_k)
    nullify(x_ptr)
    out = c_out
  end function get_generic_object_ref
  !> @brief Determine if a map object has a certain key.
  !> @param[in] x generic_t Generic object that is presumed to contain a map.
  !> @param[in] key char* Key to check for.
  !> @returns int 1 if the key is present, 0 otherwise.
  function generic_map_has_key(x, key) &
       result(out)
    implicit none
    type(ygggeneric), value, intent(in) :: x
    character(len = *), intent(in) :: key
    integer :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_out = generic_map_has_key_c(x, c_key)
    deallocate(c_key)
    out = c_out
  end function generic_map_has_key
  !> @brief Set the data in the given item to the value given by the json character string
  !> @param[in, out] x The item to set
  !> @param[in] json The value to set x to
  !> @return 0 on success, 1 on error
  function generic_set_json(x, json) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: json
    integer :: out
    character(kind = c_char), dimension(:), allocatable :: c_json
    integer(kind = c_int) :: c_out
    c_json = convert_string_f2c(json)
    c_out = generic_set_json_c(x, c_json)
    deallocate(c_json)
    out = c_out
  end function generic_set_json
  !> @brief Initialize Python if it is not initialized.
  !> @returns int 0 if successful, other values indicate errors.
  subroutine init_python_api()
    implicit none
    integer(kind = c_int) :: c_out
    c_out = init_python_api_c()
    if (c_out.lt.0) then
       stop "init_python_api: Error initializing Python."
    end if
  end subroutine init_python_api
  !> @brief Initialize a Python wrapper object.
  !> @returns Initialized object.
  function init_python() &
       result(out)
    implicit none
    type(yggpython) :: out
    out = init_python_c()
  end function init_python
  !> @brief Destroy a structure containing a Python object.
  !> @param[in] x Pointer to Python object structure that should be freed.
  subroutine free_python(x)
    implicit none
    type(yggpython), target :: x
    type(yggpython), pointer :: x_ptr
    type(c_ptr) :: c_x
    x_ptr => x
    c_x = c_loc(x_ptr)
    call free_python_c(c_x)
    nullify(x_ptr)
  end subroutine free_python
  !> @brief Copy a Python object structure (NOTE: this dosn't copy the
  !>   underlying Python object but does increment the reference count).
  !> @param[in] x Structure containing Python object to copy.
  !> @returns python_t Copy of x.
  function copy_python(x) &
       result(out)
    implicit none
    type(yggpython), value :: x
    type(yggpython) :: out
    out = copy_python_c(x)
  end function copy_python
  !> @brief Display a Python object structure.
  !> @param[in] x Structure containing Python object to display.
  subroutine display_python(x)
    implicit none
    type(yggpython), value :: x
    call display_python_c(x)
  end subroutine display_python
  !> @brief Determine if a datatype is empty.
  !> @param[in] dtype structure to test.
  !> @returns int 1 if dtype is empty, 0 otherwise.
  function is_empty_dtype(dtype) &
       result(out)
    implicit none
    type(yggdtype), value, intent(in) :: dtype
    logical :: out
    integer(kind = c_int) :: c_out
    c_out = is_empty_dtype_c(dtype)
    out = (c_out.eq.1)
  end function is_empty_dtype
  !> @brief Determine if a datatype was created from a format.
  !> @param[in] type_struct Datatype structure.
  !> @returns 1 if the datatype was created from a format, 0 if it was not,
  !>   -1 if there is an error.
  function is_dtype_format_array(type_struct) &
       result(out)
    implicit none
    type(yggdtype), value, intent(in) :: type_struct
    logical :: out
    integer(kind = c_int) :: c_out
    c_out = is_dtype_format_array_c(type_struct)
    out = (c_out.eq.1)
    if ((c_out.ne.0).and.(c_out.ne.1)) then
       stop "is_dtype_format_array: Error checking data type"
    end if
  end function is_dtype_format_array
  !> @brief Get the name of the type from the class.
  !> @param[in] type_class Type structure..
  !> @returns Type name.
  function dtype_name(type_class) &
       result(out)
    implicit none
    type(yggdtype), value, intent(in) :: type_class
    character(len=:), allocatable :: out
    type(c_ptr) :: c_out
    c_out = dtype_name_c(type_class)
    out = convert_string_c2f(c_out)
  end function dtype_name
  !> @brief Get the subtype of the type.
  !> @param[in] type_class Type structure..
  !> @returns The subtype of the class, "" if there is an error.
  function dtype_subtype(type_class) &
       result(out)
    implicit none
    type(yggdtype), value, intent(in) :: type_class
    character(len=:), allocatable :: out
    type(c_ptr) :: c_out
    c_out = dtype_subtype_c(type_class)
    out = convert_string_c2f(c_out)
  end function dtype_subtype
  !> @brief Compare two datatypes structures.
  !> @param[in] a First datatype for comparison
  !> @param[in] b Second datatype for comparison
  function compare_dtype(a, b) &
       result(out)
    implicit none
    type(yggdtype), value, intent(in) :: a
    type(yggdtype), value, intent(in) :: b
    logical :: out
    logical(kind = c_bool) :: c_out
    c_out = compare_dtype_c(a, b)
    out = c_out
  end function compare_dtype
  !> @brief Get the precision of the type.
  !> @param[in] type_class Type structure..
  !> @returns The precision of the class, 0 if there is an error.
  function dtype_precision(type_class) &
       result(out)
    implicit none
    type(yggdtype), value, intent(in) :: type_class
    integer :: out
    integer(kind = c_size_t) :: c_out
    c_out = dtype_precision_c(type_class)
    out = c_out
  end function dtype_precision
  !> @brief Set the type name in the datatype structure.
  !> @param[in,out] dtype Datatype structure to update. It must have
  !>   been initialized.
  !> @param[in] name Type name to set in dtype.
  !> @returns 0 on success, -1 if there is an error.
  function set_dtype_name(dtype, name) &
       result(out)
    implicit none
    type(yggdtype), value :: dtype
    character(len = *), intent(in) :: name
    integer :: out
    character(kind = c_char), dimension(:), allocatable :: c_name
    integer(kind = c_int) :: c_out
    c_name = convert_string_f2c(name)
    c_out = set_dtype_name_c(dtype, c_name)
    deallocate(c_name)
    out = c_out
  end function set_dtype_name
  !> @brief Initialize a datatype structure including setting the type string.
  !> @param[in] dtype Type structure.
  !> @param[in] use_generic If true, serialized or deserialized
  !>   objects will be expected to be generic_t instances.
  !> @returns Initialized type structure.
  function complete_dtype(dtype, use_generic) &
       result(out)
    implicit none
    type(yggdtype), value :: dtype
    logical, value, intent(in) :: use_generic
    type(yggdtype) :: out
    logical(kind = c_bool) :: c_use_generic
    c_use_generic = use_generic
    out = complete_dtype_c(dtype, c_use_generic)
  end function complete_dtype
  !> @brief Construct a type object from a JSON schema.
  !> @param[in] schema Serialized JSON schema.
  !> @param[in] use_generic If true, serialized or deserialized objects will
  !>   be expected to be generic_t instances.
  !> @returns Type structure.
  function create_dtype_from_schema(schema, use_generic) &
       result(out)
    implicit none
    character(len = *), intent(in) :: schema
    logical, value, intent(in) :: use_generic
    type(yggdtype) :: out
    character(kind = c_char), dimension(:), allocatable :: c_schema
    logical(kind = c_bool) :: c_use_generic
    c_schema = convert_string_f2c(schema)
    c_use_generic = use_generic
    out = create_dtype_from_schema_c(c_schema, c_use_generic)
    deallocate(c_schema)
  end function create_dtype_from_schema
  !> @brief Construct and empty type object.
  !> @param[in] use_generic If true, serialized or deserialized
  !>   objects will be expected to be generic_t instances.
  !> @returns Type structure.
  function create_dtype_empty(use_generic) &
       result(out)
    implicit none
    logical, value, intent(in) :: use_generic
    type(yggdtype) :: out
    logical(kind = c_bool) :: c_use_generic
    c_use_generic = use_generic
    out = create_dtype_empty_c(c_use_generic)
  end function create_dtype_empty
  !> @brief Create a datatype based on a Python dictionary.
  !> @param[in] pyobj Python dictionary.
  !> @param[in] use_generic If true, serialized or deserialized
  !>   objects will be expected to be generic_t instances.
  !> @returns Type structure.
  function create_dtype_python(pyobj, use_generic) &
       result(out)
    implicit none
    type(c_ptr), value :: pyobj
    logical, value, intent(in) :: use_generic
    type(yggdtype) :: out
    logical(kind = c_bool) :: c_use_generic
    c_use_generic = use_generic
    out = create_dtype_python_c(pyobj, c_use_generic)
  end function create_dtype_python
  !> @brief Construct a Direct type object.
  !> @param[in] use_generic If true, serialized or deserialized
  !>   objects will be expected to be generic_t instances.
  !> @returns Type structure.
  function create_dtype_direct(use_generic) &
       result(out)
    implicit none
    logical, value, intent(in) :: use_generic
    type(yggdtype) :: out
    logical(kind = c_bool) :: c_use_generic
    c_use_generic = use_generic
    out = create_dtype_direct_c(c_use_generic)
  end function create_dtype_direct
  !> @brief Construct a type object for one of the default JSON types.
  !> @param[in] type Name of the type.
  !> @param[in] use_generic If true, serialized or deserialized
  !>   objects will be expected to be generic_t instances.
  !> @returns Type structure.
  function create_dtype_default(type, use_generic) &
       result(out)
    implicit none
    character(len = *), intent(in) :: type
    logical, value, intent(in) :: use_generic
    type(yggdtype) :: out
    character(kind = c_char), dimension(:), allocatable :: c_type
    logical(kind = c_bool) :: c_use_generic
    c_type = convert_string_f2c(type)
    c_use_generic = use_generic
    out = create_dtype_default_c(c_type, c_use_generic)
    deallocate(c_type)
  end function create_dtype_default
  !> @brief Construct a Scalar type object.
  !> @param[in] subtype Name of the scalar subtype (e.g. int, uint, float,
  !>   bytes).
  !> @param[in] precision Precision of the scalar in bits.
  !> @param[in] units Units for scalar. (e.g. "cm", "g", "" for unitless)
  !> @param[in] use_generic If true, serialized or deserialized
  !>   objects will be expected to be generic_t instances.
  !> @returns Type structure.
  function create_dtype_scalar(subtype, precision, units, use_generic) &
       result(out)
    implicit none
    character(len = *), intent(in) :: subtype
    integer, value, intent(in) :: precision
    character(len = *), intent(in) :: units
    logical, value, intent(in) :: use_generic
    type(yggdtype) :: out
    character(kind = c_char), dimension(:), allocatable :: c_subtype
    integer(kind = c_size_t) :: c_precision
    character(kind = c_char), dimension(:), allocatable :: c_units
    logical(kind = c_bool) :: c_use_generic
    c_subtype = convert_string_f2c(subtype)
    c_precision = precision
    c_units = convert_string_f2c(units)
    c_use_generic = use_generic
    out = create_dtype_scalar_c(c_subtype, c_precision, c_units, c_use_generic)
    deallocate(c_subtype)
    deallocate(c_units)
  end function create_dtype_scalar
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
  function create_dtype_1darray(subtype, precision, length, units, use_generic) &
       result(out)
    implicit none
    character(len = *), intent(in) :: subtype
    integer, value, intent(in) :: precision
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    logical, value, intent(in) :: use_generic
    type(yggdtype) :: out
    character(kind = c_char), dimension(:), allocatable :: c_subtype
    integer(kind = c_size_t) :: c_precision
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    logical(kind = c_bool) :: c_use_generic
    c_subtype = convert_string_f2c(subtype)
    c_precision = precision
    c_length = length
    c_units = convert_string_f2c(units)
    c_use_generic = use_generic
    out = create_dtype_1darray_c(c_subtype, c_precision, c_length, c_units, c_use_generic)
    deallocate(c_subtype)
    deallocate(c_units)
  end function create_dtype_1darray
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
  function create_dtype_ndarray(subtype, precision, ndim, shape, units, use_generic) &
       result(out)
    implicit none
    character(len = *), intent(in) :: subtype
    integer, value, intent(in) :: precision
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    logical, value, intent(in) :: use_generic
    type(yggdtype) :: out
    character(kind = c_char), dimension(:), allocatable :: c_subtype
    integer(kind = c_size_t) :: c_precision
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    logical(kind = c_bool) :: c_use_generic
    c_subtype = convert_string_f2c(subtype)
    c_precision = precision
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_use_generic = use_generic
    out = create_dtype_ndarray_c(c_subtype, c_precision, c_ndim, c_shape, c_units, c_use_generic)
    deallocate(c_subtype)
    deallocate(c_units)
  end function create_dtype_ndarray
  !> @brief Construct a JSON array type object.
  !> @param[in] nitems Number of types in items.
  !> @param[in] items Pointer to array of types describing the array
  !>   elements.
  !> @param[in] use_generic If true, serialized or deserialized
  !>   objects will be expected to be generic_t instances.
  !> @returns Type structure.
  function create_dtype_json_array(nitems, items, use_generic) &
       result(out)
    implicit none
    integer, value, intent(in) :: nitems
    type(yggdtype), target, dimension(:), intent(in) :: items
    logical, value, intent(in) :: use_generic
    type(yggdtype) :: out
    type(yggdtype), dimension(:), pointer :: items_ptr
    integer(kind = c_size_t) :: c_nitems
    type(c_ptr) :: c_items
    logical(kind = c_bool) :: c_use_generic
    c_nitems = nitems
    items_ptr => items
    c_items = c_loc(items_ptr)
    c_use_generic = use_generic
    out = create_dtype_json_array_c(c_nitems, c_items, c_use_generic)
    nullify(items_ptr)
  end function create_dtype_json_array
  !> @brief Construct a JSON object type object.
  !> @param[in] nitems Number of items in keys and values.
  !> @param[in] keys Pointer to array of keys for each type.
  !> @param[in] values Pointer to array of types describing the values
  !>   for each key.
  !> @param[in] use_generic If true, serialized or deserialized
  !>   objects will be expected to be generic_t instances.
  !> @returns Type structure.
  function create_dtype_json_object(nitems, keys, values, use_generic) &
       result(out)
    implicit none
    integer, value, intent(in) :: nitems
    character(len=*), dimension(:), intent(in), target :: keys
    type(yggdtype), target, dimension(:), intent(in) :: values
    logical, value, intent(in) :: use_generic
    type(yggdtype) :: out
    character(kind=c_char, len=len(keys(1))), pointer :: keys_iele
    integer :: i, keys_ilen
    type(c_ptr), target :: c_keys_int(size(keys))
    type(yggdtype), dimension(:), pointer :: values_ptr
    integer(kind = c_size_t) :: c_nitems
    type(c_ptr) :: c_keys
    type(c_ptr) :: c_values
    logical(kind = c_bool) :: c_use_generic
    c_nitems = nitems
    do i = 1, size(keys)
       keys_iele => keys(i)
       keys_ilen = len_trim(keys_iele)
       if (keys_ilen.lt.len(keys_iele)) then
          keys_iele((keys_ilen+1):(keys_ilen+1)) = c_null_char
       end if
       c_keys_int(i) = c_loc(keys_iele(1:1))
    end do
    c_keys = c_loc(c_keys_int(1))
    values_ptr => values
    c_values = c_loc(values_ptr)
    c_use_generic = use_generic
    out = create_dtype_json_object_c(c_nitems, c_keys, c_values, c_use_generic)
    nullify(values_ptr)
  end function create_dtype_json_object
  !> @brief Construct a Ply type object.
  !> @param[in] use_generic If true, serialized or deserialized
  !>   objects will be expected to be generic_t instances.
  !> @returns Type structure.
  function create_dtype_ply(use_generic) &
       result(out)
    implicit none
    logical, value, intent(in) :: use_generic
    type(yggdtype) :: out
    logical(kind = c_bool) :: c_use_generic
    c_use_generic = use_generic
    out = create_dtype_ply_c(c_use_generic)
  end function create_dtype_ply
  !> @brief Construct a Obj type object.
  !> @param[in] use_generic If true, serialized or deserialized
  !>   objects will be expected to be generic_t instances.
  !> @returns Type structure.
  function create_dtype_obj(use_generic) &
       result(out)
    implicit none
    logical, value, intent(in) :: use_generic
    type(yggdtype) :: out
    logical(kind = c_bool) :: c_use_generic
    c_use_generic = use_generic
    out = create_dtype_obj_c(c_use_generic)
  end function create_dtype_obj
  !> @brief Construct an AsciiTable type object.
  !> @param[in] format_str C-style format string that will be used to
  !>   determine the type of elements in arrays that will be
  !>   serialized or deserialized using the resulting type.
  !> @param[in] as_array If true, the types will be arrays. Otherwise they
  !>   will be scalars.
  !> @param[in] use_generic If true, serialized or deserialized
  !>   objects will be expected to be generic_t instances.
  !> @returns Type structure.
  function create_dtype_ascii_table(format_str, as_array, use_generic) &
       result(out)
    implicit none
    character(len = *), intent(in) :: format_str
    logical, value, intent(in) :: as_array
    logical, value, intent(in) :: use_generic
    type(yggdtype) :: out
    character(kind = c_char), dimension(:), allocatable :: c_format_str
    logical(kind = c_bool) :: c_as_array
    logical(kind = c_bool) :: c_use_generic
    c_format_str = convert_string_f2c(format_str)
    c_as_array = as_array
    c_use_generic = use_generic
    out = create_dtype_ascii_table_c(c_format_str, c_as_array, c_use_generic)
    deallocate(c_format_str)
  end function create_dtype_ascii_table
  !> @brief Construct a type object based on the provided format string.
  !> @param[in] format_str C-style format string that will be used to
  !>   determine the type of elements in arrays that will be
  !>   serialized or deserialized using the resulting type.
  !> @param[in] as_array If true, the types will be arrays. Otherwise they
  !>   will be scalars.
  !> @param[in] use_generic If true, serialized or deserialized
  !>   objects will be expected to be generic_t instances.
  !> @returns Type structure.
  function create_dtype_format(format_str, as_array, use_generic) &
       result(out)
    implicit none
    character(len = *), intent(in) :: format_str
    logical, value, intent(in) :: as_array
    logical, value, intent(in) :: use_generic
    type(yggdtype) :: out
    character(kind = c_char), dimension(:), allocatable :: c_format_str
    logical(kind = c_bool) :: c_as_array
    logical(kind = c_bool) :: c_use_generic
    c_format_str = convert_format_f2c(format_str)
    c_as_array = as_array
    c_use_generic = use_generic
    out = create_dtype_format_c(c_format_str, c_as_array, c_use_generic)
    deallocate(c_format_str)
  end function create_dtype_format
  !> @brief Construct a type object for Python objects.
  !> @param[in] type Type string.
  !> @param[in] use_generic If true, serialized or deserialized
  !>   objects will be expected to be generic_t instances.
  !> @returns Type structure.
  function create_dtype_pyobj(type, use_generic) &
       result(out)
    implicit none
    character(len = *), intent(in) :: type
    logical, value, intent(in) :: use_generic
    type(yggdtype) :: out
    character(kind = c_char), dimension(:), allocatable :: c_type
    logical(kind = c_bool) :: c_use_generic
    c_type = convert_string_f2c(type)
    c_use_generic = use_generic
    out = create_dtype_pyobj_c(c_type, c_use_generic)
    deallocate(c_type)
  end function create_dtype_pyobj
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
  function create_dtype_pyinst(class_name, args_dtype, kwargs_dtype, use_generic) &
       result(out)
    implicit none
    character(len = *), intent(in) :: class_name
    type(yggdtype), target :: args_dtype
    type(yggdtype), target :: kwargs_dtype
    logical, value, intent(in) :: use_generic
    type(yggdtype) :: out
    type(yggdtype), pointer :: args_dtype_ptr
    type(yggdtype), pointer :: kwargs_dtype_ptr
    character(kind = c_char), dimension(:), allocatable :: c_class_name
    type(c_ptr) :: c_args_dtype
    type(c_ptr) :: c_kwargs_dtype
    logical(kind = c_bool) :: c_use_generic
    c_class_name = convert_string_f2c(class_name)
    args_dtype_ptr => args_dtype
    c_args_dtype = c_loc(args_dtype_ptr)
    kwargs_dtype_ptr => kwargs_dtype
    c_kwargs_dtype = c_loc(kwargs_dtype_ptr)
    c_use_generic = use_generic
    out = create_dtype_pyinst_c(c_class_name, c_args_dtype, c_kwargs_dtype, c_use_generic)
    deallocate(c_class_name)
    nullify(args_dtype_ptr)
    nullify(kwargs_dtype_ptr)
  end function create_dtype_pyinst
  !> @brief Construct a type object for a schema.
  !> @param[in] use_generic If true, serialized or deserialized
  !>   objects will be expected to be generic_t instances.
  !> @returns Type structure.
  function create_dtype_schema(use_generic) &
       result(out)
    implicit none
    logical, value, intent(in) :: use_generic
    type(yggdtype) :: out
    logical(kind = c_bool) :: c_use_generic
    c_use_generic = use_generic
    out = create_dtype_schema_c(c_use_generic)
  end function create_dtype_schema
  !> @brief Construct a type object for receiving any type.
  !> @param[in] use_generic If true, serialized or deserialized
  !>   objects will be expected to be generic_t instances.
  !> @returns Type structure.
  function create_dtype_any(use_generic) &
       result(out)
    implicit none
    logical, value, intent(in) :: use_generic
    type(yggdtype) :: out
    logical(kind = c_bool) :: c_use_generic
    c_use_generic = use_generic
    out = create_dtype_any_c(c_use_generic)
  end function create_dtype_any
  !> @brief Wrapper for freeing rapidjson::Document class wrapper struct.
  !> @param[in] dtype Wrapper struct for C++ Metadata.
  !> @returns int 0 if free was successfull, -1 if there was an error.
  function destroy_dtype(dtype) &
       result(out)
    implicit none
    type(yggdtype), target :: dtype
    integer :: out
    type(yggdtype), pointer :: dtype_ptr
    type(c_ptr) :: c_dtype
    integer(kind = c_int) :: c_out
    dtype_ptr => dtype
    c_dtype = c_loc(dtype_ptr)
    c_out = destroy_dtype_c(c_dtype)
    nullify(dtype_ptr)
    out = c_out
  end function destroy_dtype
  !> @brief Get a copy of a type structure.
  !> @param[in] dtype Wrapper struct for C++ Metadata.
  !> @returns: Type class.
  function copy_dtype(dtype) &
       result(out)
    implicit none
    type(yggdtype), value, intent(in) :: dtype
    type(yggdtype) :: out
    out = copy_dtype_c(dtype)
  end function copy_dtype
  !> @brief Determine if a type structure indicates that generic objects
  !>   should be used.
  !> @param[in] dtype Wrapper struct for C++ Metadata.
  !> @returns 1 if generic objects will be used, 0 if not, -1 for errors.
  function dtype_uses_generic(dtype) &
       result(out)
    implicit none
    type(yggdtype), value :: dtype
    integer :: out
    integer(kind = c_int) :: c_out
    c_out = dtype_uses_generic_c(dtype)
    out = c_out
  end function dtype_uses_generic
  !> @brief Initialize empty obj structure.
  !> @returns obj_t Obj structure.
  function init_obj() &
       result(out)
    implicit none
    type(yggobj) :: out
    out = init_obj_c()
  end function init_obj
  !> @brief Create a obj structure with generated data.
  !> @returns obj_t Obj structure.
  function generate_obj() &
       result(out)
    implicit none
    type(yggobj) :: out
    out = generate_obj_c()
  end function generate_obj
  !> @brief Free obj structure.
  !> @param[in] p *obj_t Pointer to obj structure.
  subroutine free_obj(p)
    implicit none
    type(yggobj), target :: p
    type(yggobj), pointer :: p_ptr
    type(c_ptr) :: c_p
    p_ptr => p
    c_p = c_loc(p_ptr)
    call free_obj_c(c_p)
    nullify(p_ptr)
  end subroutine free_obj
  !> @brief Copy an obj structure.
  !> @param[in] src obj_t Obj structure that should be copied.
  !> @returns Copy of obj structure.
  function copy_obj(src) &
       result(out)
    implicit none
    type(yggobj), value :: src
    type(yggobj) :: out
    out = copy_obj_c(src)
  end function copy_obj
  !> @brief Display the information contained by an Obj struct.
  !> @param[in] p obj_t Obj structure.
  !> @param[in] indent const char* Indentation that should be added to
  !>   each line.
  subroutine display_obj_indent(p, indent)
    implicit none
    type(yggobj), value :: p
    character(len = *), intent(in) :: indent
    character(kind = c_char), dimension(:), allocatable :: c_indent
    c_indent = convert_string_f2c(indent)
    call display_obj_indent_c(p, c_indent)
    deallocate(c_indent)
  end subroutine display_obj_indent
  !> @brief Display the information contained by an Obj struct.
  !> @param[in] p obj_t Obj structure.
  subroutine display_obj(p)
    implicit none
    type(yggobj), value :: p
    call display_obj_c(p)
  end subroutine display_obj
  !> @brief Get the number of elements of a certain type in the structure.
  !> @param[in] p obj_t ObjWavefront structure.
  !> @param[in] name Name of element type to count.
  !> @returns Number of elements of the specified type.
  function nelements_obj(p, name) &
       result(out)
    implicit none
    type(yggobj), value :: p
    character(len = *), intent(in) :: name
    integer :: out
    character(kind = c_char), dimension(:), allocatable :: c_name
    integer(kind = c_int) :: c_out
    c_name = convert_string_f2c(name)
    c_out = nelements_obj_c(p, c_name)
    deallocate(c_name)
    out = c_out
  end function nelements_obj
  !> @brief Compare two obj structures for equality.
  !> @param[in] a First structure for comparison.
  !> @param[in] b Second structure for comparison.
  !> @returns true if a and b are equal, false otherwise.
  function compare_obj(a, b) &
       result(out)
    implicit none
    type(yggobj), value, intent(in) :: a
    type(yggobj), value, intent(in) :: b
    logical :: out
    logical(kind = c_bool) :: c_out
    c_out = compare_obj_c(a, b)
    out = c_out
  end function compare_obj
  !> @brief Initialize empty ply structure.
  !> @returns ply_t Ply structure.
  function init_ply() &
       result(out)
    implicit none
    type(yggply) :: out
    out = init_ply_c()
  end function init_ply
  !> @brief Create a ply structure with generated data.
  !> @returns ply_t Ply structure.
  function generate_ply() &
       result(out)
    implicit none
    type(yggply) :: out
    out = generate_ply_c()
  end function generate_ply
  !> @brief Free ply structure.
  !> @param[in] p *ply_t Pointer to ply structure.
  subroutine free_ply(p)
    implicit none
    type(yggply), target :: p
    type(yggply), pointer :: p_ptr
    type(c_ptr) :: c_p
    p_ptr => p
    c_p = c_loc(p_ptr)
    call free_ply_c(c_p)
    nullify(p_ptr)
  end subroutine free_ply
  !> @brief Copy an ply structure.
  !> @param[in] src ply_t Ply structure that should be copied.
  !> @returns Copy of ply structure.
  function copy_ply(src) &
       result(out)
    implicit none
    type(yggply), value :: src
    type(yggply) :: out
    out = copy_ply_c(src)
  end function copy_ply
  !> @brief Display the information contained by an Ply struct.
  !> @param[in] p ply_t Ply structure.
  !> @param[in] indent const char* Indentation that should be added to
  !>   each line.
  subroutine display_ply_indent(p, indent)
    implicit none
    type(yggply), value :: p
    character(len = *), intent(in) :: indent
    character(kind = c_char), dimension(:), allocatable :: c_indent
    c_indent = convert_string_f2c(indent)
    call display_ply_indent_c(p, c_indent)
    deallocate(c_indent)
  end subroutine display_ply_indent
  !> @brief Display the information contained by an Ply struct.
  !> @param[in] p ply_t Ply structure.
  subroutine display_ply(p)
    implicit none
    type(yggply), value :: p
    call display_ply_c(p)
  end subroutine display_ply
  !> @brief Get the number of elements of a certain type in the structure.
  !> @param[in] p ply_t Ply structure.
  !> @param[in] name Name of element type to count.
  !> @returns Number of elements of the specified type.
  function nelements_ply(p, name) &
       result(out)
    implicit none
    type(yggply), value :: p
    character(len = *), intent(in) :: name
    integer :: out
    character(kind = c_char), dimension(:), allocatable :: c_name
    integer(kind = c_int) :: c_out
    c_name = convert_string_f2c(name)
    c_out = nelements_ply_c(p, c_name)
    deallocate(c_name)
    out = c_out
  end function nelements_ply
  !> @brief Compare two ply structures for equality.
  !> @param[in] a First structure for comparison.
  !> @param[in] b Second structure for comparison.
  !> @returns true if a and b are equal, false otherwise.
  function compare_ply(a, b) &
       result(out)
    implicit none
    type(yggply), value, intent(in) :: a
    type(yggply), value, intent(in) :: b
    logical :: out
    logical(kind = c_bool) :: c_out
    c_out = compare_ply_c(a, b)
    out = c_out
  end function compare_ply
  !> @brief Get the number of elements in a array
  !> @param[in] x generic_t Generic object that is presumed to contain a
  !>   array
  !> @returns size_t Number of elements in array
  function generic_array_get_size(x) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer :: out
    integer(kind = c_size_t) :: c_out
    c_out = generic_array_get_size_c(x)
    out = c_out
  end function generic_array_get_size
  !> @brief Get the number of elements in a object
  !> @param[in] x generic_t Generic object that is presumed to contain a
  !>   object
  !> @returns size_t Number of elements in object
  function generic_object_get_size(x) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer :: out
    integer(kind = c_size_t) :: c_out
    c_out = generic_object_get_size_c(x)
    out = c_out
  end function generic_object_get_size
  !> @brief Set a given generic item to a null
  !> @param[in] x The generic item to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_null(x, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(yggnull), value, intent(in) :: value
    integer :: out
    type(c_ptr) :: c_value
    integer(kind = c_int) :: c_out
    c_value = value%ptr
    c_out = generic_set_null_c(x, c_value)
    out = c_out
  end function generic_set_null
  !> @brief Set a given generic item to a boolean
  !> @param[in] x The generic item to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_bool(x, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    logical, value, intent(in) :: value
    integer :: out
    logical(kind = c_bool) :: c_value
    integer(kind = c_int) :: c_out
    c_value = value
    c_out = generic_set_bool_c(x, c_value)
    out = c_out
  end function generic_set_bool
  !> @brief Set a given generic item to a integer
  !> @param[in] x The generic item to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_integer(x, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: value
    integer :: out
    integer(kind = c_int) :: c_value
    integer(kind = c_int) :: c_out
    c_value = value
    c_out = generic_set_integer_c(x, c_value)
    out = c_out
  end function generic_set_integer
  !> @brief Set a given generic item to a number
  !> @param[in] x The generic item to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_number(x, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    real(kind = 8), value, intent(in) :: value
    integer :: out
    real(kind = c_double) :: c_value
    integer(kind = c_int) :: c_out
    c_value = value
    c_out = generic_set_number_c(x, c_value)
    out = c_out
  end function generic_set_number
  !> @brief Set a given generic item to a string
  !> @param[in] x The generic item to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_string(x, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: value
    integer :: out
    character(kind = c_char), dimension(:), allocatable :: c_value
    integer(kind = c_int) :: c_out
    c_value = convert_string_f2c(value)
    c_out = generic_set_string_c(x, c_value)
    deallocate(c_value)
    out = c_out
  end function generic_set_string
  !> @brief Set a given generic item to a item
  !> @param[in] x The generic item to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_item(x, type, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: type
    type(c_ptr), value :: value
    integer :: out
    character(kind = c_char), dimension(:), allocatable :: c_type
    integer(kind = c_int) :: c_out
    c_type = convert_string_f2c(type)
    c_out = generic_set_item_c(x, c_type, value)
    deallocate(c_type)
    out = c_out
  end function generic_set_item
  !> @brief Set a given generic item to a array
  !> @param[in] x The generic item to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_array(x, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(ygggeneric), value, intent(in) :: value
    integer :: out
    integer(kind = c_int) :: c_out
    c_out = generic_set_array_c(x, value)
    out = c_out
  end function generic_set_array
  !> @brief Set a given generic item to a object
  !> @param[in] x The generic item to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_object(x, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(ygggeneric), value, intent(in) :: value
    integer :: out
    integer(kind = c_int) :: c_out
    c_out = generic_set_object_c(x, value)
    out = c_out
  end function generic_set_object
  !> @brief Set a given generic item to a ply
  !> @param[in] x The generic item to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_ply(x, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(yggply), value, intent(in) :: value
    integer :: out
    integer(kind = c_int) :: c_out
    c_out = generic_set_ply_c(x, value)
    out = c_out
  end function generic_set_ply
  !> @brief Set a given generic item to a obj
  !> @param[in] x The generic item to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_obj(x, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(yggobj), value, intent(in) :: value
    integer :: out
    integer(kind = c_int) :: c_out
    c_out = generic_set_obj_c(x, value)
    out = c_out
  end function generic_set_obj
  !> @brief Set a given generic item to a class
  !> @param[in] x The generic item to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_python_class(x, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(yggpython), value, intent(in) :: value
    integer :: out
    integer(kind = c_int) :: c_out
    c_out = generic_set_python_class_c(x, value)
    out = c_out
  end function generic_set_python_class
  !> @brief Set a given generic item to a function
  !> @param[in] x The generic item to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_python_function(x, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(yggpython), value, intent(in) :: value
    integer :: out
    integer(kind = c_int) :: c_out
    c_out = generic_set_python_function_c(x, value)
    out = c_out
  end function generic_set_python_function
  !> @brief Set a given generic item to a instance
  !> @param[in] x The generic item to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_python_instance(x, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(yggpython), value, intent(in) :: value
    integer :: out
    integer(kind = c_int) :: c_out
    c_out = generic_set_python_instance_c(x, value)
    out = c_out
  end function generic_set_python_instance
  !> @brief Set a given generic item to a scalar
  !> @param[in] x The generic item to set
  !> @param[in] value Pointer to the memory containing the value to assign to x
  !> @param[in] subtype Subtype of data contained in value
  !> @param[in] precision The precision of the data in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_scalar(x, value, subtype, precision, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(c_ptr), value, intent(in) :: value
    character(len = *), intent(in) :: subtype
    integer, value, intent(in) :: precision
    character(len = *), intent(in) :: units
    integer :: out
    character(kind = c_char), dimension(:), allocatable :: c_subtype
    integer(kind = c_size_t) :: c_precision
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_subtype = convert_string_f2c(subtype)
    c_precision = precision
    c_units = convert_string_f2c(units)
    c_out = generic_set_scalar_c(x, value, c_subtype, c_precision, c_units)
    deallocate(c_subtype)
    deallocate(c_units)
    out = c_out
  end function generic_set_scalar
  !> @brief Set a given generic item to a int scalar
  !> @param[in] x The generic item to set
  !> @param[in] value The value to assign to x
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_int16(x, value, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer(kind = 2), value, intent(in) :: value
    character(len = *), intent(in) :: units
    integer :: out
    integer(kind = c_int16_t) :: c_value
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_value = value
    c_units = convert_string_f2c(units)
    c_out = generic_set_int16_c(x, c_value, c_units)
    deallocate(c_units)
    out = c_out
  end function generic_set_int16
  !> @brief Set a given generic item to a int scalar
  !> @param[in] x The generic item to set
  !> @param[in] value The value to assign to x
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_int32(x, value, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer(kind = 4), value, intent(in) :: value
    character(len = *), intent(in) :: units
    integer :: out
    integer(kind = c_int32_t) :: c_value
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_value = value
    c_units = convert_string_f2c(units)
    c_out = generic_set_int32_c(x, c_value, c_units)
    deallocate(c_units)
    out = c_out
  end function generic_set_int32
  !> @brief Set a given generic item to a int scalar
  !> @param[in] x The generic item to set
  !> @param[in] value The value to assign to x
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_int64(x, value, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer(kind=int64), value, intent(in) :: value
    character(len = *), intent(in) :: units
    integer :: out
    integer(kind=c_int64_t) :: c_value
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_value = value
    c_units = convert_string_f2c(units)
    c_out = generic_set_int64_c(x, c_value, c_units)
    deallocate(c_units)
    out = c_out
  end function generic_set_int64
  !> @brief Set a given generic item to a float scalar
  !> @param[in] x The generic item to set
  !> @param[in] value The value to assign to x
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_float(x, value, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    real(kind = 4), value, intent(in) :: value
    character(len = *), intent(in) :: units
    integer :: out
    real(kind = c_float) :: c_value
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_value = value
    c_units = convert_string_f2c(units)
    c_out = generic_set_float_c(x, c_value, c_units)
    deallocate(c_units)
    out = c_out
  end function generic_set_float
  !> @brief Set a given generic item to a float scalar
  !> @param[in] x The generic item to set
  !> @param[in] value The value to assign to x
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_double(x, value, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    real(kind = 8), value, intent(in) :: value
    character(len = *), intent(in) :: units
    integer :: out
    real(kind = c_double) :: c_value
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_value = value
    c_units = convert_string_f2c(units)
    c_out = generic_set_double_c(x, c_value, c_units)
    deallocate(c_units)
    out = c_out
  end function generic_set_double
  !> @brief Set a given generic item to a complex scalar
  !> @param[in] x The generic item to set
  !> @param[in] value The value to assign to x
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_complex_float(x, value, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    complex(kind = 4), value, intent(in) :: value
    character(len = *), intent(in) :: units
    integer :: out
    type(yggcomplex_float) :: c_value
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_value%re = real(value)
    c_value%im = aimag(value)
    c_units = convert_string_f2c(units)
    c_out = generic_set_complex_float_c(x, c_value, c_units)
    deallocate(c_units)
    out = c_out
  end function generic_set_complex_float
  !> @brief Set a given generic item to a complex scalar
  !> @param[in] x The generic item to set
  !> @param[in] value The value to assign to x
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_complex_double(x, value, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    complex(kind = 8), value, intent(in) :: value
    character(len = *), intent(in) :: units
    integer :: out
    type(yggcomplex_double) :: c_value
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_value%re = real(value)
    c_value%im = aimag(value)
    c_units = convert_string_f2c(units)
    c_out = generic_set_complex_double_c(x, c_value, c_units)
    deallocate(c_units)
    out = c_out
  end function generic_set_complex_double
  !> @brief Set a given generic item to a 1darray
  !> @param[in] x The generic item to set
  !> @param[in] value Pointer to the memory containing the array to assign
  !>   to x
  !> @param[in] subtype Subtype of data contained in value
  !> @param[in] precision The precision of the elements in value
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_1darray(x, value, subtype, precision, length, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(c_ptr), value, intent(in) :: value
    character(len = *), intent(in) :: subtype
    integer, value, intent(in) :: precision
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    integer :: out
    character(kind = c_char), dimension(:), allocatable :: c_subtype
    integer(kind = c_size_t) :: c_precision
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_subtype = convert_string_f2c(subtype)
    c_precision = precision
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_set_1darray_c(x, value, c_subtype, c_precision, c_length, c_units)
    deallocate(c_subtype)
    deallocate(c_units)
    out = c_out
  end function generic_set_1darray
  !> @brief Set a given generic item to a int 1darray
  !> @param[in] x The generic item to set
  !> @param[in] value The array of values to assign to x
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_1darray_int16(x, value, length, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(integer2_nd), value, intent(in) :: value
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    integer :: out
    type(yggptr) :: c_value_int
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_set_1darray_int16_c(x, c_value, c_length, c_units)
    deallocate(c_units)
    out = c_out
  end function generic_set_1darray_int16
  !> @brief Set a given generic item to a int 1darray
  !> @param[in] x The generic item to set
  !> @param[in] value The array of values to assign to x
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_1darray_int32(x, value, length, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(integer4_nd), value, intent(in) :: value
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    integer :: out
    type(yggptr) :: c_value_int
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_set_1darray_int32_c(x, c_value, c_length, c_units)
    deallocate(c_units)
    out = c_out
  end function generic_set_1darray_int32
  !> @brief Set a given generic item to a int 1darray
  !> @param[in] x The generic item to set
  !> @param[in] value The array of values to assign to x
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_1darray_int64(x, value, length, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(integer8_nd), value, intent(in) :: value
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    integer :: out
    type(yggptr) :: c_value_int
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_set_1darray_int64_c(x, c_value, c_length, c_units)
    deallocate(c_units)
    out = c_out
  end function generic_set_1darray_int64
  !> @brief Set a given generic item to a float 1darray
  !> @param[in] x The generic item to set
  !> @param[in] value The array of values to assign to x
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_1darray_float(x, value, length, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(real4_nd), value, intent(in) :: value
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    integer :: out
    type(yggptr) :: c_value_int
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_set_1darray_float_c(x, c_value, c_length, c_units)
    deallocate(c_units)
    out = c_out
  end function generic_set_1darray_float
  !> @brief Set a given generic item to a float 1darray
  !> @param[in] x The generic item to set
  !> @param[in] value The array of values to assign to x
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_1darray_double(x, value, length, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(real8_nd), value, intent(in) :: value
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    integer :: out
    type(yggptr) :: c_value_int
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_set_1darray_double_c(x, c_value, c_length, c_units)
    deallocate(c_units)
    out = c_out
  end function generic_set_1darray_double
  !> @brief Set a given generic item to a complex 1darray
  !> @param[in] x The generic item to set
  !> @param[in] value The array of values to assign to x
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_1darray_complex_float(x, value, length, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(complex4_nd), value, intent(in) :: value
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    integer :: out
    type(yggptr) :: c_value_int
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_set_1darray_complex_float_c(x, c_value, c_length, c_units)
    deallocate(c_units)
    out = c_out
  end function generic_set_1darray_complex_float
  !> @brief Set a given generic item to a complex 1darray
  !> @param[in] x The generic item to set
  !> @param[in] value The array of values to assign to x
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_1darray_complex_double(x, value, length, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(complex8_nd), value, intent(in) :: value
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    integer :: out
    type(yggptr) :: c_value_int
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_set_1darray_complex_double_c(x, c_value, c_length, c_units)
    deallocate(c_units)
    out = c_out
  end function generic_set_1darray_complex_double
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
  function generic_set_ndarray(x, value, subtype, precision, ndim, shape, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(c_ptr), value, intent(in) :: value
    character(len = *), intent(in) :: subtype
    integer, value, intent(in) :: precision
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    integer :: out
    character(kind = c_char), dimension(:), allocatable :: c_subtype
    integer(kind = c_size_t) :: c_precision
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_subtype = convert_string_f2c(subtype)
    c_precision = precision
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_set_ndarray_c(x, value, c_subtype, c_precision, c_ndim, c_shape, c_units)
    deallocate(c_subtype)
    deallocate(c_units)
    out = c_out
  end function generic_set_ndarray
  !> @brief Set a given generic item to a int ndarray
  !> @param[in] x The generic item to set
  !> @param[in] value The array of values to assign to x
  !>   in row-major order
  !> @param[in] ndim The number of dimensions in value, or 0 on error
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_ndarray_int16(x, value, ndim, shape, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(integer2_nd), value, intent(in) :: value
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    integer :: out
    type(yggptr) :: c_value_int
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_set_ndarray_int16_c(x, c_value, c_ndim, c_shape, c_units)
    deallocate(c_units)
    out = c_out
  end function generic_set_ndarray_int16
  !> @brief Set a given generic item to a int ndarray
  !> @param[in] x The generic item to set
  !> @param[in] value The array of values to assign to x
  !>   in row-major order
  !> @param[in] ndim The number of dimensions in value, or 0 on error
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_ndarray_int32(x, value, ndim, shape, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(integer4_nd), value, intent(in) :: value
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    integer :: out
    type(yggptr) :: c_value_int
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_set_ndarray_int32_c(x, c_value, c_ndim, c_shape, c_units)
    deallocate(c_units)
    out = c_out
  end function generic_set_ndarray_int32
  !> @brief Set a given generic item to a int ndarray
  !> @param[in] x The generic item to set
  !> @param[in] value The array of values to assign to x
  !>   in row-major order
  !> @param[in] ndim The number of dimensions in value, or 0 on error
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_ndarray_int64(x, value, ndim, shape, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(integer8_nd), value, intent(in) :: value
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    integer :: out
    type(yggptr) :: c_value_int
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_set_ndarray_int64_c(x, c_value, c_ndim, c_shape, c_units)
    deallocate(c_units)
    out = c_out
  end function generic_set_ndarray_int64
  !> @brief Set a given generic item to a float ndarray
  !> @param[in] x The generic item to set
  !> @param[in] value The array of values to assign to x
  !>   in row-major order
  !> @param[in] ndim The number of dimensions in value, or 0 on error
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_ndarray_float(x, value, ndim, shape, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(real4_nd), value, intent(in) :: value
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    integer :: out
    type(yggptr) :: c_value_int
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_set_ndarray_float_c(x, c_value, c_ndim, c_shape, c_units)
    deallocate(c_units)
    out = c_out
  end function generic_set_ndarray_float
  !> @brief Set a given generic item to a float ndarray
  !> @param[in] x The generic item to set
  !> @param[in] value The array of values to assign to x
  !>   in row-major order
  !> @param[in] ndim The number of dimensions in value, or 0 on error
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_ndarray_double(x, value, ndim, shape, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(real8_nd), value, intent(in) :: value
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    integer :: out
    type(yggptr) :: c_value_int
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_set_ndarray_double_c(x, c_value, c_ndim, c_shape, c_units)
    deallocate(c_units)
    out = c_out
  end function generic_set_ndarray_double
  !> @brief Set a given generic item to a complex ndarray
  !> @param[in] x The generic item to set
  !> @param[in] value The array of values to assign to x
  !>   in row-major order
  !> @param[in] ndim The number of dimensions in value, or 0 on error
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_ndarray_complex_float(x, value, ndim, shape, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(complex4_nd), value, intent(in) :: value
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    integer :: out
    type(yggptr) :: c_value_int
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_set_ndarray_complex_float_c(x, c_value, c_ndim, c_shape, c_units)
    deallocate(c_units)
    out = c_out
  end function generic_set_ndarray_complex_float
  !> @brief Set a given generic item to a complex ndarray
  !> @param[in] x The generic item to set
  !> @param[in] value The array of values to assign to x
  !>   in row-major order
  !> @param[in] ndim The number of dimensions in value, or 0 on error
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_ndarray_complex_double(x, value, ndim, shape, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(complex8_nd), value, intent(in) :: value
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    integer :: out
    type(yggptr) :: c_value_int
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_set_ndarray_complex_double_c(x, c_value, c_ndim, c_shape, c_units)
    deallocate(c_units)
    out = c_out
  end function generic_set_ndarray_complex_double
  !> @brief Set a given generic item to a schema
  !> @param[in] x The generic item to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_schema(x, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(ygggeneric), value, intent(in) :: value
    integer :: out
    integer(kind = c_int) :: c_out
    c_out = generic_set_schema_c(x, value)
    out = c_out
  end function generic_set_schema
  !> @brief Set a given generic item to a any
  !> @param[in] x The generic item to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_any(x, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(ygggeneric), value, intent(in) :: value
    integer :: out
    integer(kind = c_int) :: c_out
    c_out = generic_set_any_c(x, value)
    out = c_out
  end function generic_set_any
  !> @brief Get a null from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @returns Value from x
  function generic_get_null(x) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(yggnull), pointer :: out
    type(c_ptr) :: c_out
    c_out = generic_get_null_c(x)
    call c_f_pointer(c_out, out)
  end function generic_get_null
  !> @brief Get a boolean from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @returns Value from x
  function generic_get_bool(x) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    logical :: out
    logical(kind = c_bool) :: c_out
    c_out = generic_get_bool_c(x)
    out = c_out
  end function generic_get_bool
  !> @brief Get a integer from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @returns Value from x
  function generic_get_integer(x) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer :: out
    integer(kind = c_int) :: c_out
    c_out = generic_get_integer_c(x)
    out = c_out
  end function generic_get_integer
  !> @brief Get a number from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @returns Value from x
  function generic_get_number(x) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    real(kind = 8) :: out
    real(kind = c_double) :: c_out
    c_out = generic_get_number_c(x)
    out = c_out
  end function generic_get_number
  !> @brief Get a string from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @returns Value from x
  function generic_get_string(x) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len=:), allocatable :: out
    type(c_ptr) :: c_out
    c_out = generic_get_string_c(x)
    out = convert_string_c2f(c_out)
  end function generic_get_string
  !> @brief Get the raw item data
  !> @param[in] x Generic item to retrieve data from
  !> @param[in] type Type of item to retrieve
  !> @returns Pointer to data containing raw item data, NULL on error
  function generic_get_item(x, type) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: type
    type(c_ptr) :: out
    character(kind = c_char), dimension(:), allocatable :: c_type
    c_type = convert_string_f2c(type)
    out = generic_get_item_c(x, c_type)
    deallocate(c_type)
  end function generic_get_item
  !> @brief Get the size of the raw item data
  !> @param[in] x Generic item to retrieve data size from
  !> @param[in] type Type of item to retrieve
  !> @returns Number of bytes in raw item data, 0 on error
  function generic_get_item_nbytes(x, type) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: type
    integer :: out
    character(kind = c_char), dimension(:), allocatable :: c_type
    integer(kind = c_int) :: c_out
    c_type = convert_string_f2c(type)
    c_out = generic_get_item_nbytes_c(x, c_type)
    deallocate(c_type)
    out = c_out
    if (c_out.lt.0) then
       stop "generic_get_item_nbytes: Error getting element"
    end if
  end function generic_get_item_nbytes
  !> @brief Get a array from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @returns Value from x
  function generic_get_array(x) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(ygggeneric) :: out
    out = generic_get_array_c(x)
  end function generic_get_array
  !> @brief Get a object from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @returns Value from x
  function generic_get_object(x) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(ygggeneric) :: out
    out = generic_get_object_c(x)
  end function generic_get_object
  !> @brief Get a ply from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @returns Value from x
  function generic_get_ply(x) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(yggply) :: out
    out = generic_get_ply_c(x)
  end function generic_get_ply
  !> @brief Get a obj from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @returns Value from x
  function generic_get_obj(x) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(yggobj) :: out
    out = generic_get_obj_c(x)
  end function generic_get_obj
  !> @brief Get a class from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @returns Value from x
  function generic_get_python_class(x) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(yggpython) :: out
    out = generic_get_python_class_c(x)
  end function generic_get_python_class
  !> @brief Get a function from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @returns Value from x
  function generic_get_python_function(x) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(yggpython) :: out
    out = generic_get_python_function_c(x)
  end function generic_get_python_function
  !> @brief Get a instance from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @returns Value from x
  function generic_get_python_instance(x) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(yggpython) :: out
    out = generic_get_python_instance_c(x)
  end function generic_get_python_instance
  !> @brief Get a scalar from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @param[in] subtype Subtype of data to return
  !> @param[in] precision Precision of the data to return
  !> @returns Pointer to value in x
  function generic_get_scalar(x, subtype, precision) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: subtype
    integer, value, intent(in) :: precision
    type(c_ptr) :: out
    character(kind = c_char), dimension(:), allocatable :: c_subtype
    integer(kind = c_size_t) :: c_precision
    c_subtype = convert_string_f2c(subtype)
    c_precision = precision
    out = generic_get_scalar_c(x, c_subtype, c_precision)
    deallocate(c_subtype)
  end function generic_get_scalar
  !> @brief Get a int scalar from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @returns Value from x
  function generic_get_int16(x) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer(kind = 2) :: out
    integer(kind = c_int16_t) :: c_out
    c_out = generic_get_int16_c(x)
    out = c_out
  end function generic_get_int16
  !> @brief Get a int scalar from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @returns Value from x
  function generic_get_int32(x) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer(kind = 4) :: out
    integer(kind = c_int32_t) :: c_out
    c_out = generic_get_int32_c(x)
    out = c_out
  end function generic_get_int32
  !> @brief Get a int scalar from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @returns Value from x
  function generic_get_int64(x) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer(kind=int64) :: out
    integer(kind=c_int64_t) :: c_out
    c_out = generic_get_int64_c(x)
    out = c_out
  end function generic_get_int64
  !> @brief Get a float scalar from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @returns Value from x
  function generic_get_float(x) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    real(kind = 4) :: out
    real(kind = c_float) :: c_out
    c_out = generic_get_float_c(x)
    out = c_out
  end function generic_get_float
  !> @brief Get a float scalar from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @returns Value from x
  function generic_get_double(x) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    real(kind = 8) :: out
    real(kind = c_double) :: c_out
    c_out = generic_get_double_c(x)
    out = c_out
  end function generic_get_double
  !> @brief Get a complex scalar from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @returns Value from x
  function generic_get_complex_float(x) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    complex(kind = 4) :: out
    type(yggcomplex_float) :: c_out
    c_out = generic_get_complex_float_c(x)
    out = cmplx(c_out%re, c_out%im)
  end function generic_get_complex_float
  !> @brief Get a complex scalar from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @returns Value from x
  function generic_get_complex_double(x) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    complex(kind = 8) :: out
    type(yggcomplex_double) :: c_out
    c_out = generic_get_complex_double_c(x)
    out = cmplx(c_out%re, c_out%im)
  end function generic_get_complex_double
  !> @brief Get a 1darray from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @param[in] subtype Subtype of data to return
  !> @param[in] precision Precision of the data to return
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_get_1darray(x, subtype, precision, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: subtype
    integer, value, intent(in) :: precision
    type(c_ptr), value :: value
    integer :: out
    character(kind = c_char), dimension(:), allocatable :: c_subtype
    integer(kind = c_size_t) :: c_precision
    integer(kind = c_size_t) :: c_out
    c_subtype = convert_string_f2c(subtype)
    c_precision = precision
    c_out = generic_get_1darray_c(x, c_subtype, c_precision, value)
    deallocate(c_subtype)
    out = c_out
  end function generic_get_1darray
  !> @brief Get a int 1darray from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_get_1darray_int16(x, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(integer2_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_get_1darray_int16_c(x, c_value)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_get_1darray_int16
  !> @brief Get a int 1darray from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_get_1darray_int32(x, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(integer4_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_get_1darray_int32_c(x, c_value)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_get_1darray_int32
  !> @brief Get a int 1darray from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_get_1darray_int64(x, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(integer8_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_get_1darray_int64_c(x, c_value)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_get_1darray_int64
  !> @brief Get a float 1darray from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_get_1darray_float(x, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(real4_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_get_1darray_float_c(x, c_value)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_get_1darray_float
  !> @brief Get a float 1darray from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_get_1darray_double(x, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(real8_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_get_1darray_double_c(x, c_value)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_get_1darray_double
  !> @brief Get a complex 1darray from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_get_1darray_complex_float(x, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(complex4_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_get_1darray_complex_float_c(x, c_value)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_get_1darray_complex_float
  !> @brief Get a complex 1darray from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_get_1darray_complex_double(x, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(complex8_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_get_1darray_complex_double_c(x, c_value)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_get_1darray_complex_double
  !> @brief Get a ndarray from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @param[in] subtype Subtype of data to return
  !> @param[in] precision Precision of the data to return
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_get_ndarray(x, subtype, precision, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: subtype
    integer, value, intent(in) :: precision
    type(c_ptr), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(c_ptr), target :: c_shape_target
    character(kind = c_char), dimension(:), allocatable :: c_subtype
    integer(kind = c_size_t) :: c_precision
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_subtype = convert_string_f2c(subtype)
    c_precision = precision
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_get_ndarray_c(x, c_subtype, c_precision, value, c_shape)
    deallocate(c_subtype)
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_get_ndarray
  !> @brief Get a int ndarray from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_get_ndarray_int16(x, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(integer2_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_get_ndarray_int16_c(x, c_value, c_shape)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_get_ndarray_int16
  !> @brief Get a int ndarray from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_get_ndarray_int32(x, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(integer4_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_get_ndarray_int32_c(x, c_value, c_shape)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_get_ndarray_int32
  !> @brief Get a int ndarray from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_get_ndarray_int64(x, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(integer8_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_get_ndarray_int64_c(x, c_value, c_shape)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_get_ndarray_int64
  !> @brief Get a float ndarray from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_get_ndarray_float(x, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(real4_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_get_ndarray_float_c(x, c_value, c_shape)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_get_ndarray_float
  !> @brief Get a float ndarray from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_get_ndarray_double(x, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(real8_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_get_ndarray_double_c(x, c_value, c_shape)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_get_ndarray_double
  !> @brief Get a complex ndarray from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_get_ndarray_complex_float(x, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(complex4_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_get_ndarray_complex_float_c(x, c_value, c_shape)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_get_ndarray_complex_float
  !> @brief Get a complex ndarray from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_get_ndarray_complex_double(x, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(complex8_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_get_ndarray_complex_double_c(x, c_value, c_shape)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_get_ndarray_complex_double
  !> @brief Get a schema from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @returns Value from x
  function generic_get_schema(x) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(ygggeneric) :: out
    out = generic_get_schema_c(x)
  end function generic_get_schema
  !> @brief Get a any from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @returns Value from x
  function generic_get_any(x) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(ygggeneric) :: out
    out = generic_get_any_c(x)
  end function generic_get_any
  !> @brief Get a null from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @returns Value from x
  function generic_ref_get_null(x) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(yggnull), pointer :: out
    type(c_ptr) :: c_out
    c_out = generic_ref_get_null_c(x)
    call c_f_pointer(c_out, out)
  end function generic_ref_get_null
  !> @brief Get a boolean from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @returns Value from x
  function generic_ref_get_bool(x) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    logical :: out
    logical(kind = c_bool) :: c_out
    c_out = generic_ref_get_bool_c(x)
    out = c_out
  end function generic_ref_get_bool
  !> @brief Get a integer from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @returns Value from x
  function generic_ref_get_integer(x) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    integer :: out
    integer(kind = c_int) :: c_out
    c_out = generic_ref_get_integer_c(x)
    out = c_out
  end function generic_ref_get_integer
  !> @brief Get a number from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @returns Value from x
  function generic_ref_get_number(x) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    real(kind = 8) :: out
    real(kind = c_double) :: c_out
    c_out = generic_ref_get_number_c(x)
    out = c_out
  end function generic_ref_get_number
  !> @brief Get a string from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @returns Value from x
  function generic_ref_get_string(x) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    character(len=:), allocatable :: out
    type(c_ptr) :: c_out
    c_out = generic_ref_get_string_c(x)
    out = convert_string_c2f(c_out)
  end function generic_ref_get_string
  !> @brief Get the raw item data
  !> @param[in] x Generic item to retrieve data from
  !> @param[in] type Type of item to retrieve
  !> @returns Pointer to data containing raw item data, NULL on error
  function generic_ref_get_item(x, type) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    character(len = *), intent(in) :: type
    type(c_ptr) :: out
    character(kind = c_char), dimension(:), allocatable :: c_type
    c_type = convert_string_f2c(type)
    out = generic_ref_get_item_c(x, c_type)
    deallocate(c_type)
  end function generic_ref_get_item
  !> @brief Get the size of the raw item data
  !> @param[in] x Generic item to retrieve data size from
  !> @param[in] type Type of item to retrieve
  !> @returns Number of bytes in raw item data, 0 on error
  function generic_ref_get_item_nbytes(x, type) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    character(len = *), intent(in) :: type
    integer :: out
    character(kind = c_char), dimension(:), allocatable :: c_type
    integer(kind = c_int) :: c_out
    c_type = convert_string_f2c(type)
    c_out = generic_ref_get_item_nbytes_c(x, c_type)
    deallocate(c_type)
    out = c_out
  end function generic_ref_get_item_nbytes
  !> @brief Get a array from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @returns Value from x
  function generic_ref_get_array(x) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(ygggeneric) :: out
    out = generic_ref_get_array_c(x)
  end function generic_ref_get_array
  !> @brief Get a object from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @returns Value from x
  function generic_ref_get_object(x) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(ygggeneric) :: out
    out = generic_ref_get_object_c(x)
  end function generic_ref_get_object
  !> @brief Get a ply from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @returns Value from x
  function generic_ref_get_ply(x) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(yggply) :: out
    out = generic_ref_get_ply_c(x)
  end function generic_ref_get_ply
  !> @brief Get a obj from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @returns Value from x
  function generic_ref_get_obj(x) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(yggobj) :: out
    out = generic_ref_get_obj_c(x)
  end function generic_ref_get_obj
  !> @brief Get a class from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @returns Value from x
  function generic_ref_get_python_class(x) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(yggpython) :: out
    out = generic_ref_get_python_class_c(x)
  end function generic_ref_get_python_class
  !> @brief Get a function from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @returns Value from x
  function generic_ref_get_python_function(x) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(yggpython) :: out
    out = generic_ref_get_python_function_c(x)
  end function generic_ref_get_python_function
  !> @brief Get a instance from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @returns Value from x
  function generic_ref_get_python_instance(x) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(yggpython) :: out
    out = generic_ref_get_python_instance_c(x)
  end function generic_ref_get_python_instance
  !> @brief Get a scalar from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @param[in] subtype Subtype of data to return
  !> @param[in] precision Precision of the data to return
  !> @returns Pointer to value in x
  function generic_ref_get_scalar(x, subtype, precision) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    character(len = *), intent(in) :: subtype
    integer, value, intent(in) :: precision
    type(c_ptr) :: out
    character(kind = c_char), dimension(:), allocatable :: c_subtype
    integer(kind = c_size_t) :: c_precision
    c_subtype = convert_string_f2c(subtype)
    c_precision = precision
    out = generic_ref_get_scalar_c(x, c_subtype, c_precision)
    deallocate(c_subtype)
  end function generic_ref_get_scalar
  !> @brief Get a int scalar from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @returns Value from x
  function generic_ref_get_int16(x) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    integer(kind = 2) :: out
    integer(kind = c_int16_t) :: c_out
    c_out = generic_ref_get_int16_c(x)
    out = c_out
  end function generic_ref_get_int16
  !> @brief Get a int scalar from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @returns Value from x
  function generic_ref_get_int32(x) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    integer(kind = 4) :: out
    integer(kind = c_int32_t) :: c_out
    c_out = generic_ref_get_int32_c(x)
    out = c_out
  end function generic_ref_get_int32
  !> @brief Get a int scalar from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @returns Value from x
  function generic_ref_get_int64(x) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    integer(kind=int64) :: out
    integer(kind=c_int64_t) :: c_out
    c_out = generic_ref_get_int64_c(x)
    out = c_out
  end function generic_ref_get_int64
  !> @brief Get a float scalar from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @returns Value from x
  function generic_ref_get_float(x) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    real(kind = 4) :: out
    real(kind = c_float) :: c_out
    c_out = generic_ref_get_float_c(x)
    out = c_out
  end function generic_ref_get_float
  !> @brief Get a float scalar from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @returns Value from x
  function generic_ref_get_double(x) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    real(kind = 8) :: out
    real(kind = c_double) :: c_out
    c_out = generic_ref_get_double_c(x)
    out = c_out
  end function generic_ref_get_double
  !> @brief Get a complex scalar from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @returns Value from x
  function generic_ref_get_complex_float(x) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    complex(kind = 4) :: out
    type(yggcomplex_float) :: c_out
    c_out = generic_ref_get_complex_float_c(x)
    out = cmplx(c_out%re, c_out%im)
  end function generic_ref_get_complex_float
  !> @brief Get a complex scalar from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @returns Value from x
  function generic_ref_get_complex_double(x) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    complex(kind = 8) :: out
    type(yggcomplex_double) :: c_out
    c_out = generic_ref_get_complex_double_c(x)
    out = cmplx(c_out%re, c_out%im)
  end function generic_ref_get_complex_double
  !> @brief Get a 1darray from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @param[in] subtype Subtype of data to return
  !> @param[in] precision Precision of the data to return
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_ref_get_1darray(x, subtype, precision, value) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    character(len = *), intent(in) :: subtype
    integer, value, intent(in) :: precision
    type(c_ptr), value :: value
    integer :: out
    character(kind = c_char), dimension(:), allocatable :: c_subtype
    integer(kind = c_size_t) :: c_precision
    integer(kind = c_size_t) :: c_out
    c_subtype = convert_string_f2c(subtype)
    c_precision = precision
    c_out = generic_ref_get_1darray_c(x, c_subtype, c_precision, value)
    deallocate(c_subtype)
    out = c_out
  end function generic_ref_get_1darray
  !> @brief Get a int 1darray from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_ref_get_1darray_int16(x, value) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(integer2_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_ref_get_1darray_int16_c(x, c_value)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_ref_get_1darray_int16
  !> @brief Get a int 1darray from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_ref_get_1darray_int32(x, value) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(integer4_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_ref_get_1darray_int32_c(x, c_value)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_ref_get_1darray_int32
  !> @brief Get a int 1darray from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_ref_get_1darray_int64(x, value) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(integer8_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_ref_get_1darray_int64_c(x, c_value)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_ref_get_1darray_int64
  !> @brief Get a float 1darray from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_ref_get_1darray_float(x, value) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(real4_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_ref_get_1darray_float_c(x, c_value)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_ref_get_1darray_float
  !> @brief Get a float 1darray from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_ref_get_1darray_double(x, value) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(real8_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_ref_get_1darray_double_c(x, c_value)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_ref_get_1darray_double
  !> @brief Get a complex 1darray from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_ref_get_1darray_complex_float(x, value) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(complex4_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_ref_get_1darray_complex_float_c(x, c_value)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_ref_get_1darray_complex_float
  !> @brief Get a complex 1darray from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_ref_get_1darray_complex_double(x, value) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(complex8_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_ref_get_1darray_complex_double_c(x, c_value)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_ref_get_1darray_complex_double
  !> @brief Get a ndarray from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @param[in] subtype Subtype of data to return
  !> @param[in] precision Precision of the data to return
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_ref_get_ndarray(x, subtype, precision, value, shape) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    character(len = *), intent(in) :: subtype
    integer, value, intent(in) :: precision
    type(c_ptr), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(c_ptr), target :: c_shape_target
    character(kind = c_char), dimension(:), allocatable :: c_subtype
    integer(kind = c_size_t) :: c_precision
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_subtype = convert_string_f2c(subtype)
    c_precision = precision
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_ref_get_ndarray_c(x, c_subtype, c_precision, value, c_shape)
    deallocate(c_subtype)
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_ref_get_ndarray
  !> @brief Get a int ndarray from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_ref_get_ndarray_int16(x, value, shape) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(integer2_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_ref_get_ndarray_int16_c(x, c_value, c_shape)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_ref_get_ndarray_int16
  !> @brief Get a int ndarray from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_ref_get_ndarray_int32(x, value, shape) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(integer4_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_ref_get_ndarray_int32_c(x, c_value, c_shape)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_ref_get_ndarray_int32
  !> @brief Get a int ndarray from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_ref_get_ndarray_int64(x, value, shape) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(integer8_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_ref_get_ndarray_int64_c(x, c_value, c_shape)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_ref_get_ndarray_int64
  !> @brief Get a float ndarray from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_ref_get_ndarray_float(x, value, shape) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(real4_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_ref_get_ndarray_float_c(x, c_value, c_shape)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_ref_get_ndarray_float
  !> @brief Get a float ndarray from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_ref_get_ndarray_double(x, value, shape) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(real8_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_ref_get_ndarray_double_c(x, c_value, c_shape)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_ref_get_ndarray_double
  !> @brief Get a complex ndarray from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_ref_get_ndarray_complex_float(x, value, shape) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(complex4_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_ref_get_ndarray_complex_float_c(x, c_value, c_shape)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_ref_get_ndarray_complex_float
  !> @brief Get a complex ndarray from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_ref_get_ndarray_complex_double(x, value, shape) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(complex8_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_ref_get_ndarray_complex_double_c(x, c_value, c_shape)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_ref_get_ndarray_complex_double
  !> @brief Get a schema from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @returns Value from x
  function generic_ref_get_schema(x) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(ygggeneric) :: out
    out = generic_ref_get_schema_c(x)
  end function generic_ref_get_schema
  !> @brief Get a any from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @returns Value from x
  function generic_ref_get_any(x) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(ygggeneric) :: out
    out = generic_ref_get_any_c(x)
  end function generic_ref_get_any
  !> @brief Set an element in a array to a null
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_null(x, index, value)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(yggnull), value, intent(in) :: value
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value = value%ptr
    c_out = generic_array_set_null_c(x, c_index, c_value)
    if (c_out.lt.0) then
       stop "generic_array_set_null: Error setting element"
    end if
  end subroutine generic_array_set_null
  !> @brief Set an element in a array to a boolean
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_bool(x, index, value)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    logical, value, intent(in) :: value
    integer(kind = c_size_t) :: c_index
    logical(kind = c_bool) :: c_value
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value = value
    c_out = generic_array_set_bool_c(x, c_index, c_value)
    if (c_out.lt.0) then
       stop "generic_array_set_bool: Error setting element"
    end if
  end subroutine generic_array_set_bool
  !> @brief Set an element in a array to a integer
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_integer(x, index, value)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    integer, value, intent(in) :: value
    integer(kind = c_size_t) :: c_index
    integer(kind = c_int) :: c_value
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value = value
    c_out = generic_array_set_integer_c(x, c_index, c_value)
    if (c_out.lt.0) then
       stop "generic_array_set_integer: Error setting element"
    end if
  end subroutine generic_array_set_integer
  !> @brief Set an element in a array to a number
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_number(x, index, value)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    real(kind = 8), value, intent(in) :: value
    integer(kind = c_size_t) :: c_index
    real(kind = c_double) :: c_value
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value = value
    c_out = generic_array_set_number_c(x, c_index, c_value)
    if (c_out.lt.0) then
       stop "generic_array_set_number: Error setting element"
    end if
  end subroutine generic_array_set_number
  !> @brief Set an element in a array to a string
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_string(x, index, value)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    character(len = *), intent(in) :: value
    integer(kind = c_size_t) :: c_index
    character(kind = c_char), dimension(:), allocatable :: c_value
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value = convert_string_f2c(value)
    c_out = generic_array_set_string_c(x, c_index, c_value)
    deallocate(c_value)
    if (c_out.lt.0) then
       stop "generic_array_set_string: Error setting element"
    end if
  end subroutine generic_array_set_string
  !> @brief Set an element in a array to a item
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_item(x, index, type, value)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    character(len = *), intent(in) :: type
    type(c_ptr), value :: value
    integer(kind = c_size_t) :: c_index
    character(kind = c_char), dimension(:), allocatable :: c_type
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_type = convert_string_f2c(type)
    c_out = generic_array_set_item_c(x, c_index, c_type, value)
    deallocate(c_type)
    if (c_out.lt.0) then
       stop "generic_array_set_item: Error setting element"
    end if
  end subroutine generic_array_set_item
  !> @brief Set an element in a array to a array
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_array(x, index, value)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(ygggeneric), value, intent(in) :: value
    integer(kind = c_size_t) :: c_index
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_out = generic_array_set_array_c(x, c_index, value)
    if (c_out.lt.0) then
       stop "generic_array_set_array: Error setting element"
    end if
  end subroutine generic_array_set_array
  !> @brief Set an element in a array to a object
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_object(x, index, value)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(ygggeneric), value, intent(in) :: value
    integer(kind = c_size_t) :: c_index
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_out = generic_array_set_object_c(x, c_index, value)
    if (c_out.lt.0) then
       stop "generic_array_set_object: Error setting element"
    end if
  end subroutine generic_array_set_object
  !> @brief Set an element in a array to a ply
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_ply(x, index, value)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(yggply), value, intent(in) :: value
    integer(kind = c_size_t) :: c_index
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_out = generic_array_set_ply_c(x, c_index, value)
    if (c_out.lt.0) then
       stop "generic_array_set_ply: Error setting element"
    end if
  end subroutine generic_array_set_ply
  !> @brief Set an element in a array to a obj
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_obj(x, index, value)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(yggobj), value, intent(in) :: value
    integer(kind = c_size_t) :: c_index
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_out = generic_array_set_obj_c(x, c_index, value)
    if (c_out.lt.0) then
       stop "generic_array_set_obj: Error setting element"
    end if
  end subroutine generic_array_set_obj
  !> @brief Set an element in a array to a class
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_python_class(x, index, value)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(yggpython), value, intent(in) :: value
    integer(kind = c_size_t) :: c_index
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_out = generic_array_set_python_class_c(x, c_index, value)
    if (c_out.lt.0) then
       stop "generic_array_set_python_class: Error setting element"
    end if
  end subroutine generic_array_set_python_class
  !> @brief Set an element in a array to a function
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_python_function(x, index, value)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(yggpython), value, intent(in) :: value
    integer(kind = c_size_t) :: c_index
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_out = generic_array_set_python_function_c(x, c_index, value)
    if (c_out.lt.0) then
       stop "generic_array_set_python_function: Error setting element"
    end if
  end subroutine generic_array_set_python_function
  !> @brief Set an element in a array to a instance
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_python_instance(x, index, value)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(yggpython), value, intent(in) :: value
    integer(kind = c_size_t) :: c_index
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_out = generic_array_set_python_instance_c(x, c_index, value)
    if (c_out.lt.0) then
       stop "generic_array_set_python_instance: Error setting element"
    end if
  end subroutine generic_array_set_python_instance
  !> @brief Set an element in a array to a scalar
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value Pointer to the memory containing the value to assign to x
  !> @param[in] subtype Subtype of data contained in value
  !> @param[in] precision The precision of the data in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_scalar(x, index, value, subtype, precision, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(c_ptr), value, intent(in) :: value
    character(len = *), intent(in) :: subtype
    integer, value, intent(in) :: precision
    character(len = *), intent(in), optional :: units
    integer(kind = c_size_t) :: c_index
    character(kind = c_char), dimension(:), allocatable :: c_subtype
    integer(kind = c_size_t) :: c_precision
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_subtype = convert_string_f2c(subtype)
    c_precision = precision
    if (present(units)) then
       c_units = convert_string_f2c(units)
    else
       allocate(c_units(1))
       c_units(1) = c_null_char
    end if
    c_out = generic_array_set_scalar_c(x, c_index, value, c_subtype, c_precision, c_units)
    deallocate(c_subtype)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_scalar: Error setting element"
    end if
  end subroutine generic_array_set_scalar
  !> @brief Set an element in a array to a int scalar
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The value to assign to x
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_int16(x, index, value, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    integer(kind = 2), value, intent(in) :: value
    character(len = *), intent(in) :: units
    integer(kind = c_size_t) :: c_index
    integer(kind = c_int16_t) :: c_value
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value = value
    c_units = convert_string_f2c(units)
    c_out = generic_array_set_int16_c(x, c_index, c_value, c_units)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_int16: Error setting element"
    end if
  end subroutine generic_array_set_int16
  !> @brief Set an element in a array to a int scalar
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The value to assign to x
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_int32(x, index, value, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    integer(kind = 4), value, intent(in) :: value
    character(len = *), intent(in) :: units
    integer(kind = c_size_t) :: c_index
    integer(kind = c_int32_t) :: c_value
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value = value
    c_units = convert_string_f2c(units)
    c_out = generic_array_set_int32_c(x, c_index, c_value, c_units)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_int32: Error setting element"
    end if
  end subroutine generic_array_set_int32
  !> @brief Set an element in a array to a int scalar
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The value to assign to x
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_int64(x, index, value, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    integer(kind=int64), value, intent(in) :: value
    character(len = *), intent(in) :: units
    integer(kind = c_size_t) :: c_index
    integer(kind=c_int64_t) :: c_value
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value = value
    c_units = convert_string_f2c(units)
    c_out = generic_array_set_int64_c(x, c_index, c_value, c_units)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_int64: Error setting element"
    end if
  end subroutine generic_array_set_int64
  !> @brief Set an element in a array to a float scalar
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The value to assign to x
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_float(x, index, value, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    real(kind = 4), value, intent(in) :: value
    character(len = *), intent(in) :: units
    integer(kind = c_size_t) :: c_index
    real(kind = c_float) :: c_value
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value = value
    c_units = convert_string_f2c(units)
    c_out = generic_array_set_float_c(x, c_index, c_value, c_units)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_float: Error setting element"
    end if
  end subroutine generic_array_set_float
  !> @brief Set an element in a array to a float scalar
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The value to assign to x
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_double(x, index, value, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    real(kind = 8), value, intent(in) :: value
    character(len = *), intent(in) :: units
    integer(kind = c_size_t) :: c_index
    real(kind = c_double) :: c_value
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value = value
    c_units = convert_string_f2c(units)
    c_out = generic_array_set_double_c(x, c_index, c_value, c_units)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_double: Error setting element"
    end if
  end subroutine generic_array_set_double
  !> @brief Set an element in a array to a complex scalar
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The value to assign to x
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_complex_float(x, index, value, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    complex(kind = 4), value, intent(in) :: value
    character(len = *), intent(in) :: units
    integer(kind = c_size_t) :: c_index
    type(yggcomplex_float) :: c_value
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value%re = real(value)
    c_value%im = aimag(value)
    c_units = convert_string_f2c(units)
    c_out = generic_array_set_complex_float_c(x, c_index, c_value, c_units)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_complex_float: Error setting element"
    end if
  end subroutine generic_array_set_complex_float
  !> @brief Set an element in a array to a complex scalar
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The value to assign to x
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_complex_double(x, index, value, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    complex(kind = 8), value, intent(in) :: value
    character(len = *), intent(in) :: units
    integer(kind = c_size_t) :: c_index
    type(yggcomplex_double) :: c_value
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value%re = real(value)
    c_value%im = aimag(value)
    c_units = convert_string_f2c(units)
    c_out = generic_array_set_complex_double_c(x, c_index, c_value, c_units)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_complex_double: Error setting element"
    end if
  end subroutine generic_array_set_complex_double
  !> @brief Set an element in a array to a 1darray
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value Pointer to the memory containing the array to assign
  !>   to x
  !> @param[in] subtype Subtype of data contained in value
  !> @param[in] precision The precision of the elements in value
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_1darray(x, index, value, subtype, precision, length, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(c_ptr), value, intent(in) :: value
    character(len = *), intent(in) :: subtype
    integer, value, intent(in) :: precision
    integer, value, intent(in) :: length
    character(len = *), intent(in), optional :: units
    integer(kind = c_size_t) :: c_index
    character(kind = c_char), dimension(:), allocatable :: c_subtype
    integer(kind = c_size_t) :: c_precision
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_subtype = convert_string_f2c(subtype)
    c_precision = precision
    c_length = length
    if (present(units)) then
       c_units = convert_string_f2c(units)
    else
       allocate(c_units(1))
       c_units(1) = c_null_char
    end if
    c_out = generic_array_set_1darray_c(x, c_index, value, c_subtype, c_precision, c_length, c_units)
    deallocate(c_subtype)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_1darray: Error setting element"
    end if
  end subroutine generic_array_set_1darray
  !> @brief Set an element in a array to a int 1darray
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The array of values to assign to x
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_1darray_int16(x, index, value, length, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(integer2_nd), value, intent(in) :: value
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_array_set_1darray_int16_c(x, c_index, c_value, c_length, c_units)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_1darray_int16: Error setting element"
    end if
  end subroutine generic_array_set_1darray_int16
  !> @brief Set an element in a array to a int 1darray
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The array of values to assign to x
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_1darray_int32(x, index, value, length, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(integer4_nd), value, intent(in) :: value
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_array_set_1darray_int32_c(x, c_index, c_value, c_length, c_units)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_1darray_int32: Error setting element"
    end if
  end subroutine generic_array_set_1darray_int32
  !> @brief Set an element in a array to a int 1darray
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The array of values to assign to x
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_1darray_int64(x, index, value, length, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(integer8_nd), value, intent(in) :: value
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_array_set_1darray_int64_c(x, c_index, c_value, c_length, c_units)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_1darray_int64: Error setting element"
    end if
  end subroutine generic_array_set_1darray_int64
  !> @brief Set an element in a array to a float 1darray
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The array of values to assign to x
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_1darray_float(x, index, value, length, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(real4_nd), value, intent(in) :: value
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_array_set_1darray_float_c(x, c_index, c_value, c_length, c_units)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_1darray_float: Error setting element"
    end if
  end subroutine generic_array_set_1darray_float
  !> @brief Set an element in a array to a float 1darray
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The array of values to assign to x
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_1darray_double(x, index, value, length, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(real8_nd), value, intent(in) :: value
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_array_set_1darray_double_c(x, c_index, c_value, c_length, c_units)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_1darray_double: Error setting element"
    end if
  end subroutine generic_array_set_1darray_double
  !> @brief Set an element in a array to a complex 1darray
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The array of values to assign to x
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_1darray_complex_float(x, index, value, length, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(complex4_nd), value, intent(in) :: value
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_array_set_1darray_complex_float_c(x, c_index, c_value, c_length, c_units)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_1darray_complex_float: Error setting element"
    end if
  end subroutine generic_array_set_1darray_complex_float
  !> @brief Set an element in a array to a complex 1darray
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The array of values to assign to x
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_1darray_complex_double(x, index, value, length, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(complex8_nd), value, intent(in) :: value
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_array_set_1darray_complex_double_c(x, c_index, c_value, c_length, c_units)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_1darray_complex_double: Error setting element"
    end if
  end subroutine generic_array_set_1darray_complex_double
  !> @brief Set an element in a array to a ndarray
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value Pointer to the memory containing the array to assign
  !>   to x
  !> @param[in] subtype Subtype of data contained in value
  !> @param[in] precision The precision of the elements in value
  !> @param[in] ndim The number of dimensions in value
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_ndarray(x, index, value, subtype, precision, shape, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(c_ptr), value, intent(in) :: value
    character(len = *), intent(in) :: subtype
    integer, value, intent(in) :: precision
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in), optional :: units
    integer(kind = c_size_t) :: c_index
    character(kind = c_char), dimension(:), allocatable :: c_subtype
    integer(kind = c_size_t) :: c_precision
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_subtype = convert_string_f2c(subtype)
    c_precision = precision
    c_ndim = size(shape)
    c_shape = c_loc(shape(1))
    if (present(units)) then
       c_units = convert_string_f2c(units)
    else
       allocate(c_units(1))
       c_units(1) = c_null_char
    end if
    c_out = generic_array_set_ndarray_c(x, c_index, value, c_subtype, c_precision, c_ndim, c_shape, c_units)
    deallocate(c_subtype)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_ndarray: Error setting element"
    end if
  end subroutine generic_array_set_ndarray
  !> @brief Set an element in a array to a int ndarray
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The array of values to assign to x
  !>   in row-major order
  !> @param[in] ndim The number of dimensions in value, or 0 on error
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_ndarray_int16(x, index, value, ndim, shape, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(integer2_nd), value, intent(in) :: value
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_array_set_ndarray_int16_c(x, c_index, c_value, c_ndim, c_shape, c_units)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_ndarray_int16: Error setting element"
    end if
  end subroutine generic_array_set_ndarray_int16
  !> @brief Set an element in a array to a int ndarray
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The array of values to assign to x
  !>   in row-major order
  !> @param[in] ndim The number of dimensions in value, or 0 on error
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_ndarray_int32(x, index, value, ndim, shape, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(integer4_nd), value, intent(in) :: value
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_array_set_ndarray_int32_c(x, c_index, c_value, c_ndim, c_shape, c_units)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_ndarray_int32: Error setting element"
    end if
  end subroutine generic_array_set_ndarray_int32
  !> @brief Set an element in a array to a int ndarray
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The array of values to assign to x
  !>   in row-major order
  !> @param[in] ndim The number of dimensions in value, or 0 on error
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_ndarray_int64(x, index, value, ndim, shape, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(integer8_nd), value, intent(in) :: value
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_array_set_ndarray_int64_c(x, c_index, c_value, c_ndim, c_shape, c_units)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_ndarray_int64: Error setting element"
    end if
  end subroutine generic_array_set_ndarray_int64
  !> @brief Set an element in a array to a float ndarray
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The array of values to assign to x
  !>   in row-major order
  !> @param[in] ndim The number of dimensions in value, or 0 on error
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_ndarray_float(x, index, value, ndim, shape, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(real4_nd), value, intent(in) :: value
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_array_set_ndarray_float_c(x, c_index, c_value, c_ndim, c_shape, c_units)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_ndarray_float: Error setting element"
    end if
  end subroutine generic_array_set_ndarray_float
  !> @brief Set an element in a array to a float ndarray
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The array of values to assign to x
  !>   in row-major order
  !> @param[in] ndim The number of dimensions in value, or 0 on error
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_ndarray_double(x, index, value, ndim, shape, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(real8_nd), value, intent(in) :: value
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_array_set_ndarray_double_c(x, c_index, c_value, c_ndim, c_shape, c_units)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_ndarray_double: Error setting element"
    end if
  end subroutine generic_array_set_ndarray_double
  !> @brief Set an element in a array to a complex ndarray
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The array of values to assign to x
  !>   in row-major order
  !> @param[in] ndim The number of dimensions in value, or 0 on error
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_ndarray_complex_float(x, index, value, ndim, shape, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(complex4_nd), value, intent(in) :: value
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_array_set_ndarray_complex_float_c(x, c_index, c_value, c_ndim, c_shape, c_units)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_ndarray_complex_float: Error setting element"
    end if
  end subroutine generic_array_set_ndarray_complex_float
  !> @brief Set an element in a array to a complex ndarray
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The array of values to assign to x
  !>   in row-major order
  !> @param[in] ndim The number of dimensions in value, or 0 on error
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_ndarray_complex_double(x, index, value, ndim, shape, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(complex8_nd), value, intent(in) :: value
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_array_set_ndarray_complex_double_c(x, c_index, c_value, c_ndim, c_shape, c_units)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_ndarray_complex_double: Error setting element"
    end if
  end subroutine generic_array_set_ndarray_complex_double
  !> @brief Set an element in a array to a schema
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_schema(x, index, value)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(ygggeneric), value, intent(in) :: value
    integer(kind = c_size_t) :: c_index
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_out = generic_array_set_schema_c(x, c_index, value)
    if (c_out.lt.0) then
       stop "generic_array_set_schema: Error setting element"
    end if
  end subroutine generic_array_set_schema
  !> @brief Set an element in a array to a any
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_any(x, index, value)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(ygggeneric), value, intent(in) :: value
    integer(kind = c_size_t) :: c_index
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_out = generic_array_set_any_c(x, c_index, value)
    if (c_out.lt.0) then
       stop "generic_array_set_any: Error setting element"
    end if
  end subroutine generic_array_set_any
  !> @brief Get a null from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @returns Value from x
  function generic_array_get_null(x, index) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(yggnull), pointer :: out
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_out
    c_index = index - 1
    c_out = generic_array_get_null_c(x, c_index)
    call c_f_pointer(c_out, out)
  end function generic_array_get_null
  !> @brief Get a boolean from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @returns Value from x
  function generic_array_get_bool(x, index) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    logical :: out
    integer(kind = c_size_t) :: c_index
    logical(kind = c_bool) :: c_out
    c_index = index - 1
    c_out = generic_array_get_bool_c(x, c_index)
    out = c_out
  end function generic_array_get_bool
  !> @brief Get a integer from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @returns Value from x
  function generic_array_get_integer(x, index) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    integer :: out
    integer(kind = c_size_t) :: c_index
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_out = generic_array_get_integer_c(x, c_index)
    out = c_out
  end function generic_array_get_integer
  !> @brief Get a number from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @returns Value from x
  function generic_array_get_number(x, index) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    real(kind = 8) :: out
    integer(kind = c_size_t) :: c_index
    real(kind = c_double) :: c_out
    c_index = index - 1
    c_out = generic_array_get_number_c(x, c_index)
    out = c_out
  end function generic_array_get_number
  !> @brief Get a string from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @returns Value from x
  function generic_array_get_string(x, index) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    character(len=:), allocatable :: out
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_out
    c_index = index - 1
    c_out = generic_array_get_string_c(x, c_index)
    out = convert_string_c2f(c_out)
  end function generic_array_get_string
  !> @brief Get a item from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @param[in] type Type of item to retrieve
  !> @returns Pointer to data containing raw item data, NULL on error
  function generic_array_get_item(x, index, type) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    character(len = *), intent(in) :: type
    type(c_ptr) :: out
    integer(kind = c_size_t) :: c_index
    character(kind = c_char), dimension(:), allocatable :: c_type
    c_index = index - 1
    c_type = convert_string_f2c(type)
    out = generic_array_get_item_c(x, c_index, c_type)
    deallocate(c_type)
  end function generic_array_get_item
  !> @brief Get a item_nbytes from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @param[in] type Type of item to retrieve
  !> @returns Number of bytes in raw item data, 0 on error
  function generic_array_get_item_nbytes(x, index, type) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    character(len = *), intent(in) :: type
    integer :: out
    integer(kind = c_size_t) :: c_index
    character(kind = c_char), dimension(:), allocatable :: c_type
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_type = convert_string_f2c(type)
    c_out = generic_array_get_item_nbytes_c(x, c_index, c_type)
    deallocate(c_type)
    out = c_out
    if (c_out.lt.0) then
       stop "generic_array_get_item_nbytes: Error getting element"
    end if
  end function generic_array_get_item_nbytes
  !> @brief Get a array from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @returns Value from x
  function generic_array_get_array(x, index) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(ygggeneric) :: out
    integer(kind = c_size_t) :: c_index
    c_index = index - 1
    out = generic_array_get_array_c(x, c_index)
  end function generic_array_get_array
  !> @brief Get a object from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @returns Value from x
  function generic_array_get_object(x, index) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(ygggeneric) :: out
    integer(kind = c_size_t) :: c_index
    c_index = index - 1
    out = generic_array_get_object_c(x, c_index)
  end function generic_array_get_object
  !> @brief Get a ply from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @returns Value from x
  function generic_array_get_ply(x, index) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(yggply) :: out
    integer(kind = c_size_t) :: c_index
    c_index = index - 1
    out = generic_array_get_ply_c(x, c_index)
  end function generic_array_get_ply
  !> @brief Get a obj from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @returns Value from x
  function generic_array_get_obj(x, index) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(yggobj) :: out
    integer(kind = c_size_t) :: c_index
    c_index = index - 1
    out = generic_array_get_obj_c(x, c_index)
  end function generic_array_get_obj
  !> @brief Get a class from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @returns Value from x
  function generic_array_get_python_class(x, index) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(yggpython) :: out
    integer(kind = c_size_t) :: c_index
    c_index = index - 1
    out = generic_array_get_python_class_c(x, c_index)
  end function generic_array_get_python_class
  !> @brief Get a function from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @returns Value from x
  function generic_array_get_python_function(x, index) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(yggpython) :: out
    integer(kind = c_size_t) :: c_index
    c_index = index - 1
    out = generic_array_get_python_function_c(x, c_index)
  end function generic_array_get_python_function
  !> @brief Get a instance from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @returns Value from x
  function generic_array_get_python_instance(x, index) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(yggpython) :: out
    integer(kind = c_size_t) :: c_index
    c_index = index - 1
    out = generic_array_get_python_instance_c(x, c_index)
  end function generic_array_get_python_instance
  !> @brief Get a scalar from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @param[in] subtype Subtype of data to return
  !> @param[in] precision Precision of the data to return
  !> @returns Pointer to value in x
  function generic_array_get_scalar(x, index, subtype, precision) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    character(len = *), intent(in) :: subtype
    integer, value, intent(in) :: precision
    type(c_ptr) :: out
    integer(kind = c_size_t) :: c_index
    character(kind = c_char), dimension(:), allocatable :: c_subtype
    integer(kind = c_size_t) :: c_precision
    c_index = index - 1
    c_subtype = convert_string_f2c(subtype)
    c_precision = precision
    out = generic_array_get_scalar_c(x, c_index, c_subtype, c_precision)
    deallocate(c_subtype)
  end function generic_array_get_scalar
  !> @brief Get a int scalar from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @returns Value from x
  function generic_array_get_int16(x, index) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    integer(kind = 2) :: out
    integer(kind = c_size_t) :: c_index
    integer(kind = c_int16_t) :: c_out
    c_index = index - 1
    c_out = generic_array_get_int16_c(x, c_index)
    out = c_out
  end function generic_array_get_int16
  !> @brief Get a int scalar from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @returns Value from x
  function generic_array_get_int32(x, index) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    integer(kind = 4) :: out
    integer(kind = c_size_t) :: c_index
    integer(kind = c_int32_t) :: c_out
    c_index = index - 1
    c_out = generic_array_get_int32_c(x, c_index)
    out = c_out
  end function generic_array_get_int32
  !> @brief Get a int scalar from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @returns Value from x
  function generic_array_get_int64(x, index) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    integer(kind=int64) :: out
    integer(kind = c_size_t) :: c_index
    integer(kind=c_int64_t) :: c_out
    c_index = index - 1
    c_out = generic_array_get_int64_c(x, c_index)
    out = c_out
  end function generic_array_get_int64
  !> @brief Get a float scalar from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @returns Value from x
  function generic_array_get_float(x, index) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    real(kind = 4) :: out
    integer(kind = c_size_t) :: c_index
    real(kind = c_float) :: c_out
    c_index = index - 1
    c_out = generic_array_get_float_c(x, c_index)
    out = c_out
  end function generic_array_get_float
  !> @brief Get a float scalar from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @returns Value from x
  function generic_array_get_double(x, index) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    real(kind = 8) :: out
    integer(kind = c_size_t) :: c_index
    real(kind = c_double) :: c_out
    c_index = index - 1
    c_out = generic_array_get_double_c(x, c_index)
    out = c_out
  end function generic_array_get_double
  !> @brief Get a complex scalar from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @returns Value from x
  function generic_array_get_complex_float(x, index) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    complex(kind = 4) :: out
    integer(kind = c_size_t) :: c_index
    type(yggcomplex_float) :: c_out
    c_index = index - 1
    c_out = generic_array_get_complex_float_c(x, c_index)
    out = cmplx(c_out%re, c_out%im)
  end function generic_array_get_complex_float
  !> @brief Get a complex scalar from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @returns Value from x
  function generic_array_get_complex_double(x, index) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    complex(kind = 8) :: out
    integer(kind = c_size_t) :: c_index
    type(yggcomplex_double) :: c_out
    c_index = index - 1
    c_out = generic_array_get_complex_double_c(x, c_index)
    out = cmplx(c_out%re, c_out%im)
  end function generic_array_get_complex_double
  !> @brief Get a 1darray from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @param[in] subtype Subtype of data to return
  !> @param[in] precision Precision of the data to return
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_array_get_1darray(x, index, subtype, precision, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    character(len = *), intent(in) :: subtype
    integer, value, intent(in) :: precision
    type(c_ptr), value :: value
    integer :: out
    integer(kind = c_size_t) :: c_index
    character(kind = c_char), dimension(:), allocatable :: c_subtype
    integer(kind = c_size_t) :: c_precision
    integer(kind = c_size_t) :: c_out
    c_index = index - 1
    c_subtype = convert_string_f2c(subtype)
    c_precision = precision
    c_out = generic_array_get_1darray_c(x, c_index, c_subtype, c_precision, value)
    deallocate(c_subtype)
    out = c_out
  end function generic_array_get_1darray
  !> @brief Get a int 1darray from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_array_get_1darray_int16(x, index, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(integer2_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_index = index - 1
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_array_get_1darray_int16_c(x, c_index, c_value)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_array_get_1darray_int16
  !> @brief Get a int 1darray from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_array_get_1darray_int32(x, index, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(integer4_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_index = index - 1
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_array_get_1darray_int32_c(x, c_index, c_value)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_array_get_1darray_int32
  !> @brief Get a int 1darray from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_array_get_1darray_int64(x, index, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(integer8_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_index = index - 1
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_array_get_1darray_int64_c(x, c_index, c_value)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_array_get_1darray_int64
  !> @brief Get a float 1darray from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_array_get_1darray_float(x, index, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(real4_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_index = index - 1
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_array_get_1darray_float_c(x, c_index, c_value)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_array_get_1darray_float
  !> @brief Get a float 1darray from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_array_get_1darray_double(x, index, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(real8_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_index = index - 1
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_array_get_1darray_double_c(x, c_index, c_value)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_array_get_1darray_double
  !> @brief Get a complex 1darray from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_array_get_1darray_complex_float(x, index, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(complex4_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_index = index - 1
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_array_get_1darray_complex_float_c(x, c_index, c_value)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_array_get_1darray_complex_float
  !> @brief Get a complex 1darray from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_array_get_1darray_complex_double(x, index, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(complex8_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_index = index - 1
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_array_get_1darray_complex_double_c(x, c_index, c_value)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_array_get_1darray_complex_double
  !> @brief Get a ndarray from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @param[in] subtype Subtype of data to return
  !> @param[in] precision Precision of the data to return
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_array_get_ndarray(x, index, subtype, precision, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    character(len = *), intent(in) :: subtype
    integer, value, intent(in) :: precision
    type(c_ptr), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(c_ptr), target :: c_shape_target
    integer(kind = c_size_t) :: c_index
    character(kind = c_char), dimension(:), allocatable :: c_subtype
    integer(kind = c_size_t) :: c_precision
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_index = index - 1
    c_subtype = convert_string_f2c(subtype)
    c_precision = precision
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_array_get_ndarray_c(x, c_index, c_subtype, c_precision, value, c_shape)
    deallocate(c_subtype)
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_array_get_ndarray
  !> @brief Get a int ndarray from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_array_get_ndarray_int16(x, index, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(integer2_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_index = index - 1
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_array_get_ndarray_int16_c(x, c_index, c_value, c_shape)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_array_get_ndarray_int16
  !> @brief Get a int ndarray from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_array_get_ndarray_int32(x, index, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(integer4_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_index = index - 1
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_array_get_ndarray_int32_c(x, c_index, c_value, c_shape)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_array_get_ndarray_int32
  !> @brief Get a int ndarray from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_array_get_ndarray_int64(x, index, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(integer8_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_index = index - 1
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_array_get_ndarray_int64_c(x, c_index, c_value, c_shape)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_array_get_ndarray_int64
  !> @brief Get a float ndarray from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_array_get_ndarray_float(x, index, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(real4_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_index = index - 1
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_array_get_ndarray_float_c(x, c_index, c_value, c_shape)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_array_get_ndarray_float
  !> @brief Get a float ndarray from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_array_get_ndarray_double(x, index, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(real8_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_index = index - 1
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_array_get_ndarray_double_c(x, c_index, c_value, c_shape)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_array_get_ndarray_double
  !> @brief Get a complex ndarray from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_array_get_ndarray_complex_float(x, index, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(complex4_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_index = index - 1
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_array_get_ndarray_complex_float_c(x, c_index, c_value, c_shape)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_array_get_ndarray_complex_float
  !> @brief Get a complex ndarray from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_array_get_ndarray_complex_double(x, index, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(complex8_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_index = index - 1
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_array_get_ndarray_complex_double_c(x, c_index, c_value, c_shape)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_array_get_ndarray_complex_double
  !> @brief Get a schema from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @returns Value from x
  function generic_array_get_schema(x, index) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(ygggeneric) :: out
    integer(kind = c_size_t) :: c_index
    c_index = index - 1
    out = generic_array_get_schema_c(x, c_index)
  end function generic_array_get_schema
  !> @brief Get a any from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @returns Value from x
  function generic_array_get_any(x, index) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(ygggeneric) :: out
    integer(kind = c_size_t) :: c_index
    c_index = index - 1
    out = generic_array_get_any_c(x, c_index)
  end function generic_array_get_any
  !> @brief Set an element in a object to a null
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_null(x, key, value)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(yggnull), value, intent(in) :: value
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value = value%ptr
    c_out = generic_object_set_null_c(x, c_key, c_value)
    deallocate(c_key)
    if (c_out.lt.0) then
       stop "generic_object_set_null: Error setting element"
    end if
  end subroutine generic_object_set_null
  !> @brief Set an element in a object to a boolean
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_bool(x, key, value)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    logical, value, intent(in) :: value
    character(kind = c_char), dimension(:), allocatable :: c_key
    logical(kind = c_bool) :: c_value
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value = value
    c_out = generic_object_set_bool_c(x, c_key, c_value)
    deallocate(c_key)
    if (c_out.lt.0) then
       stop "generic_object_set_bool: Error setting element"
    end if
  end subroutine generic_object_set_bool
  !> @brief Set an element in a object to a integer
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_integer(x, key, value)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    integer, value, intent(in) :: value
    character(kind = c_char), dimension(:), allocatable :: c_key
    integer(kind = c_int) :: c_value
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value = value
    c_out = generic_object_set_integer_c(x, c_key, c_value)
    deallocate(c_key)
    if (c_out.lt.0) then
       stop "generic_object_set_integer: Error setting element"
    end if
  end subroutine generic_object_set_integer
  !> @brief Set an element in a object to a number
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_number(x, key, value)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    real(kind = 8), value, intent(in) :: value
    character(kind = c_char), dimension(:), allocatable :: c_key
    real(kind = c_double) :: c_value
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value = value
    c_out = generic_object_set_number_c(x, c_key, c_value)
    deallocate(c_key)
    if (c_out.lt.0) then
       stop "generic_object_set_number: Error setting element"
    end if
  end subroutine generic_object_set_number
  !> @brief Set an element in a object to a string
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_string(x, key, value)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    character(len = *), intent(in) :: value
    character(kind = c_char), dimension(:), allocatable :: c_key
    character(kind = c_char), dimension(:), allocatable :: c_value
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value = convert_string_f2c(value)
    c_out = generic_object_set_string_c(x, c_key, c_value)
    deallocate(c_key)
    deallocate(c_value)
    if (c_out.lt.0) then
       stop "generic_object_set_string: Error setting element"
    end if
  end subroutine generic_object_set_string
  !> @brief Set an element in a object to a item
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_item(x, key, type, value)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    character(len = *), intent(in) :: type
    type(c_ptr), value :: value
    character(kind = c_char), dimension(:), allocatable :: c_key
    character(kind = c_char), dimension(:), allocatable :: c_type
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_type = convert_string_f2c(type)
    c_out = generic_object_set_item_c(x, c_key, c_type, value)
    deallocate(c_key)
    deallocate(c_type)
    if (c_out.lt.0) then
       stop "generic_object_set_item: Error setting element"
    end if
  end subroutine generic_object_set_item
  !> @brief Set an element in a object to a array
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_array(x, key, value)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(ygggeneric), value, intent(in) :: value
    character(kind = c_char), dimension(:), allocatable :: c_key
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_out = generic_object_set_array_c(x, c_key, value)
    deallocate(c_key)
    if (c_out.lt.0) then
       stop "generic_object_set_array: Error setting element"
    end if
  end subroutine generic_object_set_array
  !> @brief Set an element in a object to a object
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_object(x, key, value)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(ygggeneric), value, intent(in) :: value
    character(kind = c_char), dimension(:), allocatable :: c_key
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_out = generic_object_set_object_c(x, c_key, value)
    deallocate(c_key)
    if (c_out.lt.0) then
       stop "generic_object_set_object: Error setting element"
    end if
  end subroutine generic_object_set_object
  !> @brief Set an element in a object to a ply
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_ply(x, key, value)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(yggply), value, intent(in) :: value
    character(kind = c_char), dimension(:), allocatable :: c_key
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_out = generic_object_set_ply_c(x, c_key, value)
    deallocate(c_key)
    if (c_out.lt.0) then
       stop "generic_object_set_ply: Error setting element"
    end if
  end subroutine generic_object_set_ply
  !> @brief Set an element in a object to a obj
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_obj(x, key, value)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(yggobj), value, intent(in) :: value
    character(kind = c_char), dimension(:), allocatable :: c_key
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_out = generic_object_set_obj_c(x, c_key, value)
    deallocate(c_key)
    if (c_out.lt.0) then
       stop "generic_object_set_obj: Error setting element"
    end if
  end subroutine generic_object_set_obj
  !> @brief Set an element in a object to a class
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_python_class(x, key, value)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(yggpython), value, intent(in) :: value
    character(kind = c_char), dimension(:), allocatable :: c_key
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_out = generic_object_set_python_class_c(x, c_key, value)
    deallocate(c_key)
    if (c_out.lt.0) then
       stop "generic_object_set_python_class: Error setting element"
    end if
  end subroutine generic_object_set_python_class
  !> @brief Set an element in a object to a function
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_python_function(x, key, value)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(yggpython), value, intent(in) :: value
    character(kind = c_char), dimension(:), allocatable :: c_key
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_out = generic_object_set_python_function_c(x, c_key, value)
    deallocate(c_key)
    if (c_out.lt.0) then
       stop "generic_object_set_python_function: Error setting element"
    end if
  end subroutine generic_object_set_python_function
  !> @brief Set an element in a object to a instance
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_python_instance(x, key, value)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(yggpython), value, intent(in) :: value
    character(kind = c_char), dimension(:), allocatable :: c_key
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_out = generic_object_set_python_instance_c(x, c_key, value)
    deallocate(c_key)
    if (c_out.lt.0) then
       stop "generic_object_set_python_instance: Error setting element"
    end if
  end subroutine generic_object_set_python_instance
  !> @brief Set an element in a object to a scalar
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value Pointer to the memory containing the value to assign to x
  !> @param[in] subtype Subtype of data contained in value
  !> @param[in] precision The precision of the data in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_scalar(x, key, value, subtype, precision, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(c_ptr), value, intent(in) :: value
    character(len = *), intent(in) :: subtype
    integer, value, intent(in) :: precision
    character(len = *), intent(in), optional :: units
    character(kind = c_char), dimension(:), allocatable :: c_key
    character(kind = c_char), dimension(:), allocatable :: c_subtype
    integer(kind = c_size_t) :: c_precision
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_subtype = convert_string_f2c(subtype)
    c_precision = precision
    if (present(units)) then
       c_units = convert_string_f2c(units)
    else
       allocate(c_units(1))
       c_units(1) = c_null_char
    end if
    c_out = generic_object_set_scalar_c(x, c_key, value, c_subtype, c_precision, c_units)
    deallocate(c_key)
    deallocate(c_subtype)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_scalar: Error setting element"
    end if
  end subroutine generic_object_set_scalar
  !> @brief Set an element in a object to a int scalar
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The value to assign to x
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_int16(x, key, value, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    integer(kind = 2), value, intent(in) :: value
    character(len = *), intent(in) :: units
    character(kind = c_char), dimension(:), allocatable :: c_key
    integer(kind = c_int16_t) :: c_value
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value = value
    c_units = convert_string_f2c(units)
    c_out = generic_object_set_int16_c(x, c_key, c_value, c_units)
    deallocate(c_key)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_int16: Error setting element"
    end if
  end subroutine generic_object_set_int16
  !> @brief Set an element in a object to a int scalar
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The value to assign to x
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_int32(x, key, value, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    integer(kind = 4), value, intent(in) :: value
    character(len = *), intent(in) :: units
    character(kind = c_char), dimension(:), allocatable :: c_key
    integer(kind = c_int32_t) :: c_value
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value = value
    c_units = convert_string_f2c(units)
    c_out = generic_object_set_int32_c(x, c_key, c_value, c_units)
    deallocate(c_key)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_int32: Error setting element"
    end if
  end subroutine generic_object_set_int32
  !> @brief Set an element in a object to a int scalar
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The value to assign to x
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_int64(x, key, value, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    integer(kind=int64), value, intent(in) :: value
    character(len = *), intent(in) :: units
    character(kind = c_char), dimension(:), allocatable :: c_key
    integer(kind=c_int64_t) :: c_value
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value = value
    c_units = convert_string_f2c(units)
    c_out = generic_object_set_int64_c(x, c_key, c_value, c_units)
    deallocate(c_key)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_int64: Error setting element"
    end if
  end subroutine generic_object_set_int64
  !> @brief Set an element in a object to a float scalar
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The value to assign to x
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_float(x, key, value, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    real(kind = 4), value, intent(in) :: value
    character(len = *), intent(in) :: units
    character(kind = c_char), dimension(:), allocatable :: c_key
    real(kind = c_float) :: c_value
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value = value
    c_units = convert_string_f2c(units)
    c_out = generic_object_set_float_c(x, c_key, c_value, c_units)
    deallocate(c_key)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_float: Error setting element"
    end if
  end subroutine generic_object_set_float
  !> @brief Set an element in a object to a float scalar
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The value to assign to x
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_double(x, key, value, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    real(kind = 8), value, intent(in) :: value
    character(len = *), intent(in) :: units
    character(kind = c_char), dimension(:), allocatable :: c_key
    real(kind = c_double) :: c_value
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value = value
    c_units = convert_string_f2c(units)
    c_out = generic_object_set_double_c(x, c_key, c_value, c_units)
    deallocate(c_key)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_double: Error setting element"
    end if
  end subroutine generic_object_set_double
  !> @brief Set an element in a object to a complex scalar
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The value to assign to x
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_complex_float(x, key, value, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    complex(kind = 4), value, intent(in) :: value
    character(len = *), intent(in) :: units
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(yggcomplex_float) :: c_value
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value%re = real(value)
    c_value%im = aimag(value)
    c_units = convert_string_f2c(units)
    c_out = generic_object_set_complex_float_c(x, c_key, c_value, c_units)
    deallocate(c_key)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_complex_float: Error setting element"
    end if
  end subroutine generic_object_set_complex_float
  !> @brief Set an element in a object to a complex scalar
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The value to assign to x
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_complex_double(x, key, value, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    complex(kind = 8), value, intent(in) :: value
    character(len = *), intent(in) :: units
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(yggcomplex_double) :: c_value
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value%re = real(value)
    c_value%im = aimag(value)
    c_units = convert_string_f2c(units)
    c_out = generic_object_set_complex_double_c(x, c_key, c_value, c_units)
    deallocate(c_key)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_complex_double: Error setting element"
    end if
  end subroutine generic_object_set_complex_double
  !> @brief Set an element in a object to a 1darray
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value Pointer to the memory containing the array to assign
  !>   to x
  !> @param[in] subtype Subtype of data contained in value
  !> @param[in] precision The precision of the elements in value
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_1darray(x, key, value, subtype, precision, length, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(c_ptr), value, intent(in) :: value
    character(len = *), intent(in) :: subtype
    integer, value, intent(in) :: precision
    integer, value, intent(in) :: length
    character(len = *), intent(in), optional :: units
    character(kind = c_char), dimension(:), allocatable :: c_key
    character(kind = c_char), dimension(:), allocatable :: c_subtype
    integer(kind = c_size_t) :: c_precision
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_subtype = convert_string_f2c(subtype)
    c_precision = precision
    c_length = length
    if (present(units)) then
       c_units = convert_string_f2c(units)
    else
       allocate(c_units(1))
       c_units(1) = c_null_char
    end if
    c_out = generic_object_set_1darray_c(x, c_key, value, c_subtype, c_precision, c_length, c_units)
    deallocate(c_key)
    deallocate(c_subtype)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_1darray: Error setting element"
    end if
  end subroutine generic_object_set_1darray
  !> @brief Set an element in a object to a int 1darray
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The array of values to assign to x
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_1darray_int16(x, key, value, length, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(integer2_nd), value, intent(in) :: value
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_object_set_1darray_int16_c(x, c_key, c_value, c_length, c_units)
    deallocate(c_key)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_1darray_int16: Error setting element"
    end if
  end subroutine generic_object_set_1darray_int16
  !> @brief Set an element in a object to a int 1darray
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The array of values to assign to x
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_1darray_int32(x, key, value, length, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(integer4_nd), value, intent(in) :: value
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_object_set_1darray_int32_c(x, c_key, c_value, c_length, c_units)
    deallocate(c_key)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_1darray_int32: Error setting element"
    end if
  end subroutine generic_object_set_1darray_int32
  !> @brief Set an element in a object to a int 1darray
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The array of values to assign to x
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_1darray_int64(x, key, value, length, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(integer8_nd), value, intent(in) :: value
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_object_set_1darray_int64_c(x, c_key, c_value, c_length, c_units)
    deallocate(c_key)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_1darray_int64: Error setting element"
    end if
  end subroutine generic_object_set_1darray_int64
  !> @brief Set an element in a object to a float 1darray
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The array of values to assign to x
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_1darray_float(x, key, value, length, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(real4_nd), value, intent(in) :: value
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_object_set_1darray_float_c(x, c_key, c_value, c_length, c_units)
    deallocate(c_key)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_1darray_float: Error setting element"
    end if
  end subroutine generic_object_set_1darray_float
  !> @brief Set an element in a object to a float 1darray
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The array of values to assign to x
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_1darray_double(x, key, value, length, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(real8_nd), value, intent(in) :: value
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_object_set_1darray_double_c(x, c_key, c_value, c_length, c_units)
    deallocate(c_key)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_1darray_double: Error setting element"
    end if
  end subroutine generic_object_set_1darray_double
  !> @brief Set an element in a object to a complex 1darray
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The array of values to assign to x
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_1darray_complex_float(x, key, value, length, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(complex4_nd), value, intent(in) :: value
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_object_set_1darray_complex_float_c(x, c_key, c_value, c_length, c_units)
    deallocate(c_key)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_1darray_complex_float: Error setting element"
    end if
  end subroutine generic_object_set_1darray_complex_float
  !> @brief Set an element in a object to a complex 1darray
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The array of values to assign to x
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_1darray_complex_double(x, key, value, length, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(complex8_nd), value, intent(in) :: value
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_object_set_1darray_complex_double_c(x, c_key, c_value, c_length, c_units)
    deallocate(c_key)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_1darray_complex_double: Error setting element"
    end if
  end subroutine generic_object_set_1darray_complex_double
  !> @brief Set an element in a object to a ndarray
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value Pointer to the memory containing the array to assign
  !>   to x
  !> @param[in] subtype Subtype of data contained in value
  !> @param[in] precision The precision of the elements in value
  !> @param[in] ndim The number of dimensions in value
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_ndarray(x, key, value, subtype, precision, shape, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(c_ptr), value, intent(in) :: value
    character(len = *), intent(in) :: subtype
    integer, value, intent(in) :: precision
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in), optional :: units
    character(kind = c_char), dimension(:), allocatable :: c_key
    character(kind = c_char), dimension(:), allocatable :: c_subtype
    integer(kind = c_size_t) :: c_precision
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_subtype = convert_string_f2c(subtype)
    c_precision = precision
    c_ndim = size(shape)
    c_shape = c_loc(shape(1))
    if (present(units)) then
       c_units = convert_string_f2c(units)
    else
       allocate(c_units(1))
       c_units(1) = c_null_char
    end if
    c_out = generic_object_set_ndarray_c(x, c_key, value, c_subtype, c_precision, c_ndim, c_shape, c_units)
    deallocate(c_key)
    deallocate(c_subtype)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_ndarray: Error setting element"
    end if
  end subroutine generic_object_set_ndarray
  !> @brief Set an element in a object to a int ndarray
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The array of values to assign to x
  !>   in row-major order
  !> @param[in] ndim The number of dimensions in value, or 0 on error
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_ndarray_int16(x, key, value, ndim, shape, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(integer2_nd), value, intent(in) :: value
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_object_set_ndarray_int16_c(x, c_key, c_value, c_ndim, c_shape, c_units)
    deallocate(c_key)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_ndarray_int16: Error setting element"
    end if
  end subroutine generic_object_set_ndarray_int16
  !> @brief Set an element in a object to a int ndarray
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The array of values to assign to x
  !>   in row-major order
  !> @param[in] ndim The number of dimensions in value, or 0 on error
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_ndarray_int32(x, key, value, ndim, shape, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(integer4_nd), value, intent(in) :: value
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_object_set_ndarray_int32_c(x, c_key, c_value, c_ndim, c_shape, c_units)
    deallocate(c_key)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_ndarray_int32: Error setting element"
    end if
  end subroutine generic_object_set_ndarray_int32
  !> @brief Set an element in a object to a int ndarray
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The array of values to assign to x
  !>   in row-major order
  !> @param[in] ndim The number of dimensions in value, or 0 on error
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_ndarray_int64(x, key, value, ndim, shape, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(integer8_nd), value, intent(in) :: value
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_object_set_ndarray_int64_c(x, c_key, c_value, c_ndim, c_shape, c_units)
    deallocate(c_key)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_ndarray_int64: Error setting element"
    end if
  end subroutine generic_object_set_ndarray_int64
  !> @brief Set an element in a object to a float ndarray
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The array of values to assign to x
  !>   in row-major order
  !> @param[in] ndim The number of dimensions in value, or 0 on error
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_ndarray_float(x, key, value, ndim, shape, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(real4_nd), value, intent(in) :: value
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_object_set_ndarray_float_c(x, c_key, c_value, c_ndim, c_shape, c_units)
    deallocate(c_key)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_ndarray_float: Error setting element"
    end if
  end subroutine generic_object_set_ndarray_float
  !> @brief Set an element in a object to a float ndarray
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The array of values to assign to x
  !>   in row-major order
  !> @param[in] ndim The number of dimensions in value, or 0 on error
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_ndarray_double(x, key, value, ndim, shape, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(real8_nd), value, intent(in) :: value
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_object_set_ndarray_double_c(x, c_key, c_value, c_ndim, c_shape, c_units)
    deallocate(c_key)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_ndarray_double: Error setting element"
    end if
  end subroutine generic_object_set_ndarray_double
  !> @brief Set an element in a object to a complex ndarray
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The array of values to assign to x
  !>   in row-major order
  !> @param[in] ndim The number of dimensions in value, or 0 on error
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_ndarray_complex_float(x, key, value, ndim, shape, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(complex4_nd), value, intent(in) :: value
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_object_set_ndarray_complex_float_c(x, c_key, c_value, c_ndim, c_shape, c_units)
    deallocate(c_key)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_ndarray_complex_float: Error setting element"
    end if
  end subroutine generic_object_set_ndarray_complex_float
  !> @brief Set an element in a object to a complex ndarray
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The array of values to assign to x
  !>   in row-major order
  !> @param[in] ndim The number of dimensions in value, or 0 on error
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_ndarray_complex_double(x, key, value, ndim, shape, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(complex8_nd), value, intent(in) :: value
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_object_set_ndarray_complex_double_c(x, c_key, c_value, c_ndim, c_shape, c_units)
    deallocate(c_key)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_ndarray_complex_double: Error setting element"
    end if
  end subroutine generic_object_set_ndarray_complex_double
  !> @brief Set an element in a object to a schema
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_schema(x, key, value)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(ygggeneric), value, intent(in) :: value
    character(kind = c_char), dimension(:), allocatable :: c_key
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_out = generic_object_set_schema_c(x, c_key, value)
    deallocate(c_key)
    if (c_out.lt.0) then
       stop "generic_object_set_schema: Error setting element"
    end if
  end subroutine generic_object_set_schema
  !> @brief Set an element in a object to a any
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The value to assign to x
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_any(x, key, value)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(ygggeneric), value, intent(in) :: value
    character(kind = c_char), dimension(:), allocatable :: c_key
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_out = generic_object_set_any_c(x, c_key, value)
    deallocate(c_key)
    if (c_out.lt.0) then
       stop "generic_object_set_any: Error setting element"
    end if
  end subroutine generic_object_set_any
  !> @brief Get a null from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @returns Value from x
  function generic_object_get_null(x, key) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(yggnull), pointer :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_out
    c_key = convert_string_f2c(key)
    c_out = generic_object_get_null_c(x, c_key)
    deallocate(c_key)
    call c_f_pointer(c_out, out)
  end function generic_object_get_null
  !> @brief Get a boolean from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @returns Value from x
  function generic_object_get_bool(x, key) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    logical :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    logical(kind = c_bool) :: c_out
    c_key = convert_string_f2c(key)
    c_out = generic_object_get_bool_c(x, c_key)
    deallocate(c_key)
    out = c_out
  end function generic_object_get_bool
  !> @brief Get a integer from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @returns Value from x
  function generic_object_get_integer(x, key) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    integer :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_out = generic_object_get_integer_c(x, c_key)
    deallocate(c_key)
    out = c_out
  end function generic_object_get_integer
  !> @brief Get a number from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @returns Value from x
  function generic_object_get_number(x, key) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    real(kind = 8) :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    real(kind = c_double) :: c_out
    c_key = convert_string_f2c(key)
    c_out = generic_object_get_number_c(x, c_key)
    deallocate(c_key)
    out = c_out
  end function generic_object_get_number
  !> @brief Get a string from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @returns Value from x
  function generic_object_get_string(x, key) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    character(len=:), allocatable :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_out
    c_key = convert_string_f2c(key)
    c_out = generic_object_get_string_c(x, c_key)
    deallocate(c_key)
    out = convert_string_c2f(c_out)
  end function generic_object_get_string
  !> @brief Get a item from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @param[in] type Type of item to retrieve
  !> @returns Pointer to data containing raw item data, NULL on error
  function generic_object_get_item(x, key, type) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    character(len = *), intent(in) :: type
    type(c_ptr) :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    character(kind = c_char), dimension(:), allocatable :: c_type
    c_key = convert_string_f2c(key)
    c_type = convert_string_f2c(type)
    out = generic_object_get_item_c(x, c_key, c_type)
    deallocate(c_key)
    deallocate(c_type)
  end function generic_object_get_item
  !> @brief Get a item_nbytes from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @param[in] type Type of item to retrieve
  !> @returns Number of bytes in raw item data, 0 on error
  function generic_object_get_item_nbytes(x, key, type) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    character(len = *), intent(in) :: type
    integer :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    character(kind = c_char), dimension(:), allocatable :: c_type
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_type = convert_string_f2c(type)
    c_out = generic_object_get_item_nbytes_c(x, c_key, c_type)
    deallocate(c_key)
    deallocate(c_type)
    out = c_out
    if (c_out.lt.0) then
       stop "generic_object_get_item_nbytes: Error getting element"
    end if
  end function generic_object_get_item_nbytes
  !> @brief Get a array from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @returns Value from x
  function generic_object_get_array(x, key) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(ygggeneric) :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    c_key = convert_string_f2c(key)
    out = generic_object_get_array_c(x, c_key)
    deallocate(c_key)
  end function generic_object_get_array
  !> @brief Get a object from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @returns Value from x
  function generic_object_get_object(x, key) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(ygggeneric) :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    c_key = convert_string_f2c(key)
    out = generic_object_get_object_c(x, c_key)
    deallocate(c_key)
  end function generic_object_get_object
  !> @brief Get a ply from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @returns Value from x
  function generic_object_get_ply(x, key) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(yggply) :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    c_key = convert_string_f2c(key)
    out = generic_object_get_ply_c(x, c_key)
    deallocate(c_key)
  end function generic_object_get_ply
  !> @brief Get a obj from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @returns Value from x
  function generic_object_get_obj(x, key) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(yggobj) :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    c_key = convert_string_f2c(key)
    out = generic_object_get_obj_c(x, c_key)
    deallocate(c_key)
  end function generic_object_get_obj
  !> @brief Get a class from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @returns Value from x
  function generic_object_get_python_class(x, key) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(yggpython) :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    c_key = convert_string_f2c(key)
    out = generic_object_get_python_class_c(x, c_key)
    deallocate(c_key)
  end function generic_object_get_python_class
  !> @brief Get a function from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @returns Value from x
  function generic_object_get_python_function(x, key) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(yggpython) :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    c_key = convert_string_f2c(key)
    out = generic_object_get_python_function_c(x, c_key)
    deallocate(c_key)
  end function generic_object_get_python_function
  !> @brief Get a instance from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @returns Value from x
  function generic_object_get_python_instance(x, key) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(yggpython) :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    c_key = convert_string_f2c(key)
    out = generic_object_get_python_instance_c(x, c_key)
    deallocate(c_key)
  end function generic_object_get_python_instance
  !> @brief Get a scalar from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @param[in] subtype Subtype of data to return
  !> @param[in] precision Precision of the data to return
  !> @returns Pointer to value in x
  function generic_object_get_scalar(x, key, subtype, precision) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    character(len = *), intent(in) :: subtype
    integer, value, intent(in) :: precision
    type(c_ptr) :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    character(kind = c_char), dimension(:), allocatable :: c_subtype
    integer(kind = c_size_t) :: c_precision
    c_key = convert_string_f2c(key)
    c_subtype = convert_string_f2c(subtype)
    c_precision = precision
    out = generic_object_get_scalar_c(x, c_key, c_subtype, c_precision)
    deallocate(c_key)
    deallocate(c_subtype)
  end function generic_object_get_scalar
  !> @brief Get a int scalar from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @returns Value from x
  function generic_object_get_int16(x, key) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    integer(kind = 2) :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    integer(kind = c_int16_t) :: c_out
    c_key = convert_string_f2c(key)
    c_out = generic_object_get_int16_c(x, c_key)
    deallocate(c_key)
    out = c_out
  end function generic_object_get_int16
  !> @brief Get a int scalar from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @returns Value from x
  function generic_object_get_int32(x, key) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    integer(kind = 4) :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    integer(kind = c_int32_t) :: c_out
    c_key = convert_string_f2c(key)
    c_out = generic_object_get_int32_c(x, c_key)
    deallocate(c_key)
    out = c_out
  end function generic_object_get_int32
  !> @brief Get a int scalar from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @returns Value from x
  function generic_object_get_int64(x, key) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    integer(kind=int64) :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    integer(kind=c_int64_t) :: c_out
    c_key = convert_string_f2c(key)
    c_out = generic_object_get_int64_c(x, c_key)
    deallocate(c_key)
    out = c_out
  end function generic_object_get_int64
  !> @brief Get a float scalar from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @returns Value from x
  function generic_object_get_float(x, key) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    real(kind = 4) :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    real(kind = c_float) :: c_out
    c_key = convert_string_f2c(key)
    c_out = generic_object_get_float_c(x, c_key)
    deallocate(c_key)
    out = c_out
  end function generic_object_get_float
  !> @brief Get a float scalar from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @returns Value from x
  function generic_object_get_double(x, key) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    real(kind = 8) :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    real(kind = c_double) :: c_out
    c_key = convert_string_f2c(key)
    c_out = generic_object_get_double_c(x, c_key)
    deallocate(c_key)
    out = c_out
  end function generic_object_get_double
  !> @brief Get a complex scalar from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @returns Value from x
  function generic_object_get_complex_float(x, key) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    complex(kind = 4) :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(yggcomplex_float) :: c_out
    c_key = convert_string_f2c(key)
    c_out = generic_object_get_complex_float_c(x, c_key)
    deallocate(c_key)
    out = cmplx(c_out%re, c_out%im)
  end function generic_object_get_complex_float
  !> @brief Get a complex scalar from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @returns Value from x
  function generic_object_get_complex_double(x, key) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    complex(kind = 8) :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(yggcomplex_double) :: c_out
    c_key = convert_string_f2c(key)
    c_out = generic_object_get_complex_double_c(x, c_key)
    deallocate(c_key)
    out = cmplx(c_out%re, c_out%im)
  end function generic_object_get_complex_double
  !> @brief Get a 1darray from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @param[in] subtype Subtype of data to return
  !> @param[in] precision Precision of the data to return
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_object_get_1darray(x, key, subtype, precision, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    character(len = *), intent(in) :: subtype
    integer, value, intent(in) :: precision
    type(c_ptr), value :: value
    integer :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    character(kind = c_char), dimension(:), allocatable :: c_subtype
    integer(kind = c_size_t) :: c_precision
    integer(kind = c_size_t) :: c_out
    c_key = convert_string_f2c(key)
    c_subtype = convert_string_f2c(subtype)
    c_precision = precision
    c_out = generic_object_get_1darray_c(x, c_key, c_subtype, c_precision, value)
    deallocate(c_key)
    deallocate(c_subtype)
    out = c_out
  end function generic_object_get_1darray
  !> @brief Get a int 1darray from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_object_get_1darray_int16(x, key, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(integer2_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_key = convert_string_f2c(key)
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_object_get_1darray_int16_c(x, c_key, c_value)
    deallocate(c_key)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_object_get_1darray_int16
  !> @brief Get a int 1darray from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_object_get_1darray_int32(x, key, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(integer4_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_key = convert_string_f2c(key)
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_object_get_1darray_int32_c(x, c_key, c_value)
    deallocate(c_key)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_object_get_1darray_int32
  !> @brief Get a int 1darray from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_object_get_1darray_int64(x, key, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(integer8_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_key = convert_string_f2c(key)
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_object_get_1darray_int64_c(x, c_key, c_value)
    deallocate(c_key)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_object_get_1darray_int64
  !> @brief Get a float 1darray from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_object_get_1darray_float(x, key, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(real4_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_key = convert_string_f2c(key)
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_object_get_1darray_float_c(x, c_key, c_value)
    deallocate(c_key)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_object_get_1darray_float
  !> @brief Get a float 1darray from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_object_get_1darray_double(x, key, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(real8_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_key = convert_string_f2c(key)
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_object_get_1darray_double_c(x, c_key, c_value)
    deallocate(c_key)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_object_get_1darray_double
  !> @brief Get a complex 1darray from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_object_get_1darray_complex_float(x, key, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(complex4_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_key = convert_string_f2c(key)
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_object_get_1darray_complex_float_c(x, c_key, c_value)
    deallocate(c_key)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_object_get_1darray_complex_float
  !> @brief Get a complex 1darray from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_object_get_1darray_complex_double(x, key, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(complex8_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_key = convert_string_f2c(key)
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_object_get_1darray_complex_double_c(x, c_key, c_value)
    deallocate(c_key)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_object_get_1darray_complex_double
  !> @brief Get a ndarray from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @param[in] subtype Subtype of data to return
  !> @param[in] precision Precision of the data to return
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_object_get_ndarray(x, key, subtype, precision, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    character(len = *), intent(in) :: subtype
    integer, value, intent(in) :: precision
    type(c_ptr), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(c_ptr), target :: c_shape_target
    character(kind = c_char), dimension(:), allocatable :: c_key
    character(kind = c_char), dimension(:), allocatable :: c_subtype
    integer(kind = c_size_t) :: c_precision
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_key = convert_string_f2c(key)
    c_subtype = convert_string_f2c(subtype)
    c_precision = precision
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_object_get_ndarray_c(x, c_key, c_subtype, c_precision, value, c_shape)
    deallocate(c_key)
    deallocate(c_subtype)
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_object_get_ndarray
  !> @brief Get a int ndarray from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_object_get_ndarray_int16(x, key, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(integer2_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_key = convert_string_f2c(key)
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_object_get_ndarray_int16_c(x, c_key, c_value, c_shape)
    deallocate(c_key)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_object_get_ndarray_int16
  !> @brief Get a int ndarray from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_object_get_ndarray_int32(x, key, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(integer4_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_key = convert_string_f2c(key)
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_object_get_ndarray_int32_c(x, c_key, c_value, c_shape)
    deallocate(c_key)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_object_get_ndarray_int32
  !> @brief Get a int ndarray from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_object_get_ndarray_int64(x, key, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(integer8_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_key = convert_string_f2c(key)
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_object_get_ndarray_int64_c(x, c_key, c_value, c_shape)
    deallocate(c_key)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_object_get_ndarray_int64
  !> @brief Get a float ndarray from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_object_get_ndarray_float(x, key, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(real4_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_key = convert_string_f2c(key)
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_object_get_ndarray_float_c(x, c_key, c_value, c_shape)
    deallocate(c_key)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_object_get_ndarray_float
  !> @brief Get a float ndarray from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_object_get_ndarray_double(x, key, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(real8_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_key = convert_string_f2c(key)
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_object_get_ndarray_double_c(x, c_key, c_value, c_shape)
    deallocate(c_key)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_object_get_ndarray_double
  !> @brief Get a complex ndarray from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_object_get_ndarray_complex_float(x, key, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(complex4_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_key = convert_string_f2c(key)
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_object_get_ndarray_complex_float_c(x, c_key, c_value, c_shape)
    deallocate(c_key)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_object_get_ndarray_complex_float
  !> @brief Get a complex ndarray from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_object_get_ndarray_complex_double(x, key, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(complex8_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_key = convert_string_f2c(key)
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_object_get_ndarray_complex_double_c(x, c_key, c_value, c_shape)
    deallocate(c_key)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_object_get_ndarray_complex_double
  !> @brief Get a schema from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @returns Value from x
  function generic_object_get_schema(x, key) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(ygggeneric) :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    c_key = convert_string_f2c(key)
    out = generic_object_get_schema_c(x, c_key)
    deallocate(c_key)
  end function generic_object_get_schema
  !> @brief Get a any from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @returns Value from x
  function generic_object_get_any(x, key) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(ygggeneric) :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    c_key = convert_string_f2c(key)
    out = generic_object_get_any_c(x, c_key)
    deallocate(c_key)
  end function generic_object_get_any
#ifdef YGGDRASIL_LONG_DOUBLE_AVAILABLE
  !> @brief Set a given generic item to a float scalar
  !> @param[in] x The generic item to set
  !> @param[in] value The value to assign to x
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_long_double(x, value, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    real(kind = 16), value, intent(in) :: value
    character(len = *), intent(in) :: units
    integer :: out
    real(kind = c_long_double) :: c_value
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_value = value
    c_units = convert_string_f2c(units)
    c_out = generic_set_long_double_c(x, c_value, c_units)
    deallocate(c_units)
    out = c_out
  end function generic_set_long_double
  !> @brief Set a given generic item to a complex scalar
  !> @param[in] x The generic item to set
  !> @param[in] value The value to assign to x
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_complex_long_double(x, value, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    complex(kind = 16), value, intent(in) :: value
    character(len = *), intent(in) :: units
    integer :: out
    type(yggcomplex_long_double) :: c_value
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_value%re = real(value)
    c_value%im = aimag(value)
    c_units = convert_string_f2c(units)
    c_out = generic_set_complex_long_double_c(x, c_value, c_units)
    deallocate(c_units)
    out = c_out
  end function generic_set_complex_long_double
  !> @brief Set a given generic item to a float 1darray
  !> @param[in] x The generic item to set
  !> @param[in] value The array of values to assign to x
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_1darray_long_double(x, value, length, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(real16_nd), value, intent(in) :: value
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    integer :: out
    type(yggptr) :: c_value_int
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_set_1darray_long_double_c(x, c_value, c_length, c_units)
    deallocate(c_units)
    out = c_out
  end function generic_set_1darray_long_double
  !> @brief Set a given generic item to a complex 1darray
  !> @param[in] x The generic item to set
  !> @param[in] value The array of values to assign to x
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_1darray_complex_long_double(x, value, length, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(complex16_nd), value, intent(in) :: value
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    integer :: out
    type(yggptr) :: c_value_int
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_set_1darray_complex_long_double_c(x, c_value, c_length, c_units)
    deallocate(c_units)
    out = c_out
  end function generic_set_1darray_complex_long_double
  !> @brief Set a given generic item to a float ndarray
  !> @param[in] x The generic item to set
  !> @param[in] value The array of values to assign to x
  !>   in row-major order
  !> @param[in] ndim The number of dimensions in value, or 0 on error
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_ndarray_long_double(x, value, ndim, shape, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(real16_nd), value, intent(in) :: value
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    integer :: out
    type(yggptr) :: c_value_int
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_set_ndarray_long_double_c(x, c_value, c_ndim, c_shape, c_units)
    deallocate(c_units)
    out = c_out
  end function generic_set_ndarray_long_double
  !> @brief Set a given generic item to a complex ndarray
  !> @param[in] x The generic item to set
  !> @param[in] value The array of values to assign to x
  !>   in row-major order
  !> @param[in] ndim The number of dimensions in value, or 0 on error
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  function generic_set_ndarray_complex_long_double(x, value, ndim, shape, units) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(complex16_nd), value, intent(in) :: value
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    integer :: out
    type(yggptr) :: c_value_int
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_set_ndarray_complex_long_double_c(x, c_value, c_ndim, c_shape, c_units)
    deallocate(c_units)
    out = c_out
  end function generic_set_ndarray_complex_long_double
  !> @brief Get a float scalar from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @returns Value from x
  function generic_get_long_double(x) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    real(kind = 16) :: out
    real(kind = c_long_double) :: c_out
    c_out = generic_get_long_double_c(x)
    out = c_out
  end function generic_get_long_double
  !> @brief Get a complex scalar from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @returns Value from x
  function generic_get_complex_long_double(x) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    complex(kind = 16) :: out
    type(yggcomplex_long_double) :: c_out
    c_out = generic_get_complex_long_double_c(x)
    out = cmplx(c_out%re, c_out%im)
  end function generic_get_complex_long_double
  !> @brief Get a float 1darray from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_get_1darray_long_double(x, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(real16_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_get_1darray_long_double_c(x, c_value)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_get_1darray_long_double
  !> @brief Get a complex 1darray from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_get_1darray_complex_long_double(x, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(complex16_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_get_1darray_complex_long_double_c(x, c_value)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_get_1darray_complex_long_double
  !> @brief Get a float ndarray from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_get_ndarray_long_double(x, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(real16_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_get_ndarray_long_double_c(x, c_value, c_shape)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_get_ndarray_long_double
  !> @brief Get a complex ndarray from a generic item
  !> @param[in] x Generic item to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_get_ndarray_complex_long_double(x, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    type(complex16_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_get_ndarray_complex_long_double_c(x, c_value, c_shape)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_get_ndarray_complex_long_double
  !> @brief Get a float scalar from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @returns Value from x
  function generic_ref_get_long_double(x) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    real(kind = 16) :: out
    real(kind = c_long_double) :: c_out
    c_out = generic_ref_get_long_double_c(x)
    out = c_out
  end function generic_ref_get_long_double
  !> @brief Get a complex scalar from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @returns Value from x
  function generic_ref_get_complex_long_double(x) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    complex(kind = 16) :: out
    type(yggcomplex_long_double) :: c_out
    c_out = generic_ref_get_complex_long_double_c(x)
    out = cmplx(c_out%re, c_out%im)
  end function generic_ref_get_complex_long_double
  !> @brief Get a float 1darray from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_ref_get_1darray_long_double(x, value) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(real16_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_ref_get_1darray_long_double_c(x, c_value)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_ref_get_1darray_long_double
  !> @brief Get a complex 1darray from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_ref_get_1darray_complex_long_double(x, value) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(complex16_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_ref_get_1darray_complex_long_double_c(x, c_value)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_ref_get_1darray_complex_long_double
  !> @brief Get a float ndarray from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_ref_get_ndarray_long_double(x, value, shape) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(real16_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_ref_get_ndarray_long_double_c(x, c_value, c_shape)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_ref_get_ndarray_long_double
  !> @brief Get a complex ndarray from a generic item reference
  !> @param[in] x Generic item reference to retrieve data from
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_ref_get_ndarray_complex_long_double(x, value, shape) &
       result(out)
    implicit none
    type(ygggenericref), value :: x
    type(complex16_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_ref_get_ndarray_complex_long_double_c(x, c_value, c_shape)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_ref_get_ndarray_complex_long_double
  !> @brief Set an element in a array to a float scalar
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The value to assign to x
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_long_double(x, index, value, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    real(kind = 16), value, intent(in) :: value
    character(len = *), intent(in) :: units
    integer(kind = c_size_t) :: c_index
    real(kind = c_long_double) :: c_value
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value = value
    c_units = convert_string_f2c(units)
    c_out = generic_array_set_long_double_c(x, c_index, c_value, c_units)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_long_double: Error setting element"
    end if
  end subroutine generic_array_set_long_double
  !> @brief Set an element in a array to a complex scalar
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The value to assign to x
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_complex_long_double(x, index, value, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    complex(kind = 16), value, intent(in) :: value
    character(len = *), intent(in) :: units
    integer(kind = c_size_t) :: c_index
    type(yggcomplex_long_double) :: c_value
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value%re = real(value)
    c_value%im = aimag(value)
    c_units = convert_string_f2c(units)
    c_out = generic_array_set_complex_long_double_c(x, c_index, c_value, c_units)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_complex_long_double: Error setting element"
    end if
  end subroutine generic_array_set_complex_long_double
  !> @brief Set an element in a array to a float 1darray
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The array of values to assign to x
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_1darray_long_double(x, index, value, length, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(real16_nd), value, intent(in) :: value
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_array_set_1darray_long_double_c(x, c_index, c_value, c_length, c_units)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_1darray_long_double: Error setting element"
    end if
  end subroutine generic_array_set_1darray_long_double
  !> @brief Set an element in a array to a complex 1darray
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The array of values to assign to x
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_1darray_complex_long_double(x, index, value, length, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(complex16_nd), value, intent(in) :: value
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_array_set_1darray_complex_long_double_c(x, c_index, c_value, c_length, c_units)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_1darray_complex_long_double: Error setting element"
    end if
  end subroutine generic_array_set_1darray_complex_long_double
  !> @brief Set an element in a array to a float ndarray
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The array of values to assign to x
  !>   in row-major order
  !> @param[in] ndim The number of dimensions in value, or 0 on error
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_ndarray_long_double(x, index, value, ndim, shape, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(real16_nd), value, intent(in) :: value
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_array_set_ndarray_long_double_c(x, c_index, c_value, c_ndim, c_shape, c_units)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_ndarray_long_double: Error setting element"
    end if
  end subroutine generic_array_set_ndarray_long_double
  !> @brief Set an element in a array to a complex ndarray
  !> @param[in] x array to set element in
  !> @param[in] index index of element to set
  !> @param[in] value The array of values to assign to x
  !>   in row-major order
  !> @param[in] ndim The number of dimensions in value, or 0 on error
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_array_set_ndarray_complex_long_double(x, index, value, ndim, shape, units)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(complex16_nd), value, intent(in) :: value
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_index = index - 1
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_array_set_ndarray_complex_long_double_c(x, c_index, c_value, c_ndim, c_shape, c_units)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_array_set_ndarray_complex_long_double: Error setting element"
    end if
  end subroutine generic_array_set_ndarray_complex_long_double
  !> @brief Get a float scalar from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @returns Value from x
  function generic_array_get_long_double(x, index) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    real(kind = 16) :: out
    integer(kind = c_size_t) :: c_index
    real(kind = c_long_double) :: c_out
    c_index = index - 1
    c_out = generic_array_get_long_double_c(x, c_index)
    out = c_out
  end function generic_array_get_long_double
  !> @brief Get a complex scalar from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @returns Value from x
  function generic_array_get_complex_long_double(x, index) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    complex(kind = 16) :: out
    integer(kind = c_size_t) :: c_index
    type(yggcomplex_long_double) :: c_out
    c_index = index - 1
    c_out = generic_array_get_complex_long_double_c(x, c_index)
    out = cmplx(c_out%re, c_out%im)
  end function generic_array_get_complex_long_double
  !> @brief Get a float 1darray from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_array_get_1darray_long_double(x, index, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(real16_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_index = index - 1
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_array_get_1darray_long_double_c(x, c_index, c_value)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_array_get_1darray_long_double
  !> @brief Get a complex 1darray from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_array_get_1darray_complex_long_double(x, index, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(complex16_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_index = index - 1
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_array_get_1darray_complex_long_double_c(x, c_index, c_value)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_array_get_1darray_complex_long_double
  !> @brief Get a float ndarray from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_array_get_ndarray_long_double(x, index, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(real16_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_index = index - 1
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_array_get_ndarray_long_double_c(x, c_index, c_value, c_shape)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_array_get_ndarray_long_double
  !> @brief Get a complex ndarray from an element in a array
  !> @param[in] x array to get element from
  !> @param[in] index index of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_array_get_ndarray_complex_long_double(x, index, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    integer, value, intent(in) :: index
    type(complex16_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    integer(kind = c_size_t) :: c_index
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_index = index - 1
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_array_get_ndarray_complex_long_double_c(x, c_index, c_value, c_shape)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_array_get_ndarray_complex_long_double
  !> @brief Set an element in a object to a float scalar
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The value to assign to x
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_long_double(x, key, value, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    real(kind = 16), value, intent(in) :: value
    character(len = *), intent(in) :: units
    character(kind = c_char), dimension(:), allocatable :: c_key
    real(kind = c_long_double) :: c_value
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value = value
    c_units = convert_string_f2c(units)
    c_out = generic_object_set_long_double_c(x, c_key, c_value, c_units)
    deallocate(c_key)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_long_double: Error setting element"
    end if
  end subroutine generic_object_set_long_double
  !> @brief Set an element in a object to a complex scalar
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The value to assign to x
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_complex_long_double(x, key, value, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    complex(kind = 16), value, intent(in) :: value
    character(len = *), intent(in) :: units
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(yggcomplex_long_double) :: c_value
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value%re = real(value)
    c_value%im = aimag(value)
    c_units = convert_string_f2c(units)
    c_out = generic_object_set_complex_long_double_c(x, c_key, c_value, c_units)
    deallocate(c_key)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_complex_long_double: Error setting element"
    end if
  end subroutine generic_object_set_complex_long_double
  !> @brief Set an element in a object to a float 1darray
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The array of values to assign to x
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_1darray_long_double(x, key, value, length, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(real16_nd), value, intent(in) :: value
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_object_set_1darray_long_double_c(x, c_key, c_value, c_length, c_units)
    deallocate(c_key)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_1darray_long_double: Error setting element"
    end if
  end subroutine generic_object_set_1darray_long_double
  !> @brief Set an element in a object to a complex 1darray
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The array of values to assign to x
  !> @param[in] length The number of elements in value
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_1darray_complex_long_double(x, key, value, length, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(complex16_nd), value, intent(in) :: value
    integer, value, intent(in) :: length
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_length
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_length = length
    c_units = convert_string_f2c(units)
    c_out = generic_object_set_1darray_complex_long_double_c(x, c_key, c_value, c_length, c_units)
    deallocate(c_key)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_1darray_complex_long_double: Error setting element"
    end if
  end subroutine generic_object_set_1darray_complex_long_double
  !> @brief Set an element in a object to a float ndarray
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The array of values to assign to x
  !>   in row-major order
  !> @param[in] ndim The number of dimensions in value, or 0 on error
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_ndarray_long_double(x, key, value, ndim, shape, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(real16_nd), value, intent(in) :: value
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_object_set_ndarray_long_double_c(x, c_key, c_value, c_ndim, c_shape, c_units)
    deallocate(c_key)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_ndarray_long_double: Error setting element"
    end if
  end subroutine generic_object_set_ndarray_long_double
  !> @brief Set an element in a object to a complex ndarray
  !> @param[in] x object to set element in
  !> @param[in] key key of element to set
  !> @param[in] value The array of values to assign to x
  !>   in row-major order
  !> @param[in] ndim The number of dimensions in value, or 0 on error
  !> @param[in] shape The size of value in each dimension
  !> @param[in] units Units of value
  !> @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
  subroutine generic_object_set_ndarray_complex_long_double(x, key, value, ndim, shape, units)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(complex16_nd), value, intent(in) :: value
    integer, value, intent(in) :: ndim
    integer(kind=c_size_t), dimension(:), target :: shape
    character(len = *), intent(in) :: units
    type(yggptr) :: c_value_int
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_ndim
    type(c_ptr) :: c_shape
    character(kind = c_char), dimension(:), allocatable :: c_units
    integer(kind = c_int) :: c_out
    c_key = convert_string_f2c(key)
    c_value_int = yggarg(value)
    c_value = c_value_int%ptr
    c_ndim = ndim
    c_shape = c_loc(shape(1))
    c_units = convert_string_f2c(units)
    c_out = generic_object_set_ndarray_complex_long_double_c(x, c_key, c_value, c_ndim, c_shape, c_units)
    deallocate(c_key)
    deallocate(c_units)
    if (c_out.lt.0) then
       stop "generic_object_set_ndarray_complex_long_double: Error setting element"
    end if
  end subroutine generic_object_set_ndarray_complex_long_double
  !> @brief Get a float scalar from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @returns Value from x
  function generic_object_get_long_double(x, key) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    real(kind = 16) :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    real(kind = c_long_double) :: c_out
    c_key = convert_string_f2c(key)
    c_out = generic_object_get_long_double_c(x, c_key)
    deallocate(c_key)
    out = c_out
  end function generic_object_get_long_double
  !> @brief Get a complex scalar from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @returns Value from x
  function generic_object_get_complex_long_double(x, key) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    complex(kind = 16) :: out
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(yggcomplex_long_double) :: c_out
    c_key = convert_string_f2c(key)
    c_out = generic_object_get_complex_long_double_c(x, c_key)
    deallocate(c_key)
    out = cmplx(c_out%re, c_out%im)
  end function generic_object_get_complex_long_double
  !> @brief Get a float 1darray from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_object_get_1darray_long_double(x, key, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(real16_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_key = convert_string_f2c(key)
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_object_get_1darray_long_double_c(x, c_key, c_value)
    deallocate(c_key)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_object_get_1darray_long_double
  !> @brief Get a complex 1darray from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x
  !> @returns Number of elements in the array, or 0 on error
  function generic_object_get_1darray_complex_long_double(x, key, value) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(complex16_nd), value :: value
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    integer(kind = c_size_t) :: c_out
    c_key = convert_string_f2c(key)
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_out = generic_object_get_1darray_complex_long_double_c(x, c_key, c_value)
    deallocate(c_key)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    out = c_out
  end function generic_object_get_1darray_complex_long_double
  !> @brief Get a float ndarray from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_object_get_ndarray_long_double(x, key, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(real16_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_key = convert_string_f2c(key)
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_object_get_ndarray_long_double_c(x, c_key, c_value, c_shape)
    deallocate(c_key)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_object_get_ndarray_long_double
  !> @brief Get a complex ndarray from an element in a object
  !> @param[in] x object to get element from
  !> @param[in] key key of element to get
  !> @param[out] value Pointer to memory that should be reallocated and
  !>   filled with the array contents of x in row-major order
  !> @param[out] shape Pointer to memory that should be reallocated and
  !>   filled with the size of the array in each dimension
  !> @returns Number of dimensions in the array, or 0 on error
  function generic_object_get_ndarray_complex_long_double(x, key, value, shape) &
       result(out)
    implicit none
    type(ygggeneric), value :: x
    character(len = *), intent(in) :: key
    type(complex16_nd), value :: value
    integer(kind=c_size_t), dimension(:), pointer :: shape
    integer :: out
    type(yggptr) :: c_value_int
    logical :: c_value_flag
    type(c_ptr), target :: c_shape_target
    character(kind = c_char), dimension(:), allocatable :: c_key
    type(c_ptr) :: c_value
    type(c_ptr) :: c_shape
    integer(kind = c_size_t) :: c_out
    c_key = convert_string_f2c(key)
    c_value = c_null_ptr
    c_value_int = yggarg(value)
    c_shape_target = c_null_ptr
    c_shape = c_loc(c_shape_target)
    c_out = generic_object_get_ndarray_complex_long_double_c(x, c_key, c_value, c_shape)
    deallocate(c_key)
    c_value_int%ptr = c_value
    c_value_flag = yggptr_c2f(c_value_int, .false.)
    if (.not.c_value_flag) then
       stop "Error recovering fortran pointer for variable"
    end if
    call c_f_pointer(c_shape_target, shape, [c_out])
    out = c_out
  end function generic_object_get_ndarray_complex_long_double
#endif // YGGDRASIL_LONG_DOUBLE_AVAILABLE

end module YggInterface
