
module example
  
  use iso_c_binding
  use YggInterface
  implicit none

contains
  
  logical(kind=c_bool) function example_model_function(data_send, data_recv) &
       result(out) bind(c)
    type(ygggeneric), value, intent(in) :: data_send
    type(ygggeneric), value :: data_recv
    character(len=:), allocatable :: data
    data = generic_get_string(data_send)
    out = (generic_set_integer(data_recv, len_trim(data)).eq.0)
  end function example_model_function
  
end module example

integer function test_ygg_function_1() result(r)
  use YggInterface
  use example
  type(yggcomm) :: sComm, rComm
  type(yggdtype) :: sDtype
  character(len=5) :: data_send
  integer :: data_recv
  call register_function_wrapper( &
       "example_model_function", example_model_function)
  r = 1
  data_send = "alpha"
  data_recv = 0
  sDtype = create_dtype_from_schema('{"type": "string"}', .false.)
  sComm = init_comm("test_name", SEND, FUNCTION_COMM, sDtype, &
       IOR(COMM_FLAG_ASYNC, COMM_FLAG_SET_OPP_ENV))
  rComm = ygg_input("test_name")
  if (.NOT.c_associated(rComm%comm)) then
     write(*,*) "error in comm init"
     return
  end if
  if (.NOT.ygg_send(sComm, yggarg(data_send))) then
     write(*,*) "send failed"
     return
  end if
  if (.NOT.ygg_recv(rComm, yggarg(data_recv))) then
     write(*,*) "recv failed"
     return
  end if
  if (data_recv.NE.5) then
     write(*,*) "data not equal", data_send, data_recv
     return
  end if
  r = 0
end function test_ygg_function_1
