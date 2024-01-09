integer function test_ygg_input_type_1() result(r)
  use YggInterface
  type(yggcomm) :: sComm, rComm
  type(yggdtype) :: sDtype, rDtype
  integer(kind=8) :: data_recv, data_send
  r = 1
  data_send = 5
  data_recv = 0
  sDtype = create_dtype_from_schema('{"type": "integer"}', .false.)
  rDtype = create_dtype_from_schema('{"type": "integer"}', .false.)
  sComm = init_comm("test_name", SEND, DEFAULT_COMM, sDtype, &
       IOR(COMM_FLAG_ASYNC, COMM_FLAG_SET_OPP_ENV))
  call display_dtype(sDtype)
  rComm = ygg_input("test_name", rDtype)
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
  if (data_recv.NE.data_send) then
     write(*,*) "data not equal", data_send, data_recv
     return
  end if
  r = 0
end function test_ygg_input_type_1
