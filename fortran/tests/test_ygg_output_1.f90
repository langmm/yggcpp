integer function test_ygg_output_1() result(r)
  use YggInterface
  type(yggcomm) :: sComm, rComm
  type(yggdtype) :: rDtype
  character(len=20) :: data_recv, data_send
  integer :: data_recv_len, data_send_len
  r = 1
  data_send = "Test message"
  data_send_len = 12
  data_recv_len = 20
  rDtype = create_dtype_from_schema('{"type": "string"}', .false.)
  rComm = init_comm("test_name", RECV, DEFAULT_COMM, rDtype, &
       COMM_FLAG_SET_OPP_ENV)
  sComm = ygg_output("test_name")
  if (.NOT.c_associated(rComm%comm)) then
     write(*,*) "error in comm init"
     return
  end if
  if (.NOT.ygg_send(sComm, data_send, data_send_len)) then
     write(*,*) "send failed"
     return
  end if
  if (.NOT.ygg_recv(rComm, data_recv, data_recv_len)) then
     write(*,*) "recv failed"
     return
  end if
  if (data_recv_len.NE.data_send_len) then
     write(*,*) "data size not equal", data_send_len, data_recv_len
     return
  end if
  if (data_recv.NE.data_send) then
     write(*,*) "data not equal", data_send, data_recv
     return
  end if
  r = 0
end function test_ygg_output_1
