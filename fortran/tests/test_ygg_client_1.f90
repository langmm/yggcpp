integer function test_ygg_client_1() result(r)
  use YggInterface
  type(yggcomm) :: sComm, rComm
  type(yggdtype) :: rDtype
  character(len=20) :: data_recv, data_send
  integer :: data_recv_len, data_send_len
  r = 1
  rDtype = create_dtype_from_schema('{"type": "string"}', .false.)
  rComm = init_comm("test_name", 0, 5, rDtype, 163840)
  sComm = ygg_rpc_client("test_name")
  if (.NOT.c_associated(sComm%comm)) then
     write(*,*) "error in comm init"
     return
  end if
  data_send = "REQUEST"
  data_send_len = 7
  data_recv_len = 20
  if (.NOT.ygg_send(sComm, data_send, data_send_len)) then
     write(*,*) "request send failed"
     return
  end if
  if (.NOT.ygg_recv(rComm, data_recv, data_recv_len)) then
     write(*,*) "request recv failed"
     return
  end if
  if (data_recv.NE.data_send) then
     write(*,*) "requests not equal", data_send, data_recv
  end if
  data_send = "RESPONSE"
  data_send_len = 8
  data_recv_len = 20
  if (.NOT.ygg_send(rComm, data_send, data_send_len)) then
     write(*,*) "response send failed"
     return
  end if
  if (.NOT.ygg_recv(sComm, data_recv, data_recv_len)) then
     write(*,*) "response recv failed"
     return
  end if
  if (data_recv.NE.data_send) then
     write(*,*) "responses not equal", data_send, data_recv
  end if
  r = 0
end function test_ygg_client_1
