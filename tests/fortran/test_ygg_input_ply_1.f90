integer function test_ygg_input_ply_1() result(r)
  use YggInterface
  type(yggcomm) :: sComm, rComm
  type(yggdtype) :: sDtype
  type(yggply) :: data_recv, data_send
  r = 1
  data_send = generate_ply();
  sDtype = create_dtype_from_schema('{"type": "ply"}', .false.)
  sComm = init_comm("test_name", SEND, DEFAULT_COMM, sDtype, &
       IOR(COMM_FLAG_ASYNC, COMM_FLAG_SET_OPP_ENV))
  rComm = ygg_ply_input("test_name")
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
  if (.NOT.compare_ply(data_recv, data_send)) then
     write(*,*) "objects not equal"
     call display_ply(data_send)
     call display_ply(data_recv)
  end if
  r = 0
end function test_ygg_input_ply_1
