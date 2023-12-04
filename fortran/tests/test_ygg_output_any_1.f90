integer function test_ygg_output_any_1() result(r)
  use YggInterface
  type(yggcomm) :: sComm, rComm
  type(yggdtype) :: rDtype
  type(ygggeneric) :: data_recv, data_send
  r = 1
  data_send = init_generic_generate('{"type": "ply"}')
  rDtype = create_dtype_from_schema('{"type": "any"}', .true.)
  rComm = init_comm("test_name", RECV, DEFAULT_COMM, rDtype, &
       COMM_FLAG_SET_OPP_ENV)
  sComm = ygg_any_output("test_name")
  if (.NOT.c_associated(sComm%comm)) then
     write(*,*) "comm init failed"
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
  if (.NOT.compare_generic(data_recv, data_send)) then
     write(*,*) "objects not equal"
     call display_generic(data_send)
     call display_generic(data_recv)
  end if
  r = 0
end function test_ygg_output_any_1
