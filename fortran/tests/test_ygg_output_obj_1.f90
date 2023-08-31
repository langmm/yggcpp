integer function test_ygg_output_obj_1() result(r)
  use YggInterface
  type(yggcomm) :: sComm, rComm
  type(yggdtype) :: rDtype
  type(yggobj) :: data_recv, data_send
  r = 1
  data_send = generate_obj();
  rDtype = create_dtype_from_schema('{"type": "obj"}', .false.)
  rComm = init_comm("test_name", 2, 1, rDtype, 131072)
  sComm = ygg_obj_output("test_name")
  if (.NOT.c_associated(sComm%comm)) then
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
  if (.NOT.compare_obj(data_recv, data_send)) then
     write(*,*) "objects not equal"
     call display_obj(data_send)
     call display_obj(data_recv)
  end if
  r = 0
end function test_ygg_output_obj_1
