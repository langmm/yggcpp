integer function test_ygg_input_json_array_1() result(r)
  use YggInterface
  type(yggcomm) :: sComm, rComm
  type(yggdtype) :: sDtype
  type(ygggeneric) :: data_recv, data_send
  r = 1
  data_send = init_generic_generate('{"type": "array", &
       &"items": [{"type": "integer"}, {"type": "string"}]}')
  sDtype = create_dtype_from_schema('{"type": "any"}', .true.)
  sComm = init_comm("test_name", 0, 1, sDtype, 131072)
  rComm = ygg_json_array_input("test_name")
  if (.NOT.c_associated(rComm%comm)) then
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
end function test_ygg_input_json_array_1
