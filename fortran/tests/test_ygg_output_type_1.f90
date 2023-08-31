integer function test_ygg_output_type_1() result(r)
  use YggInterface
  type(yggcomm) :: sComm, rComm
  type(yggdtype) :: sDtype, rDtype
  integer(kind=8) :: data_recv, data_send
  r = 1
  data_send = 5
  data_recv = 0
  rDtype = create_dtype_from_schema('{"type": "integer"}', .false.)
  sDtype = create_dtype_from_schema('{"type": "integer"}', .false.)
  rComm = init_comm("test_name", 2, 1, rDtype, 131072)
  sComm = ygg_output("test_name", sDtype)
  if (c_associated(rComm%comm)) then
     if (ygg_send(sComm, yggarg(data_send))) then
        if (ygg_recv(rComm, yggarg(data_recv))) then
           if (data_recv.EQ.data_send) then
              r = 0
           end if
        end if
     end if
  end if
end function test_ygg_output_type_1
