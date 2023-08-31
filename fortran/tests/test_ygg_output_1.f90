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
  rComm = init_comm("test_name", 2, 1, rDtype, 131072)
  sComm = ygg_output("test_name")
  if (c_associated(rComm%comm)) then
     if (ygg_send(sComm, data_send, data_send_len)) then
        if (ygg_recv(rComm, data_recv, data_recv_len)) then
           if (data_recv_len.EQ.data_send_len) then
              if (data_recv.EQ.data_send) then
                 r = 0
              end if
           end if
        end if
     end if
  end if
end function test_ygg_output_1
