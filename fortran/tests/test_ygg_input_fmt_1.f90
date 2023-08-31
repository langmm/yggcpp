integer function test_ygg_input_fmt_1() result(r)
  use YggInterface
  type(yggcomm) :: sComm, rComm
  type(yggdtype) :: sDtype
  integer(kind=8) :: a_send, a_recv
  real(kind=8) :: b_send, b_recv
  character(len=5) :: c_send, c_recv
  r = 1
  a_send = 5
  a_recv = 0
  b_send = 1.5
  b_recv = 0.0
  c_send = 'hello'
  c_recv = ''
  sDtype = create_dtype_format('%d\t%lf\t%5s', .false., .false.)
  sComm = init_comm("test_name", 0, 1, sDtype, 131072)
  rComm = ygg_input("test_name", '%d\t%lf\t%5s')
  if (c_associated(rComm%comm)) then
     if (ygg_send(sComm, [yggarg(a_send), yggarg(b_send), &
          yggarg(c_send)])) then
        if (ygg_recv(rComm, [yggarg(a_recv), yggarg(b_recv), &
             yggarg(c_recv)])) then
           if (a_recv.EQ.a_send) then
              if (b_recv.EQ.b_send) then
                 if (c_recv.EQ.c_send) then
                    r = 0
                 end if
              end if
           end if
        end if
     end if
  end if
end function test_ygg_input_fmt_1
