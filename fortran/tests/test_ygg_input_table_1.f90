integer function test_ygg_input_table_1() result(r)
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
  sComm = init_comm("test_name", SEND, DEFAULT_COMM, sDtype, &
       COMM_FLAG_SET_OPP_ENV)
  rComm = ygg_ascii_table_input("test_name")
  if (.NOT.c_associated(rComm%comm)) then
     write(*,*) "error in comm init"
     return
  end if
  if (.NOT.ygg_send(sComm, [yggarg(a_send), yggarg(b_send), &
       yggarg(c_send)])) then
     write(*,*) "send failed"
     return
  end if
  if (.NOT.ygg_recv(rComm, [yggarg(a_recv), yggarg(b_recv), &
       yggarg(c_recv)])) then
     write(*,*) "recv failed"
     return
  end if
  if (a_recv.NE.a_send) then
     write(*,*) "a not equal", a_send, a_recv
     return
  end if
  if (b_recv.NE.b_send) then
     write(*,*) "b not equal", b_send, b_recv
     return
  end if
  if (c_recv.NE.c_send) then
     write(*,*) "c not equal", c_send, c_recv
     return
  end if
  r = 0
end function test_ygg_input_table_1
