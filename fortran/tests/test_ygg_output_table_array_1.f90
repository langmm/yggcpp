integer function test_ygg_output_table_array_1() result(r)
  use YggInterface
  type(yggcomm) :: sComm, rComm
  type(yggdtype) :: rDtype
  integer(kind=4), dimension(3) :: a_send, a_recv
  real(kind=8), dimension(3) :: b_send, b_recv
  character(kind=c_char, len=5), dimension(3) :: c_send, c_recv
  integer(kind=c_size_t) :: nrow_send, nrow_recv
  integer :: i
  r = 1
  a_send(1) = 0
  a_send(2) = 1
  a_send(3) = 2
  b_send(1) = 0.0
  b_send(2) = 1.0
  b_send(3) = 2.0
  c_send(1) = 'abcd'
  c_send(2) = 'bcde'
  c_send(3) = 'cdef'
  nrow_send = 3
  nrow_recv = 3
  rDtype = create_dtype_format('%d\t%lf\t%5s', .true., .false.)
  rComm = init_comm("test_name", 2, 1, rDtype, 131072)
  sComm = ygg_ascii_array_output("test_name", '%d\t%lf\t%5s')
  if (c_associated(sComm%comm)) then
     if (ygg_send(sComm, [yggarg(nrow_send), yggarg(a_send), &
          yggarg(b_send), yggarg(c_send)])) then
        if (ygg_recv(rComm, [yggarg(nrow_recv), yggarg(a_recv), &
             yggarg(b_recv), yggarg(c_recv)])) then
           r = 0
           do i = 1, size(a_send)
              if (a_recv(i).NE.a_send(i)) then
                 r = 1
                 write (*,*) "a not equal", i, a_send(i), a_recv(i)
              end if
              if (b_recv(i).NE.b_send(i)) then
                 r = 1
                 write (*,*) "b not equal", i, b_send(i), b_recv(i)
              end if
              if ((c_recv(i)).NE.c_send(i)) then
                 r = 1
                 write (*,*) "c not equal", i, c_send(i), c_recv(i)
              end if
           end do
        end if
     end if
  end if
end function test_ygg_output_table_array_1
