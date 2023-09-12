integer function test_ygg_output_table_array_realloc_1() result(r)
  use YggInterface
  type(yggcomm) :: sComm, rComm
  type(yggdtype) :: rDtype
  integer(kind=4), dimension(3) :: a_send
  real(kind=8), dimension(3) :: b_send
  character(kind=c_char, len=5), dimension(3) :: c_send
  integer(kind=c_size_t) :: nrow_send, nrow_recv
  type(integer4_1d) :: a_recv
  type(real8_1d) :: b_recv
  type(character_1d) :: c_recv
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
  nrow_recv = 0
  rDtype = create_dtype_format('%d\t%lf\t%5s', .true., .false.)
  rComm = init_comm("test_name", 2, 1, rDtype, 131072)
  sComm = ygg_ascii_array_output("test_name", '%d\t%lf\t%5s')
  if (.NOT.c_associated(sComm%comm)) then
     write(*,*) "error in comm init"
     return
  end if
  if (.NOT.ygg_send(sComm, [yggarg(nrow_send), yggarg(a_send), &
       yggarg(b_send), yggarg(c_send)])) then
     write(*,*) "send failed"
     return
  end if
  if (.NOT.ygg_recv_realloc(rComm, [yggarg(nrow_recv), yggarg(a_recv), &
       yggarg(b_recv), yggarg(c_recv)])) then
     write(*,*) "recv failed"
     return
  end if
  if (.NOT.((nrow_recv.EQ.nrow_send).AND. &
       (size(a_recv%x).EQ.size(a_send)).AND. &
       (size(b_recv%x).EQ.size(b_send)).AND. &
       (size(c_recv%x).EQ.size(c_send)))) then
     write(*,*) "sizes not equal"
     return
  end if
  r = 0
  do i = 1, size(a_send)
     if (a_recv%x(i).NE.a_send(i)) then
        r = 1
        write (*,*) "a not equal", i, a_send(i), a_recv%x(i)
     end if
     if (b_recv%x(i).NE.b_send(i)) then
        r = 1
        write (*,*) "b not equal", i, b_send(i), b_recv%x(i)
     end if
     if (size(c_recv%x(i)%x).NE.len(c_send(i))) then
        r = 1
        write (*,*) "c size not equal", size(c_recv%x(i)%x), &
             len(c_send(i))
     else
        do j = 1, len(c_send(i))
           if (c_recv%x(i)%x(j).NE.c_send(i)(j:j)) then
              r = 1
              write (*,*) "c not equal", i, c_send(i), c_recv%x(i)%x
           end if
        end do
     end if
  end do
end function test_ygg_output_table_array_realloc_1
