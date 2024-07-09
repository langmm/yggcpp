module example_fortran
  
  use iso_c_binding
  use YggInterface
  implicit none

contains
  
  logical(kind=c_bool) function example_model_function(data_send, data_recv) &
       result(out) bind(c)
    type(ygggeneric), value, intent(in) :: data_send
    type(ygggeneric), value :: data_recv
    character(len=:), allocatable :: data
    write(*, *) "in example_model_function 1"
    data = generic_get_string(data_send)
    write(*, *) "in example_model_function 2"
    out = (generic_set_integer(data_recv, len_trim(data)).eq.0)
    write(*, *) "in example_model_function 3", out
  end function example_model_function
  
end module example_fortran
