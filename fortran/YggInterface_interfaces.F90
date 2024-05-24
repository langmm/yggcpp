
  ! LINES AFTER THIS WERE GENERATED AND SHOULD NOT BE MODIFIED DIRECTLY
  !====================================================================
#ifndef DOXYGEN_SHOULD_SKIP_THIS
  
    !> @brief Get an element from an array.
    !> @param[in] x Generic object that is presumed to contain an array.
    !> @param[in] index Index of element to return.
    !> @param[out] out Variable to store element in. For ND arrays
    !>   this should be a reallocatable type of the form \<type\>\<precision\>_nd
    !>   (e.g. integer2_nd, real4_nd)
    interface generic_array_get
       module procedure generic_array_get_null
       module procedure generic_array_get_bool
       module procedure generic_array_get_integer
       module procedure generic_array_get_number
       module procedure generic_array_get_string
       module procedure generic_array_get_array
       module procedure generic_array_get_object
       module procedure generic_array_get_ply
       module procedure generic_array_get_obj
       module procedure generic_array_get_python_class
       module procedure generic_array_get_python_function
       module procedure generic_array_get_python_instance
       module procedure generic_array_get_scalar
       module procedure generic_array_get_int16
       module procedure generic_array_get_int32
       module procedure generic_array_get_int64
       module procedure generic_array_get_float
       module procedure generic_array_get_double
       module procedure generic_array_get_complex_float
       module procedure generic_array_get_complex_double
       module procedure generic_array_get_1darray
       module procedure generic_array_get_1darray_int16
       module procedure generic_array_get_1darray_int32
       module procedure generic_array_get_1darray_int64
       module procedure generic_array_get_1darray_float
       module procedure generic_array_get_1darray_double
       module procedure generic_array_get_1darray_complex_float
       module procedure generic_array_get_1darray_complex_double
       module procedure generic_array_get_ndarray
       module procedure generic_array_get_ndarray_int16
       module procedure generic_array_get_ndarray_int32
       module procedure generic_array_get_ndarray_int64
       module procedure generic_array_get_ndarray_float
       module procedure generic_array_get_ndarray_double
       module procedure generic_array_get_ndarray_complex_float
       module procedure generic_array_get_ndarray_complex_double
       module procedure generic_array_get_schema
       module procedure generic_array_get_any
       module procedure generic_array_get_long_double
       module procedure generic_array_get_complex_long_double
       module procedure generic_array_get_1darray_long_double
       module procedure generic_array_get_1darray_complex_long_double
       module procedure generic_array_get_ndarray_long_double
       module procedure generic_array_get_ndarray_complex_long_double
    end interface generic_array_get
  
    !> @brief Get an item from a map.
    !> @param[in] x Generic object that is presumed to contain a map.
    !> @param[in] key Key for item in the map that should be returned.
    !> @param[in] out Variable to store the item in.
    interface generic_object_get
       module procedure generic_object_get_null
       module procedure generic_object_get_bool
       module procedure generic_object_get_integer
       module procedure generic_object_get_number
       module procedure generic_object_get_string
       module procedure generic_object_get_array
       module procedure generic_object_get_object
       module procedure generic_object_get_ply
       module procedure generic_object_get_obj
       module procedure generic_object_get_python_class
       module procedure generic_object_get_python_function
       module procedure generic_object_get_python_instance
       module procedure generic_object_get_scalar
       module procedure generic_object_get_int16
       module procedure generic_object_get_int32
       module procedure generic_object_get_int64
       module procedure generic_object_get_float
       module procedure generic_object_get_double
       module procedure generic_object_get_complex_float
       module procedure generic_object_get_complex_double
       module procedure generic_object_get_1darray
       module procedure generic_object_get_1darray_int16
       module procedure generic_object_get_1darray_int32
       module procedure generic_object_get_1darray_int64
       module procedure generic_object_get_1darray_float
       module procedure generic_object_get_1darray_double
       module procedure generic_object_get_1darray_complex_float
       module procedure generic_object_get_1darray_complex_double
       module procedure generic_object_get_ndarray
       module procedure generic_object_get_ndarray_int16
       module procedure generic_object_get_ndarray_int32
       module procedure generic_object_get_ndarray_int64
       module procedure generic_object_get_ndarray_float
       module procedure generic_object_get_ndarray_double
       module procedure generic_object_get_ndarray_complex_float
       module procedure generic_object_get_ndarray_complex_double
       module procedure generic_object_get_schema
       module procedure generic_object_get_any
       module procedure generic_object_get_long_double
       module procedure generic_object_get_complex_long_double
       module procedure generic_object_get_1darray_long_double
       module procedure generic_object_get_1darray_complex_long_double
       module procedure generic_object_get_ndarray_long_double
       module procedure generic_object_get_ndarray_complex_long_double
    end interface generic_object_get
  
    !> @brief Set an element in an array.
    !> @param[in] x Generic object that is presumed to contain an array.
    !> @param[in] index Index for element that should be set.
    !> @param[in] val Variable containing value for the element.
    !> @param[in] units Optional units for scalars, 1D, & ND arrays.
    interface generic_array_set
       module procedure generic_array_set_null
       module procedure generic_array_set_bool
       module procedure generic_array_set_integer
       module procedure generic_array_set_number
       module procedure generic_array_set_string
       module procedure generic_array_set_item
       module procedure generic_array_set_array
       module procedure generic_array_set_object
       module procedure generic_array_set_ply
       module procedure generic_array_set_obj
       module procedure generic_array_set_python_class
       module procedure generic_array_set_python_function
       module procedure generic_array_set_python_instance
       module procedure generic_array_set_scalar
       module procedure generic_array_set_int16
       module procedure generic_array_set_int32
       module procedure generic_array_set_int64
       module procedure generic_array_set_float
       module procedure generic_array_set_double
       module procedure generic_array_set_complex_float
       module procedure generic_array_set_complex_double
       module procedure generic_array_set_1darray
       module procedure generic_array_set_1darray_int16
       module procedure generic_array_set_1darray_int32
       module procedure generic_array_set_1darray_int64
       module procedure generic_array_set_1darray_float
       module procedure generic_array_set_1darray_double
       module procedure generic_array_set_1darray_complex_float
       module procedure generic_array_set_1darray_complex_double
       module procedure generic_array_set_ndarray
       module procedure generic_array_set_ndarray_int16
       module procedure generic_array_set_ndarray_int32
       module procedure generic_array_set_ndarray_int64
       module procedure generic_array_set_ndarray_float
       module procedure generic_array_set_ndarray_double
       module procedure generic_array_set_ndarray_complex_float
       module procedure generic_array_set_ndarray_complex_double
       module procedure generic_array_set_schema
       module procedure generic_array_set_any
       module procedure generic_array_set_long_double
       module procedure generic_array_set_complex_long_double
       module procedure generic_array_set_1darray_long_double
       module procedure generic_array_set_1darray_complex_long_double
       module procedure generic_array_set_ndarray_long_double
       module procedure generic_array_set_ndarray_complex_long_double
    end interface generic_array_set
  
    !> @brief Set an item in a map.
    !> @param[in] x Generic object that is presumed to contain a map.
    !> @param[in] key Key string for item that should be set.
    !> @param[in] val Value to assign to the item.
    !> @param[in] units Units to assign to the item for scalars, 1D, & ND
    !>   arrays.
    interface generic_object_set
       module procedure generic_object_set_null
       module procedure generic_object_set_bool
       module procedure generic_object_set_integer
       module procedure generic_object_set_number
       module procedure generic_object_set_string
       module procedure generic_object_set_item
       module procedure generic_object_set_array
       module procedure generic_object_set_object
       module procedure generic_object_set_ply
       module procedure generic_object_set_obj
       module procedure generic_object_set_python_class
       module procedure generic_object_set_python_function
       module procedure generic_object_set_python_instance
       module procedure generic_object_set_scalar
       module procedure generic_object_set_int16
       module procedure generic_object_set_int32
       module procedure generic_object_set_int64
       module procedure generic_object_set_float
       module procedure generic_object_set_double
       module procedure generic_object_set_complex_float
       module procedure generic_object_set_complex_double
       module procedure generic_object_set_1darray
       module procedure generic_object_set_1darray_int16
       module procedure generic_object_set_1darray_int32
       module procedure generic_object_set_1darray_int64
       module procedure generic_object_set_1darray_float
       module procedure generic_object_set_1darray_double
       module procedure generic_object_set_1darray_complex_float
       module procedure generic_object_set_1darray_complex_double
       module procedure generic_object_set_ndarray
       module procedure generic_object_set_ndarray_int16
       module procedure generic_object_set_ndarray_int32
       module procedure generic_object_set_ndarray_int64
       module procedure generic_object_set_ndarray_float
       module procedure generic_object_set_ndarray_double
       module procedure generic_object_set_ndarray_complex_float
       module procedure generic_object_set_ndarray_complex_double
       module procedure generic_object_set_schema
       module procedure generic_object_set_any
       module procedure generic_object_set_long_double
       module procedure generic_object_set_complex_long_double
       module procedure generic_object_set_1darray_long_double
       module procedure generic_object_set_1darray_complex_long_double
       module procedure generic_object_set_ndarray_long_double
       module procedure generic_object_set_ndarray_complex_long_double
    end interface generic_object_set

end module YggInterface

#endif