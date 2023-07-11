#include "../../unittest.hpp"
#include "datatypes/dtype_t.h"


TEST(generic_t, Container) {
  generic_t x_arr = init_generic_array();
  generic_t x_obj = init_generic_map();
  EXPECT_TRUE(is_generic_init(x_arr));
  EXPECT_TRUE(is_generic_init(x_obj));
#define ADD_ITEM(idx, schema)						\
  {									\
    generic_t v = init_generic_json(schema);				\
    EXPECT_EQ(set_generic_array(x_arr, idx, v), 0);			\
    EXPECT_EQ(set_generic_map(x_obj, #idx, v), 0);			\
    {									\
      generic_t v_cpy;							\
      generic_ref_t v_ref;						\
      EXPECT_EQ(get_generic_array(x_arr, idx, &v_cpy), 0);		\
      EXPECT_EQ(get_generic_array_ref(x_arr, idx, &v_ref), 0);		\
      display_generic(v);						\
      display_generic(v_cpy);						\
      EXPECT_TRUE(compare_generic(v, v_cpy));				\
      destroy_generic(&v_cpy);						\
    }									\
    {									\
      generic_t v_cpy;							\
      generic_ref_t v_ref;						\
      EXPECT_EQ(get_generic_map(x_obj, #idx, &v_cpy), 0);		\
      EXPECT_EQ(get_generic_map_ref(x_obj, #idx, &v_ref), 0);		\
      display_generic(v);						\
      display_generic(v_cpy);						\
      EXPECT_TRUE(compare_generic(v, v_cpy));				\
      destroy_generic(&v_cpy);						\
    }									\
    destroy_generic(&v);						\
    EXPECT_EQ(generic_array_get_size(x_arr), idx + 1);			\
    EXPECT_EQ(generic_map_get_size(x_obj), idx + 1);			\
    EXPECT_EQ(generic_map_has_key(x_obj, #idx), 1);			\
    char** keys = NULL;							\
    EXPECT_EQ(generic_map_get_keys(x_obj, &keys), idx + 1);		\
    EXPECT_EQ(strcmp(keys[idx], #idx), 0);				\
  }
  ADD_ITEM(0, "1")
  ADD_ITEM(0, "2")
#undef ADD_ITEM
  destroy_generic(&x_arr);
  destroy_generic(&x_obj);
}


TEST(generic_t, ContainerErrors) {
  char** keys = NULL;
  {
    // Empty object
    generic_t x = init_generic();
    EXPECT_EQ(generic_array_get_size(x), 0);
    EXPECT_EQ(generic_map_get_size(x), 0);
    EXPECT_EQ(generic_map_has_key(x, "x"), 0);
    EXPECT_EQ(generic_map_get_keys(x, &keys), 0);
  }
  {
    // NULL object
    generic_t x = init_generic_null();
    EXPECT_EQ(generic_array_get_size(x), 0);
    EXPECT_EQ(generic_map_get_size(x), 0);
    EXPECT_EQ(generic_map_has_key(x, "x"), 0);
    EXPECT_EQ(generic_map_get_keys(x, &keys), 0);
  }
}

#define STANDARD_TEST(name, type, value)				\
  TEST(generic_t, name) {						\
    generic_t x_arr = init_generic_array();				\
    generic_t x_obj = init_generic_map();				\
    type data = value;							\
    EXPECT_EQ(generic_array_set_ ## name(x_arr, 0, data), 0);		\
    EXPECT_EQ(generic_array_get_ ## name(x_arr, 0), data);		\
    EXPECT_EQ(generic_map_set_ ## name(x_obj, "x", data), 0);		\
    EXPECT_EQ(generic_map_get_ ## name(x_obj, "x"), data);		\
    destroy_generic(&x_arr);						\
    destroy_generic(&x_obj);						\
  }
STANDARD_TEST(bool, bool, true)
STANDARD_TEST(integer, int, 5)
STANDARD_TEST(null, void*, NULL)
STANDARD_TEST(number, double, 5.5)
#undef STANDARD_TEST
  
#define SCALAR_TEST(name, type, value)					\
  TEST(generic_t, name) {						\
    generic_t x_arr = init_generic_array();				\
    generic_t x_obj = init_generic_map();				\
    type data = value;							\
    EXPECT_EQ(generic_array_set_ ## name(x_arr, 0, data, "cm"), 0);	\
    EXPECT_EQ(generic_array_get_ ## name(x_arr, 0), data);		\
    EXPECT_EQ(generic_map_set_ ## name(x_obj, "x", data, "cm"), 0);	\
    EXPECT_EQ(generic_map_get_ ## name(x_obj, "x"), data);		\
    destroy_generic(&x_arr);						\
    destroy_generic(&x_obj);						\
  }
#define COMPLEX_TEST(name, type, value)					\
  TEST(generic_t, name) {						\
    generic_t x_arr = init_generic_array();				\
    generic_t x_obj = init_generic_map();				\
    type data = {value, value};						\
    EXPECT_EQ(generic_array_set_ ## name(x_arr, 0, data, "cm"), 0);	\
    EXPECT_EQ(generic_array_get_ ## name(x_arr, 0), data);		\
    EXPECT_EQ(generic_map_set_ ## name(x_obj, "x", data, "cm"), 0);	\
    EXPECT_EQ(generic_map_get_ ## name(x_obj, "x"), data);		\
    destroy_generic(&x_arr);						\
    destroy_generic(&x_obj);						\
  }
SCALAR_TEST(int8, int8_t, 3)
SCALAR_TEST(int16, int16_t, 3)
SCALAR_TEST(int32, int32_t, 3)
SCALAR_TEST(int64, int64_t, 3)
SCALAR_TEST(uint8, uint8_t, 3)
SCALAR_TEST(uint16, uint16_t, 3)
SCALAR_TEST(uint32, uint32_t, 3)
SCALAR_TEST(uint64, uint64_t, 3)
SCALAR_TEST(float, float, 3.3f)
SCALAR_TEST(double, double, 3.3)
COMPLEX_TEST(complex_float, complex_float_t, 1.2f)
COMPLEX_TEST(complex_double, complex_double_t, 1.2)
#ifdef YGGDRASIL_LONG_DOUBLE_AVAILABLE
SCALAR_TEST(long_double, long double, 3.3l)
COMPLEX_TEST(complex_long_double, complex_long_double_t, 3.3l)
#endif // YGGDRASIL_LONG_DOUBLE_AVAILABLE
#undef SCALAR_TEST
#undef COMPLEX_TEST

// TODO:
// - Python (destroy_python, copy_python, display_python, init_python_API)
// - dtype (is_empty_dtype, is_dtype_format_array, dtype_name, dtype_subtype, dtype_precision, set_dtype_name, complete_dtype, destroy_dtype, create_dtype_direct, create_dtype_scalar, create_dtype_1darray, create_dtype_ndarray_arr, copy_dtype, dtype_uses_generic, display_dtype)
// - geom (set_obj, free_obj, copy_obj, display_obj, nelements_obj, set_ply, free_ply, copy_ply, display_ply, nelements_ply)
