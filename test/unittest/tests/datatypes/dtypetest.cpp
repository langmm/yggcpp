#include "../../unittest.hpp"
#include "datatypes/dtype_t.h"


TEST(dtype, GenericContainer) {
  generic_t x_arr = init_generic_array();
  generic_t x_obj = init_generic_map();
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
  }
  ADD_ITEM(0, "1")
#undef ADD_ITEM
  destroy_generic(&x_arr);
  destroy_generic(&x_obj);
}
