#include "../../unittest.hpp"
#include "datatypes/dtype_t.h"
#include "utils/serialization.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"


TEST(generic_t, Container) {
  generic_t x_arr = init_generic_array();
  generic_t x_obj = init_generic_map();
  EXPECT_TRUE(is_generic_init(x_arr));
  EXPECT_TRUE(is_generic_init(x_obj));
  {
    generic_t v;
    generic_ref_t v_ref;
    EXPECT_EQ(get_generic_array(x_arr, 0, &v), -1);
    EXPECT_EQ(get_generic_array_ref(x_arr, 0, &v_ref), -1);
    EXPECT_EQ(get_generic_map(x_obj, "x", &v), -1);
    EXPECT_EQ(get_generic_map_ref(x_obj, "x", &v_ref), -1);
  }
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
    EXPECT_FALSE(compare_generic(v, v));				\
  }
  ADD_ITEM(0, "1")
  ADD_ITEM(0, "2")
  generic_t v_fin = init_generic_json("1");
  EXPECT_EQ(add_generic_array(x_arr, v_fin), 0);
#undef ADD_ITEM
  destroy_generic(&x_arr);
  destroy_generic(&x_obj);
  destroy_generic(&v_fin);
}


TEST(generic_t, ContainerErrors) {
  char** keys = NULL;
  {
    // Empty object
    generic_t x = init_generic();
    generic_ref_t x_ref = init_generic_ref(x);
    generic_t v = init_generic();
    generic_t w = init_generic();
    generic_ref_t w_ref;
    EXPECT_EQ(generic_array_get_size(x), 0);
    EXPECT_EQ(generic_map_get_size(x), 0);
    EXPECT_EQ(generic_map_has_key(x, "x"), 0);
    EXPECT_EQ(generic_map_get_keys(x, &keys), 0);
    EXPECT_EQ(set_generic_array(x, 0, v), -1);
    EXPECT_EQ(set_generic_map(x, "x", v), -1);
    EXPECT_EQ(add_generic_array(x, v), -1);
    EXPECT_EQ(get_generic_array(x, 0, &w), -1);
    EXPECT_EQ(get_generic_array_ref(x, 0, &w_ref), -1);
    EXPECT_EQ(get_generic_map(x, "x", &w), -1);
    EXPECT_EQ(get_generic_map_ref(x, "x", &w_ref), -1);
    EXPECT_FALSE(generic_get_item(x, "string"));
    EXPECT_EQ(generic_get_item_nbytes(x, "string"), -1);
    EXPECT_FALSE(generic_ref_get_item(x_ref, "string"));
    EXPECT_EQ(generic_ref_get_item_nbytes(x_ref, "string"), -1);
    EXPECT_FALSE(generic_ref_get_scalar(x_ref, "float", 8));
    void* data = NULL;
    size_t* shape = NULL;
    EXPECT_EQ(generic_ref_get_1darray(x_ref, "float", 0, &data), 0);
    EXPECT_EQ(generic_ref_get_ndarray(x_ref, "float", 0, &data, &shape), 0);
    EXPECT_EQ(generic_set_1darray(x, NULL, "float", 0, 0, "cm"), -1);
    EXPECT_EQ(generic_set_ndarray(x, NULL, "float", 0, 0, shape, "cm"), -1);
    EXPECT_EQ(generic_set_item(x, "null", NULL), -1);
    EXPECT_EQ(generic_set_json(x, "1"), -1);
    EXPECT_EQ(copy_generic_into(NULL, x), -1);
    EXPECT_EQ(copy_generic_into(&v, x), -1);
    display_generic(x);
  }
  {
    // NULL object
    generic_t x = init_generic_null();
    generic_ref_t x_ref = init_generic_ref(x);
    generic_t v = init_generic();
    generic_t w = init_generic();
    generic_ref_t w_ref;
    EXPECT_EQ(generic_array_get_size(x), 0);
    EXPECT_EQ(generic_map_get_size(x), 0);
    EXPECT_EQ(generic_map_has_key(x, "x"), 0);
    EXPECT_EQ(generic_map_get_keys(x, &keys), 0);
    EXPECT_EQ(set_generic_array(x, 0, v), -1);
    EXPECT_EQ(set_generic_map(x, "x", v), -1);
    EXPECT_EQ(add_generic_array(x, v), -1);
    v = init_generic_null();
    EXPECT_EQ(set_generic_array(x, 0, v), -1);
    EXPECT_EQ(set_generic_map(x, "x", v), -1);
    EXPECT_EQ(add_generic_array(x, v), -1);
    EXPECT_EQ(get_generic_array(x, 0, &w), -1);
    EXPECT_EQ(get_generic_array_ref(x, 0, &w_ref), -1);
    EXPECT_EQ(get_generic_map(x, "x", &w), -1);
    EXPECT_EQ(get_generic_map_ref(x, "x", &w_ref), -1);
    EXPECT_FALSE(generic_get_item(x, "string"));
    EXPECT_EQ(generic_get_item_nbytes(x, "string"), -1);
    EXPECT_FALSE(generic_ref_get_item(x_ref, "string"));
    EXPECT_EQ(generic_ref_get_item_nbytes(x_ref, "string"), -1);
    EXPECT_EQ(generic_set_item(x, "invalid", NULL), -1);
    destroy_generic(&x);
    destroy_generic(&v);
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
#define NESTED_TEST(name, schema)					\
  TEST(generic_t, name) {						\
    generic_t x_arr = init_generic_array();				\
    generic_t x_obj = init_generic_map();				\
    generic_t data = init_generic_generate(schema);			\
    EXPECT_EQ(generic_array_set_ ## name(x_arr, 0, data), 0);		\
    generic_t x_arr_res = generic_array_get_ ## name(x_arr, 0);		\
    EXPECT_TRUE(compare_generic(x_arr_res, data));			\
    EXPECT_EQ(generic_map_set_ ## name(x_obj, "x", data), 0);		\
    generic_t x_obj_res = generic_map_get_ ## name(x_obj, "x");		\
    EXPECT_TRUE(compare_generic(x_obj_res, data));			\
    destroy_generic(&x_arr);						\
    destroy_generic(&x_obj);						\
    destroy_generic(&data);						\
    destroy_generic(&x_arr_res);					\
    destroy_generic(&x_obj_res);					\
  }
NESTED_TEST(object, "{\"type\": \"object\"}")
NESTED_TEST(array, "{\"type\": \"array\"}")
NESTED_TEST(any, "{\"type\": \"integer\"}")
NESTED_TEST(schema, "{\"type\": \"schema\"}")
#undef NESTED_TEST

TEST(generic_t, string) {
  generic_t x_arr = init_generic_array();
  generic_t x_obj = init_generic_map();
  const char* data = "hello";
  EXPECT_EQ(generic_array_set_string(x_arr, 0, data), 0);
  EXPECT_EQ(strcmp(generic_array_get_string(x_arr, 0), data), 0);
  EXPECT_EQ(generic_map_set_string(x_obj, "x", data), 0);
  EXPECT_EQ(strcmp(generic_map_get_string(x_obj, "x"), data), 0);
  destroy_generic(&x_arr);
  destroy_generic(&x_obj);
}
  
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

#define DO_TYPE(type)							\
  TEST(generic_t, data_ ## type) {					\
    generic_t v = init_generic_generate("{\"type\": \"" #type "\"}");	\
    generic_t x = init_generic_null();					\
    EXPECT_GT(generic_get_item_nbytes(v, #type), 0);			\
    void* data = generic_get_item(v, #type);				\
    EXPECT_TRUE(data);							\
    EXPECT_EQ(generic_set_item(x, #type, data), 0);			\
    EXPECT_TRUE(compare_generic(x, v));					\
    generic_t v_cpy = copy_generic(v);					\
    EXPECT_TRUE(compare_generic(v, v_cpy));				\
    destroy_generic(&v);						\
    destroy_generic(&x);						\
    destroy_generic(&v_cpy);						\
  }									\
  TEST(generic_t, data_array_ ## type) {				\
    generic_t vc = init_generic_generate(				\
      "{\"type\": \"array\", \"items\": [{\"type\": \"" #type "\"}]}"); \
    generic_t x = init_generic_null();					\
    generic_t v;							\
    get_generic_array(vc, 0, &v);					\
    EXPECT_GT(generic_array_get_item_nbytes(vc, 0, #type), 0);		\
    void* data = generic_array_get_item(vc, 0, #type);			\
    EXPECT_TRUE(data);							\
    if (data) {								\
      EXPECT_EQ(generic_set_item(x, #type, data), 0);			\
    }									\
    EXPECT_TRUE(compare_generic(x, v));					\
    destroy_generic(&v);						\
    destroy_generic(&x);						\
    destroy_generic(&vc);						\
  }									\
  TEST(generic_t, data_map_ ## type) {					\
    generic_t vc = init_generic_generate(				\
      "{\"type\": \"object\", \"properties\": {\"a\": {\"type\": \"" #type "\"}}}"); \
    generic_t x = init_generic_null();					\
    generic_t v;							\
    get_generic_map(vc, "a", &v);					\
    EXPECT_GT(generic_map_get_item_nbytes(vc, "a", #type), 0);		\
    void* data = generic_map_get_item(vc, "a", #type);			\
    EXPECT_TRUE(data);							\
    if (data) {								\
      EXPECT_EQ(generic_set_item(x, #type, data), 0);			\
    }									\
    EXPECT_TRUE(compare_generic(x, v));					\
    destroy_generic(&v);						\
    destroy_generic(&x);						\
    destroy_generic(&vc);						\
  }
DO_TYPE(null)
DO_TYPE(boolean)
DO_TYPE(number)
DO_TYPE(integer)
DO_TYPE(string)
DO_TYPE(object)
// TODO: DO_TYPE(class)
DO_TYPE(obj)
DO_TYPE(ply)
#undef DO_TYPE

TEST(generic_t, scalar) {
  generic_t v = init_generic_generate("{\"type\": \"scalar\", \"subtype\": \"float\", \"precision\": 8, \"units\": \"cm\"}");
  generic_t x = init_generic_null();
  EXPECT_FALSE(generic_get_scalar(v, "int", 8));
  EXPECT_FALSE(generic_get_scalar(v, "float", 2));
  void* data = generic_get_scalar(v, "float", 8);
  EXPECT_TRUE(data);
  EXPECT_EQ(generic_set_scalar(x, data, "float", 8, "cm"), 0);
  EXPECT_TRUE(compare_generic(x, v));
  destroy_generic(&v);
  destroy_generic(&x);
  EXPECT_EQ(generic_set_scalar(x, NULL, "float", 8, "cm"), -1);
}

TEST(generic_t, 1darray) {
  generic_t v = init_generic_generate("{\"type\": \"1darray\", \"subtype\": \"float\", \"precision\": 8, \"units\": \"cm\", \"length\": 3}");
  generic_t x = init_generic_null();
  void* data = NULL;
  EXPECT_EQ(generic_get_1darray(v, "int", 8, &data), 0);
  EXPECT_EQ(generic_get_1darray(v, "float", 2, &data), 0);
  EXPECT_EQ(generic_get_1darray(v, "float", 8, &data), 3);
  EXPECT_TRUE(data);
  EXPECT_EQ(generic_set_1darray(x, data, "float", 8, 3, "cm"), 0);
  EXPECT_TRUE(compare_generic(x, v));
// #ifdef ELF_AVAILABLE
//   ELF_BEGIN;
//   ELF_BEGIN_F(malloc);
//   data = NULL;
//   EXPECT_EQ(generic_get_1darray(v, "float", 8, &data), 0);
//   ELF_END_F(malloc);
//   ELF_END;
// #endif // ELF_AVAILABLE
  destroy_generic(&v);
  destroy_generic(&x);
}

TEST(generic_t, ndarray) {
  generic_t v = init_generic_generate("{\"type\": \"ndarray\", \"subtype\": \"float\", \"precision\": 8, \"units\": \"cm\", \"shape\": [2, 3]}");
  generic_t x = init_generic_null();
  void* data = NULL;
  size_t* shape = NULL;
  EXPECT_EQ(generic_get_ndarray(v, "int", 8, &data, &shape), 0);
  EXPECT_EQ(generic_get_ndarray(v, "float", 2, &data, &shape), 0);
  EXPECT_EQ(generic_get_ndarray(v, "float", 8, &data, &shape), 2);
  EXPECT_TRUE(data);
  EXPECT_TRUE(shape);
  EXPECT_EQ(generic_set_ndarray(x, data, "float", 8, 2, shape, "cm"), 0);
  EXPECT_TRUE(compare_generic(x, v));
// #ifdef ELF_AVAILABLE
//   ELF_BEGIN;
//   ELF_BEGIN_F(malloc);
//   data = NULL;
//   shape = NULL;
//   EXPECT_EQ(generic_get_ndarray(v, "float", 8, &data, &shape), 0);
//   ELF_END_F(malloc);
//   ELF_END;
// #endif // ELF_AVAILABLE
  destroy_generic(&v);
  destroy_generic(&x);
}

TEST(dtype_t, utils) {
  dtype_t x, y;
  x.metadata = NULL;
  EXPECT_EQ(is_empty_dtype(x), 1);
  EXPECT_EQ(is_dtype_format_array(x), -1);
  EXPECT_EQ(dtype_uses_generic(x), 0);
  EXPECT_EQ(set_dtype_name(x, "integer"), -1);
  EXPECT_EQ(strcmp(dtype_name(x), ""), 0);
  EXPECT_EQ(strcmp(dtype_subtype(x), ""), 0);
  EXPECT_EQ(dtype_precision(x), 0);
  x = complete_dtype(x, true);
  EXPECT_EQ(is_empty_dtype(x), 1);
  EXPECT_EQ(is_dtype_format_array(x), 0);
  EXPECT_EQ(dtype_uses_generic(x), 1);
  EXPECT_EQ(set_dtype_name(x, "integer"), 0);
  display_dtype(x, "");
  EXPECT_EQ(strcmp(dtype_name(x), "integer"), 0);
  EXPECT_EQ(strcmp(dtype_subtype(x), ""), 0);
  EXPECT_EQ(dtype_precision(x), 0);
  y = copy_dtype(x);
  EXPECT_EQ(destroy_dtype(&x), 0);
  EXPECT_EQ(destroy_dtype(&y), 0);
  y = copy_dtype(x);
  EXPECT_EQ(is_empty_dtype(x), 1);
  EXPECT_EQ(is_empty_dtype(y), 1);
}

TEST(dtype_t, create) {
  {
    dtype_t x = create_dtype_direct(false);
    EXPECT_EQ(strcmp(dtype_name(x), "string"), 0);
    destroy_dtype(&x);
  }
  {
    dtype_t x = create_dtype_scalar("float", 8, "cm", false);
    EXPECT_EQ(strcmp(dtype_name(x), "scalar"), 0);
    EXPECT_EQ(strcmp(dtype_subtype(x), "float"), 0);
    EXPECT_EQ(dtype_precision(x), 8);
    destroy_dtype(&x);
  }
  {
    dtype_t x = create_dtype_1darray("float", 8, 3, "cm", false);
    display_dtype(x, "");
    EXPECT_EQ(strcmp(dtype_name(x), "ndarray"), 0);
    EXPECT_EQ(strcmp(dtype_subtype(x), "float"), 0);
    EXPECT_EQ(dtype_precision(x), 8);
    destroy_dtype(&x);
  }
  {
    int64_t shape[2] = {2, 3};
    dtype_t x = create_dtype_ndarray_arr("float", 8, 2, shape, "cm", false);
    EXPECT_EQ(strcmp(dtype_name(x), "ndarray"), 0);
    EXPECT_EQ(strcmp(dtype_subtype(x), "float"), 0);
    EXPECT_EQ(dtype_precision(x), 8);
    destroy_dtype(&x);
  }
  {
    dtype_t x = create_dtype_json_array(5, NULL, false);
    EXPECT_FALSE(x.metadata);
    EXPECT_TRUE(is_empty_dtype(x));
    dtype_t item;
    item.metadata = NULL;
    x = create_dtype_json_array(1, &item, false);
    EXPECT_FALSE(x.metadata);
    EXPECT_TRUE(is_empty_dtype(x));
    item = create_dtype_from_schema("{\"type\": \"integer\"}", false);
    x = create_dtype_json_array(1, &item, false);
    EXPECT_TRUE(x.metadata);
    EXPECT_FALSE(is_empty_dtype(x));
    EXPECT_FALSE(item.metadata);
    EXPECT_TRUE(is_empty_dtype(item));
    destroy_dtype(&x);
  }
  {
    dtype_t x = create_dtype_json_object(5, NULL, NULL, false);
    EXPECT_FALSE(x.metadata);
    EXPECT_TRUE(is_empty_dtype(x));
    const char* key = "a";
    dtype_t item;
    item.metadata = NULL;
    x = create_dtype_json_object(1, &key, &item, false);
    EXPECT_FALSE(x.metadata);
    EXPECT_TRUE(is_empty_dtype(x));
    item = create_dtype_from_schema("{\"type\": \"integer\"}", false);
    x = create_dtype_json_object(1, &key, &item, false);
    EXPECT_TRUE(x.metadata);
    EXPECT_FALSE(is_empty_dtype(x));
    EXPECT_FALSE(item.metadata);
    EXPECT_TRUE(is_empty_dtype(item));
    destroy_dtype(&x);
  }
  {
    dtype_t x = create_dtype_pyobj("instance", false);
    EXPECT_TRUE(x.metadata);
    EXPECT_FALSE(is_empty_dtype(x));
    EXPECT_EQ(strcmp(dtype_name(x), "instance"), 0);
    destroy_dtype(&x);
  }
  {
    dtype_t x = create_dtype_format("%d\t%f\n", true, false);
    EXPECT_TRUE(x.metadata);
    EXPECT_FALSE(is_empty_dtype(x));
    EXPECT_EQ(strcmp(dtype_name(x), "array"), 0);
    EXPECT_EQ(is_dtype_format_array(x), 1);
    destroy_dtype(&x);
  }
  {
    dtype_t x = create_dtype_ascii_table("%d\t%f\n", true, false);
    EXPECT_TRUE(x.metadata);
    EXPECT_FALSE(is_empty_dtype(x));
    EXPECT_EQ(strcmp(dtype_name(x), "array"), 0);
    EXPECT_EQ(is_dtype_format_array(x), 1);
    destroy_dtype(&x);
  }
#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
  {
    dtype_t args = create_dtype_from_schema("{\"type\": \"array\", \"items\": [{\"type\": \"string\"}, {\"type\", \"integer\"}]}", false);
    dtype_t kwargs = create_dtype_from_schema("{\"type\": \"object\", \"properties\": {\"a\": {\"type\": \"boolean\"}}}", false);
    dtype_t x = create_dtype_pyinst("collections:OrderedDict",
				    &args, &kwargs, false);
    EXPECT_TRUE(x.metadata);
    EXPECT_FALSE(is_empty_dtype(x));
    EXPECT_EQ(strcmp(dtype_name(x), "instance"), 0);
    EXPECT_TRUE(is_empty_dtype(args));
    EXPECT_TRUE(is_empty_dtype(kwargs));
    destroy_dtype(&x);
    destroy_dtype(&args);
    destroy_dtype(&kwargs);
  }
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
}

TEST(dtype_t, errors) {
  {
    dtype_t x = create_dtype_default("invalid", false);
    EXPECT_FALSE(x.metadata);
    EXPECT_TRUE(is_empty_dtype(x));
  }
  {
    dtype_t x = create_dtype_scalar("invalid", 5, "cm", false);
    EXPECT_FALSE(x.metadata);
    EXPECT_TRUE(is_empty_dtype(x));
  }
  {
    dtype_t x = create_dtype_1darray("invalid", 5, 1, "cm", false);
    EXPECT_FALSE(x.metadata);
    EXPECT_TRUE(is_empty_dtype(x));
  }
  {
    size_t shape = 5;
    dtype_t x = create_dtype_ndarray("invalid", 5, 1, &shape,
				     "cm", false);
    EXPECT_FALSE(x.metadata);
    EXPECT_TRUE(is_empty_dtype(x));
  }
  {
    dtype_t args = create_dtype_from_schema("{\"type\": \"boolean\"}", false);
    dtype_t x = create_dtype_pyinst("collections:OrderedDict",
				    &args, NULL, false);
    EXPECT_FALSE(x.metadata);
    EXPECT_TRUE(is_empty_dtype(x));
    EXPECT_TRUE(is_empty_dtype(args));
    destroy_dtype(&x);
    destroy_dtype(&args);
  }
}

#define DO_GEOM(name)							\
  TEST(generic_t, name) {						\
    generic_t v = init_generic_generate("{\"type\": \"" #name "\"}");	\
    generic_t x = init_generic_null();					\
    name ## _t data = generic_get_ ## name(v);				\
    EXPECT_EQ(generic_set_ ## name(x, data), 0);			\
    EXPECT_TRUE(compare_generic(v, x));					\
    name ## _t copy = copy_ ## name(data);				\
    display_ ## name(data);						\
    display_ ## name ## _indent(data, "  ");				\
    EXPECT_GT(nelements_ ## name(data, "vertex"), 0);			\
    name ## _t raw1 = init_ ## name();					\
    name ## _t raw2 = init_ ## name();					\
    void* raw_obj = generic_get_item(v, #name);				\
    EXPECT_TRUE(raw_obj);						\
    set_ ## name(NULL, raw_obj, 0);					\
    set_ ## name(&raw1, NULL, 1);					\
    set_ ## name(&raw1, raw_obj, 1);					\
    set_ ## name(&raw2, raw_obj, 0);					\
    free_ ## name(&copy);						\
    free_ ## name(&data);						\
    free_ ## name(&raw1);						\
    free_ ## name(&raw2);						\
    destroy_generic(&v);						\
    destroy_generic(&x);						\
    name ## _t empty;							\
    empty.obj = NULL;							\
    display_ ## name(empty);						\
    EXPECT_EQ(nelements_ ## name(empty, "vertex"), -1);			\
  }
DO_GEOM(ply)
DO_GEOM(obj)
#undef DO_GEOM

TEST(dtype, PythonInit) {
// #ifdef YGGDRASIL_DISABLE_PYTHON_C_API
//   EXPECT_EQ(init_python_API(), 1);
// #else
  EXPECT_EQ(init_python_API(), 0);
// #endif
}

#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
#ifdef RAPIDJSON_DONT_IMPORT_NUMPY
#define CHECK_ARRAY_API EXPECT_FALSE(YggInterface::utils::numpy_arrays_imported())
#else // RAPIDJSON_DONT_IMPORT_NUMPY
#define CHECK_ARRAY_API EXPECT_TRUE(YggInterface::utils::numpy_arrays_imported())
#endif // RAPIDJSON_DONT_IMPORT_NUMPY
#define DO_PYTHON(name)							\
  TEST(generic_t, name) {						\
    CHECK_ARRAY_API;							\
    generic_t v = init_generic_generate("{\"type\": \"" #name "\"}");	\
    generic_t x = init_generic_null();					\
    python_t data;							\
    data.obj = NULL;							\
    display_python(data);						\
    data = generic_get_python_ ## name(v);				\
    EXPECT_EQ(generic_set_python_ ## name(x, data), 0);			\
    display_generic(v);							\
    display_generic(x);							\
    EXPECT_TRUE(compare_generic(v, x));					\
    python_t copy = copy_python(data);					\
    display_python(data);						\
    destroy_python(&data);						\
    destroy_python(&copy);						\
    destroy_generic(&v);						\
    destroy_generic(&x);						\
  }
DO_PYTHON(class)
DO_PYTHON(function)
DO_PYTHON(instance)
#undef DO_PYTHON
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
