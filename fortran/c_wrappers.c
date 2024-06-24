#include "c_wrappers.h"

#ifdef __cplusplus /* If this is a C++ compiler, use C linkage */
extern "C" {
#endif
  
#ifndef DOXYGEN_SHOULD_SKIP_THIS
/* void dummy_function_f(const int64_t message) { */
/*   printf("dummy function (c printf) %lld\n", message); */
/* } */
/* void dummy_function_f(const char* message) { */
/*   printf("dummy function (c printf) %s\n", message); */
/*   ygglog_info("dummy function (c)"); */
/* } */

int YGG_MSG_MAX_F = YGG_MSG_MAX;

// Utilities
void ygg_c_free(void *x) {
  if (x != NULL) {
    free(x);
  }
}

void register_function_f(const char* name, c_function func) {
  // bool (*func)(generic_t, generic_t)) {
  /* c_function* func_c = (c_function*)func; */
  printf("Before malloc in c_wrappers.c\n");
  size_t name_size = strlen(name) + 10;
  char* prefixed_name = (char*)malloc(name_size);
  printf("Before sprintf in c_wrappers.c\n");
  int res = snprintf(prefixed_name, name_size, "fortran::%s", name);
  if (res < 0 || res >= name_size)
    printf("Error in snprintf in c_wrappers.c\n");
  printf("Before _register_function in c_wrappers.c: %p\n", func);
  _register_function(prefixed_name, func, true);
  printf("After _register_function in c_wrappers.c\n");
  free(prefixed_name);
}

// Methods for initializing channels
comm_t _init_comm_f(const char *name, const int dir, const int t,
		    void* datatype, const int64_t flags, const size_t ncomm) {
#ifdef __cplusplus
  comm_t out = _init_comm(name, static_cast<DIRECTION>(dir),
			  static_cast<COMM_TYPE>(t), (dtype_t*)datatype,
			  static_cast<FLAG_TYPE>(flags), ncomm);
#else
  comm_t out = _init_comm(name, dir, t, (dtype_t*)datatype,
			  (FLAG_TYPE)(flags), ncomm);
#endif
  set_comm_language(out, FORTRAN_LANGUAGE);
  return out;
}

// Method for constructing data types
void display_dtype_f(const dtype_t datatype) {
  display_dtype(datatype, "");
}

// Methods for sending/receiving
int ygg_send_var_f(const comm_t yggQ, int nargs, void *args) {
  if (args == NULL) {
    ygglog_error("ygg_send_var_f: args pointer is NULL.");
    return -1;
  }
  return pcommSend(yggQ, nargs, (void**)args, 1);
}

int ygg_recv_var_f(comm_t yggQ, int nargs, void *args) {
  if (args == NULL) {
    ygglog_error("ygg_recv_var_f: args pointer is NULL.");
    return -1;
  }
  return pcommRecv(yggQ, 0, nargs, (void**)args, 1);
}

int ygg_recv_var_realloc_f(comm_t yggQ, int nargs, void *args) {
  if (args == NULL) {
    ygglog_error("ygg_recv_var_realloc_f: args pointer is NULL.");
    return -1;
  }
  return pcommRecv(yggQ, 1, nargs, (void**)args, 1);
}

int rpc_send_f(const comm_t yggQ, int nargs, void *args) {
  return ygg_send_var_f(yggQ, nargs, args);
}

int rpc_recv_f(comm_t yggQ, int nargs, void *args) {
  return ygg_recv_var_f(yggQ, nargs, args);
}

int rpc_recv_realloc_f(comm_t yggQ, int nargs, void *args) {
  return ygg_recv_var_realloc_f(yggQ, nargs, args);
}

int rpc_call_f(comm_t yggQ, int nargs, void *args) {
  if (args == NULL) {
    ygglog_error("rpc_call_f: args pointer is NULL.");
    return -1;
  }
  return pcommCall(yggQ, 0, nargs, (void**)args, 1);
}

int rpc_call_realloc_f(comm_t yggQ, int nargs, void *args) {
  if (args == NULL) {
    ygglog_error("rpc_call_realloc_f: args pointer is NULL.");
    return -1;
  }
  return pcommCall(yggQ, 1, nargs, (void**)args, 1);
}

// Interface for getting/setting generic map elements
void* generic_map_get_keys_f(generic_t x, void* n_keys_f, void* key_size_f) {
  char** keys_c = NULL;
  char* keys_f = NULL;
  size_t n_keys = generic_map_get_keys(x, &keys_c);
  size_t i, i_key_size, max_key_size = 0;
  for (i = 0; i < n_keys; i++) {
    i_key_size = strlen(keys_c[i]);
    if (i_key_size > max_key_size) {
      max_key_size = i_key_size;
    }
  }
  max_key_size++;
  keys_f = (char*)malloc(max_key_size * n_keys);
  for (i = 0; i < (n_keys * max_key_size); i++) {
    keys_f[i] = ' ';
  }
  for (i = 0; i < n_keys; i++) {
    memcpy(keys_f + (i * max_key_size), keys_c[i], strlen(keys_c[i]));
  }
  ((size_t*)n_keys_f)[0] = n_keys;
  ((size_t*)key_size_f)[0] = max_key_size;
  return (void*)keys_f;
}

#endif // DOXYGEN_SHOULD_SKIP_THIS

#ifdef __cplusplus /* If this is a C++ compiler, use C linkage */
}
#endif

// LINES AFTER THIS WERE GENERATED AND SHOULD NOT BE MODIFIED DIRECTLY
//====================================================================
comm_t ygg_output_fmt_f(const char* name, const char* fmtString) {
  return yggOutputFmt(name, fmtString);
}
comm_t ygg_input_fmt_f(const char* name, const char* fmtString) {
  return yggInputFmt(name, fmtString);
}
int ygg_send_f(const comm_t yggQ, const char* data, const size_t len) {
  return ygg_send(yggQ, data, len);
}
int ygg_send_eof_f(const comm_t yggQ) {
  return ygg_send_eof(yggQ);
}
long ygg_recv_f(comm_t yggQ, char* data, const size_t len) {
  return ygg_recv(yggQ, data, len);
}
comm_t ygg_rpc_client_type_f(const char* name, void* outType, void* inType) {
  return yggRpcClientType(name, (dtype_t*)outType, (dtype_t*)inType);
}
comm_t ygg_rpc_server_type_f(const char* name, void* inType, void* outType) {
  return yggRpcServerType(name, (dtype_t*)inType, (dtype_t*)outType);
}
comm_t ygg_rpc_client_f(const char* name, const char* outFormat, const char* inFormat) {
  return yggRpcClient(name, outFormat, inFormat);
}
comm_t ygg_rpc_server_f(const char* name, const char* inFormat, const char* outFormat) {
  return yggRpcServer(name, inFormat, outFormat);
}
comm_t ygg_timesync_f(const char* name, const char* t_units) {
  return yggTimesync(name, t_units);
}
comm_t ygg_ascii_table_output_f(const char* name, const char* format_str) {
  return yggAsciiTableOutput(name, format_str);
}
comm_t ygg_ascii_table_input_f(const char* name) {
  return yggAsciiTableInput(name);
}
comm_t ygg_ascii_array_output_f(const char* name, const char* format_str) {
  return yggAsciiArrayOutput(name, format_str);
}
comm_t ygg_ascii_array_input_f(const char* name) {
  return yggAsciiArrayInput(name);
}
comm_t ygg_ply_output_f(const char* name) {
  return yggPlyOutput(name);
}
comm_t ygg_ply_input_f(const char* name) {
  return yggPlyInput(name);
}
comm_t ygg_obj_output_f(const char* name) {
  return yggObjOutput(name);
}
comm_t ygg_obj_input_f(const char* name) {
  return yggObjInput(name);
}
comm_t ygg_generic_output_f(const char* name) {
  return yggGenericOutput(name);
}
comm_t ygg_generic_input_f(const char* name) {
  return yggGenericInput(name);
}
comm_t ygg_any_output_f(const char* name) {
  return yggAnyOutput(name);
}
comm_t ygg_any_input_f(const char* name) {
  return yggAnyInput(name);
}
comm_t ygg_json_array_output_f(const char* name) {
  return yggJSONArrayOutput(name);
}
comm_t ygg_json_array_input_f(const char* name) {
  return yggJSONArrayInput(name);
}
comm_t ygg_json_object_output_f(const char* name) {
  return yggJSONObjectOutput(name);
}
comm_t ygg_json_object_input_f(const char* name) {
  return yggJSONObjectInput(name);
}
void ygglog_error_f(const char* fmt) {
  ygglog_error(fmt);
}
void ygglog_debug_f(const char* fmt) {
  ygglog_debug(fmt);
}
void ygglog_info_f(const char* fmt) {
  ygglog_info(fmt);
}
int ygg_init_f() {
  return ygg_init();
}
void ygg_exit_f() {
  ygg_exit();
}
void free_comm_f(void* comm) {
  free_comm((comm_t*)comm);
}
void close_comm_f(void* comm) {
  close_comm((comm_t*)comm);
}
int set_response_format_f(comm_t comm, const char* fmt) {
  return set_response_format(comm, fmt);
}
int set_response_datatype_f(comm_t x, void* datatype) {
  return set_response_datatype(x, (dtype_t*)datatype);
}
int comm_wait_for_recv_f(const comm_t x, const int64_t tout) {
  return comm_wait_for_recv(x, tout);
}
dtype_t comm_get_datatype_f(comm_t x) {
  return comm_get_datatype(x);
}
int comm_set_datatype_f(comm_t x, void* datatype) {
  return comm_set_datatype(x, (dtype_t*)datatype);
}
int comm_send_f(comm_t comm, const char* data, const size_t len) {
  return comm_send(comm, data, len);
}
int comm_send_eof_f(comm_t comm) {
  return comm_send_eof(comm);
}
int is_comm_format_array_type_f(const comm_t x) {
  return is_comm_format_array_type(x);
}
long comm_recv_f(comm_t comm, char* data, const size_t len) {
  return comm_recv(comm, data, len);
}
int comm_nmsg_f(comm_t comm) {
  return comm_nmsg(comm);
}
int pcomm_send_f(const comm_t comm, const size_t nargs, void* ptrs, const int for_fortran) {
  return pcommSend(comm, nargs, (void**)ptrs, for_fortran);
}
long pcomm_recv_f(comm_t comm, const int allow_realloc, const size_t nargs, void* ptrs, const int for_fortran) {
  return pcommRecv(comm, allow_realloc, nargs, (void**)ptrs, for_fortran);
}
long pcomm_call_f(comm_t comm, const int allow_realloc, const size_t nargs, void* ptrs, const int for_fortran) {
  return pcommCall(comm, allow_realloc, nargs, (void**)ptrs, for_fortran);
}
void set_global_comm_f() {
  global_scope_comm_on_c();
}
void unset_global_comm_f() {
  global_scope_comm_off_c();
}
size_t pointer_strlen_f(const void* x) {
  return pointer_strlen(x);
}
generic_t init_generic_f() {
  return init_generic();
}
generic_ref_t init_generic_ref_f(generic_t parent) {
  return init_generic_ref(parent);
}
generic_t init_generic_null_f() {
  return init_generic_null();
}
generic_t init_generic_array_f() {
  return init_generic_array();
}
generic_t init_generic_map_f() {
  return init_generic_map();
}
generic_t init_generic_json_f(const char* json) {
  return init_generic_json(json);
}
generic_t init_generic_generate_f(const char* schema) {
  return init_generic_generate(schema);
}
int is_generic_init_f(const generic_t x) {
  return is_generic_init(x);
}
int is_generic_ref_init_f(const generic_ref_t x) {
  return is_generic_ref_init(x);
}
int free_generic_f(void* x) {
  return destroy_generic((generic_t*)x);
}
int copy_generic_into_f(void* dst, const generic_t src) {
  return copy_generic_into((generic_t*)dst, src);
}
generic_t copy_generic_f(const generic_t src) {
  return copy_generic(src);
}
bool compare_generic_f(const generic_t a, const generic_t b) {
  return compare_generic(a, b);
}
void display_generic_f(const generic_t x) {
  display_generic(x);
}
int add_generic_array_f(generic_t arr, const generic_t x) {
  return add_generic_array(arr, x);
}
int set_generic_array_f(generic_t arr, const size_t i, const generic_t x) {
  return set_generic_array(arr, i, x);
}
int get_generic_array_f(const generic_t arr, const size_t i, void* x) {
  return get_generic_array(arr, i, (generic_t*)x);
}
int get_generic_array_ref_f(const generic_t arr, const size_t i, void* x) {
  return get_generic_array_ref(arr, i, (generic_ref_t*)x);
}
int set_generic_object_f(generic_t arr, const char* k, const generic_t x) {
  return set_generic_object(arr, k, x);
}
int get_generic_object_f(const generic_t arr, const char* k, void* x) {
  return get_generic_object(arr, k, (generic_t*)x);
}
int get_generic_object_ref_f(const generic_t arr, const char* k, void* x) {
  return get_generic_object_ref(arr, k, (generic_ref_t*)x);
}
int generic_map_has_key_f(const generic_t x, const char* key) {
  return generic_map_has_key(x, key);
}
int generic_set_json_f(generic_t x, const char* json) {
  return generic_set_json(x, json);
}
int init_python_api_f() {
  return init_python_API();
}
python_t init_python_f() {
  return init_python();
}
void free_python_f(void* x) {
  destroy_python((python_t*)x);
}
python_t copy_python_f(python_t x) {
  return copy_python(x);
}
void display_python_f(python_t x) {
  display_python(x);
}
int is_empty_dtype_f(const dtype_t dtype) {
  return is_empty_dtype(dtype);
}
int is_dtype_format_array_f(const dtype_t type_struct) {
  return is_dtype_format_array(type_struct);
}
const char* dtype_name_f(const dtype_t type_class) {
  return dtype_name(type_class);
}
const char* dtype_subtype_f(const dtype_t type_class) {
  return dtype_subtype(type_class);
}
bool compare_dtype_f(const dtype_t a, const dtype_t b) {
  return compare_dtype(a, b);
}
size_t dtype_precision_f(const dtype_t type_class) {
  return dtype_precision(type_class);
}
int set_dtype_name_f(dtype_t dtype, const char* name) {
  return set_dtype_name(dtype, name);
}
dtype_t complete_dtype_f(dtype_t dtype, const bool use_generic) {
  return complete_dtype(dtype, use_generic);
}
dtype_t create_dtype_from_schema_f(const char* schema, const bool use_generic) {
  return create_dtype_from_schema(schema, use_generic);
}
dtype_t create_dtype_empty_f(const bool use_generic) {
  return create_dtype_empty(use_generic);
}
dtype_t create_dtype_python_f(void* pyobj, const bool use_generic) {
  return create_dtype_python((PyObject*)pyobj, use_generic);
}
dtype_t create_dtype_direct_f(const bool use_generic) {
  return create_dtype_direct(use_generic);
}
dtype_t create_dtype_default_f(const char* type, const bool use_generic) {
  return create_dtype_default(type, use_generic);
}
dtype_t create_dtype_scalar_f(const char* subtype, const size_t precision, const char* units, const bool use_generic) {
  return create_dtype_scalar(subtype, precision, units, use_generic);
}
dtype_t create_dtype_1darray_f(const char* subtype, const size_t precision, const size_t length, const char* units, const bool use_generic) {
  return create_dtype_1darray(subtype, precision, length, units, use_generic);
}
dtype_t create_dtype_ndarray_f(const char* subtype, const size_t precision, const size_t ndim, const void* shape, const char* units, const bool use_generic) {
  return create_dtype_ndarray(subtype, precision, ndim, (const size_t*)shape, units, use_generic);
}
dtype_t create_dtype_json_array_f(const size_t nitems, void* items, const bool use_generic) {
  return create_dtype_json_array(nitems, (dtype_t*)items, use_generic);
}
dtype_t create_dtype_json_object_f(const size_t nitems, const void* keys, void* values, const bool use_generic) {
  return create_dtype_json_object(nitems, (const char**)keys, (dtype_t*)values, use_generic);
}
dtype_t create_dtype_ply_f(const bool use_generic) {
  return create_dtype_ply(use_generic);
}
dtype_t create_dtype_obj_f(const bool use_generic) {
  return create_dtype_obj(use_generic);
}
dtype_t create_dtype_ascii_table_f(const char* format_str, const bool as_array, const bool use_generic) {
  return create_dtype_ascii_table(format_str, as_array, use_generic);
}
dtype_t create_dtype_format_f(const char* format_str, const bool as_array, const bool use_generic) {
  return create_dtype_format(format_str, as_array, use_generic);
}
dtype_t create_dtype_pyobj_f(const char* type, const bool use_generic) {
  return create_dtype_pyobj(type, use_generic);
}
dtype_t create_dtype_pyinst_f(const char* class_name, void* args_dtype, void* kwargs_dtype, const bool use_generic) {
  return create_dtype_pyinst(class_name, (dtype_t*)args_dtype, (dtype_t*)kwargs_dtype, use_generic);
}
dtype_t create_dtype_schema_f(const bool use_generic) {
  return create_dtype_schema(use_generic);
}
dtype_t create_dtype_any_f(const bool use_generic) {
  return create_dtype_any(use_generic);
}
int destroy_dtype_f(void* dtype) {
  return destroy_dtype((dtype_t*)dtype);
}
dtype_t copy_dtype_f(const dtype_t dtype) {
  return copy_dtype(dtype);
}
int dtype_uses_generic_f(dtype_t dtype) {
  return dtype_uses_generic(dtype);
}
obj_t init_obj_f() {
  return init_obj();
}
obj_t generate_obj_f() {
  return generate_obj();
}
void free_obj_f(void* p) {
  free_obj((obj_t*)p);
}
void set_obj_f(void* x, void* obj, int copy) {
  set_obj((obj_t*)x, obj, copy);
}
obj_t copy_obj_f(obj_t src) {
  return copy_obj(src);
}
void display_obj_indent_f(obj_t p, const char* indent) {
  display_obj_indent(p, indent);
}
void display_obj_f(obj_t p) {
  display_obj(p);
}
int nelements_obj_f(obj_t p, const char* name) {
  return nelements_obj(p, name);
}
bool compare_obj_f(const obj_t a, const obj_t b) {
  return compare_obj(a, b);
}
ply_t init_ply_f() {
  return init_ply();
}
ply_t generate_ply_f() {
  return generate_ply();
}
void free_ply_f(void* p) {
  free_ply((ply_t*)p);
}
void set_ply_f(void* x, void* ply, int copy) {
  set_ply((ply_t*)x, ply, copy);
}
ply_t copy_ply_f(ply_t src) {
  return copy_ply(src);
}
void display_ply_indent_f(ply_t p, const char* indent) {
  display_ply_indent(p, indent);
}
void display_ply_f(ply_t p) {
  display_ply(p);
}
int nelements_ply_f(ply_t p, const char* name) {
  return nelements_ply(p, name);
}
bool compare_ply_f(const ply_t a, const ply_t b) {
  return compare_ply(a, b);
}
size_t generic_array_get_size_f(generic_t x) {
  return generic_array_get_size(x);
}
size_t generic_object_get_size_f(generic_t x) {
  return generic_object_get_size(x);
}
int generic_set_null_f(generic_t x, const void* value) {
  return generic_set_null(x, value);
}
int generic_set_bool_f(generic_t x, const bool value) {
  return generic_set_bool(x, value);
}
int generic_set_integer_f(generic_t x, const int value) {
  return generic_set_integer(x, value);
}
int generic_set_number_f(generic_t x, const double value) {
  return generic_set_number(x, value);
}
int generic_set_string_f(generic_t x, const char* value) {
  return generic_set_string(x, value);
}
int generic_set_item_f(generic_t x, const char* type, void* value) {
  return generic_set_item(x, type, value);
}
int generic_set_array_f(generic_t x, const generic_t value) {
  return generic_set_array(x, value);
}
int generic_set_object_f(generic_t x, const generic_t value) {
  return generic_set_object(x, value);
}
int generic_set_ply_f(generic_t x, const ply_t value) {
  return generic_set_ply(x, value);
}
int generic_set_obj_f(generic_t x, const obj_t value) {
  return generic_set_obj(x, value);
}
int generic_set_python_class_f(generic_t x, const python_t value) {
  return generic_set_python_class(x, value);
}
int generic_set_python_function_f(generic_t x, const python_t value) {
  return generic_set_python_function(x, value);
}
int generic_set_python_instance_f(generic_t x, const python_t value) {
  return generic_set_python_instance(x, value);
}
int generic_set_scalar_f(generic_t x, const void* value, const char* subtype, const size_t precision, const char* units) {
  return generic_set_scalar(x, value, subtype, precision, units);
}
int generic_set_int16_f(generic_t x, const int16_t value, const char* units) {
  return generic_set_int16(x, value, units);
}
int generic_set_int32_f(generic_t x, const int32_t value, const char* units) {
  return generic_set_int32(x, value, units);
}
int generic_set_int64_f(generic_t x, const int64_t value, const char* units) {
  return generic_set_int64(x, value, units);
}
int generic_set_float_f(generic_t x, const float value, const char* units) {
  return generic_set_float(x, value, units);
}
int generic_set_double_f(generic_t x, const double value, const char* units) {
  return generic_set_double(x, value, units);
}
int generic_set_complex_float_f(generic_t x, const complex_float_t value, const char* units) {
  return generic_set_complex_float(x, value, units);
}
int generic_set_complex_double_f(generic_t x, const complex_double_t value, const char* units) {
  return generic_set_complex_double(x, value, units);
}
int generic_set_1darray_f(generic_t x, const void* value, const char* subtype, const size_t precision, const size_t length, const char* units) {
  return generic_set_1darray(x, value, subtype, precision, length, units);
}
int generic_set_1darray_int16_f(generic_t x, const void* value, const size_t length, const char* units) {
  return generic_set_1darray_int16(x, (const int16_t*)value, length, units);
}
int generic_set_1darray_int32_f(generic_t x, const void* value, const size_t length, const char* units) {
  return generic_set_1darray_int32(x, (const int32_t*)value, length, units);
}
int generic_set_1darray_int64_f(generic_t x, const void* value, const size_t length, const char* units) {
  return generic_set_1darray_int64(x, (const int64_t*)value, length, units);
}
int generic_set_1darray_float_f(generic_t x, const void* value, const size_t length, const char* units) {
  return generic_set_1darray_float(x, (const float*)value, length, units);
}
int generic_set_1darray_double_f(generic_t x, const void* value, const size_t length, const char* units) {
  return generic_set_1darray_double(x, (const double*)value, length, units);
}
int generic_set_1darray_complex_float_f(generic_t x, const void* value, const size_t length, const char* units) {
  return generic_set_1darray_complex_float(x, (const complex_float_t*)value, length, units);
}
int generic_set_1darray_complex_double_f(generic_t x, const void* value, const size_t length, const char* units) {
  return generic_set_1darray_complex_double(x, (const complex_double_t*)value, length, units);
}
int generic_set_ndarray_f(generic_t x, const void* value, const char* subtype, const size_t precision, const size_t ndim, const void* shape, const char* units) {
  return generic_set_ndarray(x, value, subtype, precision, ndim, (const size_t*)shape, units);
}
int generic_set_ndarray_int16_f(generic_t x, const void* value, const size_t ndim, const void* shape, const char* units) {
  return generic_set_ndarray_int16(x, (const int16_t*)value, ndim, (const size_t*)shape, units);
}
int generic_set_ndarray_int32_f(generic_t x, const void* value, const size_t ndim, const void* shape, const char* units) {
  return generic_set_ndarray_int32(x, (const int32_t*)value, ndim, (const size_t*)shape, units);
}
int generic_set_ndarray_int64_f(generic_t x, const void* value, const size_t ndim, const void* shape, const char* units) {
  return generic_set_ndarray_int64(x, (const int64_t*)value, ndim, (const size_t*)shape, units);
}
int generic_set_ndarray_float_f(generic_t x, const void* value, const size_t ndim, const void* shape, const char* units) {
  return generic_set_ndarray_float(x, (const float*)value, ndim, (const size_t*)shape, units);
}
int generic_set_ndarray_double_f(generic_t x, const void* value, const size_t ndim, const void* shape, const char* units) {
  return generic_set_ndarray_double(x, (const double*)value, ndim, (const size_t*)shape, units);
}
int generic_set_ndarray_complex_float_f(generic_t x, const void* value, const size_t ndim, const void* shape, const char* units) {
  return generic_set_ndarray_complex_float(x, (const complex_float_t*)value, ndim, (const size_t*)shape, units);
}
int generic_set_ndarray_complex_double_f(generic_t x, const void* value, const size_t ndim, const void* shape, const char* units) {
  return generic_set_ndarray_complex_double(x, (const complex_double_t*)value, ndim, (const size_t*)shape, units);
}
int generic_set_schema_f(generic_t x, const generic_t value) {
  return generic_set_schema(x, value);
}
int generic_set_any_f(generic_t x, const generic_t value) {
  return generic_set_any(x, value);
}
void* generic_get_null_f(generic_t x) {
  return generic_get_null(x);
}
bool generic_get_bool_f(generic_t x) {
  return generic_get_bool(x);
}
int generic_get_integer_f(generic_t x) {
  return generic_get_integer(x);
}
double generic_get_number_f(generic_t x) {
  return generic_get_number(x);
}
const char* generic_get_string_f(generic_t x) {
  return generic_get_string(x);
}
void* generic_get_item_f(generic_t x, const char* type) {
  return generic_get_item(x, type);
}
int generic_get_item_nbytes_f(generic_t x, const char* type) {
  return generic_get_item_nbytes(x, type);
}
generic_t generic_get_array_f(generic_t x) {
  return generic_get_array(x);
}
generic_t generic_get_object_f(generic_t x) {
  return generic_get_object(x);
}
ply_t generic_get_ply_f(generic_t x) {
  return generic_get_ply(x);
}
obj_t generic_get_obj_f(generic_t x) {
  return generic_get_obj(x);
}
python_t generic_get_python_class_f(generic_t x) {
  return generic_get_python_class(x);
}
python_t generic_get_python_function_f(generic_t x) {
  return generic_get_python_function(x);
}
python_t generic_get_python_instance_f(generic_t x) {
  return generic_get_python_instance(x);
}
void* generic_get_scalar_f(generic_t x, const char* subtype, const size_t precision) {
  return generic_get_scalar(x, subtype, precision);
}
int16_t generic_get_int16_f(generic_t x) {
  return generic_get_int16(x);
}
int32_t generic_get_int32_f(generic_t x) {
  return generic_get_int32(x);
}
int64_t generic_get_int64_f(generic_t x) {
  return generic_get_int64(x);
}
float generic_get_float_f(generic_t x) {
  return generic_get_float(x);
}
double generic_get_double_f(generic_t x) {
  return generic_get_double(x);
}
complex_float_t generic_get_complex_float_f(generic_t x) {
  return generic_get_complex_float(x);
}
complex_double_t generic_get_complex_double_f(generic_t x) {
  return generic_get_complex_double(x);
}
size_t generic_get_1darray_f(generic_t x, const char* subtype, const size_t precision, void* value) {
  return generic_get_1darray(x, subtype, precision, (void**)value);
}
size_t generic_get_1darray_int16_f(generic_t x, void* value) {
  return generic_get_1darray_int16(x, (int16_t**)value);
}
size_t generic_get_1darray_int32_f(generic_t x, void* value) {
  return generic_get_1darray_int32(x, (int32_t**)value);
}
size_t generic_get_1darray_int64_f(generic_t x, void* value) {
  return generic_get_1darray_int64(x, (int64_t**)value);
}
size_t generic_get_1darray_float_f(generic_t x, void* value) {
  return generic_get_1darray_float(x, (float**)value);
}
size_t generic_get_1darray_double_f(generic_t x, void* value) {
  return generic_get_1darray_double(x, (double**)value);
}
size_t generic_get_1darray_complex_float_f(generic_t x, void* value) {
  return generic_get_1darray_complex_float(x, (complex_float_t**)value);
}
size_t generic_get_1darray_complex_double_f(generic_t x, void* value) {
  return generic_get_1darray_complex_double(x, (complex_double_t**)value);
}
size_t generic_get_ndarray_f(generic_t x, const char* subtype, const size_t precision, void* value, void* shape) {
  return generic_get_ndarray(x, subtype, precision, (void**)value, (size_t**)shape);
}
size_t generic_get_ndarray_int16_f(generic_t x, void* value, void* shape) {
  return generic_get_ndarray_int16(x, (int16_t**)value, (size_t**)shape);
}
size_t generic_get_ndarray_int32_f(generic_t x, void* value, void* shape) {
  return generic_get_ndarray_int32(x, (int32_t**)value, (size_t**)shape);
}
size_t generic_get_ndarray_int64_f(generic_t x, void* value, void* shape) {
  return generic_get_ndarray_int64(x, (int64_t**)value, (size_t**)shape);
}
size_t generic_get_ndarray_float_f(generic_t x, void* value, void* shape) {
  return generic_get_ndarray_float(x, (float**)value, (size_t**)shape);
}
size_t generic_get_ndarray_double_f(generic_t x, void* value, void* shape) {
  return generic_get_ndarray_double(x, (double**)value, (size_t**)shape);
}
size_t generic_get_ndarray_complex_float_f(generic_t x, void* value, void* shape) {
  return generic_get_ndarray_complex_float(x, (complex_float_t**)value, (size_t**)shape);
}
size_t generic_get_ndarray_complex_double_f(generic_t x, void* value, void* shape) {
  return generic_get_ndarray_complex_double(x, (complex_double_t**)value, (size_t**)shape);
}
generic_t generic_get_schema_f(generic_t x) {
  return generic_get_schema(x);
}
generic_t generic_get_any_f(generic_t x) {
  return generic_get_any(x);
}
void* generic_ref_get_null_f(generic_ref_t x) {
  return generic_ref_get_null(x);
}
bool generic_ref_get_bool_f(generic_ref_t x) {
  return generic_ref_get_bool(x);
}
int generic_ref_get_integer_f(generic_ref_t x) {
  return generic_ref_get_integer(x);
}
double generic_ref_get_number_f(generic_ref_t x) {
  return generic_ref_get_number(x);
}
const char* generic_ref_get_string_f(generic_ref_t x) {
  return generic_ref_get_string(x);
}
void* generic_ref_get_item_f(generic_ref_t x, const char* type) {
  return generic_ref_get_item(x, type);
}
int generic_ref_get_item_nbytes_f(generic_ref_t x, const char* type) {
  return generic_ref_get_item_nbytes(x, type);
}
generic_t generic_ref_get_array_f(generic_ref_t x) {
  return generic_ref_get_array(x);
}
generic_t generic_ref_get_object_f(generic_ref_t x) {
  return generic_ref_get_object(x);
}
ply_t generic_ref_get_ply_f(generic_ref_t x) {
  return generic_ref_get_ply(x);
}
obj_t generic_ref_get_obj_f(generic_ref_t x) {
  return generic_ref_get_obj(x);
}
python_t generic_ref_get_python_class_f(generic_ref_t x) {
  return generic_ref_get_python_class(x);
}
python_t generic_ref_get_python_function_f(generic_ref_t x) {
  return generic_ref_get_python_function(x);
}
python_t generic_ref_get_python_instance_f(generic_ref_t x) {
  return generic_ref_get_python_instance(x);
}
void* generic_ref_get_scalar_f(generic_ref_t x, const char* subtype, const size_t precision) {
  return generic_ref_get_scalar(x, subtype, precision);
}
int16_t generic_ref_get_int16_f(generic_ref_t x) {
  return generic_ref_get_int16(x);
}
int32_t generic_ref_get_int32_f(generic_ref_t x) {
  return generic_ref_get_int32(x);
}
int64_t generic_ref_get_int64_f(generic_ref_t x) {
  return generic_ref_get_int64(x);
}
float generic_ref_get_float_f(generic_ref_t x) {
  return generic_ref_get_float(x);
}
double generic_ref_get_double_f(generic_ref_t x) {
  return generic_ref_get_double(x);
}
complex_float_t generic_ref_get_complex_float_f(generic_ref_t x) {
  return generic_ref_get_complex_float(x);
}
complex_double_t generic_ref_get_complex_double_f(generic_ref_t x) {
  return generic_ref_get_complex_double(x);
}
size_t generic_ref_get_1darray_f(generic_ref_t x, const char* subtype, const size_t precision, void* value) {
  return generic_ref_get_1darray(x, subtype, precision, (void**)value);
}
size_t generic_ref_get_1darray_int16_f(generic_ref_t x, void* value) {
  return generic_ref_get_1darray_int16(x, (int16_t**)value);
}
size_t generic_ref_get_1darray_int32_f(generic_ref_t x, void* value) {
  return generic_ref_get_1darray_int32(x, (int32_t**)value);
}
size_t generic_ref_get_1darray_int64_f(generic_ref_t x, void* value) {
  return generic_ref_get_1darray_int64(x, (int64_t**)value);
}
size_t generic_ref_get_1darray_float_f(generic_ref_t x, void* value) {
  return generic_ref_get_1darray_float(x, (float**)value);
}
size_t generic_ref_get_1darray_double_f(generic_ref_t x, void* value) {
  return generic_ref_get_1darray_double(x, (double**)value);
}
size_t generic_ref_get_1darray_complex_float_f(generic_ref_t x, void* value) {
  return generic_ref_get_1darray_complex_float(x, (complex_float_t**)value);
}
size_t generic_ref_get_1darray_complex_double_f(generic_ref_t x, void* value) {
  return generic_ref_get_1darray_complex_double(x, (complex_double_t**)value);
}
size_t generic_ref_get_ndarray_f(generic_ref_t x, const char* subtype, const size_t precision, void* value, void* shape) {
  return generic_ref_get_ndarray(x, subtype, precision, (void**)value, (size_t**)shape);
}
size_t generic_ref_get_ndarray_int16_f(generic_ref_t x, void* value, void* shape) {
  return generic_ref_get_ndarray_int16(x, (int16_t**)value, (size_t**)shape);
}
size_t generic_ref_get_ndarray_int32_f(generic_ref_t x, void* value, void* shape) {
  return generic_ref_get_ndarray_int32(x, (int32_t**)value, (size_t**)shape);
}
size_t generic_ref_get_ndarray_int64_f(generic_ref_t x, void* value, void* shape) {
  return generic_ref_get_ndarray_int64(x, (int64_t**)value, (size_t**)shape);
}
size_t generic_ref_get_ndarray_float_f(generic_ref_t x, void* value, void* shape) {
  return generic_ref_get_ndarray_float(x, (float**)value, (size_t**)shape);
}
size_t generic_ref_get_ndarray_double_f(generic_ref_t x, void* value, void* shape) {
  return generic_ref_get_ndarray_double(x, (double**)value, (size_t**)shape);
}
size_t generic_ref_get_ndarray_complex_float_f(generic_ref_t x, void* value, void* shape) {
  return generic_ref_get_ndarray_complex_float(x, (complex_float_t**)value, (size_t**)shape);
}
size_t generic_ref_get_ndarray_complex_double_f(generic_ref_t x, void* value, void* shape) {
  return generic_ref_get_ndarray_complex_double(x, (complex_double_t**)value, (size_t**)shape);
}
generic_t generic_ref_get_schema_f(generic_ref_t x) {
  return generic_ref_get_schema(x);
}
generic_t generic_ref_get_any_f(generic_ref_t x) {
  return generic_ref_get_any(x);
}
int generic_array_set_null_f(generic_t x, const size_t index, const void* value) {
  return generic_array_set_null(x, index, value);
}
int generic_array_set_bool_f(generic_t x, const size_t index, const bool value) {
  return generic_array_set_bool(x, index, value);
}
int generic_array_set_integer_f(generic_t x, const size_t index, const int value) {
  return generic_array_set_integer(x, index, value);
}
int generic_array_set_number_f(generic_t x, const size_t index, const double value) {
  return generic_array_set_number(x, index, value);
}
int generic_array_set_string_f(generic_t x, const size_t index, const char* value) {
  return generic_array_set_string(x, index, value);
}
int generic_array_set_item_f(generic_t x, const size_t index, const char* type, void* value) {
  return generic_array_set_item(x, index, type, value);
}
int generic_array_set_array_f(generic_t x, const size_t index, const generic_t value) {
  return generic_array_set_array(x, index, value);
}
int generic_array_set_object_f(generic_t x, const size_t index, const generic_t value) {
  return generic_array_set_object(x, index, value);
}
int generic_array_set_ply_f(generic_t x, const size_t index, const ply_t value) {
  return generic_array_set_ply(x, index, value);
}
int generic_array_set_obj_f(generic_t x, const size_t index, const obj_t value) {
  return generic_array_set_obj(x, index, value);
}
int generic_array_set_python_class_f(generic_t x, const size_t index, const python_t value) {
  return generic_array_set_python_class(x, index, value);
}
int generic_array_set_python_function_f(generic_t x, const size_t index, const python_t value) {
  return generic_array_set_python_function(x, index, value);
}
int generic_array_set_python_instance_f(generic_t x, const size_t index, const python_t value) {
  return generic_array_set_python_instance(x, index, value);
}
int generic_array_set_scalar_f(generic_t x, const size_t index, const void* value, const char* subtype, const size_t precision, const char* units) {
  return generic_array_set_scalar(x, index, value, subtype, precision, units);
}
int generic_array_set_int16_f(generic_t x, const size_t index, const int16_t value, const char* units) {
  return generic_array_set_int16(x, index, value, units);
}
int generic_array_set_int32_f(generic_t x, const size_t index, const int32_t value, const char* units) {
  return generic_array_set_int32(x, index, value, units);
}
int generic_array_set_int64_f(generic_t x, const size_t index, const int64_t value, const char* units) {
  return generic_array_set_int64(x, index, value, units);
}
int generic_array_set_float_f(generic_t x, const size_t index, const float value, const char* units) {
  return generic_array_set_float(x, index, value, units);
}
int generic_array_set_double_f(generic_t x, const size_t index, const double value, const char* units) {
  return generic_array_set_double(x, index, value, units);
}
int generic_array_set_complex_float_f(generic_t x, const size_t index, const complex_float_t value, const char* units) {
  return generic_array_set_complex_float(x, index, value, units);
}
int generic_array_set_complex_double_f(generic_t x, const size_t index, const complex_double_t value, const char* units) {
  return generic_array_set_complex_double(x, index, value, units);
}
int generic_array_set_1darray_f(generic_t x, const size_t index, const void* value, const char* subtype, const size_t precision, const size_t length, const char* units) {
  return generic_array_set_1darray(x, index, value, subtype, precision, length, units);
}
int generic_array_set_1darray_int16_f(generic_t x, const size_t index, const void* value, const size_t length, const char* units) {
  return generic_array_set_1darray_int16(x, index, (const int16_t*)value, length, units);
}
int generic_array_set_1darray_int32_f(generic_t x, const size_t index, const void* value, const size_t length, const char* units) {
  return generic_array_set_1darray_int32(x, index, (const int32_t*)value, length, units);
}
int generic_array_set_1darray_int64_f(generic_t x, const size_t index, const void* value, const size_t length, const char* units) {
  return generic_array_set_1darray_int64(x, index, (const int64_t*)value, length, units);
}
int generic_array_set_1darray_float_f(generic_t x, const size_t index, const void* value, const size_t length, const char* units) {
  return generic_array_set_1darray_float(x, index, (const float*)value, length, units);
}
int generic_array_set_1darray_double_f(generic_t x, const size_t index, const void* value, const size_t length, const char* units) {
  return generic_array_set_1darray_double(x, index, (const double*)value, length, units);
}
int generic_array_set_1darray_complex_float_f(generic_t x, const size_t index, const void* value, const size_t length, const char* units) {
  return generic_array_set_1darray_complex_float(x, index, (const complex_float_t*)value, length, units);
}
int generic_array_set_1darray_complex_double_f(generic_t x, const size_t index, const void* value, const size_t length, const char* units) {
  return generic_array_set_1darray_complex_double(x, index, (const complex_double_t*)value, length, units);
}
int generic_array_set_ndarray_f(generic_t x, const size_t index, const void* value, const char* subtype, const size_t precision, const size_t ndim, const void* shape, const char* units) {
  return generic_array_set_ndarray(x, index, value, subtype, precision, ndim, (const size_t*)shape, units);
}
int generic_array_set_ndarray_int16_f(generic_t x, const size_t index, const void* value, const size_t ndim, const void* shape, const char* units) {
  return generic_array_set_ndarray_int16(x, index, (const int16_t*)value, ndim, (const size_t*)shape, units);
}
int generic_array_set_ndarray_int32_f(generic_t x, const size_t index, const void* value, const size_t ndim, const void* shape, const char* units) {
  return generic_array_set_ndarray_int32(x, index, (const int32_t*)value, ndim, (const size_t*)shape, units);
}
int generic_array_set_ndarray_int64_f(generic_t x, const size_t index, const void* value, const size_t ndim, const void* shape, const char* units) {
  return generic_array_set_ndarray_int64(x, index, (const int64_t*)value, ndim, (const size_t*)shape, units);
}
int generic_array_set_ndarray_float_f(generic_t x, const size_t index, const void* value, const size_t ndim, const void* shape, const char* units) {
  return generic_array_set_ndarray_float(x, index, (const float*)value, ndim, (const size_t*)shape, units);
}
int generic_array_set_ndarray_double_f(generic_t x, const size_t index, const void* value, const size_t ndim, const void* shape, const char* units) {
  return generic_array_set_ndarray_double(x, index, (const double*)value, ndim, (const size_t*)shape, units);
}
int generic_array_set_ndarray_complex_float_f(generic_t x, const size_t index, const void* value, const size_t ndim, const void* shape, const char* units) {
  return generic_array_set_ndarray_complex_float(x, index, (const complex_float_t*)value, ndim, (const size_t*)shape, units);
}
int generic_array_set_ndarray_complex_double_f(generic_t x, const size_t index, const void* value, const size_t ndim, const void* shape, const char* units) {
  return generic_array_set_ndarray_complex_double(x, index, (const complex_double_t*)value, ndim, (const size_t*)shape, units);
}
int generic_array_set_schema_f(generic_t x, const size_t index, const generic_t value) {
  return generic_array_set_schema(x, index, value);
}
int generic_array_set_any_f(generic_t x, const size_t index, const generic_t value) {
  return generic_array_set_any(x, index, value);
}
void* generic_array_get_null_f(generic_t x, const size_t index) {
  return generic_array_get_null(x, index);
}
bool generic_array_get_bool_f(generic_t x, const size_t index) {
  return generic_array_get_bool(x, index);
}
int generic_array_get_integer_f(generic_t x, const size_t index) {
  return generic_array_get_integer(x, index);
}
double generic_array_get_number_f(generic_t x, const size_t index) {
  return generic_array_get_number(x, index);
}
const char* generic_array_get_string_f(generic_t x, const size_t index) {
  return generic_array_get_string(x, index);
}
void* generic_array_get_item_f(generic_t x, const size_t index, const char* type) {
  return generic_array_get_item(x, index, type);
}
int generic_array_get_item_nbytes_f(generic_t x, const size_t index, const char* type) {
  return generic_array_get_item_nbytes(x, index, type);
}
generic_t generic_array_get_array_f(generic_t x, const size_t index) {
  return generic_array_get_array(x, index);
}
generic_t generic_array_get_object_f(generic_t x, const size_t index) {
  return generic_array_get_object(x, index);
}
ply_t generic_array_get_ply_f(generic_t x, const size_t index) {
  return generic_array_get_ply(x, index);
}
obj_t generic_array_get_obj_f(generic_t x, const size_t index) {
  return generic_array_get_obj(x, index);
}
python_t generic_array_get_python_class_f(generic_t x, const size_t index) {
  return generic_array_get_python_class(x, index);
}
python_t generic_array_get_python_function_f(generic_t x, const size_t index) {
  return generic_array_get_python_function(x, index);
}
python_t generic_array_get_python_instance_f(generic_t x, const size_t index) {
  return generic_array_get_python_instance(x, index);
}
void* generic_array_get_scalar_f(generic_t x, const size_t index, const char* subtype, const size_t precision) {
  return generic_array_get_scalar(x, index, subtype, precision);
}
int16_t generic_array_get_int16_f(generic_t x, const size_t index) {
  return generic_array_get_int16(x, index);
}
int32_t generic_array_get_int32_f(generic_t x, const size_t index) {
  return generic_array_get_int32(x, index);
}
int64_t generic_array_get_int64_f(generic_t x, const size_t index) {
  return generic_array_get_int64(x, index);
}
float generic_array_get_float_f(generic_t x, const size_t index) {
  return generic_array_get_float(x, index);
}
double generic_array_get_double_f(generic_t x, const size_t index) {
  return generic_array_get_double(x, index);
}
complex_float_t generic_array_get_complex_float_f(generic_t x, const size_t index) {
  return generic_array_get_complex_float(x, index);
}
complex_double_t generic_array_get_complex_double_f(generic_t x, const size_t index) {
  return generic_array_get_complex_double(x, index);
}
size_t generic_array_get_1darray_f(generic_t x, const size_t index, const char* subtype, const size_t precision, void* value) {
  return generic_array_get_1darray(x, index, subtype, precision, (void**)value);
}
size_t generic_array_get_1darray_int16_f(generic_t x, const size_t index, void* value) {
  return generic_array_get_1darray_int16(x, index, (int16_t**)value);
}
size_t generic_array_get_1darray_int32_f(generic_t x, const size_t index, void* value) {
  return generic_array_get_1darray_int32(x, index, (int32_t**)value);
}
size_t generic_array_get_1darray_int64_f(generic_t x, const size_t index, void* value) {
  return generic_array_get_1darray_int64(x, index, (int64_t**)value);
}
size_t generic_array_get_1darray_float_f(generic_t x, const size_t index, void* value) {
  return generic_array_get_1darray_float(x, index, (float**)value);
}
size_t generic_array_get_1darray_double_f(generic_t x, const size_t index, void* value) {
  return generic_array_get_1darray_double(x, index, (double**)value);
}
size_t generic_array_get_1darray_complex_float_f(generic_t x, const size_t index, void* value) {
  return generic_array_get_1darray_complex_float(x, index, (complex_float_t**)value);
}
size_t generic_array_get_1darray_complex_double_f(generic_t x, const size_t index, void* value) {
  return generic_array_get_1darray_complex_double(x, index, (complex_double_t**)value);
}
size_t generic_array_get_ndarray_f(generic_t x, const size_t index, const char* subtype, const size_t precision, void* value, void* shape) {
  return generic_array_get_ndarray(x, index, subtype, precision, (void**)value, (size_t**)shape);
}
size_t generic_array_get_ndarray_int16_f(generic_t x, const size_t index, void* value, void* shape) {
  return generic_array_get_ndarray_int16(x, index, (int16_t**)value, (size_t**)shape);
}
size_t generic_array_get_ndarray_int32_f(generic_t x, const size_t index, void* value, void* shape) {
  return generic_array_get_ndarray_int32(x, index, (int32_t**)value, (size_t**)shape);
}
size_t generic_array_get_ndarray_int64_f(generic_t x, const size_t index, void* value, void* shape) {
  return generic_array_get_ndarray_int64(x, index, (int64_t**)value, (size_t**)shape);
}
size_t generic_array_get_ndarray_float_f(generic_t x, const size_t index, void* value, void* shape) {
  return generic_array_get_ndarray_float(x, index, (float**)value, (size_t**)shape);
}
size_t generic_array_get_ndarray_double_f(generic_t x, const size_t index, void* value, void* shape) {
  return generic_array_get_ndarray_double(x, index, (double**)value, (size_t**)shape);
}
size_t generic_array_get_ndarray_complex_float_f(generic_t x, const size_t index, void* value, void* shape) {
  return generic_array_get_ndarray_complex_float(x, index, (complex_float_t**)value, (size_t**)shape);
}
size_t generic_array_get_ndarray_complex_double_f(generic_t x, const size_t index, void* value, void* shape) {
  return generic_array_get_ndarray_complex_double(x, index, (complex_double_t**)value, (size_t**)shape);
}
generic_t generic_array_get_schema_f(generic_t x, const size_t index) {
  return generic_array_get_schema(x, index);
}
generic_t generic_array_get_any_f(generic_t x, const size_t index) {
  return generic_array_get_any(x, index);
}
int generic_object_set_null_f(generic_t x, const char* key, const void* value) {
  return generic_object_set_null(x, key, value);
}
int generic_object_set_bool_f(generic_t x, const char* key, const bool value) {
  return generic_object_set_bool(x, key, value);
}
int generic_object_set_integer_f(generic_t x, const char* key, const int value) {
  return generic_object_set_integer(x, key, value);
}
int generic_object_set_number_f(generic_t x, const char* key, const double value) {
  return generic_object_set_number(x, key, value);
}
int generic_object_set_string_f(generic_t x, const char* key, const char* value) {
  return generic_object_set_string(x, key, value);
}
int generic_object_set_item_f(generic_t x, const char* key, const char* type, void* value) {
  return generic_object_set_item(x, key, type, value);
}
int generic_object_set_array_f(generic_t x, const char* key, const generic_t value) {
  return generic_object_set_array(x, key, value);
}
int generic_object_set_object_f(generic_t x, const char* key, const generic_t value) {
  return generic_object_set_object(x, key, value);
}
int generic_object_set_ply_f(generic_t x, const char* key, const ply_t value) {
  return generic_object_set_ply(x, key, value);
}
int generic_object_set_obj_f(generic_t x, const char* key, const obj_t value) {
  return generic_object_set_obj(x, key, value);
}
int generic_object_set_python_class_f(generic_t x, const char* key, const python_t value) {
  return generic_object_set_python_class(x, key, value);
}
int generic_object_set_python_function_f(generic_t x, const char* key, const python_t value) {
  return generic_object_set_python_function(x, key, value);
}
int generic_object_set_python_instance_f(generic_t x, const char* key, const python_t value) {
  return generic_object_set_python_instance(x, key, value);
}
int generic_object_set_scalar_f(generic_t x, const char* key, const void* value, const char* subtype, const size_t precision, const char* units) {
  return generic_object_set_scalar(x, key, value, subtype, precision, units);
}
int generic_object_set_int16_f(generic_t x, const char* key, const int16_t value, const char* units) {
  return generic_object_set_int16(x, key, value, units);
}
int generic_object_set_int32_f(generic_t x, const char* key, const int32_t value, const char* units) {
  return generic_object_set_int32(x, key, value, units);
}
int generic_object_set_int64_f(generic_t x, const char* key, const int64_t value, const char* units) {
  return generic_object_set_int64(x, key, value, units);
}
int generic_object_set_float_f(generic_t x, const char* key, const float value, const char* units) {
  return generic_object_set_float(x, key, value, units);
}
int generic_object_set_double_f(generic_t x, const char* key, const double value, const char* units) {
  return generic_object_set_double(x, key, value, units);
}
int generic_object_set_complex_float_f(generic_t x, const char* key, const complex_float_t value, const char* units) {
  return generic_object_set_complex_float(x, key, value, units);
}
int generic_object_set_complex_double_f(generic_t x, const char* key, const complex_double_t value, const char* units) {
  return generic_object_set_complex_double(x, key, value, units);
}
int generic_object_set_1darray_f(generic_t x, const char* key, const void* value, const char* subtype, const size_t precision, const size_t length, const char* units) {
  return generic_object_set_1darray(x, key, value, subtype, precision, length, units);
}
int generic_object_set_1darray_int16_f(generic_t x, const char* key, const void* value, const size_t length, const char* units) {
  return generic_object_set_1darray_int16(x, key, (const int16_t*)value, length, units);
}
int generic_object_set_1darray_int32_f(generic_t x, const char* key, const void* value, const size_t length, const char* units) {
  return generic_object_set_1darray_int32(x, key, (const int32_t*)value, length, units);
}
int generic_object_set_1darray_int64_f(generic_t x, const char* key, const void* value, const size_t length, const char* units) {
  return generic_object_set_1darray_int64(x, key, (const int64_t*)value, length, units);
}
int generic_object_set_1darray_float_f(generic_t x, const char* key, const void* value, const size_t length, const char* units) {
  return generic_object_set_1darray_float(x, key, (const float*)value, length, units);
}
int generic_object_set_1darray_double_f(generic_t x, const char* key, const void* value, const size_t length, const char* units) {
  return generic_object_set_1darray_double(x, key, (const double*)value, length, units);
}
int generic_object_set_1darray_complex_float_f(generic_t x, const char* key, const void* value, const size_t length, const char* units) {
  return generic_object_set_1darray_complex_float(x, key, (const complex_float_t*)value, length, units);
}
int generic_object_set_1darray_complex_double_f(generic_t x, const char* key, const void* value, const size_t length, const char* units) {
  return generic_object_set_1darray_complex_double(x, key, (const complex_double_t*)value, length, units);
}
int generic_object_set_ndarray_f(generic_t x, const char* key, const void* value, const char* subtype, const size_t precision, const size_t ndim, const void* shape, const char* units) {
  return generic_object_set_ndarray(x, key, value, subtype, precision, ndim, (const size_t*)shape, units);
}
int generic_object_set_ndarray_int16_f(generic_t x, const char* key, const void* value, const size_t ndim, const void* shape, const char* units) {
  return generic_object_set_ndarray_int16(x, key, (const int16_t*)value, ndim, (const size_t*)shape, units);
}
int generic_object_set_ndarray_int32_f(generic_t x, const char* key, const void* value, const size_t ndim, const void* shape, const char* units) {
  return generic_object_set_ndarray_int32(x, key, (const int32_t*)value, ndim, (const size_t*)shape, units);
}
int generic_object_set_ndarray_int64_f(generic_t x, const char* key, const void* value, const size_t ndim, const void* shape, const char* units) {
  return generic_object_set_ndarray_int64(x, key, (const int64_t*)value, ndim, (const size_t*)shape, units);
}
int generic_object_set_ndarray_float_f(generic_t x, const char* key, const void* value, const size_t ndim, const void* shape, const char* units) {
  return generic_object_set_ndarray_float(x, key, (const float*)value, ndim, (const size_t*)shape, units);
}
int generic_object_set_ndarray_double_f(generic_t x, const char* key, const void* value, const size_t ndim, const void* shape, const char* units) {
  return generic_object_set_ndarray_double(x, key, (const double*)value, ndim, (const size_t*)shape, units);
}
int generic_object_set_ndarray_complex_float_f(generic_t x, const char* key, const void* value, const size_t ndim, const void* shape, const char* units) {
  return generic_object_set_ndarray_complex_float(x, key, (const complex_float_t*)value, ndim, (const size_t*)shape, units);
}
int generic_object_set_ndarray_complex_double_f(generic_t x, const char* key, const void* value, const size_t ndim, const void* shape, const char* units) {
  return generic_object_set_ndarray_complex_double(x, key, (const complex_double_t*)value, ndim, (const size_t*)shape, units);
}
int generic_object_set_schema_f(generic_t x, const char* key, const generic_t value) {
  return generic_object_set_schema(x, key, value);
}
int generic_object_set_any_f(generic_t x, const char* key, const generic_t value) {
  return generic_object_set_any(x, key, value);
}
void* generic_object_get_null_f(generic_t x, const char* key) {
  return generic_object_get_null(x, key);
}
bool generic_object_get_bool_f(generic_t x, const char* key) {
  return generic_object_get_bool(x, key);
}
int generic_object_get_integer_f(generic_t x, const char* key) {
  return generic_object_get_integer(x, key);
}
double generic_object_get_number_f(generic_t x, const char* key) {
  return generic_object_get_number(x, key);
}
const char* generic_object_get_string_f(generic_t x, const char* key) {
  return generic_object_get_string(x, key);
}
void* generic_object_get_item_f(generic_t x, const char* key, const char* type) {
  return generic_object_get_item(x, key, type);
}
int generic_object_get_item_nbytes_f(generic_t x, const char* key, const char* type) {
  return generic_object_get_item_nbytes(x, key, type);
}
generic_t generic_object_get_array_f(generic_t x, const char* key) {
  return generic_object_get_array(x, key);
}
generic_t generic_object_get_object_f(generic_t x, const char* key) {
  return generic_object_get_object(x, key);
}
ply_t generic_object_get_ply_f(generic_t x, const char* key) {
  return generic_object_get_ply(x, key);
}
obj_t generic_object_get_obj_f(generic_t x, const char* key) {
  return generic_object_get_obj(x, key);
}
python_t generic_object_get_python_class_f(generic_t x, const char* key) {
  return generic_object_get_python_class(x, key);
}
python_t generic_object_get_python_function_f(generic_t x, const char* key) {
  return generic_object_get_python_function(x, key);
}
python_t generic_object_get_python_instance_f(generic_t x, const char* key) {
  return generic_object_get_python_instance(x, key);
}
void* generic_object_get_scalar_f(generic_t x, const char* key, const char* subtype, const size_t precision) {
  return generic_object_get_scalar(x, key, subtype, precision);
}
int16_t generic_object_get_int16_f(generic_t x, const char* key) {
  return generic_object_get_int16(x, key);
}
int32_t generic_object_get_int32_f(generic_t x, const char* key) {
  return generic_object_get_int32(x, key);
}
int64_t generic_object_get_int64_f(generic_t x, const char* key) {
  return generic_object_get_int64(x, key);
}
float generic_object_get_float_f(generic_t x, const char* key) {
  return generic_object_get_float(x, key);
}
double generic_object_get_double_f(generic_t x, const char* key) {
  return generic_object_get_double(x, key);
}
complex_float_t generic_object_get_complex_float_f(generic_t x, const char* key) {
  return generic_object_get_complex_float(x, key);
}
complex_double_t generic_object_get_complex_double_f(generic_t x, const char* key) {
  return generic_object_get_complex_double(x, key);
}
size_t generic_object_get_1darray_f(generic_t x, const char* key, const char* subtype, const size_t precision, void* value) {
  return generic_object_get_1darray(x, key, subtype, precision, (void**)value);
}
size_t generic_object_get_1darray_int16_f(generic_t x, const char* key, void* value) {
  return generic_object_get_1darray_int16(x, key, (int16_t**)value);
}
size_t generic_object_get_1darray_int32_f(generic_t x, const char* key, void* value) {
  return generic_object_get_1darray_int32(x, key, (int32_t**)value);
}
size_t generic_object_get_1darray_int64_f(generic_t x, const char* key, void* value) {
  return generic_object_get_1darray_int64(x, key, (int64_t**)value);
}
size_t generic_object_get_1darray_float_f(generic_t x, const char* key, void* value) {
  return generic_object_get_1darray_float(x, key, (float**)value);
}
size_t generic_object_get_1darray_double_f(generic_t x, const char* key, void* value) {
  return generic_object_get_1darray_double(x, key, (double**)value);
}
size_t generic_object_get_1darray_complex_float_f(generic_t x, const char* key, void* value) {
  return generic_object_get_1darray_complex_float(x, key, (complex_float_t**)value);
}
size_t generic_object_get_1darray_complex_double_f(generic_t x, const char* key, void* value) {
  return generic_object_get_1darray_complex_double(x, key, (complex_double_t**)value);
}
size_t generic_object_get_ndarray_f(generic_t x, const char* key, const char* subtype, const size_t precision, void* value, void* shape) {
  return generic_object_get_ndarray(x, key, subtype, precision, (void**)value, (size_t**)shape);
}
size_t generic_object_get_ndarray_int16_f(generic_t x, const char* key, void* value, void* shape) {
  return generic_object_get_ndarray_int16(x, key, (int16_t**)value, (size_t**)shape);
}
size_t generic_object_get_ndarray_int32_f(generic_t x, const char* key, void* value, void* shape) {
  return generic_object_get_ndarray_int32(x, key, (int32_t**)value, (size_t**)shape);
}
size_t generic_object_get_ndarray_int64_f(generic_t x, const char* key, void* value, void* shape) {
  return generic_object_get_ndarray_int64(x, key, (int64_t**)value, (size_t**)shape);
}
size_t generic_object_get_ndarray_float_f(generic_t x, const char* key, void* value, void* shape) {
  return generic_object_get_ndarray_float(x, key, (float**)value, (size_t**)shape);
}
size_t generic_object_get_ndarray_double_f(generic_t x, const char* key, void* value, void* shape) {
  return generic_object_get_ndarray_double(x, key, (double**)value, (size_t**)shape);
}
size_t generic_object_get_ndarray_complex_float_f(generic_t x, const char* key, void* value, void* shape) {
  return generic_object_get_ndarray_complex_float(x, key, (complex_float_t**)value, (size_t**)shape);
}
size_t generic_object_get_ndarray_complex_double_f(generic_t x, const char* key, void* value, void* shape) {
  return generic_object_get_ndarray_complex_double(x, key, (complex_double_t**)value, (size_t**)shape);
}
generic_t generic_object_get_schema_f(generic_t x, const char* key) {
  return generic_object_get_schema(x, key);
}
generic_t generic_object_get_any_f(generic_t x, const char* key) {
  return generic_object_get_any(x, key);
}
#ifdef YGGDRASIL_LONG_DOUBLE_AVAILABLE
int generic_set_long_double_f(generic_t x, const long double value, const char* units) {
  return generic_set_long_double(x, value, units);
}
int generic_set_complex_long_double_f(generic_t x, const complex_long_double_t value, const char* units) {
  return generic_set_complex_long_double(x, value, units);
}
int generic_set_1darray_long_double_f(generic_t x, const void* value, const size_t length, const char* units) {
  return generic_set_1darray_long_double(x, (const long double*)value, length, units);
}
int generic_set_1darray_complex_long_double_f(generic_t x, const void* value, const size_t length, const char* units) {
  return generic_set_1darray_complex_long_double(x, (const complex_long_double_t*)value, length, units);
}
int generic_set_ndarray_long_double_f(generic_t x, const void* value, const size_t ndim, const void* shape, const char* units) {
  return generic_set_ndarray_long_double(x, (const long double*)value, ndim, (const size_t*)shape, units);
}
int generic_set_ndarray_complex_long_double_f(generic_t x, const void* value, const size_t ndim, const void* shape, const char* units) {
  return generic_set_ndarray_complex_long_double(x, (const complex_long_double_t*)value, ndim, (const size_t*)shape, units);
}
long double generic_get_long_double_f(generic_t x) {
  return generic_get_long_double(x);
}
complex_long_double_t generic_get_complex_long_double_f(generic_t x) {
  return generic_get_complex_long_double(x);
}
size_t generic_get_1darray_long_double_f(generic_t x, void* value) {
  return generic_get_1darray_long_double(x, (long double**)value);
}
size_t generic_get_1darray_complex_long_double_f(generic_t x, void* value) {
  return generic_get_1darray_complex_long_double(x, (complex_long_double_t**)value);
}
size_t generic_get_ndarray_long_double_f(generic_t x, void* value, void* shape) {
  return generic_get_ndarray_long_double(x, (long double**)value, (size_t**)shape);
}
size_t generic_get_ndarray_complex_long_double_f(generic_t x, void* value, void* shape) {
  return generic_get_ndarray_complex_long_double(x, (complex_long_double_t**)value, (size_t**)shape);
}
long double generic_ref_get_long_double_f(generic_ref_t x) {
  return generic_ref_get_long_double(x);
}
complex_long_double_t generic_ref_get_complex_long_double_f(generic_ref_t x) {
  return generic_ref_get_complex_long_double(x);
}
size_t generic_ref_get_1darray_long_double_f(generic_ref_t x, void* value) {
  return generic_ref_get_1darray_long_double(x, (long double**)value);
}
size_t generic_ref_get_1darray_complex_long_double_f(generic_ref_t x, void* value) {
  return generic_ref_get_1darray_complex_long_double(x, (complex_long_double_t**)value);
}
size_t generic_ref_get_ndarray_long_double_f(generic_ref_t x, void* value, void* shape) {
  return generic_ref_get_ndarray_long_double(x, (long double**)value, (size_t**)shape);
}
size_t generic_ref_get_ndarray_complex_long_double_f(generic_ref_t x, void* value, void* shape) {
  return generic_ref_get_ndarray_complex_long_double(x, (complex_long_double_t**)value, (size_t**)shape);
}
int generic_array_set_long_double_f(generic_t x, const size_t index, const long double value, const char* units) {
  return generic_array_set_long_double(x, index, value, units);
}
int generic_array_set_complex_long_double_f(generic_t x, const size_t index, const complex_long_double_t value, const char* units) {
  return generic_array_set_complex_long_double(x, index, value, units);
}
int generic_array_set_1darray_long_double_f(generic_t x, const size_t index, const void* value, const size_t length, const char* units) {
  return generic_array_set_1darray_long_double(x, index, (const long double*)value, length, units);
}
int generic_array_set_1darray_complex_long_double_f(generic_t x, const size_t index, const void* value, const size_t length, const char* units) {
  return generic_array_set_1darray_complex_long_double(x, index, (const complex_long_double_t*)value, length, units);
}
int generic_array_set_ndarray_long_double_f(generic_t x, const size_t index, const void* value, const size_t ndim, const void* shape, const char* units) {
  return generic_array_set_ndarray_long_double(x, index, (const long double*)value, ndim, (const size_t*)shape, units);
}
int generic_array_set_ndarray_complex_long_double_f(generic_t x, const size_t index, const void* value, const size_t ndim, const void* shape, const char* units) {
  return generic_array_set_ndarray_complex_long_double(x, index, (const complex_long_double_t*)value, ndim, (const size_t*)shape, units);
}
long double generic_array_get_long_double_f(generic_t x, const size_t index) {
  return generic_array_get_long_double(x, index);
}
complex_long_double_t generic_array_get_complex_long_double_f(generic_t x, const size_t index) {
  return generic_array_get_complex_long_double(x, index);
}
size_t generic_array_get_1darray_long_double_f(generic_t x, const size_t index, void* value) {
  return generic_array_get_1darray_long_double(x, index, (long double**)value);
}
size_t generic_array_get_1darray_complex_long_double_f(generic_t x, const size_t index, void* value) {
  return generic_array_get_1darray_complex_long_double(x, index, (complex_long_double_t**)value);
}
size_t generic_array_get_ndarray_long_double_f(generic_t x, const size_t index, void* value, void* shape) {
  return generic_array_get_ndarray_long_double(x, index, (long double**)value, (size_t**)shape);
}
size_t generic_array_get_ndarray_complex_long_double_f(generic_t x, const size_t index, void* value, void* shape) {
  return generic_array_get_ndarray_complex_long_double(x, index, (complex_long_double_t**)value, (size_t**)shape);
}
int generic_object_set_long_double_f(generic_t x, const char* key, const long double value, const char* units) {
  return generic_object_set_long_double(x, key, value, units);
}
int generic_object_set_complex_long_double_f(generic_t x, const char* key, const complex_long_double_t value, const char* units) {
  return generic_object_set_complex_long_double(x, key, value, units);
}
int generic_object_set_1darray_long_double_f(generic_t x, const char* key, const void* value, const size_t length, const char* units) {
  return generic_object_set_1darray_long_double(x, key, (const long double*)value, length, units);
}
int generic_object_set_1darray_complex_long_double_f(generic_t x, const char* key, const void* value, const size_t length, const char* units) {
  return generic_object_set_1darray_complex_long_double(x, key, (const complex_long_double_t*)value, length, units);
}
int generic_object_set_ndarray_long_double_f(generic_t x, const char* key, const void* value, const size_t ndim, const void* shape, const char* units) {
  return generic_object_set_ndarray_long_double(x, key, (const long double*)value, ndim, (const size_t*)shape, units);
}
int generic_object_set_ndarray_complex_long_double_f(generic_t x, const char* key, const void* value, const size_t ndim, const void* shape, const char* units) {
  return generic_object_set_ndarray_complex_long_double(x, key, (const complex_long_double_t*)value, ndim, (const size_t*)shape, units);
}
long double generic_object_get_long_double_f(generic_t x, const char* key) {
  return generic_object_get_long_double(x, key);
}
complex_long_double_t generic_object_get_complex_long_double_f(generic_t x, const char* key) {
  return generic_object_get_complex_long_double(x, key);
}
size_t generic_object_get_1darray_long_double_f(generic_t x, const char* key, void* value) {
  return generic_object_get_1darray_long_double(x, key, (long double**)value);
}
size_t generic_object_get_1darray_complex_long_double_f(generic_t x, const char* key, void* value) {
  return generic_object_get_1darray_complex_long_double(x, key, (complex_long_double_t**)value);
}
size_t generic_object_get_ndarray_long_double_f(generic_t x, const char* key, void* value, void* shape) {
  return generic_object_get_ndarray_long_double(x, key, (long double**)value, (size_t**)shape);
}
size_t generic_object_get_ndarray_complex_long_double_f(generic_t x, const char* key, void* value, void* shape) {
  return generic_object_get_ndarray_complex_long_double(x, key, (complex_long_double_t**)value, (size_t**)shape);
}
#endif // YGGDRASIL_LONG_DOUBLE_AVAILABLE