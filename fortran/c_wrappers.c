#include "c_wrappers.h"

#ifdef __cplusplus /* If this is a C++ compiler, use C linkage */
extern "C" {
#endif
  
#include "utils/enums.hpp"

#ifndef DOXYGEN_SHOULD_SKIP_THIS
/* void dummy_function_f(const int64_t message) { */
/*   printf("dummy function (c printf) %lld\n", message); */
/* } */
/* void dummy_function_f(const char* message) { */
/*   printf("dummy function (c printf) %s\n", message); */
/*   ygglog_info("dummy function (c)"); */
/* } */

int YGG_MSG_MAX_F = YGG_MSG_MAX;

#define CREATE_COMM_FLAG(name)				\
  FLAG_TYPE name ## _F = name

  CREATE_COMM_FLAG(COMM_FLAG_VALID);
  CREATE_COMM_FLAG(COMM_FLAG_GLOBAL);
  CREATE_COMM_FLAG(COMM_FLAG_WORKER);
  CREATE_COMM_FLAG(COMM_FLAG_DELAYED_OPEN);
  CREATE_COMM_FLAG(COMM_FLAG_CLIENT);
  CREATE_COMM_FLAG(COMM_FLAG_SERVER);
  CREATE_COMM_FLAG(COMM_FLAG_CLIENT_RESPONSE);
  CREATE_COMM_FLAG(COMM_FLAG_SERVER_RESPONSE);
  CREATE_COMM_FLAG(COMM_FLAG_ALWAYS_SEND_HEADER);
  CREATE_COMM_FLAG(COMM_FLAG_ALLOW_MULTIPLE_COMMS);
  CREATE_COMM_FLAG(COMM_FLAG_USED_SENT);
  CREATE_COMM_FLAG(COMM_FLAG_USED_RECV);
  CREATE_COMM_FLAG(COMM_FLAG_EOF_SENT);
  CREATE_COMM_FLAG(COMM_FLAG_EOF_RECV);
  CREATE_COMM_FLAG(COMM_FLAG_CLOSE_ON_EOF_RECV);
  CREATE_COMM_FLAG(COMM_FLAG_CLOSE_ON_EOF_SEND);
  CREATE_COMM_FLAG(COMM_FLAG_INTERFACE);
  CREATE_COMM_FLAG(COMM_FLAG_DELETE);
  CREATE_COMM_FLAG(COMM_FLAG_ASYNC);
  CREATE_COMM_FLAG(COMM_FLAG_ASYNC_WRAPPED);
  CREATE_COMM_FLAG(COMM_FLAG_SET_OPP_ENV);
  CREATE_COMM_FLAG(COMM_FLAG_WRAPPER);
  CREATE_COMM_FLAG(COMM_FLAG_FORK_CYCLE);
  CREATE_COMM_FLAG(COMM_FLAG_FORK_BROADCAST);
  CREATE_COMM_FLAG(COMM_FLAG_FORK_COMPOSITE);
  CREATE_COMM_FLAG(COMM_FLAG_FORK_TINE);
  CREATE_COMM_FLAG(FILE_FLAG_APPEND);
  CREATE_COMM_FLAG(FILE_FLAG_BINARY);
  CREATE_COMM_FLAG(FILE_FLAG_READLINE);

#undef CREATE_COMM_FLAG


// Utilities
int ygg_init_f() {
  return ygg_init();
}

void ygg_c_free(void *x) {
  if (x != NULL) {
    free(x);
  }
}

void ygg_log_info_f(const char* fmt) {
  ygglog_info(fmt);
}
void ygg_log_debug_f(const char* fmt) {
  ygglog_debug(fmt);
}
void ygg_log_error_f(const char* fmt) {
  ygglog_error(fmt);
}

void set_global_comm_f() {
  global_scope_comm_on_c();
}
void unset_global_comm_f() {
  global_scope_comm_off_c();
}

// Methods for initializing channels
int is_comm_format_array_type_f(const comm_t x) {
  return is_comm_format_array_type(x);
}

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

comm_t ygg_output_f(const char *name) {
  return yggOutput(name);
}

comm_t ygg_input_f(const char *name) {
  return yggInput(name);
}

comm_t yggOutputType_f(const char *name, void* datatype) {
  return yggOutputType(name, (dtype_t*)datatype);
}

comm_t yggInputType_f(const char *name, void* datatype) {
  return yggInputType(name, (dtype_t*)datatype);
}

comm_t yggOutputFmt_f(const char *name, const char *fmt) {
  return yggOutputFmt(name, fmt);
}

comm_t yggInputFmt_f(const char *name, const char *fmt) {
  return yggInputFmt(name, fmt);
}

comm_t yggAsciiTableOutput_f(const char *name, const char *format_str) {
  return yggAsciiTableOutput(name, format_str);
}

comm_t yggAsciiTableInput_f(const char *name) {
  return yggAsciiTableInput(name);
}

comm_t yggAsciiArrayOutput_f(const char *name, const char *format_str) {
  return yggAsciiArrayOutput(name, format_str);
}

comm_t yggAsciiArrayInput_f(const char *name) {
  return yggAsciiArrayInput(name);
}

comm_t yggPlyOutput_f(const char *name) {
  return yggPlyOutput(name);
}

comm_t yggPlyInput_f(const char *name) {
  return yggPlyInput(name);
}

comm_t yggObjOutput_f(const char *name) {
  return yggObjOutput(name);
}

comm_t yggObjInput_f(const char *name) {
  return yggObjInput(name);
}

comm_t yggGenericOutput_f(const char *name) {
  return yggGenericOutput(name);
}

comm_t yggGenericInput_f(const char *name) {
  return yggGenericInput(name);
}

comm_t yggAnyOutput_f(const char *name) {
  return yggAnyOutput(name);
}

comm_t yggAnyInput_f(const char *name) {
  return yggAnyInput(name);
}

comm_t yggJSONArrayOutput_f(const char *name) {
  return yggJSONArrayOutput(name);
}

comm_t yggJSONArrayInput_f(const char *name) {
  return yggJSONArrayInput(name);
}

comm_t yggJSONObjectOutput_f(const char *name) {
  return yggJSONObjectOutput(name);
}

comm_t yggJSONObjectInput_f(const char *name) {
  return yggJSONObjectInput(name);
}

comm_t yggRpcClient_f(const char *name, const char *out_fmt,
		      const char *in_fmt) {
  return yggRpcClient(name, out_fmt, in_fmt);
}

comm_t yggRpcServer_f(const char *name, const char *in_fmt,
		      const char *out_fmt) {
  return yggRpcServer(name, in_fmt, out_fmt);
}

comm_t yggRpcClientType_f(const char *name, void* outType, void* inType) {
  return yggRpcClientType(name, (dtype_t*)outType, (dtype_t*)inType);
}
comm_t yggRpcServerType_f(const char *name, void* inType, void* outType) {
  return yggRpcServerType(name, (dtype_t*)inType, (dtype_t*)outType);
}

comm_t yggTimesync_f(const char *name, const char *t_units) {
  return yggTimesync(name, t_units);
}

// Method for constructing data types
void display_dtype_f(const dtype_t datatype) {
  display_dtype(datatype, "");
}
int is_dtype_format_array_f(dtype_t type_struct) {
  return is_dtype_format_array(type_struct);
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

dtype_t create_dtype_scalar_f(const char* subtype, const size_t precision,
			    const char* units, const bool use_generic) {
  return create_dtype_scalar(subtype, precision, units, use_generic);
}

dtype_t create_dtype_1darray_f(const char* subtype, const size_t precision,
			     const size_t length, const char* units,
			     const bool use_generic) {
  return create_dtype_1darray(subtype, precision, length,
				     units, use_generic);
}

dtype_t create_dtype_ndarray_f(const char* subtype, const size_t precision,
			     const size_t ndim, const size_t* shape,
			     const char* units, const bool use_generic) {
  return create_dtype_ndarray(subtype, precision, ndim, shape,
				     units, use_generic);
}

dtype_t create_dtype_json_array_f(const size_t nitems, void* items,
				  const bool use_generic) {
  return create_dtype_json_array(nitems, (dtype_t*)items,
					use_generic);
}

dtype_t create_dtype_json_object_f(const size_t nitems, void* keys,
				 void* values, const bool use_generic) {
  return create_dtype_json_object(nitems, (const char**)keys,
				  (dtype_t*)values, use_generic);
}

dtype_t create_dtype_ply_f(const bool use_generic) {
  return create_dtype_ply(use_generic);
}

dtype_t create_dtype_obj_f(const bool use_generic) {
  return create_dtype_obj(use_generic);
}

dtype_t create_dtype_format_f(const char *format_str,
			      const bool as_array,
			      const bool use_generic) {
  return create_dtype_format(format_str, as_array, use_generic);
}

dtype_t create_dtype_pyobj_f(const char* type, const bool use_generic) {
  return create_dtype_pyobj(type, use_generic);
}

dtype_t create_dtype_schema_f(const bool use_generic) {
  return create_dtype_schema(use_generic);
}

dtype_t create_dtype_any_f(const bool use_generic) {
  return create_dtype_any(use_generic);
}

// Methods for sending/receiving
int ygg_send_f(const comm_t yggQ, const char *data, const size_t len) {
  return ygg_send(yggQ, data, len);
}

int ygg_recv_f(comm_t yggQ, char *data, const size_t len) {
  return ygg_recv(yggQ, data, len);
}

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

#define GEOM_INTERFACE(name)						\
  name ## _t init_ ## name ## _f() {					\
    return init_ ## name();						\
  }									\
  name ## _t generate_ ## name ## _f() {				\
    return generate_ ## name();						\
  }									\
  void free_ ## name ## _f(void *p) {					\
    free_ ## name((name ## _t*)p);					\
  }									\
  void set_ ## name ## _f(void* x, void* obj, int copy) {		\
    set_ ## name((name ## _t*)x, obj, copy);				\
  }									\
  name ## _t copy_ ## name ## _f(name ## _t src) {			\
    return copy_ ## name(src);						\
  }									\
  void display_ ## name ## _indent_f(name ## _t p, const char* indent) { \
    display_ ## name ## _indent(p, indent);				\
  }									\
  void display_ ## name ## _f(name ## _t p) {				\
    display_ ## name(p);						\
  }									\
  int nelements_ ## name ## _f(name ## _t p, const char* name) {	\
    return nelements_ ## name(p, name);					\
  }									\
  bool compare_ ## name ## _f(const name ## _t a, const name ## _t b) {	\
    return compare_ ## name(a, b);					\
  }
GEOM_INTERFACE(ply)
GEOM_INTERFACE(obj)
#undef GEOM_INTERFACE

// Generic interface
generic_t init_generic_f() {
  return init_generic();
}

generic_t init_generic_array_f() {
  return init_generic_array();
}

generic_t init_generic_map_f() {
  return init_generic_map();
}

generic_t init_generic_generate_f(const char* schema) {
  return init_generic_generate(schema);
}

/* generic_t create_generic_f(void* type_class, void* data, size_t nbytes) { */
/*   return create_generic((dtype_t*)type_class, data, nbytes); */
/* } */

int free_generic_f(void* x) {
  return destroy_generic((generic_t*)x);
}

int copy_generic_into_f(void* dst, generic_t src) {
  return copy_generic_into((generic_t*)dst, src);
}

generic_t copy_generic_f(generic_t src) {
  return copy_generic(src);
}

bool compare_generic_f(generic_t a, generic_t b) {
  return compare_generic(a, b);
}

int is_generic_init_f(generic_t x) {
  return is_generic_init(x);
}

void display_generic_f(generic_t x) {
  display_generic(x);
}

int add_generic_array_f(generic_t arr, generic_t x) {
  return add_generic_array(arr, x);
}

int set_generic_array_f(generic_t arr, const size_t i, generic_t x) {
  return set_generic_array(arr, i, x);
}

int get_generic_array_f(generic_t arr, const size_t i, void* x) {
  return get_generic_array(arr, i, (generic_t*)x);
}

int get_generic_array_ref_f(generic_t arr, const size_t i, void* x) {
  return get_generic_array_ref(arr, i, (generic_ref_t*)x);
}

int set_generic_object_f(generic_t arr, const char* k, generic_t x) {
  return set_generic_object(arr, k, x);
}

int get_generic_object_f(generic_t arr, const char* k, void* x) {
  return get_generic_object(arr, k, (generic_t*)x);
}

int get_generic_object_ref_f(generic_t arr, const char* k, void* x) {
  return get_generic_object_ref(arr, k, (generic_ref_t*)x);
}

// Python interface
python_t init_python_f() {
  python_t out = init_python();
  return out;
}

void free_python_f(void *x) {
  destroy_python((python_t*)x);
}

python_t copy_python_f(python_t x) {
  python_t out = copy_python(x);
  return out;
}

void display_python_f(python_t x) {
  display_python(x);
}

// Interface for getting/setting generic array elements
size_t generic_array_get_size_f(generic_t x) {
  return generic_array_get_size(x);
}

void* generic_array_get_item_f(generic_t x, const size_t index, const char *type) {
  return generic_array_get_item(x, index, type);
}

int generic_array_get_item_nbytes_f(generic_t x, const size_t index, const char* type) {
  return generic_array_get_item_nbytes(x, index, type);
}

void* generic_array_get_scalar_f(generic_t x, const size_t index,
				 const char *subtype, const size_t precision) {
  return generic_array_get_scalar(x, index, subtype, precision);
}

size_t generic_array_get_1darray_f(generic_t x, const size_t index,
				   const char *subtype, const size_t precision,
				   void* data) {
  return generic_array_get_1darray(x, index, subtype, precision, (void**)data);
}

size_t generic_array_get_ndarray_f(generic_t x, const size_t index,
				   const char *subtype, const size_t precision,
				   void* data, void* shape) {
  return generic_array_get_ndarray(x, index, subtype, precision, (void**)data, (size_t**)shape);
}

int generic_array_set_item_f(generic_t x, const size_t index,
			     const char *type, void* value) {
  return generic_array_set_item(x, index, type, value);
}

int generic_array_set_scalar_f(generic_t x, const size_t index,
			       void* value, const char *subtype,
			       const size_t precision,
			       const char* units) {
  return generic_array_set_scalar(x, index, value, subtype, precision, units);
}

int generic_array_set_1darray_f(generic_t x, const size_t index,
				void* value, const char *subtype,
				const size_t precision,
				const size_t length,
				const char* units) {
  return generic_array_set_1darray(x, index, value, subtype, precision, length, units);
}

int generic_array_set_ndarray_f(generic_t x, const size_t index,
				void* data, const char *subtype,
				const size_t precision,
				const size_t ndim, const void* shape,
				const char* units) {
  return generic_array_set_ndarray(x, index, data, subtype, precision,
				   ndim, (const size_t*)shape, units);
}


// Interface for getting/setting generic map elements
size_t generic_map_get_size_f(generic_t x) {
  return generic_map_get_size(x);
}

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

void* generic_map_get_item_f(generic_t x, const char* key,
			     const char *type) {
  return generic_map_get_item(x, key, type);
}

int generic_map_get_item_nbytes_f(generic_t x, const char* key, const char* type) {
  return generic_map_get_item_nbytes(x, key, type);
}

void* generic_map_get_scalar_f(generic_t x, const char* key,
			       const char *subtype, const size_t precision) {
  return generic_map_get_scalar(x, key, subtype, precision);
}

size_t generic_map_get_1darray_f(generic_t x, const char* key,
				 const char *subtype, const size_t precision,
				 void* data) {
  return generic_map_get_1darray(x, key, subtype, precision, (void**)data);
}

size_t generic_map_get_ndarray_f(generic_t x, const char* key,
				 const char *subtype, const size_t precision,
				 void* data, void* shape) {
  return generic_map_get_ndarray(x, key, subtype, precision, (void**)data, (size_t**)shape);
}

int generic_map_set_item_f(generic_t x, const char* key,
			   const char* type, void* value) {
  return generic_map_set_item(x, key, type, value);
}

int generic_map_set_scalar_f(generic_t x, const char* key,
			     void* value, const char *subtype,
			     const size_t precision,
			     const char* units) {
  return generic_map_set_scalar(x, key, value, subtype, precision, units);
}

int generic_map_set_1darray_f(generic_t x, const char* key,
			      void* value, const char *subtype,
			      const size_t precision,
			      const size_t length,
			      const char* units) {
  return generic_map_set_1darray(x, key, value, subtype, precision,
				 length, units);
}

int generic_map_set_ndarray_f(generic_t x, const char* key,
			      void* data, const char *subtype,
			      const size_t precision,
			      const size_t ndim, const void* shape,
			      const char* units) {
  return generic_map_set_ndarray(x, key, data, subtype, precision,
				 ndim, (const size_t*)shape, units);
}

int init_python_API_f() {
  return init_python_API();
}

#endif // DOXYGEN_SHOULD_SKIP_THIS

#ifdef __cplusplus /* If this is a C++ compiler, use C linkage */
}
#endif
