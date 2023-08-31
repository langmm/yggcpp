#ifndef YGG_FC_WRAPPERS_H_
#define YGG_FC_WRAPPERS_H_

#include "YggInterface.h"

#ifdef __cplusplus /* If this is a C++ compiler, use C linkage */
extern "C" {
#endif

#ifndef DOXYGEN_SHOULD_SKIP_THIS

const int YGG_MSG_MAX_F = YGG_MSG_MAX;

// Utilities
int ygg_init_f();
void ygg_c_free(void *x);
void ygg_log_info_f(const char* fmt);
void ygg_log_debug_f(const char* fmt);
void ygg_log_error_f(const char* fmt);
void set_global_comm();
void unset_global_comm();
// Methods for initializing channels
int is_comm_format_array_type_f(const comm_t x);
comm_t _init_comm_f(const char *name, const int dir, const int t,
		    void* datatype, const int flags);
comm_t ygg_output_f(const char *name);
comm_t ygg_input_f(const char *name);
comm_t yggOutputType_f(const char *name, void* datatype);
comm_t yggInputType_f(const char *name, void* datatype);
comm_t yggOutputFmt_f(const char *name, const char *fmt);
comm_t yggInputFmt_f(const char *name, const char *fmt);
comm_t yggAsciiTableOutput_f(const char *name, const char *format_str);
comm_t yggAsciiTableInput_f(const char *name);
comm_t yggAsciiArrayOutput_f(const char *name, const char *format_str);
comm_t yggAsciiArrayInput_f(const char *name);
comm_t yggPlyOutput_f(const char *name);
comm_t yggPlyInput_f(const char *name);
comm_t yggObjOutput_f(const char *name);
comm_t yggObjInput_f(const char *name);
comm_t yggGenericOutput_f(const char *name);
comm_t yggGenericInput_f(const char *name);
comm_t yggAnyOutput_f(const char *name);
comm_t yggAnyInput_f(const char *name);
comm_t yggJSONArrayOutput_f(const char *name);
comm_t yggJSONArrayInput_f(const char *name);
comm_t yggJSONObjectOutput_f(const char *name);
comm_t yggJSONObjectInput_f(const char *name);
comm_t yggRpcClient_f(const char *name, const char *out_fmt,
		      const char *in_fmt);
comm_t yggRpcServer_f(const char *name, const char *in_fmt,
		      const char *out_fmt);
comm_t yggRpcClientType_f(const char *name, void* outType, void* inType);
comm_t yggRpcServerType_f(const char *name, void* inType, void* outType);
comm_t yggTimesync_f(const char *name, const char *t_units);
// Method for constructing data types
void display_dtype_f(const dtype_t);
int is_dtype_format_array_f(dtype_t type_struct);
dtype_t create_dtype_from_schema_f(const char* schema, const bool use_generic);
dtype_t create_dtype_empty_f(const bool use_generic);
dtype_t create_dtype_python_f(void* pyobj, const bool use_generic);
dtype_t create_dtype_direct_f(const bool use_generic);
dtype_t create_dtype_default_f(const char* type, const bool use_generic);
dtype_t create_dtype_scalar_f(const char* subtype, const size_t precision,
			      const char* units, const bool use_generic);
dtype_t create_dtype_1darray_f(const char* subtype, const size_t precision,
			       const size_t length, const char* units,
			       const bool use_generic);
dtype_t create_dtype_ndarray_f(const char* subtype, const size_t precision,
			       const size_t ndim, const size_t* shape,
			       const char* units, const bool use_generic);
dtype_t create_dtype_json_array_f(const size_t nitems, void* items,
				  const bool use_generic);
dtype_t create_dtype_json_object_f(const size_t nitems, void* keys,
				   void* values, const bool use_generic);
dtype_t create_dtype_ply_f(const bool use_generic);
dtype_t create_dtype_obj_f(const bool use_generic);
dtype_t create_dtype_format_f(const char *format_str,
			      const bool as_array,
			      const bool use_generic);
dtype_t create_dtype_pyobj_f(const char* type, const bool use_generic);
dtype_t create_dtype_schema_f(const bool use_generic);
dtype_t create_dtype_any_f(const bool use_generic);
// Methods for sending/receiving
int ygg_send_f(const comm_t yggQ, const char *data, const size_t len);
int ygg_recv_f(comm_t yggQ, char *data, const size_t len);
int ygg_send_var_f(const comm_t yggQ, int nargs, void *args);
int ygg_recv_var_f(comm_t yggQ, int nargs, void *args);
int ygg_recv_var_realloc_f(comm_t yggQ, int nargs, void *args);
int rpc_send_f(const comm_t yggQ, int nargs, void *args);
int rpc_recv_f(comm_t yggQ, int nargs, void *args);
int rpc_recv_realloc_f(comm_t yggQ, int nargs, void *args);
int rpc_call_f(comm_t yggQ, int nargs, void *args);
int rpc_call_realloc_f(comm_t yggQ, int nargs, void *args);
#define GEOM_INTERFACE(name)						\
  name ## _t init_ ## name ## _f();					\
  name ## _t generate_ ## name ## _f();					\
  void free_ ## name ## _f(void *p);					\
  void set_ ## name ## _f(void* x, void* obj, int copy);		\
  name ## _t copy_ ## name ## _f(name ## _t src);			\
  void display_ ## name ## _indent_f(name ## _t p, const char* indent);	\
  void display_ ## name ## _f(name ## _t p);				\
  int nelements_ ## name ## _f(name ## _t p, const char* name);		\
  bool compare_ ## name ## _f(const name ## _t a, const name ## _t b);
GEOM_INTERFACE(ply)
GEOM_INTERFACE(obj)
#undef GEOM_INTERFACE
// Generic interface
generic_t init_generic_f();
generic_t init_generic_array_f();
generic_t init_generic_map_f();
generic_t init_generic_generate_f(const char* schema);
/* generic_t create_generic_f(void* type_class, void* data, size_t nbytes); */
int free_generic_f(void* x);
int copy_generic_into_f(void* dst, generic_t src);
generic_t copy_generic_f(generic_t src);
bool compare_generic_f(generic_t a, generic_t b);
int is_generic_init_f(generic_t x);
void display_generic_f(generic_t x);
int add_generic_array_f(generic_t arr, generic_t x);
int set_generic_array_f(generic_t arr, const size_t i, generic_t x);
int get_generic_array_f(generic_t arr, const size_t i, void *x);
int get_generic_array_ref_f(generic_t arr, const size_t i, void *x);
int set_generic_object_f(generic_t arr, const char* k, generic_t x);
int get_generic_object_f(generic_t arr, const char* k, void *x);
int get_generic_object_ref_f(generic_t arr, const char* k, void *x);
// Python interface
python_t init_python_f();
void free_python_f(void *x);
python_t copy_python_f(python_t x);
void display_python_f(python_t x);
// Interface for getting generic array elements
size_t generic_array_get_size_f(generic_t x);
void* generic_array_get_item_f(generic_t x, const size_t index, const char *type);
int generic_array_get_item_nbytes_f(generic_t x, const size_t index, const char *type);
void* generic_array_get_scalar_f(generic_t x, const size_t index,
				 const char *subtype, const size_t precision);
size_t generic_array_get_1darray_f(generic_t x, const size_t index,
				   const char *subtype, const size_t precision,
				   void* data);
size_t generic_array_get_ndarray_f(generic_t x, const size_t index,
				   const char *subtype, const size_t precision,
				   void* data, void* shape);
size_t generic_map_get_size_f(generic_t x);
void* generic_map_get_keys_f(generic_t x, void* n_keys_f, void* key_size_f);
void* generic_map_get_item_f(generic_t x, const char* key,
			   const char *type);
int generic_map_get_item_nbytes_f(generic_t x, const char* key, const char *type);
void* generic_map_get_scalar_f(generic_t x, const char* key,
			     const char *subtype, const size_t precision);
size_t generic_map_get_1darray_f(generic_t x, const char* key,
				 const char *subtype, const size_t precision,
				 void* data);
size_t generic_map_get_ndarray_f(generic_t x, const char* key,
				 const char *subtype, const size_t precision,
				 void* data, void* shape);
// Interface for setting generic array elements
int generic_array_set_item_f(generic_t x, const size_t index,
			     const char *type, void* value);
int generic_array_set_scalar_f(generic_t x, const size_t index,
			       void* value, const char *subtype,
			       const size_t precision,
			       const char* units);
int generic_array_set_1darray_f(generic_t x, const size_t index,
				void* value, const char *subtype,
				const size_t precision,
				const size_t length,
				const char* units);
int generic_array_set_ndarray_f(generic_t x, const size_t index,
				void* data, const char *subtype,
				const size_t precision,
				const size_t ndim, const void* shape,
				const char* units);
// Interface for setting generic map elements
int generic_map_set_item_f(generic_t x, const char* key,
			   const char* type, void* value);
int generic_map_set_scalar_f(generic_t x, const char* key,
			     void* value, const char *subtype,
			     const size_t precision,
			     const char* units);
int generic_map_set_1darray_f(generic_t x, const char* key,
			      void* value, const char *subtype,
			      const size_t precision,
			      const size_t length,
			      const char* units);
int generic_map_set_ndarray_f(generic_t x, const char* key,
			      void* data, const char *subtype,
			      const size_t precision,
			      const size_t ndim, const void* shape,
			      const char* units);
int init_python_API_f();
#endif // DOXYGEN_SHOULD_SKIP_THIS

#ifdef __cplusplus /* If this is a C++ compiler, end C linkage */
}
#endif

#endif /*YGG_FC_WRAPPERS_H_*/
