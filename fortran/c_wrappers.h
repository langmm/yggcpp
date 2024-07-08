/* #ifndef YGG_FC_WRAPPERS_H_ */
/* #define YGG_FC_WRAPPERS_H_ */
#pragma once
#include <stdlib.h>
#include <string.h>
#include "YggInterface_fortran_export.h"
#include "YggInterface.h"
#ifdef YGG_FORTRAN_WRAP_ENUMS
#include "c_wrappers_enums.h"
#endif

#ifdef __cplusplus /* If this is a C++ compiler, use C linkage */
extern "C" {
#endif

#ifndef DOXYGEN_SHOULD_SKIP_THIS

  extern FYGG_API int YGG_MSG_MAX_F;

/* void dummy_function_f(const int64_t message); */
/* void dummy_function_f(const char* message); */

// Utilities
void setenv_f(const char* name, const char* value);
void unsetenv_f(const char* name);
void ygg_c_free(void *x);
void register_function_f(const char* name, c_function func);
  // bool (*func)(generic_t, generic_t));
// Methods for initializing channels
comm_t _init_comm_f(const char *name, const int dir, const int t,
		    void* datatype, const int64_t flags, const size_t ncomm);
// Method for constructing data types
void display_dtype_f(const dtype_t);
// Methods for sending/receiving
int ygg_send_var_f(const comm_t yggQ, int nargs, void *args);
int ygg_recv_var_f(comm_t yggQ, int nargs, void *args);
int ygg_recv_var_realloc_f(comm_t yggQ, int nargs, void *args);
int rpc_send_f(const comm_t yggQ, int nargs, void *args);
int rpc_recv_f(comm_t yggQ, int nargs, void *args);
int rpc_recv_realloc_f(comm_t yggQ, int nargs, void *args);
int rpc_call_f(comm_t yggQ, int nargs, void *args);
int rpc_call_realloc_f(comm_t yggQ, int nargs, void *args);

// Interface for getting generic array elements
void* generic_map_get_keys_f(generic_t x, void* n_keys_f, void* key_size_f);
  
#endif // DOXYGEN_SHOULD_SKIP_THIS

// LINES AFTER THIS WERE GENERATED AND SHOULD NOT BE MODIFIED DIRECTLY
//====================================================================
/**
 * @brief Constructor for comm_t structure with format.
 *   Create a comm_t structure based on a provided name that is used to
 *   locate a particular comm address stored in the environment variable name
 *   and a format string that can be used to format arguments into outgoing
 *   messages for the queue.
 * @param[in] name constant character pointer to name of queue.
 * @param[in] fmtString character pointer to format string.
 * @returns comm_t output queue structure.
 */
comm_t ygg_output_fmt_f(const char* name, const char* fmtString);
/**
 * @brief Constructor for comm_t structure with format.
 *   Create a comm_t structure based on a provided name that is used to
 *   locate a particular comm address stored in the environment variable name
 *   and a format stirng that can be used to extract arguments from received
 *   messages.
 * @param[in] name constant character pointer to name of queue.
 * @param[in] fmtString character pointer to format string.
 * @returns comm_t input queue structure.
 */
comm_t ygg_input_fmt_f(const char* name, const char* fmtString);
/**
 * @brief Send a message to an output queue.
 *   Send a message smaller than YGG_MSG_MAX bytes to an output queue. If the
 *   message is larger, it will not be sent.
 * @param[in] yggQ comm_t structure that queue should be sent to.
 * @param[in] data character pointer to message that should be sent.
 * @param[in] len size_t length of message to be sent.
 * @returns int 0 if send succesfull, -1 if send unsuccessful.
 */
int ygg_send_f(const comm_t yggQ, const char* data, const size_t len);
/**
 * @brief Send EOF message to the output queue.
 * @param[in] yggQ comm_t structure that message should be sent to.
 * @returns int 0 if send successfull, -1 if unsuccessful.
 */
int ygg_send_eof_f(const comm_t yggQ);
/**
 * @brief Receive a message from an input queue.
 *   Receive a message smaller than YGG_MSG_MAX bytes from an input queue.
 * @param[in] yggQ Communicator that message should be sent to.
 * @param[out] data Pointer to allocated buffer where the message
 *   should be saved.
 * @param[in] len Length of the allocated message buffer in bytes.
 * @returns -1 if message could not be received. Length of the received
 *   message if message was received.
 */
long ygg_recv_f(comm_t yggQ, char* data, const size_t len);
/**
 * @brief Constructor for client side RPC structure with explicit type info.
 *   Creates an instance of yggRpc_t with provided information.
 * @param[in] name constant character pointer to name for queues.
 * @param[in] outType Pointer to a dtype_t data structure containing type info
 *   for data that will be sent by the client.
 * @param[in] inType Pointer to a dtype_t data structure containing type info
 *   for data that will be received by the client.
 * @return yggRpc_t structure with provided info.
 */
comm_t ygg_rpc_client_type_f(const char* name, void* outType, void* inType);
/**
 * @brief Constructor for server side RPC structure with explicit type info.
 *   Creates an instance of yggRpc_t with provided information.
 * @param[in] name constant character pointer to name for queues.
 * @param[in] inType Pointer to a dtype_t data structure containing type info
 *   for data that will be received by the server.
 * @param[in] outType Pointer to a dtype_t data structure containing type info
 *   for data that will be sent by the server.
 * @return yggRpc_t structure with provided info.
 */
comm_t ygg_rpc_server_type_f(const char* name, void* inType, void* outType);
/**
 * @brief Constructor for client side RPC structure.
 *   Creates an instance of yggRpc_t with provided information.
 * @param[in] name constant character pointer to name for queues.
 * @param[in] outFormat character pointer to format that should be used for
 *   formatting output.
 * @param[in] inFormat character pointer to format that should be used for
 *   parsing input.
 * @return yggRpc_t structure with provided info.
 */
comm_t ygg_rpc_client_f(const char* name, const char* outFormat, const char* inFormat);
/**
 * @brief Constructor for server side RPC structure.
 *   Creates an instance of yggRpc_t with provided information.
 * @param[in] name constant character pointer to name for queues.
 * @param[in] inFormat character pointer to format that should be used for
 *   parsing input.
 * @param[in] outFormat character pointer to format that should be used for
 *   formatting output.
 * @return yggRpc_t structure with provided info.
 */
comm_t ygg_rpc_server_f(const char* name, const char* inFormat, const char* outFormat);
/**
 * @brief Constructor for client side timestep synchronization calls.
 *   Creates an instance of comm_t with provided information.
 * @param[in] name constant character pointer to name for queues.
 * @param[in] t_units const char* Units that should be used for the
 *   timestep. "" indicates no units.
 * @return comm_t structure with provided info.
 */
comm_t ygg_timesync_f(const char* name, const char* t_units);
/**
 * @brief Constructor for table output comm to an output channel.
 * @param[in] name constant character pointer to output channel name.
 * @param[in] format_str character pointer to format string that should be used
 *   to format rows into table lines.
 * @returns comm_t output structure.
 */
comm_t ygg_ascii_table_output_f(const char* name, const char* format_str);
/**
 * @brief Constructor for AsciiTable input comm from an input channel.
 * @param[in] name constant character pointer to input channel name.
 * @returns comm_t input structure.
 */
comm_t ygg_ascii_table_input_f(const char* name);
/**
 * @brief Constructor for table output comm with array output.
 * @param[in] name constant character pointer to an output channel name.
 * @param[in] format_str character pointer to format string that should be
 *   used to format rows into table lines.
 * @returns comm_t output structure.
 */
comm_t ygg_ascii_array_output_f(const char* name, const char* format_str);
/**
 * @brief Constructor for AsciiTable input comm with array input.
 * @param[in] name constant character pointer to an input channel name.
 * @returns comm_t input structure.
 */
comm_t ygg_ascii_array_input_f(const char* name);
/**
 * @brief Constructor for ply output comm to an output channel.
 * @param[in] name constant character pointer to output channel name.
 * @returns comm_t output structure.
 */
comm_t ygg_ply_output_f(const char* name);
/**
 * @brief Constructor for ply input comm from an input channel.
 * @param[in] name constant character pointer to input channel name.
 * @returns comm_t input structure.
 */
comm_t ygg_ply_input_f(const char* name);
/**
 * @brief Constructor for obj output comm to an output channel.
 * @param[in] name constant character pointer to output channel name.
 * @returns comm_t output structure.
 */
comm_t ygg_obj_output_f(const char* name);
/**
 * @brief Constructor for obj input comm from an input channel.
 * @param[in] name constant character pointer to input channel name.
 * @returns comm_t input structure.
 */
comm_t ygg_obj_input_f(const char* name);
/**
 * @brief Constructor for generic output comm to an output channel.
 * @param[in] name constant character pointer to output channel name.
 * @returns comm_t output structure.
 */
comm_t ygg_generic_output_f(const char* name);
/**
 * @brief Constructor for generic input comm from an input channel.
 * @param[in] name constant character pointer to input channel name.
 * @returns comm_t input structure.
 */
comm_t ygg_generic_input_f(const char* name);
/**
 * @brief Constructor for generic output comm to an output channel.
 * @param[in] name constant character pointer to output channel name.
 * @returns comm_t output structure.
 */
comm_t ygg_any_output_f(const char* name);
/**
 * @brief Constructor for generic input comm from an input channel.
 * @param[in] name constant character pointer to input channel name.
 * @returns comm_t input structure.
 */
comm_t ygg_any_input_f(const char* name);
/**
 * @brief Constructor for vector output comm to an output channel.
 * @param[in] name constant character pointer to output channel name.
 * @returns comm_t output structure.
 */
comm_t ygg_json_array_output_f(const char* name);
/**
 * @brief Constructor for vector input comm from an input channel.
 * @param[in] name constant character pointer to input channel name.
 * @returns comm_t input structure.
 */
comm_t ygg_json_array_input_f(const char* name);
/**
 * @brief Constructor for map output comm to an output channel.
 * @param[in] name constant character pointer to output channel name.
 * @returns comm_t output structure.
 */
comm_t ygg_json_object_output_f(const char* name);
/**
 * @brief Constructor for map input comm from an input channel.
 * @param[in] name constant character pointer to input channel name.
 * @returns comm_t input structure.
 */
comm_t ygg_json_object_input_f(const char* name);
/**
 * @brief Write a log message at the ERROR level. This will also cause
 *   the calling model to return an error code on exit.
 * @param[in] fmt Log message.
 * @param[in] ... Additional arguments are formated into the log message
 *   via sprintf.
 */
void ygglog_error_f(const char* fmt);
/**
 * @brief Write a log message at the DEBUG level.
 * @param[in] fmt Log message.
 * @param[in] ... Additional arguments are formated into the log message
 *   via sprintf.
 */
void ygglog_debug_f(const char* fmt);
/**
 * @brief Write a log message at the INFO level.
 * @param[in] fmt Log message.
 * @param[in] ... Additional arguments are formated into the log message
 *   via sprintf.
 */
void ygglog_info_f(const char* fmt);
/**
 *   Initialize yggdrasil interface.
 */
int ygg_init_f();
/**
 *   Cleanup yggdrasil interface prior to exit.
 */
void ygg_exit_f();
/**
 * @brief Delete the underlying communicator
 * @param comm The communicator to delete
 */
void free_comm_f(void* comm);
/**
 * @brief Close the underlying communicator
 * @param comm The communicator to close
 */
void close_comm_f(void* comm);
/**
 *   Set a communicators datatype based on a C-style format string.
 * @param comm Communicator
 * @param fmt C-style format string
 * @return 1 if successful, 0 otherwise.
 */
int set_response_format_f(comm_t comm, const char* fmt);
/**
 * @brief Set a communicators datatype.
 * @param x Communicator
 * @param datatype Pointer to datatype. The underlying data will be
 *   consumed.
 * @return 1 if successful, 0 otherwise.
 */
int set_response_datatype_f(comm_t x, void* datatype);
/**
 * @brief Wait for a message to become available to receive.
 * @param x Communicator
 * @param tout Time (in micro-seconds) that should be waited. If -1
 *   the process will wait forever.
 * @return Number of messages available for receive. -1 if an error
 *   occurred.
 */
int comm_wait_for_recv_f(const comm_t x, const int64_t tout);
/**
 * @brief Get the datatype associated with a communicator.
 * @param x Communicator
 * @return The datatype
 */
dtype_t comm_get_datatype_f(comm_t x);
/**
 * @brief Set the datatype associated with a communicator.
 * @param x Communicator
 * @param datatype The datatype
 * @return 1 if successful, 0 otherwise.
 */
int comm_set_datatype_f(comm_t x, void* datatype);
/**
 * @brief Send a message with the given communicator
 * @param comm The communicator to use
 * @param data The message to send
 * @param len The size of data in bytes
 * @return Any value greater than 0 indicates success
 */
int comm_send_f(comm_t comm, const char* data, const size_t len);
/**
 * @brief Send an end-of-file notification on the given communicator
 * @param comm The communicator to use
 * @return Any value greater than 0 indicates success
 */
int comm_send_eof_f(comm_t comm);
/**
 * @brief Determine if a communicator's datatype indicates an table of
 *   arrays
 * @param x The communicator to check.
 * @return 1 if true, 0 otherwise.
 */
int is_comm_format_array_type_f(const comm_t x);
/**
 * @brief Receive a message with the given communicator
 * @param comm The communicator to use
 * @param data An allocated buffer to put the received message into
 * @param len The size of the allocated buffer in data
 * @return On success, the size of the received message will be returned.
 *   Negative values indicate there was an error.
 */
long comm_recv_f(comm_t comm, char* data, const size_t len);
/**
 * @brief The number of messages in the communicators queue
 * @param comm The communicator to query
 * @return The number of messages in the queue
 */
int comm_nmsg_f(comm_t comm);
/**
 * @brief Send a message from a list of pointers
 * @param comm The communicator to use
 * @param nargs The number of arguments
 * @param ptrs Pointer array of pointers of the data to send
 * @param for_fortran If set to true then the list is of explicit fortran pointers
 * @return Any value greater than 0 indicates success
 */
int pcomm_send_f(const comm_t comm, const size_t nargs, void* ptrs, const int for_fortran);
/**
 * @brief Receive a messag into a list of pointers
 * @param comm The communciator to use
 * @param allow_realloc If true then the number of pointers may change based on the message contents
 * @param nargs The number of pointers
 * @param ptrs Pointer array of pointers to hold the message
 * @param for_fortran If true then the list is of explicit fortran pointers
 * @return Any value greater than 0 indicates success
 */
long pcomm_recv_f(comm_t comm, const int allow_realloc, const size_t nargs, void* ptrs, const int for_fortran);
/**
 * @brief Send a request and receive a response from a list of pointers containing data for both the request and response.
 * @param comm The communciator to use
 * @param allow_realloc If true then the number of pointers may change based on the message contents
 * @param nargs The number of pointers
 * @param ptrs Pointer array of pointers to hold the message
 * @param for_fortran If true then the list is of explicit fortran pointers
 * @return Any value greater than 0 indicates success
 */
long pcomm_call_f(comm_t comm, const int allow_realloc, const size_t nargs, void* ptrs, const int for_fortran);
void set_global_comm_f();
void unset_global_comm_f();
/**
 * @brief Get the length of a C string stored in a pointer.
 * @param[in] x String pointer.
 * @returns Length of the string.
 */
size_t pointer_strlen_f(const void* x);
/**
 * @brief Initialize an empty generic object.
 * @returns generic_t New generic object structure.
 */
generic_t init_generic_f();
/**
 * @brief Initialize an empty generic reference object.
 * @param[in] parent Generic object that will be the parent of the
 *   returned reference object.
 * @returns New generic reference object structure.
 */
generic_ref_t init_generic_ref_f(generic_t parent);
/**
 * @brief Initialize an empty generic object with a null JSON document
 * @returns generic_t New generic object structure.
 */
generic_t init_generic_null_f();
/**
 * @brief Initialize an empty array of mixed types with generic wrappers.
 * @returns generic_t New generic object structure containing an empty
 *   array.
 */
generic_t init_generic_array_f();
/**
 * @brief Initialize an empty map (JSON object) of mixed types with
 *   generic wrappers.
 * @returns New generic object structure contaiing an empty map (JSON
 *   object).
 */
generic_t init_generic_map_f();
/**
 * @brief Initialize a generic object from a JSON string.
 * @param[in] json JSON encoded string.
 * @returns New generic object structure wrapping a rapidjson::Document
 *   instance.
 */
generic_t init_generic_json_f(const char* json);
/**
 * @brief Initialize a generic object from a JSON string.
 * @param[in] schema JSON encoded schema describing object to generate.
 * @returns New generic object structure wrapping a rapidjson::Document
 *   instance.
 */
generic_t init_generic_generate_f(const char* schema);
/**
 * @brief Determine if a generic structure is initialized.
 * @param[in] x Generic structure to test.
 * @returns 1 if the structure is initialized, 0 otherwise.
 */
int is_generic_init_f(const generic_t x);
/**
 * @brief Determine if a generic reference structure is initialized.
 * @param[in] x Generic reference structure to test.
 * @returns 1 if the structure is initialized, 0 otherwise.
 */
int is_generic_ref_init_f(const generic_ref_t x);
/**
 * @brief Destroy a generic object.
 * @param[in] x generic_t* Pointer to generic object structure to destory.
 * @returns int -1 if unsuccessful, 0 otherwise.
 */
int free_generic_f(void* x);
/**
 * @brief Copy data from one generic object into another.
 * @param[in,out] dst Pointer to destination object.
 * @param[in] src Source object.
 * @returns int -1 if unsuccessful, 0 otherwise.
 */
int copy_generic_into_f(void* dst, const generic_t src);
/**
 * @brief Copy data from one generic object to the other.
 * @param[in] src generic_t Generic structure that data should be copied from.
 * @returns generic_t Copied structure.
 */
generic_t copy_generic_f(const generic_t src);
/**
 * @brief Compare two generic objects.
 * @param[in] a First object for comparison.
 * @param[in] b Second object for comparison.
 * @returns true if the two objects are equivalent, false otherwise.
 */
bool compare_generic_f(const generic_t a, const generic_t b);
/**
 * @brief Display information about the generic type.
 * @param[in] x generic_t* Wrapper for generic object.
 */
void display_generic_f(const generic_t x);
/**
 * @brief Add an element to the end of an array of generic elements.
 * @param[in] arr generic_t Array to add element to.
 * @param[in] x generic_t Element to add.
 * @returns int Flag that is 1 if there is an error and 0 otherwise.
 */
int add_generic_array_f(generic_t arr, const generic_t x);
/**
 * @brief Set an element in the array at a given index to a new value.
 * @param[in] arr generic_t Array to add element to.
 * @param[in] i size_t Index where element should be added.
 * @param[in] x generic_t Element to add.
 * @returns int Flag that is 1 if there is an error and 0 otherwise.
 */
int set_generic_array_f(generic_t arr, const size_t i, const generic_t x);
/**
 * @brief Get an element from an array.
 * @param[in] arr generic_t Array to get element from.
 * @param[in] i size_t Index of element to get.
 * @param[out] x generic_t* Pointer to address where element should be
 *   stored.
 * @returns int Flag that is 1 if there is an error and 0 otherwise.
 */
int get_generic_array_f(const generic_t arr, const size_t i, void* x);
int get_generic_array_ref_f(const generic_t arr, const size_t i, void* x);
/**
 * @brief Set an element in the object at for a given key to a new value.
 * @param[in] arr generic_t Object to add element to.
 * @param[in] k const char* Key where element should be added.
 * @param[in] x generic_t Element to add.
 * @returns int Flag that is 1 if there is an error and 0 otherwise.
 */
int set_generic_object_f(generic_t arr, const char* k, const generic_t x);
/**
 * @brief Get an element from an object.
 * @param[in] arr generic_t Object to get element from.
 * @param[in] k const char* Key of element to return.
 * @param[out] x generic_t* Pointer to address where element should be
 *   stored.
 * @returns int Flag that is 1 if there is an error and 0 otherwise.
 */
int get_generic_object_f(const generic_t arr, const char* k, void* x);
int get_generic_object_ref_f(const generic_t arr, const char* k, void* x);
/**
 * @brief Determine if a map object has a certain key.
 * @param[in] x generic_t Generic object that is presumed to contain a map.
 * @param[in] key char* Key to check for.
 * @returns int 1 if the key is present, 0 otherwise.
 */
int generic_map_has_key_f(const generic_t x, const char* key);
/**
 * @brief Set the data in the given item to the value given by the json character string
 * @param[in, out] x The item to set
 * @param[in] json The value to set x to
 * @return 0 on success, 1 on error
 */
int generic_set_json_f(generic_t x, const char* json);
/**
 * @brief Initialize Python if it is not initialized.
 * @returns int 0 if successful, other values indicate errors.
 */
int init_python_api_f();
/**
 * @brief Initialize a Python wrapper object.
 * @returns Initialized object.
 */
python_t init_python_f();
/**
 * @brief Destroy a structure containing a Python object.
 * @param[in] x Pointer to Python object structure that should be freed.
 */
void free_python_f(void* x);
/**
 * @brief Copy a Python object structure (NOTE: this dosn't copy the
 *   underlying Python object but does increment the reference count).
 * @param[in] x Structure containing Python object to copy.
 * @returns python_t Copy of x.
 */
python_t copy_python_f(python_t x);
/**
 * @brief Display a Python object structure.
 * @param[in] x Structure containing Python object to display.
 */
void display_python_f(python_t x);
/**
 * @brief Determine if a datatype is empty.
 * @param[in] dtype structure to test.
 * @returns int 1 if dtype is empty, 0 otherwise.
 */
int is_empty_dtype_f(const dtype_t dtype);
/**
 * @brief Determine if a datatype was created from a format.
 * @param[in] type_struct Datatype structure.
 * @returns 1 if the datatype was created from a format, 0 if it was not,
 *   -1 if there is an error.
 */
int is_dtype_format_array_f(const dtype_t type_struct);
/**
 * @brief Get the name of the type from the class.
 * @param[in] type_class Type structure..
 * @returns Type name.
 */
const char* dtype_name_f(const dtype_t type_class);
/**
 * @brief Get the subtype of the type.
 * @param[in] type_class Type structure..
 * @returns The subtype of the class, "" if there is an error.
 */
const char* dtype_subtype_f(const dtype_t type_class);
/**
 * @brief Compare two datatypes structures.
 * @param[in] a First datatype for comparison
 * @param[in] b Second datatype for comparison
 */
bool compare_dtype_f(const dtype_t a, const dtype_t b);
/**
 * @brief Get the precision of the type.
 * @param[in] type_class Type structure..
 * @returns The precision of the class, 0 if there is an error.
 */
size_t dtype_precision_f(const dtype_t type_class);
/**
 * @brief Set the type name in the datatype structure.
 * @param[in,out] dtype Datatype structure to update. It must have
 *   been initialized.
 * @param[in] name Type name to set in dtype.
 * @returns 0 on success, -1 if there is an error.
 */
int set_dtype_name_f(dtype_t dtype, const char* name);
/**
 * @brief Initialize a datatype structure including setting the type string.
 * @param[in] dtype Type structure.
 * @param[in] use_generic If true, serialized or deserialized
 *   objects will be expected to be generic_t instances.
 * @returns Initialized type structure.
 */
dtype_t complete_dtype_f(dtype_t dtype, const bool use_generic);
/**
 * @brief Construct a type object from a JSON schema.
 * @param[in] schema Serialized JSON schema.
 * @param[in] use_generic If true, serialized or deserialized objects will
 *   be expected to be generic_t instances.
 * @returns Type structure.
 */
dtype_t create_dtype_from_schema_f(const char* schema, const bool use_generic);
/**
 * @brief Construct and empty type object.
 * @param[in] use_generic If true, serialized or deserialized
 *   objects will be expected to be generic_t instances.
 * @returns Type structure.
 */
dtype_t create_dtype_empty_f(const bool use_generic);
/**
 * @brief Create a datatype based on a Python dictionary.
 * @param[in] pyobj Python dictionary.
 * @param[in] use_generic If true, serialized or deserialized
 *   objects will be expected to be generic_t instances.
 * @returns Type structure.
 */
dtype_t create_dtype_python_f(void* pyobj, const bool use_generic);
/**
 * @brief Construct a Direct type object.
 * @param[in] use_generic If true, serialized or deserialized
 *   objects will be expected to be generic_t instances.
 * @returns Type structure.
 */
dtype_t create_dtype_direct_f(const bool use_generic);
/**
 * @brief Construct a type object for one of the default JSON types.
 * @param[in] type Name of the type.
 * @param[in] use_generic If true, serialized or deserialized
 *   objects will be expected to be generic_t instances.
 * @returns Type structure.
 */
dtype_t create_dtype_default_f(const char* type, const bool use_generic);
/**
 * @brief Construct a Scalar type object.
 * @param[in] subtype Name of the scalar subtype (e.g. int, uint, float,
 *   bytes).
 * @param[in] precision Precision of the scalar in bits.
 * @param[in] units Units for scalar. (e.g. "cm", "g", "" for unitless)
 * @param[in] use_generic If true, serialized or deserialized
 *   objects will be expected to be generic_t instances.
 * @returns Type structure.
 */
dtype_t create_dtype_scalar_f(const char* subtype, const size_t precision, const char* units, const bool use_generic);
/**
 * @brief Construct a 1D array type object.
 * @param[in] subtype Name of the array subtype (e.g. int, uint, float,
 *   bytes).
 * @param[in] precision Precision of the array in bits.
 * @param[in] length Number of elements in the array.
 * @param[in] units Units for array elements. (e.g. "cm", "g", "" for
 *   unitless)
 * @param[in] use_generic If true, serialized or deserialized
 *   objects will be expected to be generic_t instances.
 * @returns Type structure.
 */
dtype_t create_dtype_1darray_f(const char* subtype, const size_t precision, const size_t length, const char* units, const bool use_generic);
/**
 * @brief Construct a ND array type object.
 * @param[in] subtype Name of the array subtype (e.g. int, uint, float,
 *   bytes).
 * @param[in] precision Precision of the array in bits.
 * @param[in] ndim Number of dimensions in the array (and therefore also
 *   the number of elements in shape).
 * @param[in] shape Pointer to array where each element is the
 *   size of the array in that dimension.
 * @param[in] units Units for array elements. (e.g. "cm", "g", "" for
 *   unitless)
 * @param[in] use_generic If true, serialized or deserialized
 *   objects will be expected to be generic_t instances.
 * @returns Type structure.
 */
dtype_t create_dtype_ndarray_f(const char* subtype, const size_t precision, const size_t ndim, const void* shape, const char* units, const bool use_generic);
/**
 * @brief Construct a JSON array type object.
 * @param[in] nitems Number of types in items.
 * @param[in] items Pointer to array of types describing the array
 *   elements.
 * @param[in] use_generic If true, serialized or deserialized
 *   objects will be expected to be generic_t instances.
 * @returns Type structure.
 */
dtype_t create_dtype_json_array_f(const size_t nitems, void* items, const bool use_generic);
/**
 * @brief Construct a JSON object type object.
 * @param[in] nitems Number of items in keys and values.
 * @param[in] keys Pointer to array of keys for each type.
 * @param[in] values Pointer to array of types describing the values
 *   for each key.
 * @param[in] use_generic If true, serialized or deserialized
 *   objects will be expected to be generic_t instances.
 * @returns Type structure.
 */
dtype_t create_dtype_json_object_f(const size_t nitems, const void* keys, void* values, const bool use_generic);
/**
 * @brief Construct a Ply type object.
 * @param[in] use_generic If true, serialized or deserialized
 *   objects will be expected to be generic_t instances.
 * @returns Type structure.
 */
dtype_t create_dtype_ply_f(const bool use_generic);
/**
 * @brief Construct a Obj type object.
 * @param[in] use_generic If true, serialized or deserialized
 *   objects will be expected to be generic_t instances.
 * @returns Type structure.
 */
dtype_t create_dtype_obj_f(const bool use_generic);
/**
 * @brief Construct an AsciiTable type object.
 * @param[in] format_str C-style format string that will be used to
 *   determine the type of elements in arrays that will be
 *   serialized or deserialized using the resulting type.
 * @param[in] as_array If true, the types will be arrays. Otherwise they
 *   will be scalars.
 * @param[in] use_generic If true, serialized or deserialized
 *   objects will be expected to be generic_t instances.
 * @returns Type structure.
 */
dtype_t create_dtype_ascii_table_f(const char* format_str, const bool as_array, const bool use_generic);
/**
 * @brief Construct a type object based on the provided format string.
 * @param[in] format_str C-style format string that will be used to
 *   determine the type of elements in arrays that will be
 *   serialized or deserialized using the resulting type.
 * @param[in] as_array If true, the types will be arrays. Otherwise they
 *   will be scalars.
 * @param[in] use_generic If true, serialized or deserialized
 *   objects will be expected to be generic_t instances.
 * @returns Type structure.
 */
dtype_t create_dtype_format_f(const char* format_str, const bool as_array, const bool use_generic);
/**
 * @brief Construct a type object for Python objects.
 * @param[in] type Type string.
 * @param[in] use_generic If true, serialized or deserialized
 *   objects will be expected to be generic_t instances.
 * @returns Type structure.
 */
dtype_t create_dtype_pyobj_f(const char* type, const bool use_generic);
/**
 * @brief Construct a type object for Python object instances.
 * @param[in] class_name Python class name that instance should be a
 *   subclass of. If NULL or an empty string, no class constraints will
 *   be placed on the instance.
 * @param[in] args_dtype Datatype describing the arguments creating the
 *   instance. The datatype will be consumed and does not need to be freed.
 * @param[in] kwargs_dtype Datatype describing the keyword arguments
 *   creating the instance. The datatype will be consumed and does not
 *   need to be freed.
 * @param[in] use_generic If true, serialized or deserialized
 *   objects will be expected to be generic_t instances.
 * @returns Type structure.
 */
dtype_t create_dtype_pyinst_f(const char* class_name, void* args_dtype, void* kwargs_dtype, const bool use_generic);
/**
 * @brief Construct a type object for a schema.
 * @param[in] use_generic If true, serialized or deserialized
 *   objects will be expected to be generic_t instances.
 * @returns Type structure.
 */
dtype_t create_dtype_schema_f(const bool use_generic);
/**
 * @brief Construct a type object for receiving any type.
 * @param[in] use_generic If true, serialized or deserialized
 *   objects will be expected to be generic_t instances.
 * @returns Type structure.
 */
dtype_t create_dtype_any_f(const bool use_generic);
/**
 * @brief Wrapper for freeing rapidjson::Document class wrapper struct.
 * @param[in] dtype Wrapper struct for C++ Metadata.
 * @returns int 0 if free was successfull, -1 if there was an error.
 */
int destroy_dtype_f(void* dtype);
/**
 * @brief Get a copy of a type structure.
 * @param[in] dtype Wrapper struct for C++ Metadata.
 * @returns: Type class.
 */
dtype_t copy_dtype_f(const dtype_t dtype);
/**
 * @brief Determine if a type structure indicates that generic objects
 *   should be used.
 * @param[in] dtype Wrapper struct for C++ Metadata.
 * @returns 1 if generic objects will be used, 0 if not, -1 for errors.
 */
int dtype_uses_generic_f(dtype_t dtype);
/**
 * @brief Initialize empty obj structure.
 * @returns obj_t Obj structure.
 */
obj_t init_obj_f();
/**
 * @brief Create a obj structure with generated data.
 * @returns obj_t Obj structure.
 */
obj_t generate_obj_f();
/**
 * @brief Free obj structure.
 * @param[in] p *obj_t Pointer to obj structure.
 */
void free_obj_f(void* p);
/**
 * @brief Set parameters from a rapidjson::ObjWavefront object.
 * @param[in,out] x Structure to modify.
 * @param[in] obj rapidjson::ObjWavefront object to copy.
 * @param[in] copy If 1, the provided object will be copied, otherwise
 *   the pointer will be added to the structured directly and it will
 *   be freed on destruction.
 */
void set_obj_f(void* x, void* obj, int copy);
/**
 * @brief Copy an obj structure.
 * @param[in] src obj_t Obj structure that should be copied.
 * @returns Copy of obj structure.
 */
obj_t copy_obj_f(obj_t src);
/**
 * @brief Display the information contained by an Obj struct.
 * @param[in] p obj_t Obj structure.
 * @param[in] indent const char* Indentation that should be added to
 *   each line.
 */
void display_obj_indent_f(obj_t p, const char* indent);
/**
 * @brief Display the information contained by an Obj struct.
 * @param[in] p obj_t Obj structure.
 */
void display_obj_f(obj_t p);
/**
 * @brief Get the number of elements of a certain type in the structure.
 * @param[in] p obj_t ObjWavefront structure.
 * @param[in] name Name of element type to count.
 * @returns Number of elements of the specified type.
 */
int nelements_obj_f(obj_t p, const char* name);
/**
 * @brief Compare two obj structures for equality.
 * @param[in] a First structure for comparison.
 * @param[in] b Second structure for comparison.
 * @returns true if a and b are equal, false otherwise.
 */
bool compare_obj_f(const obj_t a, const obj_t b);
/**
 * @brief Initialize empty ply structure.
 * @returns ply_t Ply structure.
 */
ply_t init_ply_f();
/**
 * @brief Create a ply structure with generated data.
 * @returns ply_t Ply structure.
 */
ply_t generate_ply_f();
/**
 * @brief Free ply structure.
 * @param[in] p *ply_t Pointer to ply structure.
 */
void free_ply_f(void* p);
/**
 * @brief Set parameters from a rapidjson::Ply object.
 * @param[in,out] x Structure to modify.
 * @param[in] ply rapidjson::Ply object to copy.
 * @param[in] copy If 1, the provided object will be copied, otherwise
 *   the pointer will be added to the structured directly and it will
 *   be freed on destruction.
 */
void set_ply_f(void* x, void* ply, int copy);
/**
 * @brief Copy an ply structure.
 * @param[in] src ply_t Ply structure that should be copied.
 * @returns Copy of ply structure.
 */
ply_t copy_ply_f(ply_t src);
/**
 * @brief Display the information contained by an Ply struct.
 * @param[in] p ply_t Ply structure.
 * @param[in] indent const char* Indentation that should be added to
 *   each line.
 */
void display_ply_indent_f(ply_t p, const char* indent);
/**
 * @brief Display the information contained by an Ply struct.
 * @param[in] p ply_t Ply structure.
 */
void display_ply_f(ply_t p);
/**
 * @brief Get the number of elements of a certain type in the structure.
 * @param[in] p ply_t Ply structure.
 * @param[in] name Name of element type to count.
 * @returns Number of elements of the specified type.
 */
int nelements_ply_f(ply_t p, const char* name);
/**
 * @brief Compare two ply structures for equality.
 * @param[in] a First structure for comparison.
 * @param[in] b Second structure for comparison.
 * @returns true if a and b are equal, false otherwise.
 */
bool compare_ply_f(const ply_t a, const ply_t b);
/**
 * @brief Get the number of elements in a array
 * @param[in] x generic_t Generic object that is presumed to contain a
 *   array
 * @returns size_t Number of elements in array
 */
size_t generic_array_get_size_f(generic_t x);
/**
 * @brief Get the number of elements in a object
 * @param[in] x generic_t Generic object that is presumed to contain a
 *   object
 * @returns size_t Number of elements in object
 */
size_t generic_object_get_size_f(generic_t x);
#define generic_map_get_size_f generic_object_get_size_f
/**
 * @brief Set a given generic item to a null
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_null_f(generic_t x, const void* value);
/**
 * @brief Set a given generic item to a boolean
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_bool_f(generic_t x, const bool value);
/**
 * @brief Set a given generic item to a integer
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_integer_f(generic_t x, const int value);
/**
 * @brief Set a given generic item to a number
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_number_f(generic_t x, const double value);
/**
 * @brief Set a given generic item to a string
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_string_f(generic_t x, const char* value);
/**
 * @brief Set a given generic item to a item
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_item_f(generic_t x, const char* type, void* value);
/**
 * @brief Set a given generic item to a array
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_array_f(generic_t x, const generic_t value);
/**
 * @brief Set a given generic item to a object
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_object_f(generic_t x, const generic_t value);
#define generic_set_map_f generic_set_object_f
/**
 * @brief Set a given generic item to a ply
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_ply_f(generic_t x, const ply_t value);
/**
 * @brief Set a given generic item to a obj
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_obj_f(generic_t x, const obj_t value);
/**
 * @brief Set a given generic item to a class
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_python_class_f(generic_t x, const python_t value);
/**
 * @brief Set a given generic item to a function
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_python_function_f(generic_t x, const python_t value);
/**
 * @brief Set a given generic item to a instance
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_python_instance_f(generic_t x, const python_t value);
/**
 * @brief Set a given generic item to a scalar
 * @param[in] x The generic item to set
 * @param[in] value Pointer to the memory containing the value to assign to x
 * @param[in] subtype Subtype of data contained in value
 * @param[in] precision The precision of the data in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_scalar_f(generic_t x, const void* value, const char* subtype, const size_t precision, const char* units);
/**
 * @brief Set a given generic item to a int scalar
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_int16_f(generic_t x, const int16_t value, const char* units);
/**
 * @brief Set a given generic item to a int scalar
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_int32_f(generic_t x, const int32_t value, const char* units);
/**
 * @brief Set a given generic item to a int scalar
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_int64_f(generic_t x, const int64_t value, const char* units);
/**
 * @brief Set a given generic item to a float scalar
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_float_f(generic_t x, const float value, const char* units);
/**
 * @brief Set a given generic item to a float scalar
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_double_f(generic_t x, const double value, const char* units);
/**
 * @brief Set a given generic item to a complex scalar
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_complex_float_f(generic_t x, const complex_float_t value, const char* units);
/**
 * @brief Set a given generic item to a complex scalar
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_complex_double_f(generic_t x, const complex_double_t value, const char* units);
/**
 * @brief Set a given generic item to a 1darray
 * @param[in] x The generic item to set
 * @param[in] value Pointer to the memory containing the array to assign
 *   to x
 * @param[in] subtype Subtype of data contained in value
 * @param[in] precision The precision of the elements in value
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_1darray_f(generic_t x, const void* value, const char* subtype, const size_t precision, const size_t length, const char* units);
/**
 * @brief Set a given generic item to a int 1darray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_1darray_int16_f(generic_t x, const void* value, const size_t length, const char* units);
/**
 * @brief Set a given generic item to a int 1darray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_1darray_int32_f(generic_t x, const void* value, const size_t length, const char* units);
/**
 * @brief Set a given generic item to a int 1darray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_1darray_int64_f(generic_t x, const void* value, const size_t length, const char* units);
/**
 * @brief Set a given generic item to a float 1darray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_1darray_float_f(generic_t x, const void* value, const size_t length, const char* units);
/**
 * @brief Set a given generic item to a float 1darray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_1darray_double_f(generic_t x, const void* value, const size_t length, const char* units);
/**
 * @brief Set a given generic item to a complex 1darray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_1darray_complex_float_f(generic_t x, const void* value, const size_t length, const char* units);
/**
 * @brief Set a given generic item to a complex 1darray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_1darray_complex_double_f(generic_t x, const void* value, const size_t length, const char* units);
/**
 * @brief Set a given generic item to a ndarray
 * @param[in] x The generic item to set
 * @param[in] value Pointer to the memory containing the array to assign
 *   to x
 * @param[in] subtype Subtype of data contained in value
 * @param[in] precision The precision of the elements in value
 * @param[in] ndim The number of dimensions in value
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_ndarray_f(generic_t x, const void* value, const char* subtype, const size_t precision, const size_t ndim, const void* shape, const char* units);
/**
 * @brief Set a given generic item to a int ndarray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_ndarray_int16_f(generic_t x, const void* value, const size_t ndim, const void* shape, const char* units);
/**
 * @brief Set a given generic item to a int ndarray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_ndarray_int32_f(generic_t x, const void* value, const size_t ndim, const void* shape, const char* units);
/**
 * @brief Set a given generic item to a int ndarray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_ndarray_int64_f(generic_t x, const void* value, const size_t ndim, const void* shape, const char* units);
/**
 * @brief Set a given generic item to a float ndarray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_ndarray_float_f(generic_t x, const void* value, const size_t ndim, const void* shape, const char* units);
/**
 * @brief Set a given generic item to a float ndarray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_ndarray_double_f(generic_t x, const void* value, const size_t ndim, const void* shape, const char* units);
/**
 * @brief Set a given generic item to a complex ndarray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_ndarray_complex_float_f(generic_t x, const void* value, const size_t ndim, const void* shape, const char* units);
/**
 * @brief Set a given generic item to a complex ndarray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_ndarray_complex_double_f(generic_t x, const void* value, const size_t ndim, const void* shape, const char* units);
/**
 * @brief Set a given generic item to a schema
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_schema_f(generic_t x, const generic_t value);
/**
 * @brief Set a given generic item to a any
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_any_f(generic_t x, const generic_t value);
/**
 * @brief Get a null from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
void* generic_get_null_f(generic_t x);
/**
 * @brief Get a boolean from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
bool generic_get_bool_f(generic_t x);
/**
 * @brief Get a integer from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
int generic_get_integer_f(generic_t x);
/**
 * @brief Get a number from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
double generic_get_number_f(generic_t x);
/**
 * @brief Get a string from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
const char* generic_get_string_f(generic_t x);
/**
 * @brief Get the raw item data
 * @param[in] x Generic item to retrieve data from
 * @param[in] type Type of item to retrieve
 * @returns Pointer to data containing raw item data, NULL on error
 */
void* generic_get_item_f(generic_t x, const char* type);
/**
 * @brief Get the size of the raw item data
 * @param[in] x Generic item to retrieve data size from
 * @param[in] type Type of item to retrieve
 * @returns Number of bytes in raw item data, 0 on error
 */
int generic_get_item_nbytes_f(generic_t x, const char* type);
/**
 * @brief Get a array from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
generic_t generic_get_array_f(generic_t x);
/**
 * @brief Get a object from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
generic_t generic_get_object_f(generic_t x);
#define generic_get_map_f generic_get_object_f
/**
 * @brief Get a ply from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
ply_t generic_get_ply_f(generic_t x);
/**
 * @brief Get a obj from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
obj_t generic_get_obj_f(generic_t x);
/**
 * @brief Get a class from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
python_t generic_get_python_class_f(generic_t x);
/**
 * @brief Get a function from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
python_t generic_get_python_function_f(generic_t x);
/**
 * @brief Get a instance from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
python_t generic_get_python_instance_f(generic_t x);
/**
 * @brief Get a scalar from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[in] subtype Subtype of data to return
 * @param[in] precision Precision of the data to return
 * @returns Pointer to value in x
 */
void* generic_get_scalar_f(generic_t x, const char* subtype, const size_t precision);
/**
 * @brief Get a int scalar from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
int16_t generic_get_int16_f(generic_t x);
/**
 * @brief Get a int scalar from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
int32_t generic_get_int32_f(generic_t x);
/**
 * @brief Get a int scalar from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
int64_t generic_get_int64_f(generic_t x);
/**
 * @brief Get a float scalar from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
float generic_get_float_f(generic_t x);
/**
 * @brief Get a float scalar from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
double generic_get_double_f(generic_t x);
/**
 * @brief Get a complex scalar from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
complex_float_t generic_get_complex_float_f(generic_t x);
/**
 * @brief Get a complex scalar from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
complex_double_t generic_get_complex_double_f(generic_t x);
/**
 * @brief Get a 1darray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[in] subtype Subtype of data to return
 * @param[in] precision Precision of the data to return
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_get_1darray_f(generic_t x, const char* subtype, const size_t precision, void* value);
/**
 * @brief Get a int 1darray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_get_1darray_int16_f(generic_t x, void* value);
/**
 * @brief Get a int 1darray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_get_1darray_int32_f(generic_t x, void* value);
/**
 * @brief Get a int 1darray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_get_1darray_int64_f(generic_t x, void* value);
/**
 * @brief Get a float 1darray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_get_1darray_float_f(generic_t x, void* value);
/**
 * @brief Get a float 1darray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_get_1darray_double_f(generic_t x, void* value);
/**
 * @brief Get a complex 1darray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_get_1darray_complex_float_f(generic_t x, void* value);
/**
 * @brief Get a complex 1darray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_get_1darray_complex_double_f(generic_t x, void* value);
/**
 * @brief Get a ndarray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[in] subtype Subtype of data to return
 * @param[in] precision Precision of the data to return
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_get_ndarray_f(generic_t x, const char* subtype, const size_t precision, void* value, void* shape);
/**
 * @brief Get a int ndarray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_get_ndarray_int16_f(generic_t x, void* value, void* shape);
/**
 * @brief Get a int ndarray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_get_ndarray_int32_f(generic_t x, void* value, void* shape);
/**
 * @brief Get a int ndarray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_get_ndarray_int64_f(generic_t x, void* value, void* shape);
/**
 * @brief Get a float ndarray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_get_ndarray_float_f(generic_t x, void* value, void* shape);
/**
 * @brief Get a float ndarray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_get_ndarray_double_f(generic_t x, void* value, void* shape);
/**
 * @brief Get a complex ndarray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_get_ndarray_complex_float_f(generic_t x, void* value, void* shape);
/**
 * @brief Get a complex ndarray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_get_ndarray_complex_double_f(generic_t x, void* value, void* shape);
/**
 * @brief Get a schema from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
generic_t generic_get_schema_f(generic_t x);
/**
 * @brief Get a any from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
generic_t generic_get_any_f(generic_t x);
/**
 * @brief Get a null from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
void* generic_ref_get_null_f(generic_ref_t x);
/**
 * @brief Get a boolean from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
bool generic_ref_get_bool_f(generic_ref_t x);
/**
 * @brief Get a integer from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
int generic_ref_get_integer_f(generic_ref_t x);
/**
 * @brief Get a number from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
double generic_ref_get_number_f(generic_ref_t x);
/**
 * @brief Get a string from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
const char* generic_ref_get_string_f(generic_ref_t x);
/**
 * @brief Get the raw item data
 * @param[in] x Generic item to retrieve data from
 * @param[in] type Type of item to retrieve
 * @returns Pointer to data containing raw item data, NULL on error
 */
void* generic_ref_get_item_f(generic_ref_t x, const char* type);
/**
 * @brief Get the size of the raw item data
 * @param[in] x Generic item to retrieve data size from
 * @param[in] type Type of item to retrieve
 * @returns Number of bytes in raw item data, 0 on error
 */
int generic_ref_get_item_nbytes_f(generic_ref_t x, const char* type);
/**
 * @brief Get a array from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
generic_t generic_ref_get_array_f(generic_ref_t x);
/**
 * @brief Get a object from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
generic_t generic_ref_get_object_f(generic_ref_t x);
#define generic_ref_get_map_f generic_ref_get_object_f
/**
 * @brief Get a ply from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
ply_t generic_ref_get_ply_f(generic_ref_t x);
/**
 * @brief Get a obj from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
obj_t generic_ref_get_obj_f(generic_ref_t x);
/**
 * @brief Get a class from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
python_t generic_ref_get_python_class_f(generic_ref_t x);
/**
 * @brief Get a function from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
python_t generic_ref_get_python_function_f(generic_ref_t x);
/**
 * @brief Get a instance from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
python_t generic_ref_get_python_instance_f(generic_ref_t x);
/**
 * @brief Get a scalar from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[in] subtype Subtype of data to return
 * @param[in] precision Precision of the data to return
 * @returns Pointer to value in x
 */
void* generic_ref_get_scalar_f(generic_ref_t x, const char* subtype, const size_t precision);
/**
 * @brief Get a int scalar from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
int16_t generic_ref_get_int16_f(generic_ref_t x);
/**
 * @brief Get a int scalar from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
int32_t generic_ref_get_int32_f(generic_ref_t x);
/**
 * @brief Get a int scalar from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
int64_t generic_ref_get_int64_f(generic_ref_t x);
/**
 * @brief Get a float scalar from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
float generic_ref_get_float_f(generic_ref_t x);
/**
 * @brief Get a float scalar from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
double generic_ref_get_double_f(generic_ref_t x);
/**
 * @brief Get a complex scalar from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
complex_float_t generic_ref_get_complex_float_f(generic_ref_t x);
/**
 * @brief Get a complex scalar from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
complex_double_t generic_ref_get_complex_double_f(generic_ref_t x);
/**
 * @brief Get a 1darray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[in] subtype Subtype of data to return
 * @param[in] precision Precision of the data to return
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_ref_get_1darray_f(generic_ref_t x, const char* subtype, const size_t precision, void* value);
/**
 * @brief Get a int 1darray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_ref_get_1darray_int16_f(generic_ref_t x, void* value);
/**
 * @brief Get a int 1darray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_ref_get_1darray_int32_f(generic_ref_t x, void* value);
/**
 * @brief Get a int 1darray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_ref_get_1darray_int64_f(generic_ref_t x, void* value);
/**
 * @brief Get a float 1darray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_ref_get_1darray_float_f(generic_ref_t x, void* value);
/**
 * @brief Get a float 1darray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_ref_get_1darray_double_f(generic_ref_t x, void* value);
/**
 * @brief Get a complex 1darray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_ref_get_1darray_complex_float_f(generic_ref_t x, void* value);
/**
 * @brief Get a complex 1darray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_ref_get_1darray_complex_double_f(generic_ref_t x, void* value);
/**
 * @brief Get a ndarray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[in] subtype Subtype of data to return
 * @param[in] precision Precision of the data to return
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_ref_get_ndarray_f(generic_ref_t x, const char* subtype, const size_t precision, void* value, void* shape);
/**
 * @brief Get a int ndarray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_ref_get_ndarray_int16_f(generic_ref_t x, void* value, void* shape);
/**
 * @brief Get a int ndarray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_ref_get_ndarray_int32_f(generic_ref_t x, void* value, void* shape);
/**
 * @brief Get a int ndarray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_ref_get_ndarray_int64_f(generic_ref_t x, void* value, void* shape);
/**
 * @brief Get a float ndarray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_ref_get_ndarray_float_f(generic_ref_t x, void* value, void* shape);
/**
 * @brief Get a float ndarray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_ref_get_ndarray_double_f(generic_ref_t x, void* value, void* shape);
/**
 * @brief Get a complex ndarray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_ref_get_ndarray_complex_float_f(generic_ref_t x, void* value, void* shape);
/**
 * @brief Get a complex ndarray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_ref_get_ndarray_complex_double_f(generic_ref_t x, void* value, void* shape);
/**
 * @brief Get a schema from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
generic_t generic_ref_get_schema_f(generic_ref_t x);
/**
 * @brief Get a any from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
generic_t generic_ref_get_any_f(generic_ref_t x);
/**
 * @brief Set an element in a array to a null
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_null_f(generic_t x, const size_t index, const void* value);
/**
 * @brief Set an element in a array to a boolean
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_bool_f(generic_t x, const size_t index, const bool value);
/**
 * @brief Set an element in a array to a integer
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_integer_f(generic_t x, const size_t index, const int value);
/**
 * @brief Set an element in a array to a number
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_number_f(generic_t x, const size_t index, const double value);
/**
 * @brief Set an element in a array to a string
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_string_f(generic_t x, const size_t index, const char* value);
/**
 * @brief Set an element in a array to a item
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_item_f(generic_t x, const size_t index, const char* type, void* value);
/**
 * @brief Set an element in a array to a array
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_array_f(generic_t x, const size_t index, const generic_t value);
/**
 * @brief Set an element in a array to a object
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_object_f(generic_t x, const size_t index, const generic_t value);
#define generic_array_set_map_f generic_array_set_object_f
/**
 * @brief Set an element in a array to a ply
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_ply_f(generic_t x, const size_t index, const ply_t value);
/**
 * @brief Set an element in a array to a obj
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_obj_f(generic_t x, const size_t index, const obj_t value);
/**
 * @brief Set an element in a array to a class
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_python_class_f(generic_t x, const size_t index, const python_t value);
/**
 * @brief Set an element in a array to a function
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_python_function_f(generic_t x, const size_t index, const python_t value);
/**
 * @brief Set an element in a array to a instance
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_python_instance_f(generic_t x, const size_t index, const python_t value);
/**
 * @brief Set an element in a array to a scalar
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value Pointer to the memory containing the value to assign to x
 * @param[in] subtype Subtype of data contained in value
 * @param[in] precision The precision of the data in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_scalar_f(generic_t x, const size_t index, const void* value, const char* subtype, const size_t precision, const char* units);
/**
 * @brief Set an element in a array to a int scalar
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_int16_f(generic_t x, const size_t index, const int16_t value, const char* units);
/**
 * @brief Set an element in a array to a int scalar
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_int32_f(generic_t x, const size_t index, const int32_t value, const char* units);
/**
 * @brief Set an element in a array to a int scalar
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_int64_f(generic_t x, const size_t index, const int64_t value, const char* units);
/**
 * @brief Set an element in a array to a float scalar
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_float_f(generic_t x, const size_t index, const float value, const char* units);
/**
 * @brief Set an element in a array to a float scalar
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_double_f(generic_t x, const size_t index, const double value, const char* units);
/**
 * @brief Set an element in a array to a complex scalar
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_complex_float_f(generic_t x, const size_t index, const complex_float_t value, const char* units);
/**
 * @brief Set an element in a array to a complex scalar
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_complex_double_f(generic_t x, const size_t index, const complex_double_t value, const char* units);
/**
 * @brief Set an element in a array to a 1darray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value Pointer to the memory containing the array to assign
 *   to x
 * @param[in] subtype Subtype of data contained in value
 * @param[in] precision The precision of the elements in value
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_1darray_f(generic_t x, const size_t index, const void* value, const char* subtype, const size_t precision, const size_t length, const char* units);
/**
 * @brief Set an element in a array to a int 1darray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_1darray_int16_f(generic_t x, const size_t index, const void* value, const size_t length, const char* units);
/**
 * @brief Set an element in a array to a int 1darray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_1darray_int32_f(generic_t x, const size_t index, const void* value, const size_t length, const char* units);
/**
 * @brief Set an element in a array to a int 1darray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_1darray_int64_f(generic_t x, const size_t index, const void* value, const size_t length, const char* units);
/**
 * @brief Set an element in a array to a float 1darray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_1darray_float_f(generic_t x, const size_t index, const void* value, const size_t length, const char* units);
/**
 * @brief Set an element in a array to a float 1darray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_1darray_double_f(generic_t x, const size_t index, const void* value, const size_t length, const char* units);
/**
 * @brief Set an element in a array to a complex 1darray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_1darray_complex_float_f(generic_t x, const size_t index, const void* value, const size_t length, const char* units);
/**
 * @brief Set an element in a array to a complex 1darray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_1darray_complex_double_f(generic_t x, const size_t index, const void* value, const size_t length, const char* units);
/**
 * @brief Set an element in a array to a ndarray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value Pointer to the memory containing the array to assign
 *   to x
 * @param[in] subtype Subtype of data contained in value
 * @param[in] precision The precision of the elements in value
 * @param[in] ndim The number of dimensions in value
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_ndarray_f(generic_t x, const size_t index, const void* value, const char* subtype, const size_t precision, const size_t ndim, const void* shape, const char* units);
/**
 * @brief Set an element in a array to a int ndarray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_ndarray_int16_f(generic_t x, const size_t index, const void* value, const size_t ndim, const void* shape, const char* units);
/**
 * @brief Set an element in a array to a int ndarray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_ndarray_int32_f(generic_t x, const size_t index, const void* value, const size_t ndim, const void* shape, const char* units);
/**
 * @brief Set an element in a array to a int ndarray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_ndarray_int64_f(generic_t x, const size_t index, const void* value, const size_t ndim, const void* shape, const char* units);
/**
 * @brief Set an element in a array to a float ndarray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_ndarray_float_f(generic_t x, const size_t index, const void* value, const size_t ndim, const void* shape, const char* units);
/**
 * @brief Set an element in a array to a float ndarray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_ndarray_double_f(generic_t x, const size_t index, const void* value, const size_t ndim, const void* shape, const char* units);
/**
 * @brief Set an element in a array to a complex ndarray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_ndarray_complex_float_f(generic_t x, const size_t index, const void* value, const size_t ndim, const void* shape, const char* units);
/**
 * @brief Set an element in a array to a complex ndarray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_ndarray_complex_double_f(generic_t x, const size_t index, const void* value, const size_t ndim, const void* shape, const char* units);
/**
 * @brief Set an element in a array to a schema
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_schema_f(generic_t x, const size_t index, const generic_t value);
/**
 * @brief Set an element in a array to a any
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_any_f(generic_t x, const size_t index, const generic_t value);
/**
 * @brief Get a null from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
void* generic_array_get_null_f(generic_t x, const size_t index);
/**
 * @brief Get a boolean from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
bool generic_array_get_bool_f(generic_t x, const size_t index);
/**
 * @brief Get a integer from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
int generic_array_get_integer_f(generic_t x, const size_t index);
/**
 * @brief Get a number from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
double generic_array_get_number_f(generic_t x, const size_t index);
/**
 * @brief Get a string from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
const char* generic_array_get_string_f(generic_t x, const size_t index);
/**
 * @brief Get a item from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[in] type Type of item to retrieve
 * @returns Pointer to data containing raw item data, NULL on error
 */
void* generic_array_get_item_f(generic_t x, const size_t index, const char* type);
/**
 * @brief Get a item_nbytes from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[in] type Type of item to retrieve
 * @returns Number of bytes in raw item data, 0 on error
 */
int generic_array_get_item_nbytes_f(generic_t x, const size_t index, const char* type);
/**
 * @brief Get a array from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
generic_t generic_array_get_array_f(generic_t x, const size_t index);
/**
 * @brief Get a object from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
generic_t generic_array_get_object_f(generic_t x, const size_t index);
#define generic_array_get_map_f generic_array_get_object_f
/**
 * @brief Get a ply from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
ply_t generic_array_get_ply_f(generic_t x, const size_t index);
/**
 * @brief Get a obj from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
obj_t generic_array_get_obj_f(generic_t x, const size_t index);
/**
 * @brief Get a class from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
python_t generic_array_get_python_class_f(generic_t x, const size_t index);
/**
 * @brief Get a function from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
python_t generic_array_get_python_function_f(generic_t x, const size_t index);
/**
 * @brief Get a instance from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
python_t generic_array_get_python_instance_f(generic_t x, const size_t index);
/**
 * @brief Get a scalar from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[in] subtype Subtype of data to return
 * @param[in] precision Precision of the data to return
 * @returns Pointer to value in x
 */
void* generic_array_get_scalar_f(generic_t x, const size_t index, const char* subtype, const size_t precision);
/**
 * @brief Get a int scalar from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
int16_t generic_array_get_int16_f(generic_t x, const size_t index);
/**
 * @brief Get a int scalar from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
int32_t generic_array_get_int32_f(generic_t x, const size_t index);
/**
 * @brief Get a int scalar from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
int64_t generic_array_get_int64_f(generic_t x, const size_t index);
/**
 * @brief Get a float scalar from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
float generic_array_get_float_f(generic_t x, const size_t index);
/**
 * @brief Get a float scalar from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
double generic_array_get_double_f(generic_t x, const size_t index);
/**
 * @brief Get a complex scalar from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
complex_float_t generic_array_get_complex_float_f(generic_t x, const size_t index);
/**
 * @brief Get a complex scalar from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
complex_double_t generic_array_get_complex_double_f(generic_t x, const size_t index);
/**
 * @brief Get a 1darray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[in] subtype Subtype of data to return
 * @param[in] precision Precision of the data to return
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_array_get_1darray_f(generic_t x, const size_t index, const char* subtype, const size_t precision, void* value);
/**
 * @brief Get a int 1darray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_array_get_1darray_int16_f(generic_t x, const size_t index, void* value);
/**
 * @brief Get a int 1darray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_array_get_1darray_int32_f(generic_t x, const size_t index, void* value);
/**
 * @brief Get a int 1darray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_array_get_1darray_int64_f(generic_t x, const size_t index, void* value);
/**
 * @brief Get a float 1darray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_array_get_1darray_float_f(generic_t x, const size_t index, void* value);
/**
 * @brief Get a float 1darray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_array_get_1darray_double_f(generic_t x, const size_t index, void* value);
/**
 * @brief Get a complex 1darray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_array_get_1darray_complex_float_f(generic_t x, const size_t index, void* value);
/**
 * @brief Get a complex 1darray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_array_get_1darray_complex_double_f(generic_t x, const size_t index, void* value);
/**
 * @brief Get a ndarray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[in] subtype Subtype of data to return
 * @param[in] precision Precision of the data to return
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_array_get_ndarray_f(generic_t x, const size_t index, const char* subtype, const size_t precision, void* value, void* shape);
/**
 * @brief Get a int ndarray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_array_get_ndarray_int16_f(generic_t x, const size_t index, void* value, void* shape);
/**
 * @brief Get a int ndarray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_array_get_ndarray_int32_f(generic_t x, const size_t index, void* value, void* shape);
/**
 * @brief Get a int ndarray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_array_get_ndarray_int64_f(generic_t x, const size_t index, void* value, void* shape);
/**
 * @brief Get a float ndarray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_array_get_ndarray_float_f(generic_t x, const size_t index, void* value, void* shape);
/**
 * @brief Get a float ndarray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_array_get_ndarray_double_f(generic_t x, const size_t index, void* value, void* shape);
/**
 * @brief Get a complex ndarray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_array_get_ndarray_complex_float_f(generic_t x, const size_t index, void* value, void* shape);
/**
 * @brief Get a complex ndarray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_array_get_ndarray_complex_double_f(generic_t x, const size_t index, void* value, void* shape);
/**
 * @brief Get a schema from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
generic_t generic_array_get_schema_f(generic_t x, const size_t index);
/**
 * @brief Get a any from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
generic_t generic_array_get_any_f(generic_t x, const size_t index);
/**
 * @brief Set an element in a object to a null
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_null_f(generic_t x, const char* key, const void* value);
#define generic_map_set_null_f generic_object_set_null_f
/**
 * @brief Set an element in a object to a boolean
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_bool_f(generic_t x, const char* key, const bool value);
#define generic_map_set_bool_f generic_object_set_bool_f
/**
 * @brief Set an element in a object to a integer
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_integer_f(generic_t x, const char* key, const int value);
#define generic_map_set_integer_f generic_object_set_integer_f
/**
 * @brief Set an element in a object to a number
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_number_f(generic_t x, const char* key, const double value);
#define generic_map_set_number_f generic_object_set_number_f
/**
 * @brief Set an element in a object to a string
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_string_f(generic_t x, const char* key, const char* value);
#define generic_map_set_string_f generic_object_set_string_f
/**
 * @brief Set an element in a object to a item
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_item_f(generic_t x, const char* key, const char* type, void* value);
#define generic_map_set_item_f generic_object_set_item_f
/**
 * @brief Set an element in a object to a array
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_array_f(generic_t x, const char* key, const generic_t value);
#define generic_map_set_array_f generic_object_set_array_f
/**
 * @brief Set an element in a object to a object
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_object_f(generic_t x, const char* key, const generic_t value);
#define generic_object_set_map_f generic_object_set_object_f
#define generic_map_set_object_f generic_object_set_object_f
/**
 * @brief Set an element in a object to a ply
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_ply_f(generic_t x, const char* key, const ply_t value);
#define generic_map_set_ply_f generic_object_set_ply_f
/**
 * @brief Set an element in a object to a obj
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_obj_f(generic_t x, const char* key, const obj_t value);
#define generic_map_set_obj_f generic_object_set_obj_f
/**
 * @brief Set an element in a object to a class
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_python_class_f(generic_t x, const char* key, const python_t value);
#define generic_map_set_python_class_f generic_object_set_python_class_f
/**
 * @brief Set an element in a object to a function
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_python_function_f(generic_t x, const char* key, const python_t value);
#define generic_map_set_python_function_f generic_object_set_python_function_f
/**
 * @brief Set an element in a object to a instance
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_python_instance_f(generic_t x, const char* key, const python_t value);
#define generic_map_set_python_instance_f generic_object_set_python_instance_f
/**
 * @brief Set an element in a object to a scalar
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value Pointer to the memory containing the value to assign to x
 * @param[in] subtype Subtype of data contained in value
 * @param[in] precision The precision of the data in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_scalar_f(generic_t x, const char* key, const void* value, const char* subtype, const size_t precision, const char* units);
#define generic_map_set_scalar_f generic_object_set_scalar_f
/**
 * @brief Set an element in a object to a int scalar
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_int16_f(generic_t x, const char* key, const int16_t value, const char* units);
#define generic_map_set_int16_f generic_object_set_int16_f
/**
 * @brief Set an element in a object to a int scalar
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_int32_f(generic_t x, const char* key, const int32_t value, const char* units);
#define generic_map_set_int32_f generic_object_set_int32_f
/**
 * @brief Set an element in a object to a int scalar
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_int64_f(generic_t x, const char* key, const int64_t value, const char* units);
#define generic_map_set_int64_f generic_object_set_int64_f
/**
 * @brief Set an element in a object to a float scalar
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_float_f(generic_t x, const char* key, const float value, const char* units);
#define generic_map_set_float_f generic_object_set_float_f
/**
 * @brief Set an element in a object to a float scalar
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_double_f(generic_t x, const char* key, const double value, const char* units);
#define generic_map_set_double_f generic_object_set_double_f
/**
 * @brief Set an element in a object to a complex scalar
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_complex_float_f(generic_t x, const char* key, const complex_float_t value, const char* units);
#define generic_map_set_complex_float_f generic_object_set_complex_float_f
/**
 * @brief Set an element in a object to a complex scalar
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_complex_double_f(generic_t x, const char* key, const complex_double_t value, const char* units);
#define generic_map_set_complex_double_f generic_object_set_complex_double_f
/**
 * @brief Set an element in a object to a 1darray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value Pointer to the memory containing the array to assign
 *   to x
 * @param[in] subtype Subtype of data contained in value
 * @param[in] precision The precision of the elements in value
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_1darray_f(generic_t x, const char* key, const void* value, const char* subtype, const size_t precision, const size_t length, const char* units);
#define generic_map_set_1darray_f generic_object_set_1darray_f
/**
 * @brief Set an element in a object to a int 1darray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_1darray_int16_f(generic_t x, const char* key, const void* value, const size_t length, const char* units);
#define generic_map_set_1darray_int16_f generic_object_set_1darray_int16_f
/**
 * @brief Set an element in a object to a int 1darray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_1darray_int32_f(generic_t x, const char* key, const void* value, const size_t length, const char* units);
#define generic_map_set_1darray_int32_f generic_object_set_1darray_int32_f
/**
 * @brief Set an element in a object to a int 1darray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_1darray_int64_f(generic_t x, const char* key, const void* value, const size_t length, const char* units);
#define generic_map_set_1darray_int64_f generic_object_set_1darray_int64_f
/**
 * @brief Set an element in a object to a float 1darray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_1darray_float_f(generic_t x, const char* key, const void* value, const size_t length, const char* units);
#define generic_map_set_1darray_float_f generic_object_set_1darray_float_f
/**
 * @brief Set an element in a object to a float 1darray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_1darray_double_f(generic_t x, const char* key, const void* value, const size_t length, const char* units);
#define generic_map_set_1darray_double_f generic_object_set_1darray_double_f
/**
 * @brief Set an element in a object to a complex 1darray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_1darray_complex_float_f(generic_t x, const char* key, const void* value, const size_t length, const char* units);
#define generic_map_set_1darray_complex_float_f generic_object_set_1darray_complex_float_f
/**
 * @brief Set an element in a object to a complex 1darray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_1darray_complex_double_f(generic_t x, const char* key, const void* value, const size_t length, const char* units);
#define generic_map_set_1darray_complex_double_f generic_object_set_1darray_complex_double_f
/**
 * @brief Set an element in a object to a ndarray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value Pointer to the memory containing the array to assign
 *   to x
 * @param[in] subtype Subtype of data contained in value
 * @param[in] precision The precision of the elements in value
 * @param[in] ndim The number of dimensions in value
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_ndarray_f(generic_t x, const char* key, const void* value, const char* subtype, const size_t precision, const size_t ndim, const void* shape, const char* units);
#define generic_map_set_ndarray_f generic_object_set_ndarray_f
/**
 * @brief Set an element in a object to a int ndarray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_ndarray_int16_f(generic_t x, const char* key, const void* value, const size_t ndim, const void* shape, const char* units);
#define generic_map_set_ndarray_int16_f generic_object_set_ndarray_int16_f
/**
 * @brief Set an element in a object to a int ndarray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_ndarray_int32_f(generic_t x, const char* key, const void* value, const size_t ndim, const void* shape, const char* units);
#define generic_map_set_ndarray_int32_f generic_object_set_ndarray_int32_f
/**
 * @brief Set an element in a object to a int ndarray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_ndarray_int64_f(generic_t x, const char* key, const void* value, const size_t ndim, const void* shape, const char* units);
#define generic_map_set_ndarray_int64_f generic_object_set_ndarray_int64_f
/**
 * @brief Set an element in a object to a float ndarray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_ndarray_float_f(generic_t x, const char* key, const void* value, const size_t ndim, const void* shape, const char* units);
#define generic_map_set_ndarray_float_f generic_object_set_ndarray_float_f
/**
 * @brief Set an element in a object to a float ndarray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_ndarray_double_f(generic_t x, const char* key, const void* value, const size_t ndim, const void* shape, const char* units);
#define generic_map_set_ndarray_double_f generic_object_set_ndarray_double_f
/**
 * @brief Set an element in a object to a complex ndarray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_ndarray_complex_float_f(generic_t x, const char* key, const void* value, const size_t ndim, const void* shape, const char* units);
#define generic_map_set_ndarray_complex_float_f generic_object_set_ndarray_complex_float_f
/**
 * @brief Set an element in a object to a complex ndarray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_ndarray_complex_double_f(generic_t x, const char* key, const void* value, const size_t ndim, const void* shape, const char* units);
#define generic_map_set_ndarray_complex_double_f generic_object_set_ndarray_complex_double_f
/**
 * @brief Set an element in a object to a schema
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_schema_f(generic_t x, const char* key, const generic_t value);
#define generic_map_set_schema_f generic_object_set_schema_f
/**
 * @brief Set an element in a object to a any
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_any_f(generic_t x, const char* key, const generic_t value);
#define generic_map_set_any_f generic_object_set_any_f
/**
 * @brief Get a null from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
void* generic_object_get_null_f(generic_t x, const char* key);
#define generic_map_get_null_f generic_object_get_null_f
/**
 * @brief Get a boolean from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
bool generic_object_get_bool_f(generic_t x, const char* key);
#define generic_map_get_bool_f generic_object_get_bool_f
/**
 * @brief Get a integer from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
int generic_object_get_integer_f(generic_t x, const char* key);
#define generic_map_get_integer_f generic_object_get_integer_f
/**
 * @brief Get a number from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
double generic_object_get_number_f(generic_t x, const char* key);
#define generic_map_get_number_f generic_object_get_number_f
/**
 * @brief Get a string from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
const char* generic_object_get_string_f(generic_t x, const char* key);
#define generic_map_get_string_f generic_object_get_string_f
/**
 * @brief Get a item from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[in] type Type of item to retrieve
 * @returns Pointer to data containing raw item data, NULL on error
 */
void* generic_object_get_item_f(generic_t x, const char* key, const char* type);
#define generic_map_get_item_f generic_object_get_item_f
/**
 * @brief Get a item_nbytes from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[in] type Type of item to retrieve
 * @returns Number of bytes in raw item data, 0 on error
 */
int generic_object_get_item_nbytes_f(generic_t x, const char* key, const char* type);
#define generic_map_get_item_nbytes_f generic_object_get_item_nbytes_f
/**
 * @brief Get a array from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
generic_t generic_object_get_array_f(generic_t x, const char* key);
#define generic_map_get_array_f generic_object_get_array_f
/**
 * @brief Get a object from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
generic_t generic_object_get_object_f(generic_t x, const char* key);
#define generic_object_get_map_f generic_object_get_object_f
#define generic_map_get_object_f generic_object_get_object_f
/**
 * @brief Get a ply from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
ply_t generic_object_get_ply_f(generic_t x, const char* key);
#define generic_map_get_ply_f generic_object_get_ply_f
/**
 * @brief Get a obj from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
obj_t generic_object_get_obj_f(generic_t x, const char* key);
#define generic_map_get_obj_f generic_object_get_obj_f
/**
 * @brief Get a class from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
python_t generic_object_get_python_class_f(generic_t x, const char* key);
#define generic_map_get_python_class_f generic_object_get_python_class_f
/**
 * @brief Get a function from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
python_t generic_object_get_python_function_f(generic_t x, const char* key);
#define generic_map_get_python_function_f generic_object_get_python_function_f
/**
 * @brief Get a instance from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
python_t generic_object_get_python_instance_f(generic_t x, const char* key);
#define generic_map_get_python_instance_f generic_object_get_python_instance_f
/**
 * @brief Get a scalar from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[in] subtype Subtype of data to return
 * @param[in] precision Precision of the data to return
 * @returns Pointer to value in x
 */
void* generic_object_get_scalar_f(generic_t x, const char* key, const char* subtype, const size_t precision);
#define generic_map_get_scalar_f generic_object_get_scalar_f
/**
 * @brief Get a int scalar from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
int16_t generic_object_get_int16_f(generic_t x, const char* key);
#define generic_map_get_int16_f generic_object_get_int16_f
/**
 * @brief Get a int scalar from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
int32_t generic_object_get_int32_f(generic_t x, const char* key);
#define generic_map_get_int32_f generic_object_get_int32_f
/**
 * @brief Get a int scalar from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
int64_t generic_object_get_int64_f(generic_t x, const char* key);
#define generic_map_get_int64_f generic_object_get_int64_f
/**
 * @brief Get a float scalar from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
float generic_object_get_float_f(generic_t x, const char* key);
#define generic_map_get_float_f generic_object_get_float_f
/**
 * @brief Get a float scalar from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
double generic_object_get_double_f(generic_t x, const char* key);
#define generic_map_get_double_f generic_object_get_double_f
/**
 * @brief Get a complex scalar from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
complex_float_t generic_object_get_complex_float_f(generic_t x, const char* key);
#define generic_map_get_complex_float_f generic_object_get_complex_float_f
/**
 * @brief Get a complex scalar from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
complex_double_t generic_object_get_complex_double_f(generic_t x, const char* key);
#define generic_map_get_complex_double_f generic_object_get_complex_double_f
/**
 * @brief Get a 1darray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[in] subtype Subtype of data to return
 * @param[in] precision Precision of the data to return
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_object_get_1darray_f(generic_t x, const char* key, const char* subtype, const size_t precision, void* value);
#define generic_map_get_1darray_f generic_object_get_1darray_f
/**
 * @brief Get a int 1darray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_object_get_1darray_int16_f(generic_t x, const char* key, void* value);
#define generic_map_get_1darray_int16_f generic_object_get_1darray_int16_f
/**
 * @brief Get a int 1darray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_object_get_1darray_int32_f(generic_t x, const char* key, void* value);
#define generic_map_get_1darray_int32_f generic_object_get_1darray_int32_f
/**
 * @brief Get a int 1darray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_object_get_1darray_int64_f(generic_t x, const char* key, void* value);
#define generic_map_get_1darray_int64_f generic_object_get_1darray_int64_f
/**
 * @brief Get a float 1darray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_object_get_1darray_float_f(generic_t x, const char* key, void* value);
#define generic_map_get_1darray_float_f generic_object_get_1darray_float_f
/**
 * @brief Get a float 1darray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_object_get_1darray_double_f(generic_t x, const char* key, void* value);
#define generic_map_get_1darray_double_f generic_object_get_1darray_double_f
/**
 * @brief Get a complex 1darray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_object_get_1darray_complex_float_f(generic_t x, const char* key, void* value);
#define generic_map_get_1darray_complex_float_f generic_object_get_1darray_complex_float_f
/**
 * @brief Get a complex 1darray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_object_get_1darray_complex_double_f(generic_t x, const char* key, void* value);
#define generic_map_get_1darray_complex_double_f generic_object_get_1darray_complex_double_f
/**
 * @brief Get a ndarray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[in] subtype Subtype of data to return
 * @param[in] precision Precision of the data to return
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_object_get_ndarray_f(generic_t x, const char* key, const char* subtype, const size_t precision, void* value, void* shape);
#define generic_map_get_ndarray_f generic_object_get_ndarray_f
/**
 * @brief Get a int ndarray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_object_get_ndarray_int16_f(generic_t x, const char* key, void* value, void* shape);
#define generic_map_get_ndarray_int16_f generic_object_get_ndarray_int16_f
/**
 * @brief Get a int ndarray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_object_get_ndarray_int32_f(generic_t x, const char* key, void* value, void* shape);
#define generic_map_get_ndarray_int32_f generic_object_get_ndarray_int32_f
/**
 * @brief Get a int ndarray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_object_get_ndarray_int64_f(generic_t x, const char* key, void* value, void* shape);
#define generic_map_get_ndarray_int64_f generic_object_get_ndarray_int64_f
/**
 * @brief Get a float ndarray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_object_get_ndarray_float_f(generic_t x, const char* key, void* value, void* shape);
#define generic_map_get_ndarray_float_f generic_object_get_ndarray_float_f
/**
 * @brief Get a float ndarray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_object_get_ndarray_double_f(generic_t x, const char* key, void* value, void* shape);
#define generic_map_get_ndarray_double_f generic_object_get_ndarray_double_f
/**
 * @brief Get a complex ndarray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_object_get_ndarray_complex_float_f(generic_t x, const char* key, void* value, void* shape);
#define generic_map_get_ndarray_complex_float_f generic_object_get_ndarray_complex_float_f
/**
 * @brief Get a complex ndarray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_object_get_ndarray_complex_double_f(generic_t x, const char* key, void* value, void* shape);
#define generic_map_get_ndarray_complex_double_f generic_object_get_ndarray_complex_double_f
/**
 * @brief Get a schema from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
generic_t generic_object_get_schema_f(generic_t x, const char* key);
#define generic_map_get_schema_f generic_object_get_schema_f
/**
 * @brief Get a any from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
generic_t generic_object_get_any_f(generic_t x, const char* key);
#define generic_map_get_any_f generic_object_get_any_f
#ifdef YGGDRASIL_LONG_DOUBLE_AVAILABLE
/**
 * @brief Set a given generic item to a float scalar
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_long_double_f(generic_t x, const long double value, const char* units);
/**
 * @brief Set a given generic item to a complex scalar
 * @param[in] x The generic item to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_complex_long_double_f(generic_t x, const complex_long_double_t value, const char* units);
/**
 * @brief Set a given generic item to a float 1darray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_1darray_long_double_f(generic_t x, const void* value, const size_t length, const char* units);
/**
 * @brief Set a given generic item to a complex 1darray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_1darray_complex_long_double_f(generic_t x, const void* value, const size_t length, const char* units);
/**
 * @brief Set a given generic item to a float ndarray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_ndarray_long_double_f(generic_t x, const void* value, const size_t ndim, const void* shape, const char* units);
/**
 * @brief Set a given generic item to a complex ndarray
 * @param[in] x The generic item to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_set_ndarray_complex_long_double_f(generic_t x, const void* value, const size_t ndim, const void* shape, const char* units);
/**
 * @brief Get a float scalar from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
long double generic_get_long_double_f(generic_t x);
/**
 * @brief Get a complex scalar from a generic item
 * @param[in] x Generic item to retrieve data from
 * @returns Value from x
 */
complex_long_double_t generic_get_complex_long_double_f(generic_t x);
/**
 * @brief Get a float 1darray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_get_1darray_long_double_f(generic_t x, void* value);
/**
 * @brief Get a complex 1darray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_get_1darray_complex_long_double_f(generic_t x, void* value);
/**
 * @brief Get a float ndarray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_get_ndarray_long_double_f(generic_t x, void* value, void* shape);
/**
 * @brief Get a complex ndarray from a generic item
 * @param[in] x Generic item to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_get_ndarray_complex_long_double_f(generic_t x, void* value, void* shape);
/**
 * @brief Get a float scalar from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
long double generic_ref_get_long_double_f(generic_ref_t x);
/**
 * @brief Get a complex scalar from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @returns Value from x
 */
complex_long_double_t generic_ref_get_complex_long_double_f(generic_ref_t x);
/**
 * @brief Get a float 1darray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_ref_get_1darray_long_double_f(generic_ref_t x, void* value);
/**
 * @brief Get a complex 1darray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_ref_get_1darray_complex_long_double_f(generic_ref_t x, void* value);
/**
 * @brief Get a float ndarray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_ref_get_ndarray_long_double_f(generic_ref_t x, void* value, void* shape);
/**
 * @brief Get a complex ndarray from a generic item reference
 * @param[in] x Generic item reference to retrieve data from
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_ref_get_ndarray_complex_long_double_f(generic_ref_t x, void* value, void* shape);
/**
 * @brief Set an element in a array to a float scalar
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_long_double_f(generic_t x, const size_t index, const long double value, const char* units);
/**
 * @brief Set an element in a array to a complex scalar
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_complex_long_double_f(generic_t x, const size_t index, const complex_long_double_t value, const char* units);
/**
 * @brief Set an element in a array to a float 1darray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_1darray_long_double_f(generic_t x, const size_t index, const void* value, const size_t length, const char* units);
/**
 * @brief Set an element in a array to a complex 1darray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_1darray_complex_long_double_f(generic_t x, const size_t index, const void* value, const size_t length, const char* units);
/**
 * @brief Set an element in a array to a float ndarray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_ndarray_long_double_f(generic_t x, const size_t index, const void* value, const size_t ndim, const void* shape, const char* units);
/**
 * @brief Set an element in a array to a complex ndarray
 * @param[in] x array to set element in
 * @param[in] index index of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_array_set_ndarray_complex_long_double_f(generic_t x, const size_t index, const void* value, const size_t ndim, const void* shape, const char* units);
/**
 * @brief Get a float scalar from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
long double generic_array_get_long_double_f(generic_t x, const size_t index);
/**
 * @brief Get a complex scalar from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @returns Value from x
 */
complex_long_double_t generic_array_get_complex_long_double_f(generic_t x, const size_t index);
/**
 * @brief Get a float 1darray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_array_get_1darray_long_double_f(generic_t x, const size_t index, void* value);
/**
 * @brief Get a complex 1darray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_array_get_1darray_complex_long_double_f(generic_t x, const size_t index, void* value);
/**
 * @brief Get a float ndarray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_array_get_ndarray_long_double_f(generic_t x, const size_t index, void* value, void* shape);
/**
 * @brief Get a complex ndarray from an element in a array
 * @param[in] x array to get element from
 * @param[in] index index of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_array_get_ndarray_complex_long_double_f(generic_t x, const size_t index, void* value, void* shape);
/**
 * @brief Set an element in a object to a float scalar
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_long_double_f(generic_t x, const char* key, const long double value, const char* units);
#define generic_map_set_long_double_f generic_object_set_long_double_f
/**
 * @brief Set an element in a object to a complex scalar
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The value to assign to x
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_complex_long_double_f(generic_t x, const char* key, const complex_long_double_t value, const char* units);
#define generic_map_set_complex_long_double_f generic_object_set_complex_long_double_f
/**
 * @brief Set an element in a object to a float 1darray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_1darray_long_double_f(generic_t x, const char* key, const void* value, const size_t length, const char* units);
#define generic_map_set_1darray_long_double_f generic_object_set_1darray_long_double_f
/**
 * @brief Set an element in a object to a complex 1darray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 * @param[in] length The number of elements in value
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_1darray_complex_long_double_f(generic_t x, const char* key, const void* value, const size_t length, const char* units);
#define generic_map_set_1darray_complex_long_double_f generic_object_set_1darray_complex_long_double_f
/**
 * @brief Set an element in a object to a float ndarray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_ndarray_long_double_f(generic_t x, const char* key, const void* value, const size_t ndim, const void* shape, const char* units);
#define generic_map_set_ndarray_long_double_f generic_object_set_ndarray_long_double_f
/**
 * @brief Set an element in a object to a complex ndarray
 * @param[in] x object to set element in
 * @param[in] key key of element to set
 * @param[in] value The array of values to assign to x
 *   in row-major order
 * @param[in] ndim The number of dimensions in value, or 0 on error
 * @param[in] shape The size of value in each dimension
 * @param[in] units Units of value
 * @returns GENERIC_ERROR_ on error, GENERIC_SUCCESS_ on success
 */
int generic_object_set_ndarray_complex_long_double_f(generic_t x, const char* key, const void* value, const size_t ndim, const void* shape, const char* units);
#define generic_map_set_ndarray_complex_long_double_f generic_object_set_ndarray_complex_long_double_f
/**
 * @brief Get a float scalar from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
long double generic_object_get_long_double_f(generic_t x, const char* key);
#define generic_map_get_long_double_f generic_object_get_long_double_f
/**
 * @brief Get a complex scalar from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @returns Value from x
 */
complex_long_double_t generic_object_get_complex_long_double_f(generic_t x, const char* key);
#define generic_map_get_complex_long_double_f generic_object_get_complex_long_double_f
/**
 * @brief Get a float 1darray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_object_get_1darray_long_double_f(generic_t x, const char* key, void* value);
#define generic_map_get_1darray_long_double_f generic_object_get_1darray_long_double_f
/**
 * @brief Get a complex 1darray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x
 * @returns Number of elements in the array, or 0 on error
 */
size_t generic_object_get_1darray_complex_long_double_f(generic_t x, const char* key, void* value);
#define generic_map_get_1darray_complex_long_double_f generic_object_get_1darray_complex_long_double_f
/**
 * @brief Get a float ndarray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_object_get_ndarray_long_double_f(generic_t x, const char* key, void* value, void* shape);
#define generic_map_get_ndarray_long_double_f generic_object_get_ndarray_long_double_f
/**
 * @brief Get a complex ndarray from an element in a object
 * @param[in] x object to get element from
 * @param[in] key key of element to get
 * @param[out] value Pointer to memory that should be reallocated and
 *   filled with the array contents of x in row-major order
 * @param[out] shape Pointer to memory that should be reallocated and
 *   filled with the size of the array in each dimension
 * @returns Number of dimensions in the array, or 0 on error
 */
size_t generic_object_get_ndarray_complex_long_double_f(generic_t x, const char* key, void* value, void* shape);
#define generic_map_get_ndarray_complex_long_double_f generic_object_get_ndarray_complex_long_double_f
#endif // YGGDRASIL_LONG_DOUBLE_AVAILABLE

#ifdef __cplusplus
}
#endif
