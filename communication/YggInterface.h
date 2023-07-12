// Flag for checking if YggInterface.h has already been included
#ifndef YGGINTERFACE_H_
#define YGGINTERFACE_H_

#include "utils/logging.hpp"
#include "communicators/comm_t.hpp"
#include "datatypes/dtype_t.h"

// TODO: Allow use of ygglog_error as function in C
using namespace communication::utils;

#ifdef __cplusplus /* If this is a C++ compiler, use C linkage */
extern "C" {
#endif

/*! @brief Pointer to an output comm. */
#define yggOutput_t comm_t
/*! @brief Pointer to an input comm. */
#define yggInput_t comm_t
/*! @brief Aliases to method for freeing comms. */
#define ygg_free free_comm
/*! @brief Initialize a comm object. */
#define yggComm init_comm

/*! @brief Memory to keep track of global scope comms. */
#define WITH_GLOBAL_SCOPE(COMM) global_scope_comm_on_c(); COMM; global_scope_comm_off_c()
  
// Forward declaration of server interface to allow replacemnt
static inline
comm_t yggRpcServerType_global(const char *name, dtype_t inType, dtype_t outType);

//==============================================================================
/*!
  Basic IO 

  Output Usage:
      1. One-time: Create output channel (store in named variables)
```
            yggOutput_t output_channel = yggOutput("out_name");
```
      2. Prepare: Format data to a character array buffer.
```
            char buffer[YGG_MSG_BUF]; 
	    snprintf(buffer, YGG_MSG_BUF, "a=%d, b=%d", 1, 2);
```
      3. Send:
```
	    ret = ygg_send(output_channel, buffer, strlen(buffer));
```

  Input Usage:
      1. One-time: Create output channel (store in named variables)
```
            yggInput_t input_channel = yggInput("in_name");
```
      2. Prepare: Allocate a character array buffer.
```
            char buffer[YGG_MSG_BUF];
```
      3. Receive:
```
            int ret = ygg_recv(input_channel, buffer, YGG_MSG_BUF);
```
*/
//==============================================================================

/*!
  @brief Constructor for yggOutput_t structure with explicit data type.
    Create a yggOutput_t structure based on a provided name that is used to
    locate a particular comm address stored in the environment variable name
    and a structure definining the datatype of outgoing messages for the
    queue.
  @param[in] name constant character pointer to name of queue.
  @param[in] datatype Data structure containing type informaiton.
  @returns yggOutput_t output queue structure.
 */
static inline
yggOutput_t yggOutputType(const char *name, dtype_t datatype) {
  return init_comm(name, SEND, DEFAULT_COMM, datatype);
};

/*!
  @brief Constructor for yggInput_t structure with explicit data type.
    Create a yggInput_t structure based on a provided name that is used to
    locate a particular comm address stored in the environment variable name
    and a structure defining the expected datatype of received messages.
  @param[in] name constant character pointer to name of queue.
  @param[in] datatype Data structure containing type informaiton.
  @returns yggInput_t input queue structure.
 */
static inline
yggInput_t yggInputType(const char *name, dtype_t datatype) {
  return init_comm(name, RECV, DEFAULT_COMM, datatype);
};
  
/*!
  @brief Constructor for yggOutput_t structure with format.
    Create a yggOutput_t structure based on a provided name that is used to
    locate a particular comm address stored in the environment variable name
    and a format string that can be used to format arguments into outgoing 
    messages for the queue.   
  @param[in] name constant character pointer to name of queue.
  @param[in] fmtString character pointer to format string.
  @returns yggOutput_t output queue structure.
 */
static inline
yggOutput_t yggOutputFmt(const char *name, const char *fmtString){
  dtype_t datatype = create_dtype_format(fmtString, 0, false);
  comm_t out = yggOutputType(name, datatype);
  if ((fmtString != NULL) && (datatype.metadata == NULL)) {
    ygglog_error_c("yggOutputFmt: Failed to create type from format_str.");
  }
  return out;
};

/*!
  @brief Constructor for yggInput_t structure with format.
    Create a yggInput_t structure based on a provided name that is used to
    locate a particular comm address stored in the environment variable name
    and a format stirng that can be used to extract arguments from received
    messages.
  @param[in] name constant character pointer to name of queue.
  @param[in] fmtString character pointer to format string.
  @returns yggInput_t input queue structure.
 */
static inline
yggInput_t yggInputFmt(const char *name, const char *fmtString){
  dtype_t datatype = create_dtype_format(fmtString, 0, false);
  comm_t out = yggInputType(name, datatype);
  if ((fmtString != NULL) && (datatype.metadata == NULL)) {
    ygglog_error_c("yggInputFmt: Failed to create type from format_str.");
  }
  return out;
};

/*!
  @brief Constructor for yggOutput_t output structure.
    Create a yggOutput_t structure based on a provided name that is used to
    locate a particular comm address stored in the environment variable name.
  @param[in] name constant character pointer to name of queue.
  @returns yggOutput_t output queue structure.
 */
static inline
yggOutput_t yggOutput(const char *name) {
  dtype_t datatype = create_dtype_empty(false);
  return yggOutputType(name, datatype);
};

/*!
  @brief Constructor for yggInput_t structure.
    Create a yggInput_t structure based on a provided name that is used to
    locate a particular comm address stored in the environment variable name.
  @param[in] name constant character pointer to name of queue.
  @returns yggInput_t input queue structure.
 */
static inline
yggInput_t yggInput(const char *name) {
  dtype_t datatype = create_dtype_empty(false);
  return yggInputType(name, datatype);
};

/*!
  @brief Send a message to an output queue.
    Send a message smaller than YGG_MSG_MAX bytes to an output queue. If the
    message is larger, it will not be sent.
  @param[in] yggQ yggOutput_t structure that queue should be sent to.
  @param[in] data character pointer to message that should be sent.
  @param[in] len size_t length of message to be sent.
  @returns int 0 if send succesfull, -1 if send unsuccessful.
 */
static inline
int ygg_send(const yggOutput_t yggQ, const char *data, const size_t len) {
  return comm_send(yggQ, data, len);
};

/*!
  @brief Send EOF message to the output queue.
  @param[in] yggQ yggOutput_t structure that message should be sent to.
  @returns int 0 if send successfull, -1 if unsuccessful.
*/
static inline
int ygg_send_eof(const yggOutput_t yggQ) {
  return comm_send_eof(yggQ);
};

/*!
  @brief Receive a message from an input queue.
    Receive a message smaller than YGG_MSG_MAX bytes from an input queue.
  @param[in] yggQ Communicator that message should be sent to.
  @param[out] data Pointer to allocated buffer where the message
    should be saved.
  @param[in] len Length of the allocated message buffer in bytes.
  @returns -1 if message could not be received. Length of the received
    message if message was received.
 */
static inline
long ygg_recv(yggInput_t yggQ, char *data, const size_t len) {
  return comm_recv(yggQ, data, len);
};

/*!
  @brief Send a large message to an output queue.
    Send a message larger than YGG_MSG_MAX bytes to an output queue by 
    breaking it up between several smaller messages and sending initial 
    message with the size of the message that should be expected. Must be 
    partnered with ygg_recv_nolimit for communication to make sense.
  @param[in] yggQ yggOutput_t structure that message should be sent to.
  @param[in] data character pointer to message that should be sent.
  @param[in] len size_t length of message to be sent.
  @returns int 0 if send succesfull, -1 if send unsuccessful.
 */
static inline
int ygg_send_nolimit(const yggOutput_t yggQ, const char *data, const size_t len) {
  return ygg_send(yggQ, data, len);
};

/*!
  @brief Send EOF message to the output queue.
  @param[in] yggQ yggOutput_t structure that message should be sent to.
  @returns int 0 if send successfull, -1 if unsuccessful.
*/
static inline
int ygg_send_nolimit_eof(const yggOutput_t yggQ) {
  return comm_send_eof(yggQ);
};

/*!
  @brief Receive a large message from an input queue.
    Receive a message larger than YGG_MSG_MAX bytes from an input queue by
    receiving it in parts. This expects the first message to be the size
    of the total message.
  @param[in] yggQ Communicator that message should be sent to.
  @param[out] data Pointer to pointer for allocated buffer where the
    message should be stored. A pointer to a pointer is used so that the 
    buffer may be reallocated as necessary for the incoming message.
  @param[in] len Length of the initial allocated message buffer in bytes.
  @returns -1 if message could not be received. Length of the received
    message if message was received.
 */
static inline
long ygg_recv_nolimit(yggInput_t yggQ, char **data, const size_t len) {
  return comm_recv_realloc(yggQ, data, len);
};


//==============================================================================
/*!
  Formatted IO 

  Output Usage:
      1. One-time: Create output channel with format specifier.
```
            yggOutput_t output_channel = yggOutputFmt("out_name", "a=%d, b=%d");
```
      2. Send:
```
	    ret = yggSend(output_channel, 1, 2);
```
      3. Free:
```
            ygg_free(&output_channel);
```

  Input Usage:
      1. One-time: Create output channel with format specifier.
```
            yggInput_t input_channel = yggInput("in_name", "a=%d, b=%d");
```
      2. Prepare: Allocate space for recovered variables.
```
            int a, b;
```
      3. Receive:
```
            int ret = yggRecv(input_channel, &a, &b);
```
*/
//==============================================================================

/*!
  @brief Send arguments as a small formatted message to an output queue.
    Use the associated data type to format the message from the input
    arguments.
  @param[in] yggQ Output or RPC comm structure that queue should be sent to.
  @param[in] ... Variables to be formatted into a message based on the data
    type associated with the comm.
  @returns int 0 if send succesfull, -1 if send unsuccessful.
 */
#define yggSend commSend

/*!
  @brief Receive a message from a comm into variables passed as arguments.
    If received message data will exceed the bounds of provided variables,
    an error will be returned.
  @param[in] yggQ yggOutput_t structure that message should be sent to.
  @param[out] ... Pointers to variables that should be assigned by 
    parsing the received message using the associated data type. If no data
    type has been added to the comm, one will be created from the data types
    of the first received message. As these are being assigned, they should
    be pointers to memory that has already been allocated (heap or stack).
  @returns int -1 if message could not be received or could not be parsed,
    length of the received message if message was received and parsed.
 */
#define yggRecv commRecv
  
/*!
  @brief Receive a message from a comm into variables passed as arguments.
    If received message data will exceed the bounds of provided variables,
    the variables will be reallocated.
  @param[in] yggQ yggOutput_t structure that message should be sent to.
  @param[out] ... Pointers to variables that should be assigned to by 
    parsing the received message using the associated data type. If no data 
    type has been added to the comm, one will be created from the data types
    of the first received message. As these are being assigned and possibly 
    reallocated, they should be pointers to memory addresses that can be
    reallocated (heap).
  @returns int -1 if message could not be received or could not be parsed,
    length of the received message if the message was received and parsed.
 */
#define yggRecvRealloc commRecvRealloc


#ifndef DOXYGEN_SHOULD_SKIP_THIS
/*! @brief Definitions for symmetry, but there is no difference. */
#define yggSend_nolimit commSend
#define yggRecv_nolimit commRecv
#endif // DOXYGEN_SHOULD_SKIP_THIS
 
//==============================================================================
/*!
  Remote Procedure Call (RPC) IO 

  Handle IO case of a server receiving input from clients, performing some
  calculation, and then sending a response back to the client.

  Server Usage:
      1. One-time: Create server channels with format specifiers for input and
         output.
```
            yggRpc_t srv = yggRpcServer("srv_name", "%d", "%d %d");
```
      2. Prepare: Allocate space for recovered variables from request.
```
            int a;
```
      3. Receive request:
```
            int ret = rpcRecv(srv, &a);
```
      4. Process: Do tasks the server should do with input to produce output.
```
            int b = 2*a;
	    int c = 3*a;
```
      5. Send response:
```
	    ret = rpcSend(srv, b, c);
```

  Client Usage:
      1. One-time: Create client channels to desired server with format
         specifiers for output and input (should be the same arguments as for
	 the server except for name).
```
	    yggRpc_t cli = yggRpcClient("cli_name", "%d", "%d %d"); 
```
      2. Prepare: Allocate space for recovered variables from response.
```
            int b, c;
```
      3. Call server:
```
            int ret = rpcCall(cli, 1, &b, &c);
```

   Clients can also send several requests at once before receiving any
   responses. This allows the server to be processing the next requests
   while the client handles the previous response, thereby increasing
   efficiency. The responses are assumed to be in the same order as the
   generating requests (i.e. first come, first served).

*/
//==============================================================================

/*!
  @brief Remote Procedure Call (RPC) alias for a comm_t pointer.
    Contains information required to coordinate sending/receiving 
    response/requests from/to an RPC server/client.
 */
#define yggRpc_t comm_t

/*!
  @brief Constructor for client side RPC structure.
    Creates an instance of yggRpc_t with provided information.  
  @param[in] name constant character pointer to name for queues.
  @param[in] outFormat character pointer to format that should be used for
    formatting output.
  @param[in] inFormat character pointer to format that should be used for
    parsing input.
  @return yggRpc_t structure with provided info.
 */
static inline
comm_t yggRpcClient(const char *name, const char *outFormat, const char *inFormat) {
  dtype_t outType = create_dtype_format(outFormat, 0, false);
  comm_t ret = init_comm(name, SEND, CLIENT_COMM, outType);
  if (ret.comm && inFormat && !set_response_format(ret, inFormat)) {
    free_comm(&ret);
  }
  return ret;
};

/*!
  @brief Constructor for server side RPC structure.
    Creates an instance of yggRpc_t with provided information.  
  @param[in] name constant character pointer to name for queues.
  @param[in] inFormat character pointer to format that should be used for
    parsing input.
  @param[in] outFormat character pointer to format that should be used for
    formatting output.
  @return yggRpc_t structure with provided info.
 */
static inline
comm_t yggRpcServer(const char *name, const char *inFormat, const char *outFormat){
  dtype_t inType = create_dtype_format(inFormat, 0, false);
  comm_t ret = init_comm(name, RECV, SERVER_COMM, inType);
  if (ret.comm && outFormat && !set_response_format(ret, outFormat)) {
    free_comm(&ret);
  }
  return ret;
};

/*!
  @brief Constructor for client side RPC structure w/ explicit type info.
    Creates an instance of yggRpc_t with provided information.  
  @param[in] name constant character pointer to name for queues.
  @param[in] outType Pointer to a dtype_t data structure containing type info
    for data that will be sent by the client.
  @param[in] inType Pointer to a dtype_t data structure containing type info
    for data that will be received by the client.
  @return yggRpc_t structure with provided info.
 */
static inline
comm_t yggRpcClientType(const char *name, dtype_t outType, dtype_t inType) {
  // Allow for any type if provided is NULL
  if (outType.metadata == NULL) {
    outType = create_dtype_empty(true);
  }
  if (inType.metadata == NULL) {
    inType = create_dtype_empty(true);
  }
  comm_t ret = init_comm(name, SEND, CLIENT_COMM, outType);
  if (ret.comm && !set_response_datatype(ret, inType)) {
    free_comm(&ret);
  }
  return ret;
};

/*!
  @brief Constructor for server side RPC structure w/ explicit type info.
    Creates an instance of yggRpc_t with provided information.  
  @param[in] name constant character pointer to name for queues.
  @param[in] inType Pointer to a dtype_t data structure containing type info
    for data that will be received by the server.
  @param[in] outType Pointer to a dtype_t data structure containing type info
    for data that will be sent by the server.
  @return yggRpc_t structure with provided info.
 */
static inline
comm_t yggRpcServerType(const char *name, dtype_t inType, dtype_t outType) {
  // Allow for any type if provided is NULL
  if (inType.metadata == NULL) {
    inType = create_dtype_empty(true);
  }
  if (outType.metadata == NULL) {
    outType = create_dtype_empty(true);
  }
  comm_t ret = init_comm(name, RECV, SERVER_COMM, inType);
  if (ret.comm && !set_response_datatype(ret, outType)) {
    free_comm(&ret);
  }
  return ret;
};

/*!
  @brief Constructor for server side RPC structure w/ explicit type info.
    Creates an instance of yggRpc_t with provided information after first
    checking for a pre-existing global comm of the same name. If one
    doesn't exist, one is created.
  @param[in] name constant character pointer to name for queues.
  @param[in] inType Data structure containing type info for data that will
    be received by the server.
  @param[in] outType Data structure containing type info for data that
    will be sent by the server.
  @return yggRpc_t structure with provided info.
 */
static inline
comm_t yggRpcServerType_global(const char *name, dtype_t inType,
			       dtype_t outType) {
  comm_t out;
  WITH_GLOBAL_SCOPE(out = get_global_scope_comm(name, RECV, SERVER_COMM));
  if (out.comm == NULL) {
    WITH_GLOBAL_SCOPE(out = yggRpcServerType(name, inType, outType));
    return out;
  }
  return out;
};

/*!
  @brief Constructor for client side timestep synchronization calls.
    Creates an instance of comm_t with provided information.  
  @param[in] name constant character pointer to name for queues.
  @param[in] t_units const char* Units that should be used for the
    timestep. "" indicates no units.
  @return comm_t structure with provided info.
 */
static inline
comm_t yggTimesync(const char *name, const char *t_units) {
  char schema[256];
  dtype_t dtype_out;
  if (snprintf(schema, 256, 
	       "{\"type\": \"array\", \"items\": ["
	       "  {\"type\": \"scalar\", \"subtype\": \"float\","
	       "   \"precision\": 8, \"units\": \"%s\"},"
	       "  {\"type\": \"object\"}"
	       "]}", t_units) < 256)
    dtype_out = create_dtype_from_schema(schema, false);
  dtype_t dtype_in = create_dtype_from_schema(
    "{\"type\": \"object\"}", true);
  return yggRpcClientType(name, dtype_out, dtype_in);
};


/*!
  @brief Format and send a message to an RPC output queue.
    Format provided arguments using the output queue format string and
    then sends it to the output queue under the assumption that it is larger
    than the maximum message size.
  @param[in] rpc yggRpc_t structure with RPC information.
  @param[in] ... arguments for formatting.
  @return integer specifying if the send was succesful. Values >= 0 indicate
    success.
 */
#define rpcSend commSend

/*!
  @brief Receive and parse a message from an RPC input queue.
    Receive a message from the input queue under the assumption that it is
    larger than the maximum message size. Then parse the message using the
    input queue format string to extract parameters and assign them to the
    arguments.
  @param[in] rpc RPC comm structure that message should be sent to.
  @param[out] ... mixed arguments that should be assigned parameters
    extracted using the format string. Since these will be assigned, they 
    should be pointers to memory that has already been allocated.
  @return integer specifying if the receive was succesful. Values >= 0 
    indicate success.
*/
#define rpcRecv commRecv
/*!
  @brief Receive a message from a comm into variables passed as arguments.
    If received message data will exceed the bounds of provided variables,
    the variables will be reallocated.
  @param[in] rpc RPC comm structure that message should be sent to.
  @param[out] ... Pointers to variables that should be assigned to by 
    parsing the received message using the associated data type. If no data 
    type has been added to the comm, one will be created from the data types
    of the first received message. As these are being assigned and possibly 
    reallocated, they should be pointers to memory addresses that can be
    reallocated (heap).
  @returns int -1 if message could not be received or could not be parsed,
    length of the received message if the message was received and parsed.
 */
#define rpcRecvRealloc commRecvRealloc

/*!
  @brief Send request to an RPC server from the client and wait for a 
    response. Format arguments using the output queue format string, send 
    the message to the output queue, receive a response from the input 
    queue, and assign arguments from the message using the input queue 
    format string to parse it.
  @param[in] rpc yggRpc_t structure with RPC information.
  @param[in] allow_realloc int If 1, output arguments are assumed to be 
    pointers to pointers such that they can be reallocated as necessary to 
    receive incoming data. If 0, output arguments are assumed to be 
    preallocated.
  @param[in] nargs size_t Number of arguments contained in ap including both
    input and output arguments.
  @param[in,out] ... mixed arguments that include those that should be
    formatted using the output format string, followed by those that should 
    be assigned parameters extracted using the input format string. If 
    allow_realloc is 0, those arguments that will be assigned should be
    pointers to memory that has already been allocated. If allow_realloc is
    1, those arguments that will be assigned should be pointers to pointers
    for memory that can be reallocated.
  @return integer specifying if the receive was succesful. Values >= 0 
    indicate success.
 */
  
/*! @brief Macro to call nrpcCallBase with allow_realloc=0 and the argument count. */
#define rpcCall commCall

/*! @brief Macro to call nrpcCallBase with allow_realloc=1 and the argument count. */
#define rpcCallRealloc commCallRealloc


//==============================================================================
/*!
  Table IO

  Handle I/O from/to an ASCII table either line-by-line or as an array.

  Row-by-Row
  ==========

  Input by Row Usage:
      1. One-time: Create file interface by providing a channel name.
```
	    comm_t fin = yggAsciiTableInput("file_channel");
```
      2. Prepare: Allocate space for variables in row (the format in this
         example is "%5s %d %f\n" like the output example below).
```
	    char a[5];
	    int b;
	    double c;
```
      3. Receive each row, terminating when receive returns -1 (EOF or channel
         closed).
```
	    int ret = 1;
	    while (ret > 0) {
	      ret = yggRecv(fin, &a, &b, &c);
	      // Do something with the row
	    }
```

  Output by Row Usage:
      1. One-time: Create file interface by providing a channel name and a
         format string for rows.
```
	    comm_t fout = yggAsciiTableOutput("file_channel", "%5s %d %f\n");
```
      2. Send rows to the file by providing entries. Formatting is handled by
         the interface. If return value is not 0, the send was not succesful.
```
	    int ret;
	    ret = yggSend(fout, "one", 1, 1.0);
	    ret = yggSend(fout, "two", 2, 2.0);
```

  Array
  =====

  Input by Array Usage:
      1. One-time: Create file interface by providing a channel name.
	    comm_t fin = yggAsciiArrayInput("file_channel");
      2. Prepare: Declare pointers for table columns (they will be allocated by
         the interface once the number of rows is known).
```
	    char *aCol;
	    int *bCol;
	    double *cCol;
```
      3. Receive entire table as columns. Return value will be the number of
         elements in each column (the number of table rows). Negative values
	 indicate errors.
```
            int ret = yggRecv(fin, &a, &b, &c);
```
      4. Cleanup. Call functions to deallocate structures.
```
            free(a);
            free(b);
            free(c);
```

  Output by Array Usage:
      1. One-time: Create file interface by providing a channel name and a
         format string for rows.
```
	    comm_t fout = yggAsciiArrayOutput("file_channel", "%5s %d %f\n");
```
      2. Send columns to the file by providing pointers (or arrays). Formatting
         is handled by the interface. If return value is not 0, the send was not
	 succesful.
```
	    char aCol[] = {"one  ", "two  ", "three"}; \\ Each str is of len 5
	    int bCol[3] = {1, 2, 3};
	    float cCol[3] = {1.0, 2.0, 3.0};
            int ret = yggSend(fout, a, b, c);
```

*/
//==============================================================================

/*! @brief Pointer to an input comm for an ASCII table. */
#define yggAsciiTableInput_t comm_t
/*! @brief Pointer to an output comm for an ASCII table. */
#define yggAsciiTableOutput_t comm_t
/*! @brief Pointer to an input comm for an ASCII table passed as arrays. */
#define yggAsciiArrayInput_t comm_t
/*! @brief Pointer to an output comm for an ASCII table passed as arrays. */
#define yggAsciiArrayOutput_t comm_t

/*!
  @brief Constructor for table output comm to an output channel.
  @param[in] name constant character pointer to output channel name.
  @param[in] format_str character pointer to format string that should be used
  to format rows into table lines.
  @returns comm_t output structure.
 */
static inline
comm_t yggAsciiTableOutput(const char *name, const char *format_str) {
  return yggOutputFmt(name, format_str);
};

/*!
  @brief Constructor for AsciiTable input comm from an input channel.
  @param[in] name constant character pointer to input channel name.
  @returns comm_t input structure.
 */
static inline
comm_t yggAsciiTableInput(const char *name) {
  return yggInput(name);
};

/*!
  @brief Constructor for table output comm with array output.
  @param[in] name constant character pointer to an output channel name.
  @param[in] format_str character pointer to format string that should be 
    used to format rows into table lines.
  @returns comm_t output structure.
 */
static inline
comm_t yggAsciiArrayOutput(const char *name, const char *format_str) {
  dtype_t datatype = create_dtype_format(format_str, 1, false);
  comm_t out = yggOutputType(name, datatype);
  if ((format_str != NULL) && (datatype.metadata == NULL)) {
    ygglog_error_c("yggAsciiArrayOutput: Failed to create type from format_str.");
  }
  return out;
};

/*!
  @brief Constructor for AsciiTable input comm with array input.
  @param[in] name constant character pointer to an input channel name.
  @returns comm_t input structure.
 */
static inline
comm_t yggAsciiArrayInput(const char *name) {
  return yggAsciiTableInput(name);
};


//==============================================================================
/*!
  Ply IO

  Handle I/O from/to a Ply file.

  Input Usage:
      1. One-time: Create file interface by providing a channel name.
```
	    comm_t fin = yggPlyInput("file_channel");  // channel
```
      2. Prepare: Allocate ply structure.
```
            ply_t p;
```
      3. Receive each structure, terminating when receive returns -1 (EOF or channel
         closed).
```
	    int ret = 1;
	    while (ret > 0) {
	      ret = yggRecv(fin, &p);
	      // Do something with the ply structure
	    }
```

  Output by Usage:
      1. One-time: Create file interface by providing a channel name.
```
	    comm_t fout = yggPlyOutput("file_channel");  // channel
```
      2. Send structure to the file by providing entries. Formatting is handled by
         the interface. If return value is not 0, the send was not succesful.
```
	    int ret;
	    ply_t p;
	    // Populate the structure
	    ret = yggSend(fout, p);
	    ret = yggSend(fout, p);
```

*/
//==============================================================================

/*! @brief Pointer to an input comm for ply meshes. */
#define yggPlyInput_t comm_t
/*! @brief Pointer to an output comm for ply meshes. */
#define yggPlyOutput_t comm_t

/*!
  @brief Constructor for ply output comm to an output channel.
  @param[in] name constant character pointer to output channel name.
  @returns comm_t output structure.
 */
static inline
comm_t yggPlyOutput(const char *name) {
  return init_comm(name, SEND, DEFAULT_COMM, create_dtype_ply(false));
};

/*!
  @brief Constructor for ply input comm from an input channel.
  @param[in] name constant character pointer to input channel name.
  @returns comm_t input structure.
 */
static inline
comm_t yggPlyInput(const char *name) {
  return init_comm(name, RECV, DEFAULT_COMM, create_dtype_ply(false));
};


//==============================================================================
/*!
  Obj IO

  Handle I/O from/to a Obj file.

  Input Usage:
      1. One-time: Create file interface by providing a channel name.
```
	    comm_t fin = yggObjInput("file_channel");  // channel
```
      2. Prepare: Allocate obj structure.
```
            obj_t p;
```
      3. Receive each structure, terminating when receive returns -1 (EOF or channel
         closed).
```
	    int ret = 1;
	    while (ret > 0) {
	      ret = yggRecv(fin, &p);
	      // Do something with the obj structure
	    }
```

  Output by Usage:
      1. One-time: Create file interface by providing a channel name.
```
	    comm_t fout = yggObjOutput("file_channel");  // channel
```
      2. Send structure to the file by providing entries. Formatting is handled by
         the interface. If return value is not 0, the send was not succesful.
```
	    int ret;
	    obj_t p;
	    // Populate the structure
	    ret = yggSend(fout, p);
	    ret = yggSend(fout, p);
```

*/
//==============================================================================

/*! @brief Pointer to an input comm for obj meshes. */
#define yggObjInput_t comm_t
/*! @brief Pointer to an output comm for obj meshes. */
#define yggObjOutput_t comm_t

/*!
  @brief Constructor for obj output comm to an output channel.
  @param[in] name constant character pointer to output channel name.
  @returns comm_t output structure.
 */
static inline
comm_t yggObjOutput(const char *name) {
  return init_comm(name, SEND, DEFAULT_COMM, create_dtype_obj(false));
};

/*!
  @brief Constructor for obj input comm from an input channel.
  @param[in] name constant character pointer to input channel name.
  @returns comm_t input structure.
 */
static inline
comm_t yggObjInput(const char *name) {
  return init_comm(name, RECV, DEFAULT_COMM, create_dtype_obj(false));
};


//==============================================================================
/*!
  Generic object I/O.

  Handle I/O from/to a generic object.

  Input Usage:
      1. One-time: Create interface by providing a channel name.
```
	    comm_t fin = yggGenericInput("file_channel");  // channel
```
      2. Prepare: Allocate generic structure.
```
            generic_t p;
```
      3. Receive each structure, terminating when receive returns -1 (EOF or channel
         closed).
```
	    int ret = 1;
	    while (ret > 0) {
	      ret = yggRecv(fin, &p);
	      // Do something with the generic structure
	    }
```

  Output by Usage:
      1. One-time: Create file interface by providing a channel name.
```
	    comm_t fout = yggGenericOutput("file_channel");  // channel
```
      2. Send structure to the file by providing entries. Formatting is handled by
         the interface. If return value is not 0, the send was not succesful.
```
	    int ret;
	    generic_t p;
	    // Populate the structure
	    ret = yggSend(fout, p);
```

*/
//==============================================================================

/*!
  @brief Constructor for generic output comm to an output channel.
  @param[in] name constant character pointer to output channel name.
  @returns comm_t output structure.
 */
static inline
comm_t yggGenericOutput(const char *name) {
  return init_comm(name, SEND, DEFAULT_COMM, create_dtype_empty(true));
};

/*!
  @brief Constructor for generic input comm from an input channel.
  @param[in] name constant character pointer to input channel name.
  @returns comm_t input structure.
 */
static inline
comm_t yggGenericInput(const char *name) {
  return init_comm(name, RECV, DEFAULT_COMM, create_dtype_empty(true));
};

  
//==============================================================================
/*!
  Generic object I/O of any type.

  Handle I/O from/to a generic object of any type.

  Input Usage:
      1. One-time: Create interface by providing a channel name.
```
	    comm_t fin = yggAnyInput("file_channel");  // channel
```
      2. Prepare: Allocate generic structure.
```
            generic_t p;
```
      3. Receive each structure, terminating when receive returns -1 (EOF or channel
         closed).
```
	    int ret = 1;
	    while (ret > 0) {
	      ret = yggRecv(fin, &p);
	      // Do something with the generic structure
	    }
```

  Output by Usage:
      1. One-time: Create file interface by providing a channel name.
```
	    comm_t fout = yggAnyOutput("file_channel");  // channel
```
      2. Send structure to the file by providing entries. Formatting is handled by
         the interface. If return value is not 0, the send was not succesful.
```
	    int ret;
	    generic_t p;
	    // Populate the structure
	    ret = yggSend(fout, p);
```

*/
//==============================================================================

/*!
  @brief Constructor for generic output comm to an output channel.
  @param[in] name constant character pointer to output channel name.
  @returns comm_t output structure.
 */
static inline
comm_t yggAnyOutput(const char *name) {
  return init_comm(name, SEND, DEFAULT_COMM, create_dtype_any(true));
};

/*!
  @brief Constructor for generic input comm from an input channel.
  @param[in] name constant character pointer to input channel name.
  @returns comm_t input structure.
 */
static inline
comm_t yggAnyInput(const char *name) {
  return init_comm(name, RECV, DEFAULT_COMM, create_dtype_any(true));
};

  
//==============================================================================
/*!
  JSON array IO

  Handle I/O from/to a JSON array.

  Input Usage:
      1. One-time: Create interface by providing a channel name.
```
	    comm_t fin = yggJSONArrayInput("file_channel");  // channel
```
      2. Prepare: Allocate vector structure.
```
            json_array_t p;
```
      3. Receive each structure, terminating when receive returns -1 (EOF or channel
         closed).
```
	    int ret = 1;
	    while (ret > 0) {
	      ret = yggRecv(fin, &p);
	      // Do something with the vector structure
	    }
```

  Output Usage:
      1. One-time: Create file interface by providing a channel name.
```
	    comm_t fout = yggJSONArrayOutput("file_channel");  // channel
```
      2. Send structure to the file by providing entries. Formatting is handled by
         the interface. If return value is not 0, the send was not succesful.
```
	    int ret;
	    json_array_t p;
	    // Populate the structure
	    ret = yggSend(fout, p);
```

*/
//==============================================================================

/*!
  @brief Constructor for vector output comm to an output channel.
  @param[in] name constant character pointer to output channel name.
  @returns comm_t output structure.
 */
static inline
comm_t yggJSONArrayOutput(const char *name) {
  return init_comm(name, SEND, DEFAULT_COMM, create_dtype_json_array(0, NULL, true));
};

/*!
  @brief Constructor for vector input comm from an input channel.
  @param[in] name constant character pointer to input channel name.
  @returns comm_t input structure.
 */
static inline
comm_t yggJSONArrayInput(const char *name) {
  return init_comm(name, RECV, DEFAULT_COMM, create_dtype_json_array(0, NULL, true));
};

/*! @brief An alias for yggJSONArrayOutput. */
#define yggVectorOutput yggJSONArrayOutput
/*! @brief An alias for yggJSONArrayInput. */
#define yggVectorInput yggJSONArrayInput
  

//==============================================================================
/*!
  JSON object IO

  Handle I/O from/to a JSON object.

  Input Usage:
      1. One-time: Create interface by providing a channel name.
```
	    comm_t fin = yggJSONObjectInput("file_channel");  // channel
```
      2. Prepare: Allocate map structure.
```
            json_object_t p;
```
      3. Receive each structure, terminating when receive returns -1 (EOF or channel
         closed).
```
	    int ret = 1;
	    while (ret > 0) {
	      ret = yggRecv(fin, &p);
	      // Do something with the map structure
	    }
```

  Output by Usage:
      1. One-time: Create file interface by providing a channel name.
```
	    comm_t fout = yggJSONObjectOutput("file_channel");  // channel
```
      2. Send structure to the file by providing entries. Formatting is handled by
         the interface. If return value is not 0, the send was not succesful.
```
	    int ret;
	    json_object_t p;
	    // Populate the structure
	    ret = yggSend(fout, p);
```

*/
//==============================================================================

/*!
  @brief Constructor for map output comm to an output channel.
  @param[in] name constant character pointer to output channel name.
  @returns comm_t output structure.
 */
static inline
comm_t yggJSONObjectOutput(const char *name) {
  return init_comm(name, SEND, DEFAULT_COMM,
		   create_dtype_json_object(0, NULL, NULL, true));
};

/*!
  @brief Constructor for map input comm from an input channel.
  @param[in] name constant character pointer to input channel name.
  @returns comm_t input structure.
 */
static inline
comm_t yggJSONObjectInput(const char *name) {
  return init_comm(name, RECV, DEFAULT_COMM,
		   create_dtype_json_object(0, NULL, NULL, true));
};

/*! @brief An alias for yggJSONObjectOutput. */
#define yggMapOutput yggJSONObjectOutput
/*! @brief An alias for yggJSONObjectInput. */
#define yggMapInput yggJSONObjectInput


#ifdef __cplusplus /* If this is a C++ compiler, end C linkage */
}
#endif

#endif /*YGGINTERFACE_H_*/
