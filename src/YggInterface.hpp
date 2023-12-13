#pragma once
#include "communicators/comms.hpp"

/*! @brief Memory to keep track of global scope comms. */
#define WITH_GLOBAL_SCOPE(COMM) global_scope_comm_on(); COMM; global_scope_comm_off()


using namespace YggInterface::communicator;

/*!
  @brief Input communicator that can be used to receive messages from
    other models/files in a Yggdrasil integration.
 */
class YggInput : public COMM_BASE {
public:

  /*!
    @brief Constructor for YggInput.
    @param[in] name Name of input channel. This should be named as a
      model input the in YAML for the model calling it.
   */
  explicit YggInput(const char *name) :
    COMM_BASE(name, RECV, COMM_FLAG_INTERFACE) {}
  
  /*!
    @brief Constructor for YggInput w/ C++ std::string.
    @param[in] name Name of input channel. This should be named as a
      model input the in YAML for the model calling it.
   */
  YggInput(const std::string& name) :
    COMM_BASE(name, RECV, COMM_FLAG_INTERFACE) {}

  /*!
    @brief Constructor for YggInput with format.
    @param[in] name Name of input channel. This should be named as a
      model input the in YAML for the model calling it.
    @param[in] fmt Format string specifying the datatype of messages
      that will be received using this comm.
    @param[in] as_array If true, messages will contain arrays for columns
      in the table.
   */
  YggInput(const char *name, const char *fmt, bool as_array = false) :
    COMM_BASE(name, RECV, COMM_FLAG_INTERFACE) {
    this->addFormat(fmt, as_array);
  }

  /*!
    @brief Constructor for YggInput with format.
    @param[in] name Name of input channel. This should be named as a
      model input the in YAML for the model calling it.
    @param[in] fmt Format string specifying the datatype of messages
      that will be received using this comm.
    @param[in] as_array If true, messages will contain arrays for columns
      in the table.
   */
  YggInput(const std::string& name, const std::string& fmt,
	   bool as_array = false) :
    COMM_BASE(name, RECV, COMM_FLAG_INTERFACE) {
    this->addFormat(fmt, as_array);
  }    

  /*!
    @brief Constructor for YggInput with explicit datatype.
    @param[in] name Name of input channel. This should be named as a
      model input the in YAML for the model calling it.
    @param[in] schema Document containing JSON schema describing the type
      of data expected by the communicator.
   */
  YggInput(const char *name, const rapidjson::Document& schema) :
    COMM_BASE(name, RECV, COMM_FLAG_INTERFACE) {
    this->addSchema(schema);
  }

  /*!
    @brief Constructor for YggInput with explicit datatype.
    @param[in] name Name of input channel. This should be named as a
      model input the in YAML for the model calling it.
    @param[in] schema Document containing JSON schema describing the type
      of data expected by the communicator.
   */
  YggInput(const std::string& name, const rapidjson::Document& schema) :
    COMM_BASE(name, RECV, COMM_FLAG_INTERFACE) {
    this->addSchema(schema);
  }

};


/*!
  @brief Output communicator that can be used to send messages to
    other models/files in a Yggdrasil integration.
 */
class YggOutput : public COMM_BASE {
public:
  
  /*!
    @brief Constructor for YggOutput.
    @param[in] name Name of output channel. This should be named as a
      model output the in YAML for the model calling it.
   */
  explicit YggOutput(const char *name) :
    COMM_BASE(name, SEND, COMM_FLAG_INTERFACE) {}
  
  /*!
    @brief Constructor for YggOutput.
    @param[in] name Name of output channel. This should be named as a
      model output the in YAML for the model calling it.
   */
  YggOutput(const std::string& name) :
    COMM_BASE(name, SEND, COMM_FLAG_INTERFACE) {}
  
  /*!
    @brief Constructor for YggOutput with format.
    @param[in] name Name of output channel. This should be named as a
      model output the in YAML for the model calling it.
    @param[in] fmt Format string specifying the datatype of messages
      that will be sent using this comm. If messages are sent to an
      ASCII table, fmt will also be used to format messages in the table.
    @param[in] as_array If true, messages will contain arrays for columns
      in the table.
   */
  YggOutput(const char *name, const char *fmt, bool as_array=false) :
    COMM_BASE(name, SEND, COMM_FLAG_INTERFACE) {
    this->addFormat(fmt, as_array);
  }

  /*!
    @brief Constructor for YggOutput with format.
    @param[in] name Name of output channel. This should be named as a
      model output the in YAML for the model calling it.
    @param[in] fmt Format string specifying the datatype of messages
      that will be sent using this comm. If messages are sent to an
      ASCII table, fmt will also be used to format messages in the table.
    @param[in] as_array If true, messages will contain arrays for columns
      in the table.
   */
  YggOutput(const std::string& name, const std::string& fmt,
	    bool as_array=false) :
    COMM_BASE(name, SEND, COMM_FLAG_INTERFACE) {
    this->addFormat(fmt, as_array);
  }
    

  /*!
    @brief Constructor for YggOutput with explicit datatype.
    @param[in] name Name of output channel. This should be named as a
      model output the in YAML for the model calling it.
    @param[in] schema Document containing JSON schema describing the type
      of data expected by the communicator.
   */
  YggOutput(const char *name, rapidjson::Document& schema) :
    COMM_BASE(name, SEND, COMM_FLAG_INTERFACE) {
    this->addSchema(schema);
  }

  /*!
    @brief Constructor for YggOutput with explicit datatype.
    @param[in] name Name of output channel. This should be named as a
      model output the in YAML for the model calling it.
    @param[in] schema Document containing JSON schema describing the type
      of data expected by the communicator.
   */
  YggOutput(const std::string& name, rapidjson::Document& schema) :
    COMM_BASE(name, SEND, COMM_FLAG_INTERFACE) {
    this->addSchema(schema);
  }

};
	

/*!
  @brief Server communicator that can be used to receive and reply to
    requests from client models in a Yggdrasil integration.
 */
class YggRpcServer : public ServerComm {
public:

  /*!
    @brief Constructor for YggRpcServer.
    @param[in] name Name of server input channel. This should be named in
      a model's 'server' field in the YAML or the name of the model with
      'server' set to 'true'.
  */
  explicit YggRpcServer(const char *name) :
    ServerComm(name, COMM_FLAG_INTERFACE) {}
  /*!
    @brief Constructor for YggRpcServer.
    @param[in] name Name of server input channel. This should be named in
      a model's 'server' field in the YAML or the name of the model with
      'server' set to 'true'.
    @param[in] inFormat Format string specifying the datatype of messages
      that will be received using this comm.
    @param[in] outFormat Format string specifying the datatype of
      messages that will be sent using this comm.
   */
  YggRpcServer(const char *name, const char *inFormat,
	       const char *outFormat) :
    ServerComm(name, COMM_FLAG_INTERFACE) {
    this->addFormat(inFormat);
    this->addResponseFormat(outFormat);
  }

  /*!
    @brief Constructor for YggRpcServer.
    @param[in] name Name of server input channel. This should be named in
      a model's 'server' field in the YAML or the name of the model with
      'server' set to 'true'.
    @param[in] inFormat Format string specifying the datatype of messages
      that will be received using this comm.
    @param[in] outFormat Format string specifying the datatype of
      messages that will be sent using this comm.
   */
  YggRpcServer(const std::string& name, const std::string& inFormat,
	       const std::string& outFormat) :
    ServerComm(name, COMM_FLAG_INTERFACE) {
    this->addFormat(inFormat);
    this->addResponseFormat(outFormat);
  }

  /*!
    @brief Constructor for YggRpcServer with explicit datatype.
    @param[in] name Name of server input channel. This should be named in
      a model's 'server' field in the YAML or the name of the model with
      'server' set to 'true'.
    @param[in] inType Document containing JSON schema describing the type
      of data expected to be received by the communicator.
    @param[in] outType Document containing JSON schema describing the type
      of data expected to be sent by the communicator.
   */
  YggRpcServer(const std::string& name, const rapidjson::Document& inType,
	       const rapidjson::Document& outType) :
    ServerComm(name, COMM_FLAG_INTERFACE) {
    this->addSchema(inType);
    this->addResponseSchema(outType);
  }

};


/*!
  @brief Client communicator that can be used to send requests to and
    receive replies from server models in a Yggdrasil integration.
 */
class YggRpcClient : public ClientComm {
public:

  /*!
    @brief Constructor for YggRpcClient.
    @param[in] name Name of client input channel. This should be of the
      format '{server model name}_{client model name}' with the model
      names specified in the YAML.
  */
  explicit YggRpcClient(const char *name) :
    ClientComm(name, COMM_FLAG_INTERFACE) {}
  /*!
    @brief Constructor for YggRpcClient.
    @param[in] name Name of client input channel. This should be of the
      format '{server model name}_{client model name}' with the model
      names specified in the YAML.
  */
  YggRpcClient(const std::string& name) :
    ClientComm(name, COMM_FLAG_INTERFACE) {}
  /*!
    @brief Constructor for YggRpcClient.
    @param[in] name Name of client input channel. This should be of the
      format '{server model name}_{client model name}' with the model
      names specified in the YAML.
    @param[in] outFormat Format string specifying the datatype of
      messages that will be sent using this comm.
    @param[in] inFormat Format string specifying the datatype of messages
      that will be received using this comm.
   */
  YggRpcClient(const char *name, const char *outFormat,
	       const char *inFormat) :
    ClientComm(name, COMM_FLAG_INTERFACE) {
    this->addFormat(std::string(outFormat));
    this->addResponseFormat(std::string(inFormat));
  }
    

  /*!
    @brief Constructor for YggRpcClient.
    @param[in] name Name of client input channel. This should be of the
      format '{server model name}_{client model name}' with the model
      names specified in the YAML.
    @param[in] outFormat Format string specifying the datatype of
      messages that will be sent using this comm.
    @param[in] inFormat Format string specifying the datatype of messages
      that will be received using this comm.
   */
  YggRpcClient(const std::string& name, const std::string& outFormat,
	       const std::string& inFormat) :
    ClientComm(name, COMM_FLAG_INTERFACE) {
    this->addFormat(outFormat);
    this->addResponseFormat(inFormat);
  }

  /*!
    @brief Constructor for YggRpcClient with explicit datatype.
    @param[in] name Name of client input channel. This should be of the
      format '{server model name}_{client model name}' with the model
      names specified in the YAML.
    @param[in] outType Document containing JSON schema describing the type
      of data expected to be sent by the communicator.
    @param[in] inType Document containing JSON schema describing the type
      of data expected to be received by the communicator.
   */
  YggRpcClient(const std::string& name, const rapidjson::Document& outType,
	       const rapidjson::Document& inType) :
    ClientComm(name, COMM_FLAG_INTERFACE) {
    this->addSchema(outType);
    this->addResponseSchema(inType);
  }
  
};


/*!
  @brief Timesync communicator for syncing model states with other
    models in a Yggdrasil integration.
 */
class YggTimesync : public YggRpcClient {
public:

  /*!
    @brief Constructor for YggTimesync.
    @param[in] name Name of timesync input channel. This should be
      the name of the timesync parameter in the YAML of the model
      calling it. If one is not provided, it will default to
      'timesync'.
    @param[in] t_units Units that should be used for the timestep. ""
      indicates no units.
   */
  YggTimesync(const std::string& name="timesync",
	      const std::string& t_units="") :
    YggRpcClient(name) {
    this->addSchema(
      "{ \"type\": \"array\","
      "  \"items\": ["
      "    {"
      "      \"type\": \"scalar\", "
      "      \"subtype\": \"float\","
      "      \"precision\": 8"
      "    },"
      "    { \"type\": \"object\" }"
      "  ]"
      "}");
    if (t_units.size() > 0) {
      (*(this->metadata.schema))["items"][0].AddMember(
	 rapidjson::Value("units", 5,
			  this->metadata.GetAllocator()).Move(),
	 rapidjson::Value(t_units.c_str(),
			  static_cast<rapidjson::SizeType>(t_units.size()),
			  this->metadata.GetAllocator()).Move(),
	 this->metadata.GetAllocator());
    }
    this->addResponseSchema("{ \"type\": \"object\" }", true);
  }
  
  /*!
    @brief Constructor for YggTimesync.
    @param[in] name Name of timesync input channel. This should be
      the name of the timesync parameter in the YAML of the model
      calling it. If one is not provided, it will default to
      'timesync'.
    @param[in] t_units Units that should be used for the timestep. ""
      indicates no units.
   */
  YggTimesync(const char *name="timesync", const char *t_units="") :
    YggTimesync(std::string(name), std::string(t_units)) {}
  
};


/*!
  @brief Output communicator that can be used to send bytes messages to
    other models/files in a Yggdrasil integration.
 */
class YggAsciiFileOutput : public YggOutput {
public:

  /*!
    @brief Constructor for YggAsciiFileOutput.
    @param[in] name Name of output channel. This should be named as a
      model output the in YAML for the model calling it.
   */
  explicit YggAsciiFileOutput(const char *name) :
    YggOutput(name) {}
  
  /*!
    @brief Constructor for YggAsciiFileOutput.
    @param[in] name Name of output channel. This should be named as a
      model output the in YAML for the model calling it.
   */
  explicit YggAsciiFileOutput(const std::string& name) :
    YggOutput(name) {}
  
  /*!
    @brief Send a single line to a file or queue.
    @param[in] line character pointer to line that should be sent.
    @returns int 0 if send was succesfull. All other values indicate errors.
   */
  int send_line(const char *line) { return send(line, strlen(line)); }

};


/*!
  @brief Input communicator that can be used to receive bytes messages
    from other models/files in a Yggdrasil integration.
 */
class YggAsciiFileInput : public YggInput {
public:

  /*!
    @brief Constructor for YggAsciiFileInput.
    @param[in] name Name of input channel. This should be named as a
      model input the in YAML for the model calling it.
   */
  explicit YggAsciiFileInput(const char *name) :
    YggInput(name) {}

  /*!
    @brief Constructor for YggAsciiFileInput.
    @param[in] name Name of input channel. This should be named as a
      model input the in YAML for the model calling it.
   */
  explicit YggAsciiFileInput(const std::string& name) :
    YggInput(name) {}

  /*!
    @brief Receive a single line from an associated file or queue.
    @param[out] line Pointer to allocate memory where the received
      should be stored.
    @param[in] n Size of the allocated memory block in bytes.
    @returns Number of bytes read/received. Negative values indicate
      that there was either an error or the EOF message was received.
   */
  long recv_line(char *line, const size_t n)
  { return this->recv(line, n, false); }
  
};


/*!
  @brief Output communicator that can be used to send table rows to
    other models/files in a Yggdrasil integration.
 */
class YggAsciiTableOutput : public YggOutput {
public:

  /*!
    @brief Constructor for YggAsciiTableOutput.
    @param[in] name Name of output channel. This should be named as a
      model output the in YAML for the model calling it.
    @param[in] fmt Format string specifying the datatype of messages
      that will be sent using this comm. If messages are sent to an
      ASCII table, fmt will also be used to format messages in the table.
   */
  YggAsciiTableOutput(const char *name, const char *fmt) :
    YggOutput(name, fmt) {}

  /*!
    @brief Constructor for YggAsciiTableOutput.
    @param[in] name Name of output channel. This should be named as a
      model output the in YAML for the model calling it.
    @param[in] fmt Format string specifying the datatype of messages
      that will be sent using this comm. If messages are sent to an
      ASCII table, fmt will also be used to format messages in the table.
   */
  YggAsciiTableOutput(const std::string& name,
		      const std::string& fmt) :
    YggOutput(name, fmt) {}

};


/*!
  @brief Input communicator that can be used to receive table rows
    from other models/files in a Yggdrasil integration.
 */
class YggAsciiTableInput : public YggInput {
public:

  /*!
    @brief Constructor for YggAsciiTableInput.
    @param[in] name Name of input channel. This should be named as a
      model input the in YAML for the model calling it.
   */
  explicit YggAsciiTableInput(const char *name) :
    YggInput(name) {}

  /*!
    @brief Constructor for YggAsciiTableInput.
    @param[in] name Name of input channel. This should be named as a
      model input the in YAML for the model calling it.
   */
  YggAsciiTableInput(const std::string& name) :
    YggInput(name) {}

};

/*!
  @brief Output communicator that can be used to send table columns to
    other models/files in a Yggdrasil integration as arrays.
 */
class YggAsciiArrayOutput : public YggOutput {
public:

  /*!
    @brief Constructor for YggAsciiArrayOutput.
    @param[in] name Name of output channel. This should be named as a
      model output the in YAML for the model calling it.
    @param[in] fmt Format string specifying the datatype of messages
      that will be sent using this comm. If messages are sent to an
      ASCII table, fmt will also be used to format messages in the table.
   */
  YggAsciiArrayOutput(const char *name, const char *fmt) :
    YggOutput(name, fmt, true) {}

  /*!
    @brief Constructor for YggAsciiArrayOutput.
    @param[in] name Name of output channel. This should be named as a
      model output the in YAML for the model calling it.
    @param[in] fmt Format string specifying the datatype of messages
      that will be sent using this comm. If messages are sent to an
      ASCII table, fmt will also be used to format messages in the table.
   */
  YggAsciiArrayOutput(const std::string& name, const std::string& fmt) :
    YggOutput(name, fmt, true) {}

};


/*!
  @brief Input communicator that can be used to receive table columns
    from other models/files in a Yggdrasil integration as arrays.
 */
class YggAsciiArrayInput : public YggInput {
public:

  /*!
    @brief Constructor for YggAsciiArrayInput.
    @param[in] name Name of output channel. This should be named as a
      model output the in YAML for the model calling it.
   */
  explicit YggAsciiArrayInput(const char *name) :
    YggInput(name) {}

  /*!
    @brief Constructor for YggAsciiArrayInput.
    @param[in] name Name of output channel. This should be named as a
      model output the in YAML for the model calling it.
   */
  YggAsciiArrayInput(const std::string& name) :
    YggInput(name) {}

};


/*!
  @brief Output communicator that can be used to send Ply objects to
    other models/files in a Yggdrasil integration.
 */
class YggPlyOutput : public YggOutput {
public:

  /*!
    @brief Constructor for YggPlyOutput.
    @param[in] name Name of output channel. This should be named as a
      model output the in YAML for the model calling it.
   */
  explicit YggPlyOutput(const char *name) :
    YggOutput(name) {
    this->addSchema("{\"type\": \"ply\"}");
  }
  
  /*!
    @brief Constructor for YggPlyOutput.
    @param[in] name Name of output channel. This should be named as a
      model output the in YAML for the model calling it.
   */
  YggPlyOutput(const std::string& name) :
    YggOutput(name) {
    this->addSchema("{\"type\": \"ply\"}");
  }

};


/*!
  @brief Input communicator that can be used to receive Ply objects from
    other models/files in a Yggdrasil integration.
 */
class YggPlyInput : public YggInput {
public:

  /*!
    @brief Constructor for YggPlyInput.
    @param[in] name Name of input channel. This should be named as a
      model input the in YAML for the model calling it.
   */
  explicit YggPlyInput(const char *name) :
    YggInput(name) {
    this->addSchema("{\"type\": \"ply\"}");
  }

  /*!
    @brief Constructor for YggPlyInput.
    @param[in] name Name of input channel. This should be named as a
      model input the in YAML for the model calling it.
   */
  YggPlyInput(const std::string& name) :
    YggInput(name) {
    this->addSchema("{\"type\": \"ply\"}");
  }
  
};


/*!
  @brief Output communicator that can be used to send ObjWavefront
    objects to other models/files in a Yggdrasil integration.
 */
class YggObjOutput : public YggOutput {
public:

  /*!
    @brief Constructor for YggObjOutput.
    @param[in] name Name of output channel. This should be named as a
      model output the in YAML for the model calling it.
   */
  explicit YggObjOutput(const char *name) :
    YggOutput(name) {
    this->addSchema("{\"type\": \"obj\"}");
  }
  
  /*!
    @brief Constructor for YggObjOutput.
    @param[in] name Name of output channel. This should be named as a
      model output the in YAML for the model calling it.
   */
  YggObjOutput(const std::string& name) :
    YggOutput(name) {
    this->addSchema("{\"type\": \"obj\"}");
  }
  
};


/*!
  @brief Input communicator that can be used to receive ObjWavefront
    objects from other models/files in a Yggdrasil integration.
 */
class YggObjInput : public YggInput {
public:

  /*!
    @brief Constructor for YggObjInput.
    @param[in] name Name of input channel. This should be named as a
      model input the in YAML for the model calling it.
   */
  explicit YggObjInput(const char *name) :
    YggInput(name) {
    this->addSchema("{\"type\": \"obj\"}");
  }

  /*!
    @brief Constructor for YggObjInput.
    @param[in] name Name of input channel. This should be named as a
      model input the in YAML for the model calling it.
   */
  YggObjInput(const std::string& name) :
    YggInput(name) {
    this->addSchema("{\"type\": \"obj\"}");
  }

};


/*!
  @brief Output communicator that can be used to send rapidjson::Document
    objects to other models/files in a Yggdrasil integration.
 */
class YggGenericOutput : public YggOutput {
public:

  /*!
    @brief Constructor for YggGenericOutput.
    @param[in] name Name of output channel. This should be named as a
      model output the in YAML for the model calling it.
   */
  explicit YggGenericOutput(const char *name) :
    YggOutput(name) {
    this->addSchema("{\"type\": \"any\"}");
  }
  
  /*!
    @brief Constructor for YggGenericOutput.
    @param[in] name Name of output channel. This should be named as a
      model output the in YAML for the model calling it.
   */
  YggGenericOutput(const std::string& name) :
    YggOutput(name) {
    this->addSchema("{\"type\": \"any\"}");
  }
  
};


/*!
  @brief Input communicator that can be used to receive
    rapidjson::Document objects from other models/files in a
    Yggdrasil integration.
 */
class YggGenericInput : public YggInput {
public:

  /*!
    @brief Constructor for YggGenericInput.
    @param[in] name Name of input channel. This should be named as a
      model input the in YAML for the model calling it.
   */
  explicit YggGenericInput(const char *name) :
    YggInput(name) {
    this->addSchema("{\"type\": \"any\"}");
  }

  /*!
    @brief Constructor for YggGenericInput.
    @param[in] name Name of input channel. This should be named as a
      model input the in YAML for the model calling it.
   */
  YggGenericInput(const std::string& name) :
    YggInput(name) {
    this->addSchema("{\"type\": \"any\"}");
  }

};


/*!
  @brief Output communicator that can be used to send rapidjson::Document
    objects to other models/files in a Yggdrasil integration.
 */
class YggAnyOutput : public YggOutput {
public:

  /*!
    @brief Constructor for YggAnyOutput.
    @param[in] name Name of output channel. This should be named as a
      model output the in YAML for the model calling it.
   */
  explicit YggAnyOutput(const char *name) :
    YggOutput(name) {
    this->addSchema("{\"type\": \"any\"}");
  }
  
  /*!
    @brief Constructor for YggAnyOutput.
    @param[in] name Name of output channel. This should be named as a
      model output the in YAML for the model calling it.
   */
  YggAnyOutput(const std::string& name) :
    YggOutput(name) {
    this->addSchema("{\"type\": \"any\"}");
  }
  
};


/*!
  @brief Input communicator that can be used to receive
    rapidjson::Document objects from other models/files in a
    Yggdrasil integration.
 */
class YggAnyInput : public YggInput {
public:

  /*!
    @brief Constructor for YggAnyInput.
    @param[in] name Name of input channel. This should be named as a
      model input the in YAML for the model calling it.
   */
  explicit YggAnyInput(const char *name) :
    YggInput(name) {
    this->addSchema("{\"type\": \"any\"}");
  }

  /*!
    @brief Constructor for YggAnyInput.
    @param[in] name Name of input channel. This should be named as a
      model input the in YAML for the model calling it.
   */
  YggAnyInput(const std::string& name) :
    YggInput(name) {
    this->addSchema("{\"type\": \"any\"}");
  }

};


/*!
  @brief Output communicator that can be used to send rapidjson::Document
    array type objects to other models/files in a Yggdrasil integration.
 */
class YggJSONArrayOutput : public YggOutput {
public:

  /*!
    @brief Constructor for YggJSONArrayOutput.
    @param[in] name Name of output channel. This should be named as a
      model output the in YAML for the model calling it.
   */
  explicit YggJSONArrayOutput(const char *name) :
    YggOutput(name) {
    this->addSchema("{\"type\": \"array\"}");
  }
  
  /*!
    @brief Constructor for YggJSONArrayOutput.
    @param[in] name Name of output channel. This should be named as a
      model output the in YAML for the model calling it.
   */
  YggJSONArrayOutput(const std::string& name) :
    YggOutput(name) {
    this->addSchema("{\"type\": \"array\"}");
  }
  
};


/*!
  @brief Input communicator that can be used to receive
    rapidjson::Document array type objects from other models/files in a
    Yggdrasil integration.
 */
class YggJSONArrayInput : public YggInput {
public:

  /*!
    @brief Constructor for YggJSONArrayInput.
    @param[in] name Name of input channel. This should be named as a
      model input the in YAML for the model calling it.
   */
  explicit YggJSONArrayInput(const char *name) :
    YggInput(name) {
    this->addSchema("{\"type\": \"array\"}");
  }

  /*!
    @brief Constructor for YggJSONArrayInput.
    @param[in] name constant std::string the name of an input channel.
   */
  YggJSONArrayInput(const std::string& name) :
    YggInput(name) {
    this->addSchema("{\"type\": \"array\"}");
  }

};


/*!
  @brief Output communicator that can be used to send rapidjson::Document
    object type objects to other models/files in a Yggdrasil integration.
 */
class YggJSONObjectOutput : public YggOutput {
public:

  /*!
    @brief Constructor for YggJSONObjectOutput.
    @param[in] name Name of output channel. This should be named as a
      model output the in YAML for the model calling it.
   */
  explicit YggJSONObjectOutput(const char *name) :
    YggOutput(name) {
    this->addSchema("{\"type\": \"object\"}");
  }
  
  /*!
    @brief Constructor for YggJSONObjectOutput.
    @param[in] name Name of output channel. This should be named as a
      model output the in YAML for the model calling it.
   */
  YggJSONObjectOutput(const std::string& name) :
    YggOutput(name) {
    this->addSchema("{\"type\": \"object\"}");
  }
  
};


/*!
  @brief Input communicator that can be used to receive
    rapidjson::Document object type objects from other models/files in a
    Yggdrasil integration.
 */
class YggJSONObjectInput : public YggInput {
public:

  /*!
    @brief Constructor for YggJSONObjectInput.
    @param[in] name Name of input channel. This should be named as a
      model input the in YAML for the model calling it.
   */
  explicit YggJSONObjectInput(const char *name) :
    YggInput(name) {
    this->addSchema("{\"type\": \"object\"}");
  }

  /*!
    @brief Constructor for YggJSONObjectInput.
    @param[in] name Name of input channel. This should be named as a
      model input the in YAML for the model calling it.
   */
  YggJSONObjectInput(const std::string& name) :
    YggInput(name) {
    this->addSchema("{\"type\": \"object\"}");
  }
  
};
