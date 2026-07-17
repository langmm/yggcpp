#include "YggInterface.hpp"

using namespace YggInterface;
using namespace YggInterface::communicator;

YggInput::YggInput(const std::string nme, FLAG_TYPE flags,
                   const COMM_TYPE commtype) :
  WrapComm(std::move(nme), utils::blankAddress,
           RECV, flags | COMM_FLAG_INTERFACE, commtype) {}


YggInput::YggInput(const std::string nme, const std::string fmt,
                   bool as_array, FLAG_TYPE flags,
                   const COMM_TYPE commtype) :
  WrapComm(nme, utils::blankAddress,
           RECV, flags | COMM_FLAG_INTERFACE, commtype) {
  if (!this->addFormat(fmt, as_array))
    this->throw_error("Invalid format");  // GCOV_EXCL_LINE
}

YggInput::YggInput(const std::string nme,
                   const yggdrasil_rapidjson::Document& schema,
                   FLAG_TYPE flags, const COMM_TYPE commtype) :
  WrapComm(nme, utils::blankAddress,
           RECV, flags | COMM_FLAG_INTERFACE, commtype) {
  if (!this->addSchema(schema))
    this->throw_error("Invalid schema");  // GCOV_EXCL_LINE
}

YggOutput::YggOutput(const std::string nme, FLAG_TYPE flags,
                     const COMM_TYPE commtype) :
  WrapComm(nme, utils::blankAddress,
           SEND, flags | COMM_FLAG_INTERFACE, commtype) {}

YggOutput::YggOutput(const std::string nme, const std::string fmt,
                     bool as_array, FLAG_TYPE flags,
                     const COMM_TYPE commtype) :
  WrapComm(nme, utils::blankAddress,
           SEND, flags | COMM_FLAG_INTERFACE, commtype) {
  if (!this->addFormat(fmt, as_array))
    this->throw_error("Invalid format");  // GCOV_EXCL_LINE
}

YggOutput::YggOutput(const std::string nme,
                     yggdrasil_rapidjson::Document& schema,
                     FLAG_TYPE flags, const COMM_TYPE commtype) :
  WrapComm(nme, utils::blankAddress,
           SEND, flags | COMM_FLAG_INTERFACE, commtype) {
  if (!this->addSchema(schema))
    this->throw_error("Invalid schema");  // GCOV_EXCL_LINE
}

YggRpcServer::YggRpcServer(const std::string name, FLAG_TYPE flags,
                           const COMM_TYPE request_commtype,
                           const COMM_TYPE response_commtype) :
  ServerComm(name, flags | COMM_FLAG_INTERFACE,
             SERVER_COMM, request_commtype, response_commtype) {}


YggRpcServer::YggRpcServer(const std::string name, const std::string inFormat,
                           const std::string outFormat, FLAG_TYPE flags,
                           const COMM_TYPE request_commtype,
                           const COMM_TYPE response_commtype) :
  ServerComm(name, flags | COMM_FLAG_INTERFACE,
             SERVER_COMM, request_commtype, response_commtype) {
  if (!this->addFormat(inFormat))
    this->throw_error("Invalid request format");
  if (!this->addResponseFormat(outFormat))
    this->throw_error("Invalid response format");
}

YggRpcServer::YggRpcServer(const std::string name, const yggdrasil_rapidjson::Document& inType,
                           const yggdrasil_rapidjson::Document& outType, FLAG_TYPE flags,
                           const COMM_TYPE request_commtype,
                           const COMM_TYPE response_commtype) :
  ServerComm(name, flags | COMM_FLAG_INTERFACE,
             SERVER_COMM, request_commtype, response_commtype) {
  if (!this->addSchema(inType))
    this->throw_error("Invalid request schema");  // GCOV_EXCL_LINE
  if (!this->addResponseSchema(outType))
    this->throw_error("Invalid response schema");  // GCOV_EXCL_LINE
}

YggRpcClient::YggRpcClient(const std::string name, FLAG_TYPE flags,
                           const COMM_TYPE request_commtype,
                           const COMM_TYPE response_commtype) :
  ClientComm(name, flags | COMM_FLAG_INTERFACE,
             CLIENT_COMM, request_commtype, response_commtype) {}

YggRpcClient::YggRpcClient(const std::string name, const std::string outFormat,
                           const std::string inFormat, FLAG_TYPE flags,
                           const COMM_TYPE request_commtype,
                           const COMM_TYPE response_commtype) :
  ClientComm(name, flags | COMM_FLAG_INTERFACE,
             CLIENT_COMM, request_commtype, response_commtype) {
  if (!this->addFormat(outFormat))
    this->throw_error("Invalid request format");
  if (!this->addResponseFormat(inFormat))
    this->throw_error("Invalid response format");
}

YggRpcClient::YggRpcClient(const std::string name,
                           const yggdrasil_rapidjson::Document& outType,
                           const yggdrasil_rapidjson::Document& inType,
                           FLAG_TYPE flags,
                           const COMM_TYPE request_commtype,
                           const COMM_TYPE response_commtype) :
  ClientComm(name, flags | COMM_FLAG_INTERFACE,
             CLIENT_COMM, request_commtype, response_commtype) {
  if (!this->addSchema(outType))
    this->throw_error("Invalid request schema");  // GCOV_EXCL_LINE
  if (!this->addResponseSchema(inType))
    this->throw_error("Invalid response schema");  // GCOV_EXCL_LINE
}

YggTimesync::YggTimesync(const std::string name,
                         const std::string t_units,
                         FLAG_TYPE flags,
                         const COMM_TYPE request_commtype,
                         const COMM_TYPE response_commtype) :
  YggRpcClient(name, flags, request_commtype, response_commtype) {
  if (!this->addSchema("{ \"type\": \"array\","
                       "  \"items\": ["
                       "    {"
                       "      \"type\": \"scalar\", "
                       "      \"subtype\": \"float\","
                       "      \"precision\": 8"
                       "    },"
                       "    { \"type\": \"object\" }"
                       "  ]"
                       "}"))
    this->throw_error("Invalid time schema");  // GCOV_EXCL_LINE
  if (t_units.size() > 0) {
    (*(this->getMetadata().getSchema()))["items"][0].AddMember(
	 yggdrasil_rapidjson::Value("units", 5,
	       	  this->getMetadata().GetAllocator()).Move(),
	 yggdrasil_rapidjson::Value(t_units.c_str(),
	       	  static_cast<yggdrasil_rapidjson::SizeType>(t_units.size()),
       		  this->getMetadata().GetAllocator()).Move(),
	 this->getMetadata().GetAllocator());
  }
  if (!this->addResponseSchema("{ \"type\": \"object\" }", true))
    this->throw_error("Invalid state schema");  // GCOV_EXCL_LINE
}

YggAsciiFileOutput::YggAsciiFileOutput(const std::string name,
                                       FLAG_TYPE flags,
                                       const COMM_TYPE commtype) :
  YggOutput(name, flags, commtype) {}

int YggAsciiFileOutput::send_line(const char *line) {
  return send(line, strlen(line));
}

YggAsciiFileInput::YggAsciiFileInput(const std::string name,
                                     FLAG_TYPE flags,
                                     const COMM_TYPE commtype) :
  YggInput(name, flags, commtype) {}

long YggAsciiFileInput::recv_line(char *line, const size_t n) {
  return this->recv(line, n, false);
}

YggAsciiTableOutput::YggAsciiTableOutput(const std::string name,
                                         const std::string fmt,
                                         FLAG_TYPE flags,
                                         const COMM_TYPE commtype) :
  YggOutput(name, fmt, false, flags, commtype) {}

YggAsciiTableInput::YggAsciiTableInput(const std::string name,
                                       FLAG_TYPE flags,
                                       const COMM_TYPE commtype) :
  YggInput(name, flags, commtype) {}

YggAsciiTableInput::YggAsciiTableInput(const std::string name,
                                       const std::string fmt,
                                       FLAG_TYPE flags,
                                       const COMM_TYPE commtype) :
  YggInput(name, fmt, false, flags, commtype) {}

YggAsciiArrayOutput::YggAsciiArrayOutput(const std::string name, const std::string fmt,
                                         FLAG_TYPE flags,
                                         const COMM_TYPE commtype) :
  YggOutput(name, fmt, true, flags, commtype) {}

YggAsciiArrayInput::YggAsciiArrayInput(const std::string name,
                                       FLAG_TYPE flags,
                                       const COMM_TYPE commtype) :
  YggInput(name, flags, commtype) {}

YggAsciiArrayInput::YggAsciiArrayInput(const std::string name,
                                       const std::string fmt,
                                       FLAG_TYPE flags,
                                       const COMM_TYPE commtype) :
  YggInput(name, fmt, true, flags, commtype) {}

YggPlyOutput::YggPlyOutput(const std::string name,
                           FLAG_TYPE flags,
                           const COMM_TYPE commtype) :
  YggOutput(name, flags, commtype) {
  if (!this->addSchema("{\"type\": \"ply\"}"))
    this->throw_error("Invalid schema");  // GCOV_EXCL_LINE
}

YggPlyInput::YggPlyInput(const std::string name,
                         FLAG_TYPE flags,
                         const COMM_TYPE commtype) :
  YggInput(name, flags, commtype) {
  if (!this->addSchema("{\"type\": \"ply\"}"))
    this->throw_error("Invalid schema");  // GCOV_EXCL_LINE
}

YggObjOutput::YggObjOutput(const std::string name,
                           FLAG_TYPE flags,
                           const COMM_TYPE commtype) :
  YggOutput(name, flags, commtype) {
  if (!this->addSchema("{\"type\": \"obj\"}"))
    this->throw_error("Invalid schema");  // GCOV_EXCL_LINE
}

YggObjInput::YggObjInput(const std::string name,
                         FLAG_TYPE flags,
                         const COMM_TYPE commtype) :
  YggInput(name, flags, commtype) {
  if (!this->addSchema("{\"type\": \"obj\"}"))
    this->throw_error("Invalid schema");  // GCOV_EXCL_LINE
}

YggGenericOutput::YggGenericOutput(const std::string name,
                                   FLAG_TYPE flags,
                                   const COMM_TYPE commtype) :
  YggOutput(name, flags, commtype) {
  if (!this->addSchema("{\"type\": \"any\"}"))
    this->throw_error("Invalid schema");  // GCOV_EXCL_LINE
}

YggGenericInput::YggGenericInput(const std::string name,
                                 FLAG_TYPE flags,
                                 const COMM_TYPE commtype) :
  YggInput(name, flags, commtype) {
  if (!this->addSchema("{\"type\": \"any\"}"))
    this->throw_error("Invalid schema");  // GCOV_EXCL_LINE
}

YggAnyOutput::YggAnyOutput(const std::string name,
                           FLAG_TYPE flags,
                           const COMM_TYPE commtype) :
  YggOutput(name, flags, commtype) {
  if (!this->addSchema("{\"type\": \"any\"}"))
    this->throw_error("Invalid schema");  // GCOV_EXCL_LINE
}

YggAnyInput::YggAnyInput(const std::string name,
                         FLAG_TYPE flags,
                         const COMM_TYPE commtype) :
  YggInput(name, flags, commtype) {
  if (!this->addSchema("{\"type\": \"any\"}"))
    this->throw_error("Invalid schema");  // GCOV_EXCL_LINE
}

YggJSONArrayOutput::YggJSONArrayOutput(const std::string name,
                                       FLAG_TYPE flags,
                                       const COMM_TYPE commtype) :
  YggOutput(name, flags, commtype) {
  if (!this->addSchema("{\"type\": \"array\"}"))
    this->throw_error("Invalid schema");  // GCOV_EXCL_LINE
}

YggJSONArrayInput::YggJSONArrayInput(const std::string name,
                                     FLAG_TYPE flags,
                                     const COMM_TYPE commtype) :
  YggInput(name, flags, commtype) {
  if (!this->addSchema("{\"type\": \"array\"}"))
    this->throw_error("Invalid schema");  // GCOV_EXCL_LINE
}

YggJSONObjectOutput::YggJSONObjectOutput(const std::string name,
                                         FLAG_TYPE flags,
                                         const COMM_TYPE commtype) :
  YggOutput(name, flags, commtype) {
  if (!this->addSchema("{\"type\": \"object\"}"))
    this->throw_error("Invalid schema");  // GCOV_EXCL_LINE
}

YggJSONObjectInput::YggJSONObjectInput(const std::string name,
                                       FLAG_TYPE flags,
                                       const COMM_TYPE commtype) :
  YggInput(name, flags, commtype) {
  if (!this->addSchema("{\"type\": \"object\"}"))
    this->throw_error("Invalid schema");  // GCOV_EXCL_LINE
}
