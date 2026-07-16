#include "YggInterface.hpp"

using namespace YggInterface;
using namespace YggInterface::communicator;

YggInput::YggInput(const std::string nme, FLAG_TYPE flags,
                   const COMM_TYPE commtype) :
  WrapComm(std::move(nme), utils::blankAddress,
           RECV, flags | COMM_FLAG_INTERFACE, commtype) {
  std::cerr << "YggInput: " << nme << " -> " << this->name << std::endl;
}


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
