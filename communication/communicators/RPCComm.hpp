#pragma once

#include "DefaultComm.hpp"
#include "CommBase.hpp"
#include "Requests.hpp"

#ifdef COMM_BASE
namespace communication {
namespace communicator {

// @brief Structure for storing requests
class RPCComm : public COMM_BASE {
public:
  explicit RPCComm(const std::string &name, utils::Address *address,
		   int flgs, DIRECTION dir, DIRECTION req_dir,
		   const COMM_TYPE type);

  using Comm_t::send;
  using Comm_t::recv;
  /*! \copydoc Comm_t::comm_nmsg */
  int comm_nmsg(DIRECTION dir=NONE) const override;

  void close() override;
  
  bool addResponseSchema(const std::string& s, bool use_generic=false);
  bool addResponseSchema(const rapidjson::Value& s,
			 bool use_generic=false);
  bool addResponseSchema(const utils::Metadata& metadata,
			 bool use_generic=false);
  bool addResponseFormat(const std::string& fmt, bool use_generic=false);
  // \copydoc Comm_t::getMetadata
  communication::utils::Metadata& getMetadata(const DIRECTION dir=NONE) override;
  bool signonComplete() const { return requests.signon_complete; }
  static bool isInstalled() { return COMM_BASE::isInstalled(); }
  
#ifndef YGG_TEST
protected:
#else
  bool afterSendRecv(Comm_t* sComm, Comm_t* rComm) override;
  RequestList& getRequests() {
    if (global_comm)
      return (dynamic_cast<RPCComm*>(global_comm))->getRequests();
    return requests;
  }
  // std::string getResponseAddress() {
  //   if (global_comm)
  //     return (dynamic_cast<RPCComm*>(global_comm))->getResponseAddress();
  //   return requests.lastComm()->getAddress();
  // }
#endif
  
  RequestList requests;
};
  
}
} // communication

#endif
