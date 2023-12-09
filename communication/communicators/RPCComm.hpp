#pragma once

#include "DefaultComm.hpp"
#include "CommBase.hpp"
#include "Requests.hpp"
#include "WrapComm.hpp"

namespace communication {
namespace communicator {

// @brief Structure for storing requests
class YGG_API RPCComm : public WrapComm {
public:
  explicit RPCComm(const std::string &name,
		   const utils::Address& address,
		   int flgs, DIRECTION dir, DIRECTION req_dir,
		   const COMM_TYPE type, size_t ncomm,
		   const COMM_TYPE reqtype, const COMM_TYPE restype,
		   int reqflags, int resflags);
  RPCComm(const std::string& name, int flgs, DIRECTION dir,
	  DIRECTION req_dir, const COMM_TYPE type, size_t ncomm,
	  const COMM_TYPE reqtype, const COMM_TYPE restype,
	  int reqflags, int resflags);
  ADD_DESTRUCTOR(RPCComm, WrapComm)

  using Comm_t::send;
  using Comm_t::recv;
  /*! \copydoc Comm_t::comm_nmsg */
  int comm_nmsg(DIRECTION dir=NONE) const override;
  /*! \copydoc Comm_t::wait_for_recv */
  int wait_for_recv(const int64_t& tout) override;

  bool addResponseSchema(const std::string& s, bool use_generic=false);
  bool addResponseSchema(const rapidjson::Value& s,
			 bool use_generic=false);
  bool addResponseSchema(const utils::Metadata& metadata,
			 bool use_generic=false);
  bool addResponseFormat(const std::string& fmt, bool use_generic=false);
  // \copydoc Comm_t::getMetadata
  communication::utils::Metadata& getMetadata(const DIRECTION dir=NONE) override;
  bool signonComplete() const { return requests.signon_complete; }
  static bool isInstalled() { return WrapComm::isInstalled(); }
  RequestList& getRequests() {
    if (global_comm)
      return (dynamic_cast<RPCComm*>(global_comm))->getRequests();
    return requests;
  }

protected:
  
  RequestList requests;
  
  // Test methods
public:
  bool afterSendRecv(Comm_t* sComm, Comm_t* rComm) override;
  
};
  
}
} // communication
