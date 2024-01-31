#pragma once

#include "communicators/DefaultComm.hpp"
#include "communicators/CommBase.hpp"
#include "communicators/Requests.hpp"
#include "communicators/WrapComm.hpp"

namespace YggInterface {
namespace communicator {

/**
 * @brief Structure for storing requests
 */
class RPCComm : public WrapComm {
public:
  /*!
   * @brief Constructor
   * @param[in] name The name of the communicator
   * @param[in] address The address to use
   * @param[in] flgs Flags to use
   * @param[in] dir The communication direction
   * @param[in] req_dir The request direction
   * @param[in] type The communicator Type
   * @param[in] ncomm Number of communicators in a forked request comm.
   * @param[in] reqtype Communicator type to use for the request communicator.
   * @param[in] restype Communicator type to use for the response communicator.
   * @param[in] reqflags Bitwise flags describing the request communicator.
   * @param[in] resflags Bitwise flags describing the response communicator.
   * @see utils::Address
   */
  YGG_API explicit RPCComm(const std::string &name,
			   const utils::Address& address,
			   int flgs, DIRECTION dir, DIRECTION req_dir,
			   const COMM_TYPE type, size_t ncomm,
			   const COMM_TYPE reqtype,
			   const COMM_TYPE restype,
			   int reqflags, int resflags);
  /*!
   * @brief Constructor
   * @param[in] name The name of the communicator
   * @param[in] flgs Flags to use
   * @param[in] dir The communication direction
   * @param[in] req_dir The request direction
   * @param[in] type The communicator Type
   * @param[in] ncomm Number of communicators in a forked request comm.
   * @param[in] reqtype Communicator type to use for the request communicator.
   * @param[in] restype Communicator type to use for the response communicator.
   * @param[in] reqflags Bitwise flags describing the request communicator.
   * @param[in] resflags Bitwise flags describing the response communicator.
   */
  YGG_API RPCComm(const std::string& name, int flgs, DIRECTION dir,
		  DIRECTION req_dir, const COMM_TYPE type, size_t ncomm,
		  const COMM_TYPE reqtype, const COMM_TYPE restype,
		  int reqflags, int resflags);
  ADD_DESTRUCTOR(RPCComm, WrapComm)

  using Comm_t::send;
  using Comm_t::recv;
  /** \copydoc YggInterface::communicator::Comm_t::comm_nmsg */
  YGG_API int comm_nmsg(DIRECTION dir=NONE) const override;
  /** \copydoc YggInterface::communicator::Comm_t::wait_for_recv */
  YGG_API int wait_for_recv(const int64_t& tout) override;

  /**
   * @brief Add a schema to the response communicator(s).
   * @param[in] s JSON serialized schema.
   * @param[in] use_generic If true, set schema to expect generic
   *   JSON objects.
   * @return true if successful, false otherwise.
   */
  YGG_API bool addResponseSchema(const std::string& s,
				 bool use_generic=false);
  /**
   * @brief Add a schema to the response communicator(s).
   * @param[in] s JSON schema.
   * @param[in] use_generic If true, set schema to expect generic
   *   JSON objects.
   * @return true if successful, false otherwise.
   */
  YGG_API bool addResponseSchema(const rapidjson::Value& s,
				 bool use_generic=false);
  /**
   * @brief Add a schema to the response communicator(s).
   * @param[in] metadata Metadata to copy containing JSON schema.
   * @param[in] use_generic If true, set schema to expect generic
   *   JSON objects.
   * @return true if successful, false otherwise.
   */
  YGG_API bool addResponseSchema(const utils::Metadata& metadata,
				 bool use_generic=false);
  /**
   * @brief Add a schema to the response communicator based on a
   *   C-style format string.
   * @param[in] fmt C-style format string.
   * @param[in] use_generic If true, set schema to expect generic
   *   JSON objects.
   * @return true if successful, false otherwise.
   */
  YGG_API bool addResponseFormat(const std::string& fmt,
				 bool use_generic=false);
  /** \copydoc YggInterface::communicator::Comm_t::getMetadata */
  YGG_API YggInterface::utils::Metadata& getMetadata(const DIRECTION dir=NONE) override;
  /**
   * @brief Determine if the client signon has completed.
   * @return true if complete, false otherwise.
   */
  bool signonComplete() const { return requests.signon_complete; }
  /** \copydoc YggInterface::communicator::Comm_t::isInstalled */
  static bool isInstalled() { return WrapComm::isInstalled(); }
  /**
   * @brief Get the requests from the global communicator
   * @return The requests
   */
  RequestList& getRequests() {
    if (global_comm)
      return (dynamic_cast<RPCComm*>(global_comm))->getRequests();
    return requests;
  }

protected:
  
  RequestList requests;     //!< List of requests
  
  // Test methods
public:
  /*! \copydoc YggInterface::communicator::Comm_t::afterSendRecv */
  YGG_API bool afterSendRecv(Comm_t* sComm, Comm_t* rComm) override;
  
};
  
}
} // YggInterface
