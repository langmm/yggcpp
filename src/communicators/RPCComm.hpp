#pragma once

#include "DefaultComm.hpp"
#include "CommBase.hpp"
#include "Requests.hpp"

#ifdef COMM_BASE
namespace YggInterface {
namespace communicator {

/**
 * @brief Structure for storing requests
 */
class RPCComm : public COMM_BASE {
public:
    /*!
     * @brief Constructor
     * @param[in] name The name of the communicator
     * @param[in] address The address to use
     * @param[in] flgs Flags to use
     * @param[in] dir The communication direction
     * @param[in] req_dir The request direction
     * @param[in] type The communicator Type
     * @see utils::Address
     */
  explicit RPCComm(const std::string &name, utils::Address& address,
		   int flgs, DIRECTION dir, DIRECTION req_dir,
		   const COMM_TYPE type);
  /*!
   * @brief Constructor
   * @param[in] name The name of the communicator
   * @param[in] flgs Flags to use
   * @param[in] dir The communications direction
   * @param[in] req_dir The request direction
   * @param[in] type The communicator Type
   */
    RPCComm(const std::string& name, int flgs, DIRECTION dir,
            DIRECTION req_dir, const COMM_TYPE type);

    using Comm_t::send;
    using Comm_t::recv;
    /*! \copydoc YggInterface::communicator::Comm_t::comm_nmsg */
    int comm_nmsg(DIRECTION dir=NONE) const override;

    /*! \copydoc YggInterface::communicator::Comm_t::close */
    void close() override;
    /**
     * @brief Add the given schema to responses
     * @param[in] s The schema to use
     * @param[in] use_generic If true then initialize with generaic values first
     */
    void addResponseSchema(const std::string& s, bool use_generic=false);
    /**
     * @brief Add the given schema to responses
     * @param[in] s The schema to use
     * @param[in] use_generic If true then initialize with generaic values first
     */
    void addResponseSchema(const rapidjson::Value& s,
                           bool use_generic=false);
    /**
     * @brief Add the given schema to responses
     * @param[in] metadata The metadata containing the schema
     * @param[in] use_generic If true then initialize with generaic values first
     */
    void addResponseSchema(const utils::Metadata& metadata,
                           bool use_generic=false);
    /**
     * @brief Add the given format to the response
     * @param[in] fmt The format string to use
     * @param[in] use_generic If true then initialize with generic values first
     */
    void addResponseFormat(const std::string& fmt, bool use_generic=false);
    /** \copydoc Comm_t::getMetadata */
    YggInterface::utils::Metadata& getMetadata(const DIRECTION dir) override;
    bool signonComplete() const { return requests.signon_complete; }
    /**
     * @brief Determine if the default communicator has been installed
     * @return
     */
    static bool isInstalled() { return COMM_BASE::isInstalled(); }

#ifndef YGG_TEST
protected:
#else
    /*! \copydoc YggInterface::communicator::Comm_t::afterSendRecv */
    bool afterSendRecv(Comm_t* sComm, Comm_t* rComm) override;
    /**
     * @brief Get the requests from the global communicator
     * @return The requests
     */
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
  
  RequestList requests;    //!< List of requests
};
  
}
} // YggInterface

#endif
