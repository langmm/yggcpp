#pragma once
#include "communicators/CommBase.hpp"

#ifdef RESTINSTALLED
#include <curl/curl.h>
#endif // RESTINSTALLED

namespace YggInterface {
  namespace communicator {

    /**
     * @brief Wrapper for curl connection to REST API
     */
    class RESTConnection : public YggInterface::utils::LogBase {
      RESTConnection(const RESTConnection&) = delete;
      RESTConnection& operator=(const RESTConnection&) = delete;
    public:
      /**
       * Constructor
       * @param[in] logInst String that should be used in log messages
       *   identifying the instance
       * @param[in] dir Enumerated direction of the connection
       * @param[in] name Name of the conneciton
       * @param[in] address Address for the connection
       * @param[in] model Name of the model to use in generating a new
       *   address if not provided.
       */
      RESTConnection(const std::string logInst, DIRECTION dir,
		     const std::string& name="",
		     const std::string& address="",
		     const std::string& model="");
      /** \copydoc YggInterface::utils::LogBase::logClass */
      std::string logClass() const override { return "RESTConnection"; }
      /** \copydoc YggInterface::utils::LogBase::logInst */
      std::string logInst() const override { return logInst_; }
      /** @brief Destructor */
      ~RESTConnection();
      /**
       * @brief Initialize the connection
       * @return -1 if there is an error
       */
      int init();
      /**
       * @brief Close the connection
       * @return -1 if there is an error
       */
      int close();
      /** \copydoc YggInterface::communicator::Comm_t::nmsg */
      int nmsg(DIRECTION dir) const;
      /**
       * @brief Send a message through the connection.
       * @param[in] header Header containing the message to send.
       * @return Result of send. Negative values indicate errors
       */
      int send(utils::Header& header);
      /**
       * @brief Receive a message from the connection
       * @param[out] header Header that message should be stored in
       * @return Result of receive. Negative values indicate errors
       */
      long recv(utils::Header& header);
      /**
       * @brief Create the connection address from its components in
       *   url, host, user, password, port, vhost, exchange, and
       *   queue_name
       */
      std::string logInst_;         /**< Identifier to use for instance in log messages */
      std::string name;             /**< Connection name */
      std::string address;          /**< Connection address */
      DIRECTION direction;          /**< Connection direction */
      std::string model;            /**< Model to use in connection address */
#ifdef RESTINSTALLED
      CURL *curl;                   /**< Curl conneciton */
    private:
      /**
       * @brief Log an error based on the provided curl code
       * @param[in] x Curl result code
       * @return true if not an error, false otherwise.
       */
      bool _check_curl_error(CURLcode x, const std::string& context) const;
#endif // RESTINSTALLED
    };
    
    /**
     * @brief REST API based communicator
     */
    class RESTComm : public CommBase<RESTConnection> {
    public:
      COMM_CONSTRUCTOR_CORE_DEC(RESTComm, REST_COMM, REST_INSTALLED_FLAG)
	
      /** \copydoc Comm_t::nmsg */
      YGG_API int nmsg(DIRECTION dir=NONE) const override;
      
    protected:
      /** \copydoc Comm_t::send_single */
      YGG_API int send_single(utils::Header& header) override;

      /** \copydoc Comm_t::recv_single */
      YGG_API long recv_single(utils::Header& header) override;

      WORKER_METHOD_DECS(RESTComm);
      
    };
    
  }
}
