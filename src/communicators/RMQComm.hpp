#pragma once
#ifdef RMQINSTALLED

#ifdef YGG_RMQ_NOINCLUDEDIR
#include <amqp.h>
#include <amqp_tcp_socket.h>
#else // YGG_RMQ_NOINCLUDEDIR
#include <rabbitmq-c/amqp.h>
#include <rabbitmq-c/tcp_socket.h>
#endif // YGG_RMQ_NOINCLUDEDIR
#endif // RMQINSTALLED
#include "CommBase.hpp"

#define _RMQ_PARAM_SEP "_RMQPARAM_"

namespace YggInterface {
  namespace communicator {

    /**
     * @brief RabbitMQ connection manager
     */
    class RMQConnection : public YggInterface::utils::LogBase {
      RMQConnection(const RMQConnection&) = delete;
      RMQConnection& operator=(const RMQConnection&) = delete;
    public:
      /**
       * @brief Constructor
       * @param[in] logInst String that should be used in log messages
       *   identifying the instance
       * @param[in] dir Enumerated direction of the connection
       * @param[in] address Address for the connection
       */
      RMQConnection(const std::string logInst, DIRECTION dir,
		    const std::string& address="");
      /** \copydoc YggInterface::utils::LogBase::logClass */
      std::string logClass() const override { return "RMQConnection"; }
      /** \copydoc YggInterface::utils::LogBase::logInst */
      std::string logInst() const override { return logInst_; }
      /** @brief Destructor */
      ~RMQConnection();
      /**
       * @brief Initialize the connection
       */
      int init();
      /**
       * @brief Close the connection
       */
      int close();
      /** \copydoc YggInterface::communicator::Comm_t::comm_nmsg */
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
      void _format_address();
#ifdef RMQINSTALLED
      /**
       * @brief Process an amqp error
       * @param[in] x Error code
       * @param[in] context String for log message describing where the
       *   error initiated
       * @return true if successful, false otherwise
       */
      bool _check_amqp_error(int x, const std::string& context) const;
      /**
       * @brief Process an amqp error in a reply
       * @param[in] x Reply
       * @param[in] context String for log message describing where the
       *   error initiated
       * @return true if successful, false otherwise
       */
      bool _check_amqp_reply_error(amqp_rpc_reply_t x,
				   const std::string& context) const;
#endif // RMQINSTALLED
      std::string logInst_;         /**< Identifier to use for instance in log messages */
      std::string address;          /**< Connection address */
      DIRECTION direction;          /**< Connection direction */
#ifdef RMQINSTALLED
      amqp_connection_state_t conn; /**< Conneciton */
      amqp_socket_t* socket;        /**< Socket */
      amqp_channel_t channel;       /**< Channel */
#endif // RMQINSTALLED
      std::string url;              /**< Connection URL */
      std::string host;             /**< Connection host */
      std::string user;             /**< Connection user */
      std::string password;         /**< Connection password */
      int port;                     /**< Connection port */
      std::string vhost;            /**< Connection virtual host */
      std::string exchange;         /**< Connection exchange */
      std::string queue_name;       /**< Connection queue name */
    };

    /**
     * @brief RabbitMQ based communicator
     */
    class YGG_API RMQComm : public CommBase<RMQConnection> {
    public:

      /**
       * Constructor
       * @param[in] name The name for the communicator, if empty one will be generated
       * @param[in] address The address for the communicator, if empty one will be generated
       * @param[in] direction Enuerated direction for this instance
       * @param[in] flgs Bitwise flags describing the communicator
       * @param[in] commtype The enumerated type of communicator to create
       */
      explicit RMQComm(const std::string name,
		       const utils::Address& address,
		       const DIRECTION direction=NONE, int flgs=0,
		       const COMM_TYPE commtype=RMQ_COMM);
      ADD_CONSTRUCTORS(RMQ)
	
      /** \copydoc Comm_t::comm_nmsg */
      int comm_nmsg(DIRECTION dir=NONE) const override;
      
      /** \copydoc Comm_t::send_single */
      int send_single(utils::Header& header) override;

      /** \copydoc Comm_t::recv_single */
      long recv_single(utils::Header& header) override;

      WORKER_METHOD_DECS(RMQComm);
      
      using Comm_t::send;
      using Comm_t::recv;

    protected:
      /** \copydoc Comm_t::init */
      void init();

    };
    
  }
}
