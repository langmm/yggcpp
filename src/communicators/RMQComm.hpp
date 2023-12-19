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

    class RMQConnection : public YggInterface::utils::LogBase {
      RMQConnection(const RMQConnection&) = delete;
      RMQConnection& operator=(const RMQConnection&) = delete;
    public:
      RMQConnection(const std::string logInst, DIRECTION dir,
		    const std::string& address="");
      std::string logClass() const override { return "RMQConnection"; }
      std::string logInst() const override { return logInst_; }
      ~RMQConnection();
      int init();
      int close();
      int nmsg(DIRECTION dir) const;
      int send(utils::Header& header);
      long recv(utils::Header& header);
      void _format_address();
#ifdef RMQINSTALLED
      bool _check_amqp_error(int x, const std::string& context) const;
      bool _check_amqp_reply_error(amqp_rpc_reply_t x,
				   const std::string& context) const;
#endif // RMQINSTALLED
      std::string logInst_;
      std::string address;
      DIRECTION direction;
#ifdef RMQINSTALLED
      amqp_connection_state_t conn;
      amqp_socket_t* socket;
      amqp_channel_t channel;
#endif // RMQINSTALLED
      std::string url;
      std::string host;
      std::string user;
      std::string password;
      int port;
      std::string vhost;
      std::string exchange;
      std::string queue_name;
    };

    class YGG_API RMQComm : public CommBase<RMQConnection> {
    public:

      explicit RMQComm(const std::string name,
		       const utils::Address& address,
		       const DIRECTION direction=NONE, int flgs=0,
		       const COMM_TYPE commtype=RMQ_COMM);
      ADD_CONSTRUCTORS(RMQ)
	
      // \copydoc Comm_t::comm_nmsg
      int comm_nmsg(DIRECTION dir=NONE) const override;
      
      /*! \copydoc Comm_t::send_single */
      int send_single(utils::Header& header) override;

      /*! \copydoc Comm_t::recv_single */
      long recv_single(utils::Header& header) override;

      WORKER_METHOD_DECS(RMQComm);
      
      using Comm_t::send;
      using Comm_t::recv;

    protected:
      void init();

    };
    
  }
}
