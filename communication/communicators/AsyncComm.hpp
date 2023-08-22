#pragma once

#include "CommBase.hpp"
#include "utils/Message.hpp"
#include <atomic>

namespace communication {
  namespace communicator {

    class AsyncBacklog {
    private:
      AsyncBacklog(const AsyncBacklog&) = delete;
      AsyncBacklog& operator=(const AsyncBacklog&) = delete;
    public:
      AsyncBacklog(Comm_t* parent);
      ~AsyncBacklog();
      bool on_thread(Comm_t* parent);
      int send();
      long recv();
      Comm_t* comm;
      std::mutex comm_mutex;
      std::atomic_bool opened;
      std::atomic_bool closing;
      std::vector<utils::Message> backlog;
      std::thread backlog_thread;
    };

    /**
     * Asynchonous communication class.
     **/
    class AsyncComm : public CommBase<AsyncBacklog> {
    public:
      /**
       * Constructor
       * @param name The name of the communicator
       * @param address The address to associate with the communicator,
       *   if the address is nullptr, then an address will be created.
       * @param direction Enumerated direction for communicator
       * @param flgs Bitwise flags describing the communicator
       * @param type Enumerated communicator type
       **/
      explicit AsyncComm(const std::string name = "",
			 utils::Address *address = new utils::Address(),
			 const DIRECTION direction = NONE, int flgs = 0,
			 const COMM_TYPE type = DEFAULT_COMM);
      explicit AsyncComm(const std::string nme,
			 const DIRECTION dirn, int flgs = 0,
			 const COMM_TYPE type = DEFAULT_COMM);
      explicit AsyncComm(utils::Address *addr,
			 const DIRECTION dirn, int flgs = 0,
			 const COMM_TYPE type = DEFAULT_COMM);

      // \copydoc Comm_t::defaultCommType
      static COMM_TYPE defaultCommType() { return DEFAULT_COMM; }
      
      // \copydoc Comm_t::comm_nmsg
      int comm_nmsg() const override;
      
      // \copydoc Comm_t::getMetadata
      utils::Metadata& getMetadata(const DIRECTION dir=NONE) override;
      
      using Comm_t::send;
      using Comm_t::recv;
      
    protected:
      int send_single(const char *data, const size_t &len,
		      const utils::Header& header) override;
      long recv_single(char*& data, const size_t &len,
		       bool allow_realloc) override;
      bool create_header_send(utils::Header& header, const char* data,
			      const size_t &len) override;
      bool create_header_recv(utils::Header& header, char*& data,
			      const size_t &len, size_t msg_len,
			      int allow_realloc, int temp) override;
      Comm_t* create_worker(utils::Address* address,
			    const DIRECTION& dir, int flgs) override;
      
    };

  }
} // communication
