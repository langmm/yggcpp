#pragma once

#include "CommBase.hpp"
#include "utils/serialization.hpp"
#include <atomic>

namespace YggInterface {
  namespace communicator {

    class AsyncBacklog {
    public:
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
      std::atomic_bool locked;
      std::vector<utils::Header> backlog;
      std::thread backlog_thread;
    };
    
    class AsyncLockGuard {
    public:
        AsyncLockGuard(const AsyncLockGuard&) = delete;
        AsyncLockGuard& operator=(const AsyncLockGuard&) = delete;
    public:
      AsyncLockGuard(AsyncBacklog* backlog, bool dont_lock=false);
      ~AsyncLockGuard();
      bool locked;
      AsyncBacklog* backlog;
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
      explicit AsyncComm(const std::string name,
			 utils::Address& address,
			 const DIRECTION direction = NONE, int flgs = 0,
			 const COMM_TYPE type = DEFAULT_COMM);
      explicit AsyncComm(const std::string nme,
			 const DIRECTION dirn, int flgs = 0,
			 const COMM_TYPE type = DEFAULT_COMM);
      explicit AsyncComm(utils::Address &addr,
			 const DIRECTION dirn, int flgs = 0,
			 const COMM_TYPE type = DEFAULT_COMM);

      // \copydoc Comm_t::defaultCommType
      static COMM_TYPE defaultCommType() { return DEFAULT_COMM; }
      
      // \copydoc Comm_t::comm_nmsg
      int comm_nmsg(DIRECTION dir=NONE) const override;
      // \copydoc Comm_t::getMetadata
      YggInterface::utils::Metadata& getMetadata(const DIRECTION dir=NONE) override;
      // \copydoc Comm_t::set_timeout_recv
      void set_timeout_recv(int new_timeout) override;
      // \copydoc Comm_t::get_timeout_recv
      int get_timeout_recv() override;
      
      using Comm_t::send;
      using Comm_t::recv;
      
    protected:
      int send_single(utils::Header& header) override;
      long recv_single(utils::Header& header) override;
      bool create_header_send(utils::Header& header) override;
      Comm_t* create_worker(utils::Address& address,
			    const DIRECTION& dir, int flgs) override;
      
    };

  }
} // YggInterface
