#pragma once

#include "CommBase.hpp"
#include "utils/serialization.hpp"
#ifdef THREADSINSTALLED
#include <atomic>
#include <condition_variable>
#endif // THREADSINSTALLED

namespace communication {
  namespace communicator {

    class AsyncBacklog;

    class AsyncBuffer : public communication::utils::LogBase {
    public:
      AsyncBuffer(const std::string logInst);
      std::string logClass() const override { return "AsyncBuffer"; }
      std::string logInst() const override { return logInst_; }
      void close();
      bool is_closed() const;
      size_t size();
      bool insert(utils::Header& header, size_t idx, bool move=false,
		  bool dont_notify=false, bool for_send=false);
      bool append(utils::Header& header, bool move=false,
		  bool dont_notify=false, bool for_send=false);
      bool prepend(utils::Header& header, bool move=false,
		   bool dont_notify=false, bool for_send=false);
      bool get(utils::Header& header, size_t idx=0,
	       bool move=false, bool erase=false, bool dont_notify=false);
      bool pop(utils::Header& header, size_t idx=0,
	       bool dont_notify=false);
      void notify() { cv.notify_all(); }
#ifdef THREADSINSTALLED
      bool message_waiting(const std::string id="",
			   const bool negative=false);
      bool wait(const std::string id="", const bool negative=false);
      template< class Rep, class Period >
      bool wait_for(const std::chrono::duration<Rep, Period>& rel_time,
		    const std::string id="", const bool negative=false) {
	std::unique_lock<std::mutex> lk(m);
	if (message_waiting(id, negative))
	  return true;
	return cv.wait_for(lk, rel_time, [this, id, negative]{
	  return message_waiting(id, negative); });
      }
#endif // THREADSINSTALLED
    private:
      std::vector<utils::Header> buffer;
#ifdef THREADSINSTALLED
      std::atomic_bool closed;
      std::mutex m;
      std::condition_variable cv;
#endif // THREADSINSTALLED
      std::string logInst_;
    };

    class AsyncBacklog : public communication::utils::LogBase {
    private:
      AsyncBacklog(const AsyncBacklog&) = delete;
      AsyncBacklog& operator=(const AsyncBacklog&) = delete;
    public:
      AsyncBacklog(Comm_t* parent);
      ~AsyncBacklog();
      void on_thread(Comm_t* parent);
      int signon_status();
      bool wait_for_signon();
      int send();
      long recv();
      std::string logClass() const override { return "AsyncBacklog"; }
      std::string logInst() const override { return logInst_; }
      bool is_closing() const { return backlog.is_closed(); }
#ifdef THREADSINSTALLED
      void set_status(const int new_status, bool dont_notify=false,
		      bool negative=false);
      void set_status_lock(const int new_status, bool dont_notify=false,
			   bool negative=false);
      bool _wait_status(const int new_status,
			std::unique_lock<std::mutex>& lk);
      bool wait_status(const int new_status);
      template< class Rep, class Period >
      bool wait_for_status(const std::chrono::duration<Rep, Period>& rel_time,
			   const int new_status) {
	if (status.load() & new_status)
	  return true;
	std::unique_lock<std::mutex> lk(comm_mutex);
	AsyncBacklog* _this = this;
	return cv_status.wait_for(lk, rel_time, [_this, new_status]{
	  return (_this->status.load() & new_status); });
      }
#endif // THREADSINSTALLED
      Comm_t* comm;
      AsyncBuffer backlog;
#ifdef THREADSINSTALLED
      std::mutex comm_mutex;
      std::atomic_bool locked;
      std::unique_ptr<std::thread> backlog_thread;
      std::atomic_int status;
      std::condition_variable cv_status;
#endif // THREADSINSTALLED
      std::string logInst_;
    };
    
    class AsyncLockGuard {
    private:
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
    class YGG_API AsyncComm : public CommBase<AsyncBacklog> {
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
      ADD_DESTRUCTOR(AsyncComm, CommBase)

      // \copydoc Comm_t::defaultCommType
      static COMM_TYPE defaultCommType() { return DEFAULT_COMM; }
      
      // \copydoc Comm_t::comm_nmsg
      int comm_nmsg(DIRECTION dir=NONE) const override;
#ifdef THREADSINSTALLED
      // \copydoc Comm_t::wait_for_recv
      int wait_for_recv(const int64_t& tout) override;
#endif // THREADSINSTALLED
      // \copydoc Comm_t::getMetadata
      communication::utils::Metadata& getMetadata(const DIRECTION dir=NONE) override;
      // \copydoc Comm_t::set_timeout_recv
      void set_timeout_recv(int64_t new_timeout) override;
      // \copydoc Comm_t::get_timeout_recv
      int64_t get_timeout_recv() override;
      // \copydoc Comm_t::logClass
      std::string logClass() const override;
      
      using Comm_t::send;
      using Comm_t::recv;

    protected:
      int send_single(utils::Header& header) override;
      long recv_single(utils::Header& header) override;
      bool create_header_send(utils::Header& header) override;
      Comm_t* create_worker(utils::Address* address,
			    const DIRECTION& dir, int flgs) override;
      
      utils::Metadata response_metadata;

    public:
      // RPC methods
      bool addResponseSchema(const std::string& s,
			     bool use_generic=false);
      bool addResponseSchema(const rapidjson::Value& s,
			     bool use_generic=false);
      bool addResponseSchema(const utils::Metadata& metadata,
			     bool use_generic=false);
      bool addResponseFormat(const std::string& fmt,
			     bool use_generic=false);
      
    };

  }
} // communication
