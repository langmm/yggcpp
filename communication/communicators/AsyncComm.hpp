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

    class AsyncStatus : public communication::utils::LogBase {
    private:
      AsyncStatus(const AsyncStatus&) = delete;
      AsyncStatus& operator=(const AsyncStatus&) = delete;
    public:
      AsyncStatus(const std::string& logInst = "");
#ifdef THREADSINSTALLED
      template<typename... T>
      void start(T&&... t) {
	std::unique_lock<std::mutex> lk(mutex);
	thread = std::unique_ptr<std::thread>(new std::thread(std::forward<T>(t)...));
	log_debug() << "start: waiting for thread to start" << std::endl;
	_wait_status(THREAD_STARTED | THREAD_COMPLETE, lk);
	log_debug() << "start: thread started" << std::endl;
	// set_status_lock(THREAD_INIT);
      }
      void stop();
#endif // THREADSINSTALLED
      std::string logClass() const override { return "AsyncStatus"; }
      std::string logInst() const override { return logInst_; }
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
	std::unique_lock<std::mutex> lk(mutex);
	return cv_status.wait_for(lk, rel_time, [this, new_status]{
	  return (this->status.load() & new_status); });
      }
      std::mutex mutex;
      std::atomic_bool locked;
      std::atomic_int status;
      std::condition_variable cv_status;
      std::unique_ptr<std::thread> thread;
#endif // THREADSINSTALLED
      std::string logInst_;
    };

    class AsyncBacklog : public AsyncStatus {
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
      bool is_closing() const { return backlog.is_closed(); }
      Comm_t* comm;
      AsyncBuffer backlog;
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
      explicit AsyncComm(const std::string name,
			 const utils::Address& address,
			 const DIRECTION direction = NONE, int flgs = 0,
			 const COMM_TYPE type = DEFAULT_COMM);
      ADD_CONSTRUCTORS_BASE_NOLOG(AsyncComm, DEFAULT_COMM, true)

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
      Comm_t* create_worker(utils::Address& address,
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
