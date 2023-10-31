#pragma once

#include "CommBase.hpp"
#include "utils/serialization.hpp"
#ifdef THREADSINSTALLED
#include <atomic>
#include <condition_variable>
#define LOCK_BUFFER(name)					\
  log_verbose() << #name << ": Before lock" << std::endl;	\
  const std::lock_guard<std::mutex> lk(m);			\
  log_verbose() << #name << ": After lock" << std::endl
#else
#define LOCK_BUFFER(name)
#endif // THREADSINSTALLED

namespace communication {
  namespace communicator {

    class AsyncBuffer : public communication::utils::LogBase {
    public:
      AsyncBuffer(const std::string logInst);
      std::string logClass() const override { return "AsyncBuffer"; }
      std::string logInst() const override { return logInst_; }
      void close();
      bool is_closed() const;
      size_t size();
      bool append(utils::Header& header, bool for_send=false);
      bool get(utils::Header& header);
      bool pop();
      bool pop(utils::Header& header);
#ifdef THREADSINSTALLED
      bool wait() {
	std::unique_lock<std::mutex> lk(m);
	AsyncBuffer* this_buffer = this;
	cv.wait(lk,
		[this_buffer]{ return (this_buffer->is_closed() ||
				       this_buffer->buffer.size() > 0); });
	return (is_closed() || buffer.size() > 0);
      }
      template< class Rep, class Period >
      bool wait_for(const std::chrono::duration<Rep, Period>& rel_time) {
	std::unique_lock<std::mutex> lk(m);
	AsyncBuffer* this_buffer = this;
	return cv.wait_for(lk, rel_time,
			   [this_buffer]{ return (this_buffer->is_closed() ||
						  this_buffer->buffer.size() > 0); });
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
      int send();
      long recv();
      std::string logClass() const override { return "AsyncBacklog"; }
      std::string logInst() const override { return logInst_; }
      bool is_closing() const { return backlog.is_closed(); }
      Comm_t* comm;
      AsyncBuffer backlog;
#ifdef THREADSINSTALLED
      std::mutex comm_mutex;
      std::atomic_bool opened;
      std::atomic_bool locked;
      std::atomic_bool complete;
      std::atomic_bool result;
      std::thread backlog_thread;
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
      
    };

  }
} // communication
