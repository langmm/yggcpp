#pragma once

#include "communicators/CommBase.hpp"
#include "utils/serialization.hpp"
#ifdef THREADSINSTALLED
#include <atomic>
#include <condition_variable>
#endif // THREADSINSTALLED

namespace YggInterface {
  namespace communicator {

    class AsyncBacklog;

    /**
     * @brief Buffer for storying async messages across threads.
     */
    class AsyncBuffer : public YggInterface::utils::LogBase {
    public:
      /**
       * @brief Constructor
       * @param[in] logInst String that should be used in log messages
       *   identifying the instance
       */
      AsyncBuffer(const std::string logInst);
      /** \copydoc YggInterface::utils::LogBase::logClass */
      std::string logClass() const override { return "AsyncBuffer"; }
      /** \copydoc YggInterface::utils::LogBase::logInst */
      std::string logInst() const override { return logInst_; }
      /**
       * @brief Close the buffer.
       */
      void close();
      /**
       * @brief Determine if the buffer is closing.
       * @return true if closing, false otherwise.
       */
      bool is_closed() const;
      /**
       * @brief Get the number of messages in the buffer.
       * @return Number of messages.
       */
      size_t size();
      /**
       * @brief Insert a message into the buffer at the specified index.
       * @param[in] header Message with header data.
       * @param[in] idx Index to insert message at.
       * @param[in] move If true, the message will be moved when added.
       * @param[in] dont_notify If true, threads waiting for messages
       *   will not be notified when the new message is added.
       * @param[in] for_send If true, the message being added will be
       *   sent.
       * @return true if successful, flase otherwise.
       */
      bool insert(utils::Header& header, size_t idx, bool move=false,
		  bool dont_notify=false, bool for_send=false);
      /**
       * @brief Append a message onto the end of the buffer.
       * @param[in] header Message with header data.
       * @param[in] move If true, the message will be moved when added.
       * @param[in] dont_notify If true, threads waiting for messages
       *   will not be notified when the new message is added.
       * @param[in] for_send If true, the message being added will be
       *   sent.
       * @return true if successful, flase otherwise.
       */
      bool append(utils::Header& header, bool move=false,
		  bool dont_notify=false, bool for_send=false);
      /**
       * @brief Prepend a message onto the beginning of the buffer.
       * @param[in] header Message with header data.
       * @param[in] move If true, the message will be moved when added.
       * @param[in] dont_notify If true, threads waiting for messages
       *   will not be notified when the new message is added.
       * @param[in] for_send If true, the message being added will be
       *   sent.
       * @return true if successful, flase otherwise.
       */
      bool prepend(utils::Header& header, bool move=false,
		   bool dont_notify=false, bool for_send=false);
      /**
       * @brief Get a message from the buffer at the specified index.
       * @param[out] header Location where message should be copied/moved
       * @param[in] idx Index of message to retrieve.
       * @param[in] move If true, the message will be moved from the
       *   buffer into header.
       * @param[in] erase If True, erase the message from the buffer.
       * @param[in] dont_notify If true, don't notify threads waiting for
       *   a change to the buffer if the buffer is modified.
       * @return true if successful, flase otherwise.
       */
      bool get(utils::Header& header, size_t idx=0,
	       bool move=false, bool erase=false, bool dont_notify=false);
      /**
       * @brief Pop a message from the buffer at the specified index.
       * @param[out] header Location where message should be copied/moved
       * @param[in] idx Index of message to retrieve.
       * @param[in] dont_notify If true, don't notify threads waiting for
       *   a change to the buffer.
       * @return true if successful, flase otherwise.
       */
      bool pop(utils::Header& header, size_t idx=0,
	       bool dont_notify=false);
      /**
       * @brief Notify all threads waiting on the buffer.
       */
      void notify() { cv.notify_all(); }
#ifdef THREADSINSTALLED
      /**
       * @brief Check if a message is waiting in the buffer.
       * @param[in] id ID of message that should be checked for in the
       *   buffer. If not provided, any message will be checked for.
       * @param[in] negative If true, the check will be that a message
       *   is NOT in the buffer (i.e. the absence of a message) that
       *   matches the provided id constraint.
       * @return true if a negative is false and a message that matches
       *   id is present or negative is true and a message that matches
       *   id is NOT present.
       */
      bool message_waiting(const std::string id="",
			   const bool negative=false);
      /**
       * @brief Wait until a message is added to the buffer.
       * @param[in] id ID of message that should be checked for in the
       *   buffer. If not provided, any message will be checked for.
       * @param[in] negative If true, the check will be that a message
       *   is NOT in the buffer (i.e. the absence of a message) that
       *   matches the provided id constraint.
       * @return true if a negative is false and a message that matches
       *   id is present or negative is true and a message that matches
       *   id is NOT present.
       */
      bool wait(const std::string id="", const bool negative=false);
      /**
       * @brief Wait until a message is added to the buffer or the
       *   specified time has passed.
       * @param[in] rel_time Maximum time to wait.
       * @param[in] id ID of message that should be checked for in the
       *   buffer. If not provided, any message will be checked for.
       * @param[in] negative If true, the check will be that a message
       *   is NOT in the buffer (i.e. the absence of a message) that
       *   matches the provided id constraint.
       * @return true if a negative is false and a message that matches
       *   id is present or negative is true and a message that matches
       *   id is NOT present.
       */
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
      std::vector<utils::Header> buffer; /**< messages to be processed */
#ifdef THREADSINSTALLED
      std::atomic_bool closed;           /**< whether the buffer is closed */
      std::mutex m;                      /**< mutex for locking buffer */
      std::condition_variable cv;        /**< conditional variable for buffer state */
#endif // THREADSINSTALLED
      std::string logInst_;              /**< log string for instance */
    };

#define START_THREAD(args)						\
    std::unique_lock<std::mutex> lk(mutex);				\
    thread = std::unique_ptr<std::thread>(new std::thread args);	\
    log_debug() << "start: waiting for thread to start" << std::endl;	\
    _wait_status(THREAD_STARTED | THREAD_COMPLETE, lk);			\
    log_debug() << "start: thread started" << std::endl
    // set_status_lock(THREAD_INIT)
#define STOP_THREAD							\
    log_debug() << "stop: begin" << std::endl;				\
    set_status_lock(THREAD_CLOSING);					\
    wait_status(THREAD_COMPLETE);					\
    try {								\
      if (thread->joinable()) {						\
	thread->join();							\
      }									\
      log_debug() << "stop: joinable = " << thread->joinable() << std::endl; \
    } catch (const std::system_error& e) {				\
      log_error() << "stop: Error joining thread (" << e.code() << "): " << e.what() << std::endl; \
    }									\
    if (status.load() & THREAD_ERROR) {					\
      log_error() << "stop: Error on thread" << std::endl;		\
    }									\
    log_debug() << "stop: end" << std::endl
    

    /**
     * @brief Base class for handling synchronization between async
     *   threads via status flags.
     */
    class AsyncStatus : public YggInterface::utils::LogBase {
    private:
      AsyncStatus(const AsyncStatus&) = delete;
      AsyncStatus& operator=(const AsyncStatus&) = delete;
    public:
      /**
       * @brief Constructor
       * @param[in] logInst String that should be used to describe the
       *   instance in log messages.
       */
      AsyncStatus(const std::string& logInst = "");
#ifdef THREADSINSTALLED
      /**
       * @brief Start the thread, passing the provided arguments on to
       *   the thread constructor.
       */
      template<typename... T>
      void start(T&&... t) {
	START_THREAD((std::forward<T>(t)...));
      }
      /**
       * @brief Stop the thread execution.
       */
      void stop();
#endif // THREADSINSTALLED
      /** \copydoc YggInterface::utils::LogBase::logClass */
      std::string logClass() const override { return "AsyncStatus"; }
      /** \copydoc YggInterface::utils::LogBase::logInst */
      std::string logInst() const override { return logInst_; }
#ifdef THREADSINSTALLED
      /**
       * @brief Update the thread status with the provided bitwise flags
       *   without locking the thread (assumes lock acquired in larger
       *   context).
       * @param[in] new_status New bitwise status flags to set.
       * @param[in] dont_notify If true, don't notify threads waiting for
       *   a change in this status.
       * @param[in] negative If true, the provided bitwise status flags
       *   should be unset.
       */
      void set_status(const int new_status, bool dont_notify=false,
		      bool negative=false);
      /**
       * @brief Update the thread status with the provided bitwise flags
       *   after locking the thread.
       * @param[in] new_status New bitwise status flags to set.
       * @param[in] dont_notify If true, don't notify threads waiting for
       *   a change in this status.
       * @param[in] negative If true, the provided bitwise status flags
       *   should be unset.
       */
      void set_status_lock(const int new_status, bool dont_notify=false,
			   bool negative=false);
      /**
       * @brief Wait for the status to match a set of status flags.
       * @param[in] new_status Status flags to wait for.
       * @param[in] lk Lock to use for status.
       * @return true if Status flags set.
       */
      bool _wait_status(const int new_status,
			std::unique_lock<std::mutex>& lk);
      /**
       * @brief Wait for the status to match a set of status flags.
       * @param[in] new_status Status flags to wait for.
       * @return true if Status flags set.
       */
      bool wait_status(const int new_status);
      /**
       * @brief Wait for the status to match a set of status flags or
       *   the specified time to have elapsed.
       * @param[in] rel_time Maximum time to wait.
       * @param[in] new_status Status flags to wait for.
       * @return true if Status flags set.
       */
      template< class Rep, class Period >
      bool wait_for_status(const std::chrono::duration<Rep, Period>& rel_time,
			   const int new_status) {
	if (status.load() & new_status)
	  return true;
	std::unique_lock<std::mutex> lk(mutex);
	return cv_status.wait_for(lk, rel_time, [this, new_status]{
	  return (this->status.load() & new_status); });
      }
      std::mutex mutex;                    /**< mutex for locking the thread */
      std::atomic_bool locked;             /**< whether the thread is locked */
      std::atomic_int status;              /**< bit flags describing thread status */
      std::condition_variable cv_status;   /**< conditional variable for waiting on a status */
      std::unique_ptr<std::thread> thread; /**< thread for performing async task */
#endif // THREADSINSTALLED
      std::string logInst_;                /**< log string for instance */
    };

    /**
     *  @brief Asynchronous backlog class for holding data to be transmitted
     */
    class AsyncBacklog : public AsyncStatus {
    private:
      AsyncBacklog(const AsyncBacklog&) = delete;
      AsyncBacklog& operator=(const AsyncBacklog&) = delete;
    public:
      /**
       * @brief Create a new instance
       * @param[in] parent The parent communicator
       */
      AsyncBacklog(AsyncComm* parent);
      /** @brief Destructor */
      ~AsyncBacklog();
      /**
       * @brief Function to run in a thread which sends/receives messages
       * @param[in] parent The parent communicator
       */
      void on_thread(AsyncComm* parent);
      /**
       * @brief Get the bitwise flag describing the status of RPC signon.
       * @return Bitwise flag.
       */
      int signon_status();
      /**
       * @brief Wait for RPC signon to complete.
       * @return true if the signon completes, false otherwise.
       */
      bool wait_for_signon();
      /**
       * @brief Send a message from the backlog
       * @return The length of data sent, in bytes.
       */
      int send();
      /**
       * @brief Receive message into the backlog
       * @return The length of data received, in bytes.
       */
      long recv();
      /** \copydoc YggInterface::utils::LogBase::logClass */
      std::string logClass() const override { return "AsyncBacklog"; }
      /**
       * @brief Determine if the buffer is closing.
       * @return true if closing, false otherwise.
       */
      bool is_closing() const { return backlog.is_closed(); }
      Comm_t* comm;             /**< parent communicator */
      AsyncBuffer backlog;      /**< set of messages to be processed */
    };
    
    /**
     * @brief Lock guard/mutex for asynchronous communication with a AsyncBacklog class.
     */
    class AsyncLockGuard {
    private:
      AsyncLockGuard(const AsyncLockGuard&) = delete;
      AsyncLockGuard& operator=(const AsyncLockGuard&) = delete;
    public:
      /**
       * @brief Create an instance
       * @param[in] backlog Instance of the AsyncBacklog class being used for communication
       * @param[in] dont_lock If true, then immediately lock the mutex.
       */
      AsyncLockGuard(AsyncBacklog* backlog, bool dont_lock=false);
      /** @brief Destructor */
      ~AsyncLockGuard();
      bool locked;            /**< indicates whether the lock is currently enabled */
      AsyncBacklog* backlog;  /**< the AsyncBacklog instance to work with */
    };

    /**
     * @brief Asynchonous communication class.
     **/
    class YGG_API AsyncComm : public CommBase<AsyncBacklog> {
    public:
      /**
       * @brief Constructor
       * @param[in] name The name of the communicator
       * @param[in] address The address to associate with the communicator,
       *   if the address is nullptr, then an address will be created.
       * @param[in] direction Enumerated direction for communicator
       * @param[in] flgs Bitwise flags describing the communicator
       * @param[in] type Enumerated communicator type
       * @param[in] reqtype Request communicator type for RPC comm.
       * @param[in] restype Response communicator type for RPC comm.
       * @param[in] reqflags Request communicator flags for RPC comm.
       * @param[in] resflags Response communicator flags for RPC comm.
       * @see utils::Address
       **/
      explicit AsyncComm(const std::string& name,
			 const utils::Address& address,
			 const DIRECTION direction = NONE, int flgs = 0,
			 const COMM_TYPE type = DEFAULT_COMM,
			 const COMM_TYPE reqtype = DEFAULT_COMM,
			 const COMM_TYPE restype = DEFAULT_COMM,
			 int reqflags = 0, int resflags = 0);
      /**
       * @brief Constructor
       * @param[in] name The name of the communicator
       * @param[in] direction Enumerated direction for communicator
       * @param[in] flgs Bitwise flags describing the communicator
       * @param[in] type Enumerated communicator type
       * @param[in] reqtype Request communicator type for RPC comm.
       * @param[in] restype Response communicator type for RPC comm.
       * @param[in] reqflags Request communicator flags for RPC comm.
       * @param[in] resflags Response communicator flags for RPC comm.
       **/
      explicit AsyncComm(const std::string& name,
			 const DIRECTION direction = NONE, int flgs = 0,
			 const COMM_TYPE type = DEFAULT_COMM,
			 const COMM_TYPE reqtype = DEFAULT_COMM,
			 const COMM_TYPE restype = DEFAULT_COMM,
			 int reqflags = 0, int resflags = 0);
      /**
       * @brief Constructor
       * @param[in] address The address to associate with the communicator,
       *   if the address is nullptr, then an address will be created.
       * @param[in] direction Enumerated direction for communicator
       * @param[in] flgs Bitwise flags describing the communicator
       * @param[in] type Enumerated communicator type
       * @param[in] reqtype Request communicator type for RPC comm.
       * @param[in] restype Response communicator type for RPC comm.
       * @param[in] reqflags Request communicator flags for RPC comm.
       * @param[in] resflags Response communicator flags for RPC comm.
       * @see utils::Address
       **/
      explicit AsyncComm(utils::Address& address,
			 const DIRECTION direction = NONE, int flgs = 0,
			 const COMM_TYPE type = DEFAULT_COMM,
			 const COMM_TYPE reqtype = DEFAULT_COMM,
			 const COMM_TYPE restype = DEFAULT_COMM,
			 int reqflags = 0, int resflags = 0);
      ADD_METHODS_BASE(AsyncComm, DEFAULT_COMM, true)

      /** \copydoc YggInterface::communicator::Comm_t::comm_nmsg */
      int comm_nmsg(DIRECTION dir=NONE) const override;
#ifdef THREADSINSTALLED
      /** \copydoc YggInterface::communicator::Comm_t::wait_for_recv */
      int wait_for_recv(const int64_t& tout) override;
#endif // THREADSINSTALLED
      /** \copydoc YggInterface::communicator::Comm_t::getMetadata */
      YggInterface::utils::Metadata& getMetadata(const DIRECTION dir=NONE) override;
      /** \copydoc YggInterface::communicator::Comm_t::set_timeout_recv */
      void set_timeout_recv(int64_t new_timeout) override;
      /** \copydoc YggInterface::communicator::Comm_t::get_timeout_recv */
      int64_t get_timeout_recv() const override;
      /** \copydoc YggInterface::utils::LogBase::logClass */
      std::string logClass() const override;
      
      using Comm_t::send;
      using Comm_t::recv;

    protected:
      /** \copydoc YggInterface::communicator::Comm_t::send_single */
      int send_single(utils::Header& header) override;
      /** \copydoc YggInterface::communicator::Comm_t::recv_single */
      long recv_single(utils::Header& header) override;
      /** \copydoc YggInterface::communicator::Comm_t::create_header_send */
      bool create_header_send(utils::Header& header) override;
      /** \copydoc YggInterface::communicator::Comm_t::create_worker */
      Comm_t* create_worker(utils::Address& address,
			    const DIRECTION dir, int flgs) override;
      
      utils::Metadata response_metadata; /**< Metadata for response communicator in RPC comm */
      COMM_TYPE request_commtype;        /**< Request communicator type for RPC comm */
      COMM_TYPE response_commtype;       /**< Response communicator type for RPC comm */
      int request_flags;                 /**< Request communicator flags for RPC comm */
      int response_flags;                /**< Request communicator flags for RPC comm */

    public:
      // RPC methods
      /**
       * @brief Add a schema to an RPC response communicator.
       * @param[in] s JSON serialized schema.
       * @param[in] use_generic If true, set schema to expect generic
       *   JSON objects.
       * @return true if successful, flase otherwise.
       */
      bool addResponseSchema(const std::string& s,
			     bool use_generic=false);
      /**
       * @brief Add a schema to an RPC response communicator(s).
       * @param[in] s JSON schema.
       * @param[in] use_generic If true, set schema to expect generic
       *   JSON objects.
       * @return true if successful, flase otherwise.
       */
      bool addResponseSchema(const rapidjson::Value& s,
			     bool use_generic=false);
      /**
       * @brief Add a schema to an RPC response communicator(s).
       * @param[in] metadata Metadata to copy containing JSON schema.
       * @param[in] use_generic If true, set schema to expect generic
       *   JSON objects.
       * @return true if successful, flase otherwise.
       */
      bool addResponseSchema(const utils::Metadata& metadata,
			     bool use_generic=false);
      /**
       * @brief Add a schema to an RPC response communicator based on a
       *   C-style format string.
       * @param[in] fmt C-style format string.
       * @param[in] use_generic If true, set schema to expect generic
       *   JSON objects.
       * @return true if successful, flase otherwise.
       */
      bool addResponseFormat(const std::string& fmt,
			     bool use_generic=false);

      friend class AsyncBacklog;
    };

  }
} // YggInterface
