#pragma once

#include "communicators/CommBase.hpp"
#include "utils/serialization.hpp"

namespace YggInterface {
  namespace communicator {

    class AsyncBacklog;
    class Proxy;

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
      /** @brief Destructor */
      ~AsyncBuffer();
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
      void notify();
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
       * @param[in] twait Maximum time to wait (in microseconds).
       * @param[in] id ID of message that should be checked for in the
       *   buffer. If not provided, any message will be checked for.
       * @param[in] negative If true, the check will be that a message
       *   is NOT in the buffer (i.e. the absence of a message) that
       *   matches the provided id constraint.
       * @return true if a negative is false and a message that matches
       *   id is present or negative is true and a message that matches
       *   id is NOT present.
       */
      bool wait_for(const int64_t& twait,
		    const std::string id="", const bool negative=false);
    private:
      std::vector<utils::Header> buffer; /**< messages to be processed */
      std::string logInst_;              /**< log string for instance */
      class ImplBuffer;                  /**< Forward declartion of buffer implementation */
      std::unique_ptr<ImplBuffer> pImplBuffer; /**< Pointer to buffer implementation */
    };

    /**
     * @brief Base class for handling synchronization between async
     *   threads via status flags.
     */
    class AsyncStatus : public YggInterface::utils::LogBase {
    private:
      AsyncStatus(const AsyncStatus&) = delete;
      AsyncStatus& operator=(const AsyncStatus&) = delete;
    public:
      friend class AsyncBacklog;
      friend class Proxy;
      /**
       * @brief Constructor
       * @param[in] logInst String that should be used to describe the
       *   instance in log messages.
       */
      AsyncStatus(const std::string& logInst = "");
      /** @brief Destructor */
      ~AsyncStatus();
      /** \copydoc YggInterface::utils::LogBase::logInst */
      std::string logInst() const override { return logInst_; }
      /**
       * @brief Notify all threads waiting on the status.
       */
      void notify();
      /**
       * @brief Check of the status is locked.
       * @returns true if locked, false otherwise.
       */
      bool is_locked() const;
      /**
       * @brief Lock the status.
       */
      void lock();
      /**
       * @brief Unlock the status.
       */
      void unlock();
      /**
       * @brief Stop the thread.
       */
      void stop_thread();
      /**
       * @brief Get a pointer to the thread.
       * @returns Thread pointer.
       */
      void* get_thread();
      /**
       * @brief Get a pointer to the thread.
       * @returns Thread pointer.
       */
      const void* get_thread() const;
      /**
       * @brief Set the managed thread pointer and wait for it to report
       *   that it has started.
       * @param[in] ptr Pointer to thread that will be managed.
       * @param[in] lock_ptr Pointer to mutex lock that should be used
       *   to wait for the thread to report it has started.
       */
      void set_thread(void* ptr, void* lock_ptr = nullptr);
      /**
       * @brief Get a pointer to the mutex.
       * @returns Mutex pointer.
       */
      void* get_mutex();
      /**
       * @brief Get the thread status.
       * @returns int Current status.
       */
      int get_status() const;
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
       * @return true if Status flags set.
       */
      bool wait_status(const int new_status);
      /**
       * @brief Wait for the status to match a set of status flags or
       *   the specified time to have elapsed.
       * @param[in] twait Maximum time to wait in micro seconds.
       * @param[in] new_status Status flags to wait for.
       * @return true if Status flags set.
       */
      bool wait_for_status(const int64_t& twait,
			   const int new_status);
    private:
      std::string logInst_;                /**< log string for instance */
      class ImplStatus;                    /**< Forward declartion of status implementation */
      std::unique_ptr<ImplStatus> pImplStatus; /**< Pointer to status implementation */
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
     * @brief Lock guard/mutex for asynchronous communication with a AsyncStatus class.
     */
    class AsyncLockGuard {
    private:
      AsyncLockGuard(const AsyncLockGuard&) = delete;
      AsyncLockGuard& operator=(const AsyncLockGuard&) = delete;
    public:
      /**
       * @brief Create an instance
       * @param[in] status Instance of the AsyncStatus class being used
       *   for communication
       * @param[in] dont_lock If true, then immediately lock the mutex.
       */
      AsyncLockGuard(AsyncStatus* status, bool dont_lock=false);
      /** @brief Destructor */
      ~AsyncLockGuard();
      bool locked;            /**< indicates whether the lock is currently enabled */
      AsyncStatus* status;  /**< the AsyncStatus instance to work with */
    };

    /**
     * @brief Asynchonous communication class.
     **/
    class AsyncComm : public CommBase<AsyncBacklog> {
    public:
      COMM_CONSTRUCTOR_CORE_DEC_NOLOG(AsyncComm, DEFAULT_COMM, true)

      /** \copydoc YggInterface::communicator::Comm_t::nmsg */
      YGG_API int nmsg(DIRECTION dir=NONE) const override;
      /** \copydoc YggInterface::communicator::Comm_t::wait_for_recv */
      YGG_API int wait_for_recv(const int64_t& tout) const override;
      /** \copydoc YggInterface::communicator::Comm_t::getMetadata */
      YGG_API YggInterface::utils::Metadata& getMetadata(const DIRECTION dir=NONE) override;
      /** \copydoc YggInterface::communicator::Comm_t::set_timeout_recv */
      YGG_API void set_timeout_recv(int64_t new_timeout) override;
      /** \copydoc YggInterface::communicator::Comm_t::get_timeout_recv */
      YGG_API int64_t get_timeout_recv() const override;
      /** \copydoc YggInterface::utils::LogBase::logClass */
      YGG_API std::string logClass() const override;
      
      /**
       * @brief Close the thread
       */
      YGG_API void close_thread();

    protected:
      /** \copydoc YggInterface::communicator::Comm_t::send_single */
      YGG_API int send_single(utils::Header& header) override;
      /** \copydoc YggInterface::communicator::Comm_t::recv_single */
      YGG_API long recv_single(utils::Header& header) override;
      /** \copydoc YggInterface::communicator::Comm_t::create_header_send */
      YGG_API bool create_header_send(utils::Header& header) override;
      /** \copydoc YggInterface::communicator::Comm_t::create_worker */
      YGG_API Comm_t* create_worker(utils::Address& address,
				    const DIRECTION dir,
				    FLAG_TYPE flgs) override;
      
      utils::Metadata response_metadata; /**< Metadata for response communicator in RPC comm */
      COMM_TYPE request_commtype;        /**< Request communicator type for RPC comm */
      COMM_TYPE response_commtype;       /**< Response communicator type for RPC comm */
      FLAG_TYPE request_flags;                 /**< Request communicator flags for RPC comm */
      FLAG_TYPE response_flags;                /**< Request communicator flags for RPC comm */

    public:
      // RPC methods
      /**
       * @brief Add a schema to an RPC response communicator.
       * @param[in] s JSON serialized schema.
       * @param[in] use_generic If true, set schema to expect generic
       *   JSON objects.
       * @return true if successful, flase otherwise.
       */
      YGG_API bool addResponseSchema(const std::string& s,
				     bool use_generic=false);
      /**
       * @brief Add a schema to an RPC response communicator(s).
       * @param[in] s JSON schema.
       * @param[in] use_generic If true, set schema to expect generic
       *   JSON objects.
       * @return true if successful, flase otherwise.
       */
      YGG_API bool addResponseSchema(const yggdrasil_rapidjson::Value& s,
				     bool use_generic=false);
      /**
       * @brief Add a schema to an RPC response communicator(s).
       * @param[in] metadata Metadata to copy containing JSON schema.
       * @param[in] use_generic If true, set schema to expect generic
       *   JSON objects.
       * @return true if successful, flase otherwise.
       */
      YGG_API bool addResponseSchema(const utils::Metadata& metadata,
				     bool use_generic=false);
      /**
       * @brief Add a schema to an RPC response communicator based on a
       *   C-style format string.
       * @param[in] fmt C-style format string.
       * @param[in] use_generic If true, set schema to expect generic
       *   JSON objects.
       * @return true if successful, flase otherwise.
       */
      YGG_API bool addResponseFormat(const std::string& fmt,
				     bool use_generic=false);

      friend class AsyncBacklog;
    };

  }
} // YggInterface
