#pragma once

#include "utils/tools.hpp"
#ifdef _WIN32
#include <windows.h>
#include <system_error>
#else
#include <errno.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#endif

namespace YggInterface {
  namespace communicator {

    /**
     * @brief Mutual exclusion for inter-process synchronization similar
     *   to the std::mutex class for threads.
     */
    class ProcessMutex : public YggInterface::utils::LogBase {
      ProcessMutex(const ProcessMutex&) = delete;
      ProcessMutex& operator=(const ProcessMutex&) = delete;
    public:
      /**
       * @brief Constructor
       * @param[in] address The name for the mutex. If not provided, a
       *   random address will be generated.
       */
      YGG_API ProcessMutex(const std::string& address="");
      /**
       * @brief Destructor
       */
      YGG_API ~ProcessMutex();
      /**
       * @brief Initialize the mutex.
       * @param[in] address Name for the mutex.
       * @param[in] created If true, an error will be raised if the mutex
       *   is not new.
       */
      YGG_API void init(const std::string& address,
			bool created=false);
      /**
       * @brief Close the mutex
       * @param[in] remove_file If true, remove the underlying file
       *   when the semaphore is closed
       */
      YGG_API void close(bool remove_file=false);
      /**
       * @brief Locks the mutex, blocks if the mutex is not available
       */
      YGG_API void lock();
      /**
       * @brief Tries to lock the mutex, returns if the mutex is not
       *   available
       * @return true if the lock was acquired successfully, false
       *   otherwise
       */
      YGG_API bool try_lock();
      /**
       * @brief Unlocks the mutex
       */
      YGG_API void unlock();
      /** \copydoc YggInterface::utils::LogBase::logClass */
      std::string logClass() const override { return "ProcessMutex"; }
      /** \copydoc YggInterface::utils::LogBase::logInst */
      std::string logInst() const override { return address; }
      std::string address;                 /**< Mutex name */
      bool created;                        /**< Status of if the mutex was created by this process */
#ifndef _WIN32
      /**
       * @brief Get the number of processes connected to the mutex.
       * @return Number of processes, -1 indicates an error.
       */
      YGG_API int nproc() const;
      /**
       * @brief Perform a semaphore operation.
       * @param[in] op Operation
       * @param[in] flags Operation flags
       * @param[out] err Error message if an error occurs
       * @param[in] id ID of semaphore to perform operation on. If -1,
       *   the mutex semaphore will be used.
       * @return -1 if there is an error, 0 otherwise.
       */
      YGG_API int _semaphore_op(short op, short flags, std::string& err,
				int id=-1);
      /**
       * @brief Initialize a semaphore from a string address.
       * @param[in] address Address that should be used to initialize
       *   the semaphore via ftok.
       * @param[out] err Error message if an error occurs.
       * @param[in] create If true, the semaphore should be new.
       * @param[in] ftok_int Integer that should be used to create the key
       *   via ftok
       * @param[in] is_proc_count If true, the processor count is stored
       *   in the semaphore's value.
       * @return ID associated with the semaphore or -1 if there
       *   was an error.
       */
      YGG_API static int _new_semaphore(const std::string& address,
					std::string& err,
					bool create=false, int ftok_int=0,
					bool is_proc_count=false);
#endif
    private:
#ifdef _WIN32
      HANDLE handle;                       /**< Named mutex handle */
#else
      int* handle;                         /**< Semaphore handle */
      int nproc_semid;                     /**< Semaphore for processor count */
#endif
    };

    /**
     * @brief Lock guard for inter-process synchronization similar to the
     *   std::lock_guard class for threads
     * @tparam mutex_type Mutex type.
     */
    template<class Mutex>
    class ProcessLockGuard : public YggInterface::utils::LogBase {
      ProcessLockGuard(const ProcessLockGuard&) = delete;
      ProcessLockGuard& operator=(const ProcessLockGuard&) = delete;
    public:
      typedef Mutex mutex_type; /**< Mutex type */
      /**
       * @brief Constructor
       * @param[in] m Process mutex that should be locked until the
       *   lock guard goes out of scope.
       */
      explicit ProcessLockGuard(mutex_type& m) :
	mutex(m) {
	mutex.lock();
      }
      /**
       * @brief Destructor to unlock the mutex.
       */
      ~ProcessLockGuard() {
	mutex.unlock();
      }
      /** \copydoc YggInterface::utils::LogBase::logClass */
      std::string logClass() const override { return "ProcessLockGuard"; }
      /** \copydoc YggInterface::utils::LogBase::logInst */
      std::string logInst() const override { return mutex.address; }
    private:
      mutex_type& mutex;                   /**< Locked mutex */
    };

    
  }
}
