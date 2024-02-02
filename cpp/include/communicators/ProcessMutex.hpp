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
#include <sys/shm.h>
#endif

namespace YggInterface {
  namespace communicator {

#define IPC_DESTRUCTOR(cls)						\
    /** @brief Destructor */						\
    YGG_API ~cls() override;						\
    /** \copydoc IPCBase::local_cleanup */				\
    YGG_API int local_cleanup();					\
    /** \copydoc IPCBase::cleanup */					\
    YGG_API int cleanup() override;					\
    /** \copydoc IPCBase::local_destroy */				\
    YGG_API int local_destroy();					\
    /** \copydoc IPCBase::destroy */					\
    YGG_API int destroy() override

    /** @brief Base class for IPC wrappers */
    class IPCBase : public YggInterface::utils::LogBase {
      IPCBase(const IPCBase&) = delete;
      IPCBase& operator=(const IPCBase&) = delete;
    public:
      /**
       * @brief Constructor
       * @param[in] address Name used to generate the id
       * @param[in] preserve_address If true, don't remove the underlying
       *   file specified by address during destruction
       * @param[in] logClass Name of class to use in log messages
       */
      YGG_API IPCBase(const std::string& address,
			bool preserve_address=false,
			const std::string& logClass="");
      /**
       * @brief Destructor
       */
      YGG_API virtual ~IPCBase();
      /** \copydoc YggInterface::utils::LogBase::logClass */
      std::string logClass() const override { return logClass_; }
      /** \copydoc YggInterface::utils::LogBase::logInst */
      std::string logInst() const override { return address; }

      /**
       * @brief Cleanup C++ interface for this instance, but not the base
       * @return -1 on error
       */
      YGG_API int local_cleanup() { return 0; }
      /**
       * @brief Cleanup C++ interface for this instance
       * @return -1 on error
       */
      YGG_API virtual int cleanup() { return local_cleanup(); }
      /**
       * @brief Destroy the instance for this class only
       * @return -1 on error
       */
      YGG_API int local_destroy();
      /**
       * @brief Destroy the instance
       * @return -1 on error
       */
      YGG_API virtual int destroy() { return local_destroy(); }
      
      std::string address;      /**< Unique identifier for the handle */
      bool preserve_address;    /**< If true, preserve the underlying file after destruction */
    private:
      std::string logClass_;    /**< Name of class to use in log messages */
    };

#ifdef _WIN32

    /** @brief Base class for Windows API IPC wrappers */
    class Win32Base : public IPCBase {
      Win32Base(const Win32Base&) = delete;
      Win32Base& operator=(const Win32Base&) = delete;
    public:
      /**
       * @brief Constructor
       * @param[in] address Name used to generate the id
       * @param[in] preserve_address If true, don't remove the underlying
       *   file specified by address during destruction
       * @param[in] logClass Name of class to use in log messages
       */
      YGG_API Win32Base(const std::string& address,
			bool preserve_address=false,
			const std::string& logClass="");
      IPC_DESTRUCTOR(Win32Base);
      /**
       * @brief Get the current error message
       * @param[in] Context that should be added to the error message
       * @return Error message
       */
      static std::string error(const std::string& context="") {
	std::string out(std::system_category().message(GetLastError()));
	if (!context.empty())
	  out = context + " - " + out;
	return out;
      }
      PVOID handle;             /**< Object handle */
    };

    /** @brief Wrapper for Windows API mutex */
    class Win32Mutex : public Win32Base {
      Win32Mutex(const Win32Mutex&) = delete;
      Win32Mutex& operator=(const Win32Mutex&) = delete;
    public:
      /**
       * @brief Constructor
       * @param[in] address Name used to generate the id
       * @param[in] preserve_address If true, don't remove the underlying
       *   file specified by address during destruction
       */
      YGG_API Win32Mutex(const std::string& address,
			 bool preserve_addr = false);
      IPC_DESTRUCTOR(Win32Mutex);

      /**
       * @brief Locks the mutex, blocks if the mutex is not available
       * @param[in] dont_wait If true, don't wait for the semaphore to be
       *   available and return -2 if it is not. Otherwise, this will
       *   block until the semaphore is available.
       * @return 0 on success, -1 on error, -2 on unavailable
       */
      YGG_API int lock(bool dont_wait = false);
      /**
       * @brief Unlocks the mutex
       * @param[in] dont_wait If true, don't wait for the semaphore to be
       *   available and return -2 if it is not. Otherwise, this will
       *   block until the semaphore is available.
       * @return 0 on success, -1 on error, -2 on unavailable
       */
      YGG_API int unlock(bool dont_wait = false);
    };
    
    /** @brief Wrapper for Windows API shared memory */
    class Win32SharedMem : public Win32Base {
      Win32SharedMem(const Win32SharedMem&) = delete;
      Win32SharedMem& operator=(const Win32SharedMem&) = delete;
    public:
      /**
       * @brief Constructor
       * @param[in] size Size of shared memory that should be created or
       *   attached to
       * @param[in] address Name used to generate the id
       * @param[in] preserve_address If true, don't remove the underlying
       *   file specified by address during destruction
       */
      YGG_API Win32SharedMem(size_t size, const std::string& address,
			     bool preserve_addr = false);
      
      IPC_DESTRUCTOR(Win32SharedMem);
      
      int id;        /**< Shared memory id */
      void* memory;  /**< Address of shared memory */
    };
    
#else

    /** @brief Forward declarations */
    class SysVSemaphore;
    
    /** @brief Base class for Sys V IPC wrappers */
    class SysVBase : public IPCBase {
      SysVBase(const SysVBase&) = delete;
      SysVBase& operator=(const SysVBase&) = delete;
    public:
      /**
       * @brief Constructor
       * @param[in] address Name used to generate the id
       * @param[in] seed Integer used to generate the key via ftok
       * @param[in] create If true, this should be a new id
       * @param[in] track_nproc If true, use a semaphore to track
       *   the number of processes that are using this object. If false,
       *   the underlying objects will not be destroyed during destruction
       *   but can be done explicitly by calling the destroy method
       * @param[in] preserve_address If true, don't remove the underlying
       *   file specified by address during destruction
       * @param[in] logClass Name of class to use in log messages
       */
      YGG_API SysVBase(const std::string& address, const int seed=0,
		       bool create=false, bool track_nproc=false,
		       bool preserve_address=false,
		       const std::string& logClass="");
      IPC_DESTRUCTOR(SysVBase);
      /**
       * @brief Get the current error message
       * @param[in] context Context that should be added to the error
       *   message
       * @return Error message
       */
      static std::string error(const std::string& context="") {
	std::string out(strerror(errno));
	if (!context.empty())
	  out = context + " - " + out;
	return out;
      }
      /**
       * @brief Destroy the instance if there are no longer any processes
       *   attached.
       * @return -1 on error
       */
      YGG_API int destroy_if_unused();
      /**
       * @brief Get the number of processes using the instance
       * @return Number of processes, -1 indicates an error.
       */
      YGG_API int nproc() const;
      /**
       * @brief Create a key from a file, creating it if necessary
       * @param[in] address Address that should be used to initialize
       *   the key via ftok.
       * @param[in] ftok_int Integer that should be used to create the
       *   key via ftok
       * @param[in] create If true, the key should be new.
       * @return Created key, -1 on error
       */
      YGG_API static key_t safe_ftok(const std::string& address,
				     int ftok_int=0,
				     bool create=false);

      key_t key;                /**< Key that should be used to create the ID */
      SysVSemaphore* nproc_sem; /**< Semaphore used to track the number of processes using this instance */
    };

    /** @brief Wrapper class for Sys V IPC shared memory */
    class SysVSharedMem : public SysVBase {
      SysVSharedMem(const SysVSharedMem&) = delete;
      SysVSharedMem& operator=(const SysVSharedMem&) = delete;
    public:
      /**
       * @brief Constructor
       * @param[in] size Size of shared memory that should be created or
       *   attached to
       * @param[in] address Name used to generate the id
       * @param[in] seed Integer used to generate the key via ftok
       * @param[in] create If true, this should be a new id
       * @param[in] track_nproc If true, use a semaphore to track
       *   the number of processes that are using this object
       * @param[in] preserve_address If true, don't remove the underlying
       *   file specified by address during destruction
       */
      YGG_API SysVSharedMem(size_t size,
			    const std::string& address, const int seed=0,
			    bool create=false, bool track_nproc=false,
			    bool preserve_address=false);
      
      IPC_DESTRUCTOR(SysVSharedMem);

      int id;        /**< Shared memory id */
      void* memory;  /**< Address of shared memory */
    };
    
    /** @brief Wrapper class for Sys V IPC semaphore */
    class SysVSemaphore : public SysVBase {
      SysVSemaphore(const SysVSemaphore&) = delete;
      SysVSemaphore& operator=(const SysVSemaphore&) = delete;
    public:
      /**
       * @brief Constructor
       * @param[in] address Name used to generate the id
       * @param[in] seed Integer used to generate the key via ftok
       * @param[in] create If true, this should be a new id
       * @param[in] track_nproc If true, use a semaphore to track
       *   the number of processes that are using this object
       * @param[in] preserve_address If true, don't remove the underlying
       *   file specified by address during destruction
       * @param[in] value Value that semaphore should be initialized with
       */
      YGG_API SysVSemaphore(const std::string& address, const int seed=0,
			    bool create=false, bool track_nproc=false,
			    bool preserve_address=false, int value=1);
      
      IPC_DESTRUCTOR(SysVSemaphore);
      
      /**
       * @brief Get the semaphore's value
       * @return Value, -1 on error
       */
      YGG_API int get();
      /**
       * @brief Set the semaphore's value
       * @param[in] value New value
       * @return 0 on success, -1 on error, -2 on unavailable
       */
      YGG_API int set(const int& value);
      /**
       * @brief Perform an operation on a semaphore
       * @param[in] op Operation
       * @param[in] dont_wait If true, don't wait for the semaphore to be
       *   available and return -2 if it is not. Otherwise, this will
       *   block until the semaphore is available.
       * @param[in] flags Additional operation flags
       * @return 0 on success, -1 on error, -2 on unavailable
       */
      YGG_API int op(const short& op, bool dont_wait=false,
		     short flags=0);
      /**
       * @brief Increment the semaphore
       * @param[in] dont_wait If true, don't wait for the semaphore to be
       *   available and return -2 if it is not. Otherwise, this will
       *   block until the semaphore is available.
       * @return 0 on success, -1 on error, -2 on unavailable
       */
      YGG_API int inc(bool dont_wait=false);
      /**
       * @brief Decrement the semaphore
       * @param[in] dont_wait If true, don't wait for the semaphore to be
       *   available and return -2 if it is not. Otherwise, this will
       *   block until the semaphore is available.
       * @return 0 on success, -1 on error, -2 on unavailable
       */
      YGG_API int dec(bool dont_wait=false);

      /**
       * @brief Locks the mutex, blocks if the mutex is not available
       * @param[in] dont_wait If true, don't wait for the semaphore to be
       *   available and return -2 if it is not. Otherwise, this will
       *   block until the semaphore is available.
       * @return 0 on success, -1 on error, -2 on unavailable
       */
      int lock(bool dont_wait=false) { return dec(dont_wait); }
      /**
       * @brief Unlocks the mutex
       * @param[in] dont_wait If true, don't wait for the semaphore to be
       *   available and return -2 if it is not. Otherwise, this will
       *   block until the semaphore is available.
       * @return 0 on success, -1 on error, -2 on unavailable
       */
      int unlock(bool dont_wait=false) { return inc(dont_wait); }
      
      int id;               /**< Semaphore ID */
    };

#endif
    
#undef IPC_DESTRUCTOR

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
       * @param[in] created If true, an error will be raised if the mutex
       *   is not new.
       */
      YGG_API ProcessMutex(const std::string& address="",
			   bool created=false);
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
      /**
       * @brief Get the number of processes connected to the mutex.
       * @return Number of processes, -1 indicates an error.
       */
      YGG_API int nproc() const;
    private:
#ifdef _WIN32
      Win32Mutex* handle;           /**< Named mutex handle */
#else
      SysVSemaphore* handle;        /**< Semaphore handle */
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

    /**
     * @brief Shared memory object.
     */
    class ProcessSharedMemory : public YggInterface::utils::LogBase {
      ProcessSharedMemory(const ProcessSharedMemory&) = delete;
      ProcessSharedMemory& operator=(const ProcessSharedMemory&) = delete;
    public:
      /**
       * @brief Constructor
       * @param[in] size Size of the shared memory that will be created
       * @param[in] address Unique identifier for the shared memory. If
       *   empty, an address will be generated.
       * @param[in] created If true, this should be a new shared memory
       *   block
       */
      YGG_API ProcessSharedMemory(size_t size,
				  const std::string& address="",
				  bool created=false);
      /**
       * @brief Destructor
       */
      YGG_API ~ProcessSharedMemory();
      /** \copydoc YggInterface::utils::LogBase::logClass */
      std::string logClass() const override {
	return "ProcessSharedMemory";
      }
      /** \copydoc YggInterface::utils::LogBase::logInst */
      std::string logInst() const override { return address; }

      std::string address;    /**< Unique identifier for the memory */
      ProcessMutex mutex;     /**< Mutex used to synchronize access to the memory */
      size_t size;            /**< Size of the shared memory */
      void* memory;           /**< Address of the shared memory */
    private:
#ifdef _WIN32
      Win32SharedMem* handle; /**< File mapping handle */
#else
      SysVSharedMem* handle;  /**< Shared memory id */
#endif
    };
    
  }
}
