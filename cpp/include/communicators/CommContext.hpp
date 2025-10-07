#pragma once

#include "YggInterface_export.h"
#include "utils/tools.hpp"
#include "utils/logging.hpp"
// #if defined(ZMQINSTALLED) && defined(_MSC_VER)
// #define YGG_ZMQ_CATCH_ERROR_POST_UNLOAD 1
// #endif
#ifdef YGG_ZMQ_PRELOAD
#include <windows.h>
#endif // YGG_ZMQ_PRELOAD
#ifdef YGG_ZMQ_CATCH_ERROR_POST_UNLOAD
#include <windows.h> // for EXCEPTION_ACCESS_VIOLATION
#include <excpt.h>
#endif // YGG_ZMQ_CATCH_ERROR_POST_UNLOAD
#if defined(_WIN32) && !(defined(YGG_ZMQ_PRELOAD) || defined(YGG_ZMQ_CATCH_ERROR_POST_UNLOAD))
#define YGG_ZMQ_PRESERVE_CONTEXT 1
#endif
#include <random>

namespace YggInterface {
  namespace communicator {

    // Forward declaration
    class Comm_t;
    class FunctionWrapper;

    /**
     * @brief Class for handling Yggdrasil communicator contexts
     *   including cleanup on exit. It is intended to be used as a
     *   singleton.
     */
    class CommContext : public YggInterface::utils::LogBase {
    private:
      CommContext(const CommContext&) = delete;
      CommContext& operator=(const CommContext&) = delete;
    public:
      /**
       * @brief Constructor
       * @param[in] for_testing true if for testing purposes.
       */
      YGG_API CommContext(bool for_testing=false);
      YGG_API ~CommContext();
      /** \copydoc YggInterface::utils::LogBase::logClass */
      std::string logClass() const override { return "CommContext"; }

      // Class members
      std::vector<Comm_t*> registry_; //!< Registry of comms created
      std::map<std::string, FunctionWrapper*> func_registry_; //!< Registry of functions
      std::string thread_id;          //!< ID of thread context was created on
      bool for_testing_;              //!< true if context used for testing
      CLEANUP_MODE cleanup_mode_;     //!< Mode of current cleanup action
      void* zmq_ctx;                  //!< ZeroMQ context
      std::mt19937 rng;               //!< Random number generator
#ifdef YGG_ZMQ_PRELOAD
      HINSTANCE hzmqDLL;              //!< Preloaded ZeroMQ DLL
#endif // YGG_ZMQ_PRELOAD
#ifdef THREADSINSTALLED
#define YGG_THREAD_MUTEX(name)			\
      std::mutex name ## _mutex;
      YGG_THREAD_MUTEX(init)
      YGG_THREAD_MUTEX(clean)
      YGG_THREAD_MUTEX(client)
      YGG_THREAD_MUTEX(comms)
      YGG_THREAD_MUTEX(IPCComm)
      YGG_THREAD_MUTEX(zmq)
      YGG_THREAD_MUTEX(zmqport)
      YGG_THREAD_MUTEX(uuid)
      YGG_THREAD_MUTEX(functions)
#undef YGG_THREAD_MUTEX
#endif // THREADSINSTALLED

      /**
       * @brief Initialize the context.
       * @param[in] for_testing true if the context is used for testing.
       * @return 0 on success
       */
      YGG_API int init(bool for_testing=false);
      /**
       * @brief Cleanup the context's resources
       * @param[in] mode Enumerated flag specifying which resources
       *   should be cleaned up.
       */
      YGG_API void cleanup(CLEANUP_MODE mode=CLEANUP_DEFAULT);
      /**
       * @brief Register a communicator.
       * @param[in] x Communicator to register.
       */
      YGG_API void register_comm(Comm_t* x);
      /**
       * @brief Find the registered communicator based on the given information
       * @param[in] name The name of the communicator to find
       * @param[in] dir The direction of the communicator to find
       * @param[in] type The type of cummunicator to find
       * @return The communicator or NULL if none was found
       */
      YGG_API Comm_t* find_registered_comm(const std::string& name,
					   const DIRECTION dir,
					   const COMM_TYPE type);
      /**
       * @brief Register a function.
       * @param[in] x Function wrapper containing function to register.
       */
      YGG_API void register_function(FunctionWrapper* x);
      /**
       * @brief Find a registered function wrapper from the provided info.
       * @param[in] name The name of the function to return, including the
       *   language of the function in the prefix
       * @return The function wrapper or NULL if none was found
       */
      YGG_API FunctionWrapper* find_registered_function(const std::string& name);
      /**
       * @brief Get a random unique identifier
       * @return ID
       */
      YGG_API uint64_t uuid();
      /**
       * @brief Get a random unique identifier string
       * @param[in] length Length of ID string that should be generated
       * @return ID
       */
      YGG_API std::string uuid_str(const size_t& length);
#ifdef YGG_ZMQ_CATCH_ERROR_POST_UNLOAD
    protected:
      DWORD _HandleWSAStartupError(unsigned int code,
				   struct _EXCEPTION_POINTERS *ep);
#endif
    };

#ifndef DOXYGEN_SHOULD_SKIP_THIS
    YGG_THREAD_GLOBAL_VAR(int, global_scope_comm, )
    extern std::shared_ptr<CommContext> global_context; //!< Global context
#endif // DOXYGEN_SHOULD_SKIP_THIS

    /**
     * @brief Get the status of the global scope flag.
     * @return 1 indicates that the global scope flag is set.
     */
    YGG_API int get_global_scope_comm();
    /**
     * @brief Set the global scope flag.
     * @param[in] new_value Value to set the global scope flag to. 0
     *   indicates that the global scope flag is not set.
     */
    YGG_API void set_global_scope_comm(int new_value);
    /**
     * @brief Turn the global scope flag on so that future communicators
     *   will be treated as global.
     */
    YGG_API void global_scope_comm_on();
    /**
     * @brief Turn the global scope flag off so that future communicators
     *   will be treated as local.
     */
    YGG_API void global_scope_comm_off();
    /**
     * @brief Initialize the global context.
     * @param[in] for_testing true if the global context is used for testing.
     * @return 0 on success
     */
    YGG_API int ygg_init(bool for_testing=false);
    /**
     * @brief Clean up the global context.
     * @param[in] mode Enumerated flag specifying which resources
     *   should be cleaned up.
     */
    YGG_API void ygg_cleanup(CLEANUP_MODE mode=CLEANUP_DEFAULT);
    /**
     * @brief Clean up the global context prior to exit
     */
    YGG_API void ygg_exit();

  }
}

