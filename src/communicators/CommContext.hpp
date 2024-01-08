#pragma once

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

namespace YggInterface {
  namespace communicator {

    // Forward declaration
    class Comm_t;

    /**
     * @brief Class for handling Yggdrasil communicator contexts
     *   including cleanup on exit. It is intended to be used as a
     *   singleton.
     */
    class YGG_API CommContext : public YggInterface::utils::LogBase {
    private:
      CommContext(const CommContext&) = delete;
      CommContext& operator=(const CommContext&) = delete;
    public:
      /**
       * @brief Constructor
       * @param[in] for_testing true if for testing purposes.
       */
      CommContext(bool for_testing=false);
      ~CommContext();
      /** \copydoc YggInterface::utils::LogBase::logClass */
      std::string logClass() const override { return "CommContext"; }

      // Class members
      std::vector<Comm_t*> registry_; //!< Registry of comms created
      std::string thread_id;          //!< ID of thread context was created on
      bool for_testing_;              //!< true if context used for testing
      CLEANUP_MODE cleanup_mode_;     //!< Mode of current cleanup action
      void* zmq_ctx;                  //!< ZeroMQ context
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
#undef YGG_THREAD_MUTEX
#endif // THREADSINSTALLED

      /**
       * @brief Initialize the context.
       * @param[in] for_testing true if the context is used for testing.
       * @return 0 on success
       */
      int init(bool for_testing=false);
      /**
       * @brief Cleanup the context's resources
       * @param[in] mode Enumerated flag specifying which resources
       *   should be cleaned up.
       */
      void cleanup(CLEANUP_MODE mode=CLEANUP_DEFAULT);
      /**
       * @brief Register a communicator.
       * @param[in] x Communicator to register.
       */
      void register_comm(Comm_t* x);
      /**
       * @brief Find the registered communicator based on the given information
       * @param[in] name The name of the communicator to find
       * @param[in] dir The direction of the communicator to find
       * @param[in] type The type of cummunicator to find
       * @return The communicator, or NULL of none was found
       */
      Comm_t* find_registered_comm(const std::string& name,
				   const DIRECTION dir,
				   const COMM_TYPE type);
#ifdef YGG_ZMQ_CATCH_ERROR_POST_UNLOAD
    protected:
      DWORD _HandleWSAStartupError(unsigned int code,
				   struct _EXCEPTION_POINTERS *ep);
#endif
    };

    YGG_THREAD_GLOBAL_VAR(int, global_scope_comm, )
    extern std::shared_ptr<CommContext> global_context; //!< Global context

    /**
     * @brief Get the status of the global scope flag.
     * @return 1 indicates that the global scope flag is set.
     */
    int get_global_scope_comm();
    /**
     * @brief Set the global scope flag.
     * @param[in] new_value Value to set the global scope flag to. 0
     *   indicates that the global scope flag is not set.
     */
    void set_global_scope_comm(int new_value);
    /**
     * @brief Turn the global scope flag on so that future communicators
     *   will be treated as global.
     */
    void global_scope_comm_on();
    /**
     * @brief Turn the global scope flag off so that future communicators
     *   will be treated as local.
     */
    void global_scope_comm_off();
    /**
     * @brief Initialize the global context.
     * @param[in] for_testing true if the global context is used for testing.
     * @return 0 on success
     */
    int ygg_init(bool for_testing=false);
    /**
     * @brief Clean up the global context.
     * @param[in] mode Enumerated flag specifying which resources
     *   should be cleaned up.
     */
    void ygg_cleanup(CLEANUP_MODE mode=CLEANUP_DEFAULT);
    /**
     * @brief Clean up the global context prior to exit
     */
    void ygg_exit();

  }
}

