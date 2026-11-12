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
#include "utils/multiprocessing.hpp"

namespace YggInterface {

  namespace utils {
    // Forward declaration
    class EmbeddedLanguageBase;
  }
  
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
      /**
       * @brief Disable the embedded languages.
       * @param[in] languages Mapping between embedded languages and
       *   whether or not it should be disabled. If a language is not
       *   present, it will be disabled.
       * @returns mapping between embedded languages and initial state
       *   of each as enabled/disabled.
       */
      YGG_API std::map<LANGUAGE, bool> disable_embedded_languages(const std::map<LANGUAGE, bool>& languages={}) const;
      /**
       * @brief Enabled embedded languages based on the provided map.
       * @param[in] languages Mapping between embedded languages and
       *   whether or not it should be enabled. If a language is not
       *   present, it will be enabled.
       * @returns mapping between embedded languages and initial state
       *   of each as enabled/disabled.
       */
      YGG_API std::map<LANGUAGE, bool> enable_embedded_languages(const std::map<LANGUAGE, bool>& languages={}) const;
      // Class members
      std::vector<Comm_t*> registry_; //!< Registry of comms created
      std::map<std::string, FunctionWrapper*> func_registry_; //!< Registry of functions
      std::map<LANGUAGE, utils::EmbeddedLanguageBase*> embed_registry_; //!< Registry of embedded languages
      std::string thread_id;          //!< ID of thread context was created on
      bool for_testing_;              //!< true if context used for testing
      CLEANUP_MODE cleanup_mode_;     //!< Mode of current cleanup action
      void* zmq_ctx;                  //!< ZeroMQ context
      std::mt19937 rng;               //!< Random number generator
      // NOTE: If any additional mutexes are added, they must also be
      //   initialized in the constructor definition in CommContext.cpp
      utils::ThreadMutex init_mutex;      //!< Mutex for initialization
      utils::ThreadMutex clean_mutex;     //!< Mutex for cleanup
      utils::ThreadMutex client_mutex;    //!< Mutex for client operations
      utils::ThreadMutex comms_mutex;     //!< Mutex for communicator registry
      utils::ThreadMutex IPCComm_mutex;   //!< Mutex for IPC communicators
      utils::ThreadMutex zmq_mutex;       //!< Mutex for ZeroMQ context
      utils::ThreadMutex zmqport_mutex;   //!< Mutex for ZeroMQ port allocation
      utils::ThreadMutex uuid_mutex;      //!< Mutex for UUID generation
      utils::ThreadMutex functions_mutex; //!< Mutex for function registry
      utils::ThreadMutex embed_mutex;     //!< Mutex for embedded languages
#ifdef YGG_ZMQ_PRELOAD
      HINSTANCE hzmqDLL;              //!< Preloaded ZeroMQ DLL
#endif // YGG_ZMQ_PRELOAD

      /**
       * @brief Initialize the context.
       * @param[in] for_testing true if the context is used for testing.
       * @return 0 on success
       */
      YGG_API int init(bool for_testing=false);
      /**
       * @brief Check various configuration parameters that can be set
       *   via environment variables:
       *
       *   YGG_DEFAULT_LOG_LEVEL Default logging level.
       *   YGG_DEFAULT_COMM      Default communicator type.
       */
      YGG_API void check_env_vars();
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
       * @param[in] global_scope If true, look for a global comm even if
       *  global_scope_comm is not set.
       * @return The communicator or NULL if none was found
       */
      YGG_API Comm_t* find_registered_comm(const std::string& name,
					   const DIRECTION dir,
					   const COMM_TYPE type,
                                           bool global_scope = false);
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
       * @brief Create a registered function wrapper if one does not
       *   already exist in the registry.
       * @param[in] f Function name and import information.
       * @param[in] calling_language Language calling the function.
       * @param[in] flags Bitwise flags describing the function.
       * @returns Created function wrapper. NULL on error.
       */
      YGG_API FunctionWrapper* create_registered_function(const std::string& f,
							  const LANGUAGE calling_language=NO_LANGUAGE,
							  int flags=0);
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

