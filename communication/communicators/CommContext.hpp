#pragma once

#include "utils/tools.hpp"
#include "utils/logging.hpp"

namespace communication {
  namespace communicator {

    // Forward declaration
    class Comm_t;

    /**
     * Class for handling Yggdrasil communicator contexts including
     * cleanup on exit that is intended to be used as a singleton.
     */
    class YGG_API CommContext : public communication::utils::LogBase {
    private:
      CommContext(const CommContext&) = delete;
      CommContext& operator=(const CommContext&) = delete;
    public:
      CommContext(bool for_testing=false);
      ~CommContext();
      std::string logClass() const override { return "CommContext"; }
      // std::string logInst() const override { return logInst_; }

      // Class members
      std::vector<Comm_t*> registry_;
      std::string thread_id;
      bool for_testing_;
      CLEANUP_MODE cleanup_mode_;
      void* zmq_ctx;
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

      int init(bool for_testing=false);
      void cleanup(CLEANUP_MODE mode=CLEANUP_DEFAULT);
      void register_comm(Comm_t* x);
      Comm_t* find_registered_comm(const std::string& name,
				   const DIRECTION dir,
				   const COMM_TYPE type);
    };

  }
}

