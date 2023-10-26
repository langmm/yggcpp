#pragma once

#include "CommBase.hpp"
#ifdef THREADSINSTALLED
#include <atomic>
#endif // THREADSINSTALLED

namespace communication {
  namespace communicator {

    class Proxy : public communication::utils::LogBase {
    private:
      Proxy(const Proxy&) = delete;
      Proxy& operator=(const Proxy&) = delete;
    public:
      Proxy(const std::string iname, const std::string oname,
	    int iflgs = 0, int oflgs = 0,
	    const COMM_TYPE itype = DEFAULT_COMM,
	    const COMM_TYPE otype = DEFAULT_COMM,
	    std::vector<communication::utils::filterFunc> fltrs=std::vector<communication::utils::filterFunc>(),
	    std::vector<communication::utils::transformFunc> tforms=std::vector<communication::utils::transformFunc>());
      ~Proxy();
      std::string logClass() const override { return "Proxy"; }
      std::string logInst() const override;
#ifdef THREADSINSTALLED
      std::string getAddress(DIRECTION dir);
    private:
      bool on_thread(const std::string iname, const std::string oname,
		     int iflgs, int oflgs,
		     const COMM_TYPE itype, const COMM_TYPE otype);
      long on_message();
      Comm_t* icomm;
      Comm_t* ocomm;
      std::mutex comm_mutex;
      std::atomic_bool opened;
      std::atomic_bool closing;
      std::thread backlog_thread;
      std::vector<communication::utils::filterFunc> filters;
      std::vector<communication::utils::transformFunc> transforms;
#endif // THREADSINSTALLED
    };

  }
}
