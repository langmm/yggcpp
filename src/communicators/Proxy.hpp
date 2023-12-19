#pragma once

#include "AsyncComm.hpp"
#ifdef THREADSINSTALLED
#include <atomic>
#endif // THREADSINSTALLED

namespace YggInterface {
  namespace communicator {

    class Proxy : public AsyncStatus {
    private:
      Proxy(const Proxy&) = delete;
      Proxy& operator=(const Proxy&) = delete;
    public:
      Proxy(const std::string iname, const std::string oname,
	    int iflgs = 0, int oflgs = 0,
	    const COMM_TYPE itype = DEFAULT_COMM,
	    const COMM_TYPE otype = DEFAULT_COMM,
	    std::vector<YggInterface::utils::filterFunc> fltrs=std::vector<YggInterface::utils::filterFunc>(),
	    std::vector<YggInterface::utils::transformFunc> tforms=std::vector<YggInterface::utils::transformFunc>());
      ~Proxy();
      std::string logClass() const override { return "Proxy"; }
      std::string logInst() const override;
#ifdef THREADSINSTALLED
      std::string getAddress(DIRECTION dir);
    private:
      void on_thread(const std::string iname, const std::string oname,
		     int iflgs, int oflgs,
		     const COMM_TYPE itype, const COMM_TYPE otype);
      long on_message();
      Comm_t* icomm;
      Comm_t* ocomm;
      std::vector<YggInterface::utils::filterFunc> filters;
      std::vector<YggInterface::utils::transformFunc> transforms;
#endif // THREADSINSTALLED
    };

  }
}
