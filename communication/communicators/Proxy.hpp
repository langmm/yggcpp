#pragma once

#include "AsyncComm.hpp"
#ifdef THREADSINSTALLED
#include <atomic>
#endif // THREADSINSTALLED

namespace communication {
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
	    std::vector<communication::utils::filterFunc> fltrs=std::vector<communication::utils::filterFunc>(),
	    std::vector<communication::utils::transformFunc> tforms=std::vector<communication::utils::transformFunc>());
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
      std::vector<communication::utils::filterFunc> filters;
      std::vector<communication::utils::transformFunc> transforms;
#endif // THREADSINSTALLED
    };

  }
}
