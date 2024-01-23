#include "communicators/Proxy.hpp"
#include "utils/logging.hpp"

using namespace YggInterface::communicator;
using namespace YggInterface::utils;

#ifdef THREADSINSTALLED
Proxy::Proxy(const std::string iname, const std::string oname,
	     int iflgs, int oflgs,
	     const COMM_TYPE itype,
	     const COMM_TYPE otype,
	     std::vector<YggInterface::utils::filterFunc> fltrs,
	     std::vector<YggInterface::utils::transformFunc> tforms) :
  AsyncStatus(), icomm(nullptr), ocomm(nullptr),
  filters(fltrs), transforms(tforms) {
#ifdef _MSC_VER
  if (otype == FILE_COMM) {
    utils::YggLogThrowError("Proxy: Cannot create an asynchronous communicator that uses a file when compiling with MSVC");
  }
#endif // _MSC_VER
  START_THREAD((&Proxy::on_thread, this, iname, oname,
		iflgs, oflgs, itype, otype));
  // start(&Proxy::on_thread, this, iname, oname,
  // 	iflgs, oflgs, itype, otype);
}
#else // THREADSINSTALLED
Proxy::Proxy(const std::string, const std::string,
	     int, int, const COMM_TYPE, const COMM_TYPE,
	     std::vector<YggInterface::utils::filterFunc>,
	     std::vector<YggInterface::utils::transformFunc>) {
  UNINSTALLED_ERROR(THREADS);
}
#endif // THREADSINSTALLED

Proxy::~Proxy() {
  log_debug() << "~Proxy: begin" << std::endl;
#ifdef THREADSINSTALLED
  STOP_THREAD;
  // stop();
#endif // THREADSINSTALLED
  log_debug() << "~Proxy: end" << std::endl;
}

std::string Proxy::logInst() const {
  std::string out;
#ifdef THREADSINSTALLED
  if (icomm) out += icomm->getAddress();
  out += "-2-";
  if (ocomm) out += ocomm->getAddress();
#endif // THREADSINSTALLED
  return out;
}

#ifdef THREADSINSTALLED
void Proxy::on_thread(const std::string iname, const std::string oname,
		      int iflgs, int oflgs,
		      const COMM_TYPE itype, const COMM_TYPE otype) {
  bool out = true;
  {
    const std::lock_guard<std::mutex> comm_lock(mutex);
    // TODO: Allow multiple connections
    utils::Address iAddr;
    utils::Address oAddr;
    icomm = new_Comm_t(RECV, itype, iname, iAddr, iflgs);
    ocomm = new_Comm_t(SEND, otype, oname, oAddr,
		       oflgs | COMM_FLAG_ASYNC);
    if (icomm && ocomm) {
      icomm->getMetadata().setFilters(filters);
      icomm->getMetadata().setTransforms(transforms);
    } else {
      out = false;
    }
    set_status(THREAD_STARTED);
  }
  while (out && !(status.load() & THREAD_CLOSING)) {
    long ret = on_message();
    if (ret == 0) {
      std::this_thread::sleep_for(std::chrono::microseconds(YGG_SLEEP_TIME));
    } else if (ret < 0) {
      out = false;
      break;
    }
  }
  {
    const std::lock_guard<std::mutex> comm_lock(mutex);
    delete icomm;
    delete ocomm;
    icomm = nullptr;
    ocomm = nullptr;
    if (!out)
      set_status(THREAD_ERROR);
    set_status(THREAD_CLOSING | THREAD_COMPLETE);
  }
}

long Proxy::on_message() {
  const std::lock_guard<std::mutex> comm_lock(mutex);
  long out = 0;
  if (status.load() & THREAD_CLOSING)
    return -1;
  if (icomm->comm_nmsg() > 0) {
    rapidjson::Document msg;
    out = icomm->recvVar(msg);
    if (out == -2) {
      out = ocomm->send_eof();
    } else if (out >= 0) {
      log_debug() << "on_message: " << msg << std::endl;
      if (!(ocomm->getFlags() & COMM_FLAG_USED_SENT))
	ocomm->addSchema(icomm->getMetadata(RECV));
      out = ocomm->sendVar(msg);
    }
  }
  return out;
}

std::string Proxy::getAddress(DIRECTION dir) {
  const std::lock_guard<std::mutex> comm_lock(mutex);
  if (!(status.load() & THREAD_CLOSING)) {
    if (dir == RECV)
      return icomm->getAddress();
    else if (dir == SEND)
      return ocomm->getAddress();
  }
  return "";
}

#endif // THREADSINSTALLED
