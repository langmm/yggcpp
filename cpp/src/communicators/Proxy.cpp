#include "communicators/Proxy.hpp"
#include "utils/logging.hpp"

using namespace YggInterface::communicator;
using namespace YggInterface::utils;

#ifdef THREADSINSTALLED
Proxy::Proxy(const std::string iname, const std::string oname,
	     FLAG_TYPE iflgs, FLAG_TYPE oflgs,
	     const COMM_TYPE itype,
	     const COMM_TYPE otype,
	     std::vector<YggInterface::utils::filterFunc> fltrs,
	     std::vector<YggInterface::utils::transformFunc> tforms) :
  AsyncStatus(), icomm(nullptr), ocomm(nullptr),
  filters(fltrs), transforms(tforms) {
  START_THREAD((&Proxy::on_thread, this, iname, oname,
		iflgs, oflgs, itype, otype));
  // start(&Proxy::on_thread, this, iname, oname,
  // 	iflgs, oflgs, itype, otype);
}
#else // THREADSINSTALLED
Proxy::Proxy(const std::string, const std::string,
	     FLAG_TYPE, FLAG_TYPE, const COMM_TYPE, const COMM_TYPE,
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
		      FLAG_TYPE iflgs, FLAG_TYPE oflgs,
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
      if (icomm->getAddress() == ocomm->getAddress()) {
	log_error() << "on_thread: Send and receive communicators have the same address" << std::endl;
	out = false;
      } else {
	icomm->getMetadata().setFilters(filters);
	icomm->getMetadata().setTransforms(transforms);
      }
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

bool Proxy::is_open(const DIRECTION dir) {
  const std::lock_guard<std::mutex> comm_lock(mutex);
  if (status.load() & THREAD_CLOSING)
    return false;
  if (dir == RECV)
    return icomm->is_open();
  else if (dir == SEND)
    return ocomm->is_open();
  return (icomm->is_open() && ocomm->is_open());
}

bool Proxy::is_closed(const DIRECTION dir) {
  const std::lock_guard<std::mutex> comm_lock(mutex);
  if (status.load() & THREAD_CLOSING)
    return true;
  if (dir == RECV)
    return icomm->is_closed();
  else if (dir == SEND)
    return ocomm->is_closed();
  return (icomm->is_closed() && ocomm->is_closed());
}

void Proxy::close() {
  log_debug() << "close: begin" << std::endl;
  STOP_THREAD;
  log_debug() << "close: end" << std::endl;
}

#endif // THREADSINSTALLED
