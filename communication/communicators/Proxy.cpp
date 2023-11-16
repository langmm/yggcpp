#include "Proxy.hpp"
#include "utils/logging.hpp"

using namespace communication::communicator;
using namespace communication::utils;

#ifdef THREADSINSTALLED
Proxy::Proxy(const std::string iname, const std::string oname,
	     int iflgs, int oflgs,
	     const COMM_TYPE itype,
	     const COMM_TYPE otype,
	     std::vector<communication::utils::filterFunc> fltrs,
	     std::vector<communication::utils::transformFunc> tforms) :
  icomm(nullptr), ocomm(nullptr),
  comm_mutex(), opened(false), closing(false),
  backlog_thread(&Proxy::on_thread, this, iname, oname,
		 iflgs, oflgs, itype, otype),
  filters(fltrs), transforms(tforms) {
  while (!(opened.load() || closing.load())) {
    std::this_thread::sleep_for(std::chrono::milliseconds(1));
  }
}
#else // THREADSINSTALLED
Proxy::Proxy(const std::string, const std::string,
	     int, int, const COMM_TYPE, const COMM_TYPE,
	     std::vector<communication::utils::filterFunc>,
	     std::vector<communication::utils::transformFunc>) {
  UNINSTALLED_ERROR(THREADS);
}
#endif // THREADSINSTALLED

Proxy::~Proxy() {
  log_debug() << "~Proxy: begin" << std::endl;
#ifdef THREADSINSTALLED
  closing.store(true);
  backlog_thread.join();
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
bool Proxy::on_thread(const std::string iname, const std::string oname,
		      int iflgs, int oflgs,
		      const COMM_TYPE itype, const COMM_TYPE otype) {
  bool out = true;
  {
    const std::lock_guard<std::mutex> comm_lock(comm_mutex);
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
    opened.store(true);
  }
  while (out && !closing.load()) {
    long ret = on_message();
    if (ret == 0) {
      std::this_thread::sleep_for(std::chrono::microseconds(YGG_SLEEP_TIME));
    } else if (ret < 0) {
      out = false;
      break;
    }
  }
  closing.store(true);
  {
    const std::lock_guard<std::mutex> comm_lock(comm_mutex);
    delete icomm;
    delete ocomm;
    icomm = nullptr;
    ocomm = nullptr;
  }
  return out;
}

long Proxy::on_message() {
  const std::lock_guard<std::mutex> comm_lock(comm_mutex);
  long out = 0;
  if (closing.load())
    return -1;
  if (icomm->comm_nmsg() > 0) {
    rapidjson::Document msg;
    out = icomm->recvVar(msg);
    if (out == -2) {
      out = ocomm->send_eof();
    } else if (out >= 0) {
      log_debug() << "on_message: " << msg << std::endl;
      if (!(ocomm->getFlags() & COMM_FLAGS_USED_SENT))
	ocomm->addSchema(icomm->getMetadata(RECV));
      out = ocomm->sendVar(msg);
    }
  }
  return out;
}

std::string Proxy::getAddress(DIRECTION dir) {
  const std::lock_guard<std::mutex> comm_lock(comm_mutex);
  if (!closing.load()) {
    if (dir == RECV)
      return icomm->getAddress();
    else if (dir == SEND)
      return ocomm->getAddress();
  }
  return "";
}

#endif // THREADSINSTALLED
