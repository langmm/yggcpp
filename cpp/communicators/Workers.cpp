#include "Workers.hpp"
#include "CommBase.hpp"

using namespace YggInterface::communicator;
using namespace YggInterface::utils;

Worker::Worker(Comm_t* parent, DIRECTION dir, Address& adr) :
  comm(nullptr), request() {
  try {
    if (parent) {
      comm = parent->create_worker(adr, dir,
				   COMM_FLAG_EOF_SENT |
				   COMM_FLAG_EOF_RECV |
				   COMM_FLAG_WORKER);
    }
  } catch (...) {
    // Do nothing
  }
}
Worker::Worker(Worker&& rhs) noexcept : comm(rhs.comm), request(rhs.request) {
  rhs.comm = nullptr;
  rhs.request = "";
}
Worker& Worker::operator=(Worker&& rhs)  noexcept { // GCOVR_EXCL_START
  this->~Worker();
  new (this) Worker(std::move(rhs));
  return *this;
} // GCOVR_EXCL_STOP
bool Worker::matches(DIRECTION dir, Address& adr) const {
  return (request.empty() && comm && comm->direction == dir &&
	  ((!adr.valid()) || (adr.address() == comm->address.address())));
}
Worker::~Worker() {
  if (comm) {
    log_debug() << "~Worker: deleting comm" << std::endl;
    delete comm;
    comm = nullptr;
    log_debug() << "~Worker: deleted comm" << std::endl;
  }
}
std::string Worker::logInst() const {
  std::string out;
  if (comm)
    out = comm->logInst();
  return out;
}

WorkerList::~WorkerList() {
  workers.clear();
}

Comm_t* WorkerList::add_worker(Comm_t* parent, DIRECTION dir,
			       Address& adr) {
  if (!parent) {
    log_error() << "add_worker: No parent provided" << std::endl;
    return nullptr;
  }
  log_debug() << "add_worker: Adding worker" << std::endl;
  workers.emplace_back(parent, dir, adr);
  return workers.back().comm;
}
void WorkerList::remove_worker(Comm_t*& worker) {
  int idx = find_worker(worker);
  if (idx < 0) {
    log_error() << "remove_worker: No matching worker" << std::endl;
    return;
  }
  workers.erase(workers.begin() + static_cast<size_t>(idx));
  worker = nullptr;
}
Comm_t* WorkerList::find_worker(DIRECTION dir, Address& adr, size_t* idx) {
    for (size_t i = 0; i < workers.size(); i++) {
        if (workers[i].matches(dir, adr)) {
            if (idx)
                idx[0] = i;
            return workers[i].comm;
        }
  }
  log_debug() << "find_worker: Failed to find matching worker" << std::endl;
  return nullptr;
}
int WorkerList::find_worker(Comm_t* worker) {
    if (!worker)
        return -1;
    size_t idx = 0;
    if (!find_worker(worker->direction, worker->address, &idx))
        return -1;
    return static_cast<int>(idx);
}

Comm_t* WorkerList::get(Comm_t* parent, DIRECTION dir, Address& adr) {
  Comm_t* out = find_worker(dir, adr);
  if (!out) {
    out = add_worker(parent, dir, adr);
  } else if (adr.valid()) {
    out->address = adr;
  }
  if (!out) {
    log_error() << "get: Failed to get worker" << std::endl;
  }
  return out;
}

Comm_t* WorkerList::get(Comm_t* parent, DIRECTION dir) {
    Address tempAdr;
    return get(parent, dir, tempAdr);
}
Comm_t* WorkerList::get(Comm_t* parent, DIRECTION dir, const std::string& adr) {
    Address addrs(adr);
    return get(parent, dir, addrs);
}

bool WorkerList::setRequest(Comm_t* worker, const std::string& request) {
  int idx = find_worker(worker);
  if (idx < 0)
    return false;
  log_debug() << "setRequest: worker " << idx << " has request " << request << std::endl;
  workers[static_cast<size_t>(idx)].request = request;
  return true;
}
bool WorkerList::setResponse(const std::string& request) {
  log_debug() << "setResponse: Looking for worker with request " << request << std::endl;
  for (size_t i = 0; i < workers.size(); i++) {
    if (workers[i].request == request) {
      log_debug() << "setResponse: worker " << i << " has response for request " << request << " and is now available" << std::endl;
      workers[i].request = "";
      return true;
    }
  }
  return false;
}
