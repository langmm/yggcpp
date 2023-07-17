#include "Workers.hpp"
#include "CommBase.hpp"

using namespace communication::communicator;
using namespace communication::utils;

Worker::Worker(Comm_t* parent, DIRECTION dir, Address* adr) :
  comm(nullptr), request() {
  try {
    if (parent) {
      comm = parent->create_worker(adr, dir,
				   COMM_EOF_SENT | COMM_EOF_RECV |
				   COMM_FLAG_WORKER);
    }
  } catch (...) {
    // Do nothing
  }
}
Worker::Worker(Worker&& rhs) : comm(rhs.comm), request(rhs.request) {
  rhs.comm = nullptr;
  rhs.request = "";
}
Worker& Worker::operator=(Worker&& rhs) {
  this->~Worker();
  new (this) Worker(std::move(rhs));
  return *this;
}
bool Worker::matches(DIRECTION dir, Address* adr) {
  return (request.empty() && comm && comm->direction == dir &&
	  ((!adr) || (adr->address() == comm->address->address())));
}
Worker::~Worker() {
  if (comm) {
    ygglog_debug << "~Worker: deleting comm" << std::endl;
    delete comm;
    comm = nullptr;
    ygglog_debug << "~Worker: deleted comm" << std::endl;
  }
}


WorkerList::~WorkerList() {
  workers.clear();
}

Comm_t* WorkerList::add_worker(Comm_t* parent, DIRECTION dir,
			       Address* adr) {
  if (!parent) {
    ygglog_error << "WorkerList::add_worker: No parent provided" << std::endl;
    return nullptr;
  }
  ygglog_debug << "WorkerList::add_worker: Adding worker" << std::endl;
  workers.emplace_back(parent, dir, adr);
  return workers.back().comm;
}
void WorkerList::remove_worker(Comm_t*& worker) {
  int idx = find_worker(worker);
  if (idx < 0) {
    ygglog_error << "WorkerList::remove_worker: No matching worker" << std::endl;
    return;
  }
  workers.erase(workers.begin() + static_cast<size_t>(idx));
  worker = nullptr;
}
Comm_t* WorkerList::find_worker(DIRECTION dir, Address* adr, size_t* idx) {
  for (size_t i = 0; i < workers.size(); i++) {
    if (workers[i].matches(dir, adr)) {
      if (idx)
	idx[0] = i;
      return workers[i].comm;
    }
  }
  ygglog_debug << "WorkerList::find_worker: Failed to find matching worker" << std::endl;
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
Comm_t* WorkerList::get(Comm_t* parent, DIRECTION dir, Address* adr) {
  Comm_t* out = find_worker(dir, adr);
  if (!out) {
    out = add_worker(parent, dir, adr);
  } else if (adr) {
    delete out->address;
    out->address = adr;
  }
  if (!out) {
    ygglog_error << "WorkerList::get: Failed to get worker" << std::endl;
  }
  return out;
}
bool WorkerList::setRequest(Comm_t* worker, std::string request) {
  int idx = find_worker(worker);
  if (idx < 0)
    return false;
  ygglog_debug << "WorkerList::setRequest: worker " << idx << " has request " << request << std::endl;
  workers[static_cast<size_t>(idx)].request = request;
  return true;
}
bool WorkerList::setResponse(std::string request) {
  ygglog_debug << "WorkerList::setResponse: Looking for worker with request " << request << std::endl;
  for (size_t i = 0; i < workers.size(); i++) {
    if (workers[i].request == request) {
      ygglog_debug << "WorkerList::setResponse: worker " << i << " has response for request " << request << " and is now available" << std::endl;
      workers[i].request = "";
      return true;
    }
  }
  return false;
}
