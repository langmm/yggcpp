#include "Workers.hpp"
#include "CommBase.hpp"

using namespace communication::communicator;
using namespace communication::utils;

Worker::Worker(Comm_t* parent, DIRECTION dir, Address* adr) :
  comm(nullptr) {
  try {
    comm = parent->create_worker(adr, dir,
				 COMM_EOF_SENT | COMM_EOF_RECV |
				 COMM_FLAG_WORKER);
  } catch (...) {
    comm = nullptr;
  }
}
Worker::Worker(Worker&& rhs) : comm(rhs.comm) {
  rhs.comm = nullptr;
}
Worker& Worker::operator=(Worker&& rhs) {
  this->~Worker();
  this->comm = rhs.comm;
  rhs.comm = nullptr;
  return *this;
}
bool Worker::matches(DIRECTION dir, Address* adr) {
  return (comm && comm->direction == dir &&
	  ((!adr) || (adr->address() == comm->address->address())));
}
Worker::~Worker() {
  if (comm) {
    delete comm;
    comm = nullptr;
  }
}


Comm_t* WorkerList::add_worker(Comm_t* parent, DIRECTION dir,
			   Address* adr) {
  ygglog_debug << "WorkerList::add_worker: Adding worker" << std::endl;
  workers.emplace_back(parent, dir, adr);
  return workers.back().comm;
}
void WorkerList::remove_worker(Comm_t*& worker) {
  size_t idx = 0;
  if (!worker)
    return;
  Comm_t* match = find_worker(worker->direction, worker->address, &idx);
  if (match == NULL) {
    ygglog_error << "WorkerList::remove_worker: No matching worker" << std::endl;
    return;
  }
  workers.erase(workers.begin() + idx);
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
