#pragma once

#include <vector>
#include "utils/enums.hpp"
#include "utils/Address.hpp"
#include "utils/logging.hpp"

namespace communication {
namespace communicator {
  
  // Forward declare
  class Comm_t;

  class Worker {
  private:
    Worker(const Worker& rhs) = delete;
    Worker& operator=(const Worker& rhs) = delete;
  public:
    Worker(Comm_t* parent, DIRECTION dir, utils::Address* adr = nullptr);
    Worker(Worker&& rhs);
    Worker& operator=(Worker&& rhs);
    bool matches(DIRECTION dir, utils::Address* adr = nullptr);
    ~Worker();
    Comm_t* comm;
  };
  class WorkerList {
  private:
    WorkerList(const WorkerList& rhs) = delete;
    WorkerList& operator=(const WorkerList& rhs) = delete;
  public:
    WorkerList() : workers() {}
    Comm_t* add_worker(Comm_t* parent, DIRECTION dir,
		       utils::Address* adr = nullptr);
    void remove_worker(Comm_t*& worker);
    Comm_t* find_worker(DIRECTION dir, utils::Address* adr = nullptr,
			size_t* idx = nullptr);
    Comm_t* get(Comm_t* parent, DIRECTION dir,
		utils::Address* adr = nullptr);
    std::vector<Worker> workers;
  };
}
}
