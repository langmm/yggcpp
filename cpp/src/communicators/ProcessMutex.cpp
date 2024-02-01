#include "communicators/ProcessMutex.hpp"
#include <fstream>

using namespace YggInterface::communicator;
using namespace YggInterface::utils;

/////////////////////////////////
// ProcessMutex
/////////////////////////////////

ProcessMutex::ProcessMutex(const std::string& addr) :
  address(), handle(NULL) {
  if (!addr.empty())
    init(addr);
}

ProcessMutex::~ProcessMutex() {
  close();
}

void ProcessMutex::close(bool remove_file) {
  if (handle == NULL)
    return;
#ifdef _WIN32
  CloseHandle(handle);
  if (remove_file)
    std::remove(address.c_str());
#else
  std::string err;
  log_debug() << "~ProcessMutex: nproc_semid = " << nproc_semid << std::endl;
  if (nproc_semid >= 0 && _semaphore_op(-1, 0, err, nproc_semid) == -1)
    log_error() << "~ProcessMutex: Error decrementing nproc" << err << std::endl;
  log_debug() << "~ProcessMutex: nproc = " << nproc() << std::endl;
  if (nproc() <= 0) {
    log_debug() << "~ProcessMutex: IPC_RMID" << std::endl;
    if (semctl(*handle, 0, IPC_RMID) == -1)
      log_error() << "~ProcessMutex: Error removing semaphore - " << std::string(strerror(errno)) << std::endl;
    if (nproc_semid >= 0) {
      if (semctl(nproc_semid, 0, IPC_RMID) == -1)
	log_error() << "~ProcessMutex: Error removing nproc semaphore - " << std::string(strerror(errno)) << std::endl;
      nproc_semid = -1;
    }
    if (remove_file)
      std::remove(address.c_str());
  }
  delete handle;
#endif
  handle = NULL;
}

void ProcessMutex::init(const std::string& addr, bool created_) {
  if (addr.empty())
    throw_error("init: Provided address is empty");
  if (!address.empty()) {
    if (address != addr)
      throw_error("init: Addresses do not match (existing = " + address + ", provided = " + addr);
    return;
  }
  address = addr;
  created = created_;
  std::string err;
#ifdef _WIN32
  handle = CreateMutexA(NULL, FALSE, address.c_str());
  if (handle == NULL)
    err = std::system_category().message(GetLastError());
#else
  int semid = ProcessMutex::_new_semaphore(address, err, created, 5);
  if (semid != -1) {
    handle = new int(semid);
    nproc_semid = ProcessMutex::_new_semaphore(address, err, created, 6, true);
    log_debug() << "init: nproc_semid = " << nproc_semid << std::endl;
    if (nproc_semid < 0) {
      err = "nproc_semid - " + err;
      delete handle;
      handle = NULL;
    }
  }
  log_debug() << "init: nproc = " << nproc() << std::endl;
#endif
  if (handle == NULL)
    throw_error("init: Failed to create mutex handle (" + err + ")");
  log_debug() << "init: Initialization complete" << std::endl;
}

void ProcessMutex::lock() {
  log_debug() << "lock: begin" << std::endl;
  if (handle) {
#ifdef _WIN32
    DWORD res = WaitForSingleObject(handle, INFINITE);
    if (res != WAIT_OBJECT_0)
      throw_error("lock: Failed to take ownership of the mutex - "
		  + std::system_category().message(GetLastError()));
#else
    std::string err;
    if (_semaphore_op(-1, 0, err) == -1)
      throw_error("lock: " + err);
#endif
  }
  log_debug() << "lock: done" << std::endl; 
}

bool ProcessMutex::try_lock() {
  log_debug() << "try_lock: begin" << std::endl;
  bool out = false;
  if (handle) {
#ifdef _WIN32
    DWORD res = WaitForSingleObject(handle, 0);
    if (res == WAIT_OBJECT_0)
      out = true;
    else if (res != WAIT_TIMEOUT)
      throw_error("try_lock: Failed to take ownership of the mutex - "
		  + std::system_category().message(GetLastError()));
#else
    std::string err;
    if (_semaphore_op(-1, IPC_NOWAIT, err) != -1)
      out = true;
    if ((!out) && errno != EAGAIN)
      throw_error("try_lock: " + err);
#endif
  } else {
    out = true;
  }
  log_debug() << "try_lock: done" << std::endl;
  return out;
}

void ProcessMutex::unlock() {
  log_debug() << "unlock: begin" << std::endl;
  if (handle) {
#ifdef _WIN32
    if (!ReleaseMutex(handle))
      throw_error("unlock: Failed to release mutex - "
		  + std::system_category().message(GetLastError()));
#else
    std::string err;
    if (_semaphore_op(1, IPC_NOWAIT, err) == -1)
      throw_error("unlock: " + err);
#endif
  }
  log_debug() << "unlock: done" << std::endl;
}

#ifndef _WIN32
int ProcessMutex::_semaphore_op(short op, short flags,
				std::string& err, int id) {
  if (id == -1) {
    if (!handle) {
      err = "handle NULL";
      return -1;
    }
    id = handle[0];
  }
  struct sembuf sb = {0, op, flags};
  if (semop(id, &sb, 1) == -1) {
    // if (errno == EINVAL && handle && handle[0] == id) {
    // 	delete handle;
    // 	handle = NULL;
    // } else {
    err = "semop failed - " + std::string(strerror(errno));
    return -1;
  }
  return 0;
}
int ProcessMutex::_new_semaphore(const std::string& address,
				 std::string& err,
				 bool create, int ftok_int,
				 bool is_proc_count) {
  int semid = -1;
  key_t key = ftok(address.c_str(), ftok_int);
  union semun
  {
    int val;
    struct semid_ds *buf;
    ushort array [1];
  } sem_attr;
  if (key == -1 && errno == ENOENT && create) {
    std::ofstream tmp;
    tmp.open(address.c_str(), std::fstream::out | std::fstream::app);
    tmp.close();
    key = ftok(address.c_str(), ftok_int); 
  }
  if (key == -1) {
    err = "ftok - " + std::string(strerror(errno));
    return -1;
  }
  if (create) {
    semid = semget(key, 1, 0666 | IPC_CREAT | IPC_EXCL);
  } else {
    semid = semget(key, 1, 0666 | IPC_CREAT);
  }
  if (semid < 0) {
    err = "semget - " + std::string(strerror(errno));
    return -1;
  }
  if (create) {
    sem_attr.val = 1;
    if (semctl(semid, 0, SETVAL, sem_attr) == -1) {
      err = "semctl[SETVAL] - " + std::string(strerror(errno));
      semctl(semid, 0, IPC_RMID);
      return -1;
    }
  } else if (is_proc_count) {
    struct sembuf sb = {0, 1, 0};
    if (semop(semid, &sb, 1) == -1) {
      err = "semop failed - " + std::string(strerror(errno));
      semctl(semid, 0, IPC_RMID);
      return -1;
    }
    // int nproc = semctl(semid, 0, GETVAL);
    // if (nproc < 0) {
    //   err = "semctl[GETVAL] - " + std::string(strerror(errno));
    //   semctl(semid, 0, IPC_RMID);
    //   return -1;
    // }
    // sem_attr.val = nproc + 1;
    // if (semctl(semid, 0, SETVAL, sem_attr) == -1) {
    //   err = "semctl[SETVAL] - " + std::string(strerror(errno));
    //   semctl(semid, 0, IPC_RMID);
    //   return -1;
    // }
  }
  return semid;
}

int ProcessMutex::nproc() const {
  if (nproc_semid < 0) {
    log_error() << "nproc_semid not initialized" << std::endl;
    return -1;
  }
  int nproc = semctl(nproc_semid, 0, GETVAL);
  if (nproc < 0) {
    log_error() << "semctl[GETVAL] - " + std::string(strerror(errno)) << std::endl;
    return -1;
  }
  return nproc;
}
#endif
