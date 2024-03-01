#include "utils/multiprocessing.hpp"
#include <fstream>

using namespace YggInterface::utils;

/////////////////////////////////
// IPCBase
/////////////////////////////////

#define IPC_DESTRUCTOR(cls, base)				\
  int cls::cleanup() {						\
    if (local_cleanup() == -1)					\
      return -1;						\
    return base::local_cleanup();				\
  }								\
  int cls::destroy() {						\
    if (local_destroy() == -1)					\
      return -1;						\
    return base::local_destroy();				\
  }

IPCBase::IPCBase(const std::string& addr, bool preserve_addr,
		 const std::string& logCls) :
  LogBase(), address(addr), preserve_address(preserve_addr),
  logClass_(logCls) {
  log_debug() << "IPCBase: address = " << address << std::endl;
}
IPCBase::~IPCBase() {
  log_debug() << "~IPCBase: begin" << std::endl;
  // Currently local_destroy never returns -1
  // if (local_destroy() == -1)
  //   throw_error("IPCBase: Error destroying instance");
  local_destroy();
  log_debug() << "~IPCBase: cleanup" << std::endl;
  // Currently local_cleanup never returns -1
  // if (local_cleanup() == -1)
  //   throw_error("IPCBase: Error cleaning up instance");
  local_cleanup();
  log_debug() << "~IPCBase: done" << std::endl;
}
int IPCBase::local_destroy() {
  log_debug() << "IPCBase::local_destroy: begin" << std::endl;
  if (!preserve_address) {
    log_debug() << "IPCBase::local_destroy: Removing address" << std::endl;
    std::remove(address.c_str());
  }
  log_debug() << "IPCBase::local_destroy: done" << std::endl;
  return 0;
}

#ifdef _WIN32

#define WIN32_DESTRUCTOR(cls)			\
  cls::~cls() {							\
    if (local_destroy() == -1) {				\
      throw_error("~" #cls ": Error destroying instance");	\
    }								\
    if (local_cleanup() == -1) {				\
      throw_error("~" #cls ": Error cleaning up instance");	\
    }								\
  }								\
  IPC_DESTRUCTOR(cls, Win32Base)

/////////////////////////////////
// Win32Base
/////////////////////////////////

Win32Base::Win32Base(const std::string& addr, bool preserve_addr,
		     const std::string& logCls) :
  IPCBase(addr, preserve_addr, logCls) {}
Win32Base::~Win32Base() {
  if (local_destroy() == -1)
    throw_error("~Win32Base: Error destroying instance");
  if (local_cleanup() == -1)
    throw_error("~Win32Base: Error cleaning up instance");
}
int Win32Base::local_destroy() { return 0; }
int Win32Base::local_cleanup() {
  handle = NULL;
  return 0;
}

IPC_DESTRUCTOR(Win32Base, IPCBase)

/////////////////////////////////
// Win32Mutex
/////////////////////////////////

Win32Mutex::Win32Mutex(const std::string& addr, bool preserve_addr) :
  Win32Base(addr, preserve_addr, "Win32Mutex") {
  preserve_address = true; // TODO: should this be false?
  handle = CreateMutexA(NULL, FALSE, address.c_str());
  if (handle == NULL)
    throw_error(error("Win32Mutex: CreateMutexA failed"));
}
WIN32_DESTRUCTOR(Win32Mutex)
int Win32Mutex::local_destroy() { return 0; }
int Win32Mutex::local_cleanup() {
  log_debug() << "Win32Mutex::local_cleanup: begin" << std::endl;
  if (handle) {
    CloseHandle(handle); // TODO: Check error?
    handle = NULL;
  }
  log_debug() << "Win32Mutex::local_cleanup: end" << std::endl;
  return 0;
}

int Win32Mutex::lock(bool dont_wait) {
  if (handle == NULL) {
    log_error() << "lock: Handle is NULL" << std::endl;
    return -1;
  }
  DWORD tout = INFINITE;
  if (dont_wait)
    tout = 0;
  DWORD res = WaitForSingleObject(handle, tout);
  if (res != WAIT_OBJECT_0) {
    if (dont_wait && res == WAIT_TIMEOUT)
      return -2;
    log_error() << error("lock: Failed to take ownership of the mutex") << std::endl;
    return -1;
  }
  return 0;
}

int Win32Mutex::unlock(bool) {
  if (handle == NULL) {
    log_error() << "unlock: Handle is NULL" << std::endl;
    return -1;
  }
  if (!ReleaseMutex(handle)) {
    log_error() << error("unlock: Failed to release mutex") << std::endl;
    return -1;
  }
  return 0;
}

/////////////////////////////////
// Win32SharedMem
/////////////////////////////////

Win32SharedMem::Win32SharedMem(size_t size, const std::string& addr,
			       bool preserve_addr) :
  Win32Base(addr, preserve_addr, "Win32SharedMem"),
  memory(NULL) {
  preserve_address = true; // TODO: should this be false
  ULARGE_INTEGER maxSize;
  maxSize.QuadPart = size;
  handle = CreateFileMapping(INVALID_HANDLE_VALUE, NULL,
			     PAGE_READWRITE,
			     maxSize.HighPart, maxSize.LowPart,
			     address.c_str());
  if (handle == NULL)
    throw_error(error("Win32SharedMem: CreateFileMapping failed"));
  memory = MapViewOfFile(handle, FILE_MAP_ALL_ACCESS, 0, 0, size);
  if (memory == NULL) {
    CloseHandle(handle);
    handle = NULL;
    throw_error(error("Win32SharedMem: MapViewOfFile failed"));
  }
}
WIN32_DESTRUCTOR(Win32SharedMem)
int Win32SharedMem::local_destroy() { return 0; }
int Win32SharedMem::local_cleanup() {
  log_debug() << "Win32SharedMem::local_cleanup: begin" << std::endl;
  if (memory != NULL) {
    UnmapViewOfFile(memory);
    memory = NULL;
  }
  if (handle != NULL) {
    CloseHandle(handle); // TODO: Check error?
    handle = NULL;
  }
  log_debug() << "Win32SharedMem::local_cleanup: end" << std::endl;
  return 0;
}

#undef WIN32_DESTRUCTOR

#else

/////////////////////////////////
// SysVBase
/////////////////////////////////

#define SYSV_DESTRUCTOR(cls)					\
  cls::~cls() {							\
    log_debug() << "~" << #cls << ": begin" << std::endl;	\
    if (nproc_sem) {						\
      log_debug() << "~" << #cls << ": decrementing nproc" << std::endl; \
      if (nproc() > 0 && nproc_sem->dec() == -1) {			\
	throw_error("~" #cls ": Error decrementing nproc");	\
      }								\
      if (nproc() <= 0) {					\
	log_debug() << "~" << #cls << ": destroy" << std::endl;	\
	if (local_destroy() == -1) {				\
	  throw_error("~" #cls ": Error destroying instance");	\
	}							\
      }								\
    }								\
    log_debug() << "~" << #cls << ": cleanup" << std::endl;	\
    if (local_cleanup() == -1) {				\
      throw_error("~" #cls ": Error cleaning up instance");	\
    }								\
    log_debug() << "~" << #cls << ": done" << std::endl;	\
  }								\
  IPC_DESTRUCTOR(cls, SysVBase)

SysVBase::SysVBase(const std::string& addr, const int sd,
		   bool create, bool track_nproc, bool preserve_addr,
		   const std::string& logCls) :
  IPCBase(addr, preserve_addr, logCls), key(-1), nproc_sem(NULL) {
  log_debug() << "SysVBase: begin, address = " << address << std::endl;
  key = SysVBase::safe_ftok(address, sd, create);
  if (key == -1)
    throw_error(error("ftok"));
  if (track_nproc) {
    nproc_sem = new SysVSemaphore(address + "_nproc", sd, create);
    if ((!create) && nproc_sem->inc() == -1) {
      delete nproc_sem;
      nproc_sem = NULL;
      throw_error("Failed to increment processor count");
    }
    log_debug() << "SysVBase: nproc = " << nproc() << std::endl;
  }
  log_debug() << "SysVBase: done" << std::endl;
}
SysVBase::~SysVBase() {
  log_debug() << "~SysVBase: begin" << std::endl;
  if (nproc_sem && nproc() == 0 && local_destroy() == -1)
    throw_error("~SysVBase: Error destroying instance");
  log_debug() << "~SysVBase: cleanup" << std::endl;
  if (local_cleanup() == -1)
    throw_error("~SysVBase: Error cleaning up instance");
  log_debug() << "~SysVBase: done" << std::endl;
}

int SysVBase::local_cleanup() {
  log_debug() << "SysVBase::local_cleanup: begin" << std::endl;
  if (nproc_sem) {
    delete nproc_sem;
    nproc_sem = NULL;
  }
  log_debug() << "SysVBase::local_cleanup: done" << std::endl;
  return 0;
}
int SysVBase::local_destroy() {
  log_debug() << "SysVBase::local_destroy: begin" << std::endl;
  if (nproc_sem && nproc_sem->destroy() == -1) {
    log_error() << "local_destroy: Error destroying process counting semaphore" << std::endl;
    return -1;
  }
  log_debug() << "SysVBase::local_destroy: done" << std::endl;
  return 0;
}

IPC_DESTRUCTOR(SysVBase, IPCBase)

int SysVBase::nproc() const {
  if (!nproc_sem) {
    log_error() << "nproc: process count not being tracked" << std::endl;
    return -1;
  }
  return nproc_sem->get();
}

key_t SysVBase::safe_ftok(const std::string& address,
			  int ftok_int, bool create) {
  std::cerr << "safe_ftok: " << address << ", " << ftok_int <<
    ", " << create << std::endl;
  key_t key = ftok(address.c_str(), ftok_int);
  if (key == -1 && errno == ENOENT && create) {
    std::ofstream tmp;
    tmp.open(address.c_str(), std::fstream::out | std::fstream::app);
    tmp << address << std::endl;
    tmp.close();
    key = ftok(address.c_str(), ftok_int); 
  }
  std::cerr << "safe_ftok: " << address << ", " << ftok_int <<
    ", " << create << " => " << key << std::endl;
  return key;
}

/////////////////////////////////
// SysVSemaphore
/////////////////////////////////

SysVSemaphore::SysVSemaphore(const std::string& addr, const int sd,
			     bool create, bool track_nproc,
			     bool preserve_address, int value) :
  SysVBase(addr, sd, create, track_nproc, preserve_address,
	   "SysVSemaphore"), id(-1) {
  int flags = 0666 | IPC_CREAT;
  if (create)
    flags |= IPC_EXCL;
  id = semget(key, 1, flags);
  if (id < 0)
    throw_error(error("semget"));
  if (create && set(value) == -1)
    throw_error("Failed to initialize semaphore");
}

SYSV_DESTRUCTOR(SysVSemaphore)

int SysVSemaphore::local_cleanup() {
  id = -1;
  return 0;
}

int SysVSemaphore::local_destroy() {
  log_debug() << "SysVSemaphore::local_destroy: begin" << std::endl;
  if (id != -1 && semctl(id, 0, IPC_RMID) == -1) {
    log_error() << error("semctl[IPC_RMID]") << std::endl;
    return -1;
  }
  id = -1;
  log_debug() << "SysVSemaphore::local_destroy: done" << std::endl;
  return 0;
}

int SysVSemaphore::get() {
  int out = semctl(id, 0, GETVAL);
  if (out < 0)
    log_error() << error("semctl[GETVAL]") << std::endl;
  return out;
}

int SysVSemaphore::set(const int& value) {
  union semun
  {
    int val;
    struct semid_ds *buf;
    ushort array [1];
  } sem_attr;
  sem_attr.val = value;
  if (semctl(id, 0, SETVAL, sem_attr) == -1) {
    log_error() << error("semctl[SETVAL]") << std::endl;
    // semctl(semid, 0, IPC_RMID);
    return -1;
  }
  return 0;  
}

int SysVSemaphore::op(const short& op, bool dont_wait, short flags) {
  if (dont_wait)
    flags |= IPC_NOWAIT;
  struct sembuf sb = {0, op, flags};
  if (semop(id, &sb, 1) == -1) {
    if (dont_wait && errno == EAGAIN)
      return -2;
    log_error() << error("semop[" + std::to_string(op) + "]") << std::endl;
    return -1;
  }
  return 0;
}
int SysVSemaphore::inc(bool dont_wait) {
  return op(1, dont_wait);
}
int SysVSemaphore::dec(bool dont_wait) {
  return op(-1, dont_wait);
}

/////////////////////////////////
// SysVSharedMem
/////////////////////////////////

SysVSharedMem::SysVSharedMem(size_t size,
			     const std::string& addr, const int sd,
			     bool create, bool track_nproc,
			     bool preserve_address) :
  SysVBase(addr, sd, create, track_nproc, preserve_address,
	   "SysVSharedMem"), id(-1), memory(NULL) {
  int flags = 0666 | IPC_CREAT;
  if (create)
    flags |= IPC_EXCL;
  id = shmget(key, size, flags);
  if (id < 0)
    throw_error(error("shmget"));
  memory = shmat(id, NULL, 0);
  if (memory == (void*)(-1)) {
    memory = NULL;
    throw_error(error("SysVSharedMem: shmat failed"));
  }
}

SYSV_DESTRUCTOR(SysVSharedMem)

int SysVSharedMem::local_cleanup() {
  log_debug() << "SysVSharedMem::local_cleanup: begin" << std::endl;
  if (memory && shmdt(memory) == -1) {
    log_error() << error("local_cleanup: shmdt failed") << std::endl;
    return -1;
  }
  id = -1;
  memory = NULL;
  log_debug() << "SysVSharedMem::local_cleanup: done" << std::endl;
  return 0;
}

int SysVSharedMem::local_destroy() {
  log_debug() << "SysVSharedMem::local_destroy: begin" << std::endl;
  if (id != -1 && shmctl(id, IPC_RMID, 0) == -1) {
    log_error() << error("shmctl[IPC_RMID]") << std::endl;
    return -1;
  }
  id = -1;
  log_debug() << "SysVSharedMem::local_destroy: done" << std::endl;
  return 0;
}

#undef SYSV_DESTRUCTOR

#endif

#undef IPC_DESTRUCTOR

/////////////////////////////////
// ProcessMutex
/////////////////////////////////

ProcessMutex::ProcessMutex(const std::string& nme,
			   const std::string& addr, bool created_) :
  name(nme), address(), handle(NULL) {
  if (!addr.empty())
    init(name, addr, created_);
}

ProcessMutex::~ProcessMutex() {
  close();
}

void ProcessMutex::init(const std::string& nme,
			const std::string& addr, bool created_) {
  name = nme;
  if (addr.empty())
    throw_error("init: Provided address is empty");
  if (!address.empty()) {
    if (address != addr)
      throw_error("init: Addresses do not match (existing = " + address + ", provided = " + addr);
    return;
  }
  address = addr;
  created = created_;
  log_debug() << "init: address = " << address << std::endl;
#ifdef _WIN32
  handle = new Win32Mutex(address);
#else
  handle = new SysVSemaphore(address, 5, created, true, false);
#endif
  log_debug() << "init: Initialization complete" << std::endl;
}

void ProcessMutex::close(bool remove_file) {
  if (handle == NULL)
    return;
  if (remove_file)
    handle->preserve_address = (!remove_file);
  delete handle;
  handle = NULL;
}

void ProcessMutex::lock() {
  log_debug() << "lock: begin" << std::endl;
  if (handle) {
    if (handle->lock() == -1)
      throw_error("lock: Failed to take ownership of the mutex");
  }
  log_debug() << "lock: done" << std::endl; 
}

bool ProcessMutex::try_lock() {
  log_debug() << "try_lock: begin" << std::endl;
  bool out = false;
  if (handle) {
    int res = handle->lock(true);
    if (res >= 0)
      out = true;
    else if (res != -2)
      throw_error("try_lock: Failed to take ownership of the mutex");
  }
  log_debug() << "try_lock: done" << std::endl;
  return out;
}

void ProcessMutex::unlock() {
  log_debug() << "unlock: begin" << std::endl;
  if (handle && handle->unlock(true) == -1)
    throw_error("unlock: Failed to release mutex");
  log_debug() << "unlock: done" << std::endl;
}

int ProcessMutex::nproc() const {
  int out = -1;
  if (handle)
    out = handle->nproc();
  return out;
}

/////////////////////////////////
// ProcessSharedMemory
/////////////////////////////////

ProcessSharedMemory::ProcessSharedMemory(const std::string& nme,
					 size_t siz,
					 const std::string& addr,
					 bool created) :
  LogBase(), name(nme), address(addr), mutex(nme), size(siz),
  memory(NULL), handle(NULL) {
  log_debug() << "ProcessSharedMemory: begin" << std::endl;
  mutex.init(name + "_mutex", address + "_mutex", created);
  {
    ProcessLockGuard<ProcessMutex> lock_guard(mutex);
#ifdef _WIN32
    handle = new Win32SharedMem(size, address);
#else
    handle = new SysVSharedMem(size, address, 10, created);
#endif
    memory = handle->memory;
  }
  log_debug() << "ProcessSharedMemory: done" << std::endl;
}

ProcessSharedMemory::~ProcessSharedMemory() {
  ProcessLockGuard<ProcessMutex> lock_guard(mutex);
  if (handle) {
    log_debug() << "~ProcessSharedMemory: before destroy" << std::endl;
    if (mutex.nproc() == 1 && handle->destroy() == -1)
      throw_error("~ProcessSharedMemory: Failed to destroy the handle");  // GCOV_EXCL_LINE
    log_debug() << "~ProcessSharedMemory: after destroy" << std::endl;
    delete handle;
    log_debug() << "~ProcessSharedMemory: after delete" << std::endl;
  }
  handle = NULL;
  memory = NULL;
}

