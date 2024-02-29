#include "../../unittest.hpp"
#include "utils/multiprocessing.hpp"
#include "../../elf_hook.hpp"
#include "../../mock.hpp"


using namespace YggInterface::mock;
using namespace YggInterface::utils;


TEST(ProcessMutex, constructor) {
  ProcessMutex mutex("test", "test", true);
  mutex.init("test", "test");
  EXPECT_THROW(mutex.init("test", ""), std::exception);
  EXPECT_THROW(mutex.init("test", "invalid"), std::exception);
#ifndef _WIN32
  EXPECT_THROW(ProcessMutex("test", "invalid_file"), std::exception);
  EXPECT_THROW(ProcessMutex("test", "test", true), std::exception);
#endif // _WIN32
#ifdef ELF_AVAILABLE
  ELF_BEGIN;
  ELF_BEGIN_F(semget);
  EXPECT_THROW(ProcessMutex("test2", "test2", true), std::exception);
  ELF_END_F(semget);
  ELF_BEGIN_F(semctl);
  EXPECT_THROW(ProcessMutex("test3", "test3", true), std::exception);
  ELF_END_F(semctl);
  ELF_END;
#endif // ELF_AVAILABLE
}

TEST(ProcessMutex, destructor_error) {
  ProcessMutex* mutex = new ProcessMutex("test", "test", true);
#ifndef _WIN32
  EXPECT_EQ(mutex->handle->nproc_sem->nproc(), -1);
#endif // _WIN32
#ifdef ELF_AVAILABLE
  ELF_BEGIN;
  ELF_BEGIN_F(semctl);
  EXPECT_EQ(mutex->handle->destroy(), -1);
  ELF_END_F(semctl);
  ELF_END;
#else // ELF_AVAILABLE
  EXPECT_EQ(mutex->handle->destroy(), 0);
#endif // ELF_AVAILABLE
  mutex->~ProcessMutex();
  delete mutex;
}

#ifdef ELF_AVAILABLE
TEST(ProcessMutex, destructor_error_delete) {
  ProcessMutex* mutex = new ProcessMutex("test", "test", true);
  ELF_BEGIN;
  ELF_BEGIN_F(semctl);
  EXPECT_THROW(mutex->~ProcessMutex(), std::exception);
  ELF_END_F(semctl);
  ELF_END;
  delete mutex;
}
#endif // ELF_AVAILABLE

TEST(ProcessMutex, nproc) {
  ProcessMutex mutex("test", "test", true);
  EXPECT_EQ(mutex.nproc(), 1);
#ifdef ELF_AVAILABLE
  ELF_BEGIN;
  ELF_BEGIN_F(semctl);
  EXPECT_EQ(mutex.nproc(), -1);
  ELF_END_F(semctl);
  ELF_END;
#endif // ELF_AVAILABLE
}

TEST(ProcessMutex, lock) {
  ProcessMutex mutex("test", "test", true);
  mutex.lock();
#ifndef _WIN32
  // Windows allows a thread to reacquire a mutex it already owns
  EXPECT_FALSE(mutex.try_lock());
#endif // _WIN32
  mutex.unlock();
  EXPECT_TRUE(mutex.try_lock());
  mutex.unlock();
  errno = 0;
#ifdef ELF_AVAILABLE
  ELF_BEGIN;
  ELF_BEGIN_F(semop);
  EXPECT_THROW(mutex.lock(), std::exception);
  errno = 0;
  EXPECT_THROW(mutex.try_lock(), std::exception);
  errno = 0;
  EXPECT_THROW(ProcessMutex("test2", "test2", true), std::exception);
  ELF_END_F(semop);
  mutex.lock();
  ELF_BEGIN_F(semop);
  EXPECT_THROW(mutex.unlock(), std::exception);
  ELF_END_F(semop);
  mutex.unlock();
  ELF_END;
#endif // ELF_AVAILABLE
}


TEST(ProcessSharedMemory, constructor) {
  ProcessSharedMemory shm("test", 8, "test", true);
  EXPECT_EQ(shm.size, 8);
#ifdef ELF_AVAILABLE
  ELF_BEGIN;
  ELF_BEGIN_F(shmget);
  EXPECT_THROW(ProcessSharedMemory("test", 8, "other", true),
	       std::exception);
  ELF_END_F(shmget);
  ELF_BEGIN_F(shmat);
  EXPECT_THROW(ProcessSharedMemory("test", 8, "other", true),
	       std::exception);
  ELF_END_F(shmat);
  ELF_END;
#endif // ELF_AVAILABLE
}

TEST(ProcessSharedMemory, destructor_error) {
  ProcessSharedMemory* shm = new ProcessSharedMemory("test", 8, "test", true);
#ifdef ELF_AVAILABLE
  ELF_BEGIN;
  ELF_BEGIN_F(shmdt);
  EXPECT_EQ(shm->handle->cleanup(), -1);
  ELF_END_F(shmdt);
  ELF_BEGIN_F(shmctl);
  EXPECT_EQ(shm->handle->destroy(), -1);
  ELF_END_F(shmctl);
  ELF_END;
#else // ELF_AVAILABLE
  EXPECT_EQ(shm->handle->destroy(), 0);
  EXPECT_EQ(shm->handle->cleanup(), 0);
#endif // ELF_AVAILABLE
  shm->~ProcessSharedMemory();
  delete shm;
}

#ifdef ELF_AVAILABLE
TEST(ProcessSharedMemory, destructor_error_delete) {
  ProcessSharedMemory* shm = new ProcessSharedMemory("test", 8, "test", true);
  ELF_BEGIN;
  ELF_BEGIN_F(shmctl);
  EXPECT_THROW(shm->~ProcessSharedMemory(), std::exception);
  ELF_END_F(shmctl);
  ELF_END;
  delete shm;
}
#endif // ELF_AVAILABLE
