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
  EXPECT_THROW(ProcessMutex("test", "invalid_file"), std::exception);
#ifdef ELF_AVAILABLE
  ELF_BEGIN;
  ELF_BEGIN_F(semget);
  EXPECT_THROW(ProcessMutex("test", "test", true), std::exception);
  ELF_END_F(semget);
  ELF_BEGIN_F(semctl);
  EXPECT_THROW(ProcessMutex("test", "test", true), std::exception);
  ELF_END_F(semctl);
  ELF_END;
#endif // ELF_AVAILABLE
}

#ifdef ELF_AVAILABLE
TEST(ProcessMutex, destructor_error) {
  ProcessMutex* mutex = new ProcessMutex("test", "test", true);
  ELF_BEGIN;
  ELF_BEGIN_F(semctl);
  EXPECT_THROW(delete mutex, std::exception);
  ELF_END_F(semctl);
  delete mutex;
  ELF_END;
}
#endif // ELF_AVAILABLE

TEST(ProcessMutex, nproc) {
  ProcessMutex mutex("test", "test", true);
  EXPECT_EQ(mutex.nproc(), 1);
#ifdef ELF_AVAILABLE
  ELF_BEGIN;
  ELF_BEGIN_F(semctl);
  EXPECT_THROW(mutex.nproc(), std::exception);
  ELF_END_F(semctl);
  ELF_END;
#endif // ELF_AVAILABLE
}

TEST(ProcessMutex, lock) {
  ProcessMutex mutex("test", "test", true);
  mutex.lock();
  EXPECT_FALSE(mutex.try_lock());
  mutex.unlock();
  EXPECT_TRUE(mutex.try_lock());
  mutex.unlock();
#ifdef ELF_AVAILABLE
  ELF_BEGIN;
  ELF_BEGIN_F(semop);
  EXPECT_THROW(mutex.lock(), std::exception);
  EXPECT_THROW(mutex.try_lock(), std::exception);
  ELF_END_F(semop);
  ELF_END;
#endif // ELF_AVAILABLE
}


TEST(ProcessSharedMemory, constructor) {
  ProcessSharedMemory shm("test", 8, "test", true);
  EXPECT_EQ(shm.size, 8);
#ifdef ELF_AVAILABLE
  ELF_BEGIN;
  ELF_BEGIN_F(shmget);
  EXPECT_THROW(ProcessSharedMemory("test", 8, "other", true));
  ELF_END_F(shmget);
  ELF_BEGIN_F(shmat);
  EXPECT_THROW(ProcessSharedMemory("test", 8, "other", true));
  ELF_END_F(shmat);
  ELF_END;
#endif // ELF_AVAILABLE
}

#ifdef ELF_AVAILABLE
TEST(ProcessSharedMemory, destructor_error) {
  ProcessSharedMemory* shm = new ProcessSharedMemory("test", 8, "test", true);
  ELF_BEGIN;
  ELF_BEGIN_F(shmdt);
  EXPECT_THROW(delete shm, std::exception);
  ELF_END_F(shmdt);
  delete shm;
  shm = new ProcessSharedMemory("test", 8, "test", true);
  ELF_BEGIN_F(shmctl);
  EXPECT_THROW(delete shm, std::exception);
  ELF_END_F(shmctl);
  delete shm;
  ELF_END;
}
#endif // ELF_AVAILABLE

