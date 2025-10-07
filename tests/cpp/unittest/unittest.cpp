#include "unittest.hpp"
#include "communicators/comms.hpp"
#include <iostream>

#ifdef __clang__
#pragma GCC diagnostic push
#if __has_warning("-Wdeprecated")
#pragma GCC diagnostic ignored "-Wdeprecated"
#endif
#endif

AssertException::~AssertException() throw() {}

YggEnvironment::~YggEnvironment() {
}

void YggEnvironment::SetUp() {
#ifdef IPCINSTALLED
  Nipc = YggInterface::communicator::IPCComm::count_queues();
#endif // IPCINSTALLED
  std::cerr << "SETUP" << std::endl;
  YggInterface::communicator::ZMQComm::disable_handshake();
  YggInterface::communicator::ygg_init(true);
#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
  YggInterface::utils::global_PyGILState();
  char cwd[PATH_MAX];
  getcwd(cwd, PATH_MAX);
  // if (getcwd(cwd, PATH_MAX) != NULL) {
  //   std::cerr << "Current path is " << cwd << std::endl;
  // }
  PyObject* path = PySys_GetObject("path");
  PyObject* cwdPy = PyUnicode_FromString(cwd);
  PyList_Append(path, cwdPy);
  Py_DECREF(cwdPy);
  // PyObject_Print(path, stderr, 0);
  YggInterface::utils::global_PyGILState(true);
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
#ifdef YGGTEST_DYNAMIC_DIR
  setenv("YGGTEST_DYNAMIC_DIR", YGGTEST_DYNAMIC_DIR, 1);
  std::cerr << "YGGTEST_DYNAMIC_DIR set to " << YGGTEST_DYNAMIC_DIR << std::endl;
#else
  setenv("YGGTEST_DYNAMIC_DIR", "./", 1);
  std::cerr << "YGGTEST_DYNAMIC_DIR NOT SET" << std::endl;
#endif
  std::cerr << "SETUP COMPLETE" << std::endl;
}

void YggEnvironment::TearDown() {
  std::cerr << "TEARDOWN" << std::endl;
  YggInterface::communicator::ygg_exit();
  std::cerr << "TEARDOWN COMPLETE" << std::endl;
#ifdef IPCINSTALLED
  EXPECT_EQ(YggInterface::communicator::IPCComm::count_queues(), Nipc);
#endif // IPCINSTALLED
}

#ifdef __clang__
#pragma GCC diagnostic pop
#endif

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    std::cerr << "YGG v" << "0.1.0" << std::endl;
    ::testing::AddGlobalTestEnvironment(new YggEnvironment);

#ifdef _MSC_VER
    _CrtMemState memoryState = { 0 };
    (void)memoryState;
    _CrtMemCheckpoint(&memoryState);
    //_CrtSetBreakAlloc(X);
    //void *testWhetherMemoryLeakDetectionWorks = malloc(1);
#endif

    std::cerr << "BEFORE" << std::endl;
    int ret = RUN_ALL_TESTS();
    std::cerr << "AFTER" << std::endl;

#ifdef _MSC_VER
    // Current gtest constantly leak 2 blocks at exit
    _CrtMemDumpAllObjectsSince(&memoryState);
#endif

    return ret;
}
