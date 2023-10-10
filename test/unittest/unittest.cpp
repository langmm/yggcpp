// #define RAPIDJSON_FORCE_IMPORT_ARRAY
#include <iostream>
#include "unittest.hpp"
// #include "rapidjson/pyrj.h"
#include "communicators/ZMQComm.hpp"

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
  std::cerr << "SETUP" << std::endl;
  communication::communicator::ygg_init();
  // INIT_PYTHON();
  // INIT_ZMQ();
}

void YggEnvironment::TearDown() {
  std::cerr << "TEARDOWN" << std::endl;
  communication::communicator::ygg_exit();
  // FINALIZE_PYTHON();
  // INIT_ZMQ();
  // FINALIZE_ZMQ();
}

#ifdef __clang__
#pragma GCC diagnostic pop
#endif

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    ::testing::AddGlobalTestEnvironment(new YggEnvironment);
    std::cout << "YGG v" << "0.1.0" << std::endl;

#ifdef _MSC_VER
    _CrtMemState memoryState = { 0 };
    (void)memoryState;
    _CrtMemCheckpoint(&memoryState);
    //_CrtSetBreakAlloc(X);
    //void *testWhetherMemoryLeakDetectionWorks = malloc(1);
#endif

    int ret = RUN_ALL_TESTS();

#ifdef _MSC_VER
    // Current gtest constantly leak 2 blocks at exit
    _CrtMemDumpAllObjectsSince(&memoryState);
#endif

    return ret;
}
