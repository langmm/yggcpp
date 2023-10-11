#include <iostream>
#include "unittest.hpp"
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
  std::cerr << "SETUP COMPLETE" << std::endl;
}

void YggEnvironment::TearDown() {
  std::cerr << "TEARDOWN" << std::endl;
  communication::communicator::ygg_exit();
  std::cerr << "TEARDOWN COMPLETE" << std::endl;
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
