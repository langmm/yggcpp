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
  std::cout << "SETUP" << std::endl;
  communication::communicator::ygg_init();
}

void YggEnvironment::TearDown() {
  std::cout << "TEARDOWN" << std::endl;
  communication::communicator::ygg_exit();
}

#ifdef __clang__
#pragma GCC diagnostic pop
#endif

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    std::cout << "YGG v" << "0.1.0" << std::endl;
    ::testing::AddGlobalTestEnvironment(new YggEnvironment);

#ifdef _MSC_VER
    _CrtMemState memoryState = { 0 };
    (void)memoryState;
    _CrtMemCheckpoint(&memoryState);
    //_CrtSetBreakAlloc(X);
    //void *testWhetherMemoryLeakDetectionWorks = malloc(1);
#endif

    std::cout << "BEFORE" << std::endl;
    int ret = RUN_ALL_TESTS();
    std::cout << "AFTER" << std::endl;

#ifdef _MSC_VER
    // Current gtest constantly leak 2 blocks at exit
    _CrtMemDumpAllObjectsSince(&memoryState);
#endif

    return ret;
}
