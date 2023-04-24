#define RAPIDJSON_FORCE_IMPORT_ARRAY
#include <iostream>
#include "unittest.hpp"
#include "rapidjson/pyrj.h"

#ifdef __clang__
#pragma GCC diagnostic push
#if __has_warning("-Wdeprecated")
#pragma GCC diagnostic ignored "-Wdeprecated"
#endif
#endif

AssertException::~AssertException() throw() {}

#ifdef __clang__
#pragma GCC diagnostic pop
#endif

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);

    std::cout << "YGG v" << "0.1.0" << std::endl;

   INIT_PYTHON();

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

   FINALIZE_PYTHON();

    return ret;
}
