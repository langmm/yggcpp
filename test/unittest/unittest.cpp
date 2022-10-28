#include <iostream>
#include "unittest.hpp"

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

//#ifdef RAPIDJSON_YGGDRASIL
//    INIT_PYTHON();
//#endif // RAPIDJSON_YGGDRASIL

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

//#ifdef RAPIDJSON_YGGDRASIL
//    FINALIZE_PYTHON();
//#endif // RAPIDJSON_YGGDRASIL

    return ret;
}
