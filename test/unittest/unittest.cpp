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

// Add setenv/unsetenv
#ifdef _MSC_VER
int setenv(const char *name, const char *value, int overwrite) {
  std::cerr << "BEGIN SETENV" << std::endl;
  if (overwrite || getenv(name) == NULL) {
    std::cerr << "SETENV: AFTER GETENV" << std::endl;
    size_t len = strlen(name) + strlen(value);
    char* tmp = (char*)malloc(len * sizeof(char));
    if (tmp == NULL)
      return -1;
    tmp[0] = '\0';
    strcat(tmp, name);
    strcat(tmp, "=");
    strcat(tmp, value);
    std::cerr << "SETENV: " << tmp << std::endl;
    int out = _putenv(tmp);
    // free(tmp);
    std::cerr << "AFTER _PUTENV" << std::endl;
    return out;
  }
  return 0;
}
int unsetenv(const char *name) {
  return setenv(name, "", true);
}
#endif // _MSC_VER
