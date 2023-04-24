#pragma once
#ifndef __STDC_CONSTANT_MACROS
#ifdef __clang__
#pragma GCC diagnostic push
#if __has_warning("-Wreserved-id-macro")
#pragma GCC diagnostic ignored "-Wreserved-id-macro"
#endif
#endif

#  define __STDC_CONSTANT_MACROS 1 // required by C++ standard

#ifdef __clang__
#pragma GCC diagnostic pop
#endif
#endif

#ifdef _MSC_VER
#define _CRTDBG_MAP_ALLOC
#include <crtdbg.h>
#pragma warning(disable : 4996) // 'function': was declared deprecated
#endif

#if defined(__clang__) || defined(__GNUC__) && (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 2))
#if defined(__clang__) || (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 6))
#pragma GCC diagnostic push
#endif
#pragma GCC diagnostic ignored "-Weffc++"
#endif

#include "gtest/gtest.h"
#include <stdexcept>

#if defined(__clang__) || defined(__GNUC__) && (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 6))
#pragma GCC diagnostic pop
#endif

#ifdef __clang__
// All TEST() macro generated this warning, disable globally
#pragma GCC diagnostic ignored "-Wglobal-constructors"
#endif

// Use exception for catching assert
#ifdef _MSC_VER
#pragma warning(disable : 4127)
#endif

#ifdef __clang__
#pragma GCC diagnostic push
#if __has_warning("-Wdeprecated")
#pragma GCC diagnostic ignored "-Wdeprecated"
#endif
#endif

class AssertException : public std::logic_error {
public:
    AssertException(const char* w) : std::logic_error(w) {}
    AssertException(const AssertException& rhs) : std::logic_error(rhs) {}
    virtual ~AssertException() throw();
};

#ifdef __clang__
#pragma GCC diagnostic pop
#endif

// Not using noexcept for testing RAPIDJSON_ASSERT()
#define RAPIDJSON_HAS_CXX11_NOEXCEPT 0

#ifndef YGG_ASSERT
#define YGG_ASSERT(x) (!(x) ? throw AssertException(RAPIDJSON_STRINGIFY(x)) : (void)0u)
#ifndef YGG_ASSERT_THROWS
#define YGG_ASSERT_THROWS
#endif
#endif


#ifdef YGGDRASIL_DISABLE_PYTHON_C_API
#define INIT_PYTHON()
#define FINALIZE_PYTHON()
#else // YGGDRASIL_DISABLE_PYTHON_C_API
#define INIT_PYTHON()                                                   \
  {                                                                     \
    rapidjson::initialize_python("test");				\
  }
/*
    PyObject* path = PySys_GetObject("path");				\
    RAPIDJSON_ASSERT(path);						\
    const char* datadir = std::getenv("DATADIR");			\
    RAPIDJSON_ASSERT(datadir);                                          \
    PyObject* example_dir = PyUnicode_FromString(datadir);              \
    RAPIDJSON_ASSERT(example_dir);                                      \
    PyList_Append(path, example_dir);                                   \
    Py_DECREF(example_dir);                                             \
*/
#define FINALIZE_PYTHON()                       \
  {                                             \
    rapidjson::finalize_python("test");         \
  }
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
