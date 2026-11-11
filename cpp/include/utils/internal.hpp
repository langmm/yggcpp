#pragma once
/**
   The following macros will only be defined when the library is being
   built and should not be used in any header or installed
 */

#include "YggInterface_export.h"

#if defined(YggInterface_EXPORTS) || defined(YggInterface_fortran_EXPORTS) || defined(YGGDRASIL_RAPIDJSON_PYTHON_WRAPPER)

/** @brief Macro for declaring a thread local variable */
#ifdef THREADSINSTALLED
#define YGG_THREAD_LOCAL static thread_local
#else // THREADSINSTALLED
#define YGG_THREAD_LOCAL static
#endif // THREADSINSTALLED

/**
 * @brief Macro to lock global mutex for thread safe region
 * @param[in] name Mutex name
 */
#ifdef THREADSINSTALLED
#define YGG_THREAD_SAFE_MUTEX_GLOBAL(name)             \
  const YggInterface::utils::LockGuard<YggInterface::utils::ThreadMutex> name ## _lock(global_context->name ## _mutex);
#else // THREADSINSTALLED
#define YGG_THREAD_SAFE_MUTEX_GLOBAL(name)
#endif // THREADSINSTALLED

/**
 * @brief Macro to lock local mutex for thread safe region
 * @param[in] name Mutex name
 */
#ifdef THREADSINSTALLED
#define YGG_THREAD_SAFE_MUTEX_LOCAL(name)             \
  const YggInterface::utils::LockGuard<YggInterface::utils::ThreadMutex> name ## _lock(name ## _mutex);
#else // THREADSINSTALLED
#define YGG_THREAD_SAFE_MUTEX_LOCAL(name)
#endif // THREADSINSTALLED

/** @brief Macro to open section that should be synchronized across
  OpenMP threads, typically where thread global variables are acessed. */
#ifdef _OPENMP
#define YGG_THREAD_SAFE_OPENMP(name)            \
  _Pragma(STRINGIFY(omp critical (name)))
#else // _OPENMP
#define YGG_THREAD_SAFE_OPENMP(name)
#endif // _OPENMP

/** @brief Macro to open section that should be synchronized across
  threads, typically where thread global variables are acessed. */
#define YGG_THREAD_SAFE_BEGIN(name)             \
  YGG_THREAD_SAFE_OPENMP(name) {                \
    YGG_THREAD_SAFE_MUTEX_GLOBAL(name)

/** @brief Macro to open section that should be synchronized across
  threads using a local mutex */
#define YGG_THREAD_SAFE_BEGIN_LOCAL(name)       \
  YGG_THREAD_SAFE_OPENMP(name) {                \
    YGG_THREAD_SAFE_MUTEX_LOCAL(name)

/** @brief Macro to close section that should be synchronized across
  threads, typically where thread global variables are acessed. */
#define YGG_THREAD_SAFE_END			\
  }

#endif // YggInterface_EXPORTS
