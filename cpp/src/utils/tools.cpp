//
// Created by friedel on 8/22/22.
//

#include "utils/tools.hpp"
#include "utils/yggdrasil_rapidjson_wrapper.hpp"
#include "utils/internal.hpp"
#include <chrono>
#ifdef THREADSINSTALLED
#include <thread>
#endif // THREADSINSTALLED
#ifdef _OPENMP
#include <omp.h>
#endif // _OPENMP

std::string YggInterface::utils::getenv(const std::string& name) {
  std::string out;
  char* temp = std::getenv(name.c_str());
  if (temp)
    out.assign(temp);
  return out;
}

void YggInterface::utils::thread_usleep(const uint64_t x) {
#ifdef THREADSINSTALLED
  std::this_thread::sleep_for(std::chrono::microseconds(x));
#else // THREADSINSTALLED
  usleep(x);
#endif // THREADSINSTALLED
}

std::string YggInterface::utils::get_thread_id() {
  YGG_THREAD_LOCAL std::string out;
#ifdef _OPENMP
  if (omp_in_parallel())
    return std::to_string(omp_get_thread_num());
#endif
#if defined(YGGDRASIL_RAPIDJSON_PYTHON_WRAPPER) || defined(THREADSINSTALLED)
  bool is_empty = out.empty();
#endif
#ifdef YGGDRASIL_RAPIDJSON_PYTHON_WRAPPER
  if (is_empty) {
    PyThreadState* pystate = PyGILState_GetThisThreadState();
    if (pystate)
      out += std::to_string(PyThreadState_GetID(pystate));
  }
#endif // YGGDRASIL_RAPIDJSON_PYTHON_WRAPPER
#ifdef THREADSINSTALLED
  if (is_empty) {
    std::stringstream ss;
    ss << std::this_thread::get_id();
    out += ss.str();
  }
#endif // THREADSINSTALLED
  return out;
}
