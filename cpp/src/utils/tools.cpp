//
// Created by friedel on 8/22/22.
//

#include "utils/tools.hpp"
#include "utils/rapidjson_wrapper.hpp"


std::string YggInterface::utils::get_thread_id() {
  std::string out;
#ifdef _OPENMP
  if (omp_in_parallel())
    return std::to_string(omp_get_thread_num());
#endif
#ifdef RAPIDJSON_YGGDRASIL_PYTHON
  // TODO: Check for Python thread
  PyThreadState* pystate = PyGILState_GetThisThreadState();
  if (pystate)
    out += std::to_string(PyThreadState_GetID(pystate));
#endif // RAPIDJSON_YGGDRASIL_PYTHON
#ifdef THREADSINSTALLED
  std::stringstream ss;
  ss << std::this_thread::get_id();
  out += ss.str();
#endif // THREADSINSTALLED
  return out;
}
