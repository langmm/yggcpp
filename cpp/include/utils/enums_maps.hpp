#pragma once

#include <map>
#include <string>
#include "utils/enums.hpp"

namespace YggInterface {
  namespace utils {
    const std::map<const COMM_TYPE, const std::string>& COMM_TYPE_map();
    const std::map<const COMM_TYPE, const std::string>& COMM_TYPE_cls_map();
    const std::map<const DIRECTION, const std::string>& DIRECTION_map();
    const std::map<const CLEANUP_MODE, const std::string>& CLEANUP_MODE_map();
    const std::map<const COMM_FLAG, const std::string>& COMM_FLAG_map();
    const std::map<const COMM_FLAG, const std::string>& FILE_FLAG_map();
    const std::map<const LANGUAGE, const std::string>& LANGUAGE_map();
    const std::map<const HeadFlags, const std::string>& HeadFlags_map();
    const std::map<const HEAD_RESET_MODE, const std::string>& HEAD_RESET_MODE_map();
    const std::map<const SIGNON_STATUS, const std::string>& SIGNON_STATUS_map();
    const std::map<const THREAD_STATUS, const std::string>& THREAD_STATUS_map();
    const std::map<const FORK_TYPE, const std::string>& FORK_TYPE_map();
  }
}
