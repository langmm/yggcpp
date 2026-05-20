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
    const std::map<const ENV_VAR, const std::string>& ENV_VAR_map();
    const std::map<const COMM_FLAG, const std::string>& COMM_FLAG_map();
    const std::map<const COMM_FLAG, const std::string>& FILE_FLAG_map();
    const std::map<const LANGUAGE, const std::string>& LANGUAGE_map();
    const std::map<const HeadFlags, const std::string>& HeadFlags_map();
    const std::map<const HEAD_RESET_MODE, const std::string>& HEAD_RESET_MODE_map();
    const std::map<const SIGNON_STATUS, const std::string>& SIGNON_STATUS_map();
    const std::map<const THREAD_STATUS, const std::string>& THREAD_STATUS_map();
    const std::map<const FORK_TYPE, const std::string>& FORK_TYPE_map();
    const std::map<const FUNCTION_FLAGS, const std::string>& FUNCTION_FLAGS_map();
    std::string COMM_TYPE2str(const COMM_TYPE key);
    std::string COMM_TYPE_cls2str(const COMM_TYPE key);
    std::string DIRECTION2str(const DIRECTION key);
    std::string CLEANUP_MODE2str(const CLEANUP_MODE key);
    std::string ENV_VAR2str(const ENV_VAR key);
    std::string COMM_FLAG2str(const COMM_FLAG key);
    std::string FILE_FLAG2str(const COMM_FLAG key);
    std::string LANGUAGE2str(const LANGUAGE key);
    std::string HeadFlags2str(const HeadFlags key);
    std::string HEAD_RESET_MODE2str(const HEAD_RESET_MODE key);
    std::string SIGNON_STATUS2str(const SIGNON_STATUS key);
    std::string THREAD_STATUS2str(const THREAD_STATUS key);
    std::string FORK_TYPE2str(const FORK_TYPE key);
    std::string FUNCTION_FLAGS2str(const FUNCTION_FLAGS key);
  }
}
