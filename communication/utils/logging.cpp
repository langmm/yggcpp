#include <stdarg.h>
#include "logging.hpp"
#include "tools.hpp"

using namespace communication::utils;

int Logger::_ygg_error_flag = 0;

void Logger::yggLog(const char *prefix, const char *fmt, va_list ap) {
    fprintf(stdout, "%s: %d:%d ", prefix, ygg_getpid(), get_thread_id());
    char *model_name = getenv("YGG_MODEL_NAME");
    if (model_name != nullptr) {
        fprintf(stdout, "%s", model_name);
        char *model_copy = getenv("YGG_MODEL_COPY");
        if (model_copy != nullptr) {
            fprintf(stdout, "_copy%s", model_copy);
        }
        fprintf(stdout, " ");
    }
    vfprintf(stdout, fmt, ap);
    fprintf(stdout, "\n");
    fflush(stdout);
}

void Logger::yggLog(const std::string &prefix, const std::string &fmt, va_list ap) {
    yggLog(prefix.c_str(), fmt.c_str(), ap);
}

void Logger::yggInfo(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    yggLog("INFO", fmt, ap);
    va_end(ap);
}

void Logger::yggDebug(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    yggLog("DEBUG", fmt, ap);
    va_end(ap);
}

void Logger::yggError_va(const char *fmt, va_list ap) {
    yggLog("ERROR", fmt, ap);
    _ygg_error_flag = 1;
}

void Logger::yggError_va(const std::string &fmt, va_list &ap) {
    yggError_va(fmt.c_str(), ap);
}

void Logger::yggError(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    yggError_va(fmt, ap);
    va_end(ap);
}

void Logger::ygglogThrowError(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    yggError_va(fmt, ap);
    va_end(ap);
    throw std::exception();
}
