#include "logging.hpp"
#include "tools.hpp"
#ifndef YGG_TEST
#include <boost/log/core.hpp>
#include <boost/log/expressions.hpp>
#endif
std::string communication::utils::_getLogPretex() {
    if (!communication::utils::_loginit)
        communication::utils::init_logger();
    std::string out = std::to_string(ygg_getpid()) + ":" + std::to_string(get_thread_id()) + " ";
    char *model_name = getenv("YGG_MODEL_NAME");
    if (model_name != nullptr) {
        out += model_name;
        char *model_copy = getenv("YGG_MODEL_COPY");
        if (model_copy != nullptr) {
            out += "_copy" + std::string(model_copy);
        }
        out += " ";
    }
    return out;
}

void communication::utils::init_logger() {
#ifndef YGG_TEST
    boost::log::core::get()->set_filter(boost::log::trivial::severity >= loggingLevel);
#endif
    communication::utils::_loginit = true;
}

void communication::utils::ygglog_throw_error(const std::string& msg) {
    ygglog_error << msg;
    throw std::exception();
}