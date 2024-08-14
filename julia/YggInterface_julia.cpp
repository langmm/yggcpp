
// LINES AFTER THIS WERE GENERATED AND SHOULD NOT BE MODIFIED DIRECTLY
//====================================================================
#include "jlcxx/jlcxx.hpp"
#include "communicators/WrapComm.hpp"
namespace jlcxx
{
    template<> struct SuperType<YggInterface::communicator::WrapComm> 
{ typedef YggInterface::communicator::Comm_t type; };
}
JLCXX_MODULE define_communicator_module(jlcxx::Module& mod)
{
  using namespace YggInterface::communicator;
    mod.add_type<Comm_t>("Comm_t")
      .method<int, Comm_t>("send_eof", &Comm_t::send_eof)
      .method<int, Comm_t, const char*, const size_t&>("send_raw", &Comm_t::send_raw)
      .method<int, Comm_t, const rapidjson::Document&, bool>("send", &Comm_t::send)
      .method<void, Comm_t, int64_t>("set_timeout_recv", &Comm_t::set_timeout_recv)
      .method<int64_t, Comm_t>("get_timeout_recv", &Comm_t::get_timeout_recv)
      .method<int, Comm_t, const int64_t&>("wait_for_recv", &Comm_t::wait_for_recv)
      .method<long, Comm_t, rapidjson::Document&, bool>("recv", &Comm_t::recv)
      .method<long, Comm_t, const rapidjson::Document&, rapidjson::Document&>("call", &Comm_t::call)
      .method<int, Comm_t, DIRECTION>("nmsg", &Comm_t::nmsg)
      .method<void, Comm_t>("open", &Comm_t::open)
      .method<void, Comm_t>("close", &Comm_t::close)
      .method<bool, Comm_t>("is_closed", &Comm_t::is_closed)
      .method<bool, Comm_t>("is_open", &Comm_t::is_open)
      .method<COMM_TYPE, Comm_t>("getType", &Comm_t::getType)
      .method<void, Comm_t, COMM_TYPE>("setType", &Comm_t::setType)
      .method<bool, Comm_t>("valid", &Comm_t::valid)
      .method<bool, Comm_t>("global", &Comm_t::global)
      .method<bool, Comm_t>("async", &Comm_t::async)
      .method<FLAG_TYPE&, Comm_t>("getFlags", &Comm_t::getFlags)
      .method<FLAG_TYPE, Comm_t>("getFlags", &Comm_t::getFlags)
      .method<const std::string&, Comm_t>("getName", &Comm_t::getName)
      .method<std::string, Comm_t>("logClass", &Comm_t::logClass)
      .method<std::string, Comm_t>("logInst", &Comm_t::logInst)
      .method<std::string, Comm_t>("getAddress", &Comm_t::getAddress)
      .method<DIRECTION, Comm_t>("getDirection", &Comm_t::getDirection)
      .method<COMM_TYPE, Comm_t>("getCommType", &Comm_t::getCommType)
      .method<LANGUAGE, Comm_t>("getLanguage", &Comm_t::getLanguage)
      .method<bool, Comm_t, LANGUAGE>("setLanguage", &Comm_t::setLanguage)
      .method<size_t, Comm_t>("getMaxMsgSize", &Comm_t::getMaxMsgSize)
      .method<size_t, Comm_t>("getMsgBufSize", &Comm_t::getMsgBufSize)
      .method<WorkerList&, Comm_t>("getWorkers", &Comm_t::getWorkers)
      .method<bool, Comm_t, bool>("PyGIL_release", &Comm_t::PyGIL_release)
      .method<bool, Comm_t, bool>("PyGIL_restore", &Comm_t::PyGIL_restore)
      .method<bool, Comm_t, const rapidjson::Value&, bool, const DIRECTION>("addSchema", &Comm_t::addSchema)
      .method<bool, Comm_t, const std::string&, bool, const DIRECTION>("addSchema", &Comm_t::addSchema)
      .method<bool, Comm_t, const Comm_t*, const DIRECTION>("copySchema", &Comm_t::copySchema)
      .method<std::string, Comm_t>("getOppAddress", &Comm_t::getOppAddress)
      .method<COMM_TYPE, Comm_t>("getOppCommType", &Comm_t::getOppCommType)
      .method<void, Comm_t>("setOppEnv", &Comm_t::setOppEnv)
      .method<void, Comm_t>("unsetOppEnv", &Comm_t::unsetOppEnv)
      .method<bool, Comm_t, Comm_t*, Comm_t*>("afterSendRecv", &Comm_t::afterSendRecv)
      .method<bool, Comm_t, std::string&>("genMetadata", &Comm_t::genMetadata)
      .method<Comm_t*, Comm_t>("getGlobalComm", &Comm_t::getGlobalComm);
    mod.add_type<WrapComm>("YggComm", jlcxx::julia_base_type<Comm_t>())
      .constructor<Comm_t*>();
}