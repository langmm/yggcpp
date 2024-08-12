
// LINES AFTER THIS WERE GENERATED AND SHOULD NOT BE MODIFIED DIRECTLY
//====================================================================
#include "jlcxx/jlcxx.hpp"
#include "communicators/CommBase.hpp"
JLCXX_MODULE define_communicator_module(jlcxx::Module& mod)
{
using namespace YggInterface::communicator;
mod.add_type<CommBase>("CommBase")
    .method("send_raw", &CommBase::send_raw)
    .method("send", &CommBase::send)
    .method("send", &CommBase::send)
    .method("set_timeout_recv", &CommBase::set_timeout_recv)
    .method("get_timeout_recv", &CommBase::get_timeout_recv)
    .method("wait_for_recv", &CommBase::wait_for_recv)
    .method("recv_raw", &CommBase::recv_raw)
    .method("recv", &CommBase::recv)
    .method("recv", &CommBase::recv)
    .method("recvRealloc", &CommBase::recvRealloc)
    .method("send", &CommBase::send)
    .method("call", &CommBase::call)
    .method("callRealloc", &CommBase::callRealloc)
    .method("vRecv", &CommBase::vRecv)
    .method("vSend", &CommBase::vSend)
    .method("call", &CommBase::call)
    .method("vCall", &CommBase::vCall)
    .method("nmsg", &CommBase::nmsg)
    .method("open", &CommBase::open)
    .method("close", &CommBase::close)
    .method("is_closed", &CommBase::is_closed)
    .method("getMetadata", &CommBase::getMetadata)
    .method("logClass", &CommBase::logClass)
    .method("logInst", &CommBase::logInst)
    .method("setLanguage", &CommBase::setLanguage)
    .method("PyGIL_release", &CommBase::PyGIL_release)
    .method("PyGIL_restore", &CommBase::PyGIL_restore)
    .method("addSchema", &CommBase::addSchema)
    .method("addSchema", &CommBase::addSchema)
    .method("addSchema", &CommBase::addSchema)
    .method("copySchema", &CommBase::copySchema)
    .method("create_worker", &CommBase::create_worker)
    .method("create_worker_send", &CommBase::create_worker_send)
    .method("create_worker_recv", &CommBase::create_worker_recv)
    .method("send_single", &CommBase::send_single)
    .method("recv_single", &CommBase::recv_single);
}