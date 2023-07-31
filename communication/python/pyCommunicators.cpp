#include <boost/python.hpp>
#include "python/pyYggCommBase.hpp"
#include "communicators/comms.hpp"
#include "communicators/DefaultComm.hpp"

namespace bp = boost::python;
namespace communication {

int (communicator::Comm_t::*strsend)(const std::string&) = &communicator::Comm_t::send;
long (communicator::Comm_t::*strrecv)(std::string&) = &communicator::Comm_t::recv;

void python::exportCommunicators() {
    bp::object modModule(bp::handle<>(bp::borrowed(PyImport_AddModule("pyYggdrasil.communicators"))));
    bp::scope().attr("communicators") = modModule;
    bp::scope modScope = modModule;

    bp::class_<CommWrap, boost::noncopyable>("Comm_t", bp::init<utils::Address*, DIRECTION, const COMM_TYPE&, int>())
            .def(bp::init<const std::string&, DIRECTION, const COMM_TYPE&, int>())
            .def("comm_nmsg", bp::pure_virtual(&communicator::Comm_t::comm_nmsg))
            .def("getType", &communicator::Comm_t::getType)
            .def("valid", &communicator::Comm_t::valid)
            .def("send", strsend)
            .def("send_eof", &communicator::Comm_t::send_eof)
            .def("recv", strrecv);

    bp::class_<communicator::IPCComm, bp::bases<CommWrap>, boost::noncopyable>("IPCComm", bp::init<const std::string&, utils::Address*, DIRECTION>()[bp::with_custodian_and_ward_postcall<0,2>()])
            //.def("check_channels", &communicator::IPCComm::check_channels)
            .def("add_channel", &communicator::IPCComm::add_channel)
            .def("remove_comm", &communicator::IPCComm::remove_comm)
            .def("comm_nmsg", &communicator::IPCComm::comm_nmsg);

    bp::class_<communicator::MPIComm, bp::bases<CommWrap>, boost::noncopyable>("MPIComm", bp::init<const std::string&, utils::Address*, DIRECTION>()[bp::with_custodian_and_ward_postcall<0,2>()])
            .def("comm_nmsg", &communicator::MPIComm::comm_nmsg)
            .def("mpi_comm_source_id", &communicator::MPIComm::mpi_comm_source_id);

    bp::class_<communicator::ZMQComm, bp::bases<CommWrap>, boost::noncopyable>("ZMQComm", bp::init<const std::string&, utils::Address*, DIRECTION>()[bp::with_custodian_and_ward_postcall<0,2>()])
            .def("comm_nmsg", &communicator::MPIComm::comm_nmsg);

    bp::class_<communicator::ClientComm, bp::bases<communicator::COMM_BASE>, boost::noncopyable>("ClientComm", bp::init<const std::string&, utils::Address*, int>()[bp::with_custodian_and_ward_postcall<0,2>()])
            .def(bp::init<const std::string, int>())
            .def("set_timeout_recv", &communicator::ClientComm::set_timeout_recv)
            .def("wait_for_recv", &communicator::ClientComm::wait_for_recv)
            .def("comm_nmsg", &communicator::ClientComm::comm_nmsg);

    bp::class_<communicator::ServerComm, bp::bases<communicator::COMM_BASE>, boost::noncopyable>("ServerComm", bp::init<const std::string&, utils::Address*, int>()[bp::with_custodian_and_ward_postcall<0,2>()])
            .def(bp::init<const std::string, int>())
            .def("comm_nmsg", &communicator::ServerComm::comm_nmsg);

}
}