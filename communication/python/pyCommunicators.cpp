#include <boost/python.hpp>
#include "python/pyYggComm.hpp"
#include "communicators/comms.hpp"
#include "communicators/DefaultComm.hpp"

namespace bp = boost::python;
namespace communication {

void python::exportCommunicators() {
    bp::object modModule(bp::handle<>(bp::borrowed(PyImport_AddModule("pyYggComm.communicators"))));
    bp::scope().attr("module") = modModule;
    bp::scope modScope = modModule;

    bp::class_<CommWrap, boost::noncopyable>("Comm_t", bp::init<utils::Address*, DIRECTION, const COMM_TYPE, int>())
            .def(bp::init<const std::string&, DIRECTION, const COMM_TYPE>())
            .def("comm_nmsg", bp::pure_virtual(&communicator::Comm_t::comm_nmsg))
            .def("getType", &communicator::Comm_t::getType)
            .def("valid", &communicator::Comm_t::valid);

    bp::class_<communicator::IPCComm, bp::bases<CommWrap>, boost::noncopyable>("IPCComm", bp::init<const std::string&, utils::Address*, DIRECTION>()[bp::with_custodian_and_ward_postcall<0,2>()])
            .def("check_channels", &communicator::IPCComm::check_channels)
            .def("add_channel", &communicator::IPCComm::add_channel)
            .def("remove_comm", &communicator::IPCComm::remove_comm)
            .def("comm_nmsg", &communicator::IPCComm::comm_nmsg);

    bp::class_<communicator::MPIComm, bp::bases<CommWrap>, boost::noncopyable>("MPIComm", bp::init<const std::string&, utils::Address*, DIRECTION>()[bp::with_custodian_and_ward_postcall<0,2>()])
            .def("comm_nmsg", &communicator::MPIComm::comm_nmsg)
            .def("mpi_comm_source_id", &communicator::MPIComm::mpi_comm_source_id);

    bp::class_<communicator::ZMQComm, bp::bases<CommWrap>, boost::noncopyable>("ZMQComm", bp::init<const std::string&, utils::Address*, DIRECTION>()[bp::with_custodian_and_ward_postcall<0,2>()])
            .def("comm_nmsg", &communicator::MPIComm::comm_nmsg);

    bp::class_<communicator::ClientComm, bp::bases<CommWrap,communicator::COMM_BASE>, boost::noncopyable>("ClientComm", bp::init<const std::string&, utils::Address*>()[bp::with_custodian_and_ward_postcall<0,2>()])
            .def("has_request", &communicator::ClientComm::has_request)
            .def("has_response", &communicator::ClientComm::has_response)
            .def("add_request", &communicator::ClientComm::add_request)
            .def("add_response", &communicator::ClientComm::add_response)
            .def("remove_request", &communicator::ClientComm::remove_request)
            .def("pop_response", &communicator::ClientComm::pop_response)
            .def("new_address", &communicator::ClientComm::new_address)
            .def("comm_nmsg", &communicator::ClientComm::comm_nmsg);

    int (communicator::ServerComm::*hc1)(const std::string&) const = &communicator::ServerComm::has_comm;
    int (communicator::ServerComm::*hc2)(const utils::Address*) const = &communicator::ServerComm::has_comm;
    int (communicator::ServerComm::*ac1)(std::string&) = &communicator::ServerComm::add_comm;
    int (communicator::ServerComm::*ac2)(utils::Address*) = &communicator::ServerComm::add_comm;


    bp::class_<communicator::ServerComm, bp::bases<CommWrap,communicator::COMM_BASE>, boost::noncopyable>("ClientComm", bp::init<const std::string&, utils::Address*>()[bp::with_custodian_and_ward_postcall<0,2>()])
            .def("has_request", &communicator::ServerComm::has_request)
            .def("has_response", &communicator::ServerComm::has_response)
            .def("add_request", &communicator::ServerComm::add_request)
            .def("remove_request", &communicator::ServerComm::remove_request)
            .def("get_comm", &communicator::ServerComm::get_comm, bp::return_value_policy<bp::reference_existing_object>())
            .def("has_comm", hc1)
            .def("has_comm", hc2)
            .def("add_comm", ac1)
            .def("add_comm", ac2)
            .def("comm_nmsg", &communicator::ServerComm::comm_nmsg);

}
}