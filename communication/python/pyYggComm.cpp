#include "pyYggComm.hpp"
#include "communicators/CommBase.hpp"
using namespace communication;
namespace bp = boost::python;

BOOST_PYTHON_MODULE(libpyygg) {
    bp::object package = bp::scope();
    package.attr("__path__") = "libpyygg";

    bp::enum_<DIRECTION>("DIRECTION")
            .value("SEND", SEND)
            .value("NONE", NONE)
            .value("RECV", RECV);
    bp::enum_<COMM_TYPE>("COMM_TYPE")
            .value("NULL_COMM", NULL_COMM)
            .value("IPC_COMM", IPC_COMM)
            .value("ZMQ_COMM", ZMQ_COMM)
            .value("SERVER_COMM", SERVER_COMM)
            .value("CLIENT_COMM", CLIENT_COMM)
            .value("MPI_COMM", MPI_COMM);


    python::exportUtils();
    python::exportCommunicators();
}