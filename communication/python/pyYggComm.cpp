#include "pyYggComm.hpp"
#include "communicators/CommBase.hpp"
using namespace communication;
namespace bp = boost::python;

BOOST_PYTHON_MODULE(libpyygg) {
    bp::object package = bp::scope();
    package.attr("__path__") = "libpyygg";

    python::exportUtils();
    python::exportCommunicators();
}