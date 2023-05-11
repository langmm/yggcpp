#include "pyYggComm.hpp"
#include "communicators/CommBase.hpp"
using namespace communication;
namespace bp = boost::python;

BOOST_PYTHON_MODULE(pyYggdrasil) {
    bp::object package = bp::scope();
    package.attr("__path__") = "pyYggdrasil";

    python::exportUtils();
    python::exportCommunicators();
}