#include <boost/python.hpp>
#include "utils/Address.hpp"
#include "python/pyYggComm.hpp"

namespace bp = boost::python;
namespace communication {
void (utils::Address::*setaddress)(const std::string &) = &utils::Address::address;
const std::string &(utils::Address::*getaddress)() const = &utils::Address::address;

void python::exportUtils() {
    bp::object modModule(bp::handle<>(bp::borrowed(PyImport_AddModule("pyYggdrasil.utils"))));
    bp::scope().attr("utils") = modModule;
    bp::scope modScope = modModule;
    bp::class_<utils::Address>("Address", bp::init<std::string>())
            .def(bp::init<utils::Address *>())
            .def("__str__", &utils::Address::print)
            .def("address", setaddress)
            .def("address", getaddress, bp::return_value_policy<bp::copy_const_reference>())
            .def("key", &utils::Address::key)
            .def("valid", &utils::Address::valid);
    }
}
