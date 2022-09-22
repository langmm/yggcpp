#include "Python_t.hpp"
#include "utils/logging.hpp"
using namespace communication::datatypes;
using namespace communication::datatypes::Metaschema;

Python_t::Python_t() {
    name[0] = '\0';
    args = nullptr;
    kwargs = nullptr;
    obj = nullptr;
}

Python_t::Python_t(const Python_t* src) : Python_t() {
    name = src->name;
    if (src->args != nullptr)
        args = new YggGeneric(src->args);

    if (src->kwargs != nullptr) {
        kwargs = new YggGeneric(src->kwargs);
    }
    if (src->obj != nullptr) {
        // Increment reference count for underlying Python object
        obj = Py_BuildValue("O", src->obj);
    }

}

void Python_t::display() {
    if (obj != nullptr) {
        if (PyObject_Print_STDOUT(obj) < 0) {
            utils::ygglog_throw_error("display_python: Failed to print the Python object.");
        }
    }

}

Python_t::~Python_t() {
    if (args != nullptr) {
        delete args;
        args = nullptr;
    }
    if (kwargs != nullptr) {
        delete kwargs;
        kwargs = nullptr;
    }
    if (obj != nullptr) {
        Py_DECREF(obj);
        obj = nullptr;
    }
}
