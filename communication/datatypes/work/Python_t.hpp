#pragma once
#include <string>
#include <Python.h>
#include "Metaschema/YggGeneric.hpp"
/*! @brief Size for buffers to contain names of Python objects. */
#define PYTHON_NAME_SIZE 1000

namespace communication {
namespace datatypes {

class Python_t {
public:
    Python_t();
    Python_t(const Python_t* src);
    void display();

    ~Python_t();
    std::string name; //!<Name of the Python class/type/function.
    Metaschema::YggGeneric *args; //!< Arguments used in creating a Python instance.
    Metaschema::YggGeneric *kwargs; //!< Keyword arguments used in creating a Python instance.
    PyObject *obj; //!< Python object.
};

typedef Python_t python_t;
}
}