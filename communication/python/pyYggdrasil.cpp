#ifndef PY_SSIZE_T_CLEAN
#define PY_SSIZE_T_CLEAN
#endif
#include <Python.h>
#include "pyUtils.hpp"
#include "pyYggCommBase.hpp"

static struct PyModuleDef pyYggModule {
        PyModuleDef_HEAD_INIT,
        "pyYggdrasil",
        "",
        -1,
        nullptr,
        nullptr,
        nullptr,
        nullptr,
        nullptr
};

PyMODINIT_FUNC
PyInit_pyYggdrasil(void) {
    PyObject* m;
    if(PyType_Ready(&AddressType) < 0)
        return nullptr;
    if(PyType_Ready(&Comm_tType) < 0)
        return nullptr;
    m = PyModule_Create(&pyYggModule);
    if (m == nullptr)
        return nullptr;
    Py_INCREF(&AddressType);
    if(PyModule_AddObject(m, "Address", (PyObject*)&AddressType) < 0) {
        Py_DECREF(&AddressType);
        Py_DECREF(m);
        return nullptr;
    }
    Py_INCREF(&Comm_tType);
    if(PyModule_AddObject(m, "Address", (PyObject*)&Comm_tType) < 0) {
        Py_DECREF(&Comm_tType);
        Py_DECREF(m);
        return nullptr;
    }
    register_enums(m);
    return m;
}