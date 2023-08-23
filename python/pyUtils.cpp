
#include <Python.h>
#include "structmember.h"
#include "utils/enums.hpp"

// void Address_dealloc(pyAddress* self) {
//     delete self->address;
//     Py_TYPE(self)->tp_free((PyObject*) self);
// }

// int Address_init(pyAddress* self, PyObject* args, PyObject* kwds) {
//     (void)kwds;
//     PyObject *cpadr = NULL;
//     char* adr = NULL;
//     int adr_size;
//     self->address = NULL;
//     if(!PyArg_ParseTuple(args,"|z#O", &adr, &adr_size, &cpadr)) {
//         PyErr_SetString(PyExc_TypeError, "Either an address, or another Address object must be given1.");
//         return -1;
//     }
//     if(adr == NULL && cpadr == NULL){
//         PyErr_SetString(PyExc_TypeError, "Either an address, or another Address object must be given2.");
//         return -1;

//     }
//     else if (adr != NULL && cpadr != NULL) {
//         PyErr_SetString(PyExc_TypeError, "Either an address, or another Address object must be given3.");
//         return -1;
//     }
//     if (cpadr != NULL && Py_IS_TYPE(&cpadr, &AddressType)) {
//         pyAddress* temp = (pyAddress *)(cpadr);
//         self->address = new communication::utils::Address(temp->address);
//         Py_XDECREF(temp);
//     } else if (adr != NULL){
//         PySys_WriteStdout("X");
//         PySys_WriteStdout("_ %i _", adr_size);
//         PySys_WriteStdout("= %s =", adr);

//         self->address = new communication::utils::Address(adr);
//     } else {
//         PyErr_SetString(PyExc_TypeError, "Either an address, or another Address object must be given4.");
//         return -1;
//     }
//     return 0;
// }
// PyObject* Address_new(PyTypeObject *type, PyObject* args, PyObject* kwds) {
//     (void)args;
//     (void)kwds;
//     pyAddress *self;
//     self = (pyAddress*)type->tp_alloc(type, 0);
//     self->address = NULL;

//     return (PyObject*)self;
// }

// PyObject* Address_str(pyAddress* self) {
//     return PyUnicode_FromFormat("%s", self->address->address().c_str());
// }

// PyObject* Address_address(pyAddress* self, PyObject* arg) {
//     char* adr = NULL;
//     int adr_size;
//     PySys_WriteStdout("= %s =\n", (char*)arg);

//     if(!PyArg_ParseTuple(arg, "|z#", &adr, &adr_size)) {
//         PyErr_SetString(PyExc_TypeError, "Invalid argument given.");
//         return NULL;
//     }
//     PySys_WriteStdout("X");
//     if (adr) {
//         PySys_WriteStdout("X");
//         PySys_WriteStdout("_ %i _\n", adr_size);
//         PySys_WriteStdout("= %s =\n", (char*)arg);
//         self->address->address((char*)adr);
//         return NULL;
//     } else {
//         return PyUnicode_FromFormat("%s", self->address->address().c_str());
//     }
// }

// PyObject* Address_key(pyAddress* self) {
//     return PyLong_FromLong(self->address->key());
// }

// PyObject* Address_valid(pyAddress* self) {
//     if (self->address->valid())
//         Py_RETURN_TRUE;
//     Py_RETURN_FALSE;
// }

static PyObject* COMMTYPE;
static PyObject* DIRECTION_TYPE;
static PyObject* FLAG_TYPE;

static void register_enums(PyObject* module) {
    PyObject* enum_module = PyImport_ImportModule("enum");
    if(enum_module == NULL) {
        return;
    }
    PyObject* comm_types = PyDict_New();
    PyDict_SetItemString(comm_types, "NULL_COMM", PyLong_FromLong(COMM_TYPE::NULL_COMM));
    PyDict_SetItemString(comm_types, "DEFAULT_COMM", PyLong_FromLong(COMM_TYPE::DEFAULT_COMM));
    PyDict_SetItemString(comm_types, "IPC_COMM", PyLong_FromLong(COMM_TYPE::IPC_COMM));
    PyDict_SetItemString(comm_types, "ZMQ_COMM", PyLong_FromLong(COMM_TYPE::ZMQ_COMM));
    PyDict_SetItemString(comm_types, "MPI_COMM", PyLong_FromLong(COMM_TYPE::MPI_COMM));
    PyDict_SetItemString(comm_types, "SERVER_COMM", PyLong_FromLong(COMM_TYPE::SERVER_COMM));
    PyDict_SetItemString(comm_types, "CLIENT_COMM", PyLong_FromLong(COMM_TYPE::CLIENT_COMM));

    COMMTYPE = PyObject_CallMethod(enum_module, "IntEnum", "sO", "COMM_TYPE", comm_types);
    Py_CLEAR(comm_types);
    if(PyModule_AddObject(module, "COMM_TYPE", COMMTYPE) < 0)
        Py_CLEAR(COMMTYPE);

    PyObject* direction_types = PyDict_New();
    PyDict_SetItemString(direction_types, "SEND", PyLong_FromLong(DIRECTION::SEND));
    PyDict_SetItemString(direction_types, "NONE", PyLong_FromLong(DIRECTION::NONE));
    PyDict_SetItemString(direction_types, "RECV", PyLong_FromLong(DIRECTION::RECV));

    DIRECTION_TYPE = PyObject_CallMethod(enum_module, "IntEnum", "sO", "DIRECTION", direction_types);
    Py_CLEAR(direction_types);
    if(PyModule_AddObject(module, "DIRECTION", DIRECTION_TYPE) < 0)
        Py_CLEAR(DIRECTION_TYPE);

    PyObject* flag_types = PyDict_New();
#define add_flag(name)					\
    PyDict_SetItemString(flag_types, #name, PyLong_FromLong(CommFlags::name))
    add_flag(COMM_FLAG_VALID);
    add_flag(COMM_FLAG_GLOBAL);
    add_flag(COMM_FLAG_WORKER);
    add_flag(COMM_FLAG_CLIENT);
    add_flag(COMM_FLAG_SERVER);
    add_flag(COMM_FLAG_CLIENT_RESPONSE);
    add_flag(COMM_FLAG_SERVER_RESPONSE);
    add_flag(COMM_ALWAYS_SEND_HEADER);
    add_flag(COMM_ALLOW_MULTIPLE_COMMS);
    add_flag(COMM_FLAGS_USED_SENT);
    add_flag(COMM_FLAGS_USED_RECV);
    add_flag(COMM_EOF_SENT);
    add_flag(COMM_EOF_RECV);
    add_flag(COMM_FLAG_INTERFACE);
    // add_flag(COMM_FLAG_DELETE); Not needed by python
    add_flag(COMM_FLAG_ASYNC);
    // add_flag(COMM_FLAG_ASYNC_WRAPPED); Not needed by python
#undef add_flag

    FLAG_TYPE = PyObject_CallMethod(enum_module, "IntEnum", "sO", "COMM_FLAGS", flag_types);
    Py_CLEAR(flag_types);
    if(PyModule_AddObject(module, "COMM_FLAGS", FLAG_TYPE) < 0)
      Py_CLEAR(FLAG_TYPE);
}
