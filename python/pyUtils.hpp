#pragma once
#ifndef PY_SSIZE_T_CLEAN
#define PY_SSIZE_T_CLEAN
#endif

#include <Python.h>
#include "utils/Address.hpp"

static PyObject* COMMTYPE;
static PyObject* DIRECTION_TYPE;


// typedef struct {
//     PyObject_HEAD
//     YggInterface::utils::Address *address;
// }pyAddress;

// void Address_dealloc(pyAddress* self);
// int Address_init(pyAddress* self, PyObject* args, PyObject* kwds);
// PyObject* Address_new(PyTypeObject *type, PyObject* args, PyObject* kwds);
// PyObject* Address_str(pyAddress* self);
// PyObject* Address_address(pyAddress* self, PyObject* arg);
// PyObject* Address_key(pyAddress* self);
// PyObject* Address_valid(pyAddress* self);

// static PyMethodDef Address_methods[] = {
//         {"address", (PyCFunction)Address_address, METH_VARARGS, "Get or set the address"},
//         {"key", (PyCFunction)(void(*)(void))Address_key, METH_NOARGS, "Get the key of this instance"},
//         {"valid", (PyCFunction)(void(*)(void))Address_valid, METH_NOARGS, "Get the validity of this instance"},
//         {NULL, NULL, 0, ""}  /* Sentinel */
// };

// PyTypeObject AddressType = {
//         PyVarObject_HEAD_INIT(NULL, 0)
//         "pyYggdrasil.Address",     /* tp_name */
//         sizeof(pyAddress),         /* tp_basicsize */
//         0,                         /* tp_itemsize */
//         (destructor)Address_dealloc, /* tp_dealloc */
//         0,                         /* tp_vectorcall_offset */
//         0,                         /* tp_getattr */
//         0,                         /* tp_setattr */
//         0,                         /* tp_reserved */
//         (reprfunc)Address_str,     /* tp_repr */
//         0,                         /* tp_as_number */
//         0,                         /* tp_as_sequence */
//         0,                         /* tp_as_mapping */
//         0,                         /* tp_hash  */
//         0,                         /* tp_call */
//         (reprfunc)Address_str,     /* tp_str */
//         0,                         /* tp_getattro */
//         0,                         /* tp_setattro */
//         0,                         /* tp_as_buffer */
//         Py_TPFLAGS_DEFAULT,        /* tp_flags */
//         "Address object",          /* tp_doc */
//         0,                         /* tp_traverse */
//         0,                         /* tp_clear */
//         0,                         /* tp_richcompare */
//         0,                         /* tp_weaklistoffset */
//         0,                         /* tp_iter */
//         0,                         /* tp_iternext */
//         Address_methods,           /* tp_methods */
//         0,                         /* tp_members */
//         0,                         /* tp_getset */
//         0,                         /* tp_base */
//         0,                         /* tp_dict */
//         0,                         /* tp_descr_get */
//         0,                         /* tp_descr_set */
//         0,                         /* tp_dictoffset */
//         (initproc)Address_init,    /* tp_init */
//         0,                         /* tp_alloc */
//         Address_new,               /* tp_new */
//         0,                         /* tp_free */
//         NULL,                      /* tp_is_gc */
//         NULL,                      /* tp_bases */
//         NULL,                      /* tp_mro */
//         NULL,                      /* tp_cache */
//         NULL,                      /* tp_subclasses */
//         NULL,                      /*tp_weaklist */
//         0,                         /* tp_del */
//         0,                         /* tp_version_tag */
//         0,                         /* tp_finalize */
//         0                          /*tp_vectorcall */
// };

void register_enums(PyObject* module);
