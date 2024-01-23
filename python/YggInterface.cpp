#ifndef DOXYGEN_SHOULD_SKIP_THIS
#ifndef PY_SSIZE_T_CLEAN
#define PY_SSIZE_T_CLEAN
#endif

#ifdef YGG_PYTHON_LIBRARY_WRAP
#define YGG_MODULE_NAME "_YggInterface"
#else
#define YGG_MODULE_NAME "YggInterface"
#endif

#include <Python.h>
#ifdef YGG_LINK_PYTHON_TO_CPP
#define RAPIDJSON_FORCE_IMPORT_ARRAY
#endif
#include "utils/rapidjson_wrapper.hpp"
#include "utils.cpp"
#include "communicators.cpp"

static int
module_exec(PyObject* m)
{
    if(PyType_Ready(&Comm_tType) < 0)
        return -1;
    if (PyType_Ready(&commMetaType) < 0)
        return -1;
    Py_INCREF(&Comm_tType);
    if(PyModule_AddObject(m, "Comm_t", (PyObject*)&Comm_tType) < 0) {
        Py_DECREF(&Comm_tType);
        return -1;
    }
    Py_INCREF(&commMetaType);
    if(PyModule_AddObject(m, "CommMeta", (PyObject*)&commMetaType) < 0) {
        Py_DECREF(&commMetaType);
	return -1;
    }
    register_enums(m);
    if (register_constants(m) < 0) {
      Py_DECREF(&commMetaType);
      return -1;
    }
    return 0;
};

static struct PyModuleDef_Slot slots[] = {
  {Py_mod_exec, (void*) module_exec},
  {0, NULL}
};

static PyMethodDef functions[] = {
  {"is_comm_installed", (PyCFunction) is_comm_installed,
   METH_VARARGS, is_comm_installed_docstring},
  {NULL, NULL, 0, NULL} /* sentinel */
};

static void pyYggModule_free(void*) {
  YggInterface::communicator::ygg_cleanup();
};

static struct PyModuleDef pyYggModule {
  PyModuleDef_HEAD_INIT, /* m_base */
  YGG_MODULE_NAME,       /* m_name */
  PyDoc_STR("Python interface for Yggdrasil"),
  0,                     /* m_size */
  functions,             /* m_methods */
  slots,                 /* m_slots */
  NULL,                  /* m_traverse */
  NULL,                  /* m_clear */
  pyYggModule_free       /* m_free */
};

#ifdef YGG_PYTHON_LIBRARY_WRAP
PyMODINIT_FUNC
PyInit__YggInterface() {
#else
PyMODINIT_FUNC
PyInit_YggInterface() {
#endif // YGG_PYTHON_LIBRARY_WRAP
    if (!YggInterface::utils::import_numpy_arrays()) {
      PyErr_SetString(PyExc_ImportError, "Could not import numpy");
      return NULL;
    }
    PyObject* m = PyModuleDef_Init(&pyYggModule);
    YggInterface::communicator::ygg_init();
    return m;
}
#endif // DOXYGEN_SHOULD_SKIP_THIS
