#ifndef PY_SSIZE_T_CLEAN
#define PY_SSIZE_T_CLEAN
#endif

#ifdef YGG_PYTHON_LIBRARY_WRAP
#define YGG_MODULE_NAME "_pyYggdrasil"
#else
#define YGG_MODULE_NAME "pyYggdrasil"
#endif

#include <Python.h>
#ifndef WRAP_RAPIDJSON_FOR_DLL
#define RAPIDJSON_FORCE_IMPORT_ARRAY
#endif
#include "utils/rapidjson_wrapper.hpp"
#include "pyUtils.cpp"
#include "pyYggCommBase.cpp"

static int
module_exec(PyObject* m)
{
    if(PyType_Ready(&Comm_tType) < 0)
        return -1;
    if (PyType_Ready(&commMetaType) < 0)
        return -1;
    Py_INCREF(&Comm_tType);
    if(PyModule_AddObject(m, "CommBase", (PyObject*)&Comm_tType) < 0) {
        Py_DECREF(&Comm_tType);
        return -1;
    }
    Py_INCREF(&commMetaType);
    if(PyModule_AddObject(m, "CommMeta", (PyObject*)&commMetaType) < 0) {
        Py_DECREF(&commMetaType);
	return -1;
    }
    register_enums(m);
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
  communication::communicator::Comm_t::_ygg_cleanup();
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
PyInit__pyYggdrasil() {
#else
PyMODINIT_FUNC
PyInit_pyYggdrasil() {
#endif // YGG_PYTHON_LIBRARY_WRAP
    try {
      rapidjson::init_numpy_API();
    } catch (std::exception& e) {
      PyErr_SetString(PyExc_TypeError, e.what());
      return NULL;
    }
    PyObject* m = PyModuleDef_Init(&pyYggModule);
    communication::communicator::Comm_t::_ygg_init();
    return m;
}
