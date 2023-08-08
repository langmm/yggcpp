#ifndef PY_SSIZE_T_CLEAN
#define PY_SSIZE_T_CLEAN
#endif

#include <Python.h>
#define RAPIDJSON_FORCE_IMPORT_ARRAY
#include "rapidjson/pyrj.h"
#include "pyUtils.cpp"
#include "pyYggCommBase.cpp"

static int
module_exec(PyObject* m)
{
    if(PyType_Ready(&Comm_tType) < 0)
        return -1;
    Py_INCREF(&Comm_tType);
    if(PyModule_AddObject(m, "CommBase", (PyObject*)&Comm_tType) < 0) {
        Py_DECREF(&Comm_tType);
        Py_DECREF(m);
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
  {NULL, NULL, 0, NULL} /* sentinel */
};

static struct PyModuleDef pyYggModule {
  PyModuleDef_HEAD_INIT, /* m_base */
  "pyYggdrasil",         /* m_name */
  PyDoc_STR("Python interface for Yggdrasil"),
  0,                     /* m_size */
  functions,             /* m_methods */
  slots,                 /* m_slots */
  NULL,                  /* m_traverse */
  NULL,                  /* m_clear */
  NULL                   /* m_free */
};

PyMODINIT_FUNC
PyInit_pyYggdrasil() {
    import_array();
    // import_umath();
    PyObject* m = PyModuleDef_Init(&pyYggModule);
    return m;
}
