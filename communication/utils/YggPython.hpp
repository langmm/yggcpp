#pragma once

#ifdef _DEBUG
#undef _DEBUG
#include <Python.h>
#include <numpy/arrayobject.h>
#include <numpy/ndarrayobject.h>
#include <numpy/npy_common.h>
#define _DEBUG
#else

#include <Python.h>
#include <numpy/arrayobject.h>
#include <numpy/ndarrayobject.h>
#include <numpy/npy_common.h>

#endif

namespace communication {
namespace utils {
/*!
  @brief Initialize Numpy arrays if it is not initalized.
  @returns int 0 if successful, other values indicate errors.
 */
static inline
int init_numpy_API() {
    int out = 0;
#ifdef _OPENMP
#pragma omp critical (numpy)
    {
#endif
        if (PyArray_API == NULL) {
            if (_import_array() < 0) {
                PyErr_Print();
                out = -2;
            }
        }
#ifdef _OPENMP
    }
#endif
    return out;
}


/*!
  @brief Initialize Python if it is not initialized.
  @returns int 0 if successful, other values indicate errors.
 */
static inline
int init_python_API() {
    int out = 0;
#ifdef _OPENMP
#pragma omp critical (python)
    {
#endif
        if (!(Py_IsInitialized())) {
            char *name = getenv("YGG_PYTHON_EXEC");
            if (name != NULL) {
                wchar_t *wname = Py_DecodeLocale(name, NULL);
                if (wname == NULL) {
                    printf("Error decoding YGG_PYTHON_EXEC\n");
                    out = -1;
                } else {
                    Py_SetProgramName(wname);
                    PyMem_RawFree(wname);
                }
            }
            if (out >= 0) {
                Py_Initialize();
                if (!(Py_IsInitialized()))
                    out = -1;
            }
        }
        if (out >= 0) {
            out = init_numpy_API();
        }
#ifdef _OPENMP
    }
#endif
    return out;
}


}
}