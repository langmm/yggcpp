#pragma once
#ifndef PY_SSIZE_T_CLEAN
#define PY_SSIZE_T_CLEAN
#endif

#include <Python.h>
#include "utils/Address.hpp"

static PyObject* COMMTYPE;
static PyObject* DIRECTION_TYPE;

void register_enums(PyObject* module);
