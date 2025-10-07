
#include <Python.h>
#include "structmember.h"
#include "utils/enums.hpp"
#include "utils/enums_utils.hpp"
#include "utils/constants.hpp"

static PyObject* COMMTYPE;
static PyObject* DIRECTION_TYPE;
static PyObject* COMM_FLAG_TYPE;
static PyObject* FILE_FLAG_TYPE;

static int register_constants(PyObject* module) {
  PyObject* iobj = NULL;
#define ADD_CONSTANT(name)					\
  if (iobj == NULL) {						\
    return -1;							\
  }								\
  if (PyModule_AddObject(module, name, iobj) < 0) {		\
    Py_CLEAR(iobj);						\
    return -1;							\
  }								\
  iobj = NULL
#define ADD_CONSTANT_STRING(name)				\
  iobj = PyUnicode_FromString(name);				\
  ADD_CONSTANT(#name)
#define ADD_CONSTANT_LONG(name)					\
  iobj = PyLong_FromLong(name);					\
  ADD_CONSTANT(#name)
#define ADD_CONSTANT_LONGLONG(name)				\
  iobj = PyLong_FromLongLong(name);				\
  ADD_CONSTANT(#name)
  ADD_CONSTANT_LONG(YGG_MSG_MAX);
  ADD_CONSTANT_STRING(YGG_MSG_EOF);
  // ADD_CONSTANT_LONG(YGG_MSG_EOF_LEN);
  ADD_CONSTANT_STRING(YGG_CLIENT_EOF);
  ADD_CONSTANT_STRING(YGG_CLIENT_SIGNON);
  // ADD_CONSTANT_LONG(YGG_CLIENT_SIGNON_LEN);
  ADD_CONSTANT_STRING(YGG_SERVER_SIGNON);
  // ADD_CONSTANT_LONG(YGG_SERVER_SIGNON_LEN);
  ADD_CONSTANT_LONG(YGG_MSG_BUF);
  ADD_CONSTANT_LONG(YGG_SLEEP_TIME);
  ADD_CONSTANT_LONGLONG(YGG_MAX_TIME);
  // ADD_CONSTANT_LONG(PYTHON_NAME_SIZE);
  ADD_CONSTANT_LONG(PSI_MSG_MAX);
  ADD_CONSTANT_LONG(PSI_MSG_BUF);
  ADD_CONSTANT_STRING(PSI_MSG_EOF);
  ADD_CONSTANT_STRING(MSG_HEAD_SEP);
  // ADD_CONSTANT_LONG(COMMBUFFSIZ);
  // ADD_CONSTANT_LONG(FMT_LEN);
  // ADD_CONSTANT_LONG(MAX_KEYS_ALLOWED);
#undef ADD_CONSTANT_LONG
#undef ADD_CONSTANT_STRING
#undef ADD_CONSTANT
  return 0;
};

template<typename T>
static int register_enum(PyObject* dict,
			 const std::map<const T, const std::string>& map,
			 const std::string& prefix="",
			 const std::string& suffix="",
			 bool to_lower=false, bool to_upper=false) {
  if (dict == NULL)
    return -1;
  for (typename std::map<const T, const std::string>::const_iterator it = map.cbegin();
       it != map.cend(); it++) {
    std::string tmp = it->second;
    if (to_lower) {
      tmp = YggInterface::utils::str_tolower(tmp);
    } else if (to_upper) {
      tmp = YggInterface::utils::str_toupper(tmp);
    }
    std::string iname = prefix + tmp + suffix;
    PyObject* ival = PyLong_FromLong(it->first);
    if (PyDict_SetItemString(dict, iname.c_str(), ival) < 0)
      return -1;
    Py_DECREF(ival);
  }
  return 0;
}

static int register_enums(PyObject* module) {
    PyObject* enum_module = PyImport_ImportModule("enum");
    if(enum_module == NULL) {
      return -1;
    }
#define REGISTER_ENUM(name, var, map, prefix, suffix)			\
    {									\
      PyObject* tmp = PyDict_New();					\
      if (register_enum(tmp, YggInterface::utils::map(), prefix, suffix) < 0) { \
	Py_DECREF(tmp);							\
	return -1;							\
      }									\
      var = PyObject_CallMethod(enum_module, "IntEnum", "sO",		\
				#name, tmp);				\
      Py_CLEAR(tmp);							\
      if (!var) {							\
	return -1;							\
      }									\
      if (PyModule_AddObject(module, #name, var) < 0) {			\
	Py_CLEAR(var);							\
	return -1;							\
      }									\
    }
    REGISTER_ENUM(COMM_TYPE, COMMTYPE, COMM_TYPE_map, "", "_COMM");
    REGISTER_ENUM(DIRECTION, DIRECTION_TYPE, DIRECTION_map, "", "");
    REGISTER_ENUM(COMM_FLAG, COMM_FLAG_TYPE, COMM_FLAG_map, "COMM_FLAG_", "");
    REGISTER_ENUM(FILE_FLAG, FILE_FLAG_TYPE, FILE_FLAG_map, "FILE_FLAG_", "");
#undef REGISTER_ENUM
    {
      PyObject* tmp = PyDict_New();
      if (register_enum(tmp, YggInterface::utils::COMM_TYPE_map(),
			"", "", true) < 0) {
	Py_CLEAR(tmp);
	return -1;
      }
      if (PyModule_AddObject(module, "COMM_TYPE_map", tmp) < 0) {
	Py_CLEAR(tmp);
	return -1;
      }
    }
    return 0;
}
