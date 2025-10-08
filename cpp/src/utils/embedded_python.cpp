#include "utils/embedded_python.hpp"

using namespace YggInterface::utils;

#ifdef YGG_EMBED_PYTHON

#define YGG_EMBED_PYTHON_GIL_BEGIN YGGDRASIL_PYGIL_BEGIN
#define YGG_EMBED_PYTHON_GIL_END YGGDRASIL_PYGIL_END

/*
#ifdef RAPIDJSON_YGGDRASIL_PYTHON
#define YGG_EMBED_PYTHON_GIL_BEGIN		\
  PyGILState_STATE gstate;			\
  bool _prev_with_gil = _with_gil;		\
  if (!_prev_with_gil) {			\
    gstate = PyGILState_Ensure();		\
    _with_gil = true;				\
  }
#define YGG_EMBED_PYTHON_GIL_END		\
  if (!_prev_with_gil) {			\
    PyGILState_Release(gstate);			\
    _with_gil = false;				\
  }
#else
#define YGG_EMBED_PYTHON_GIL_BEGIN
#define YGG_EMBED_PYTHON_GIL_END
#endif
*/

EMBEDED_LANGUAGE_DEFN_ENABLED(EmbeddedPython, PyObject*, PYTHON_LANGUAGE, .py)
int64_t EmbeddedPython::commFlags() const {
  return (EmbeddedLanguageBase::commFlags() | COMM_FLAG_REQUIRES_PYGIL);
}
bool EmbeddedPython::initialize_main() {
  return utils::initialize_python("EmbeddedPython::initialize");
}
bool EmbeddedPython::finalize_main() {
  return utils::finalize_python("EmbeddedPython::finalize");
}
bool EmbeddedPython::get_error(std::string& message) const {
  bool out = false;
  PyObject *exception = NULL, *message_str = NULL;
  YGG_EMBED_PYTHON_GIL_BEGIN;
  exception = PyErr_Occurred();
  if (!exception)
    goto cleanup;
  message_str = PyObject_Str(exception);
  if (!message_str) {
    log_error() << "get_error: Error converting Python exception to a string" << std::endl;
    goto cleanup;
  } else {
    message = PyUnicode_AsUTF8(message_str);
    Py_CLEAR(message_str);
  }
  out = true;
 cleanup:
  Py_CLEAR(message_str);
  YGG_EMBED_PYTHON_GIL_END;
  return out;
}
bool EmbeddedPython::preserve_embedded(void*& x) const {
  if (x) {
    YGG_EMBED_PYTHON_GIL_BEGIN;
    PyObject* var = (PyObject*)x;
    Py_INCREF(var);
    YGG_EMBED_PYTHON_GIL_END;
  }
  return true;
}
bool EmbeddedPython::free_embedded(void*& x) const {
  if (x) {
    YGG_EMBED_PYTHON_GIL_BEGIN;
    PyObject* var = (PyObject*)x;
    Py_CLEAR(var);
    x = nullptr;
    YGG_EMBED_PYTHON_GIL_END;
  }
  return true;
}
std::string EmbeddedPython::string_embedded(const void* x) const {
  std::string out;
  if (x) {
    YGG_EMBED_PYTHON_GIL_BEGIN;
    PyObject* py_string = PyObject_Str((PyObject*)x);
    if (!py_string) {
      log_error() << "string_embedded: Failed to call string on Python object" << std::endl;
    } else {
      out = PyUnicode_AsUTF8(py_string);
      Py_CLEAR(py_string);
    }
    YGG_EMBED_PYTHON_GIL_END;
  }
  return out;
}
bool EmbeddedPython::eval(const std::string& expr,
			  rapidjson::Document& result) const {
  PyObject *expr_py = NULL, *globals = NULL, *locals = NULL,
    *result_ = NULL;
  bool out = false;
  YGG_EMBED_PYTHON_GIL_BEGIN;
  expr_py = PyUnicode_FromString(expr.c_str());
  result_ = PyEval_EvalCode(expr_py, globals, locals);
  Py_CLEAR(expr_py);
  Py_CLEAR(globals);
  Py_CLEAR(locals);
  if (check_error("eval"))
    goto cleanup;
  if (!result.SetPythonObjectRaw(result_, result.GetAllocator()))
    goto cleanup;
  out = true;
 cleanup:
  Py_CLEAR(result_);
  YGG_EMBED_PYTHON_GIL_END;
  return out;
}
bool EmbeddedPython::convert_to(const rapidjson::Value& v_in,
				void*& v_out0, bool) const {
  YGG_EMBED_PYTHON_GIL_BEGIN;
  v_out0 = NULL;
  PyObject* v_out = v_in.GetPythonObjectRaw();
  if (check_error("convert_to") || (!v_out)) return false;
  v_out0 = (void*)v_out;
  YGG_EMBED_PYTHON_GIL_END;
  return true;
}
bool EmbeddedPython::convert_from(const void*& v_in0,
				  rapidjson::Value& v_out,
				  rapidjson::Value::AllocatorType& allocator) const {
  YGG_EMBED_PYTHON_GIL_BEGIN;
  bool out = false;
  PyObject* v_in = (PyObject*)v_in0;
  if (!v_out.SetPythonObjectRaw(v_in, allocator))
    goto cleanup;
  out = true;
 cleanup:
  if (check_error("convert_from")) return false;
  YGG_EMBED_PYTHON_GIL_END;
  return out;
}
void* EmbeddedPython::load_function(const std::string& name) const {
  YGG_EMBED_PYTHON_GIL_BEGIN;
  void* out = utils::import_python_object(name.c_str());
  if (check_error("load_function"))
    out = NULL;
  YGG_EMBED_PYTHON_GIL_END;
  return out;
}
bool EmbeddedPython::call_function(void* func,
				   const rapidjson::Document& args,
				   rapidjson::Document& result) const {
  bool out = false;
  void* args_list = NULL;
  PyObject *args_ = NULL, *kwargs = NULL, *result_ = NULL,
    *args_list_py = NULL;
  YGG_EMBED_PYTHON_GIL_BEGIN;
  if ((!func) || (!convert_to(args, args_list)))
    goto cleanup;
  if (!args_list)
    goto cleanup;
  args_list_py = (PyObject*)args_list;
  if (PyList_Check(args_list_py))
    args_ = PyList_AsTuple(args_list_py);
  else
    args_ = PyTuple_Pack(1, args_list_py);
  if (!args_)
    goto cleanup;
  result_ = PyObject_Call((PyObject*)func, args_, kwargs);
  if (!result_)
    goto cleanup;
  if (!result.SetPythonObjectRaw(result_, result.GetAllocator()))
    goto cleanup;
  out = true;
 cleanup:
  args_list = NULL;
  Py_CLEAR(args_list_py);
  Py_CLEAR(args_);
  Py_CLEAR(kwargs);
  Py_CLEAR(result_);
  if (check_error("call_function")) out = false;
  YGG_EMBED_PYTHON_GIL_END;
  return out;
}
#else
EMBEDED_LANGUAGE_DEFN_DISABLED(EmbeddedPython, PyObject*, PYTHON_LANGUAGE, .py)
#endif
