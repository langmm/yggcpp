#ifndef DOXYGEN_SHOULD_SKIP_THIS
#ifndef PY_SSIZE_T_CLEAN
#define PY_SSIZE_T_CLEAN
#endif
#include <Python.h>
#include "communicators/CommBase.hpp"
#include "communicators/comms.hpp"
#include "utils/enums.hpp"

static const char* PICKLE_VERSION_KEY = "_pickle_version";
static int PICKLE_VERSION = 1;

typedef struct {
    PyObject_HEAD
    YggInterface::communicator::Comm_t *comm;
} pyComm_t;

//////////////////////////////////////////////////////////////
// Forward Declarations
//////////////////////////////////////////////////////////////

static void Comm_t_dealloc(PyObject* self);
static int Comm_t_init(PyObject* self, PyObject* args, PyObject* kwds);
static PyObject* Comm_t_new(PyTypeObject *type, PyObject* args, PyObject* kwds);
static PyObject* Comm_t_open(PyObject* self, PyObject* arg);
static PyObject* Comm_t_close(PyObject* self, PyObject* arg);
static PyObject* Comm_t_str(PyObject* self);
static PyObject* Comm_t_send(PyObject* self, PyObject* arg);
static PyObject* Comm_t_recv(PyObject* self, PyObject* arg);
static PyObject* Comm_t_send_eof(PyObject* self, PyObject* arg);
static PyObject* Comm_t_send_dict(PyObject* self, PyObject* arg, PyObject* kwargs);
static PyObject* Comm_t_recv_dict(PyObject* self, PyObject* arg, PyObject* kwargs);
static PyObject* Comm_t_call(PyObject* self, PyObject* arg);
static PyObject* Comm_t_set_timeout_recv(PyObject* self, PyObject* arg);
static PyObject* Comm_t_wait_for_recv(PyObject* self, PyObject* arg);
static PyObject* Comm_t_name_get(PyObject* self, void*);
static PyObject* Comm_t_address_get(PyObject* self, void*);
static PyObject* Comm_t_direction_get(PyObject* self, void*);
static PyObject* Comm_t_commtype_get(PyObject* self, void*);
static PyObject* Comm_t_metadata_get(PyObject* self, void*);
static int Comm_t_metadata_set(PyObject* self, PyObject* value, void* closure);
static PyObject* Comm_t_datatype_get(PyObject* self, void*);
static int Comm_t_datatype_set(PyObject* self, PyObject* value, void* closure);
static PyObject* Comm_t_timeout_recv_get(PyObject* self, void*);
static int Comm_t_timeout_recv_set(PyObject* self, PyObject* value, void* closure);
static PyObject* Comm_t_maxMsgSize_get(PyObject* self, void*);
static PyObject* Comm_t_is_open_get(PyObject* self, void*);
static PyObject* Comm_t_is_closed_get(PyObject* self, void*);
static PyObject* Comm_t_n_msg_get(PyObject* self, void*);
static PyObject* Comm_t_is_interface_get(PyObject* self, void*);
static PyObject* Comm_t___getstate__(PyObject* self, PyObject*);
static PyObject* Comm_t___setstate__(PyObject* self, PyObject* state);
static PyObject* Comm_t_richcompare(PyObject *self, PyObject *other, int op);
static PyObject* Comm_t_get_status_message(PyObject *self, PyObject* args, PyObject* kwargs);
static PyObject* Comm_t_printStatus(PyObject *self, PyObject* args, PyObject* kwargs);

static PyObject* commMeta_new(PyTypeObject *type, PyObject* args, PyObject* kwds);
static void commMeta_dealloc(PyObject* self);
static PyObject* commMeta_str(PyObject* self);
static PyObject* commMeta_repr(PyObject* self);
static Py_ssize_t commMeta_size(PyObject* self);
static PyObject* commMeta_subscript(PyObject* self, PyObject* key);
static int commMeta_subscript_ass(PyObject *self, PyObject *key, PyObject *value);
static int commMeta_contains(PyObject* self, PyObject* value);
static PyObject* commMeta_update(PyObject* self, PyObject* args, PyObject* kwargs);
static PyObject* commMeta_append(PyObject* self, PyObject* args);
static PyObject* commMeta_richcompare(PyObject *self, PyObject *other, int op);

static PyObject* is_comm_installed(PyObject* self, PyObject* args);


//////////////////////////////////////////////////////////////
// Wrapper to update communicator metadata
//////////////////////////////////////////////////////////////

typedef struct {
  PyObject_HEAD
  PyObject* comm;
  void* v;
} commMeta;

PyDoc_STRVAR(commMeta_doc,
	     "commMeta(comm, for_datatype=False)\n"
	     "\n"
	     "A container which corresponds to an underlying C++"
	     " rapidjson value within a communicator's metadata."
	     " When updated, the C++ value will be updated.");

static PyMappingMethods commMeta_mapping {
  commMeta_size, commMeta_subscript, commMeta_subscript_ass
};

static PySequenceMethods commMeta_seq {
  commMeta_size, NULL, NULL, NULL, NULL, NULL, NULL,
  commMeta_contains,
  NULL, NULL
};

#ifdef __GNUC__
#if !defined(__MINGW64_VERSION_MAJOR) || (defined(__MINGW64_VERSION_MAJOR) && __MINGW64_VERSION_MAJOR > 5)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-function-type"
#endif
#endif
static PyMethodDef commMeta_methods[] = {
  {"update", (PyCFunction)commMeta_update, METH_VARARGS | METH_KEYWORDS,
   "update([other]) or update(**kwargs):\n"
   " Update the underlying object from the provided dictionary or"
   " keyword/value pairs."},
  {"append", (PyCFunction)commMeta_append, METH_VARARGS,
   "append(x)\n"
   " Add an element to the end of the underlying array."},
  {NULL, NULL, 0, ""}  /* Sentinel */
};
#ifdef __GNUC__
#if !defined(__MINGW64_VERSION_MAJOR) || (defined(__MINGW64_VERSION_MAJOR) && __MINGW64_VERSION_MAJOR > 5)
#pragma GCC diagnostic pop
#endif
#endif

static PyTypeObject commMetaType = {
        PyVarObject_HEAD_INIT(NULL, 0)
        "YggInterface.CommMeta",   /* tp_name */
        sizeof(commMeta),          /* tp_basicsize */
        0,                         /* tp_itemsize */
        (destructor)commMeta_dealloc, /* tp_dealloc */
	0,                         /* tp_print */
	0,                         /* tp_getattr */
	0,                         /* tp_setattr */
	0,                         /* tp_compare */
        commMeta_repr,             /* tp_repr */
        0,                         /* tp_as_number */
        &commMeta_seq,             /* tp_as_sequence */
        &commMeta_mapping,         /* tp_as_mapping */
        0,                         /* tp_hash  */
        0,                         /* tp_call */
        commMeta_str,              /* tp_str */
        0,                         /* tp_getattro */
        0,                         /* tp_setattro */
        0,                         /* tp_as_buffer */
        Py_TPFLAGS_DEFAULT,        /* tp_flags */
        commMeta_doc,              /* tp_doc */
        0,                         /* tp_traverse */
        0,                         /* tp_clear */
        commMeta_richcompare,      /* tp_richcompare */
        0,                         /* tp_weaklistoffset */
        0,                         /* tp_iter */
        0,                         /* tp_iternext */
        commMeta_methods,          /* tp_methods */
	0,                         /* tp_members */
	0,                         /* tp_getset */
	0,                         /* tp_base */
	0,                         /* tp_dict */
	0,                         /* tp_descr_get */
	0,                         /* tp_descr_set */
	0,                         /* tp_dictoffset */
	0,                         /* tp_init */
	0,                         /* tp_alloc */
	commMeta_new,              /* tp_new */
	PyObject_Del,              /* tp_free */
        NULL,                      /* tp_is_gc */
        NULL,                      /* tp_bases */
        NULL,                      /* tp_mro */
        NULL,                      /* tp_cache */
        NULL,                      /* tp_subclasses */
        NULL,                      /* tp_weaklist */
        0,                         /* tp_del */
        0,                         /* tp_version_tag */
        0,                         /* tp_finalize */
        0                          /* tp_vectorcall */
#if (PY_MAJOR_VERSION > 3 || (PY_MAJOR_VERSION == 3 && PY_MINOR_VERSION >= 12))
	,
	0                          /* tp_watched */
#endif
};

PyObject* commMeta_new(PyTypeObject *type, PyObject* args, PyObject* kwds) {
  PyObject* commPy = NULL;
  int for_datatype = -1;
  static char const* kwlist[] = {
    "comm",
    "for_datatype",
    NULL
  };
  if (!PyArg_ParseTupleAndKeywords(args, kwds, "O|$p",
				   (char**) kwlist,
				   &commPy, &for_datatype))
    return NULL;
  commMeta* s = (commMeta*) type->tp_alloc(type, 0);
  s->comm = NULL;
  s->v = NULL;
  pyComm_t* comm = (pyComm_t*)commPy;
  if (for_datatype < 0) for_datatype = 0;
  rapidjson::Value* v = NULL;
  
  if (for_datatype)
    v = comm->comm->getMetadata().getSchema();
  else
    v = &(comm->comm->getMetadata().metadata);
  if (!v) {
    PyErr_SetString(PyExc_KeyError,
		    "The communicator does not have a datatype");
    return NULL;
  }
  Py_INCREF(commPy);
  s->comm = commPy;
  s->v = (void*)v;
  return (PyObject*)s;
}
void commMeta_dealloc(PyObject* self) {
  commMeta* s = (commMeta*)self;
  s->v = NULL;
  Py_XDECREF(s->comm);
  s->comm = NULL;
  Py_TYPE(self)->tp_free(self);
}
PyObject* commMeta_str(PyObject* self) {
  commMeta* s = (commMeta*)self;
  if (!s->v) {
    return PyObject_Str(Py_None);
  }
  PyObject* pyVal = ((rapidjson::Value*)(s->v))->GetPythonObjectRaw();
  if (pyVal == NULL) {
    // TODO: rapidjson error
    return NULL;
  }
  PyObject* out = PyObject_Str(pyVal);
  Py_DECREF(pyVal);
  return out;
}
PyObject* commMeta_repr(PyObject* self) {
  commMeta* s = (commMeta*)self;
  PyObject* v_str = PyObject_Str(self);
  if (v_str == NULL)
    return NULL;
  PyObject* comm_str = PyObject_Str(s->comm);
  if (comm_str == NULL) {
    Py_DECREF(v_str);
    return NULL;
  }
  PyObject* out = PyUnicode_FromFormat("CommMeta(%U, %U)",
				       comm_str, v_str);
  Py_DECREF(comm_str);
  Py_DECREF(v_str);
  return out;
}
Py_ssize_t commMeta_size(PyObject* self) {
  commMeta* s = (commMeta*)self;
  rapidjson::Value* v = (rapidjson::Value*)(s->v);
  long size = 0;
  if (v->IsArray()) {
    size = static_cast<long>(v->Size());
  } else if (v->IsObject()) {
    size = static_cast<long>(v->MemberCount());
  } else {
    PyErr_SetString(PyExc_TypeError, "rapidjson instance is not an array or object");
    return 0;
  }
  return static_cast<Py_ssize_t>(size);
}
PyObject* commMeta_subscript(PyObject* self, PyObject* key) {
  commMeta* s = (commMeta*)self;
  rapidjson::Value* v = (rapidjson::Value*)(s->v);
  rapidjson::Value* vsub = NULL;
  PyObject* out = NULL;
  if (v->IsArray()) {
    if (!PyLong_Check(key)) {
      PyErr_SetString(PyExc_TypeError, "Key for array must be an integer");
      return NULL;
    }
    long idx = PyLong_AsLong(key);
    if (idx < 0)
      return NULL;
    if (idx >= static_cast<long>(v->Size())) {
      PyErr_Format(PyExc_IndexError,
		   "Index (%d) exceeds size of array (%d)",
		   idx, static_cast<long>(v->Size()));
      return NULL;
    }
    vsub = &v[static_cast<rapidjson::SizeType>(idx)];
  } else if (v->IsObject()) {
    if (!PyUnicode_Check(key)) {
      PyErr_SetString(PyExc_TypeError, "Key for object must be a string");
      return NULL;
    }
    const char* keyS = PyUnicode_AsUTF8(key);
    if (!keyS)
      return NULL;
    if (!v->HasMember(keyS)) {
      PyErr_Format(PyExc_KeyError,
		   "Object does not contain key \"%s\"", keyS);
      return NULL;
    }
    vsub = &(v->FindMember(keyS)->value);
  } else {
    PyErr_SetString(PyExc_TypeError, "rapidjson instance is not an array or object");
    return NULL;
  }
  if (vsub->IsArray() || vsub->IsObject()) {
    PyObject* typ_args = PyTuple_Pack(1, s->comm);
    if (typ_args == NULL)
      return NULL;
    out = PyObject_Call((PyObject*)(self->ob_type), typ_args, NULL);
    Py_DECREF(typ_args);
    if (out == NULL)
      return NULL;
    ((commMeta*)out)->v = (void*)vsub;
  } else {
    out = vsub->GetPythonObjectRaw();
  }
  return out;
}
int commMeta_subscript_ass(PyObject *self, PyObject *key, PyObject *value) {
  commMeta* s = (commMeta*)self;
  pyComm_t* comm = (pyComm_t*)(s->comm);
  rapidjson::Value* v = (rapidjson::Value*)(s->v);
  rapidjson::Value vsub(value, comm->comm->getMetadata().GetAllocator());
  if (v->IsArray()) {
    if (!PyLong_Check(key)) {
      PyErr_SetString(PyExc_TypeError, "Key for array must be an integer");
      return -1;
    }
    long idx = PyLong_AsLong(key);
    if (idx < 0)
      return -1;
    if (idx >= static_cast<long>(v->Size())) {
      PyErr_Format(PyExc_IndexError,
		   "Index (%d) exceeds size of array (%d)",
		   idx, static_cast<long>(v->Size()));
      return -1;
    }
    vsub.Swap(v[static_cast<rapidjson::SizeType>(idx)]);
  } else if (v->IsObject()) {
    if (!PyUnicode_Check(key)) {
      PyErr_SetString(PyExc_TypeError, "Key for object must be a string");
      return -1;
    }
    const char* keyS = PyUnicode_AsUTF8(key);
    if (!keyS)
      return -1;
    if (v->HasMember(keyS)) {
      vsub.Swap(v->FindMember(keyS)->value);
    } else {
      rapidjson::Value keyJ(keyS,
			    comm->comm->getMetadata().GetAllocator());
      v->AddMember(keyJ, vsub, comm->comm->getMetadata().GetAllocator());
    }
  } else {
    PyErr_SetString(PyExc_TypeError, "rapidjson instance is not an array or object");
    return -1;
  }
  return 0;
}
int commMeta_contains(PyObject* self, PyObject* value) {
  commMeta* s = (commMeta*)self;
  pyComm_t* comm = (pyComm_t*)(s->comm);
  rapidjson::Value* v = (rapidjson::Value*)(s->v);
  rapidjson::Value vsub(value, comm->comm->getMetadata().GetAllocator());
  if (v->IsArray()) {
    return static_cast<int>(v->Contains(vsub));
  } else if (v->IsObject()) {
    if (!PyUnicode_Check(value)) {
      PyErr_SetString(PyExc_TypeError, "Value for object must be a string");
      return -1;
    }
    const char* keyS = PyUnicode_AsUTF8(value);
    if (!keyS)
      return -1;
    return static_cast<int>(v->HasMember(keyS));
  } else {
    PyErr_SetString(PyExc_TypeError, "rapidjson instance is not an array or object");
    return -1;
  }
}
int commMeta_update_raw(PyObject* self, PyObject* dict) {
  if (dict != NULL) {
    commMeta* s = (commMeta*)self;
    pyComm_t* comm = (pyComm_t*)(s->comm);
    rapidjson::Value* v = (rapidjson::Value*)(s->v);
    if (!v->IsObject()) {
      PyErr_SetString(PyExc_TypeError, "rapidjson instance is not an object");
      return -1;
    }
    if (!PyDict_Check(dict)) {
      PyErr_SetString(PyExc_TypeError, "Supplied value must be a dictionary");
      return -1;
    }
    rapidjson::Value vsub;
    PyObject *key, *value;
    Py_ssize_t pos = 0;
    while (PyDict_Next(dict, &pos, &key, &value)) {
      if (!PyUnicode_Check(key)) {
	PyErr_SetString(PyExc_TypeError, "Rapidjson only support string keys");
	return -1;
      }
      const char* keyS = PyUnicode_AsUTF8(key);
      if (!keyS)
	return -1;
      vsub.SetPythonObjectRaw(value,
			      comm->comm->getMetadata().GetAllocator());
      if (v->HasMember(keyS)) {
	vsub.Swap(v->FindMember(keyS)->value);
      } else {
	rapidjson::Value keyJ(keyS,
			      comm->comm->getMetadata().GetAllocator());
	v->AddMember(keyJ, vsub,
		     comm->comm->getMetadata().GetAllocator());
      }
    }
  }
  return 0;
}
PyObject* commMeta_update(PyObject* self, PyObject* args, PyObject* kwargs) {
  PyObject* other = NULL;
  if (!PyArg_ParseTuple(args, "|O", &other)) {
    return NULL;
  }
  if (commMeta_update_raw(self, other) < 0)
    return NULL;
  if (commMeta_update_raw(self, kwargs) < 0)
    return NULL;
  Py_RETURN_NONE;
}
PyObject* commMeta_append(PyObject* self, PyObject* args) {
  commMeta* s = (commMeta*)self;
  pyComm_t* comm = (pyComm_t*)(s->comm);
  rapidjson::Value* v = (rapidjson::Value*)(s->v);
  if (!v->IsArray()) {
    PyErr_SetString(PyExc_TypeError, "rapidjson instance is not an array");
    return NULL;
  }
  PyObject* other = NULL;
  if (!PyArg_ParseTuple(args, "O", &other)) {
    return NULL;
  }
  rapidjson::Value vsub(other, comm->comm->getMetadata().GetAllocator());
  v->PushBack(vsub, comm->comm->getMetadata().GetAllocator());
  Py_RETURN_NONE;
}
PyObject* commMeta_richcompare(PyObject *self, PyObject *other, int op) {
  commMeta* s = (commMeta*)self;
  rapidjson::Value* v = (rapidjson::Value*)(s->v);
  PyObject* lhs = v->GetPythonObjectRaw();
  PyObject* rhs = NULL;
  if (PyObject_IsInstance(other, (PyObject*)(&commMetaType))) {
    rhs = ((rapidjson::Value*)(((commMeta*)other)->v))->GetPythonObjectRaw();
  } else {
    Py_INCREF(other);
    rhs = other;
  }
  PyObject* out = PyObject_RichCompare(lhs, rhs, op);
  Py_DECREF(lhs);
  Py_DECREF(rhs);
  return out;
}

//////////////////////////////////////////////////////////////
// Comm_t Wrapper
//////////////////////////////////////////////////////////////

#ifdef __GNUC__
#if !defined(__MINGW64_VERSION_MAJOR) || (defined(__MINGW64_VERSION_MAJOR) && __MINGW64_VERSION_MAJOR > 5)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-function-type"
#endif
#endif
static PyMethodDef Comm_t_methods[] = {
        {"open", (PyCFunction) Comm_t_open, METH_NOARGS, ""},
        {"close", (PyCFunction) Comm_t_close, METH_NOARGS, ""},
        {"send", (PyCFunction) Comm_t_send, METH_VARARGS, ""},
        {"recv", (PyCFunction) Comm_t_recv, METH_VARARGS, ""},
        {"send_eof", (PyCFunction) Comm_t_send_eof, METH_NOARGS, ""},
	{"send_dict", (PyCFunction) Comm_t_send_dict,
	 METH_VARARGS | METH_KEYWORDS, ""},
	{"recv_dict", (PyCFunction) Comm_t_recv_dict,
	 METH_VARARGS | METH_KEYWORDS, ""},
	{"call", (PyCFunction) Comm_t_call, METH_VARARGS, ""},
        {"set_timeout_recv", (PyCFunction) Comm_t_set_timeout_recv, METH_VARARGS, ""},
        {"wait_for_recv", (PyCFunction) Comm_t_wait_for_recv, METH_VARARGS, ""},
	{"__getstate__", (PyCFunction) Comm_t___getstate__, METH_NOARGS,
	 "Pickle the Comm_t instance"},
	{"__setstate__", (PyCFunction) Comm_t___setstate__, METH_O,
	 "Un-pickle the Comm_t instance"},
	{"get_status_message", (PyCFunction) Comm_t_get_status_message,
	 METH_VARARGS | METH_KEYWORDS,
	 "Get a list of lines describing the communicator's status"},
	{"printStatus", (PyCFunction) Comm_t_printStatus,
	 METH_VARARGS | METH_KEYWORDS,
	 "Log the communicator's status"},
        {NULL, NULL, 0, ""}  /* Sentinel */
};
#ifdef __GNUC__
#if !defined(__MINGW64_VERSION_MAJOR) || (defined(__MINGW64_VERSION_MAJOR) && __MINGW64_VERSION_MAJOR > 5)
#pragma GCC diagnostic pop
#endif
#endif

// Possible attributes/properties to convert:
//   empty_bytes_msg, model_name, full_model_name, model_copies,
//   model_env, opp_name, opp_address, opp_comms, language_driver,
//   is_confirmed_send, is_confirmed_recv, is_confirmed, n_msg_recv,
//   n_msg_send, n_msg_recv_drain, n_msg_send_drain, eof_msg,
//   empty_obj_recv, get_response_comm_kwargs, get_work_comm_kwargs,
//   create_work_comm_kwargs
// Possible methods to convert:
//   opp_comm_kwargs, open, close_in_thread, linger_close, linger, atexit,
//   language_atexit, wait_for_workers, wait_for_confirm, confirm,
//   confirm_send, confirm_recv, is_eof,
//   update_message_from_serializer, update_serializer_from_message,
//   apply_transform_to_type, apply_transform, evaluate_filter, is_empty,
//   is_empty_recv, chunk_message, precheck, server_exists, new_server,
//   signon_to_server, signoff_from_server, get_work_comm,
//   create_work_comm, add_work_comm, remove_work_comm, workcomm2header,
//   header2workcomm, serialize, deserialize, _safe_send, _safe_recv,
//   _send, _recv, send_message, recv_message, prepare_header,
//   prepare_message, drain_server_signon_messages, drain_messages,
//   purge
// Possible class methods:
//   underlying_comm_class, cleanup_comms, is_registered, register_comm,
//   comm_registry, unregister_comm, comm_count, new_comm_kwargs,
//   new_comm, is_installed
static PyGetSetDef Comm_t_properties[] = {
  {"name", Comm_t_name_get, NULL,
   "The communicator's name", NULL},
  {"address", Comm_t_address_get, NULL,
   "The communicator's address", NULL},
  {"direction", Comm_t_direction_get, NULL,
   "The communicator's direction", NULL},
  {"commtype", Comm_t_commtype_get, NULL,
   "The communicator's commtype", NULL},
  {"metadata", Comm_t_metadata_get, Comm_t_metadata_set,
   "The metadata associated with the communicator that will be sent in "
   "message headers", NULL},
  {"datatype", Comm_t_datatype_get, Comm_t_datatype_set,
   "The datatype associated with the communicator that will be sent in "
   "message headers and used to validate messages", NULL},
  {"timeout_recv", Comm_t_timeout_recv_get, Comm_t_timeout_recv_set,
   "The time waited during a receive call for an incoming message.",
   NULL},
  {"maxMsgSize", Comm_t_maxMsgSize_get, NULL,
   "The maximum size for individual messages sent through the communicator."
   " Messages larger than this size will be broken into multiple parts.",
   NULL},
  {"is_open", Comm_t_is_open_get, NULL,
   "True if the communicator is open, False otherwise.", NULL},
  {"is_closed", Comm_t_is_closed_get, NULL,
   "True if the communicator is closed, False otherwise.", NULL},
  {"n_msg", Comm_t_n_msg_get, NULL,
   "Number of messages that available to be received or are in the "
   "process of being sent.", NULL},
  {"is_interface", Comm_t_is_interface_get, NULL,
   "True if the communicator is part of a model interface, False "
   "otherwise.",
   NULL},
  {NULL, NULL, NULL, NULL, NULL} /* Sentinel */
};

static PyTypeObject Comm_tType = {
        PyVarObject_HEAD_INIT(NULL, 0)
        "YggInterface.Comm_t",     /* tp_name */
        sizeof(pyComm_t),          /* tp_basicsize */
        0,                         /* tp_itemsize */
        (destructor)Comm_t_dealloc, /* tp_dealloc */
        0,                         /* tp_vectorcall_offset */
        0,                         /* tp_getattr */
        0,                         /* tp_setattr */
        0,                         /* tp_reserved */
        (reprfunc)Comm_t_str,      /* tp_repr */
        0,                         /* tp_as_number */
        0,                         /* tp_as_sequence */
        0,                         /* tp_as_mapping */
        0,                         /* tp_hash  */
        0,                         /* tp_call */
        (reprfunc)Comm_t_str,      /* tp_str */
        0,                         /* tp_getattro */
        0,                         /* tp_setattro */
        0,                         /* tp_as_buffer */
        Py_TPFLAGS_DEFAULT,        /* tp_flags */
        "Comm_t object",           /* tp_doc */
        0,                         /* tp_traverse */
        0,                         /* tp_clear */
        Comm_t_richcompare,        /* tp_richcompare */
        0,                         /* tp_weaklistoffset */
        0,                         /* tp_iter */
        0,                         /* tp_iternext */
        Comm_t_methods,            /* tp_methods */
        0,                         /* tp_members */
        Comm_t_properties,         /* tp_getset */
        0,                         /* tp_base */
        0,                         /* tp_dict */
        0,                         /* tp_descr_get */
        0,                         /* tp_descr_set */
        0,                         /* tp_dictoffset */
        (initproc)Comm_t_init,     /* tp_init */
        0,                         /* tp_alloc */
        Comm_t_new,                /* tp_new */
        0,                         /* tp_free */
        NULL,                      /* tp_is_gc */
        NULL,                      /* tp_bases */
        NULL,                      /* tp_mro */
        NULL,                      /* tp_cache */
        NULL,                      /* tp_subclasses */
        NULL,                      /* tp_weaklist */
        0,                         /* tp_del */
        0,                         /* tp_version_tag */
        0,                         /* tp_finalize */
        0                          /* tp_vectorcall */
#if (PY_MAJOR_VERSION > 3 || (PY_MAJOR_VERSION == 3 && PY_MINOR_VERSION >= 12))
	,
	0                          /* tp_watched */
#endif
};


static void Comm_t_dealloc(PyObject* self) {
    pyComm_t* s = (pyComm_t*)self;
    delete s->comm;
    Py_TYPE(self)->tp_free(self);
}

template<typename T>
static int _parse_enum_string(const std::string& desc,
			      PyObject* var, int& var_enum,
			      const std::map<const T, const std::string>& enum_map,
			      int default_var=-1,
			      bool allow_nomatch=false,
			      bool allow_anycase=false,
			      std::string prefix="",
			      std::string suffix="") {
  if (default_var >= 0)
    var_enum = default_var;
  if (var == NULL)
    return 0;
  if (PyLong_Check(var)) {
    var_enum = PyLong_AsLong(var);
    int enum_max = (int)(max_enum_value(enum_map));
    if (var_enum < 0 || var_enum > enum_max) {
      PyErr_Format(PyExc_TypeError, "Invalid %s: %d",
		   desc.c_str(), var_enum);
      return -1;
    }
  } else if (PyUnicode_Check(var)) {
    const char* varStr = PyUnicode_AsUTF8(var);
    if (!varStr)
      return -1;
    T _var_enum;
    if (!enum_value_search(enum_map, std::string(varStr), _var_enum,
			   allow_anycase, prefix, suffix)) {
      if (!allow_nomatch) {
	PyErr_Format(PyExc_TypeError, "Invalid %s: %s",
		     desc.c_str(), varStr);
      }
      return -1;
    }
    var_enum = (int)(_var_enum);
  } else {
    PyErr_Format(PyExc_TypeError, "%s must be a string or an integer.",
		 desc.c_str());
    return -1;
  }
  return 0;
}

static int _parse_direction(PyObject* dirnPy, int& dirn) {
  return _parse_enum_string("direction", dirnPy, dirn,
			    DIRECTION_map, (int)(DIRECTION::SEND),
			    false, true);
}

static int _parse_schema(PyObject* schemaPy, rapidjson::Document& schema) {
  if (schemaPy != NULL) {
    if (!schema.SetPythonObjectRaw(schemaPy, schema.GetAllocator())) {
      PyErr_SetString(PyExc_TypeError, "Error converting schema to rapidjson::Document");
      return -1;
    }
  }
  return 0;
}

static int _parse_commtype(PyObject* commtypePy, int& commtype) {
  // TODO: Verify that commtype is installed or throw Python error
  if (_parse_enum_string("commtype", commtypePy, commtype,
			 COMM_TYPE_cls_map,
			 (int)(COMM_TYPE::DEFAULT_COMM), true) >= 0)
    return 0;
  return _parse_enum_string("commtype", commtypePy, commtype,
			    COMM_TYPE_map,
			    (int)(COMM_TYPE::DEFAULT_COMM),
			    false, true, "", "_COMM");
}

static int _parse_timeout_recv(PyObject* recv_timeoutPy,
			       int64_t& timeout_recv,
			       bool& timeout_recv_set) {
  if (recv_timeoutPy != NULL && recv_timeoutPy != Py_None) {
    if (PyBool_Check(recv_timeoutPy) && recv_timeoutPy == Py_False) {
      timeout_recv = -1;
      timeout_recv_set = true;
    } else if (PyFloat_Check(recv_timeoutPy)) {
      timeout_recv = PyFloat_AsDouble(recv_timeoutPy) * 1000000;
      timeout_recv_set = true;
    } else if (PyLong_Check(recv_timeoutPy)) {
      timeout_recv = PyLong_AsLongLong(recv_timeoutPy);
      timeout_recv_set = true;
    }
  }
  return 0;
}

static int _parse_language(PyObject* languagePy, int& language) {
  return _parse_enum_string("language", languagePy, language,
			    LANGUAGE_map,
			    (int)(LANGUAGE::PYTHON_LANGUAGE),
			    false, true);
}


static int _parse_doc_funcs(const std::string& desc, PyObject* varPy,
			    std::vector<PyObject*>& varVect) {
  if (varPy == NULL)
    return 0;
  if (PySequence_Check(varPy)) {
    for (Py_ssize_t i = 0; i < PySequence_Size(varPy); i++) {
      PyObject* item = PySequence_GetItem(varPy, i);
      if (item == NULL)
	return -1;
      if (_parse_doc_funcs(desc, item, varVect) < 0) {
	Py_DECREF(item);
	return -1;
      }
      Py_DECREF(item);
    }
  } else if (PyCallable_Check(varPy)) {
    Py_INCREF(varPy);
    varVect.push_back(varPy);
  } else {
    PyErr_Format(PyExc_TypeError, "%s value is not callable", desc.c_str());
    return -1;
  }
  return 0;
}

static int _parse_string_vect(const std::string& desc, PyObject* varPy,
			      std::vector<std::string>& varVect,
			      bool require_sequence=false) {
  if (varPy == NULL)
    return 0;
  if (PyUnicode_Check(varPy)) {
    if (require_sequence) {
      PyErr_Format(PyExc_TypeError, "%s value must be a sequence", desc.c_str());
      return -1;
    }
    varVect.emplace_back(PyUnicode_AsUTF8(varPy));
  } else if (PySequence_Check(varPy)) {
    for (Py_ssize_t i = 0; i < PySequence_Size(varPy); i++) {
      PyObject* item = PySequence_GetItem(varPy, i);
      if (item == NULL)
	return -1;
      if (_parse_string_vect(desc, item, varVect) < 0) {
	Py_DECREF(item);
	return -1;
      }
      Py_DECREF(item);
    }
  } else {
    PyErr_Format(PyExc_TypeError, "%s value is not a string", desc.c_str());
    return -1;
  }
  return 0;
}

// API kwargs not handled:
//   vars, length_map, field_names, field_units,
//   filter (strings or schemas), transform (strings or schemas),
//   serializer, is_default, outside_loop, dont_copy, default_file,
//   default_value, for_service, working_dir, onexit
// Variables not handled currently:
//   dont_open, env, partner_copies,
//   partner_model, partner_language, partner_mpi_ranks, close_on_eof_recv
//   close_on_eof_send, reverse_names, no_suffix, serializer
// Variables handled via flag:
//   is_interface, allow_multiple_comms, is_client, is_response_client,
//   is_server, is_response_server, is_async, single_use
static int Comm_t_init(PyObject* self, PyObject* args, PyObject* kwds) {
    pyComm_t* s = (pyComm_t*)self;
    char* adr = NULL;
    char* name = NULL;
    PyObject* dirnPy = NULL;
    PyObject* datatypePy = NULL;
    PyObject* metadataPy = NULL;
    PyObject* commtypePy = NULL;
    PyObject* recv_timeoutPy = NULL;
    PyObject* field_namesPy = NULL;
    PyObject* field_unitsPy = NULL;
    PyObject* languagePy = NULL;
    PyObject* filterPy = NULL;
    PyObject* transformPy = NULL;
    int dirn = DIRECTION::SEND;
    rapidjson::Document datatype, metadata;
    int commtype = COMM_TYPE::DEFAULT_COMM;
    int flags = 0;
    unsigned int ncomm = 0;
    int64_t timeout_recv = -1;
    bool timeout_recv_set = false;
    const char* format_str = NULL;
    int as_array = 0;
    std::vector<std::string> field_names;
    std::vector<std::string> field_units;
    int language = LANGUAGE::NO_LANGUAGE;
    std::vector<PyObject*> filter;
    std::vector<PyObject*> transform;
    PyObject* request_commtypePy = NULL;
    PyObject* response_kwargs = NULL;
    PyObject* response_datatypePy = NULL;
    PyObject* response_metadataPy = NULL;
    PyObject* response_commtypePy = NULL;
    PyObject* response_field_namesPy = NULL;
    PyObject* response_field_unitsPy = NULL;
    PyObject* response_filterPy = NULL;
    PyObject* response_transformPy = NULL;
    int request_commtype = COMM_TYPE::DEFAULT_COMM;
    int response_commtype = COMM_TYPE::DEFAULT_COMM;
    int request_flags = 0;
    int response_flags = 0;
    const char* response_format_str = NULL;
    int response_as_array = 0;
    std::vector<std::string> response_field_names;
    std::vector<std::string> response_field_units;
    std::vector<PyObject*> response_filter;
    std::vector<PyObject*> response_transform;
    int response_dirn = DIRECTION::NONE;
    rapidjson::Document response_datatype, response_metadata;
    int dont_open = 0;
    static char const* kwlist[] = {
      "name",
      "address",
      "direction",
      "datatype",
      "commtype",
      "flags",
      "ncomm",
      "recv_timeout",
      "metadata",
      "format_str",
      "as_array",
      "field_names",
      "field_units",
      "language",
      "filter",
      "transform",
      "request_commtype",
      "request_flags",
      "response_kwargs",
      "dont_open",
      NULL
    };
    static char const* response_kwlist[] = {
      "datatype",
      "commtype",
      "flags",
      "metadata",
      "format_str",
      "as_array",
      "field_names",
      "field_units",
      "filter",
      "transform",
      NULL
    };
    s->comm = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "|ssO$OOiIOOspOOOOOOiOp",
				     (char**) kwlist,
				     &name, &adr, &dirnPy, &datatypePy,
				     &commtypePy, &flags, &ncomm,
				     &recv_timeoutPy,
				     &metadataPy, &format_str,
				     &as_array, &field_namesPy,
				     &field_unitsPy, &languagePy,
				     &filterPy, &transformPy,
				     &request_commtypePy,
				     &request_flags, &response_kwargs,
				     &dont_open))
      return -1;

    if(adr == NULL && name == NULL) {
        PyErr_SetString(PyExc_TypeError, "Neither name nor address provided");
        return -1;
    }
    if (_parse_direction(dirnPy, dirn) < 0)
      return -1;
    if (_parse_schema(datatypePy, datatype) < 0)
      return -1;
    if (_parse_schema(metadataPy, metadata) < 0)
      return -1;
    if (_parse_commtype(commtypePy, commtype) < 0)
      return -1;
    if (_parse_timeout_recv(recv_timeoutPy, timeout_recv,
			    timeout_recv_set))
      return -1;
    if(flags < 0) {
        PyErr_SetString(PyExc_TypeError, "Invalid flags value");
        return -1;
    }
    if (_parse_string_vect("field_names", field_namesPy,
			   field_names) < 0)
      return -1;
    if (_parse_string_vect("field_units", field_unitsPy,
			   field_units) < 0)
      return -1;
    if (_parse_language(languagePy, language) < 0)
      return -1;
    if (_parse_doc_funcs("filter", filterPy, filter) < 0) {
      return -1;
    }
    if (_parse_doc_funcs("transform", transformPy, transform) < 0) {
      return -1;
    }
    if (request_commtypePy != NULL) {
      if (s->comm->getType() != COMM_TYPE::CLIENT_COMM &&
	  s->comm->getType() != COMM_TYPE::SERVER_COMM) {
	PyErr_SetString(PyExc_TypeError, "request_commtype is only valid as a keyword argument for client, server, or timesync communicators");
	return -1;
      }
      if (_parse_commtype(request_commtypePy, request_commtype) < 0)
	return -1;
    }
    if (request_flags != 0) {
      if (s->comm->getType() != COMM_TYPE::CLIENT_COMM &&
	  s->comm->getType() != COMM_TYPE::SERVER_COMM) {
	PyErr_SetString(PyExc_TypeError, "request_flags is only valid as a keyword argument for client, server, or timesync communicators");
	return -1;
      }
    }
    if (response_kwargs != NULL) {
      if (s->comm->getType() != COMM_TYPE::CLIENT_COMM &&
	  s->comm->getType() != COMM_TYPE::SERVER_COMM) {
	PyErr_SetString(PyExc_TypeError, "response_kwargs is only valid as a keyword argument for client, server, or timesync communicators");
	return -1;
      }
      if (!PyDict_Check(response_kwargs)) {
	PyErr_SetString(PyExc_TypeError, "response_kwargs must be a dictionary");
	return -1;
      }
      if (s->comm->getType() == COMM_TYPE::CLIENT_COMM) {
	response_dirn = DIRECTION::RECV;
      } else if (s->comm->getType() == COMM_TYPE::SERVER_COMM) {
	response_dirn = DIRECTION::SEND;
      }
      PyObject* response_args = PyTuple_New(0);
      if (!PyArg_ParseTupleAndKeywords(response_args, response_kwargs,
				       "|$OOiOspOOOO", (char**) response_kwlist,
				       &response_datatypePy,
				       &response_commtypePy,
				       &response_flags,
				       &response_metadataPy,
				       &response_format_str,
				       &response_as_array,
				       &response_field_namesPy,
				       &response_field_unitsPy,
				       &response_filterPy,
				       &response_transformPy))
	return -1;
      if (_parse_schema(response_datatypePy, response_datatype) < 0)
	return -1;
      if (_parse_schema(response_metadataPy, response_metadata) < 0)
	return -1;
      if (_parse_commtype(response_commtypePy, response_commtype) < 0)
	return -1;
      if (_parse_string_vect("response field_names",
			     response_field_namesPy,
			     response_field_names) < 0)
	return -1;
      if (_parse_string_vect("response field_units",
			     response_field_unitsPy,
			     response_field_units) < 0)
	return -1;
      if (_parse_doc_funcs("response filter",
			   response_filterPy,
			   response_filter) < 0)
	return -1;
      if (_parse_doc_funcs("response transform",
			   response_transformPy,
			   response_transform) < 0)
	return -1;
    }
    if (dont_open) {
      flags |= COMM_FLAG_DELAYED_OPEN;
    }
    try {
      s->comm = YggInterface::communicator::new_Comm_t(
		       (DIRECTION)dirn, (COMM_TYPE)commtype,
		       name, adr, flags, static_cast<size_t>(ncomm),
		       (COMM_TYPE)request_commtype,
		       (COMM_TYPE)response_commtype,
		       request_flags, response_flags);
    } catch (...) {
      s->comm = NULL;
    }
    if (!s->comm) {
      PyErr_SetString(PyExc_TypeError, "Error initializing comm");
      return -1;
    }
    if (!datatype.IsNull()) {
      if (!s->comm->addSchema(datatype)) {
	PyErr_SetString(PyExc_TypeError, "Invalid datatype");
	return -1;
      }
    }
    if (!metadata.IsNull()) {
      if (!s->comm->addSchema(metadata, true)) {
	PyErr_SetString(PyExc_TypeError, "Invalid metadata");
	return -1;
      }
    }
    if (timeout_recv_set) {
      s->comm->set_timeout_recv(timeout_recv);
    }
    if (format_str != NULL) {
      if (!s->comm->addFormat(format_str, as_array,
			      field_names, field_units)) {
	PyErr_SetString(PyExc_TypeError, "Invalid format_str");
	return -1;
      }
    }
    if (!s->comm->setLanguage((LANGUAGE)language)) {
      PyErr_SetString(PyExc_TypeError, "Error setting language");
      return -1;
    }
    if ((!filter.empty()) && (!s->comm->setFilters(filter))) {
      PyErr_SetString(PyExc_TypeError, "Error setting filters");
      return -1;
    }
    if ((!transform.empty()) && (!s->comm->setTransforms(transform))) {
      PyErr_SetString(PyExc_TypeError, "Error setting transforms");
      return -1;
    }
    if (response_format_str != NULL) {
      if (!s->comm->addFormat(response_format_str, response_as_array,
			      response_field_names, response_field_units,
			      (DIRECTION)response_dirn)) {
	PyErr_SetString(PyExc_TypeError, "Invalid response format_str");
	return -1;
      }
    }
    if (!response_datatype.IsNull()) {
      if (!s->comm->addSchema(response_datatype, false,
			      (DIRECTION)response_dirn)) {
	PyErr_SetString(PyExc_TypeError, "Invalid response datatype");
	return -1;
      }
    }
    if (!response_metadata.IsNull()) {
      if (!s->comm->addSchema(response_metadata, true,
			      (DIRECTION)response_dirn)) {
	PyErr_SetString(PyExc_TypeError, "Invalid response metadata");
	return -1;
      }
    }
    if ((!response_filter.empty()) &&
	(!s->comm->setFilters(response_filter,
			      (DIRECTION)response_dirn))) {
      PyErr_SetString(PyExc_TypeError, "Error setting response filters");
      return -1;
    }
    if ((!response_transform.empty()) &&
	(!s->comm->setTransforms(response_transform,
				 (DIRECTION)response_dirn))) {
      PyErr_SetString(PyExc_TypeError, "Error setting response transforms");
      return -1;
    }
    return 0;
}

static PyObject* Comm_t_new(PyTypeObject *type, PyObject* args, PyObject* kwds) {
    (void)args;
    (void)kwds;
    pyComm_t *self;
    self = (pyComm_t*)type->tp_alloc(type, 0);

    return (PyObject*)self;
}

PyObject* Comm_t_open(PyObject* self, PyObject*) {
  try {
    ((pyComm_t*)self)->comm->open();
    Py_RETURN_NONE;
  } catch (...) {
    return NULL;
  }
}
PyObject* Comm_t_close(PyObject* self, PyObject*) {
  try {
    ((pyComm_t*)self)->comm->close();
    Py_RETURN_NONE;
  } catch (...) {
    return NULL;
  }
}

PyObject* Comm_t_str(PyObject* self) {
  pyComm_t* s = (pyComm_t*)self;
  const char* dirStr = "NONE";
  if (s->comm->getDirection() == DIRECTION::RECV)
    dirStr = "RECV";
  else if (s->comm->getDirection() == DIRECTION::SEND)
    dirStr = "SEND";
  return PyUnicode_FromFormat("Comm_t(%s, %s, %s)",
			      s->comm->getName().c_str(),
			      s->comm->getAddress().c_str(),
			      dirStr);
}

PyObject* Comm_t_send(PyObject* self, PyObject* arg) {
    pyComm_t* s = (pyComm_t*)self;
    rapidjson::Document doc;
    if (PyTuple_Size(arg) == 1) {
      PyObject* arg0 = PyTuple_GetItem(arg, 0);
      if (arg0 == NULL) {
	return NULL;
      }
      doc.SetPythonObjectRaw(arg0, doc.GetAllocator());
    } else {
      doc.SetPythonObjectRaw(arg, doc.GetAllocator());
    }
    int out = -1;
    Py_BEGIN_ALLOW_THREADS
    out = s->comm->sendVar(doc);
    Py_END_ALLOW_THREADS
    if (out < 0) {
      Py_RETURN_FALSE;
    }
    Py_RETURN_TRUE;
}

PyObject* Comm_t_recv(PyObject* self, PyObject*) {
    pyComm_t* s = (pyComm_t*)self;
    rapidjson::Document doc;
    long flag = -1;
    Py_BEGIN_ALLOW_THREADS
    flag = s->comm->recvVar(doc);
    Py_END_ALLOW_THREADS
    PyObject* pyFlag;
    PyObject* res;
    if (flag < 0) {
      Py_INCREF(Py_False);
      pyFlag = Py_False;
      Py_INCREF(Py_None);
      res = Py_None;
    } else {
      Py_INCREF(Py_True);
      pyFlag = Py_True;
      res = doc.GetPythonObjectRaw();
    }
    PyObject* out = PyTuple_Pack(2, pyFlag, res);
    return out;
}

PyObject* Comm_t_send_eof(PyObject* self, PyObject*) {
  int out = -1;
  Py_BEGIN_ALLOW_THREADS
  out = ((pyComm_t*)self)->comm->send_eof();
  Py_END_ALLOW_THREADS
  if (out < 0) {
    Py_RETURN_FALSE;
  }
  Py_RETURN_TRUE;
}

static PyObject* Comm_t_send_dict(PyObject* self, PyObject* arg, PyObject* kwargs) {
  pyComm_t* s = (pyComm_t*)self;
  rapidjson::Document doc;
  PyObject* key_orderPy = NULL;
  std::vector<std::string> key_order;
  size_t dim = 1;
  if (PyTuple_Size(arg) == 1) {
    PyObject* arg0 = PyTuple_GetItem(arg, 0);
    if (arg0 == NULL) {
      return NULL;
    }
    doc.SetPythonObjectRaw(arg0, doc.GetAllocator());
  } else {
    doc.SetPythonObjectRaw(arg, doc.GetAllocator());
  }
  static char const* kwlist[] = {
    "key_order",
    "dim",
    NULL
  };
  PyObject* empty_args = PyTuple_New(0);
  if (empty_args == NULL) {
    return NULL;
  }
  if (!PyArg_ParseTupleAndKeywords(empty_args, kwargs,
				   "|$Oi", (char**)kwlist,
				   &key_orderPy,
				   &dim))
    return NULL;
  Py_DECREF(empty_args);
  if (_parse_string_vect("key_order", key_orderPy, key_order, true) < 0)
    return NULL;
  int out = -1;
  Py_BEGIN_ALLOW_THREADS
  out = s->comm->send_dict(doc, key_order, dim);
  Py_END_ALLOW_THREADS
  if (out < 0) {
    Py_RETURN_FALSE;
  }
  Py_RETURN_TRUE;
}

static PyObject* Comm_t_recv_dict(PyObject* self, PyObject* arg, PyObject* kwargs) {
  pyComm_t* s = (pyComm_t*)self;
  rapidjson::Document doc;
  PyObject* key_orderPy = NULL;
  std::vector<std::string> key_order;
  size_t dim = 1;
  static char const* kwlist[] = {
    "key_order",
    "dim",
    NULL
  };
  if (!PyArg_ParseTupleAndKeywords(arg, kwargs,
				   "|$Oi", (char**)kwlist,
				   &key_orderPy,
				   &dim))
    return NULL;
  if (_parse_string_vect("key_order", key_orderPy, key_order, true) < 0)
    return NULL;
  long flag = -1;
  Py_BEGIN_ALLOW_THREADS
  flag = s->comm->recv_dict(doc, key_order, dim);
  Py_END_ALLOW_THREADS
  PyObject* pyFlag;
  PyObject* res;
  if (flag < 0) {
    Py_INCREF(Py_False);
    pyFlag = Py_False;
    Py_INCREF(Py_None);
    res = Py_None;
  } else {
    Py_INCREF(Py_True);
    pyFlag = Py_True;
    res = doc.GetPythonObjectRaw();
  }
  PyObject* out = PyTuple_Pack(2, pyFlag, res);
  return out;
}

static PyObject* Comm_t_call(PyObject* self, PyObject* arg) {
  pyComm_t* s = (pyComm_t*)self;
  rapidjson::Document doc_send, doc_recv;
  if (PyTuple_Size(arg) == 1) {
    PyObject* arg0 = PyTuple_GetItem(arg, 0);
    if (arg0 == NULL) {
      return NULL;
    }
    doc_send.SetPythonObjectRaw(arg0, doc_send.GetAllocator());
  } else {
    doc_send.SetPythonObjectRaw(arg, doc_send.GetAllocator());
  }
  int flag = -1;
  Py_BEGIN_ALLOW_THREADS
  flag = s->comm->callVar(doc_send, doc_recv);
  Py_END_ALLOW_THREADS
  PyObject* pyFlag;
  PyObject* res;
  if (flag < 0) {
    Py_INCREF(Py_False);
    pyFlag = Py_False;
    Py_INCREF(Py_None);
    res = Py_None;
  } else {
    Py_INCREF(Py_True);
    pyFlag = Py_True;
    res = doc_recv.GetPythonObjectRaw();
  }
  PyObject* out = PyTuple_Pack(2, pyFlag, res);
  return out;
}

PyObject* Comm_t_set_timeout_recv(PyObject* self, PyObject* arg) {
    pyComm_t* s = (pyComm_t*)self;
    int new_timeout;
    if(!PyArg_ParseTuple(arg, "i", &new_timeout)) {
        PyErr_SetString(PyExc_TypeError, "Invalid argument given.");
        return NULL;
    }
    s->comm->set_timeout_recv(new_timeout);
    return NULL;
}

PyObject* Comm_t_wait_for_recv(PyObject* self, PyObject* arg) {
    pyComm_t* s = (pyComm_t*)self;
    int tout;
    if(!PyArg_ParseTuple(arg, "i", &tout)) {
        PyErr_SetString(PyExc_TypeError, "Invalid argument given.");
        return NULL;
    }
    int wt = -1;
    Py_BEGIN_ALLOW_THREADS
    wt = s->comm->wait_for_recv(tout);
    Py_END_ALLOW_THREADS
    return PyLong_FromLong(wt);
}

template<typename T>
static PyObject* _get_pyfunc_array(const std::string& desc,
				   std::vector<T*>& varVect) {
  PyObject* out = PyList_New(varVect.size());
  if (out == NULL)
    return out;
  for (size_t i = 0; i < varVect.size(); i++) {
    PyObject* ival = varVect[i]->getPython();
    if (!ival) {
      PyErr_Format(PyExc_TypeError, "%s value %ld is not a Python callable",
		   desc.c_str(), i);
      Py_DECREF(out);
      return NULL;
    }
    Py_INCREF(ival);
    if (PyList_SetItem(out, i, ival) < 0) {
      Py_DECREF(out);
      return NULL;
    }
  }
  return out;
}

static PyObject* Comm_t___getstate__(PyObject* self, PyObject*) {
  pyComm_t* s = (pyComm_t*)self;
  if (s->comm->is_open()) {
    PyErr_Format(PyExc_TypeError, "Cannot pickle an open communicator");
    return NULL;
  }
  PyObject* metadataPy = s->comm->getMetadata().metadata.GetPythonObjectRaw();
  PyObject* filterPy = _get_pyfunc_array("filter",
					 s->comm->getMetadata().filters);
  PyObject* transformPy = _get_pyfunc_array("transform",
					    s->comm->getMetadata().transforms);
  if (metadataPy == NULL || filterPy == NULL || transformPy == NULL)
    return NULL;
  PyObject *ret = Py_BuildValue("{sssssisisisLsisOsOsOsi}",
				"name", s->comm->getName().c_str(),
				"address", s->comm->getAddress().c_str(),
				"direction", static_cast<int>(s->comm->getDirection()),
				"commtype", static_cast<int>(s->comm->getCommType()),
				"flags", s->comm->getFlags(),
				"recv_timeout", static_cast<long long>(s->comm->get_timeout_recv()),
				"language", static_cast<int>(s->comm->getLanguage()),
				"metadata", metadataPy,
				"filter", filterPy,
				"transform", transformPy,
				PICKLE_VERSION_KEY, PICKLE_VERSION);
  if (ret != NULL && (s->comm->getFlags() & (COMM_FLAG_CLIENT | COMM_FLAG_SERVER))) {
    YggInterface::communicator::RPCComm* rpc_comm = dynamic_cast<YggInterface::communicator::RPCComm*>(s->comm);
    YggInterface::communicator::RequestList& req = rpc_comm->getRequests();
    PyObject* res_metadataPy = req.response_metadata.metadata.GetPythonObjectRaw();
    PyObject* res_filterPy = _get_pyfunc_array("response filter",
					       req.response_metadata.filters);
    PyObject* res_transformPy = _get_pyfunc_array("response transform",
						  req.response_metadata.transforms);
    if (res_metadataPy == NULL || res_filterPy == NULL ||
	res_transformPy == NULL)
      return NULL;
    PyObject* res_kws = Py_BuildValue("{sisisOsOsO}",
				      "commtype", static_cast<int>(req.restype),
				      "flags", req.response_flags,
				      "metadata", res_metadataPy,
				      "filter", res_filterPy,
				      "transform", res_transformPy);
    if (PyDict_SetItemString(ret, "response_kwargs", res_kws) < 0) {
      Py_DECREF(ret);
      Py_DECREF(res_kws);
      return NULL;
    }
    Py_DECREF(res_kws);
    PyObject* flagsPy = PyLong_FromLong(static_cast<long>(s->comm->getFlags() & req.response_flags));
    if (PyDict_SetItemString(ret, "flags", flagsPy) < 0) {
      Py_DECREF(ret);
      Py_DECREF(flagsPy);
      return NULL;
    }
    Py_DECREF(flagsPy);
    PyObject* req_commtypePy = PyLong_FromLong(static_cast<long>(rpc_comm->wraptype));
    if (PyDict_SetItemString(ret, "request_commtype", req_commtypePy) < 0) {
      Py_DECREF(ret);
      Py_DECREF(req_commtypePy);
      return NULL;
    }
    Py_DECREF(req_commtypePy);
    PyObject* req_flagsPy = PyLong_FromLong(static_cast<long>(s->comm->getFlags()));
    if (PyDict_SetItemString(ret, "request_flags", req_flagsPy) < 0) {
      Py_DECREF(ret);
      Py_DECREF(req_flagsPy);
      return NULL;
    }
    Py_DECREF(req_flagsPy);
  }
  return ret;
}

static PyObject* Comm_t___setstate__(PyObject* self, PyObject* state) {
  if (!PyDict_CheckExact(state)) {
    PyErr_SetString(PyExc_ValueError, "Pickled object is not a dict.");
    return NULL;
  }
  /* Version check. */
  PyObject *temp = PyDict_GetItemString(state, PICKLE_VERSION_KEY);
  if (temp == NULL) {
    PyErr_Format(PyExc_KeyError, "No \"%s\" in pickled dict.",
		 PICKLE_VERSION_KEY);
    return NULL;
  }
  int pickle_version = (int) PyLong_AsLong(temp);
  if (pickle_version != PICKLE_VERSION) {
    PyErr_Format(PyExc_ValueError,
		 "Pickle version mismatch. Got version %d but expected version %d.",
		 pickle_version, PICKLE_VERSION);
    return NULL;
  }
  if (PyDict_DelItemString(state, PICKLE_VERSION_KEY) < 0) {
    return NULL;
  }
  if (PyDict_SetItemString(state, "dont_open", Py_True) < 0) {
    return NULL;
  }
  PyObject* args = PyTuple_New(0);
  if (Comm_t_init(self, args, state) < 0)
    return NULL;
  Py_DECREF(args);
  Py_RETURN_NONE;
}

static PyObject* Comm_t_richcompare(PyObject *self, PyObject *other, int op) {
  switch (op) {
  case (Py_EQ):
  case (Py_NE): {
    pyComm_t* s = (pyComm_t*)self;
    if (!PyObject_IsInstance(other, (PyObject*)(&Comm_tType))) {
      if (op == Py_EQ) {
	Py_RETURN_FALSE;
      } else {
	Py_RETURN_TRUE;
      }
    }
    pyComm_t* v = (pyComm_t*)other;
    if ((op == Py_EQ) &&
	(*(s->comm) == *(v->comm))) {
      Py_RETURN_TRUE;
    } else {
      Py_RETURN_FALSE;
    }
  }
  default: {
    Py_INCREF(Py_NotImplemented);
    return Py_NotImplemented;
  }
  }
}

static int _set_vector_str(const std::string& desc,
			   PyObject* src, std::vector<std::string>& dst) {
  if (src == NULL)
    return 0;
  if (!PyList_Check(src)) {
    PyErr_Format(PyExc_TypeError, "%s must be a list", desc.c_str());
    return -1;
  }
  for (Py_ssize_t i = 0; i < PyList_Size(src); i++) {
    PyObject* ielePy = PyList_GetItem(src, i);
    if (ielePy == NULL)
      return -1;
    if (!PyUnicode_Check(ielePy)) {
      PyErr_Format(PyExc_TypeError, "%s all elements must be strings (%d is not)",
		   desc.c_str(), i);
      return -1;
    }
    Py_ssize_t iele_len = 0;
    const char* iele = PyUnicode_AsUTF8AndSize(ielePy, &iele_len);
    if (!iele) {
      return -1;
    }
    dst.emplace_back(iele, static_cast<size_t>(iele_len));
  }
  return 0;
}

static PyObject* _get_vector_str(const std::vector<std::string>& src) {
  PyObject* out = PyList_New(src.size());
  Py_ssize_t i = 0;
  for (std::vector<std::string>::const_iterator it = src.begin();
       it != src.end(); it++, i++) {
    PyObject* iout = PyUnicode_FromStringAndSize(it->c_str(),
						 static_cast<Py_ssize_t>(it->size()));
    if (!iout) {
      Py_DECREF(out);
      return NULL;
    }
    if (PyList_SetItem(out, i, iout) < 0) {
      Py_DECREF(out);
      return NULL;
    }
  }
  return out;
}

static PyObject* Comm_t_get_status_message(PyObject *self, PyObject* args, PyObject* kwargs) {
  pyComm_t* s = (pyComm_t*)self;
  int nindent = 0;
  PyObject* extra_lines_beforePy = NULL;
  PyObject* extra_lines_afterPy = NULL;
  std::vector<std::string> extra_lines_before, extra_lines_after;
  static char const* kwlist[] = {
    "nindent",
    "extra_lines_before",
    "extra_lines_after",
    NULL
  };
  if (!PyArg_ParseTupleAndKeywords(args, kwargs,
				   "|iOO", (char**)kwlist,
				   &nindent,
				   &extra_lines_beforePy,
				   &extra_lines_afterPy))
    return NULL;
  if (_set_vector_str("extra_lines_before", extra_lines_beforePy,
		      extra_lines_before) < 0)
    return NULL;
  if (_set_vector_str("extra_lines_after", extra_lines_afterPy,
		      extra_lines_after) < 0)
    return NULL;
  std::vector<std::string> res = s->comm->get_status_message(
    nindent, extra_lines_before, extra_lines_after);
  return _get_vector_str(res);
}
static PyObject* Comm_t_printStatus(PyObject *self, PyObject* args, PyObject* kwargs) {
  pyComm_t* s = (pyComm_t*)self;
  int nindent = 0;
  PyObject* extra_lines_beforePy = NULL;
  PyObject* extra_lines_afterPy = NULL;
  std::vector<std::string> extra_lines_before, extra_lines_after;
  static char const* kwlist[] = {
    "nindent",
    "extra_lines_before",
    "extra_lines_after",
    NULL
  };
  if (!PyArg_ParseTupleAndKeywords(args, kwargs,
				   "|iOO", (char**)kwlist,
				   &nindent,
				   &extra_lines_beforePy,
				   &extra_lines_afterPy))
    return NULL;
  if (_set_vector_str("extra_lines_before", extra_lines_beforePy,
		      extra_lines_before) < 0)
    return NULL;
  if (_set_vector_str("extra_lines_after", extra_lines_afterPy,
		      extra_lines_after) < 0)
    return NULL;
  std::string res = s->comm->printStatus(
    nindent, extra_lines_before, extra_lines_after);
  return PyUnicode_FromStringAndSize(res.c_str(),
				     static_cast<Py_ssize_t>(res.size()));
}

// PyObject* Comm_t_getType(pyComm_t* self) {
//     COMM_TYPE ct = self->comm->getType();
//     return PyLong_FromLong(ct);
// }

static PyObject* Comm_t_name_get(PyObject* self, void*) {
  pyComm_t* s = (pyComm_t*)self;
  return PyUnicode_FromString(s->comm->getName().c_str());
}
static PyObject* Comm_t_address_get(PyObject* self, void*) {
  pyComm_t* s = (pyComm_t*)self;
  return PyUnicode_FromString(s->comm->getAddress().c_str());
}
static PyObject* Comm_t_direction_get(PyObject* self, void*) {
  pyComm_t* s = (pyComm_t*)self;
  return PyLong_FromLong(s->comm->getDirection());
  // Return string?
  // char* dirStr = "NONE";
  // if (s->comm->getDirection() == DIRECTION::RECV)
  //   dirStr = "RECV";
  // else if (s->comm->getDirection() == DIRECTION::SEND)
  //   dirStr = "SEND";
  // return PyUnicode_FromString(dirStr);
}
static PyObject* Comm_t_commtype_get(PyObject* self, void*) {
  pyComm_t* s = (pyComm_t*)self;
  return PyLong_FromLong(s->comm->getCommType());
}

PyObject* Comm_t_metadata_get(PyObject* self, void*) {
  // Uneditable version
  // YggInterface::utils::Metadata& metadata = ((pyComm_t*)self)->comm->getMetadata();
  // PyObject* out = metadata.getMeta().GetPythonObjectRaw();
  // if (out == NULL) {
  //   PyErr_SetString(PyExc_TypeError, "Error converting metadata to a Python object");
  // }
  PyObject* args = PyTuple_Pack(1, self);
  if (args == NULL)
    return NULL;
  PyObject* out = PyObject_Call((PyObject*)(&commMetaType), args, NULL);
  Py_DECREF(args);
  return out;
}

int Comm_t_metadata_set(PyObject* self, PyObject* value, void*) {
  if (!PyDict_Check(value)) {
    PyErr_SetString(PyExc_TypeError, "Metadata must be a dictionary");
    return -1;
  }
  rapidjson::Document doc;
  doc.SetPythonObjectRaw(value, doc.GetAllocator());
  if (!doc.IsObject()) {
    PyErr_SetString(PyExc_TypeError, "Error converting provided dictionary to a rapidjson Object");
    return -1;
  }
  if (!((pyComm_t*)self)->comm->addSchema(doc, true)) {
    PyErr_SetString(PyExc_TypeError, "Error updating metadata");
    return -1;
  }
  return 0;
}

PyObject* Comm_t_datatype_get(PyObject* self, void*) {
  // Uneditable version
  // YggInterface::utils::Metadata& metadata = ((pyComm_t*)self)->comm->getMetadata();
  // PyObject* out = metadata.getSchema()->GetPythonObjectRaw();
  // if (out == NULL) {
  //   PyErr_SetString(PyExc_TypeError, "Error converting datatype to a Python object");
  // }
  PyObject* args = PyTuple_Pack(1, self);
  if (args == NULL)
    return NULL;
  PyObject* kwargs = PyDict_New();
  if (kwargs == NULL) {
    Py_DECREF(args);
    return NULL;
  }
  if (PyDict_SetItemString(kwargs, "for_datatype", Py_True) < 0) {
    Py_DECREF(args);
    Py_DECREF(kwargs);
    return NULL;
  }
  PyObject* out = PyObject_Call((PyObject*)(&commMetaType), args, kwargs);
  Py_DECREF(args);
  Py_DECREF(kwargs);
  return out;
}

int Comm_t_datatype_set(PyObject* self, PyObject* value, void*) {
  if (!PyDict_Check(value)) {
    PyErr_SetString(PyExc_TypeError, "Datatype must be a dictionary");
    return -1;
  }
  rapidjson::Document doc;
  doc.SetPythonObjectRaw(value, doc.GetAllocator());
  if (!doc.IsObject()) {
    PyErr_SetString(PyExc_TypeError, "Error converting provided dictionary to a rapidjson Object");
    return -1;
  }
  if (!((pyComm_t*)self)->comm->addSchema(doc, false)) {
    PyErr_SetString(PyExc_TypeError, "Error updating datatype");
    return -1;
  }
  return 0;
}

static PyObject* Comm_t_timeout_recv_get(PyObject* self, void*) {
  pyComm_t* s = (pyComm_t*)self;
  return PyLong_FromLong(s->comm->get_timeout_recv());
}
static int Comm_t_timeout_recv_set(PyObject* self, PyObject* value, void*) {
  if (!PyLong_Check(value)) {
    PyErr_SetString(PyExc_TypeError, "Timeout must be an integer");
    return -1;
  }
  pyComm_t* s = (pyComm_t*)self;
  s->comm->set_timeout_recv(PyLong_AsLong(value));
  return 0;
}
static PyObject* Comm_t_maxMsgSize_get(PyObject* self, void*) {
  pyComm_t* s = (pyComm_t*)self;
  return PyLong_FromLong(static_cast<int>(s->comm->getMaxMsgSize()));
}
static PyObject* Comm_t_is_open_get(PyObject* self, void*) {
  pyComm_t* s = (pyComm_t*)self;
  if (s->comm->is_open()) {
    Py_RETURN_TRUE;
  }
  Py_RETURN_FALSE;
}
static PyObject* Comm_t_is_closed_get(PyObject* self, void*) {
  pyComm_t* s = (pyComm_t*)self;
  if (s->comm->is_closed()) {
    Py_RETURN_TRUE;
  }
  Py_RETURN_FALSE;
}
static PyObject* Comm_t_n_msg_get(PyObject* self, void*) {
  return PyLong_FromLong(((pyComm_t*)self)->comm->comm_nmsg());
}
static PyObject* Comm_t_is_interface_get(PyObject* self, void*) {
  pyComm_t* s = (pyComm_t*)self;
  if (s->comm->getFlags() & COMM_FLAG::COMM_FLAG_INTERFACE) {
    Py_RETURN_TRUE;
  }
  Py_RETURN_FALSE;
}


PyDoc_STRVAR(is_comm_installed_docstring,
	     "is_comm_installed(commtype)\n"
	     "\n"
	     "Check if a communicator type is installed.");

static PyObject* is_comm_installed(PyObject*, PyObject* args) {
  int commtype = 0;
  if (!PyArg_ParseTuple(args, "i", &commtype)) {
    return NULL;
  }
  if(commtype < 0 || commtype > (int)(max_enum_value(COMM_TYPE_map))) {
    PyErr_SetString(PyExc_TypeError, "Invalid commtype");
    return NULL;
  }
  if (YggInterface::communicator::is_commtype_installed((COMM_TYPE)commtype)) {
    Py_RETURN_TRUE;
  }
  Py_RETURN_FALSE;
}
#endif // DOXYGEN_SHOULD_SKIP_THIS
