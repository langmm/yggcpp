#ifndef PY_SSIZE_T_CLEAN
#define PY_SSIZE_T_CLEAN
#endif
#include <Python.h>
#include "communicators/CommBase.hpp"

typedef struct {
    PyObject_HEAD
    communication::communicator::Comm_t *comm;
} pyComm_t;

//////////////////////////////////////////////////////////////
// Forward Declarations
//////////////////////////////////////////////////////////////

static void Comm_t_dealloc(PyObject* self);
static int Comm_t_init(PyObject* self, PyObject* args, PyObject* kwds);
static PyObject* Comm_t_new(PyTypeObject *type, PyObject* args, PyObject* kwds);
static PyObject* Comm_t_send(PyObject* self, PyObject* arg);
static PyObject* Comm_t_recv(PyObject* self, PyObject* arg);
static PyObject* Comm_t_send_eof(PyObject* self, PyObject* arg);
static PyObject* Comm_t_call(PyObject* self, PyObject* arg);
static PyObject* Comm_t_set_timeout_recv(PyObject* self, PyObject* arg);
static PyObject* Comm_t_wait_for_recv(PyObject* self, PyObject* arg);
static PyObject* Comm_t_close(PyObject* self, PyObject* arg);
// static PyObject* Comm_t_create_worker(PyObject* self, PyObject* arg);
// static PyObject* Comm_t_send_single(PyObject* self, PyObject* arg);
// static PyObject* Comm_t_recv_single(PyObject* self, PyObject* arg);
static PyObject* Comm_t_str(PyObject* self);
// TODO: Replace this with a class so updating the returned dict updates
//   the C++ class?
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
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-function-type"
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
#pragma GCC diagnostic pop
#endif

static PyTypeObject commMetaType = {
        PyVarObject_HEAD_INIT(NULL, 0)
        "pyYggdrasil.CommMeta",    /* tp_name */
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
  
  try {
    if (for_datatype)
      v = &(comm->comm->getMetadata().getSchema());
    else
      v = &(comm->comm->getMetadata().metadata);
  } catch (...) {
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

static PyMethodDef Comm_t_methods[] = {
        {"send", (PyCFunction) Comm_t_send, METH_VARARGS, ""},
        {"close", (PyCFunction) Comm_t_close, METH_NOARGS, ""},
        // {"create_worker", (PyCFunction) Comm_t_create_worker, METH_VARARGS, ""},
        // {"send_single", (PyCFunction) Comm_t_send_single, METH_VARARGS, ""},
        // {"recv_single", (PyCFunction) Comm_t_recv_single, METH_VARARGS, ""},
        {"recv", (PyCFunction) Comm_t_recv, METH_VARARGS, ""},
        {"send_eof", (PyCFunction) Comm_t_send_eof, METH_NOARGS, ""},
	{"call", (PyCFunction) Comm_t_call, METH_VARARGS, ""},
        {"set_timeout_recv", (PyCFunction) Comm_t_set_timeout_recv, METH_VARARGS, ""},
        {"wait_for_recv", (PyCFunction) Comm_t_wait_for_recv, METH_VARARGS, ""},
        {NULL, NULL, 0, ""}  /* Sentinel */
};

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
  {NULL, NULL, NULL, NULL, NULL} /* Sentinel */
};

static PyTypeObject Comm_tType = {
        PyVarObject_HEAD_INIT(NULL, 0)
        "pyYggdrasil.Comm_t",      /* tp_name */
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
        0,                         /* tp_richcompare */
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

};


static void Comm_t_dealloc(PyObject* self) {
    pyComm_t* s = (pyComm_t*)self;
    delete s->comm;
    Py_TYPE(self)->tp_free(self);
}

static int Comm_t_init(PyObject* self, PyObject* args, PyObject* kwds) {
    pyComm_t* s = (pyComm_t*)self;
    char* adr = NULL;
    char* name = NULL;
    PyObject* dirnPy = NULL;
    PyObject* commtypePy = NULL;
    int dirn = DIRECTION::SEND;
    int commtype = COMM_TYPE::DEFAULT_COMM;
    int flags = 0;
    static char const* kwlist[] = {
      "name",
	"address",
	"direction",
	"commtype",
	"flags",
	NULL
    };
    s->comm = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "|ssO$Oi",
				     (char**) kwlist,
				     &name, &adr, &dirnPy,
				     &commtypePy, &flags))
      return -1;

#define CHECK_ENUM_STR(src, dst, base, str, num)	\
    if ((strcmp(src, #str) == 0) ||			\
	(strcmp(src, #num) == 0)) {			\
      dst = base::num;					\
    }

    if (dirnPy != NULL) {
      if (PyLong_Check(dirnPy)) {
	dirn = PyLong_AsLong(dirnPy);
      } else if (PyUnicode_Check(dirnPy)) {
	const char* dirnStr = PyUnicode_AsUTF8(dirnPy);
	if (!dirnStr)
	  return -1;
	CHECK_ENUM_STR(dirnStr, dirn, DIRECTION, recv, RECV)
	else CHECK_ENUM_STR(dirnStr, dirn, DIRECTION, send, SEND)
	else {
	  PyErr_SetString(PyExc_TypeError, "Invalid direction");
	  return -1;
	}
      } else {
	PyErr_SetString(PyExc_TypeError, "direction must be a string or an integer.");
	return -1;
      }
    }
    if (commtypePy != NULL) {
      if (PyLong_Check(commtypePy)) {
	commtype = PyLong_AsLong(commtypePy);
      } else if (PyUnicode_Check(commtypePy)) {
	const char* commtypeStr = PyUnicode_AsUTF8(commtypePy);
	if (!commtypeStr)
	  return -1;
	CHECK_ENUM_STR(commtypeStr, commtype, COMM_TYPE, default, DEFAULT_COMM)
	else CHECK_ENUM_STR(commtypeStr, commtype, COMM_TYPE, ipc, IPC_COMM)
	else CHECK_ENUM_STR(commtypeStr, commtype, COMM_TYPE, zmq, ZMQ_COMM)
	else CHECK_ENUM_STR(commtypeStr, commtype, COMM_TYPE, mpi, MPI_COMM)
	else CHECK_ENUM_STR(commtypeStr, commtype, COMM_TYPE, server, SERVER_COMM)
	else CHECK_ENUM_STR(commtypeStr, commtype, COMM_TYPE, client, CLIENT_COMM)
	else {
	  PyErr_SetString(PyExc_TypeError, "Invalid commtype");
	  return -1;
	}
      } else {
	PyErr_SetString(PyExc_TypeError, "commtype must be a string or an integer.");
	return -1;
      }
    }
    
    if(adr == NULL && name == NULL) {
        PyErr_SetString(PyExc_TypeError, "Neither name nor address provided");
        return -1;
    }
    if(dirn < 0 || dirn > DIRECTION::RECV) {
        PyErr_SetString(PyExc_TypeError, "Invalid direction");
        return -1;
    }
    if(commtype < 0 || commtype > COMM_TYPE::MPI_COMM) {
        PyErr_SetString(PyExc_TypeError, "Invalid commtype");
        return -1;
    }
    if(flags < 0) {
        PyErr_SetString(PyExc_TypeError, "Invalid flags value");
        return -1;
    }
    try {
      s->comm = communication::communicator::new_Comm_t(
		       (DIRECTION)dirn, (COMM_TYPE)commtype,
		       name, adr, flags);
    } catch (...) {
      s->comm = NULL;
    }
    if (!s->comm) {
      PyErr_SetString(PyExc_TypeError, "Error initializing comm");
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

PyObject* Comm_t_close(PyObject* self, PyObject*) {
    ((pyComm_t*)self)->comm->close();
    Py_RETURN_NONE;
}

// PyObject* Comm_t_create_worker(PyObject* self, PyObject* arg) {
// }

// PyObject* Comm_t_send_single(PyObject* self, PyObject* arg) {
// }

// PyObject* Comm_t_recv_single(PyObject* self, PyObject* arg) {
// }

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

PyObject* Comm_t_send_eof(PyObject* self, PyObject*) {
  return PyLong_FromLong(((pyComm_t*)self)->comm->send_eof());
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
  // communication::utils::Metadata& metadata = ((pyComm_t*)self)->comm->getMetadata();
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
  try {
    ((pyComm_t*)self)->comm->addSchema(doc, true);
  } catch (...) {
    PyErr_SetString(PyExc_TypeError, "Error updating metadata");
    return -1;
  }
  return 0;
}

PyObject* Comm_t_datatype_get(PyObject* self, void*) {
  // Uneditable version
  // communication::utils::Metadata& metadata = ((pyComm_t*)self)->comm->getMetadata();
  // PyObject* out = metadata.getSchema().GetPythonObjectRaw();
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
  try {
    ((pyComm_t*)self)->comm->addSchema(doc, false);
  } catch (...) {
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


PyDoc_STRVAR(is_comm_installed_docstring,
	     "is_comm_installed(commtype)\n"
	     "\n"
	     "Check if a communicator type is installed.");

static PyObject* is_comm_installed(PyObject*, PyObject* args) {
  int commtype = 0;
  if (!PyArg_ParseTuple(args, "i", &commtype)) {
    return NULL;
  }
  if(commtype < 0 || commtype > COMM_TYPE::MPI_COMM) {
    PyErr_SetString(PyExc_TypeError, "Invalid commtype");
    return NULL;
  }
  if (communication::communicator::is_commtype_installed((COMM_TYPE)commtype)) {
    Py_RETURN_TRUE;
  }
  Py_RETURN_FALSE;
}
