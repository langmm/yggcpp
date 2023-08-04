#ifndef PY_SSIZE_T_CLEAN
#define PY_SSIZE_T_CLEAN
#endif
#include <Python.h>
#include "communicators/CommBase.hpp"
#include "pyUtils.hpp"

// TODO:
// Static properties:
// - address
// - communicator type


typedef struct {
    PyObject_HEAD
    communication::communicator::Comm_t *comm;
} pyComm_t;

//////////////////////////////////////////////////////////////
// Forward Declarations
//////////////////////////////////////////////////////////////

static void Comm_t_dealloc(pyComm_t* self);
static int Comm_t_init(pyComm_t* self, PyObject* args, PyObject* kwds);
static PyObject* Comm_t_new(PyTypeObject *type, PyObject* args, PyObject* kwds);
static PyObject* Comm_t_send(pyComm_t* self, PyObject* arg);
static PyObject* Comm_t_recv(pyComm_t* self, PyObject* arg);
static PyObject* Comm_t_send_eof(pyComm_t* self);
static PyObject* Comm_t_set_timeout_recv(pyComm_t* self, PyObject* arg);
static PyObject* Comm_t_wait_for_recv(pyComm_t* self, PyObject* arg);
static PyObject* Comm_t_is_open(pyComm_t* self);
// static PyObject* Comm_t_getType(pyComm_t* self);
static PyObject* Comm_t_comm_nmsg(pyComm_t* self);
static PyObject* Comm_t_close(pyComm_t* self);
static PyObject* Comm_t_is_closed(pyComm_t* self);
// static PyObject* Comm_t_create_worker(pyComm_t* self, PyObject* arg);
// static PyObject* Comm_t_send_single(pyComm_t* self, PyObject* arg);
// static PyObject* Comm_t_recv_single(pyComm_t* self, PyObject* arg);
static PyObject* Comm_t_str(pyComm_t* self);
// TODO: Replace this with a class so updating the returned dict updates
//   the C++ class?
static PyObject* Comm_t_metadata_get(PyObject* self, void*);
static int Comm_t_metadata_set(PyObject* self, PyObject* value, void* closure);
static PyObject* Comm_t_datatype_get(PyObject* self, void*);
static int Comm_t_datatype_set(PyObject* self, PyObject* value, void* closure);

static PyObject* commMeta_new(PyTypeObject *type, PyObject* args, PyObject* kwds);
static int commMeta_init(PyObject* self, PyObject* args, PyObject* kwds);
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

static PyTypeObject commMetaType = {
        PyVarObject_HEAD_INIT(NULL, 0)
        "pyYggdrasil.CommMeta",     /* tp_name */
        sizeof(commMeta),           /* tp_basicsize */
        0,                          /* tp_itemsize */
        (destructor)commMeta_dealloc, /* tp_dealloc */
        0,                         /* tp_vectorcall_offset */
        0,                         /* tp_getattr */
        0,                         /* tp_setattr */
        0,                         /* tp_reserved */
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
        (initproc)commMeta_init,   /* tp_init */
        0,                         /* tp_alloc */
        commMeta_new,              /* tp_new */
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

PyObject* commMeta_new(PyTypeObject *type, PyObject*, PyObject*) {
  return type->tp_alloc(type, 0);
}
int commMeta_init(PyObject* self, PyObject* args, PyObject* kwds) {
  commMeta* s = (commMeta*)self;
  s->comm = NULL;
  s->v = NULL;
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
    return -1;
  pyComm_t* comm = (pyComm_t*)commPy;
  if (for_datatype < 0) for_datatype = 0;
  rapidjson::Value* v = NULL;
  try {
    if (for_datatype)
      v = &(comm->comm->getMetadata().getSchema());
    else
      v = &(comm->comm->getMetadata().getMeta());
  } catch (...) {
    // TODO: More specific error
    PyErr_SetString(PyExc_KeyError, "schema");
    return -1;
  }
  Py_INCREF(commPy);
  s->comm = commPy;
  s->v = (void*)v;
  return 0;
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
    return NULL;
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
        {"n_msg", (PyCFunction) Comm_t_comm_nmsg, METH_NOARGS, ""},
        {"send", (PyCFunction) Comm_t_send, METH_VARARGS, ""},
        {"close", (PyCFunction) Comm_t_close, METH_NOARGS, ""},
        {"is_closed", (PyCFunction) Comm_t_is_closed, METH_NOARGS, ""},
        // {"create_worker", (PyCFunction) Comm_t_create_worker, METH_VARARGS, ""},
        // {"send_single", (PyCFunction) Comm_t_send_single, METH_VARARGS, ""},
        // {"recv_single", (PyCFunction) Comm_t_recv_single, METH_VARARGS, ""},
        {"recv", (PyCFunction) Comm_t_recv, METH_VARARGS, ""},
        {"send_eof", (PyCFunction) Comm_t_send_eof, METH_NOARGS, ""},
        {"set_timeout_recv", (PyCFunction) Comm_t_set_timeout_recv, METH_VARARGS, ""},
        {"wait_for_recv", (PyCFunction) Comm_t_wait_for_recv, METH_VARARGS, ""},
        {"is_open", (PyCFunction) Comm_t_is_open, METH_NOARGS, ""},
        {NULL, NULL, 0, ""}  /* Sentinel */
};

static PyGetSetDef Comm_t_properties[] = {
  {"metadata", Comm_t_metadata_get, Comm_t_metadata_set,
   "The metadata associated with the communicator that will be sent in "
   "message headers", NULL},
  {"datatype", Comm_t_datatype_get, Comm_t_datatype_set,
   "The datatype associated with the communicator that will be sent in "
   "message headers and used to validate messages", NULL},
  {NULL} /* Sentinel */
};

static PyTypeObject Comm_tType = {
        PyVarObject_HEAD_INIT(NULL, 0)
        "pyYggdrasil.Comm_t",     /* tp_name */
        sizeof(pyComm_t),         /* tp_basicsize */
        0,                         /* tp_itemsize */
        (destructor)Comm_t_dealloc, /* tp_dealloc */
        0,                         /* tp_vectorcall_offset */
        0,                         /* tp_getattr */
        0,                         /* tp_setattr */
        0,                         /* tp_reserved */
        (reprfunc)Comm_t_str,     /* tp_repr */
        0,                         /* tp_as_number */
        0,                         /* tp_as_sequence */
        0,                         /* tp_as_mapping */
        0,                         /* tp_hash  */
        0,                         /* tp_call */
        (reprfunc)Comm_t_str,     /* tp_str */
        0,                         /* tp_getattro */
        0,                         /* tp_setattro */
        0,                         /* tp_as_buffer */
        Py_TPFLAGS_DEFAULT,        /* tp_flags */
        "Comm_t object",          /* tp_doc */
        0,                         /* tp_traverse */
        0,                         /* tp_clear */
        0,                         /* tp_richcompare */
        0,                         /* tp_weaklistoffset */
        0,                         /* tp_iter */
        0,                         /* tp_iternext */
        Comm_t_methods,           /* tp_methods */
        0,                         /* tp_members */
        Comm_t_properties,        /* tp_getset */
        0,                         /* tp_base */
        0,                         /* tp_dict */
        0,                         /* tp_descr_get */
        0,                         /* tp_descr_set */
        0,                         /* tp_dictoffset */
        (initproc)Comm_t_init,    /* tp_init */
        0,                         /* tp_alloc */
        Comm_t_new,               /* tp_new */
        0,                         /* tp_free */
        NULL,                      /* tp_is_gc */
        NULL,                      /* tp_bases */
        NULL,                      /* tp_mro */
        NULL,                      /* tp_cache */
        NULL,                      /* tp_subclasses */
        NULL,                      /*tp_weaklist */
        0,                         /* tp_del */
        0,                         /* tp_version_tag */
        0,                         /* tp_finalize */
        0                          /*tp_vectorcall */

};


static void Comm_t_dealloc(pyComm_t* self) {
    delete self->comm;
    Py_TYPE(self)->tp_free((PyObject*) self);
}

static int Comm_t_init(pyComm_t* self, PyObject* args, PyObject* kwds) {
    (void)kwds;
    PyObject* adr = NULL;
    char* name = NULL;
    int name_size;
    int dirn = -1;
    int commtype = -1;
    int flags = 0;
    self->comm = NULL;

    if(!PyArg_ParseTuple(args, "|z#Oiii", &name, &name_size, &adr, &dirn, &commtype, &flags)) {
        PyErr_SetString(PyExc_TypeError, "Invalid arguments");
        return -1;
    }
    if(adr == NULL and name == NULL) {
        PyErr_SetString(PyExc_TypeError, "");
        return -1;
    }
    if(dirn < 0 || dirn > DIRECTION::RECV) {
        PyErr_SetString(PyExc_TypeError, "");
        return -1;
    }
    if(commtype < 0 || commtype > COMM_TYPE::MPI_COMM) {
        PyErr_SetString(PyExc_TypeError, "");
        return -1;
    }
    if(flags < 0) {
        PyErr_SetString(PyExc_TypeError, "");
        return -1;
    }
    self->comm = communication::communicator::new_Comm_t(
		       (DIRECTION)dirn, (COMM_TYPE)commtype,
		       name, (communication::utils::Address*)adr,
		       flags);
    return 0;
}

static PyObject* Comm_t_new(PyTypeObject *type, PyObject* args, PyObject* kwds) {
    (void)args;
    (void)kwds;
    pyComm_t *self;
    self = (pyComm_t*)type->tp_alloc(type, 0);

    return (PyObject*)self;
}

PyObject* Comm_t_comm_nmsg(pyComm_t* self) {
    return PyLong_FromLong(self->comm->comm_nmsg());
}

PyObject* Comm_t_send(pyComm_t* self, PyObject* arg) {
    rapidjson::Document doc;
    doc.SetPythonObjectRaw(arg, doc.GetAllocator());
    if (self->comm->sendVar(doc) < 0)
      Py_RETURN_FALSE;
    Py_RETURN_TRUE;
}

PyObject* Comm_t_recv(pyComm_t* self, PyObject*) {
    rapidjson::Document doc;
    long flag = self->comm->recvVar(doc);
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
    return PyTuple_Pack(2, pyFlag, res);
}

PyObject* Comm_t_close(pyComm_t* self) {
    self->comm->close();
    return NULL;
}

PyObject* Comm_t_is_closed(pyComm_t* self) {
    if (self->comm->is_closed())
        Py_RETURN_TRUE;
    Py_RETURN_FALSE;

}

// PyObject* Comm_t_create_worker(pyComm_t* self, PyObject* arg) {
// }

// PyObject* Comm_t_send_single(pyComm_t* self, PyObject* arg) {
// }

// PyObject* Comm_t_recv_single(pyComm_t* self, PyObject* arg) {
// }

// PyObject* Comm_t_recv(pyComm_t* self, PyObject* arg) {
// }

PyObject* Comm_t_str(pyComm_t*) {
    return PyUnicode_FromFormat("%s", "Comm_t");
}

PyObject* Comm_t_send_eof(pyComm_t* self) {
    return PyLong_FromLong(self->comm->send_eof());
}

PyObject* Comm_t_set_timeout_recv(pyComm_t* self, PyObject* arg) {
    int new_timeout;
    if(!PyArg_ParseTuple(arg, "i", &new_timeout)) {
        PyErr_SetString(PyExc_TypeError, "Invalid argument given.");
        return NULL;
    }
    self->comm->set_timeout_recv(new_timeout);
    return NULL;
}

PyObject* Comm_t_wait_for_recv(pyComm_t* self, PyObject* arg) {
    int tout;
    if(!PyArg_ParseTuple(arg, "i", &tout)) {
        PyErr_SetString(PyExc_TypeError, "Invalid argument given.");
        return NULL;
    }
    int wt = self->comm->wait_for_recv(tout);
    return PyLong_FromLong(wt);
}

PyObject* Comm_t_is_open(pyComm_t* self) {
    if(self->comm->is_open())
        Py_RETURN_TRUE;
    Py_RETURN_FALSE;
}

// PyObject* Comm_t_getType(pyComm_t* self) {
//     COMM_TYPE ct = self->comm->getType();
//     return PyLong_FromLong(ct);
// }

PyObject* Comm_t_metadata_get(PyObject* self, void*) {
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
