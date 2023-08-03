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


// static PyObject* Comm_t_sendVar(pyComm_t* self, PyObject* arg);
// static PyObject* Comm_t_vRecv(pyComm_t* self, PyObject* arg);
// static PyObject* Comm_t_vSend(pyComm_t* self, PyObject* arg);

// MAYBE
// static PyObject* Comm_t_addSchema(pyComm_t* self, PyObject* arg);
// static PyObject* Comm_t_addFormat(pyComm_t* self, PyObject* arg);

// static PyObject* Comm_t_sendVar(pyComm_t* self, PyObject* arg);

// static PyObject* Comm_t_recVar(pyComm_t* self, PyObject* arg);

static PyObject* Comm_t_comm_nmsg(pyComm_t* self);
static PyObject* Comm_t_close(pyComm_t* self);
static PyObject* Comm_t_is_closed(pyComm_t* self);
// static PyObject* Comm_t_create_worker(pyComm_t* self, PyObject* arg);
// static PyObject* Comm_t_send_single(pyComm_t* self, PyObject* arg);
// static PyObject* Comm_t_recv_single(pyComm_t* self, PyObject* arg);
// static PyObject* Comm_t_recv(pyComm_t* self, PyObject* arg);
static PyObject* Comm_t_str(pyComm_t* self);

static PyMethodDef Comm_t_methods[] = {
        {"comm_nmsg", (PyCFunction) Comm_t_comm_nmsg, METH_NOARGS, ""},
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
        0,                         /* tp_getset */
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

// PyObject* Comm_t_send(pyComm_t* self, PyObject* arg) {

//     //int send(const char *data, const size_t &len)
//     //int send(const int nargs, ...);

// }

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


// long recv(char*& data, const size_t &len, bool allow_realloc)
// long recv(const int nargs, ...);
// long recv(std::string& data)


// int sendVar(const rapidjson::Document& data)
// int sendVar(const rapidjson::Ply& data)
// int sendVar(const rapidjson::ObjWavefront& data)

// long vRecv(rapidjson::VarArgList& ap);
// int vSend(rapidjson::VarArgList& ap);

// void addSchema(const utils::Metadata& s);
// void addSchema(const rapidjson::Value& s, bool isMetadata = false);
// void addSchema(const std::string& schemaStr, bool isMetadata = false);
// void addFormat(const std::string& format_str, bool as_array = false);
