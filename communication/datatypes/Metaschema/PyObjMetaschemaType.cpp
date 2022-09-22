#include "PyObjMetaschemaType.hpp"
using namespace communication::datatypes::Metaschema;
using namespace communication::utils;


Dict* PyObjMetaschemaType::copy_generic(const YggGeneric* data, Dict* orig_data) const {
if (data == NULL) {
ygglog_throw_error("PyObjMetaschemaType::copy_generic: Generic object is NULL.");
}
void* out = NULL;
if (orig_data == NULL) {
orig_data = data->get_data();
}
if (orig_data != NULL) {
python_t* old_data = (python_t*)orig_data;
python_t* new_data = (python_t*)malloc(sizeof(python_t));
if (new_data == NULL) {
ygglog_throw_error("PyObjMetaschemaType::copy_generic: Failed to malloc memory for Python wrapper struct.");
}
new_data[0] = copy_python_t(*old_data);
out = (void*)new_data;
}
return out;
}

void PyObjMetaschemaType::free_generic(YggGeneric* data) const {
if (data == NULL) {
ygglog_throw_error("PyObjMetaschemaType::free_generic: Generic object is NULL.");
}
python_t** ptr = (python_t**)(data->get_data_pointer());
if (ptr[0] != NULL) {
free_python_t(ptr[0]);
free(ptr[0]);
ptr[0] = NULL;
}
}

size_t PyObjMetaschemaType::update_from_serialization_args(size_t *nargs, va_list_t &ap) {
size_t out = MetaschemaType::update_from_serialization_args(nargs, ap);
if (use_generic())
return out;
if (ap.using_ptrs) {
va_list_t_skip(&ap, 0);
} else {
va_arg(ap.va, python_t);
}
out++;
return out;
}

PyObject* PyObjMetaschemaType::import_python(const char* name) const {
    PyObject *py_class = import_python_class("yggdrasil.metaschema.datatypes.ClassMetaschemaType",
                                             "ClassMetaschemaType",
                                             "PyObjMetaschemaType::import_python: ");
    PyObject *py_function = PyObject_CallMethod(py_class, "decode_data",
                                                "ss", name, NULL);
    Py_DECREF(py_class);
    if (py_function == NULL) {
        ygglog_throw_error("PyObjMetaschemaType::import_python: Failed to import Python object: '%s'.", name);
    }
    return py_function;
}

YggGeneric* PyObjMetaschemaType::python2c(PyObject* pyobj) const {
YggGeneric* cobj = new YggGeneric(this, NULL, 0);
void** data = cobj->get_data_pointer();
python_t* idata = (python_t*)realloc(data[0], nbytes());
if (idata == NULL) {
ygglog_throw_error("PyObjMetaschemaType::python2c: Failed to realloc data.");
}
PyObject *py_class = import_python_class("yggdrasil.metaschema.datatypes.ClassMetaschemaType",
                                         "ClassMetaschemaType",
                                         "PyObjMetaschemaType::import_python: ");
PyObject *py_name = PyObject_CallMethod(py_class, "encode_data",
                                        "Os", pyobj, NULL);
Py_DECREF(py_class);
if (py_name == NULL) {
ygglog_throw_error("PyObjMetaschemaType::python2c: Failed to get function name.");
}
idata->name[0] = '\0';
idata->args = NULL;
idata->kwargs = NULL;
idata->obj = pyobj;
convert_python2c(py_name, &(idata->name), T_BYTES,
"PyObjMetaschemaType::python2c: ",
PYTHON_NAME_SIZE);
Py_DECREF(py_name);
data[0] = (void*)idata;
return cobj;
}

bool PyObjMetaschemaType::encode_data(rapidjson::Writer<rapidjson::StringBuffer> *writer,
                 size_t *nargs, va_list_t &ap) const {
size_t bytes_precision = PYTHON_NAME_SIZE;
python_t arg0;
if (ap.using_ptrs) {
arg0 = ((python_t*)get_va_list_ptr_cpp(&ap))[0];
} else {
arg0 = va_arg(ap.va, python_t);
}
if (strlen(arg0.name) < bytes_precision) {
bytes_precision = strlen(arg0.name);
}
(*nargs)--;
bool out = writer->String(arg0.name, (rapidjson::SizeType)bytes_precision);
return out;
}

bool PyObjMetaschemaType::decode_data(rapidjson::Value &data, const int allow_realloc,
                 size_t *nargs, va_list_t &ap) const {
if ((data.IsArray()) && (data.Size() == 1)) {
data = data[0];
}
if (!(data.IsString())) {
ygglog_error("PyObjMetaschemaType::decode_data: Raw data is not a string.");
return false;
}
unsigned char* encoded_bytes = (unsigned char*)data.GetString();
size_t encoded_len = data.GetStringLength();
size_t nbytes_expected = PYTHON_NAME_SIZE;
if (encoded_len > nbytes_expected) {
ygglog_error("PyObjMetaschemaType::decode_data: Python object name has a length %lu, but the max is %lu.",
encoded_len, nbytes_expected);
}
// Decode the object
python_t *arg;
python_t **p;
if (allow_realloc) {
if (ap.using_ptrs) {
p = (python_t**)get_va_list_ptr_ref_cpp(&ap);
} else {
p = va_arg(ap.va, python_t**);
}
python_t *temp;
if (ap.for_fortran) {
temp = p[0];
} else {
temp = (python_t *)realloc(p[0], sizeof(python_t));
}
if (temp == NULL) {
ygglog_throw_error("PyObjMetaschemaType::decode_data: Failed to realloc variable.");
}
p[0] = temp;
arg = *p;
} else {
if (ap.using_ptrs) {
arg = (python_t*)get_va_list_ptr_cpp(&ap);
} else {
arg = va_arg(ap.va, python_t*);
}
p = &arg;
}
(*nargs)--;
arg->name[0] = '\0';
strncpy(arg->name, (char*)encoded_bytes, nbytes_expected);
arg->args = NULL;
arg->kwargs = NULL;
arg->obj = NULL;
arg->obj = import_python(arg->name);
return true;
}
