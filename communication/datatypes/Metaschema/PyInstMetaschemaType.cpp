#include "PyInstMetaschemaType.hpp"
using namespace communication::datatypes::Metaschema;
using namespace communication::utils;

PyInstMetaschemaType::PyInstMetaschemaType(const char* class_name,
                     const JSONArrayMetaschemaType* args_type,
                     const JSONObjectMetaschemaType* kwargs_type,
                     const bool use_generic) :
// Always generic
        PyObjMetaschemaType("instance", true), args_type_(NULL), kwargs_type_(NULL) {
    UNUSED(use_generic);
    class_name_[0] = '\0';
    if (class_name != NULL) {
        update_class_name(class_name, true);
    }
    if (args_type != NULL) {
        update_args_type(args_type, true);
    }
    if (kwargs_type != NULL) {
        update_kwargs_type(kwargs_type, true);
    }
}

PyInstMetaschemaType::PyInstMetaschemaType(const rapidjson::Value &type_doc,
                     const bool use_generic) :
// Always generic
        PyObjMetaschemaType(type_doc, true), args_type_(NULL), kwargs_type_(NULL) {
    UNUSED(use_generic);
    class_name_[0] = '\0';
    // Class
    if (!(type_doc.HasMember("class"))) {
        ygglog_throw_error("PyInstMetaschemaType: instance type must include 'class'.");
    }
    if (!(type_doc["class"].IsString())) {
        ygglog_throw_error("PyInstMetaschemaType: 'class' value must be a string.");
    }
    update_class_name(type_doc["class"].GetString(), true);
    // Args
    if (!(type_doc.HasMember("args"))) {
        ygglog_throw_error("PyInstMetaschemaType: instance type must include 'args'.");
    }
    if (!(type_doc["args"].IsArray())) {
        ygglog_throw_error("PyInstMetaschemaType: 'args' value must be an array.");
    }
    JSONArrayMetaschemaType* args_type = new JSONArrayMetaschemaType(type_doc, "", MetaschemaType::use_generic(), "args");
    if (args_type != NULL) {
        args_type->update_type("array");
        update_args_type(args_type, true);
    }
    // Kwargs
    if (!(type_doc.HasMember("kwargs"))) {
        ygglog_throw_error("PyInstMetaschemaType: instance type must include 'kwargs'.");
    }
    if (!(type_doc["kwargs"].IsObject())) {
        ygglog_throw_error("PyInstMetaschemaType: 'kwargs' value must be an object.");
    }
    JSONObjectMetaschemaType* kwargs_type = new JSONObjectMetaschemaType(type_doc, MetaschemaType::use_generic(), "kwargs");
    if (kwargs_type != NULL) {
        kwargs_type->update_type("object");
        update_kwargs_type(kwargs_type, true);
    }
}

PyInstMetaschemaType::PyInstMetaschemaType(PyObject* pyobj, const bool use_generic) :
// Always generic
PyObjMetaschemaType(pyobj, true) {
UNUSED(use_generic);
// Class
char class_name[200] = "";
get_item_python_dict_c(pyobj, "class", class_name,
"PyInstMetaschemaType: class: ",
T_STRING, 200);
update_class_name(class_name, true);
// Args type
JSONArrayMetaschemaType* args_type = new JSONArrayMetaschemaType(pyobj, MetaschemaType::use_generic(), "args");
if (args_type != NULL) {
args_type->update_type("array");
update_args_type(args_type, true);
}
// Kwargs type
JSONObjectMetaschemaType* kwargs_type = new JSONObjectMetaschemaType(pyobj, MetaschemaType::use_generic(), "kwargs");
if (kwargs_type != NULL) {
kwargs_type->update_type("object");
update_kwargs_type(kwargs_type, true);
}
}
/*!
  @brief Equivalence operator.
  @param[in] Ref MetaschemaType instance to compare against.
  @returns bool true if the instance is equivalent, false otherwise.
 */
bool PyInstMetaschemaType::operator==(const MetaschemaType &Ref) const {
if (!(MetaschemaType::operator==(Ref)))
return false;
const PyInstMetaschemaType *pRef = dynamic_cast<const PyInstMetaschemaType*>(&Ref);
if (!pRef)
return false;
if (strcmp(class_name_, pRef->class_name()) != 0)
return false;
if ((args_type_ == NULL) || (pRef->args_type() == NULL)) {
if (args_type_ != (pRef->args_type()))
return false;
}
if (*args_type_ != *(pRef->args_type()))
return false;
if ((kwargs_type_ == NULL) || (pRef->kwargs_type() == NULL)) {
if (kwargs_type_ != (pRef->kwargs_type()))
return false;
}
if (*kwargs_type_ != (*pRef->kwargs_type()))
return false;
return true;
}

void PyInstMetaschemaType::display(const char* indent="") const {
PyObjMetaschemaType::display(indent);
printf("%s%-15s = %s\n", indent, "class_name", class_name_);
if (args_type_ == NULL) {
printf("%sArgs type: NULL\n", indent);
} else {
printf("%sArgs type:\n", indent);
args_type_->display(indent);
}
if (kwargs_type_ == NULL) {
printf("%sKwargs type: NULL\n", indent);
} else {
printf("%sKwargs type:\n", indent);
kwargs_type_->display(indent);
}
}
/*!
  @brief Get type information as a Python dictionary.
  @returns PyObject* Python dictionary.
 */
PyObject* PyInstMetaschemaType::as_python_dict() const {
PyObject* out = PyObjMetaschemaType::as_python_dict();
set_item_python_dict_c(out, "class", class_name_,
"PyInstMetaschemaType::as_python_dict: ",
T_STRING, PYTHON_NAME_SIZE);
if (args_type_ == NULL) {
ygglog_throw_error("PyInstMetaschemaType::as_python_dict: Args type is NULL.");
}
PyObject* pyargs = args_type_->as_python_dict();
set_item_python_dict(out, "args", pyargs,
"PyInstMetaschemaType::as_python_dict: ",
T_ARRAY);
if (kwargs_type_ == NULL) {
ygglog_throw_error("PyInstMetaschemaType::as_python_dict: Kwargs type is NULL.");
}
PyObject* pykwargs = kwargs_type_->as_python_dict();
set_item_python_dict(out, "kwargs", pykwargs,
"PyInstMetaschemaType::as_python_dict: ",
T_OBJECT);
return out;
}

void PyInstMetaschemaType::update(const MetaschemaType* new_info) {
PyObjMetaschemaType::update(new_info);
if (new_info == NULL) {
return;
}
const PyInstMetaschemaType* new_info_inst = dynamic_cast<const PyInstMetaschemaType*>(new_info);
update_class_name(new_info_inst->class_name());
update_args_type(new_info_inst->args_type());
update_kwargs_type(new_info_inst->kwargs_type());
}

void PyInstMetaschemaType::update_class_name(const char* new_class_name, bool force) {
    if (new_class_name == NULL) {
        ygglog_throw_error("PyInstMetaschemaType::update_class_name: New class name is NULL.");
    }
    if ((!(force)) && (strlen(class_name_) > 0) && (strcmp(class_name_, new_class_name) != 0)) {
        ygglog_throw_error("PyInstMetaschemaType::update_class_name: Cannot update class name from %s to %s.",
                           class_name_, new_class_name);
    }
    strncpy(class_name_, new_class_name, PYTHON_NAME_SIZE);
}

void PyInstMetaschemaType::update_args_type(const JSONArrayMetaschemaType* new_args_type,
                      bool force) {
    if (new_args_type == NULL) {
        ygglog_throw_error("PyInstMetaschemaType::update_args_type: New args_type is NULL.");
    }
    if ((!(force)) && (args_type_ != NULL) && (*new_args_type != *args_type_)) {
        ygglog_throw_error("PyInstMetaschemaType::update_args_type: Cannot update args type.");
    }
    if (args_type_ != NULL)
        delete args_type_;
    args_type_ = new_args_type->copy();
    // Force children to follow parent use_generic
    args_type_->update_use_generic(use_generic());
}
/*!
  @brief update the instance's kwargs type.
  @param[in] new_kwargs_type JSONObjectMetaschemaType* New kwargs type.
  @param[in] force bool If true, the kwargs type will be updated reguardless of if it is compatible or not. Defaults to false.
 */
void PyInstMetaschemaType::update_kwargs_type(const JSONObjectMetaschemaType* new_kwargs_type,
                        bool force) {
    if (new_kwargs_type == NULL) {
        ygglog_throw_error("PyInstMetaschemaType::update_kwargs_type: New kwargs_type is NULL.");
    }
    if ((!(force)) && (kwargs_type_ != NULL) && (*new_kwargs_type != *kwargs_type_)) {
        ygglog_throw_error("PyInstMetaschemaType::update_kwargs_type: Cannot update kwargs type.");
    }
    if (kwargs_type_ != NULL)
        delete kwargs_type_;
    kwargs_type_ = new_kwargs_type->copy();
    // Force children to follow parent use_generic
    kwargs_type_->update_use_generic(use_generic());
}

size_t PyInstMetaschemaType::update_from_serialization_args(size_t *nargs, va_list_t &ap) {
size_t out = MetaschemaType::update_from_serialization_args(nargs, ap);
if (use_generic())
return out;
ygglog_throw_error("PyObjMetaschemaType::update_from_serialization_args: Not implemented");
python_t arg;
if (ap.using_ptrs) {
arg = ((python_t*)get_va_list_ptr_cpp(&ap))[0];
} else {
arg = va_arg(ap.va, python_t);
}
out++;
if (args_type_ == NULL) {
update_class_name(arg.name);
// Args
if ((args_type_ == NULL) && (arg.obj != NULL)) {
PyObject *py_class = import_python_class("yggdrasil.metaschema.properties.ArgsMetaschemaProperty",
                                         "ArgsMetaschemaProperty",
                                         "PyInstMetaschemaType::update_from_serialization_args: ");
PyObject *py_args = PyObject_CallMethod(py_class, "encode",
                                        "Os", arg.obj, NULL);
Py_DECREF(py_class);
if (py_args == NULL) {
ygglog_throw_error("PyObjMetaschemaType::update_from_serialization_args: Failed to get instance argument type.");
}
YggGenericVector new_args_type;
convert_python2c(py_args, &new_args_type, T_ARRAY,
"PyObjMetaschemaType::update_from_serialization_args: ");
// update_args_type(new_args_type,);
}
// Kwargs
if ((kwargs_type_ == NULL) && (arg.obj != NULL)) {
PyObject *py_class = import_python_class("yggdrasil.metaschema.properties.KwargsMetaschemaProperty",
                                         "KwargsMetaschemaProperty",
                                         "PyInstMetaschemaType::update_from_serialization_args: ");
PyObject *py_kwargs = PyObject_CallMethod(py_class, "encode",
                                          "Os", arg.obj, NULL);
Py_DECREF(py_class);
if (py_kwargs == NULL) {
ygglog_throw_error("PyObjMetaschemaType::update_from_serialization_args: Failed to get instance keyword argument type.");
}
YggGenericMap new_kwargs_type;
convert_python2c(py_kwargs, &new_kwargs_type, T_OBJECT,
"PyObjMetaschemaType::update_from_serialization_args: ");
// update_kwargs_type(new_kwargs_type,);
}
}
return out;
}

YggGeneric* PyInstMetaschemaType::python2c(PyObject* pyobj) const {
if (args_type_ == NULL)
ygglog_throw_error("PyInstMetaschemaType::python2c: Args type is NULL.");
if (kwargs_type_ == NULL)
ygglog_throw_error("PyInstMetaschemaType::python2c: Kwargs type is NULL.");
YggGeneric* cobj = new YggGeneric(this, NULL, 0);
void** data = cobj->get_data_pointer();
python_t* idata = (python_t*)realloc(data[0], nbytes());
if (idata == NULL) {
ygglog_throw_error("PyInstMetaschemaType::python2c: Failed to realloc data.");
}
PyObject *py_class = import_python_class("yggdrasil.metaschema.datatypes.InstanceMetaschemaType",
                                         "InstanceMetaschemaType",
                                         "PyInstMetaschemaType::python2c: ");
PyObject *py_enc = PyObject_CallMethod(py_class, "encode_data",
                                       "Os", pyobj, NULL);
Py_DECREF(py_class);
if (py_enc == NULL) {
ygglog_throw_error("PyObjMetaschemaType::python2c: Failed to get instance arguments.");
}
PyObject *py_args = get_item_python_list(py_enc, 0,
                                         "PyInstMetaschemaType::python2c: ",
                                         T_ARRAY);
PyObject *py_kwargs = get_item_python_list(py_enc, 1,
                                           "PyInstMetaschemaType::python2c: ",
                                           T_OBJECT);
Py_DECREF(py_enc);
// TODO: Use name from Python object?
idata->name[0] = '\0';
strcpy(idata->name, class_name_);
idata->args = args_type_->python2c(py_args);
idata->kwargs = kwargs_type_->python2c(py_kwargs);
idata->obj = pyobj;
data[0] = (void*)idata;
Py_DECREF(py_args);
Py_DECREF(py_kwargs);
return cobj;
}

// Encoding
/*!
  @brief Encode the type's properties in a JSON string.
  @param[in] writer rapidjson::Writer<rapidjson::StringBuffer> rapidjson writer.
  @returns bool true if the encoding was successful, false otherwise.
 */
bool PyInstMetaschemaType::encode_type_prop(rapidjson::Writer<rapidjson::StringBuffer> *writer) const {
if (!(MetaschemaType::encode_type_prop(writer))) { return false; }
if (args_type_ == NULL) {
ygglog_throw_error("PyInstMetaschemaType::encode_type_prop: Args type is not initialized.");
}
if (kwargs_type_ == NULL) {
ygglog_throw_error("PyInstMetaschemaType::encode_type_prop: Kwargs type is not initialized.");
}
writer->Key("class");
writer->String(class_name_);
// Args
writer->Key("args");
writer->StartArray();
MetaschemaTypeVector items = args_type_->items();
size_t i;
for (i = 0; i < items.size(); i++) {
if (items[i] == NULL) {
ygglog_throw_error("PyInstMetaschemaType::encode_type_prop: Args type item %lu is NULL.", i);
}
if (!(items[i]->encode_type(writer)))
return false;
}
writer->EndArray();
// Kwargs
writer->Key("kwargs");
writer->StartObject();
MetaschemaTypeMap properties = kwargs_type_->properties();
MetaschemaTypeMap::const_iterator it = properties.begin();
for (it = properties.begin(); it != properties.end(); it++) {
writer->Key(it->first.c_str());
if ((it->second) == NULL) {
ygglog_throw_error("PyInstMetaschemaType::encode_type_prop: Kwargs type item %s is NULL.", it->first.c_str());
}
if (!(it->second->encode_type(writer)))
return false;
}
writer->EndObject();
return true;
}

bool PyInstMetaschemaType::encode_data(rapidjson::Writer<rapidjson::StringBuffer> *writer,
                 size_t *nargs, va_list_t &ap) const {
python_t arg0;
if (ap.using_ptrs) {
arg0 = ((python_t*)get_va_list_ptr_cpp(&ap))[0];
} else {
arg0 = va_arg(ap.va, python_t);
}
writer->StartArray();
// Args
YggGeneric* args = (YggGeneric*)(arg0.args);
(*nargs)--;
if (args_type_ == NULL) {
ygglog_throw_error("PyInstMetaschemaType::encode_data: Args type is not initialized.");
}
if (args == NULL) {
ygglog_throw_error("PyInstMetaschemaType::encode_data: Args is not initialized.");
}
if (!(args_type_->encode_data(writer, args)))
return false;
// Kwargs
YggGeneric* kwargs = (YggGeneric*)(arg0.kwargs);
(*nargs)--;
if (kwargs_type_ == NULL) {
ygglog_throw_error("PyInstMetaschemaType::encode_data: Kwargs type is not initialized.");
}
if (kwargs == NULL) {
ygglog_throw_error("PyInstMetaschemaType::encode_data: Kwargs is not initialized.");
}
if (!(kwargs_type_->encode_data(writer, kwargs)))
return false;
writer->EndArray();
return true;
}

bool PyInstMetaschemaType::decode_data(rapidjson::Value &data, const int allow_realloc,
                 size_t *nargs, va_list_t &ap) const {
if (args_type_ == NULL) {
ygglog_throw_error("PyInstMetaschemaType::decode_data: Args type is not initialize.");
}
if (kwargs_type_ == NULL) {
ygglog_throw_error("PyInstMetaschemaType::decode_data: Kwargs type is not initialize.");
}
if (!(data.IsArray())) {
ygglog_error("PyInstMetaschemaType::decode_data: Raw data is not an array.");
return false;
}
if (data.Size() != 2) {
ygglog_error("PyInstMetaschemaType::decode_data: 2 items expected, but %lu found.",
data.Size());
return false;
}
// Args
YggGeneric* cargs = new YggGeneric(args_type_, NULL, 0);
if (!(args_type_->decode_data(data[(rapidjson::SizeType)0], cargs))) {
ygglog_error("PyInstMetaschemaType::decode_data: Error decoding arguments.");
return false;
}
// Kwargs
YggGeneric* ckwargs = new YggGeneric(kwargs_type_, NULL, 0);
if (!(kwargs_type_->decode_data(data[(rapidjson::SizeType)1], ckwargs))) {
ygglog_error("PyInstMetaschemaType::decode_data: Error decoding keyword arguments.");
return false;
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
python_t *temp = (python_t*)realloc(p[0], sizeof(python_t));
if (temp == NULL) {
ygglog_throw_error("PyInstMetaschemaType::decode_data: Failed to realloc variable.");
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
strncpy(arg->name, class_name_, PYTHON_NAME_SIZE);
arg->args = cargs;
arg->kwargs = ckwargs;
arg->obj = NULL;
// Get the class/function and call it
PyObject *py_class = import_python(arg->name);
PyObject *py_args = PyList_AsTuple(args_type_->c2python((YggGeneric*)(arg->args)));
PyObject *py_kwargs = kwargs_type_->c2python((YggGeneric*)(arg->kwargs));
if (py_class == NULL) {
ygglog_throw_error("PyInstMetaschemaType::decode_data: Failed to get Python class.");
}
if (py_args == NULL) {
ygglog_throw_error("PyInstMetaschemaType::decode_data: Failed to construct arguments for Python callable.");
}
if (py_kwargs == NULL) {
ygglog_throw_error("PyInstMetaschemaType::decode_data: Failed to construct keyword arguments for Python callable.");
}
arg->obj = PyObject_Call(py_class, py_args, py_kwargs);
if (arg->obj == NULL) {
ygglog_throw_error("PyInstMetaschemaType::decode_data: Failed to call constructor.");
}
Py_DECREF(py_class);
Py_DECREF(py_args);
Py_DECREF(py_kwargs);
return true;
}
