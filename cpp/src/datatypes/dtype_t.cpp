#include "datatypes/dtype_t.hpp"
#include "utils/serialization.hpp"
#include "utils/tools.hpp"
#include "utils/rapidjson_wrapper.hpp"

#define STRLEN_RJ(var)				\
  static_cast<rapidjson::SizeType>(strlen(var))

#define _GET_METADATA(name, in, err)		\
  if (in.metadata == NULL) {			\
    return err;					\
  }						\
  YggInterface::utils::Metadata* name = ((YggInterface::utils::Metadata*)(in.metadata))
#define _GET_METADATA_THROW(name, in)				\
  if (in.metadata == NULL) {					\
    ygglog_throw_error(#name ": Metadata not initialized");	\
  }								\
  YggInterface::utils::Metadata* name = ((YggInterface::utils::Metadata*)(in.metadata))

// C++ functions
rapidjson::Document::AllocatorType& generic_allocator(generic_t& x) {
  assert(is_generic_init(x));
  // if (!is_generic_init(x))
  //   ygglog_throw_error("generic_allocator: Not initialized");
  return ((rapidjson::Document*)(x.obj))->GetAllocator();
}; // GCOVR_EXCL_LINE

rapidjson::Document::AllocatorType& generic_ref_allocator(generic_ref_t& x) {
  assert(is_generic_ref_init(x));
  // if (!is_generic_ref_init(x))
  //   ygglog_throw_error("generic_ref_allocator: Not initialized");
  return *((rapidjson::Document::AllocatorType*)(x.allocator));
}; // GCOVR_EXCL_LINE

// rapidjson::Document::AllocatorType& dtype_allocator(dtype_t& x) {
//   rapidjson::Document* s = NULL;
//   if (x.metadata != NULL)
//     return ((YggInterface::utils::Metadata*)x.metadata)->GetAllocator();
//   else
//     ygglog_throw_error("dtype_allocator: Not initialized");
//   return s->GetAllocator();
// };

ply_t Ply2ply(rapidjson::Ply& x) {
  ply_t out = init_ply();
  set_ply(&out, (void*)(&x), 1);
  return out;
};

rapidjson::Ply ply2Ply(ply_t x) {
  if (x.obj == NULL) {
    return rapidjson::Ply();
  } else {
    rapidjson::Ply* obj = (rapidjson::Ply*)(x.obj);
    return rapidjson::Ply(*obj);
  }
};

obj_t ObjWavefront2obj(rapidjson::ObjWavefront& x) {
  obj_t out = init_obj();
  set_obj(&out, (void*)(&x), 1);
  return out;
};

rapidjson::ObjWavefront obj2ObjWavefront(obj_t x) {
  if (x.obj == NULL) {
    return rapidjson::ObjWavefront();
  } else {
    rapidjson::ObjWavefront* obj = (rapidjson::ObjWavefront*)(x.obj);
    return rapidjson::ObjWavefront(*obj);
  }
};

// C exposed functions
extern "C" {

  // int destroy_document(void** obj) {
  //   if (obj == NULL || obj[0] == NULL)
  //     return 0;
  //   rapidjson::Document* s = (rapidjson::Document*)(*obj);
  //   delete s;
  //   obj[0] = NULL;
  //   return 0;
  // }

  bool _call_pointer(void* ptr, generic_t data_send, generic_t data_recv) {
    c_function f = (c_function)ptr;
    return f(data_send, data_recv);
  }

  size_t pointer_strlen(const void* x) {
    if (!x)
      return 0;
    const char* x_str = (const char*)x;
    return strlen(x_str);
  }

  ////////////////////////////////////////////
  // GENERIC OBJECT WRAPPER
  ////////////////////////////////////////////
  
  generic_t init_generic() {
    generic_t out;
    out.obj = NULL;
    return out;
  }

  generic_ref_t init_generic_ref(generic_t parent) {
    generic_ref_t out;
    out.obj = parent.obj;
    out.allocator = NULL;
    if (parent.obj != NULL)
      out.allocator = (void*)(&(((rapidjson::Document*)(parent.obj))->GetAllocator()));
    return out;
  }

  generic_t init_generic_null() {
    generic_t out = init_generic();
    rapidjson::Document* x = new rapidjson::Document(rapidjson::kNullType);
    out.obj = (void*)x;
    return out;
  }

  generic_t init_generic_array() {
    generic_t out = init_generic();
    rapidjson::Document* x = new rapidjson::Document(rapidjson::kArrayType);
    out.obj = (void*)x;
    return out;
  }

  generic_t init_generic_map() {
    generic_t out = init_generic();
    rapidjson::Document* x = new rapidjson::Document(rapidjson::kObjectType);
    out.obj = (void*)x;
    return out;
  }

  generic_t init_generic_json(const char* json) {
    generic_t out = init_generic();
    out.obj = (void*)(new rapidjson::Document());
    generic_set_json(out, json);
    return out;
  }

  generic_t init_generic_generate(const char* schema) {
    generic_t out = init_generic();
    rapidjson::Document sd;
    sd.Parse(schema);
    if (sd.HasParseError()) {
      destroy_generic(&out);
      return out;
    }
    rapidjson::SchemaDocument s(sd);
    rapidjson::SchemaValidator validator(s);
    rapidjson::Document* x = new rapidjson::Document();
    validator.GenerateData(*x);
    out.obj = (void*)x;
    return out;
  }

  int is_generic_init(const generic_t x) {
    return (x.obj != NULL);
  }
  
  int is_generic_ref_init(const generic_ref_t x) {
    return (x.obj != NULL && x.allocator != NULL);
  }
  
  int destroy_generic(generic_t* x) {
    int ret = 0;
    if (x != NULL) {
      if (x->obj != NULL) {
	try {
	  rapidjson::Document* obj = (rapidjson::Document*)(x->obj);
	  delete obj;
	  obj = nullptr;
	  x->obj = NULL;
	} catch (...) {
	  YggLogError << "destroy_generic: C++ exception thrown in destructor for rapidjson::Document." << std::endl;
	  ret = -1;
	}
      }
    }
    return ret;
  }

  int copy_generic_into(generic_t* dst, const generic_t src) {
    try {
      if (!dst) {
	ygglog_throw_error("copy_generic_into: Destination is empty.");
      }
      destroy_generic(dst);
      dst[0] = init_generic();
      if (!is_generic_init(src)) {
	ygglog_throw_error("copy_generic: Generic object class is NULL.");
      }
      rapidjson::Document* doc = new rapidjson::Document();
      doc->CopyFrom(*((const rapidjson::Value*)(src.obj)),
		    doc->GetAllocator(), true);
      dst->obj = (void*)doc;
    } catch(...) {
      YggLogError << "copy_generic_into: C++ exception thrown." << std::endl;
      destroy_generic(dst);
      return -1;
    }
    return 0;
  }

  generic_t copy_generic(const generic_t src) {
    generic_t out = init_generic();
    copy_generic_into(&out, src);
    return out;
  }

  bool compare_generic(const generic_t a, const generic_t b) {
    if (!(a.obj && b.obj)) {
      return false;
    }
    display_generic(a);
    display_generic(b);
    return ((*((rapidjson::Document*)(a.obj))) ==
	    (*((rapidjson::Document*)(b.obj))));
  }
  
  void display_generic(const generic_t x) {
    try {
      if (!x.obj)
	ygglog_throw_error("display_generic: Object is NULL.");
      std::cout << *((rapidjson::Document*)(x.obj)) << std::endl;
    } catch (...) {
      YggLogError << "display_generic: C++ exception thrown." << std::endl;
    }
  }

#define GENERIC_SUCCESS_ 0
#define GENERIC_ERROR_ -1

  int generic_set_json(generic_t x, const char *json) {
    int out = GENERIC_SUCCESS_;
    try {
      if (!is_generic_init(x)) {
	ygglog_throw_error("generic_set_json: Object is NULL.");
      }
      rapidjson::Document* x_obj = (rapidjson::Document*)(x.obj);
      x_obj->SetNull();
      x_obj->Parse(json);
      if (x_obj->HasParseError()) {
	ygglog_throw_error("generic_set_json: Error parsing string %s", json);
      }
    } catch(...) {
      YggLogError << "generic_set_json: C++ exception thrown" << std::endl;
      return GENERIC_ERROR_;
    }
    return out;
  }
  
  int add_generic_array(generic_t arr, const generic_t x) {
    int out = GENERIC_SUCCESS_;
    try {
      if (!is_generic_init(arr)) {
	ygglog_throw_error("add_generic_array: Array is NULL.");
      }
      if (!is_generic_init(x)) {
	ygglog_throw_error("add_generic_array: New element is NULL.");
      }
      rapidjson::Value* arr_obj = (rapidjson::Value*)(arr.obj);
      const rapidjson::Value* x_obj = (const rapidjson::Value*)(x.obj);
      if (!arr_obj->IsArray()) {
	ygglog_throw_error("add_generic_array: Document is not an array.");
      }
      rapidjson::Value cpy(*x_obj, generic_allocator(arr), true);
      arr_obj->PushBack(cpy, generic_allocator(arr));
    } catch (...) {
      YggLogError << "add_generic_array: C++ exception thrown." << std::endl;
      out = GENERIC_ERROR_;
    }
    return out;
  }

  int set_generic_array(generic_t arr, const size_t i, const generic_t x) {
    int out = GENERIC_SUCCESS_;
    try {
      if (!is_generic_init(arr)) {
	ygglog_throw_error("set_generic_array: Array is NULL.");
      }
      if (!is_generic_init(x)) {
	ygglog_throw_error("set_generic_array: New element is NULL.");
      }
      rapidjson::Value* arr_obj = (rapidjson::Value*)(arr.obj);
      const rapidjson::Value* x_obj = (const rapidjson::Value*)(x.obj);
      if (!arr_obj->IsArray()) {
	ygglog_throw_error("set_generic_array: Document is not an array.");
      }
      if (arr_obj->Size() > i) {
	(*arr_obj)[i].CopyFrom(*((rapidjson::Value*)x_obj),
			       generic_allocator(arr), true);
      } else {
	rapidjson::Value cpy(*((rapidjson::Value*)x_obj),
			     generic_allocator(arr), true);
	arr_obj->PushBack(cpy, generic_allocator(arr));
      }
    } catch (...) {
      YggLogError << "set_generic_array: C++ exception thrown." << std::endl;
      out = GENERIC_ERROR_;
    }
    return out;
  }

  int get_generic_array_ref(const generic_t arr, const size_t i, generic_ref_t *x) {
    int out = GENERIC_SUCCESS_;
    x[0] = init_generic_ref(arr);
    try {
      if (!is_generic_init(arr)) {
	ygglog_throw_error("get_generic_array_ref: Array is NULL.");
      }
      const rapidjson::Value* arr_obj = (const rapidjson::Value*)(arr.obj);
      if (!arr_obj->IsArray()) {
	ygglog_throw_error("get_generic_array_ref: Document is not an array.");
      }
      if (arr_obj->Size() <= i) {
	ygglog_throw_error("get_generic_array_ref: Document only has %d elements", (int)(arr_obj->Size()));
      }
      x[0].obj = (void*)(&((*arr_obj)[i]));
    } catch (...) {
      YggLogError << "get_generic_array_ref: C++ exception thrown." << std::endl;
      out = GENERIC_ERROR_;
    }
    return out;
  }
  int get_generic_array(const generic_t arr, const size_t i, generic_t *x) {
    generic_ref_t tmp = init_generic_ref(arr);
    if (get_generic_array_ref(arr, i, &tmp) != GENERIC_SUCCESS_)
      return GENERIC_ERROR_;
    x[0] = init_generic();
    rapidjson::Value* src = (rapidjson::Value*)(tmp.obj);
    rapidjson::Document* cpy = new rapidjson::Document();
    cpy->CopyFrom(*src, cpy->GetAllocator(), true);
    x[0].obj = (void*)cpy;
    return GENERIC_SUCCESS_;
  }

  int set_generic_object(generic_t arr, const char* k, const generic_t x) {
    int out = GENERIC_SUCCESS_;
    try {
      if (!is_generic_init(arr)) {
	ygglog_throw_error("set_generic_object: Object is NULL.");
      }
      if (!is_generic_init(x)) {
	ygglog_throw_error("set_generic_object: New element is NULL.");
      }
      rapidjson::Value* arr_obj = (rapidjson::Value*)(arr.obj);
      rapidjson::Value* x_obj = (rapidjson::Value*)(x.obj);
      if (!arr_obj->IsObject()) {
	ygglog_throw_error("set_generic_object: Document is not an object.");
      }
      if (arr_obj->HasMember(k)) {
	(*arr_obj)[k].CopyFrom(*((rapidjson::Value*)x_obj),
			       generic_allocator(arr), true);
      } else {
	rapidjson::Value key(k, STRLEN_RJ(k), generic_allocator(arr));
	rapidjson::Value cpy(*((rapidjson::Value*)x_obj),
			     generic_allocator(arr), true);
	arr_obj->AddMember(key, cpy, generic_allocator(arr));
      }
    } catch (...) {
      YggLogError << "set_generic_object: C++ exception thrown." << std::endl;
      out = GENERIC_ERROR_;
    }
    return out;
  }

  int get_generic_object_ref(const generic_t arr, const char* k, generic_ref_t *x) {
    int out = 0;
    x[0] = init_generic_ref(arr);
    try {
      if (!is_generic_init(arr)) {
	ygglog_throw_error("get_generic_object_ref: Object is NULL.");
      }
      const rapidjson::Value* arr_obj = (const rapidjson::Value*)(arr.obj);
      if (!arr_obj->IsObject()) {
	ygglog_throw_error("get_generic_object_ref: Document is not an object.");
      }
      if (!arr_obj->HasMember(k)) {
	ygglog_throw_error("get_generic_object_ref: Document does not have the requested key.");
      }
      x[0].obj = (void*)(&((*arr_obj)[k]));
    } catch (...) {
      YggLogError << "get_generic_object_ref: C++ exception thrown." << std::endl;
      out = GENERIC_ERROR_;
    }
    return out;
  }
  int get_generic_object(const generic_t arr, const char* k, generic_t *x) {
    generic_ref_t tmp = init_generic_ref(arr);
    if (get_generic_object_ref(arr, k, &tmp) != GENERIC_SUCCESS_)
      return GENERIC_ERROR_;
    x[0] = init_generic();
    rapidjson::Value* src = (rapidjson::Value*)(tmp.obj);
    rapidjson::Document* cpy = new rapidjson::Document();
    cpy->CopyFrom(*src, cpy->GetAllocator(), true);
    x[0].obj = (void*)cpy;
    return GENERIC_SUCCESS_;
  }

  // Generic map methods
  int generic_map_has_key(const generic_t x, const char* key) {
    int out = 0;
    try {
      if (!is_generic_init(x)) {
	ygglog_throw_error("generic_map_has_key: Object is NULL.");
      }
      const rapidjson::Value* x_obj = (const rapidjson::Value*)(x.obj);
      if (!x_obj->IsObject()) {
	ygglog_throw_error("generic_map_has_key: Document is not an object.");
      }
      if (x_obj->HasMember(key)) {
	out = 1;
      }
    } catch (...) {
      YggLogError << "generic_map_has_key: C++ exception thrown." << std::endl;
    }
    return out;
  }
  size_t generic_map_get_keys(generic_t x, char*** keys) {
    size_t out = 0;
    try {
      if (!is_generic_init(x)) {
	ygglog_throw_error("generic_map_get_keys: Object is NULL.");
      }
      rapidjson::Value* x_obj = (rapidjson::Value*)(x.obj);
      if (!x_obj->IsObject()) {
	ygglog_throw_error("generic_map_get_keys: Document is not an object.");
      }
      out = (size_t)(x_obj->MemberCount());
      keys[0] = (char**)(generic_allocator(x).Malloc(out * sizeof(char*)));
      size_t i = 0;
      for (rapidjson::Document::ConstMemberIterator it = x_obj->MemberBegin();
	   it != x_obj->MemberEnd(); it++, i++) {
	keys[0][i] = (char*)(generic_allocator(x).Malloc(sizeof(char) * (it->name.GetStringLength() + 1)));
	strcpy(keys[0][i], it->name.GetString());
      }
    } catch (...) {
      YggLogError << "generic_map_get_keys: C++ exception thrown." << std::endl;
      out = 0;
    }
    return out;
  }

  int init_python_API() {
    try {
#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
      YggInterface::utils::initialize_python("init_python_API");
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
    } catch(...) {
      YggLogError << "init_python_API: C++ exception thrown." << std::endl;
      return 1;
    }
    return 0;
  }
  
  python_t init_python() {
    python_t out;
    out.obj = NULL;
    return out;
  }
  
  void destroy_python(python_t *x) {
    if (x != NULL) {
      if (x->obj != NULL) {
#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
	YGGDRASIL_PYGIL_BEGIN
	Py_DECREF(x->obj);
	YGGDRASIL_PYGIL_END
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
	x->obj = NULL;
      }
    }
  }

  python_t copy_python(python_t x) {
    python_t out = { NULL };
    if (x.obj != NULL) {
#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
      YGGDRASIL_PYGIL_BEGIN
      Py_INCREF(x.obj);
      YGGDRASIL_PYGIL_END
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
      out.obj = x.obj;
    }
    return out;
  }

  void display_python(python_t x) {
    if (x.obj != NULL) {
#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
#if defined(_WIN32) && !defined(_MSC_VER)
      printf("This function was called from outside the MSVC CRT and will be"
	     "skipped in order to avoid a segfault incurred due to the "
	     "Python C API's use of the MSVC CRT (particularly the FILE* "
	     "datatype). To fix this, please ensure "
	     "that the MSVC compiler (cl.exe) is available and cleanup any "
	     "remaining compilation products in order to trigger yggdrasil "
	     "to recompile your model during the next run.\n");
#else
      YGGDRASIL_PYGIL_BEGIN
      PyObject_Print(x.obj, stdout, 0);
      YGGDRASIL_PYGIL_END
#endif
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
    } else {
      printf("NULL");
    }
  }

  int is_empty_dtype(const dtype_t dtype) {
    _GET_METADATA(metadata, dtype, 1);
    return static_cast<int>(!metadata->hasType());
  }
  
  int is_dtype_format_array(dtype_t type_struct) {
    _BEGIN_CPP {
      _GET_METADATA_THROW(metadata, type_struct);
      return static_cast<int>(metadata->isFormatArray());
    } _END_CPP(is_dtype_format_array, -1);
  }

  const char* dtype_name(const dtype_t type_struct) {
    _GET_METADATA(metadata, type_struct, "");
    return metadata->typeName();
  }

  const char* dtype_subtype(const dtype_t type_struct) {
    _BEGIN_CPP {
      _GET_METADATA_THROW(metadata, type_struct);
      return metadata->subtypeName();
    } _END_CPP(dtype_subtype, "");
  }

  size_t dtype_precision(const dtype_t type_struct) {
    uint64_t out = 0;
    _BEGIN_CPP {
      _GET_METADATA(metadata, type_struct, 0);
      if (!metadata->GetSchemaUint("precision", out))
	out = 0;
    } _END_CPP(dtype_precision, 0);
    return static_cast<size_t>(out);
  };

  bool compare_dtype(const dtype_t a, const dtype_t b) {
    _BEGIN_CPP {
      _GET_METADATA_THROW(a_metadata, a);
      _GET_METADATA_THROW(b_metadata, b);
      return ((*a_metadata) == (*b_metadata));
    } _END_CPP(compare_dtype, false);
  }
  
  int set_dtype_name(dtype_t dtype, const char* name) {
    _BEGIN_CPP {
      _GET_METADATA_THROW(metadata, dtype);
      if (!metadata->SetSchemaString("type", name))
	return -1;
      return 0;
    } _END_CPP(set_dtype_name, -1);
  }

  dtype_t create_dtype(void* metadata, const bool use_generic) {
    dtype_t out;
    YggInterface::utils::Metadata* meta = static_cast<YggInterface::utils::Metadata*>(metadata);
    if (!meta)
      meta = new YggInterface::utils::Metadata();
    out.metadata = (void*)meta;
    if (use_generic) {
      if (!meta->setGeneric())
	destroy_dtype(&out);
    }
    return out;
  }

  dtype_t create_dtype_empty(const bool use_generic) {
    return create_dtype(NULL, use_generic);
  }

  dtype_t complete_dtype(dtype_t dtype, const bool use_generic) {
    if (!dtype.metadata)
      dtype.metadata = (void*)(new YggInterface::utils::Metadata());
    if (use_generic) {
      _GET_METADATA(metadata, dtype, dtype);
      if (!metadata->setGeneric())
	destroy_dtype(&dtype);
    }
    return dtype;
  }

  int destroy_dtype(dtype_t *dtype) {
    _BEGIN_CPP {
      _GET_METADATA(metadata, (*dtype), 0);
      dtype->metadata = NULL;
      delete metadata;
      metadata = nullptr;
      return 0;
    } _END_CPP(destroy_dtype, -1);
  }

  dtype_t create_dtype_from_schema(const char* schema,
				   const bool use_generic) {
    dtype_t out = create_dtype(NULL, false);
    _BEGIN_CPP {
      _GET_METADATA(metadata, out, out);
      if (!metadata->fromSchema(schema, use_generic)) {
	destroy_dtype(&out);
      }
    } _END_CPP_CLEANUP(create_dtype_from_schema, out,
		       destroy_dtype(&out));
    return out;
  }

  dtype_t create_dtype_python(PyObject* pyobj, const bool use_generic) {
    dtype_t out = create_dtype(NULL, false);
    _BEGIN_CPP {
      _GET_METADATA(metadata, out, out);
      if (!metadata->fromEncode(pyobj, use_generic))
	destroy_dtype(&out);
    } _END_CPP_CLEANUP(create_dtype_python, out,
		       destroy_dtype(&out));
    return out;
  }

  dtype_t create_dtype_direct(const bool use_generic) {
    return create_dtype_default("string", use_generic);
  }

  dtype_t create_dtype_default(const char* type, const bool use_generic) {
    dtype_t out = create_dtype(NULL, false);
    _BEGIN_CPP {
      _GET_METADATA_THROW(metadata, out);
      if (!metadata->fromType(type, use_generic))
	destroy_dtype(&out);
      return out;
    } _END_CPP_CLEANUP(create_dtype_default, out, destroy_dtype(&out)); // GCOV_EXCL_LINE
  }

  dtype_t create_dtype_scalar(const char* subtype, const size_t precision,
			      const char* units, const bool use_generic) {
    dtype_t out = create_dtype(NULL, false);
    _BEGIN_CPP {
      _GET_METADATA_THROW(metadata, out);
      if (!metadata->fromScalar(subtype, precision, units, use_generic))
	destroy_dtype(&out);
      return out;
    } _END_CPP_CLEANUP(create_dtype_scalar, out, destroy_dtype(&out)); // GCOV_EXCL_LINE
  }

  dtype_t create_dtype_format(const char *format_str,
			      const bool as_array = false,
			      const bool use_generic = false) {
    dtype_t out = create_dtype(NULL, false);
    _BEGIN_CPP {
      _GET_METADATA(metadata, out, out);
      if (!metadata->fromFormat(format_str, as_array, {}, {}, use_generic))
	destroy_dtype(&out);
      return out;
    } _END_CPP_CLEANUP(create_dtype_format, out, destroy_dtype(&out)); // GCOV_EXCL_LINE
  }

  dtype_t create_dtype_1darray(const char* subtype,
			       const size_t precision,
			       const size_t length, const char* units,
			       const bool use_generic) {
    dtype_t out = create_dtype(NULL, false);
    _BEGIN_CPP {
      _GET_METADATA_THROW(metadata, out);
      if (!metadata->fromNDArray(subtype, precision, 1, &length,
				 units, use_generic))
	destroy_dtype(&out);
      return out;
    } _END_CPP_CLEANUP(create_dtype_1darray, out, destroy_dtype(&out)); // GCOV_EXCL_LINE
  }

  dtype_t create_dtype_ndarray(const char* subtype,
			       const size_t precision,
			       const size_t ndim, const size_t* shape,
			       const char* units,
			       const bool use_generic) {
    dtype_t out = create_dtype(NULL, false);
    _BEGIN_CPP {
      _GET_METADATA_THROW(metadata, out);
      if (!metadata->fromNDArray(subtype, precision, ndim, shape,
				 units, use_generic))
	destroy_dtype(&out);
      return out;
    } _END_CPP_CLEANUP(create_dtype_ndarray, out, destroy_dtype(&out)); // GCOV_EXCL_LINE
  }
  dtype_t create_dtype_ndarray_arr(const char* subtype,
				   const size_t precision,
				   const size_t ndim,
				   const int64_t shape[],
				   const char* units,
				   const bool use_generic) {
    size_t *shape_ptr = (size_t*)malloc(ndim*sizeof(size_t));
    size_t i;
    for (i = 0; i < ndim; i++) {
      shape_ptr[i] = (size_t)shape[i];
    }
    dtype_t out = create_dtype_ndarray(subtype, precision, ndim,
				       shape_ptr, units, use_generic);
    free(shape_ptr);
    return out;
  }
  dtype_t create_dtype_json_array(const size_t nitems, dtype_t* items,
				  const bool use_generic=true) {
    dtype_t out = create_dtype(NULL, false);
    _BEGIN_CPP {
      if ((nitems > 0) && (items == NULL)) {
	ygglog_throw_error("create_dtype_json_array: %d items expected, but the items parameter is NULL.", nitems);
      }
      _GET_METADATA(metadata, out, out);
      if (!metadata->fromType("array", (use_generic || nitems == 0))) {
	destroy_dtype(&out);  // GCOV_EXCL_LINE
	return out;  // GCOV_EXCL_LINE
      }
      if (nitems > 0) {
	if (!metadata->SetSchemaValue(
	      "items", rapidjson::Value(rapidjson::kArrayType).Move())) {
	  destroy_dtype(&out);  // GCOV_EXCL_LINE
	  return out;  // GCOV_EXCL_LINE
	}
	for (size_t i = 0; i < nitems; i++) {
	  if (items[i].metadata == NULL) {
	    YggLogError << "create_dtype_json_array: Item metadata " << i << " is NULL" << std::endl;
	    destroy_dtype(&out);
	    return out;
	  }
	  metadata->addItem(*((YggInterface::utils::Metadata*)(items[i].metadata)));
	  destroy_dtype(&(items[i]));
	}
      }
      return out;
    } _END_CPP_CLEANUP(create_dtype_json_array, out,
		       destroy_dtype(&out)); // GCOV_EXCL_LINE
  }
  dtype_t create_dtype_json_object(const size_t nitems, const char** keys,
				   dtype_t* values,
				   const bool use_generic=true) {
    dtype_t out = create_dtype(NULL, false);
    _BEGIN_CPP {
      if ((nitems > 0) && ((keys == NULL) || (values == NULL))) {
	YggLogError << "create_dtype_json_object: " << nitems << " items expected, but the keys and/or values parameter is NULL." << std::endl;
	destroy_dtype(&out);
	return out;
      }
      _GET_METADATA(metadata, out, out);
      if (!metadata->fromType("object", use_generic)) {
	destroy_dtype(&out);
	return out;
      }
      if (nitems > 0) {
	if (!metadata->SetSchemaValue(
	       "properties", rapidjson::Value(rapidjson::kObjectType).Move())) {
	  destroy_dtype(&out);  // GCOV_EXCL_LINE
	  return out;  // GCOV_EXCL_LINE
	}
	for (size_t i = 0; i < nitems; i++) {
	  if (values[i].metadata == NULL) {
	    YggLogError << "create_dtype_json_array: Value metadata " << i << " is NULL" << std::endl;
	    destroy_dtype(&out);
	    return out;
	  }
	  metadata->addMember(keys[i],
			      *((YggInterface::utils::Metadata*)(values[i].metadata)));
	  destroy_dtype(&(values[i]));
	}
      }
      return out;
    } _END_CPP_CLEANUP(create_dtype_json_object, out,
		       destroy_dtype(&out)); // GCOV_EXCL_LINE
  }
  dtype_t create_dtype_ply(const bool use_generic) {
    return create_dtype_default("ply", use_generic);
  }
  dtype_t create_dtype_obj(const bool use_generic) {
    return create_dtype_default("obj", use_generic);
  }
  dtype_t create_dtype_ascii_table(const char *format_str,
				   const bool as_array,
				   const bool use_generic) {
    return create_dtype_format(format_str, as_array, use_generic);
  }
  dtype_t create_dtype_pyobj(const char* type, const bool use_generic) {
    return create_dtype_default(type, use_generic);
  }
  dtype_t create_dtype_pyinst(const char* class_name,
			      dtype_t* args_dtype,
			      dtype_t* kwargs_dtype,
			      const bool use_generic) {
    dtype_t out = create_dtype(NULL, false);
    _BEGIN_CPP {
      _GET_METADATA(metadata, out, out);
      if (!metadata->fromType("instance", use_generic)) {
	destroy_dtype(&out);
	return out;
      }
      if (class_name && strlen(class_name) > 0) {
	if (!metadata->SetSchemaString("class", class_name)) {
	  destroy_dtype(&out);  // GCOV_EXCL_LINE
	  return out;  // GCOV_EXCL_LINE
	}
      }
      if (args_dtype && !is_empty_dtype(*args_dtype)) {
	if (!metadata->SetSchemaMetadata("args",
					 *((YggInterface::utils::Metadata*)(args_dtype->metadata)))) {
	  destroy_dtype(&out);
	  return out;
	}
	destroy_dtype(args_dtype);
      }
      if (kwargs_dtype && !is_empty_dtype(*kwargs_dtype)) {
	if (!metadata->SetSchemaMetadata("kwargs",
					 *((YggInterface::utils::Metadata*)(kwargs_dtype->metadata)))) {
	  destroy_dtype(&out);
	  return out;
	}
	destroy_dtype(kwargs_dtype);
      }
      if (!metadata->_init(use_generic)) {
	destroy_dtype(&out);
      }
      return out;
    } _END_CPP_CLEANUP(create_dtype_pyinst, out, destroy_dtype(&out));
  }
  dtype_t create_dtype_schema(const bool use_generic) {
    return create_dtype_default("schema", use_generic);
  }
  dtype_t create_dtype_any(const bool use_generic) {
    return create_dtype_default("any", use_generic);
  }

  dtype_t copy_dtype(const dtype_t dtype) {
    dtype_t out = create_dtype(NULL, false);
    _BEGIN_CPP {
      _GET_METADATA_THROW(metadata_src, dtype);
      _GET_METADATA_THROW(metadata_dst, out);
      if (!metadata_dst->fromMetadata(*metadata_src))
	destroy_dtype(&out);
      return out;
    } _END_CPP_CLEANUP(copy_dtype, out, destroy_dtype(&out));
  }

  int dtype_uses_generic(dtype_t dtype) {
    _BEGIN_CPP {
      _GET_METADATA(metadata, dtype, 0);
      return static_cast<int>(metadata->isGeneric());
    } _END_CPP(dtype_uses_generic, -1); // GCOV_EXCL_LINE
  }

  void display_dtype(const dtype_t dtype, const char* indent) {
    _BEGIN_CPP {
      _GET_METADATA(metadata, dtype,);
      metadata->Display(indent);
    } _END_CPP(display_dtype, );
  }
  
#define GEOM_INTERFACE(name, cpp_type)					\
  name ## _t init_ ## name() {						\
    name ## _t x;							\
    x.obj = NULL;							\
    return x;								\
  }									\
  name ## _t generate_ ## name() {					\
    name ## _t x;							\
    x.obj = NULL;							\
    rapidjson::Document sd;						\
    sd.Parse("{\"type\": \"" #name "\"}");				\
    rapidjson::SchemaDocument s(sd);					\
    rapidjson::SchemaValidator validator(s);				\
    rapidjson::Document xx;						\
    validator.GenerateData(xx);						\
    rapidjson::cpp_type* obj = new rapidjson::cpp_type();		\
    xx.Get ## cpp_type(*obj);						\
    x.obj = (void*)obj;							\
    return x;								\
  }									\
  void free_ ## name(name ## _t *p) {					\
    if (p != NULL) {							\
      if (p->obj != NULL) {						\
	rapidjson::cpp_type* obj = (rapidjson::cpp_type*)(p->obj);	\
	p->obj = NULL;							\
	delete obj;							\
	obj = nullptr;							\
      }									\
    }									\
  }									\
  void set_ ## name(name ## _t* x, void* obj, int copy) {		\
    if (x == NULL)							\
      return;								\
    if (copy && obj != NULL) {						\
      rapidjson::cpp_type* objw = (rapidjson::cpp_type*)obj;		\
      rapidjson::cpp_type* cpy = new rapidjson::cpp_type(*objw);	\
      x->obj = cpy;							\
    } else {								\
      x->obj = obj;							\
    }									\
  }									\
  name ## _t copy_ ## name(name ## _t src) {				\
    name ## _t out = init_ ## name();					\
    set_ ## name(&out, src.obj, 1);					\
    return out;								\
  }									\
  void display_ ## name ## _indent(name ## _t p, const char* indent) {	\
    if (p.obj == NULL) {						\
      printf("%sNULL\n", indent);					\
    } else {								\
      rapidjson::cpp_type* obj = (rapidjson::cpp_type*)(p.obj);		\
      std::string s_indent(indent);					\
      std::string s = obj->as_string(s_indent);				\
      printf("%s%s\n", indent, s.c_str());				\
    }									\
  }									\
  void display_ ## name(name ## _t p) {					\
    return display_ ## name ## _indent(p, "");				\
  }									\
  int nelements_ ## name(name ## _t p, const char* name) {		\
    if (p.obj == NULL) {							\
      YggLogError << "nelements_" << #name << ": " << #cpp_type << " object is NULL." << std::endl; \
      return -1;							\
    }									\
    rapidjson::cpp_type* p_obj = (rapidjson::cpp_type*)(p.obj);		\
    size_t N = p_obj->count_elements(std::string(name));		\
    return static_cast<int>(N);						\
  }									\
  bool compare_ ## name(const name ## _t a, const name ## _t b) {	\
    if (a.obj == NULL || b.obj == NULL)					\
      return (a.obj == b.obj);						\
    rapidjson::cpp_type* a_obj = (rapidjson::cpp_type*)(a.obj);		\
    rapidjson::cpp_type* b_obj = (rapidjson::cpp_type*)(b.obj);		\
    return ((*a_obj) == (*b_obj));					\
  }

  ////////////////////////////////////////////
  // OBJWAVEFRONT OBJECT WRAPPER
  ////////////////////////////////////////////

  GEOM_INTERFACE(obj, ObjWavefront)

  ////////////////////////////////////////////
  // PLY OBJECT WRAPPER
  ////////////////////////////////////////////

  GEOM_INTERFACE(ply, Ply)

#undef GEOM_INTERFACE

// LINES AFTER THIS WERE GENERATED AND SHOULD NOT BE MODIFIED DIRECTLY
//====================================================================
size_t generic_array_get_size(generic_t x) {
  size_t out = 0;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "generic_array_get_size: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsArray()) {
      YggLogError << "generic_array_get_size: Generic object is not array: " << (*d) << std::endl;
      return out;
    }
    out = (size_t)(d->Size());
  } catch(...) {
    YggLogError << "generic_array_get_size: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_size(generic_t x) {
  size_t out = 0;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "generic_object_get_size: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsObject()) {
      YggLogError << "generic_object_get_size: Generic object is not object: " << (*d) << std::endl;
      return out;
    }
    out = (size_t)(d->MemberCount());
  } catch(...) {
    YggLogError << "generic_object_get_size: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_null(generic_t x, const void* value) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->SetNull(); UNUSED(value);
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_null: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_bool(generic_t x, const bool value) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->SetBool(value);
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_bool: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_integer(generic_t x, const int value) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->SetInt(value);
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_integer: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_number(generic_t x, const double value) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->SetDouble(value);
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_number: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_string(generic_t x, const char* value) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->SetString(value, STRLEN_RJ(value), generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_string: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_item(generic_t x, const char* type, void* value) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->SetDataPtr(type, value, generic_allocator(x))) {
      YggLogError << "generic_set_item: Error setting data pointer" << std::endl;
      return GENERIC_ERROR_;
    };
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_item: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_array(generic_t x, const generic_t value) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->CopyFrom(*((rapidjson::Value*)(value.obj)), generic_allocator(x), true);
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_array: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_object(generic_t x, const generic_t value) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->CopyFrom(*((rapidjson::Value*)(value.obj)), generic_allocator(x), true);
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_object: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_ply(generic_t x, const ply_t value) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->SetPly(*((rapidjson::Ply*)(value.obj)), generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_ply: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_obj(generic_t x, const obj_t value) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->SetObjWavefront(*((rapidjson::ObjWavefront*)(value.obj)), generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_obj: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_python_class(generic_t x, const python_t value) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->SetPythonObjectRaw(value.obj, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_python_class: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_python_function(generic_t x, const python_t value) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->SetPythonObjectRaw(value.obj, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_python_function: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_python_instance(generic_t x, const python_t value) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->SetPythonObjectRaw(value.obj, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_python_instance: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_scalar(generic_t x, const void* value, const char* subtype, const size_t precision, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    rapidjson::Document schema(rapidjson::kObjectType);
    schema.AddMember(rapidjson::Document::GetTypeString(),
                     rapidjson::Value("scalar", 6,
                        schema.GetAllocator()).Move(),
                     schema.GetAllocator());
    schema.AddMember(rapidjson::Document::GetSubTypeString(),
                     rapidjson::Value(subtype, STRLEN_RJ(subtype),
                                      schema.GetAllocator()).Move(),
                     schema.GetAllocator());
    schema.AddMember(rapidjson::Document::GetPrecisionString(),
                     rapidjson::Value((unsigned)precision).Move(),
                     schema.GetAllocator());
    if (units && strlen(units) > 0) {
      schema.AddMember(rapidjson::Document::GetUnitsString(),
                       rapidjson::Value(units, STRLEN_RJ(units),
                                        schema.GetAllocator()).Move(),
                       schema.GetAllocator());
    }
    size_t length = 1;
    d->SetYggdrasilString((char*)value, precision * length,
                          generic_allocator(x),
                          schema);
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_scalar: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_int8(generic_t x, const int8_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->SetScalar(value, units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_int8: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_int16(generic_t x, const int16_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->SetScalar(value, units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_int16: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_int32(generic_t x, const int32_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->SetScalar(value, units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_int32: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_int64(generic_t x, const int64_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->SetScalar(value, units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_int64: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_uint8(generic_t x, const uint8_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->SetScalar(value, units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_uint8: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_uint16(generic_t x, const uint16_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->SetScalar(value, units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_uint16: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_uint32(generic_t x, const uint32_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->SetScalar(value, units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_uint32: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_uint64(generic_t x, const uint64_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->SetScalar(value, units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_uint64: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_float(generic_t x, const float value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->SetScalar(value, units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_float: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_double(generic_t x, const double value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->SetScalar(value, units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_complex_float(generic_t x, const complex_float_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->SetScalar(std::complex<float>(value.re, value.im), units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_complex_float: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_complex_double(generic_t x, const complex_double_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->SetScalar(std::complex<double>(value.re, value.im), units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_complex_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_1darray(generic_t x, const void* value, const char* subtype, const size_t precision, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    rapidjson::Document schema(rapidjson::kObjectType);
    schema.AddMember(rapidjson::Document::GetTypeString(),
                     rapidjson::Value("1darray", 7,
                        schema.GetAllocator()).Move(),
                     schema.GetAllocator());
    schema.AddMember(rapidjson::Document::GetSubTypeString(),
                     rapidjson::Value(subtype, STRLEN_RJ(subtype),
                                      schema.GetAllocator()).Move(),
                     schema.GetAllocator());
    schema.AddMember(rapidjson::Document::GetPrecisionString(),
                     rapidjson::Value((unsigned)precision).Move(),
                     schema.GetAllocator());
    if (units && strlen(units) > 0) {
      schema.AddMember(rapidjson::Document::GetUnitsString(),
                       rapidjson::Value(units, STRLEN_RJ(units),
                                        schema.GetAllocator()).Move(),
                       schema.GetAllocator());
    }
    rapidjson::Value rjshape(rapidjson::kArrayType);
    rjshape.PushBack(rapidjson::Value((unsigned)length).Move(),
                     schema.GetAllocator());
    schema.AddMember(rapidjson::Document::GetShapeString(), rjshape,
                     schema.GetAllocator());
    d->SetYggdrasilString((char*)value, precision * length,
                          generic_allocator(x),
                          schema);
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_1darray: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_1darray_int8(generic_t x, const int8_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->Set1DArray((int8_t*)value, (rapidjson::SizeType)length, units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_1darray_int8: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_1darray_int16(generic_t x, const int16_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->Set1DArray((int16_t*)value, (rapidjson::SizeType)length, units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_1darray_int16: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_1darray_int32(generic_t x, const int32_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->Set1DArray((int32_t*)value, (rapidjson::SizeType)length, units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_1darray_int32: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_1darray_int64(generic_t x, const int64_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->Set1DArray((int64_t*)value, (rapidjson::SizeType)length, units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_1darray_int64: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_1darray_uint8(generic_t x, const uint8_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->Set1DArray((uint8_t*)value, (rapidjson::SizeType)length, units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_1darray_uint8: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_1darray_uint16(generic_t x, const uint16_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->Set1DArray((uint16_t*)value, (rapidjson::SizeType)length, units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_1darray_uint16: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_1darray_uint32(generic_t x, const uint32_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->Set1DArray((uint32_t*)value, (rapidjson::SizeType)length, units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_1darray_uint32: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_1darray_uint64(generic_t x, const uint64_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->Set1DArray((uint64_t*)value, (rapidjson::SizeType)length, units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_1darray_uint64: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_1darray_float(generic_t x, const float* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->Set1DArray((float*)value, (rapidjson::SizeType)length, units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_1darray_float: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_1darray_double(generic_t x, const double* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->Set1DArray((double*)value, (rapidjson::SizeType)length, units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_1darray_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_1darray_complex_float(generic_t x, const complex_float_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->Set1DArray((std::complex<float>*)value, (rapidjson::SizeType)length, units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_1darray_complex_float: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_1darray_complex_double(generic_t x, const complex_double_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->Set1DArray((std::complex<double>*)value, (rapidjson::SizeType)length, units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_1darray_complex_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_ndarray(generic_t x, const void* value, const char* subtype, const size_t precision, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    rapidjson::Document schema(rapidjson::kObjectType);
    schema.AddMember(rapidjson::Document::GetTypeString(),
                     rapidjson::Value("ndarray", 7,
                        schema.GetAllocator()).Move(),
                     schema.GetAllocator());
    schema.AddMember(rapidjson::Document::GetSubTypeString(),
                     rapidjson::Value(subtype, STRLEN_RJ(subtype),
                                      schema.GetAllocator()).Move(),
                     schema.GetAllocator());
    schema.AddMember(rapidjson::Document::GetPrecisionString(),
                     rapidjson::Value((unsigned)precision).Move(),
                     schema.GetAllocator());
    if (units && strlen(units) > 0) {
      schema.AddMember(rapidjson::Document::GetUnitsString(),
                       rapidjson::Value(units, STRLEN_RJ(units),
                                        schema.GetAllocator()).Move(),
                       schema.GetAllocator());
    }
    rapidjson::Value rjshape(rapidjson::kArrayType);
    size_t length = 1;
    if (ndim <= 0)
      length = 0;
    for (size_t i = 0; i < ndim; i++) {
      rjshape.PushBack(rapidjson::Value((unsigned)(shape[i])).Move(),
                       schema.GetAllocator());
      length *= shape[i];
    }
    schema.AddMember(rapidjson::Document::GetShapeString(), rjshape,
                     schema.GetAllocator());
    d->SetYggdrasilString((char*)value, precision * length,
                          generic_allocator(x),
                          schema);
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_ndarray: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_ndarray_int8(generic_t x, const int8_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    rapidjson::SizeType* rjshape = (rapidjson::SizeType*)(generic_allocator(x).Malloc(ndim * sizeof(rapidjson::SizeType)));
    for (size_t i = 0; i < ndim; i++) {
      rjshape[i] = (rapidjson::SizeType)(shape[i]);}
    d->SetNDArray((int8_t*)value, rjshape, (rapidjson::SizeType)ndim, units, generic_allocator(x));
    generic_allocator(x).Free(rjshape);;
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_ndarray_int8: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_ndarray_int16(generic_t x, const int16_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    rapidjson::SizeType* rjshape = (rapidjson::SizeType*)(generic_allocator(x).Malloc(ndim * sizeof(rapidjson::SizeType)));
    for (size_t i = 0; i < ndim; i++) {
      rjshape[i] = (rapidjson::SizeType)(shape[i]);}
    d->SetNDArray((int16_t*)value, rjshape, (rapidjson::SizeType)ndim, units, generic_allocator(x));
    generic_allocator(x).Free(rjshape);;
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_ndarray_int16: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_ndarray_int32(generic_t x, const int32_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    rapidjson::SizeType* rjshape = (rapidjson::SizeType*)(generic_allocator(x).Malloc(ndim * sizeof(rapidjson::SizeType)));
    for (size_t i = 0; i < ndim; i++) {
      rjshape[i] = (rapidjson::SizeType)(shape[i]);}
    d->SetNDArray((int32_t*)value, rjshape, (rapidjson::SizeType)ndim, units, generic_allocator(x));
    generic_allocator(x).Free(rjshape);;
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_ndarray_int32: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_ndarray_int64(generic_t x, const int64_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    rapidjson::SizeType* rjshape = (rapidjson::SizeType*)(generic_allocator(x).Malloc(ndim * sizeof(rapidjson::SizeType)));
    for (size_t i = 0; i < ndim; i++) {
      rjshape[i] = (rapidjson::SizeType)(shape[i]);}
    d->SetNDArray((int64_t*)value, rjshape, (rapidjson::SizeType)ndim, units, generic_allocator(x));
    generic_allocator(x).Free(rjshape);;
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_ndarray_int64: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_ndarray_uint8(generic_t x, const uint8_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    rapidjson::SizeType* rjshape = (rapidjson::SizeType*)(generic_allocator(x).Malloc(ndim * sizeof(rapidjson::SizeType)));
    for (size_t i = 0; i < ndim; i++) {
      rjshape[i] = (rapidjson::SizeType)(shape[i]);}
    d->SetNDArray((uint8_t*)value, rjshape, (rapidjson::SizeType)ndim, units, generic_allocator(x));
    generic_allocator(x).Free(rjshape);;
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_ndarray_uint8: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_ndarray_uint16(generic_t x, const uint16_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    rapidjson::SizeType* rjshape = (rapidjson::SizeType*)(generic_allocator(x).Malloc(ndim * sizeof(rapidjson::SizeType)));
    for (size_t i = 0; i < ndim; i++) {
      rjshape[i] = (rapidjson::SizeType)(shape[i]);}
    d->SetNDArray((uint16_t*)value, rjshape, (rapidjson::SizeType)ndim, units, generic_allocator(x));
    generic_allocator(x).Free(rjshape);;
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_ndarray_uint16: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_ndarray_uint32(generic_t x, const uint32_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    rapidjson::SizeType* rjshape = (rapidjson::SizeType*)(generic_allocator(x).Malloc(ndim * sizeof(rapidjson::SizeType)));
    for (size_t i = 0; i < ndim; i++) {
      rjshape[i] = (rapidjson::SizeType)(shape[i]);}
    d->SetNDArray((uint32_t*)value, rjshape, (rapidjson::SizeType)ndim, units, generic_allocator(x));
    generic_allocator(x).Free(rjshape);;
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_ndarray_uint32: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_ndarray_uint64(generic_t x, const uint64_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    rapidjson::SizeType* rjshape = (rapidjson::SizeType*)(generic_allocator(x).Malloc(ndim * sizeof(rapidjson::SizeType)));
    for (size_t i = 0; i < ndim; i++) {
      rjshape[i] = (rapidjson::SizeType)(shape[i]);}
    d->SetNDArray((uint64_t*)value, rjshape, (rapidjson::SizeType)ndim, units, generic_allocator(x));
    generic_allocator(x).Free(rjshape);;
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_ndarray_uint64: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_ndarray_float(generic_t x, const float* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    rapidjson::SizeType* rjshape = (rapidjson::SizeType*)(generic_allocator(x).Malloc(ndim * sizeof(rapidjson::SizeType)));
    for (size_t i = 0; i < ndim; i++) {
      rjshape[i] = (rapidjson::SizeType)(shape[i]);}
    d->SetNDArray((float*)value, rjshape, (rapidjson::SizeType)ndim, units, generic_allocator(x));
    generic_allocator(x).Free(rjshape);;
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_ndarray_float: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_ndarray_double(generic_t x, const double* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    rapidjson::SizeType* rjshape = (rapidjson::SizeType*)(generic_allocator(x).Malloc(ndim * sizeof(rapidjson::SizeType)));
    for (size_t i = 0; i < ndim; i++) {
      rjshape[i] = (rapidjson::SizeType)(shape[i]);}
    d->SetNDArray((double*)value, rjshape, (rapidjson::SizeType)ndim, units, generic_allocator(x));
    generic_allocator(x).Free(rjshape);;
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_ndarray_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_ndarray_complex_float(generic_t x, const complex_float_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    rapidjson::SizeType* rjshape = (rapidjson::SizeType*)(generic_allocator(x).Malloc(ndim * sizeof(rapidjson::SizeType)));
    for (size_t i = 0; i < ndim; i++) {
      rjshape[i] = (rapidjson::SizeType)(shape[i]);}
    d->SetNDArray((std::complex<float>*)value, rjshape, (rapidjson::SizeType)ndim, units, generic_allocator(x));
    generic_allocator(x).Free(rjshape);;
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_ndarray_complex_float: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_ndarray_complex_double(generic_t x, const complex_double_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    rapidjson::SizeType* rjshape = (rapidjson::SizeType*)(generic_allocator(x).Malloc(ndim * sizeof(rapidjson::SizeType)));
    for (size_t i = 0; i < ndim; i++) {
      rjshape[i] = (rapidjson::SizeType)(shape[i]);}
    d->SetNDArray((std::complex<double>*)value, rjshape, (rapidjson::SizeType)ndim, units, generic_allocator(x));
    generic_allocator(x).Free(rjshape);;
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_ndarray_complex_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_schema(generic_t x, const generic_t value) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->SetSchema(*((rapidjson::Value*)(value.obj)), generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_schema: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_any(generic_t x, const generic_t value) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->CopyFrom(*((rapidjson::Value*)(value.obj)), generic_allocator(x), true);
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_any: C++ exception thrown" << std::endl;
  }
  return out;
}
void* generic_get_null(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_null(x_ref);
}
bool generic_get_bool(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_bool(x_ref);
}
int generic_get_integer(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_integer(x_ref);
}
double generic_get_number(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_number(x_ref);
}
const char* generic_get_string(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_string(x_ref);
}
void* generic_get_item(generic_t x, const char* type) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_item(x_ref, type);
}
int generic_get_item_nbytes(generic_t x, const char* type) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_item_nbytes(x_ref, type);
}
generic_t generic_get_array(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_array(x_ref);
}
generic_t generic_get_object(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_object(x_ref);
}
ply_t generic_get_ply(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_ply(x_ref);
}
obj_t generic_get_obj(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_obj(x_ref);
}
python_t generic_get_python_class(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_python_class(x_ref);
}
python_t generic_get_python_function(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_python_function(x_ref);
}
python_t generic_get_python_instance(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_python_instance(x_ref);
}
void* generic_get_scalar(generic_t x, const char* subtype, const size_t precision) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_scalar(x_ref, subtype, precision);
}
int8_t generic_get_int8(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_int8(x_ref);
}
int16_t generic_get_int16(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_int16(x_ref);
}
int32_t generic_get_int32(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_int32(x_ref);
}
int64_t generic_get_int64(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_int64(x_ref);
}
uint8_t generic_get_uint8(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_uint8(x_ref);
}
uint16_t generic_get_uint16(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_uint16(x_ref);
}
uint32_t generic_get_uint32(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_uint32(x_ref);
}
uint64_t generic_get_uint64(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_uint64(x_ref);
}
float generic_get_float(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_float(x_ref);
}
double generic_get_double(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_double(x_ref);
}
complex_float_t generic_get_complex_float(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_complex_float(x_ref);
}
complex_double_t generic_get_complex_double(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_complex_double(x_ref);
}
size_t generic_get_1darray(generic_t x, const char* subtype, const size_t precision, void** value) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_1darray(x_ref, subtype, precision, value);
}
size_t generic_get_1darray_int8(generic_t x, int8_t** value) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_1darray_int8(x_ref, value);
}
size_t generic_get_1darray_int16(generic_t x, int16_t** value) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_1darray_int16(x_ref, value);
}
size_t generic_get_1darray_int32(generic_t x, int32_t** value) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_1darray_int32(x_ref, value);
}
size_t generic_get_1darray_int64(generic_t x, int64_t** value) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_1darray_int64(x_ref, value);
}
size_t generic_get_1darray_uint8(generic_t x, uint8_t** value) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_1darray_uint8(x_ref, value);
}
size_t generic_get_1darray_uint16(generic_t x, uint16_t** value) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_1darray_uint16(x_ref, value);
}
size_t generic_get_1darray_uint32(generic_t x, uint32_t** value) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_1darray_uint32(x_ref, value);
}
size_t generic_get_1darray_uint64(generic_t x, uint64_t** value) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_1darray_uint64(x_ref, value);
}
size_t generic_get_1darray_float(generic_t x, float** value) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_1darray_float(x_ref, value);
}
size_t generic_get_1darray_double(generic_t x, double** value) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_1darray_double(x_ref, value);
}
size_t generic_get_1darray_complex_float(generic_t x, complex_float_t** value) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_1darray_complex_float(x_ref, value);
}
size_t generic_get_1darray_complex_double(generic_t x, complex_double_t** value) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_1darray_complex_double(x_ref, value);
}
size_t generic_get_ndarray(generic_t x, const char* subtype, const size_t precision, void** value, size_t** shape) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_ndarray(x_ref, subtype, precision, value, shape);
}
size_t generic_get_ndarray_int8(generic_t x, int8_t** value, size_t** shape) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_ndarray_int8(x_ref, value, shape);
}
size_t generic_get_ndarray_int16(generic_t x, int16_t** value, size_t** shape) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_ndarray_int16(x_ref, value, shape);
}
size_t generic_get_ndarray_int32(generic_t x, int32_t** value, size_t** shape) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_ndarray_int32(x_ref, value, shape);
}
size_t generic_get_ndarray_int64(generic_t x, int64_t** value, size_t** shape) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_ndarray_int64(x_ref, value, shape);
}
size_t generic_get_ndarray_uint8(generic_t x, uint8_t** value, size_t** shape) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_ndarray_uint8(x_ref, value, shape);
}
size_t generic_get_ndarray_uint16(generic_t x, uint16_t** value, size_t** shape) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_ndarray_uint16(x_ref, value, shape);
}
size_t generic_get_ndarray_uint32(generic_t x, uint32_t** value, size_t** shape) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_ndarray_uint32(x_ref, value, shape);
}
size_t generic_get_ndarray_uint64(generic_t x, uint64_t** value, size_t** shape) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_ndarray_uint64(x_ref, value, shape);
}
size_t generic_get_ndarray_float(generic_t x, float** value, size_t** shape) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_ndarray_float(x_ref, value, shape);
}
size_t generic_get_ndarray_double(generic_t x, double** value, size_t** shape) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_ndarray_double(x_ref, value, shape);
}
size_t generic_get_ndarray_complex_float(generic_t x, complex_float_t** value, size_t** shape) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_ndarray_complex_float(x_ref, value, shape);
}
size_t generic_get_ndarray_complex_double(generic_t x, complex_double_t** value, size_t** shape) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_ndarray_complex_double(x_ref, value, shape);
}
generic_t generic_get_schema(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_schema(x_ref);
}
generic_t generic_get_any(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_any(x_ref);
}
void* generic_ref_get_null(generic_ref_t x) {
  void* out;
  out = NULL;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_null: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsNull()) {
      YggLogError << "generic_ref_get_null: Generic object is not null: " << (*d) << std::endl;
      return out;
    }
    out = NULL;
  } catch(...) {
    YggLogError << "generic_ref_get_null: C++ exception thrown" << std::endl;
  }
  return out;
}
bool generic_ref_get_bool(generic_ref_t x) {
  bool out;
  out = false;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_bool: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsBool()) {
      YggLogError << "generic_ref_get_bool: Generic object is not bool: " << (*d) << std::endl;
      return out;
    }
    out = d->GetBool();
  } catch(...) {
    YggLogError << "generic_ref_get_bool: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_ref_get_integer(generic_ref_t x) {
  int out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_integer: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsInt()) {
      YggLogError << "generic_ref_get_integer: Generic object is not integer: " << (*d) << std::endl;
      return out;
    }
    out = d->GetInt();
  } catch(...) {
    YggLogError << "generic_ref_get_integer: C++ exception thrown" << std::endl;
  }
  return out;
}
double generic_ref_get_number(generic_ref_t x) {
  double out;
  out = 0.0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_number: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsNumber()) {
      YggLogError << "generic_ref_get_number: Generic object is not number: " << (*d) << std::endl;
      return out;
    }
    out = d->GetDouble();
  } catch(...) {
    YggLogError << "generic_ref_get_number: C++ exception thrown" << std::endl;
  }
  return out;
}
const char* generic_ref_get_string(generic_ref_t x) {
  const char* out;
  out = "";
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_string: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsString()) {
      YggLogError << "generic_ref_get_string: Generic object is not string: " << (*d) << std::endl;
      return out;
    }
    out = d->GetString();
  } catch(...) {
    YggLogError << "generic_ref_get_string: C++ exception thrown" << std::endl;
  }
  return out;
}
void* generic_ref_get_item(generic_ref_t x, const char* type) {
  void* out;
  out = NULL;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_item: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsType(type)) {
      YggLogError << "generic_ref_get_item: Generic object is not item: " << (*d) << std::endl;
      return out;
    }
    bool requires_freeing = false;
    out = d->GetDataPtr(requires_freeing);
  } catch(...) {
    YggLogError << "generic_ref_get_item: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_ref_get_item_nbytes(generic_ref_t x, const char* type) {
  int out;
  out = -1;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_item_nbytes: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsType(type)) {
      YggLogError << "generic_ref_get_item_nbytes: Generic object is not item_nbytes: " << (*d) << std::endl;
      return out;
    }
    out = d->GetNBytes();
  } catch(...) {
    YggLogError << "generic_ref_get_item_nbytes: C++ exception thrown" << std::endl;
  }
  return out;
}
generic_t generic_ref_get_array(generic_ref_t x) {
  generic_t out;
  out = init_generic();
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_array: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsArray()) {
      YggLogError << "generic_ref_get_array: Generic object is not array: " << (*d) << std::endl;
      return out;
    }
    rapidjson::Document* cpy = new rapidjson::Document();
    cpy->CopyFrom(*d, cpy->GetAllocator(), true);
    out.obj = (void*)cpy;
  } catch(...) {
    YggLogError << "generic_ref_get_array: C++ exception thrown" << std::endl;
  }
  return out;
}
generic_t generic_ref_get_object(generic_ref_t x) {
  generic_t out;
  out = init_generic();
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_object: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsObject()) {
      YggLogError << "generic_ref_get_object: Generic object is not object: " << (*d) << std::endl;
      return out;
    }
    rapidjson::Document* cpy = new rapidjson::Document();
    cpy->CopyFrom(*d, cpy->GetAllocator(), true);
    out.obj = (void*)cpy;
  } catch(...) {
    YggLogError << "generic_ref_get_object: C++ exception thrown" << std::endl;
  }
  return out;
}
ply_t generic_ref_get_ply(generic_ref_t x) {
  ply_t out;
  out = init_ply();
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_ply: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsPly()) {
      YggLogError << "generic_ref_get_ply: Generic object is not ply: " << (*d) << std::endl;
      return out;
    }
    rapidjson::Ply tmp;
    d->GetPly(tmp);
    set_ply(&out, (void*)(&tmp), 1);
  } catch(...) {
    YggLogError << "generic_ref_get_ply: C++ exception thrown" << std::endl;
  }
  return out;
}
obj_t generic_ref_get_obj(generic_ref_t x) {
  obj_t out;
  out = init_obj();
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_obj: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsObjWavefront()) {
      YggLogError << "generic_ref_get_obj: Generic object is not obj: " << (*d) << std::endl;
      return out;
    }
    rapidjson::ObjWavefront tmp;
    d->GetObjWavefront(tmp);
    set_obj(&out, (void*)(&tmp), 1);
  } catch(...) {
    YggLogError << "generic_ref_get_obj: C++ exception thrown" << std::endl;
  }
  return out;
}
python_t generic_ref_get_python_class(generic_ref_t x) {
  python_t out;
  out = init_python();
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_python_class: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsPythonClass()) {
      YggLogError << "generic_ref_get_python_class: Generic object is not python_class: " << (*d) << std::endl;
      return out;
    }
    out.obj = d->GetPythonObjectRaw();
  } catch(...) {
    YggLogError << "generic_ref_get_python_class: C++ exception thrown" << std::endl;
  }
  return out;
}
python_t generic_ref_get_python_function(generic_ref_t x) {
  python_t out;
  out = init_python();
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_python_function: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsPythonFunction()) {
      YggLogError << "generic_ref_get_python_function: Generic object is not python_function: " << (*d) << std::endl;
      return out;
    }
    out.obj = d->GetPythonObjectRaw();
  } catch(...) {
    YggLogError << "generic_ref_get_python_function: C++ exception thrown" << std::endl;
  }
  return out;
}
python_t generic_ref_get_python_instance(generic_ref_t x) {
  python_t out;
  out = init_python();
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_python_instance: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsPythonInstance()) {
      YggLogError << "generic_ref_get_python_instance: Generic object is not python_instance: " << (*d) << std::endl;
      return out;
    }
    out.obj = d->GetPythonObjectRaw();
  } catch(...) {
    YggLogError << "generic_ref_get_python_instance: C++ exception thrown" << std::endl;
  }
  return out;
}
void* generic_ref_get_scalar(generic_ref_t x, const char* subtype, const size_t precision) {
  void* out;
  out = NULL;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_scalar: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!(d->IsType("scalar") && d->IsSubType(subtype, static_cast<rapidjson::SizeType>(precision)))) {
      YggLogError << "generic_ref_get_scalar: Generic object is not scalar of subtype '" << std::string(subtype) << "' with precision " << precision << ": " << (*d) << std::endl;
      return out;
    }
    out = generic_ref_get_item(x, "scalar");
  } catch(...) {
    YggLogError << "generic_ref_get_scalar: C++ exception thrown" << std::endl;
  }
  return out;
}
int8_t generic_ref_get_int8(generic_ref_t x) {
  int8_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_int8: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsScalar<int8_t>()) {
      YggLogError << "generic_ref_get_int8: Generic object is not int8: " << (*d) << std::endl;
      return out;
    }
    out = d->GetScalar<int8_t>();
  } catch(...) {
    YggLogError << "generic_ref_get_int8: C++ exception thrown" << std::endl;
  }
  return out;
}
int16_t generic_ref_get_int16(generic_ref_t x) {
  int16_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_int16: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsScalar<int16_t>()) {
      YggLogError << "generic_ref_get_int16: Generic object is not int16: " << (*d) << std::endl;
      return out;
    }
    out = d->GetScalar<int16_t>();
  } catch(...) {
    YggLogError << "generic_ref_get_int16: C++ exception thrown" << std::endl;
  }
  return out;
}
int32_t generic_ref_get_int32(generic_ref_t x) {
  int32_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_int32: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsScalar<int32_t>()) {
      YggLogError << "generic_ref_get_int32: Generic object is not int32: " << (*d) << std::endl;
      return out;
    }
    out = d->GetScalar<int32_t>();
  } catch(...) {
    YggLogError << "generic_ref_get_int32: C++ exception thrown" << std::endl;
  }
  return out;
}
int64_t generic_ref_get_int64(generic_ref_t x) {
  int64_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_int64: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsScalar<int64_t>()) {
      YggLogError << "generic_ref_get_int64: Generic object is not int64: " << (*d) << std::endl;
      return out;
    }
    out = d->GetScalar<int64_t>();
  } catch(...) {
    YggLogError << "generic_ref_get_int64: C++ exception thrown" << std::endl;
  }
  return out;
}
uint8_t generic_ref_get_uint8(generic_ref_t x) {
  uint8_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_uint8: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsScalar<uint8_t>()) {
      YggLogError << "generic_ref_get_uint8: Generic object is not uint8: " << (*d) << std::endl;
      return out;
    }
    out = d->GetScalar<uint8_t>();
  } catch(...) {
    YggLogError << "generic_ref_get_uint8: C++ exception thrown" << std::endl;
  }
  return out;
}
uint16_t generic_ref_get_uint16(generic_ref_t x) {
  uint16_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_uint16: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsScalar<uint16_t>()) {
      YggLogError << "generic_ref_get_uint16: Generic object is not uint16: " << (*d) << std::endl;
      return out;
    }
    out = d->GetScalar<uint16_t>();
  } catch(...) {
    YggLogError << "generic_ref_get_uint16: C++ exception thrown" << std::endl;
  }
  return out;
}
uint32_t generic_ref_get_uint32(generic_ref_t x) {
  uint32_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_uint32: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsScalar<uint32_t>()) {
      YggLogError << "generic_ref_get_uint32: Generic object is not uint32: " << (*d) << std::endl;
      return out;
    }
    out = d->GetScalar<uint32_t>();
  } catch(...) {
    YggLogError << "generic_ref_get_uint32: C++ exception thrown" << std::endl;
  }
  return out;
}
uint64_t generic_ref_get_uint64(generic_ref_t x) {
  uint64_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_uint64: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsScalar<uint64_t>()) {
      YggLogError << "generic_ref_get_uint64: Generic object is not uint64: " << (*d) << std::endl;
      return out;
    }
    out = d->GetScalar<uint64_t>();
  } catch(...) {
    YggLogError << "generic_ref_get_uint64: C++ exception thrown" << std::endl;
  }
  return out;
}
float generic_ref_get_float(generic_ref_t x) {
  float out;
  out = 0.0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_float: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsScalar<float>()) {
      YggLogError << "generic_ref_get_float: Generic object is not float: " << (*d) << std::endl;
      return out;
    }
    out = d->GetScalar<float>();
  } catch(...) {
    YggLogError << "generic_ref_get_float: C++ exception thrown" << std::endl;
  }
  return out;
}
double generic_ref_get_double(generic_ref_t x) {
  double out;
  out = 0.0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_double: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsScalar<double>()) {
      YggLogError << "generic_ref_get_double: Generic object is not double: " << (*d) << std::endl;
      return out;
    }
    out = d->GetScalar<double>();
  } catch(...) {
    YggLogError << "generic_ref_get_double: C++ exception thrown" << std::endl;
  }
  return out;
}
complex_float_t generic_ref_get_complex_float(generic_ref_t x) {
  complex_float_t out;
  out.re = 0.0;
  out.im = 0.0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_complex_float: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsScalar<std::complex<float>>()) {
      YggLogError << "generic_ref_get_complex_float: Generic object is not complex_float: " << (*d) << std::endl;
      return out;
    }
    std::complex<float> tmp = d->GetScalar<std::complex<float> >();
    out.re = tmp.real();
    out.im = tmp.imag();
  } catch(...) {
    YggLogError << "generic_ref_get_complex_float: C++ exception thrown" << std::endl;
  }
  return out;
}
complex_double_t generic_ref_get_complex_double(generic_ref_t x) {
  complex_double_t out;
  out.re = 0.0;
  out.im = 0.0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_complex_double: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsScalar<std::complex<double>>()) {
      YggLogError << "generic_ref_get_complex_double: Generic object is not complex_double: " << (*d) << std::endl;
      return out;
    }
    std::complex<double> tmp = d->GetScalar<std::complex<double> >();
    out.re = tmp.real();
    out.im = tmp.imag();
  } catch(...) {
    YggLogError << "generic_ref_get_complex_double: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_1darray(generic_ref_t x, const char* subtype, const size_t precision, void** value) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_1darray: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!(d->IsType("1darray") && d->IsSubType(subtype, static_cast<rapidjson::SizeType>(precision)))) {
      YggLogError << "generic_ref_get_1darray: Generic object is not 1darray of subtype '" << std::string(subtype) << "' with precision " << precision << ": " << (*d) << std::endl;
      return out;
    }
    void* new_data = generic_ref_get_item(x, "1darray");
    assert(new_data);
    size_t nbytes = generic_ref_get_item_nbytes(x, "1darray");
    out = (size_t)(d->GetNElements());
    value[0] = generic_ref_allocator(x).Realloc(value[0], 0, nbytes);
    if (value[0] == NULL) {
      ygglog_throw_error("generic_ref_get_1darray: Failed to reallocate array");
    }
    memcpy(value[0], new_data, nbytes);
  } catch(...) {
    YggLogError << "generic_ref_get_1darray: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_1darray_int8(generic_ref_t x, int8_t** value) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_1darray_int8: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->Is1DArray<int8_t>()) {
      YggLogError << "generic_ref_get_1darray_int8: Generic object is not 1darray_int8: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType nelements = 0;
    value[0] = (int8_t*)(d->Get1DArray<int8_t>(nelements, generic_ref_allocator(x)));
    out = (size_t)nelements;
  } catch(...) {
    YggLogError << "generic_ref_get_1darray_int8: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_1darray_int16(generic_ref_t x, int16_t** value) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_1darray_int16: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->Is1DArray<int16_t>()) {
      YggLogError << "generic_ref_get_1darray_int16: Generic object is not 1darray_int16: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType nelements = 0;
    value[0] = (int16_t*)(d->Get1DArray<int16_t>(nelements, generic_ref_allocator(x)));
    out = (size_t)nelements;
  } catch(...) {
    YggLogError << "generic_ref_get_1darray_int16: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_1darray_int32(generic_ref_t x, int32_t** value) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_1darray_int32: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->Is1DArray<int32_t>()) {
      YggLogError << "generic_ref_get_1darray_int32: Generic object is not 1darray_int32: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType nelements = 0;
    value[0] = (int32_t*)(d->Get1DArray<int32_t>(nelements, generic_ref_allocator(x)));
    out = (size_t)nelements;
  } catch(...) {
    YggLogError << "generic_ref_get_1darray_int32: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_1darray_int64(generic_ref_t x, int64_t** value) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_1darray_int64: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->Is1DArray<int64_t>()) {
      YggLogError << "generic_ref_get_1darray_int64: Generic object is not 1darray_int64: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType nelements = 0;
    value[0] = (int64_t*)(d->Get1DArray<int64_t>(nelements, generic_ref_allocator(x)));
    out = (size_t)nelements;
  } catch(...) {
    YggLogError << "generic_ref_get_1darray_int64: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_1darray_uint8(generic_ref_t x, uint8_t** value) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_1darray_uint8: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->Is1DArray<uint8_t>()) {
      YggLogError << "generic_ref_get_1darray_uint8: Generic object is not 1darray_uint8: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType nelements = 0;
    value[0] = (uint8_t*)(d->Get1DArray<uint8_t>(nelements, generic_ref_allocator(x)));
    out = (size_t)nelements;
  } catch(...) {
    YggLogError << "generic_ref_get_1darray_uint8: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_1darray_uint16(generic_ref_t x, uint16_t** value) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_1darray_uint16: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->Is1DArray<uint16_t>()) {
      YggLogError << "generic_ref_get_1darray_uint16: Generic object is not 1darray_uint16: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType nelements = 0;
    value[0] = (uint16_t*)(d->Get1DArray<uint16_t>(nelements, generic_ref_allocator(x)));
    out = (size_t)nelements;
  } catch(...) {
    YggLogError << "generic_ref_get_1darray_uint16: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_1darray_uint32(generic_ref_t x, uint32_t** value) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_1darray_uint32: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->Is1DArray<uint32_t>()) {
      YggLogError << "generic_ref_get_1darray_uint32: Generic object is not 1darray_uint32: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType nelements = 0;
    value[0] = (uint32_t*)(d->Get1DArray<uint32_t>(nelements, generic_ref_allocator(x)));
    out = (size_t)nelements;
  } catch(...) {
    YggLogError << "generic_ref_get_1darray_uint32: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_1darray_uint64(generic_ref_t x, uint64_t** value) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_1darray_uint64: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->Is1DArray<uint64_t>()) {
      YggLogError << "generic_ref_get_1darray_uint64: Generic object is not 1darray_uint64: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType nelements = 0;
    value[0] = (uint64_t*)(d->Get1DArray<uint64_t>(nelements, generic_ref_allocator(x)));
    out = (size_t)nelements;
  } catch(...) {
    YggLogError << "generic_ref_get_1darray_uint64: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_1darray_float(generic_ref_t x, float** value) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_1darray_float: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->Is1DArray<float>()) {
      YggLogError << "generic_ref_get_1darray_float: Generic object is not 1darray_float: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType nelements = 0;
    value[0] = (float*)(d->Get1DArray<float>(nelements, generic_ref_allocator(x)));
    out = (size_t)nelements;
  } catch(...) {
    YggLogError << "generic_ref_get_1darray_float: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_1darray_double(generic_ref_t x, double** value) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_1darray_double: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->Is1DArray<double>()) {
      YggLogError << "generic_ref_get_1darray_double: Generic object is not 1darray_double: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType nelements = 0;
    value[0] = (double*)(d->Get1DArray<double>(nelements, generic_ref_allocator(x)));
    out = (size_t)nelements;
  } catch(...) {
    YggLogError << "generic_ref_get_1darray_double: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_1darray_complex_float(generic_ref_t x, complex_float_t** value) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_1darray_complex_float: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->Is1DArray<std::complex<float>>()) {
      YggLogError << "generic_ref_get_1darray_complex_float: Generic object is not 1darray_complex_float: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType nelements = 0;
    value[0] = (complex_float_t*)(d->Get1DArray<std::complex<float>>(nelements, generic_ref_allocator(x)));
    out = (size_t)nelements;
  } catch(...) {
    YggLogError << "generic_ref_get_1darray_complex_float: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_1darray_complex_double(generic_ref_t x, complex_double_t** value) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_1darray_complex_double: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->Is1DArray<std::complex<double>>()) {
      YggLogError << "generic_ref_get_1darray_complex_double: Generic object is not 1darray_complex_double: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType nelements = 0;
    value[0] = (complex_double_t*)(d->Get1DArray<std::complex<double>>(nelements, generic_ref_allocator(x)));
    out = (size_t)nelements;
  } catch(...) {
    YggLogError << "generic_ref_get_1darray_complex_double: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_ndarray(generic_ref_t x, const char* subtype, const size_t precision, void** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_ndarray: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!(d->IsType("ndarray") && d->IsSubType(subtype, static_cast<rapidjson::SizeType>(precision)))) {
      YggLogError << "generic_ref_get_ndarray: Generic object is not ndarray of subtype '" << std::string(subtype) << "' with precision " << precision << ": " << (*d) << std::endl;
      return out;
    }
    void* new_data = generic_ref_get_item(x, "ndarray");
    assert(new_data);
    size_t nbytes = generic_ref_get_item_nbytes(x, "ndarray");
    value[0] = generic_ref_allocator(x).Realloc(value[0], 0, nbytes);
    if (value[0] == NULL) {
      ygglog_throw_error("generic_ref_get_ndarray: Failed to reallocate array");
    }
    memcpy(value[0], new_data, nbytes);
    const rapidjson::Value& rjshape = d->GetShape();
    out = (size_t)(rjshape.Size());
    shape[0] = (size_t*)(generic_ref_allocator(x).Realloc(shape[0], 0, out * sizeof(size_t)));
    if (shape[0] == NULL) {
      ygglog_throw_error("generic_ref_get_ndarray: Failed to reallocate shape.");
    }
    size_t i = 0;
    for (rapidjson::Value::ConstValueIterator it = rjshape.Begin();
         it != rjshape.End(); it++, i++) {
      shape[0][i] = (size_t)(it->GetInt());
    };
  } catch(...) {
    YggLogError << "generic_ref_get_ndarray: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_ndarray_int8(generic_ref_t x, int8_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_ndarray_int8: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsNDArray<int8_t>()) {
      YggLogError << "generic_ref_get_ndarray_int8: Generic object is not ndarray_int8: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType ndim = 0;
    rapidjson::SizeType* rjshape = NULL;
    value[0] = (int8_t*)(d->GetNDArray<int8_t>(rjshape, ndim, generic_ref_allocator(x)));
    shape[0] = (size_t*)(generic_ref_allocator(x).Malloc(ndim * sizeof(size_t)));
    for (rapidjson::SizeType i = 0; i < ndim; i++) {
      (*shape)[i] = rjshape[i];
    }
    generic_ref_allocator(x).Free(rjshape);
    out = (size_t)ndim;
  } catch(...) {
    YggLogError << "generic_ref_get_ndarray_int8: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_ndarray_int16(generic_ref_t x, int16_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_ndarray_int16: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsNDArray<int16_t>()) {
      YggLogError << "generic_ref_get_ndarray_int16: Generic object is not ndarray_int16: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType ndim = 0;
    rapidjson::SizeType* rjshape = NULL;
    value[0] = (int16_t*)(d->GetNDArray<int16_t>(rjshape, ndim, generic_ref_allocator(x)));
    shape[0] = (size_t*)(generic_ref_allocator(x).Malloc(ndim * sizeof(size_t)));
    for (rapidjson::SizeType i = 0; i < ndim; i++) {
      (*shape)[i] = rjshape[i];
    }
    generic_ref_allocator(x).Free(rjshape);
    out = (size_t)ndim;
  } catch(...) {
    YggLogError << "generic_ref_get_ndarray_int16: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_ndarray_int32(generic_ref_t x, int32_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_ndarray_int32: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsNDArray<int32_t>()) {
      YggLogError << "generic_ref_get_ndarray_int32: Generic object is not ndarray_int32: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType ndim = 0;
    rapidjson::SizeType* rjshape = NULL;
    value[0] = (int32_t*)(d->GetNDArray<int32_t>(rjshape, ndim, generic_ref_allocator(x)));
    shape[0] = (size_t*)(generic_ref_allocator(x).Malloc(ndim * sizeof(size_t)));
    for (rapidjson::SizeType i = 0; i < ndim; i++) {
      (*shape)[i] = rjshape[i];
    }
    generic_ref_allocator(x).Free(rjshape);
    out = (size_t)ndim;
  } catch(...) {
    YggLogError << "generic_ref_get_ndarray_int32: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_ndarray_int64(generic_ref_t x, int64_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_ndarray_int64: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsNDArray<int64_t>()) {
      YggLogError << "generic_ref_get_ndarray_int64: Generic object is not ndarray_int64: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType ndim = 0;
    rapidjson::SizeType* rjshape = NULL;
    value[0] = (int64_t*)(d->GetNDArray<int64_t>(rjshape, ndim, generic_ref_allocator(x)));
    shape[0] = (size_t*)(generic_ref_allocator(x).Malloc(ndim * sizeof(size_t)));
    for (rapidjson::SizeType i = 0; i < ndim; i++) {
      (*shape)[i] = rjshape[i];
    }
    generic_ref_allocator(x).Free(rjshape);
    out = (size_t)ndim;
  } catch(...) {
    YggLogError << "generic_ref_get_ndarray_int64: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_ndarray_uint8(generic_ref_t x, uint8_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_ndarray_uint8: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsNDArray<uint8_t>()) {
      YggLogError << "generic_ref_get_ndarray_uint8: Generic object is not ndarray_uint8: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType ndim = 0;
    rapidjson::SizeType* rjshape = NULL;
    value[0] = (uint8_t*)(d->GetNDArray<uint8_t>(rjshape, ndim, generic_ref_allocator(x)));
    shape[0] = (size_t*)(generic_ref_allocator(x).Malloc(ndim * sizeof(size_t)));
    for (rapidjson::SizeType i = 0; i < ndim; i++) {
      (*shape)[i] = rjshape[i];
    }
    generic_ref_allocator(x).Free(rjshape);
    out = (size_t)ndim;
  } catch(...) {
    YggLogError << "generic_ref_get_ndarray_uint8: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_ndarray_uint16(generic_ref_t x, uint16_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_ndarray_uint16: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsNDArray<uint16_t>()) {
      YggLogError << "generic_ref_get_ndarray_uint16: Generic object is not ndarray_uint16: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType ndim = 0;
    rapidjson::SizeType* rjshape = NULL;
    value[0] = (uint16_t*)(d->GetNDArray<uint16_t>(rjshape, ndim, generic_ref_allocator(x)));
    shape[0] = (size_t*)(generic_ref_allocator(x).Malloc(ndim * sizeof(size_t)));
    for (rapidjson::SizeType i = 0; i < ndim; i++) {
      (*shape)[i] = rjshape[i];
    }
    generic_ref_allocator(x).Free(rjshape);
    out = (size_t)ndim;
  } catch(...) {
    YggLogError << "generic_ref_get_ndarray_uint16: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_ndarray_uint32(generic_ref_t x, uint32_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_ndarray_uint32: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsNDArray<uint32_t>()) {
      YggLogError << "generic_ref_get_ndarray_uint32: Generic object is not ndarray_uint32: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType ndim = 0;
    rapidjson::SizeType* rjshape = NULL;
    value[0] = (uint32_t*)(d->GetNDArray<uint32_t>(rjshape, ndim, generic_ref_allocator(x)));
    shape[0] = (size_t*)(generic_ref_allocator(x).Malloc(ndim * sizeof(size_t)));
    for (rapidjson::SizeType i = 0; i < ndim; i++) {
      (*shape)[i] = rjshape[i];
    }
    generic_ref_allocator(x).Free(rjshape);
    out = (size_t)ndim;
  } catch(...) {
    YggLogError << "generic_ref_get_ndarray_uint32: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_ndarray_uint64(generic_ref_t x, uint64_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_ndarray_uint64: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsNDArray<uint64_t>()) {
      YggLogError << "generic_ref_get_ndarray_uint64: Generic object is not ndarray_uint64: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType ndim = 0;
    rapidjson::SizeType* rjshape = NULL;
    value[0] = (uint64_t*)(d->GetNDArray<uint64_t>(rjshape, ndim, generic_ref_allocator(x)));
    shape[0] = (size_t*)(generic_ref_allocator(x).Malloc(ndim * sizeof(size_t)));
    for (rapidjson::SizeType i = 0; i < ndim; i++) {
      (*shape)[i] = rjshape[i];
    }
    generic_ref_allocator(x).Free(rjshape);
    out = (size_t)ndim;
  } catch(...) {
    YggLogError << "generic_ref_get_ndarray_uint64: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_ndarray_float(generic_ref_t x, float** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_ndarray_float: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsNDArray<float>()) {
      YggLogError << "generic_ref_get_ndarray_float: Generic object is not ndarray_float: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType ndim = 0;
    rapidjson::SizeType* rjshape = NULL;
    value[0] = (float*)(d->GetNDArray<float>(rjshape, ndim, generic_ref_allocator(x)));
    shape[0] = (size_t*)(generic_ref_allocator(x).Malloc(ndim * sizeof(size_t)));
    for (rapidjson::SizeType i = 0; i < ndim; i++) {
      (*shape)[i] = rjshape[i];
    }
    generic_ref_allocator(x).Free(rjshape);
    out = (size_t)ndim;
  } catch(...) {
    YggLogError << "generic_ref_get_ndarray_float: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_ndarray_double(generic_ref_t x, double** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_ndarray_double: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsNDArray<double>()) {
      YggLogError << "generic_ref_get_ndarray_double: Generic object is not ndarray_double: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType ndim = 0;
    rapidjson::SizeType* rjshape = NULL;
    value[0] = (double*)(d->GetNDArray<double>(rjshape, ndim, generic_ref_allocator(x)));
    shape[0] = (size_t*)(generic_ref_allocator(x).Malloc(ndim * sizeof(size_t)));
    for (rapidjson::SizeType i = 0; i < ndim; i++) {
      (*shape)[i] = rjshape[i];
    }
    generic_ref_allocator(x).Free(rjshape);
    out = (size_t)ndim;
  } catch(...) {
    YggLogError << "generic_ref_get_ndarray_double: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_ndarray_complex_float(generic_ref_t x, complex_float_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_ndarray_complex_float: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsNDArray<std::complex<float>>()) {
      YggLogError << "generic_ref_get_ndarray_complex_float: Generic object is not ndarray_complex_float: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType ndim = 0;
    rapidjson::SizeType* rjshape = NULL;
    value[0] = (complex_float_t*)(d->GetNDArray<std::complex<float>>(rjshape, ndim, generic_ref_allocator(x)));
    shape[0] = (size_t*)(generic_ref_allocator(x).Malloc(ndim * sizeof(size_t)));
    for (rapidjson::SizeType i = 0; i < ndim; i++) {
      (*shape)[i] = rjshape[i];
    }
    generic_ref_allocator(x).Free(rjshape);
    out = (size_t)ndim;
  } catch(...) {
    YggLogError << "generic_ref_get_ndarray_complex_float: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_ndarray_complex_double(generic_ref_t x, complex_double_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_ndarray_complex_double: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsNDArray<std::complex<double>>()) {
      YggLogError << "generic_ref_get_ndarray_complex_double: Generic object is not ndarray_complex_double: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType ndim = 0;
    rapidjson::SizeType* rjshape = NULL;
    value[0] = (complex_double_t*)(d->GetNDArray<std::complex<double>>(rjshape, ndim, generic_ref_allocator(x)));
    shape[0] = (size_t*)(generic_ref_allocator(x).Malloc(ndim * sizeof(size_t)));
    for (rapidjson::SizeType i = 0; i < ndim; i++) {
      (*shape)[i] = rjshape[i];
    }
    generic_ref_allocator(x).Free(rjshape);
    out = (size_t)ndim;
  } catch(...) {
    YggLogError << "generic_ref_get_ndarray_complex_double: C++ exception thrown" << std::endl;
  }
  return out;
}
generic_t generic_ref_get_schema(generic_ref_t x) {
  generic_t out;
  out = init_generic();
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_schema: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsSchema()) {
      YggLogError << "generic_ref_get_schema: Generic object is not schema: " << (*d) << std::endl;
      return out;
    }
    rapidjson::Document* cpy = new rapidjson::Document();
    cpy->CopyFrom(*d, cpy->GetAllocator(), true);
    out.obj = (void*)cpy;
  } catch(...) {
    YggLogError << "generic_ref_get_schema: C++ exception thrown" << std::endl;
  }
  return out;
}
generic_t generic_ref_get_any(generic_ref_t x) {
  generic_t out;
  out = init_generic();
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_any: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!true) {
      YggLogError << "generic_ref_get_any: Generic object is not any: " << (*d) << std::endl;
      return out;
    }
    rapidjson::Document* cpy = new rapidjson::Document();
    cpy->CopyFrom(*d, cpy->GetAllocator(), true);
    out.obj = (void*)cpy;
  } catch(...) {
    YggLogError << "generic_ref_get_any: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_null(generic_t x, const size_t index, const void* value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_null(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_null: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_bool(generic_t x, const size_t index, const bool value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_bool(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_bool: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_integer(generic_t x, const size_t index, const int value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_integer(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_integer: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_number(generic_t x, const size_t index, const double value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_number(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_number: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_string(generic_t x, const size_t index, const char* value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_string(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_string: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_item(generic_t x, const size_t index, const char* type, void* value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_item(item, type, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_item: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_array(generic_t x, const size_t index, const generic_t value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_array(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_array: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_object(generic_t x, const size_t index, const generic_t value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_object(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_object: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_ply(generic_t x, const size_t index, const ply_t value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ply(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_ply: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_obj(generic_t x, const size_t index, const obj_t value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_obj(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_obj: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_python_class(generic_t x, const size_t index, const python_t value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_python_class(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_python_class: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_python_function(generic_t x, const size_t index, const python_t value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_python_function(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_python_function: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_python_instance(generic_t x, const size_t index, const python_t value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_python_instance(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_python_instance: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_scalar(generic_t x, const size_t index, const void* value, const char* subtype, const size_t precision, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_scalar(item, value, subtype, precision, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_scalar: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_int8(generic_t x, const size_t index, const int8_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_int8(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_int8: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_int16(generic_t x, const size_t index, const int16_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_int16(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_int16: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_int32(generic_t x, const size_t index, const int32_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_int32(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_int32: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_int64(generic_t x, const size_t index, const int64_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_int64(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_int64: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_uint8(generic_t x, const size_t index, const uint8_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_uint8(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_uint8: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_uint16(generic_t x, const size_t index, const uint16_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_uint16(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_uint16: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_uint32(generic_t x, const size_t index, const uint32_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_uint32(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_uint32: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_uint64(generic_t x, const size_t index, const uint64_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_uint64(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_uint64: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_float(generic_t x, const size_t index, const float value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_float(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_float: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_double(generic_t x, const size_t index, const double value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_double(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_complex_float(generic_t x, const size_t index, const complex_float_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_complex_float(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_complex_float: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_complex_double(generic_t x, const size_t index, const complex_double_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_complex_double(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_complex_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_1darray(generic_t x, const size_t index, const void* value, const char* subtype, const size_t precision, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray(item, value, subtype, precision, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_1darray: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_1darray_int8(generic_t x, const size_t index, const int8_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_int8(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_1darray_int8: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_1darray_int16(generic_t x, const size_t index, const int16_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_int16(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_1darray_int16: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_1darray_int32(generic_t x, const size_t index, const int32_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_int32(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_1darray_int32: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_1darray_int64(generic_t x, const size_t index, const int64_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_int64(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_1darray_int64: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_1darray_uint8(generic_t x, const size_t index, const uint8_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_uint8(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_1darray_uint8: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_1darray_uint16(generic_t x, const size_t index, const uint16_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_uint16(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_1darray_uint16: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_1darray_uint32(generic_t x, const size_t index, const uint32_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_uint32(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_1darray_uint32: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_1darray_uint64(generic_t x, const size_t index, const uint64_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_uint64(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_1darray_uint64: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_1darray_float(generic_t x, const size_t index, const float* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_float(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_1darray_float: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_1darray_double(generic_t x, const size_t index, const double* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_double(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_1darray_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_1darray_complex_float(generic_t x, const size_t index, const complex_float_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_complex_float(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_1darray_complex_float: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_1darray_complex_double(generic_t x, const size_t index, const complex_double_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_complex_double(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_1darray_complex_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_ndarray(generic_t x, const size_t index, const void* value, const char* subtype, const size_t precision, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray(item, value, subtype, precision, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_ndarray: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_ndarray_int8(generic_t x, const size_t index, const int8_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_int8(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_ndarray_int8: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_ndarray_int16(generic_t x, const size_t index, const int16_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_int16(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_ndarray_int16: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_ndarray_int32(generic_t x, const size_t index, const int32_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_int32(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_ndarray_int32: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_ndarray_int64(generic_t x, const size_t index, const int64_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_int64(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_ndarray_int64: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_ndarray_uint8(generic_t x, const size_t index, const uint8_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_uint8(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_ndarray_uint8: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_ndarray_uint16(generic_t x, const size_t index, const uint16_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_uint16(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_ndarray_uint16: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_ndarray_uint32(generic_t x, const size_t index, const uint32_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_uint32(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_ndarray_uint32: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_ndarray_uint64(generic_t x, const size_t index, const uint64_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_uint64(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_ndarray_uint64: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_ndarray_float(generic_t x, const size_t index, const float* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_float(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_ndarray_float: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_ndarray_double(generic_t x, const size_t index, const double* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_double(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_ndarray_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_ndarray_complex_float(generic_t x, const size_t index, const complex_float_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_complex_float(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_ndarray_complex_float: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_ndarray_complex_double(generic_t x, const size_t index, const complex_double_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_complex_double(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_ndarray_complex_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_schema(generic_t x, const size_t index, const generic_t value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_schema(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_schema: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_any(generic_t x, const size_t index, const generic_t value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_any(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_any: C++ exception thrown" << std::endl;
  }
  return out;
}
void* generic_array_get_null(generic_t x, const size_t index) {
  void* out;
  out = NULL;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_null(item);
  } catch(...) {
    YggLogError << "generic_array_get_null: C++ exception thrown" << std::endl;
  }
  return out;
}
bool generic_array_get_bool(generic_t x, const size_t index) {
  bool out;
  out = false;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_bool(item);
  } catch(...) {
    YggLogError << "generic_array_get_bool: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_get_integer(generic_t x, const size_t index) {
  int out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_integer(item);
  } catch(...) {
    YggLogError << "generic_array_get_integer: C++ exception thrown" << std::endl;
  }
  return out;
}
double generic_array_get_number(generic_t x, const size_t index) {
  double out;
  out = 0.0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_number(item);
  } catch(...) {
    YggLogError << "generic_array_get_number: C++ exception thrown" << std::endl;
  }
  return out;
}
const char* generic_array_get_string(generic_t x, const size_t index) {
  const char* out;
  out = "";
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_string(item);
  } catch(...) {
    YggLogError << "generic_array_get_string: C++ exception thrown" << std::endl;
  }
  return out;
}
void* generic_array_get_item(generic_t x, const size_t index, const char* type) {
  void* out;
  out = NULL;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_item(item, type);
  } catch(...) {
    YggLogError << "generic_array_get_item: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_get_item_nbytes(generic_t x, const size_t index, const char* type) {
  int out;
  out = -1;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_item_nbytes(item, type);
  } catch(...) {
    YggLogError << "generic_array_get_item_nbytes: C++ exception thrown" << std::endl;
  }
  return out;
}
generic_t generic_array_get_array(generic_t x, const size_t index) {
  generic_t out;
  out = init_generic();
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_array(item);
  } catch(...) {
    YggLogError << "generic_array_get_array: C++ exception thrown" << std::endl;
  }
  return out;
}
generic_t generic_array_get_object(generic_t x, const size_t index) {
  generic_t out;
  out = init_generic();
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_object(item);
  } catch(...) {
    YggLogError << "generic_array_get_object: C++ exception thrown" << std::endl;
  }
  return out;
}
ply_t generic_array_get_ply(generic_t x, const size_t index) {
  ply_t out;
  out = init_ply();
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ply(item);
  } catch(...) {
    YggLogError << "generic_array_get_ply: C++ exception thrown" << std::endl;
  }
  return out;
}
obj_t generic_array_get_obj(generic_t x, const size_t index) {
  obj_t out;
  out = init_obj();
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_obj(item);
  } catch(...) {
    YggLogError << "generic_array_get_obj: C++ exception thrown" << std::endl;
  }
  return out;
}
python_t generic_array_get_python_class(generic_t x, const size_t index) {
  python_t out;
  out = init_python();
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_python_class(item);
  } catch(...) {
    YggLogError << "generic_array_get_python_class: C++ exception thrown" << std::endl;
  }
  return out;
}
python_t generic_array_get_python_function(generic_t x, const size_t index) {
  python_t out;
  out = init_python();
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_python_function(item);
  } catch(...) {
    YggLogError << "generic_array_get_python_function: C++ exception thrown" << std::endl;
  }
  return out;
}
python_t generic_array_get_python_instance(generic_t x, const size_t index) {
  python_t out;
  out = init_python();
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_python_instance(item);
  } catch(...) {
    YggLogError << "generic_array_get_python_instance: C++ exception thrown" << std::endl;
  }
  return out;
}
void* generic_array_get_scalar(generic_t x, const size_t index, const char* subtype, const size_t precision) {
  void* out;
  out = NULL;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_scalar(item, subtype, precision);
  } catch(...) {
    YggLogError << "generic_array_get_scalar: C++ exception thrown" << std::endl;
  }
  return out;
}
int8_t generic_array_get_int8(generic_t x, const size_t index) {
  int8_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_int8(item);
  } catch(...) {
    YggLogError << "generic_array_get_int8: C++ exception thrown" << std::endl;
  }
  return out;
}
int16_t generic_array_get_int16(generic_t x, const size_t index) {
  int16_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_int16(item);
  } catch(...) {
    YggLogError << "generic_array_get_int16: C++ exception thrown" << std::endl;
  }
  return out;
}
int32_t generic_array_get_int32(generic_t x, const size_t index) {
  int32_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_int32(item);
  } catch(...) {
    YggLogError << "generic_array_get_int32: C++ exception thrown" << std::endl;
  }
  return out;
}
int64_t generic_array_get_int64(generic_t x, const size_t index) {
  int64_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_int64(item);
  } catch(...) {
    YggLogError << "generic_array_get_int64: C++ exception thrown" << std::endl;
  }
  return out;
}
uint8_t generic_array_get_uint8(generic_t x, const size_t index) {
  uint8_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_uint8(item);
  } catch(...) {
    YggLogError << "generic_array_get_uint8: C++ exception thrown" << std::endl;
  }
  return out;
}
uint16_t generic_array_get_uint16(generic_t x, const size_t index) {
  uint16_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_uint16(item);
  } catch(...) {
    YggLogError << "generic_array_get_uint16: C++ exception thrown" << std::endl;
  }
  return out;
}
uint32_t generic_array_get_uint32(generic_t x, const size_t index) {
  uint32_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_uint32(item);
  } catch(...) {
    YggLogError << "generic_array_get_uint32: C++ exception thrown" << std::endl;
  }
  return out;
}
uint64_t generic_array_get_uint64(generic_t x, const size_t index) {
  uint64_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_uint64(item);
  } catch(...) {
    YggLogError << "generic_array_get_uint64: C++ exception thrown" << std::endl;
  }
  return out;
}
float generic_array_get_float(generic_t x, const size_t index) {
  float out;
  out = 0.0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_float(item);
  } catch(...) {
    YggLogError << "generic_array_get_float: C++ exception thrown" << std::endl;
  }
  return out;
}
double generic_array_get_double(generic_t x, const size_t index) {
  double out;
  out = 0.0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_double(item);
  } catch(...) {
    YggLogError << "generic_array_get_double: C++ exception thrown" << std::endl;
  }
  return out;
}
complex_float_t generic_array_get_complex_float(generic_t x, const size_t index) {
  complex_float_t out;
  out.re = 0.0;
  out.im = 0.0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_complex_float(item);
  } catch(...) {
    YggLogError << "generic_array_get_complex_float: C++ exception thrown" << std::endl;
  }
  return out;
}
complex_double_t generic_array_get_complex_double(generic_t x, const size_t index) {
  complex_double_t out;
  out.re = 0.0;
  out.im = 0.0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_complex_double(item);
  } catch(...) {
    YggLogError << "generic_array_get_complex_double: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_1darray(generic_t x, const size_t index, const char* subtype, const size_t precision, void** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray(item, subtype, precision, value);
  } catch(...) {
    YggLogError << "generic_array_get_1darray: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_1darray_int8(generic_t x, const size_t index, int8_t** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_int8(item, value);
  } catch(...) {
    YggLogError << "generic_array_get_1darray_int8: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_1darray_int16(generic_t x, const size_t index, int16_t** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_int16(item, value);
  } catch(...) {
    YggLogError << "generic_array_get_1darray_int16: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_1darray_int32(generic_t x, const size_t index, int32_t** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_int32(item, value);
  } catch(...) {
    YggLogError << "generic_array_get_1darray_int32: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_1darray_int64(generic_t x, const size_t index, int64_t** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_int64(item, value);
  } catch(...) {
    YggLogError << "generic_array_get_1darray_int64: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_1darray_uint8(generic_t x, const size_t index, uint8_t** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_uint8(item, value);
  } catch(...) {
    YggLogError << "generic_array_get_1darray_uint8: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_1darray_uint16(generic_t x, const size_t index, uint16_t** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_uint16(item, value);
  } catch(...) {
    YggLogError << "generic_array_get_1darray_uint16: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_1darray_uint32(generic_t x, const size_t index, uint32_t** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_uint32(item, value);
  } catch(...) {
    YggLogError << "generic_array_get_1darray_uint32: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_1darray_uint64(generic_t x, const size_t index, uint64_t** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_uint64(item, value);
  } catch(...) {
    YggLogError << "generic_array_get_1darray_uint64: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_1darray_float(generic_t x, const size_t index, float** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_float(item, value);
  } catch(...) {
    YggLogError << "generic_array_get_1darray_float: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_1darray_double(generic_t x, const size_t index, double** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_double(item, value);
  } catch(...) {
    YggLogError << "generic_array_get_1darray_double: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_1darray_complex_float(generic_t x, const size_t index, complex_float_t** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_complex_float(item, value);
  } catch(...) {
    YggLogError << "generic_array_get_1darray_complex_float: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_1darray_complex_double(generic_t x, const size_t index, complex_double_t** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_complex_double(item, value);
  } catch(...) {
    YggLogError << "generic_array_get_1darray_complex_double: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_ndarray(generic_t x, const size_t index, const char* subtype, const size_t precision, void** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray(item, subtype, precision, value, shape);
  } catch(...) {
    YggLogError << "generic_array_get_ndarray: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_ndarray_int8(generic_t x, const size_t index, int8_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_int8(item, value, shape);
  } catch(...) {
    YggLogError << "generic_array_get_ndarray_int8: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_ndarray_int16(generic_t x, const size_t index, int16_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_int16(item, value, shape);
  } catch(...) {
    YggLogError << "generic_array_get_ndarray_int16: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_ndarray_int32(generic_t x, const size_t index, int32_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_int32(item, value, shape);
  } catch(...) {
    YggLogError << "generic_array_get_ndarray_int32: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_ndarray_int64(generic_t x, const size_t index, int64_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_int64(item, value, shape);
  } catch(...) {
    YggLogError << "generic_array_get_ndarray_int64: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_ndarray_uint8(generic_t x, const size_t index, uint8_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_uint8(item, value, shape);
  } catch(...) {
    YggLogError << "generic_array_get_ndarray_uint8: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_ndarray_uint16(generic_t x, const size_t index, uint16_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_uint16(item, value, shape);
  } catch(...) {
    YggLogError << "generic_array_get_ndarray_uint16: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_ndarray_uint32(generic_t x, const size_t index, uint32_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_uint32(item, value, shape);
  } catch(...) {
    YggLogError << "generic_array_get_ndarray_uint32: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_ndarray_uint64(generic_t x, const size_t index, uint64_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_uint64(item, value, shape);
  } catch(...) {
    YggLogError << "generic_array_get_ndarray_uint64: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_ndarray_float(generic_t x, const size_t index, float** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_float(item, value, shape);
  } catch(...) {
    YggLogError << "generic_array_get_ndarray_float: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_ndarray_double(generic_t x, const size_t index, double** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_double(item, value, shape);
  } catch(...) {
    YggLogError << "generic_array_get_ndarray_double: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_ndarray_complex_float(generic_t x, const size_t index, complex_float_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_complex_float(item, value, shape);
  } catch(...) {
    YggLogError << "generic_array_get_ndarray_complex_float: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_ndarray_complex_double(generic_t x, const size_t index, complex_double_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_complex_double(item, value, shape);
  } catch(...) {
    YggLogError << "generic_array_get_ndarray_complex_double: C++ exception thrown" << std::endl;
  }
  return out;
}
generic_t generic_array_get_schema(generic_t x, const size_t index) {
  generic_t out;
  out = init_generic();
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_schema(item);
  } catch(...) {
    YggLogError << "generic_array_get_schema: C++ exception thrown" << std::endl;
  }
  return out;
}
generic_t generic_array_get_any(generic_t x, const size_t index) {
  generic_t out;
  out = init_generic();
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_any(item);
  } catch(...) {
    YggLogError << "generic_array_get_any: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_null(generic_t x, const char* key, const void* value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_null(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_null: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_bool(generic_t x, const char* key, const bool value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_bool(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_bool: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_integer(generic_t x, const char* key, const int value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_integer(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_integer: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_number(generic_t x, const char* key, const double value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_number(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_number: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_string(generic_t x, const char* key, const char* value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_string(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_string: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_item(generic_t x, const char* key, const char* type, void* value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_item(item, type, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_item: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_array(generic_t x, const char* key, const generic_t value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_array(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_array: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_object(generic_t x, const char* key, const generic_t value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_object(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_object: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_ply(generic_t x, const char* key, const ply_t value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ply(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_ply: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_obj(generic_t x, const char* key, const obj_t value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_obj(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_obj: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_python_class(generic_t x, const char* key, const python_t value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_python_class(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_python_class: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_python_function(generic_t x, const char* key, const python_t value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_python_function(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_python_function: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_python_instance(generic_t x, const char* key, const python_t value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_python_instance(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_python_instance: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_scalar(generic_t x, const char* key, const void* value, const char* subtype, const size_t precision, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_scalar(item, value, subtype, precision, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_scalar: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_int8(generic_t x, const char* key, const int8_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_int8(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_int8: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_int16(generic_t x, const char* key, const int16_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_int16(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_int16: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_int32(generic_t x, const char* key, const int32_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_int32(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_int32: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_int64(generic_t x, const char* key, const int64_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_int64(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_int64: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_uint8(generic_t x, const char* key, const uint8_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_uint8(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_uint8: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_uint16(generic_t x, const char* key, const uint16_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_uint16(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_uint16: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_uint32(generic_t x, const char* key, const uint32_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_uint32(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_uint32: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_uint64(generic_t x, const char* key, const uint64_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_uint64(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_uint64: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_float(generic_t x, const char* key, const float value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_float(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_float: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_double(generic_t x, const char* key, const double value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_double(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_complex_float(generic_t x, const char* key, const complex_float_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_complex_float(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_complex_float: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_complex_double(generic_t x, const char* key, const complex_double_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_complex_double(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_complex_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_1darray(generic_t x, const char* key, const void* value, const char* subtype, const size_t precision, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray(item, value, subtype, precision, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_1darray: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_1darray_int8(generic_t x, const char* key, const int8_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_int8(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_1darray_int8: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_1darray_int16(generic_t x, const char* key, const int16_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_int16(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_1darray_int16: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_1darray_int32(generic_t x, const char* key, const int32_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_int32(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_1darray_int32: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_1darray_int64(generic_t x, const char* key, const int64_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_int64(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_1darray_int64: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_1darray_uint8(generic_t x, const char* key, const uint8_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_uint8(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_1darray_uint8: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_1darray_uint16(generic_t x, const char* key, const uint16_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_uint16(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_1darray_uint16: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_1darray_uint32(generic_t x, const char* key, const uint32_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_uint32(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_1darray_uint32: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_1darray_uint64(generic_t x, const char* key, const uint64_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_uint64(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_1darray_uint64: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_1darray_float(generic_t x, const char* key, const float* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_float(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_1darray_float: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_1darray_double(generic_t x, const char* key, const double* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_double(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_1darray_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_1darray_complex_float(generic_t x, const char* key, const complex_float_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_complex_float(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_1darray_complex_float: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_1darray_complex_double(generic_t x, const char* key, const complex_double_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_complex_double(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_1darray_complex_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_ndarray(generic_t x, const char* key, const void* value, const char* subtype, const size_t precision, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray(item, value, subtype, precision, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_ndarray: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_ndarray_int8(generic_t x, const char* key, const int8_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_int8(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_ndarray_int8: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_ndarray_int16(generic_t x, const char* key, const int16_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_int16(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_ndarray_int16: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_ndarray_int32(generic_t x, const char* key, const int32_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_int32(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_ndarray_int32: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_ndarray_int64(generic_t x, const char* key, const int64_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_int64(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_ndarray_int64: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_ndarray_uint8(generic_t x, const char* key, const uint8_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_uint8(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_ndarray_uint8: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_ndarray_uint16(generic_t x, const char* key, const uint16_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_uint16(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_ndarray_uint16: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_ndarray_uint32(generic_t x, const char* key, const uint32_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_uint32(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_ndarray_uint32: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_ndarray_uint64(generic_t x, const char* key, const uint64_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_uint64(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_ndarray_uint64: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_ndarray_float(generic_t x, const char* key, const float* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_float(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_ndarray_float: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_ndarray_double(generic_t x, const char* key, const double* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_double(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_ndarray_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_ndarray_complex_float(generic_t x, const char* key, const complex_float_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_complex_float(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_ndarray_complex_float: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_ndarray_complex_double(generic_t x, const char* key, const complex_double_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_complex_double(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_ndarray_complex_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_schema(generic_t x, const char* key, const generic_t value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_schema(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_schema: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_any(generic_t x, const char* key, const generic_t value) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_any(item, value) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_any: C++ exception thrown" << std::endl;
  }
  return out;
}
void* generic_object_get_null(generic_t x, const char* key) {
  void* out;
  out = NULL;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_null(item);
  } catch(...) {
    YggLogError << "generic_object_get_null: C++ exception thrown" << std::endl;
  }
  return out;
}
bool generic_object_get_bool(generic_t x, const char* key) {
  bool out;
  out = false;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_bool(item);
  } catch(...) {
    YggLogError << "generic_object_get_bool: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_get_integer(generic_t x, const char* key) {
  int out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_integer(item);
  } catch(...) {
    YggLogError << "generic_object_get_integer: C++ exception thrown" << std::endl;
  }
  return out;
}
double generic_object_get_number(generic_t x, const char* key) {
  double out;
  out = 0.0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_number(item);
  } catch(...) {
    YggLogError << "generic_object_get_number: C++ exception thrown" << std::endl;
  }
  return out;
}
const char* generic_object_get_string(generic_t x, const char* key) {
  const char* out;
  out = "";
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_string(item);
  } catch(...) {
    YggLogError << "generic_object_get_string: C++ exception thrown" << std::endl;
  }
  return out;
}
void* generic_object_get_item(generic_t x, const char* key, const char* type) {
  void* out;
  out = NULL;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_item(item, type);
  } catch(...) {
    YggLogError << "generic_object_get_item: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_get_item_nbytes(generic_t x, const char* key, const char* type) {
  int out;
  out = -1;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_item_nbytes(item, type);
  } catch(...) {
    YggLogError << "generic_object_get_item_nbytes: C++ exception thrown" << std::endl;
  }
  return out;
}
generic_t generic_object_get_array(generic_t x, const char* key) {
  generic_t out;
  out = init_generic();
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_array(item);
  } catch(...) {
    YggLogError << "generic_object_get_array: C++ exception thrown" << std::endl;
  }
  return out;
}
generic_t generic_object_get_object(generic_t x, const char* key) {
  generic_t out;
  out = init_generic();
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_object(item);
  } catch(...) {
    YggLogError << "generic_object_get_object: C++ exception thrown" << std::endl;
  }
  return out;
}
ply_t generic_object_get_ply(generic_t x, const char* key) {
  ply_t out;
  out = init_ply();
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ply(item);
  } catch(...) {
    YggLogError << "generic_object_get_ply: C++ exception thrown" << std::endl;
  }
  return out;
}
obj_t generic_object_get_obj(generic_t x, const char* key) {
  obj_t out;
  out = init_obj();
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_obj(item);
  } catch(...) {
    YggLogError << "generic_object_get_obj: C++ exception thrown" << std::endl;
  }
  return out;
}
python_t generic_object_get_python_class(generic_t x, const char* key) {
  python_t out;
  out = init_python();
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_python_class(item);
  } catch(...) {
    YggLogError << "generic_object_get_python_class: C++ exception thrown" << std::endl;
  }
  return out;
}
python_t generic_object_get_python_function(generic_t x, const char* key) {
  python_t out;
  out = init_python();
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_python_function(item);
  } catch(...) {
    YggLogError << "generic_object_get_python_function: C++ exception thrown" << std::endl;
  }
  return out;
}
python_t generic_object_get_python_instance(generic_t x, const char* key) {
  python_t out;
  out = init_python();
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_python_instance(item);
  } catch(...) {
    YggLogError << "generic_object_get_python_instance: C++ exception thrown" << std::endl;
  }
  return out;
}
void* generic_object_get_scalar(generic_t x, const char* key, const char* subtype, const size_t precision) {
  void* out;
  out = NULL;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_scalar(item, subtype, precision);
  } catch(...) {
    YggLogError << "generic_object_get_scalar: C++ exception thrown" << std::endl;
  }
  return out;
}
int8_t generic_object_get_int8(generic_t x, const char* key) {
  int8_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_int8(item);
  } catch(...) {
    YggLogError << "generic_object_get_int8: C++ exception thrown" << std::endl;
  }
  return out;
}
int16_t generic_object_get_int16(generic_t x, const char* key) {
  int16_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_int16(item);
  } catch(...) {
    YggLogError << "generic_object_get_int16: C++ exception thrown" << std::endl;
  }
  return out;
}
int32_t generic_object_get_int32(generic_t x, const char* key) {
  int32_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_int32(item);
  } catch(...) {
    YggLogError << "generic_object_get_int32: C++ exception thrown" << std::endl;
  }
  return out;
}
int64_t generic_object_get_int64(generic_t x, const char* key) {
  int64_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_int64(item);
  } catch(...) {
    YggLogError << "generic_object_get_int64: C++ exception thrown" << std::endl;
  }
  return out;
}
uint8_t generic_object_get_uint8(generic_t x, const char* key) {
  uint8_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_uint8(item);
  } catch(...) {
    YggLogError << "generic_object_get_uint8: C++ exception thrown" << std::endl;
  }
  return out;
}
uint16_t generic_object_get_uint16(generic_t x, const char* key) {
  uint16_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_uint16(item);
  } catch(...) {
    YggLogError << "generic_object_get_uint16: C++ exception thrown" << std::endl;
  }
  return out;
}
uint32_t generic_object_get_uint32(generic_t x, const char* key) {
  uint32_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_uint32(item);
  } catch(...) {
    YggLogError << "generic_object_get_uint32: C++ exception thrown" << std::endl;
  }
  return out;
}
uint64_t generic_object_get_uint64(generic_t x, const char* key) {
  uint64_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_uint64(item);
  } catch(...) {
    YggLogError << "generic_object_get_uint64: C++ exception thrown" << std::endl;
  }
  return out;
}
float generic_object_get_float(generic_t x, const char* key) {
  float out;
  out = 0.0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_float(item);
  } catch(...) {
    YggLogError << "generic_object_get_float: C++ exception thrown" << std::endl;
  }
  return out;
}
double generic_object_get_double(generic_t x, const char* key) {
  double out;
  out = 0.0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_double(item);
  } catch(...) {
    YggLogError << "generic_object_get_double: C++ exception thrown" << std::endl;
  }
  return out;
}
complex_float_t generic_object_get_complex_float(generic_t x, const char* key) {
  complex_float_t out;
  out.re = 0.0;
  out.im = 0.0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_complex_float(item);
  } catch(...) {
    YggLogError << "generic_object_get_complex_float: C++ exception thrown" << std::endl;
  }
  return out;
}
complex_double_t generic_object_get_complex_double(generic_t x, const char* key) {
  complex_double_t out;
  out.re = 0.0;
  out.im = 0.0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_complex_double(item);
  } catch(...) {
    YggLogError << "generic_object_get_complex_double: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_1darray(generic_t x, const char* key, const char* subtype, const size_t precision, void** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray(item, subtype, precision, value);
  } catch(...) {
    YggLogError << "generic_object_get_1darray: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_1darray_int8(generic_t x, const char* key, int8_t** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_int8(item, value);
  } catch(...) {
    YggLogError << "generic_object_get_1darray_int8: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_1darray_int16(generic_t x, const char* key, int16_t** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_int16(item, value);
  } catch(...) {
    YggLogError << "generic_object_get_1darray_int16: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_1darray_int32(generic_t x, const char* key, int32_t** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_int32(item, value);
  } catch(...) {
    YggLogError << "generic_object_get_1darray_int32: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_1darray_int64(generic_t x, const char* key, int64_t** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_int64(item, value);
  } catch(...) {
    YggLogError << "generic_object_get_1darray_int64: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_1darray_uint8(generic_t x, const char* key, uint8_t** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_uint8(item, value);
  } catch(...) {
    YggLogError << "generic_object_get_1darray_uint8: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_1darray_uint16(generic_t x, const char* key, uint16_t** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_uint16(item, value);
  } catch(...) {
    YggLogError << "generic_object_get_1darray_uint16: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_1darray_uint32(generic_t x, const char* key, uint32_t** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_uint32(item, value);
  } catch(...) {
    YggLogError << "generic_object_get_1darray_uint32: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_1darray_uint64(generic_t x, const char* key, uint64_t** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_uint64(item, value);
  } catch(...) {
    YggLogError << "generic_object_get_1darray_uint64: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_1darray_float(generic_t x, const char* key, float** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_float(item, value);
  } catch(...) {
    YggLogError << "generic_object_get_1darray_float: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_1darray_double(generic_t x, const char* key, double** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_double(item, value);
  } catch(...) {
    YggLogError << "generic_object_get_1darray_double: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_1darray_complex_float(generic_t x, const char* key, complex_float_t** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_complex_float(item, value);
  } catch(...) {
    YggLogError << "generic_object_get_1darray_complex_float: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_1darray_complex_double(generic_t x, const char* key, complex_double_t** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_complex_double(item, value);
  } catch(...) {
    YggLogError << "generic_object_get_1darray_complex_double: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_ndarray(generic_t x, const char* key, const char* subtype, const size_t precision, void** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray(item, subtype, precision, value, shape);
  } catch(...) {
    YggLogError << "generic_object_get_ndarray: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_ndarray_int8(generic_t x, const char* key, int8_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_int8(item, value, shape);
  } catch(...) {
    YggLogError << "generic_object_get_ndarray_int8: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_ndarray_int16(generic_t x, const char* key, int16_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_int16(item, value, shape);
  } catch(...) {
    YggLogError << "generic_object_get_ndarray_int16: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_ndarray_int32(generic_t x, const char* key, int32_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_int32(item, value, shape);
  } catch(...) {
    YggLogError << "generic_object_get_ndarray_int32: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_ndarray_int64(generic_t x, const char* key, int64_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_int64(item, value, shape);
  } catch(...) {
    YggLogError << "generic_object_get_ndarray_int64: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_ndarray_uint8(generic_t x, const char* key, uint8_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_uint8(item, value, shape);
  } catch(...) {
    YggLogError << "generic_object_get_ndarray_uint8: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_ndarray_uint16(generic_t x, const char* key, uint16_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_uint16(item, value, shape);
  } catch(...) {
    YggLogError << "generic_object_get_ndarray_uint16: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_ndarray_uint32(generic_t x, const char* key, uint32_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_uint32(item, value, shape);
  } catch(...) {
    YggLogError << "generic_object_get_ndarray_uint32: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_ndarray_uint64(generic_t x, const char* key, uint64_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_uint64(item, value, shape);
  } catch(...) {
    YggLogError << "generic_object_get_ndarray_uint64: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_ndarray_float(generic_t x, const char* key, float** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_float(item, value, shape);
  } catch(...) {
    YggLogError << "generic_object_get_ndarray_float: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_ndarray_double(generic_t x, const char* key, double** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_double(item, value, shape);
  } catch(...) {
    YggLogError << "generic_object_get_ndarray_double: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_ndarray_complex_float(generic_t x, const char* key, complex_float_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_complex_float(item, value, shape);
  } catch(...) {
    YggLogError << "generic_object_get_ndarray_complex_float: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_ndarray_complex_double(generic_t x, const char* key, complex_double_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_complex_double(item, value, shape);
  } catch(...) {
    YggLogError << "generic_object_get_ndarray_complex_double: C++ exception thrown" << std::endl;
  }
  return out;
}
generic_t generic_object_get_schema(generic_t x, const char* key) {
  generic_t out;
  out = init_generic();
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_schema(item);
  } catch(...) {
    YggLogError << "generic_object_get_schema: C++ exception thrown" << std::endl;
  }
  return out;
}
generic_t generic_object_get_any(generic_t x, const char* key) {
  generic_t out;
  out = init_generic();
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_any(item);
  } catch(...) {
    YggLogError << "generic_object_get_any: C++ exception thrown" << std::endl;
  }
  return out;
}
#ifdef YGGDRASIL_LONG_DOUBLE_AVAILABLE
int generic_set_long_double(generic_t x, const long double value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->SetScalar(value, units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_complex_long_double(generic_t x, const complex_long_double_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->SetScalar(std::complex<long double>(value.re, value.im), units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_complex_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_1darray_long_double(generic_t x, const long double* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->Set1DArray((long double*)value, (rapidjson::SizeType)length, units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_1darray_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_1darray_complex_long_double(generic_t x, const complex_long_double_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    d->Set1DArray((std::complex<long double>*)value, (rapidjson::SizeType)length, units, generic_allocator(x));
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_1darray_complex_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_ndarray_long_double(generic_t x, const long double* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    rapidjson::SizeType* rjshape = (rapidjson::SizeType*)(generic_allocator(x).Malloc(ndim * sizeof(rapidjson::SizeType)));
    for (size_t i = 0; i < ndim; i++) {
      rjshape[i] = (rapidjson::SizeType)(shape[i]);}
    d->SetNDArray((long double*)value, rjshape, (rapidjson::SizeType)ndim, units, generic_allocator(x));
    generic_allocator(x).Free(rjshape);;
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_ndarray_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_set_ndarray_complex_long_double(generic_t x, const complex_long_double_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    if (!is_generic_init(x)) {
      YggLogError << "Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    rapidjson::SizeType* rjshape = (rapidjson::SizeType*)(generic_allocator(x).Malloc(ndim * sizeof(rapidjson::SizeType)));
    for (size_t i = 0; i < ndim; i++) {
      rjshape[i] = (rapidjson::SizeType)(shape[i]);}
    d->SetNDArray((std::complex<long double>*)value, rjshape, (rapidjson::SizeType)ndim, units, generic_allocator(x));
    generic_allocator(x).Free(rjshape);;
    out = GENERIC_SUCCESS_;
  } catch(...) {
    YggLogError << "generic_set_ndarray_complex_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
long double generic_get_long_double(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_long_double(x_ref);
}
complex_long_double_t generic_get_complex_long_double(generic_t x) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_complex_long_double(x_ref);
}
size_t generic_get_1darray_long_double(generic_t x, long double** value) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_1darray_long_double(x_ref, value);
}
size_t generic_get_1darray_complex_long_double(generic_t x, complex_long_double_t** value) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_1darray_complex_long_double(x_ref, value);
}
size_t generic_get_ndarray_long_double(generic_t x, long double** value, size_t** shape) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_ndarray_long_double(x_ref, value, shape);
}
size_t generic_get_ndarray_complex_long_double(generic_t x, complex_long_double_t** value, size_t** shape) {
  generic_ref_t x_ref = init_generic_ref(x);
  return generic_ref_get_ndarray_complex_long_double(x_ref, value, shape);
}
long double generic_ref_get_long_double(generic_ref_t x) {
  long double out;
  out = 0.0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_long_double: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsScalar<long double>()) {
      YggLogError << "generic_ref_get_long_double: Generic object is not long_double: " << (*d) << std::endl;
      return out;
    }
    out = d->GetScalar<long double>();
  } catch(...) {
    YggLogError << "generic_ref_get_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
complex_long_double_t generic_ref_get_complex_long_double(generic_ref_t x) {
  complex_long_double_t out;
  out.re = 0.0;
  out.im = 0.0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_complex_long_double: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsScalar<std::complex<long double>>()) {
      YggLogError << "generic_ref_get_complex_long_double: Generic object is not complex_long_double: " << (*d) << std::endl;
      return out;
    }
    std::complex<long double> tmp = d->GetScalar<std::complex<long double> >();
    out.re = tmp.real();
    out.im = tmp.imag();
  } catch(...) {
    YggLogError << "generic_ref_get_complex_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_1darray_long_double(generic_ref_t x, long double** value) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_1darray_long_double: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->Is1DArray<long double>()) {
      YggLogError << "generic_ref_get_1darray_long_double: Generic object is not 1darray_long_double: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType nelements = 0;
    value[0] = (long double*)(d->Get1DArray<long double>(nelements, generic_ref_allocator(x)));
    out = (size_t)nelements;
  } catch(...) {
    YggLogError << "generic_ref_get_1darray_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_1darray_complex_long_double(generic_ref_t x, complex_long_double_t** value) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_1darray_complex_long_double: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->Is1DArray<std::complex<long double>>()) {
      YggLogError << "generic_ref_get_1darray_complex_long_double: Generic object is not 1darray_complex_long_double: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType nelements = 0;
    value[0] = (complex_long_double_t*)(d->Get1DArray<std::complex<long double>>(nelements, generic_ref_allocator(x)));
    out = (size_t)nelements;
  } catch(...) {
    YggLogError << "generic_ref_get_1darray_complex_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_ndarray_long_double(generic_ref_t x, long double** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_ndarray_long_double: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsNDArray<long double>()) {
      YggLogError << "generic_ref_get_ndarray_long_double: Generic object is not ndarray_long_double: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType ndim = 0;
    rapidjson::SizeType* rjshape = NULL;
    value[0] = (long double*)(d->GetNDArray<long double>(rjshape, ndim, generic_ref_allocator(x)));
    shape[0] = (size_t*)(generic_ref_allocator(x).Malloc(ndim * sizeof(size_t)));
    for (rapidjson::SizeType i = 0; i < ndim; i++) {
      (*shape)[i] = rjshape[i];
    }
    generic_ref_allocator(x).Free(rjshape);
    out = (size_t)ndim;
  } catch(...) {
    YggLogError << "generic_ref_get_ndarray_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_ref_get_ndarray_complex_long_double(generic_ref_t x, complex_long_double_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    if (!is_generic_ref_init(x)) {
      YggLogError << "generic_ref_get_ndarray_complex_long_double: Generic object is not initialized" << std::endl;
      return out;
    }
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);
    if (!d->IsNDArray<std::complex<long double>>()) {
      YggLogError << "generic_ref_get_ndarray_complex_long_double: Generic object is not ndarray_complex_long_double: " << (*d) << std::endl;
      return out;
    }
    rapidjson::SizeType ndim = 0;
    rapidjson::SizeType* rjshape = NULL;
    value[0] = (complex_long_double_t*)(d->GetNDArray<std::complex<long double>>(rjshape, ndim, generic_ref_allocator(x)));
    shape[0] = (size_t*)(generic_ref_allocator(x).Malloc(ndim * sizeof(size_t)));
    for (rapidjson::SizeType i = 0; i < ndim; i++) {
      (*shape)[i] = rjshape[i];
    }
    generic_ref_allocator(x).Free(rjshape);
    out = (size_t)ndim;
  } catch(...) {
    YggLogError << "generic_ref_get_ndarray_complex_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_long_double(generic_t x, const size_t index, const long double value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_long_double(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_complex_long_double(generic_t x, const size_t index, const complex_long_double_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_complex_long_double(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_complex_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_1darray_long_double(generic_t x, const size_t index, const long double* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_long_double(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_1darray_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_1darray_complex_long_double(generic_t x, const size_t index, const complex_long_double_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_complex_long_double(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_1darray_complex_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_ndarray_long_double(generic_t x, const size_t index, const long double* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_long_double(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_ndarray_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_array_set_ndarray_complex_long_double(generic_t x, const size_t index, const complex_long_double_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_complex_long_double(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_array(x, index, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_array_set_ndarray_complex_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
long double generic_array_get_long_double(generic_t x, const size_t index) {
  long double out;
  out = 0.0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_long_double(item);
  } catch(...) {
    YggLogError << "generic_array_get_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
complex_long_double_t generic_array_get_complex_long_double(generic_t x, const size_t index) {
  complex_long_double_t out;
  out.re = 0.0;
  out.im = 0.0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_complex_long_double(item);
  } catch(...) {
    YggLogError << "generic_array_get_complex_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_1darray_long_double(generic_t x, const size_t index, long double** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_long_double(item, value);
  } catch(...) {
    YggLogError << "generic_array_get_1darray_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_1darray_complex_long_double(generic_t x, const size_t index, complex_long_double_t** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_complex_long_double(item, value);
  } catch(...) {
    YggLogError << "generic_array_get_1darray_complex_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_ndarray_long_double(generic_t x, const size_t index, long double** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_long_double(item, value, shape);
  } catch(...) {
    YggLogError << "generic_array_get_ndarray_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_array_get_ndarray_complex_long_double(generic_t x, const size_t index, complex_long_double_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_array_ref(x, index, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_complex_long_double(item, value, shape);
  } catch(...) {
    YggLogError << "generic_array_get_ndarray_complex_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_long_double(generic_t x, const char* key, const long double value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_long_double(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_complex_long_double(generic_t x, const char* key, const complex_long_double_t value, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_complex_long_double(item, value, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_complex_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_1darray_long_double(generic_t x, const char* key, const long double* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_long_double(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_1darray_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_1darray_complex_long_double(generic_t x, const char* key, const complex_long_double_t* value, const size_t length, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_1darray_complex_long_double(item, value, length, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_1darray_complex_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_ndarray_long_double(generic_t x, const char* key, const long double* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_long_double(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_ndarray_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
int generic_object_set_ndarray_complex_long_double(generic_t x, const char* key, const complex_long_double_t* value, const size_t ndim, const size_t* shape, const char* units) {
  int out = GENERIC_ERROR_;
  try {
    generic_t item = init_generic_null();
    if (generic_set_ndarray_complex_long_double(item, value, ndim, shape, units) != GENERIC_SUCCESS_) {
      return out;
    }
    out = set_generic_object(x, key, item);
    destroy_generic(&item);
  } catch(...) {
    YggLogError << "generic_object_set_ndarray_complex_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
long double generic_object_get_long_double(generic_t x, const char* key) {
  long double out;
  out = 0.0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_long_double(item);
  } catch(...) {
    YggLogError << "generic_object_get_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
complex_long_double_t generic_object_get_complex_long_double(generic_t x, const char* key) {
  complex_long_double_t out;
  out.re = 0.0;
  out.im = 0.0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_complex_long_double(item);
  } catch(...) {
    YggLogError << "generic_object_get_complex_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_1darray_long_double(generic_t x, const char* key, long double** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_long_double(item, value);
  } catch(...) {
    YggLogError << "generic_object_get_1darray_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_1darray_complex_long_double(generic_t x, const char* key, complex_long_double_t** value) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_1darray_complex_long_double(item, value);
  } catch(...) {
    YggLogError << "generic_object_get_1darray_complex_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_ndarray_long_double(generic_t x, const char* key, long double** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_long_double(item, value, shape);
  } catch(...) {
    YggLogError << "generic_object_get_ndarray_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
size_t generic_object_get_ndarray_complex_long_double(generic_t x, const char* key, complex_long_double_t** value, size_t** shape) {
  size_t out;
  out = 0;
  try {
    generic_ref_t item = init_generic_ref(x);
    if (get_generic_object_ref(x, key, &item) != GENERIC_SUCCESS_) {
      return out;
    }
    out = generic_ref_get_ndarray_complex_long_double(item, value, shape);
  } catch(...) {
    YggLogError << "generic_object_get_ndarray_complex_long_double: C++ exception thrown" << std::endl;
  }
  return out;
}
#endif // YGGDRASIL_LONG_DOUBLE_AVAILABLE

#undef GENERIC_SUCCESS_
#undef GENERIC_ERROR_

} // extern C
