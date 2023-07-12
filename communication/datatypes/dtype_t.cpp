#include "dtype_t.hpp"
#include "utils/serialization.hpp"
#include "utils/tools.hpp"

#define _GET_METADATA(name, in, err)		\
  if (in.metadata == NULL) {			\
    return err;					\
  }						\
  Metadata* name = ((Metadata*)(in.metadata))

// C++ functions
rapidjson::Document::AllocatorType& generic_allocator(generic_t& x) {
  if (!is_generic_init(x))
    ygglog_throw_error_c("generic_allocator: Not initialized");
  return ((rapidjson::Document*)(x.obj))->GetAllocator();
};

rapidjson::Document::AllocatorType& generic_ref_allocator(generic_ref_t& x) {
  if (!is_generic_ref_init(x))
    ygglog_throw_error_c("generic_ref_allocator: Not initialized");
  return *((rapidjson::Document::AllocatorType*)(x.allocator));
};

rapidjson::Document::AllocatorType& dtype_allocator(dtype_t& x) {
  rapidjson::Document* s = NULL;
  if (x.metadata != NULL)
    return ((Metadata*)x.metadata)->GetAllocator();
  else
    ygglog_throw_error_c("dtype_allocator: Not initialized");
  return s->GetAllocator();
};

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
    rapidjson::SchemaDocument s(sd);
    rapidjson::SchemaValidator validator(s);
    rapidjson::Document* x = new rapidjson::Document();
    validator.GenerateData(*x);
    out.obj = (void*)x;
    return out;
  }

  int is_generic_init(generic_t x) {
    return (x.obj != NULL);
  }
  
  int is_generic_ref_init(generic_ref_t x) {
    return (x.obj != NULL && x.allocator != NULL);
  }
  
  int destroy_generic(generic_t* x) {
    int ret = 0;
    if (x != NULL) {
      if (x->obj != NULL) {
	try {
	  rapidjson::Document* obj = (rapidjson::Document*)(x->obj);
	  delete obj;
	  x->obj = NULL;
	} catch (...) {
	  ygglog_error_c("destroy_generic: C++ exception thrown in destructor for rapidjson::Document.");
	  ret = -1;
	}
      }
    }
    return ret;
  }

  int copy_generic_into(generic_t* dst, generic_t src) {
    try {
      if (!dst) {
	ygglog_throw_error_c("copy_generic_into: Destination is empty.");
      }
      destroy_generic(dst);
      dst[0] = init_generic();
      if (!is_generic_init(src)) {
	ygglog_throw_error_c("copy_generic: Generic object class is NULL.");
      }
      rapidjson::Document* doc = new rapidjson::Document();
      doc->CopyFrom(*((rapidjson::Value*)(src.obj)),
		    doc->GetAllocator(), true);
      dst->obj = (void*)doc;
    } catch(...) {
      ygglog_error_c("copy_generic_into: C++ exception thrown.");
      destroy_generic(dst);
      return -1;
    }
    return 0;
  }

  generic_t copy_generic(generic_t src) {
    generic_t out = init_generic();
    copy_generic_into(&out, src);
    return out;
  }

  bool compare_generic(generic_t a, generic_t b) {
    if (!(a.obj && b.obj))
      return false;
    return ((*((rapidjson::Document*)(a.obj))) ==
	    (*((rapidjson::Document*)(b.obj))));
  }
  
  void display_generic(generic_t x) {
    try {
      if (!x.obj)
	ygglog_throw_error_c("display_generic: Object is NULL.");
      std::cout << *((rapidjson::Document*)(x.obj)) << std::endl;
    } catch (...) {
      ygglog_error_c("display_generic: C++ exception thrown.");
    }
  }

#define GENERIC_SUCCESS_ 0
#define GENERIC_ERROR_ -1

  void* generic_ref_get_item(generic_ref_t x, const char *type) {
    void* out = NULL;
    try {
      if (!is_generic_ref_init(x)) {
	ygglog_throw_error_c("generic_ref_get_item: Object is NULL.");
      }
      rapidjson::Value* x_obj = (rapidjson::Value*)(x.obj);
      if (!x_obj->IsType(type)) {
	ygglog_throw_error_c("generic_ref_get_item: Object is not of type \'%s\'", type);
      }
      bool requires_freeing = false;
      out = x_obj->GetDataPtr(requires_freeing);
    } catch (...) {
      ygglog_error_c("generic_ref_get_item: C++ exception thrown.");
      out = NULL;
    }
    return out;
  }
  void* generic_get_item(generic_t x, const char *type) {
    generic_ref_t x_ref = init_generic_ref(x);
    return generic_ref_get_item(x_ref, type);
  }
  int generic_ref_get_item_nbytes(generic_ref_t x, const char *type) {
    int out = -1;
    try {
      if (!is_generic_ref_init(x)) {
	ygglog_throw_error_c("generic_ref_get_item_nbytes: Object is NULL.");
      }
      rapidjson::Value* x_obj = (rapidjson::Value*)(x.obj);
      if (!x_obj->IsType(type)) {
	ygglog_throw_error_c("generic_ref_get_item_nbytes: Object is not of type \'%s\'", type);
      }
      out = x_obj->GetNBytes();
    } catch (...) {
      ygglog_error_c("generic_ref_get_item_nbytes: C++ exception thrown.");
      out = -1;
    }
    return out;
  }
  int generic_get_item_nbytes(generic_t x, const char *type) {
    generic_ref_t x_ref = init_generic_ref(x);
    return generic_ref_get_item_nbytes(x_ref, type);
  }
  int generic_set_item(generic_t x, const char *type, void* value) {
    int out = GENERIC_SUCCESS_;
    try {
      if (!is_generic_init(x)) {
	ygglog_throw_error_c("generic_set_item: Object is NULL.");
      }
      rapidjson::Value* x_obj = (rapidjson::Value*)(x.obj);
      if (!x_obj->SetDataPtr(type, value, generic_allocator(x))) {
	ygglog_throw_error_c("generic_set_item: Error setting data pointer");
      }
    } catch(...) {
      ygglog_error_c("generic_set_item: C++ exception thrown");
      return GENERIC_ERROR_;
    }
    return out;
  }
  int generic_set_json(generic_t x, const char *json) {
    int out = GENERIC_SUCCESS_;
    try {
      if (!is_generic_init(x)) {
	ygglog_throw_error_c("generic_set_json: Object is NULL.");
      }
      rapidjson::Document* x_obj = (rapidjson::Document*)(x.obj);
      x_obj->SetNull();
      x_obj->Parse(json);
    } catch(...) {
      ygglog_error_c("generic_set_json: C++ exception thrown");
      return GENERIC_ERROR_;
    }
    return out;
  }
  void* generic_ref_get_scalar(generic_ref_t x, const char *subtype, const size_t precision) {
    try {
      if (!is_generic_ref_init(x)) {
	ygglog_throw_error_c("generic_ref_get_scalar: Object is NULL.");
      }
      rapidjson::Value* x_obj = (rapidjson::Value*)(x.obj);
      if (!(x_obj->IsType("scalar") && x_obj->IsSubType(subtype, static_cast<rapidjson::SizeType>(precision)))) {
	ygglog_throw_error_c("generic_ref_get_scalar: Object is not a scalar of subtype \'%s\' with precision %ld", subtype, precision);
      }
    } catch(...) {
      ygglog_error_c("generic_ref_get_scalar: C++ exception thrown");
      return NULL;
    }
    return generic_ref_get_item(x, "scalar");
  }
  void* generic_get_scalar(generic_t x, const char *subtype, const size_t precision) {
    generic_ref_t x_ref = init_generic_ref(x);
    return generic_ref_get_scalar(x_ref, subtype, precision);
  }
  size_t generic_ref_get_1darray(generic_ref_t x, const char *subtype, const size_t precision, void** data) {
    size_t new_length = 0;
    try {
      if (!is_generic_ref_init(x)) {
	ygglog_throw_error_c("generic_ref_get_1darray: Object is NULL.");
      }
      rapidjson::Value* x_obj = (rapidjson::Value*)(x.obj);
      if (!(x_obj->IsType("1darray") && x_obj->IsSubType(subtype, static_cast<rapidjson::SizeType>(precision)))) {
	ygglog_throw_error_c("generic_ref_get_1darray: Object is not an array of subtype \'%s\' with precision %ld", subtype, precision);
      }
      void* new_data = generic_ref_get_item(x, "1darray");
      if (new_data == NULL)
	return 0;
      size_t nbytes = generic_ref_get_item_nbytes(x, "1darray");
      if (nbytes == 0)
	return 0;
      new_length = (size_t)(x_obj->GetNElements());
      data[0] = generic_ref_allocator(x).Realloc(data[0], 0, nbytes);
      if (data[0] == NULL) {
	ygglog_throw_error_c("generic_ref_get_1darray: Failed to reallocate array.");
      }
      memcpy(data[0], new_data, nbytes);
    } catch (...) {
      ygglog_error_c("generic_ref_get_1darray: C++ exception thrown");
      return 0;
    }
    return new_length;
  }
  size_t generic_get_1darray(generic_t x, const char *subtype, const size_t precision, void** data) {
    generic_ref_t x_ref = init_generic_ref(x);
    return generic_ref_get_1darray(x_ref, subtype, precision, data);
  }
  size_t generic_ref_get_ndarray(generic_ref_t x, const char *subtype, const size_t precision, void** data, size_t** shape) {
    size_t new_ndim = 0;
    try {
      if (!is_generic_ref_init(x)) {
	ygglog_throw_error_c("generic_ref_get_ndarray: Object is NULL.");
      }
      rapidjson::Value* x_obj = (rapidjson::Value*)(x.obj);
      if (!(x_obj->IsType("ndarray") && x_obj->IsSubType(subtype, static_cast<rapidjson::SizeType>(precision)))) {
	ygglog_throw_error_c("generic_ref_get_ndarray: Object is not an array of subtype \'%s\' with precision %ld", subtype, precision);
      }
      void* new_data = generic_ref_get_item(x, "ndarray");
      if (new_data == NULL)
	return 0;
      size_t nbytes = generic_ref_get_item_nbytes(x, "ndarray");
      if (nbytes == 0)
	return 0;
      data[0] = generic_ref_allocator(x).Realloc(data[0], 0, nbytes);
      if (data[0] == NULL) {
	ygglog_throw_error_c("generic_ref_get_ndarray: Failed to reallocate array.");
      }
      memcpy(data[0], new_data, nbytes);
      const rapidjson::Value& rjshape = x_obj->GetShape();
      new_ndim = (size_t)(rjshape.Size());
      size_t i = 0;
      shape[0] = (size_t*)(generic_ref_allocator(x).Realloc(shape[0], 0,
							    new_ndim * sizeof(size_t)));
      if (shape[0] == NULL) {
	ygglog_throw_error_c("generic_ref_get_ndarray: Failed to reallocate shape.");
      }
      for (rapidjson::Value::ConstValueIterator it = rjshape.Begin();
	   it != rjshape.End(); it++, i++) {
	shape[0][i] = (size_t)(it->GetInt());
      }
    } catch (...) {
      ygglog_error_c("generic_ref_get_ndarray: C++ exception thrown");
      return 0;
    }
    return new_ndim;
  }
  size_t generic_get_ndarray(generic_t x, const char *subtype, const size_t precision, void** data, size_t** shape) {
    generic_ref_t x_ref = init_generic_ref(x);
    return generic_ref_get_ndarray(x_ref, subtype, precision, data, shape);
  }
  int generic_set_scalar(generic_t x, void* value, const char *subtype,
			 const size_t precision, const char *units) {
    int out = GENERIC_ERROR_;
    try {
      if (!is_generic_init(x)) {
	ygglog_throw_error_c("generic_set_scalar: Object is NULL.");
      }
      rapidjson::Value* x_obj = (rapidjson::Value*)(x.obj);
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
      x_obj->SetYggdrasilString((char*)value, precision,
				generic_allocator(x),
				schema);
      out = GENERIC_SUCCESS_;
    } catch(...) {
      ygglog_error_c("generic_set_scalar: C++ exception thrown");
      return GENERIC_ERROR_;
    }
    return out;
  }
  int generic_set_1darray(generic_t x, void* value, const char *subtype,
			  const size_t precision, const size_t length,
			  const char* units) {
    int out = GENERIC_ERROR_;
    try {
      if (!is_generic_init(x)) {
	ygglog_throw_error_c("generic_set_1darray: Object is NULL.");
      }
      rapidjson::Value* x_obj = (rapidjson::Value*)(x.obj);
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
      x_obj->SetYggdrasilString((char*)value, precision * length,
				generic_allocator(x),
				schema);
      out = GENERIC_SUCCESS_;
    } catch(...) {
      ygglog_error_c("generic_set_1darray: C++ exception thrown");
      return GENERIC_ERROR_;
    }
    return out;
  }
  int generic_set_ndarray(generic_t x, void* data, const char *subtype,
			  const size_t precision, const size_t ndim,
			  const size_t* shape, const char* units) {
    int out = GENERIC_ERROR_;
    try {
      if (!is_generic_init(x)) {
	ygglog_throw_error_c("generic_set_ndarray: Object is NULL.");
      }
      rapidjson::Value* x_obj = (rapidjson::Value*)(x.obj);
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
      size_t length = 0;
      if (ndim > 0)
	length = 1;
      for (size_t i = 0; i < ndim; i++) {
	rjshape.PushBack(rapidjson::Value((unsigned)(shape[i])).Move(),
			 schema.GetAllocator());
	length *= shape[i];
      }
      schema.AddMember(rapidjson::Document::GetShapeString(), rjshape,
		       schema.GetAllocator());
      x_obj->SetYggdrasilString((char*)data, precision * length,
				generic_allocator(x),
				schema);
      out = GENERIC_SUCCESS_;
    } catch(...) {
      ygglog_error_c("generic_set_ndarray: C++ exception thrown");
      return GENERIC_ERROR_;
    }
    return out;
  }
#define NESTED_BASICS_(base, idx, idxType)				\
  void* generic_ ## base ## _get_item(generic_t x, idxType idx, const char *type) { \
    try {								\
      generic_ref_t tmp = init_generic_ref(x);				\
      if (get_generic_ ## base ## _ref(x, idx, &tmp) != GENERIC_SUCCESS_) { \
	return NULL;							\
      }									\
      return generic_ref_get_item(tmp, type);				\
    } catch(...) {							\
      ygglog_error_c("generic_" #base "_get: C++ exception thrown");	\
      return NULL;							\
    }									\
  }									\
  int generic_ ## base ## _get_item_nbytes(generic_t x, idxType idx, const char *type) { \
    try {								\
      generic_ref_t tmp = init_generic_ref(x);				\
      if (get_generic_ ## base ## _ref(x, idx, &tmp) != GENERIC_SUCCESS_) { \
	return 0;							\
      }									\
      return generic_ref_get_item_nbytes(tmp, type);			\
    } catch(...) {							\
      ygglog_error_c("generic_" #base "_get_nbytes: C++ exception thrown"); \
      return 0;								\
    }									\
  }									\
  void* generic_ ## base ## _get_scalar(generic_t x, idxType idx, const char *subtype, const size_t precision) { \
    try {								\
      generic_ref_t tmp = init_generic_ref(x);				\
      if (get_generic_ ## base ## _ref(x, idx, &tmp) != GENERIC_SUCCESS_) { \
	return NULL;							\
      }									\
      return generic_ref_get_scalar(tmp, subtype, precision);		\
    } catch(...) {							\
      ygglog_error_c("generic_" #base "_get_scalar: C++ exception thrown"); \
      return NULL;							\
    }									\
  }									\
  size_t generic_ ## base ## _get_1darray(generic_t x, idxType idx, const char *subtype, const size_t precision, void** data) { \
    try {								\
      generic_ref_t tmp = init_generic_ref(x);				\
      if (get_generic_ ## base ## _ref(x, idx, &tmp) != GENERIC_SUCCESS_) { \
	return 0;							\
      }									\
      return generic_ref_get_1darray(tmp, subtype, precision, data);	\
    } catch(...) {							\
      ygglog_error_c("generic_" #base "_get_1darray: C++ exception thrown"); \
      return 0;								\
    }									\
  }									\
  size_t generic_ ## base ## _get_ndarray(generic_t x, idxType idx, const char *subtype, const size_t precision, void** data, size_t** shape) { \
    try {								\
      generic_ref_t tmp = init_generic_ref(x);				\
      if (get_generic_ ## base ## _ref(x, idx, &tmp) != GENERIC_SUCCESS_) { \
	return 0;							\
      }									\
      return generic_ref_get_ndarray(tmp, subtype, precision, data, shape); \
    } catch(...) {							\
      ygglog_error_c("generic_" #base "_get_ndarary: C++ exception thrown"); \
      return 0;								\
    }									\
  }									\
  int generic_ ## base ## _set_item(generic_t x, idxType idx, const char *type, void* value) { \
    try {								\
      generic_t tmp = init_generic();					\
      if (generic_set_item(tmp, type, value) != GENERIC_SUCCESS_) {	\
        return GENERIC_ERROR_;						\
      }									\
      if (set_generic_ ## base(x, idx, tmp) != GENERIC_SUCCESS_) {	\
	return GENERIC_ERROR_;						\
      }									\
      destroy_generic(&tmp);						\
    } catch(...) {							\
      ygglog_error_c("generic_" #base "_set_item: C++ exception thrown");	\
      return GENERIC_ERROR_;						\
    }									\
    return GENERIC_SUCCESS_;						\
  }									\
  int generic_ ## base ## _set_scalar(generic_t x, idxType idx,		\
				      void* value,			\
				      const char *subtype,		\
				      const size_t precision,		\
				      const char *units) {		\
    try {								\
      generic_t tmp = init_generic_null();				\
      if (generic_set_scalar(tmp, value, subtype, precision, units) != GENERIC_SUCCESS_) { \
        return GENERIC_ERROR_;						\
      }									\
      if (set_generic_ ## base(x, idx, tmp) != GENERIC_SUCCESS_) {	\
	return GENERIC_ERROR_;						\
      }									\
      destroy_generic(&tmp);						\
    } catch(...) {							\
      ygglog_error_c("generic_" #base "_set_scalar: C++ exception thrown"); \
      return GENERIC_ERROR_;						\
    }									\
    return GENERIC_SUCCESS_;						\
  }									\
  int generic_ ## base ## _set_1darray(generic_t x, idxType idx,	\
				       void* value,			\
				       const char *subtype,		\
				       const size_t precision,		\
				       const size_t length,		\
				       const char *units) {		\
    try {								\
      generic_t tmp = init_generic_null();				\
      if (generic_set_1darray(tmp, value, subtype, precision, length, units) != GENERIC_SUCCESS_) { \
        return GENERIC_ERROR_;						\
      }									\
      if (set_generic_ ## base(x, idx, tmp) != GENERIC_SUCCESS_) {	\
	return GENERIC_ERROR_;						\
      }									\
      destroy_generic(&tmp);						\
    } catch(...) {							\
      ygglog_error_c("generic_" #base "_set_1darray: C++ exception thrown"); \
      return GENERIC_ERROR_;						\
    }									\
    return GENERIC_SUCCESS_;						\
  }									\
  int generic_ ## base ## _set_ndarray(generic_t x, idxType idx,	\
				       void* value,			\
				       const char *subtype,		\
				       const size_t precision,		\
				       const size_t ndim,		\
				       const size_t* shape,		\
				       const char *units) {		\
    try {								\
      generic_t tmp = init_generic_null();				\
      if (generic_set_ndarray(tmp, value, subtype, precision, ndim, shape, units) != GENERIC_SUCCESS_) { \
        return GENERIC_ERROR_;						\
      }									\
      if (set_generic_ ## base(x, idx, tmp) != GENERIC_SUCCESS_) {	\
	return GENERIC_ERROR_;						\
      }									\
      destroy_generic(&tmp);						\
    } catch(...) {							\
      ygglog_error_c("generic_" #base "_set_ndarray: C++ exception thrown"); \
      return GENERIC_ERROR_;						\
    }									\
    return GENERIC_SUCCESS_;						\
  }

  NESTED_BASICS_(array, index, const size_t)
  NESTED_BASICS_(map, key, const char*)
  
#undef NESTED_BASICS_
  
  int add_generic_array(generic_t arr, generic_t x) {
    int out = GENERIC_SUCCESS_;
    try {
      if (!is_generic_init(arr)) {
	ygglog_throw_error_c("add_generic_array: Array is NULL.");
      }
      if (!is_generic_init(x)) {
	ygglog_throw_error_c("add_generic_array: New element is NULL.");
      }
      rapidjson::Value* arr_obj = (rapidjson::Value*)(arr.obj);
      rapidjson::Value* x_obj = (rapidjson::Value*)(x.obj);
      if (!arr_obj->IsArray()) {
	ygglog_throw_error_c("add_generic_array: Document is not an array.");
      }
      rapidjson::Value cpy(*x_obj, generic_allocator(arr), true);
      arr_obj->PushBack(cpy, generic_allocator(arr));
    } catch (...) {
      ygglog_error_c("add_generic_array: C++ exception thrown.");
      out = GENERIC_ERROR_;
    }
    return out;
  }

  int set_generic_array(generic_t arr, const size_t i, generic_t x) {
    int out = GENERIC_SUCCESS_;
    try {
      if (!is_generic_init(arr)) {
	ygglog_throw_error_c("set_generic_array: Array is NULL.");
      }
      if (!is_generic_init(x)) {
	ygglog_throw_error_c("set_generic_array: New element is NULL.");
      }
      rapidjson::Value* arr_obj = (rapidjson::Value*)(arr.obj);
      rapidjson::Value* x_obj = (rapidjson::Value*)(x.obj);
      if (!arr_obj->IsArray()) {
	ygglog_throw_error_c("set_generic_array: Document is not an array.");
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
      ygglog_error_c("set_generic_array: C++ exception thrown.");
      out = GENERIC_ERROR_;
    }
    return out;
  }

  int get_generic_array_ref(generic_t arr, const size_t i, generic_ref_t *x) {
    int out = GENERIC_SUCCESS_;
    x[0] = init_generic_ref(arr);
    try {
      if (!is_generic_init(arr)) {
	ygglog_throw_error_c("get_generic_array_ref: Array is NULL.");
      }
      rapidjson::Value* arr_obj = (rapidjson::Value*)(arr.obj);
      if (!arr_obj->IsArray()) {
	ygglog_throw_error_c("get_generic_array_ref: Document is not an array.");
      }
      if (arr_obj->Size() <= i) {
	ygglog_throw_error_c("get_generic_array_ref: Document only has %d elements", (int)(arr_obj->Size()));
      }
      x[0].obj = (void*)(&((*arr_obj)[i]));
    } catch (...) {
      ygglog_error_c("get_generic_array_ref: C++ exception thrown.");
      out = GENERIC_ERROR_;
    }
    return out;
  }
  int get_generic_array(generic_t arr, const size_t i, generic_t *x) {
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

  int set_generic_object(generic_t arr, const char* k, generic_t x) {
    int out = GENERIC_SUCCESS_;
    try {
      if (!is_generic_init(arr)) {
	ygglog_throw_error_c("set_generic_object: Object is NULL.");
      }
      if (!is_generic_init(x)) {
	ygglog_throw_error_c("set_generic_object: New element is NULL.");
      }
      rapidjson::Value* arr_obj = (rapidjson::Value*)(arr.obj);
      rapidjson::Value* x_obj = (rapidjson::Value*)(x.obj);
      if (!arr_obj->IsObject()) {
	ygglog_throw_error_c("set_generic_object: Document is not an object.");
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
      ygglog_error_c("set_generic_object: C++ exception thrown.");
      out = GENERIC_ERROR_;
    }
    return out;
  }

  int get_generic_object_ref(generic_t arr, const char* k, generic_ref_t *x) {
    int out = 0;
    x[0] = init_generic_ref(arr);
    try {
      if (!is_generic_init(arr)) {
	ygglog_throw_error_c("get_generic_object_ref: Object is NULL.");
      }
      rapidjson::Value* arr_obj = (rapidjson::Value*)(arr.obj);
      if (!arr_obj->IsObject()) {
	ygglog_throw_error_c("get_generic_object_ref: Document is not an object.");
      }
      if (!arr_obj->HasMember(k)) {
	ygglog_throw_error_c("get_generic_object_ref: Document does not have the requested key.");
      }
      x[0].obj = (void*)(&((*arr_obj)[k]));
    } catch (...) {
      ygglog_error_c("get_generic_object_ref: C++ exception thrown.");
      out = GENERIC_ERROR_;
    }
    return out;
  }
  int get_generic_object(generic_t arr, const char* k, generic_t *x) {
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

#define NESTED_BASE_SET_(base, idx, idxType, name, args, ...)	\
  int generic_ ## base ## _set_ ## name(generic_t x, idxType idx, __VA_ARGS__) { \
    generic_t item = init_generic_null();				\
    if (generic_set_ ## name (item, UNPACK_MACRO args) != GENERIC_SUCCESS_) { \
      return GENERIC_ERROR_;						\
    }									\
    int out = set_generic_ ## base(x, idx, item);			\
    destroy_generic(&item);						\
    return out;								\
  }
#define NESTED_BASE_GET_(base, idx, idxType, name, type, defV, args, ...) \
  type generic_ ## base ## _get_ ## name(generic_t x, idxType idx, __VA_ARGS__) { \
    generic_ref_t item;							\
    type out = defV;							\
    if (get_generic_ ## base ## _ref(x, idx, &item) != GENERIC_SUCCESS_) { \
      return out;							\
    }									\
    out = generic_ref_get_ ## name(item, UNPACK_MACRO args);		\
    return out;								\
  }
#define NESTED_BASE_GET_NOARGS_(base, idx, idxType, name, type, defV)	\
  type generic_ ## base ## _get_ ## name(generic_t x, idxType idx) {	\
    generic_ref_t item;							\
    type out = defV;							\
    if (get_generic_ ## base ## _ref(x, idx, &item) != GENERIC_SUCCESS_) { \
      return out;							\
    }									\
    out = generic_ref_get_ ## name(item);				\
    return out;								\
  }
#define NESTED_SET_(name, args, ...)					\
  NESTED_BASE_SET_(array, index, const size_t, name, args, __VA_ARGS__)	\
  NESTED_BASE_SET_(map, key, const char*, name, args, __VA_ARGS__)
#define NESTED_GET_(name, type, defV, args, ...)	\
  NESTED_BASE_GET_(array, index, const size_t, name, type, defV, args, __VA_ARGS__) \
  NESTED_BASE_GET_(map, key, const char*, name, type, defV, args, __VA_ARGS__)
#define NESTED_GET_NOARGS_(name, type, defV)	\
  NESTED_BASE_GET_NOARGS_(array, index, const size_t, name, type, defV)	\
  NESTED_BASE_GET_NOARGS_(map, key, const char*, name, type, defV)
  
#define STD_JSON_NESTED_(name)						\
  generic_t generic_array_get_ ## name(generic_t x, const size_t index) { \
    generic_t item;							\
    get_generic_array(x, index, &item);					\
    return item;							\
  }									\
  generic_t generic_map_get_ ## name(generic_t x, const char* key) {	\
    generic_t item;							\
    get_generic_object(x, key, &item);					\
    return item;							\
  }									\
  int generic_array_set_ ## name(generic_t x, const size_t index, generic_t item) { \
    return set_generic_array(x, index, item);				\
  }									\
  int generic_map_set_ ## name(generic_t x, const char* key, generic_t item) { \
    return set_generic_map(x, key, item);				\
  }

  
#define STD_JSON_BASE_(name, type, isMethod, outMethod, setMethod, defV) \
  type generic_ref_get_ ## name(generic_ref_t x) {			\
    type out = defV;							\
    if (!is_generic_ref_init(x)) {					\
      ygglog_error_c("Generic object is NULL");				\
      return out;							\
    }									\
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);			\
    if (!isMethod) {							\
      std::cout << *d << std::endl;					\
      ygglog_error_c("Generic object is not " #name);			\
      return out;							\
    }									\
    outMethod;								\
    return out;								\
  }									\
  type generic_get_ ## name(generic_t x) {				\
    generic_ref_t x_ref = init_generic_ref(x);				\
    return generic_ref_get_ ## name(x_ref);				\
  }									\
  int generic_set_ ## name(generic_t x, type value) {			\
    if (!is_generic_init(x)) {						\
      ygglog_error_c("Generic object is not initialized");		\
      return GENERIC_ERROR_;						\
    }									\
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);			\
    setMethod;								\
    return GENERIC_SUCCESS_;						\
  }									\
  NESTED_GET_NOARGS_(name, type, defV)					\
  NESTED_SET_(name, (value), type value)
#define STD_UNITS_BASE_(name, type, isMethod, outMethod, setMethod, defV) \
  type generic_ref_get_ ## name(generic_ref_t x) {			\
    type out = defV;							\
    if (!is_generic_ref_init(x)) {					\
      ygglog_error_c("Generic object is NULL");				\
      return out;							\
    }									\
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);			\
    if (!isMethod) {							\
      ygglog_error_c("Generic object is not " #name);			\
      return out;							\
    }									\
    outMethod;								\
    return out;								\
  }									\
  type generic_get_ ## name(generic_t x) {				\
    generic_ref_t x_ref = init_generic_ref(x);				\
    return generic_ref_get_ ## name(x_ref);				\
  }									\
  int generic_set_ ## name(generic_t x, type value, const char* units) { \
    if (!is_generic_init(x)) {						\
      ygglog_error_c("Generic object is not initialized");		\
      return GENERIC_ERROR_;						\
    }									\
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);			\
    setMethod;								\
    return GENERIC_SUCCESS_;						\
  }									\
  NESTED_GET_NOARGS_(name, type, defV)					\
  NESTED_SET_(name, (value, units), type value, const char* units)
#define STD_JSON_(name, type, method, defV)				\
  STD_JSON_BASE_(name, type, d->Is ## method(), out = d->Get ## method(), d->Set ## method(value), defV)
#define STD_UNITS_(name, type, method, defV)				\
  STD_UNITS_BASE_(name, type, d->Is ## method(), out = d->Get ## method(), d->Set ## method(value), defV)
#define GEOMETRY_(name, rjtype)						\
  STD_JSON_BASE_(name, name ## _t, d->Is ## rjtype(), rapidjson::rjtype* tmp = new rapidjson::rjtype(); d->Get ## rjtype(*tmp); out = rjtype ## 2 ## name(*tmp); delete tmp, d->Set ## rjtype(name ## 2 ## rjtype(value), generic_allocator(x)), init_ ## name())
#define ARRAY_(name, type, rjtype)					\
  size_t generic_ref_get_1darray_ ## name(generic_ref_t x, type** data) {	\
    if ((!is_generic_ref_init(x)) || data == NULL) {			\
      ygglog_error_c("Generic object is NULL");				\
      return 0;								\
    }									\
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);			\
    if (!d->Is1DArray<rjtype>()) {					\
      ygglog_error_c("Generic object is not " #name);			\
      return 0;								\
    }									\
    rapidjson::SizeType nelements = 0;					\
    data[0] = (type*)(d->Get1DArray<rjtype>(nelements, generic_ref_allocator(x))); \
    return (size_t)nelements;						\
  }									\
  size_t generic_get_1darray_ ## name(generic_t x, type** data) {	\
    generic_ref_t x_ref = init_generic_ref(x);				\
    return generic_ref_get_1darray_ ## name(x_ref, data);		\
  }									\
  size_t generic_ref_get_ndarray_ ## name(generic_ref_t x, type** data, size_t** shape) { \
    if ((!is_generic_ref_init(x)) || data == NULL) {			\
      ygglog_error_c("Generic object is NULL");				\
      return 0;								\
    }									\
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);			\
    if (!d->IsNDArray<rjtype>()) {					\
      ygglog_error_c("Generic object is not " #name);			\
      return 0;								\
    }									\
    rapidjson::SizeType ndim = 0;					\
    rapidjson::SizeType* rjshape = NULL;				\
    data[0] = (type*)(d->GetNDArray<rjtype>(rjshape, ndim, generic_ref_allocator(x))); \
    shape[0] = (size_t*)(generic_ref_allocator(x).Malloc(ndim * sizeof(size_t))); \
    for (rapidjson::SizeType i = 0; i < ndim; i++) {			\
      (*shape)[i] = rjshape[i];						\
    }									\
    generic_ref_allocator(x).Free(rjshape);				\
    return (size_t)ndim;						\
  }									\
  size_t generic_get_ndarray_ ## name(generic_t x, type** data, size_t** shape) { \
    generic_ref_t x_ref = init_generic_ref(x);				\
    return generic_ref_get_ndarray_ ## name(x_ref, data, shape);	\
  }									\
  int generic_set_1darray_ ## name(generic_t x, type* value, const size_t length, const char* units) { \
    if (!is_generic_init(x)) {						\
      ygglog_error_c("Generic object is not initialized");		\
      return GENERIC_ERROR_;						\
    }									\
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);			\
    d->Set1DArray((rjtype*)value, (rapidjson::SizeType)length, units,	\
		  generic_allocator(x));				\
    return GENERIC_SUCCESS_;						\
  }									\
  int generic_set_ndarray_ ## name(generic_t x, type* value, const size_t ndim, const size_t* shape, const char* units) { \
    if (!is_generic_init(x)) {						\
      ygglog_error_c("Generic object is not initialized");		\
      return GENERIC_ERROR_;						\
    }									\
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);		\
    rapidjson::SizeType* rjshape = (rapidjson::SizeType*)(generic_allocator(x).Malloc(ndim * sizeof(rapidjson::SizeType))); \
    for (size_t i = 0; i < ndim; i++) {					\
      rjshape[i] = (rapidjson::SizeType)(shape[i]);			\
    }									\
    d->SetNDArray((rjtype*)value, rjshape, (rapidjson::SizeType)ndim, units, generic_allocator(x)); \
    generic_allocator(x).Free(rjshape);					\
    return GENERIC_SUCCESS_;						\
  }									\
  NESTED_GET_(1darray_ ## name, size_t, 0, (data), type** data)		\
  NESTED_GET_(ndarray_ ## name, size_t, 0, (data, shape), type** data, size_t** shape) \
  NESTED_SET_(1darray_ ## name, (value, length, units), type* value, const size_t length, const char* units) \
  NESTED_SET_(ndarray_ ## name, (data, ndim, shape, units), type* data, const size_t ndim, const size_t* shape, const char* units)
#define SCALAR_(name, type, defV)		\
  STD_UNITS_BASE_(name, type, d->IsScalar<type>(), out = (type)(d->GetScalar<type>()), d->SetScalar(value, units, generic_allocator(x)), defV) \
  ARRAY_(name, type, type)
#define COMPLEX_(name, type, subtype, defV)				\
  STD_UNITS_BASE_(name, type, d->IsScalar<std::complex<subtype>>(), std::complex<subtype> tmp = d->GetScalar<std::complex<subtype>>(); out.re = tmp.real(); out.im = tmp.imag(), d->SetScalar(std::complex<subtype>(value.re, value.im), units, generic_allocator(x)), type({defV, defV})) \
  ARRAY_(name, type, std::complex<subtype>)
#define __COMPLEX_(name, type, subtype, defV)				\
  type generic_ref_get_ ## name(generic_ref_t x) {			\
    type out;								\
    out.re = defV;							\
    out.im = defV;							\
    if (!is_generic_ref_init(x)) {					\
      ygglog_error_c("Generic object is NULL");				\
      return out;							\
    }									\
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);		\
    if (!d->IsScalar<std::complex<subtype>>()) {			\
      ygglog_error_c("Generic object is not " #name);			\
      return out;							\
    }									\
    std::complex<subtype> tmp = d->GetScalar<std::complex<subtype>>();	\
    out.re = tmp.real();						\
    out.im = tmp.imag();						\
    return out;								\
  }									\
  type generic_get_ ## name(generic_t x) {				\
    generic_ref_t x_ref = init_generic_ref(x);				\
    return generic_ref_get_ ## name(x_ref);				\
  }									\
  int generic_set_ ## name(generic_t x, type value, const char* units) { \
    if (!is_generic_init(x)) {						\
      ygglog_error_c("Generic object is not initialized");		\
      return GENERIC_ERROR_;						\
    }									\
    rapidjson::Value* d = (rapidjson::Value*)(x.obj);		\
    std::complex<subtype> tmp(value.re, value.im);			\
    d->SetScalar(tmp, units, generic_allocator(x));			\
    return GENERIC_SUCCESS_;						\
  }									\
  NESTED_GET_NOARGS_(name, type, {defV, defV})				\
  NESTED_SET_(name, (value, units), type value, const char* units)	\
  ARRAY_(name, type, std::complex<subtype>)
#define PYTHON_(name, method)						\
  STD_JSON_BASE_(name, python_t, d->Is ## method(), out.obj = d->GetPythonObjectRaw(), d->SetPythonObjectRaw(value.obj, generic_allocator(x)), {NULL})
  
  STD_JSON_(bool, bool, Bool, false);
  STD_JSON_(integer, int, Int, 0);
  STD_JSON_BASE_(null, void*, d->IsNull(), out = NULL, d->SetNull(); UNUSED(value), NULL);
  STD_JSON_(number, double, Double, 0.0);
  STD_JSON_BASE_(string, const char*, d->IsString(), out = d->GetString(), d->SetString(value, STRLEN_RJ(value), generic_allocator(x)), 0);
  STD_JSON_NESTED_(object);
  STD_JSON_NESTED_(array);
  STD_JSON_NESTED_(any);
  STD_JSON_NESTED_(schema);
  SCALAR_(int8, int8_t, 0);
  SCALAR_(int16, int16_t, 0);
  SCALAR_(int32, int32_t, 0);
  SCALAR_(int64, int64_t, 0);
  SCALAR_(uint8, uint8_t, 0);
  SCALAR_(uint16, uint16_t, 0);
  SCALAR_(uint32, uint32_t, 0);
  SCALAR_(uint64, uint64_t, 0);
  SCALAR_(float, float, 0.0);
  SCALAR_(double, double, 0.0);
  COMPLEX_(complex_float, complex_float_t, float, 0.0);
  COMPLEX_(complex_double, complex_double_t, double, 0.0);
#ifdef YGGDRASIL_LONG_DOUBLE_AVAILABLE
  SCALAR_(long_double, long double, 0.0);
  COMPLEX_(complex_long_double, complex_long_double_t, long double, 0.0);
#endif // YGGDRASIL_LONG_DOUBLE_AVAILABLE
  // TODO: Check encoding?
  // SCALAR_(bytes, const char*, 0);
  // SCALAR_(unicode, const char*, 0);
  PYTHON_(python_class, PythonClass);
  PYTHON_(python_function, PythonFunction);
  PYTHON_(python_instance, PythonInstance);
  GEOMETRY_(obj, ObjWavefront);
  GEOMETRY_(ply, Ply);

#undef GEOMETRY_
#undef COMPLEX_
#undef PYTHON_
#undef SCALAR_
#undef ARRAY_
#undef STD_JSON_
#undef STD_UNITS_
#undef STD_JSON_BASE_
#undef STD_UNITS_BASE_
#undef STD_JSON_NESTED_
#undef NESTED_SET_
#undef NESTED_GET_
#undef NESTED_GET_NOARGS_
#undef NESTED_BASE_SET_
#undef NESTED_BASE_GET_
#undef NESTED_BASE_GET_NOARGS_
#undef GENERIC_ERROR_
#undef GENERIC_SUCCESS_

	    

  // Generic array methods
  size_t generic_array_get_size(generic_t x) {
    size_t out = 0;
    try {
      if (!is_generic_init(x)) {
	ygglog_throw_error_c("generic_array_get_size: Object is NULL.");
      }
      rapidjson::Value* x_obj = (rapidjson::Value*)(x.obj);
      if (!x_obj->IsArray()) {
	ygglog_throw_error_c("generic_array_get_size: Document is not an array.");
      }
      out = (size_t)(x_obj->Size());
    } catch (...) {
      ygglog_error_c("generic_array_get_size: C++ exception thrown.");
    }
    return out;
  }

  // Generic map methods
  size_t generic_map_get_size(generic_t x) {
    size_t out = 0;
    try {
      if (!is_generic_init(x)) {
	ygglog_throw_error_c("generic_map_get_size: Object is NULL.");
      }
      rapidjson::Value* x_obj = (rapidjson::Value*)(x.obj);
      if (!x_obj->IsObject()) {
	ygglog_throw_error_c("generic_map_get_size: Document is not an object.");
      }
      out = (size_t)(x_obj->MemberCount());
    } catch (...) {
      ygglog_error_c("generic_map_get_size: C++ exception thrown.");
    }
    return out;
  }
  int generic_map_has_key(generic_t x, const char* key) {
    int out = 0;
    try {
      if (!is_generic_init(x)) {
	ygglog_throw_error_c("generic_map_has_key: Object is NULL.");
      }
      rapidjson::Value* x_obj = (rapidjson::Value*)(x.obj);
      if (!x_obj->IsObject()) {
	ygglog_throw_error_c("generic_map_has_key: Document is not an object.");
      }
      if (x_obj->HasMember(key)) {
	out = 1;
      }
    } catch (...) {
      ygglog_error_c("generic_map_has_key: C++ exception thrown.");
    }
    return out;
  }
  size_t generic_map_get_keys(generic_t x, char*** keys) {
    size_t out = 0;
    try {
      if (!is_generic_init(x)) {
	ygglog_throw_error_c("generic_map_get_keys: Object is NULL.");
      }
      rapidjson::Value* x_obj = (rapidjson::Value*)(x.obj);
      if (!x_obj->IsObject()) {
	ygglog_throw_error_c("generic_map_get_keys: Document is not an object.");
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
      ygglog_error_c("generic_map_get_keys: C++ exception thrown.");
      out = 0;
    }
    return out;
  }

  void destroy_python(python_t *x) {
    if (x != NULL) {
      if (x->obj != NULL) {
#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
	Py_DECREF(x->obj);
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
	x->obj = NULL;
      }
    }
  }

  python_t copy_python(python_t x) {
    python_t out;
    if (x.obj != NULL) {
#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
      Py_INCREF(x.obj);
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
      PyObject_Print(x.obj, stdout, 0);
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
    try {
      _GET_METADATA(metadata, type_struct, -1);
      return static_cast<int>(metadata->isFormatArray());
    } catch(...) {
      ygglog_error_c("is_dtype_format_array: C++ exception thrown.");
      return -1;
    }
    return 1;
  }

  const char* dtype_name(const dtype_t type_struct) {
    _GET_METADATA(metadata, type_struct, "");
    return metadata->typeName();
  }

  const char* dtype_subtype(const dtype_t type_struct) {
    _BEGIN_CPP {
      _GET_METADATA(metadata, type_struct, "");
      return metadata->subtypeName();
    } _END_CPP(dtype_subtype, "");
  }

  size_t dtype_precision(const dtype_t type_struct) {
    _BEGIN_CPP {
      _GET_METADATA(metadata, type_struct, 0);
      return metadata->GetSchemaUint("precision");
    } _END_CPP(dtype_precision, 0);
  };

  int set_dtype_name(dtype_t dtype, const char* name) {
    _BEGIN_CPP {
      _GET_METADATA(metadata, dtype, -1);
      metadata->SetSchemaString("type", name);
      return 0;
    } _END_CPP(set_dtype_name, -1);
  }

  dtype_t create_dtype(void* metadata, const bool use_generic) {
    dtype_t out;
    Metadata* meta = static_cast<Metadata*>(metadata);
    if (!meta)
      meta = new Metadata();
    out.metadata = (void*)meta;
    if (use_generic)
      meta->setGeneric();
    return out;
  }

  dtype_t create_dtype_empty(const bool use_generic) {
    return create_dtype(NULL, use_generic);
  }

  dtype_t complete_dtype(dtype_t dtype, const bool use_generic) {
    _BEGIN_CPP {
      if (!dtype.metadata)
	dtype.metadata = (void*)(new Metadata());
      if (use_generic) {
	_GET_METADATA(metadata, dtype, dtype);
	metadata->setGeneric();
      }
      return dtype;
    } _END_CPP(complete_dtype, dtype);
  }

  int destroy_dtype(dtype_t *dtype) {
    _BEGIN_CPP {
      _GET_METADATA(metadata, (*dtype), 0);
      dtype->metadata = NULL;
      delete metadata;
      return 0;
    } _END_CPP(destroy_dtype, -1);
  }

  dtype_t create_dtype_from_schema(const char* schema,
				   const bool use_generic) {
    dtype_t out = create_dtype(NULL, false);
    _BEGIN_CPP {
      _GET_METADATA(metadata, out, out);
      metadata->fromSchema(schema, use_generic);
    } _END_CPP_CLEANUP(create_dtype_from_schema, out,
		       destroy_dtype(&out));
    return out;
  }

  // dtype_t create_dtype_python(PyObject* pyobj, const bool use_generic) {
  //   rapidjson::Document* obj = NULL;
  //   try {
  //     // TODO
  //     obj = type_from_pyobj(pyobj);
  //     return create_dtype(obj, use_generic);
  //   } catch(...) {
  //     ygglog_error_c("create_dtype_python: C++ exception thrown.");
  //     return NULL;
  //   }
  // }

  dtype_t create_dtype_direct(const bool use_generic) {
    return create_dtype_default("string", use_generic);
  }

  dtype_t create_dtype_default(const char* type, const bool use_generic) {
    dtype_t out = create_dtype(NULL, false);
    _BEGIN_CPP {
      _GET_METADATA(metadata, out, out);
      metadata->fromType(type, use_generic);
      return out;
    } _END_CPP_CLEANUP(create_dtype_default, out, destroy_dtype(&out));
  }

  dtype_t create_dtype_scalar(const char* subtype, const size_t precision,
			      const char* units, const bool use_generic) {
    dtype_t out = create_dtype(NULL, false);
    _BEGIN_CPP {
      _GET_METADATA(metadata, out, out);
      metadata->fromScalar(subtype, precision, units, use_generic);
      return out;
    } _END_CPP_CLEANUP(create_dtype_scalar, out, destroy_dtype(&out));
  }

  dtype_t create_dtype_format(const char *format_str,
			      const int as_array = 0,
			      const bool use_generic = false) {
    dtype_t out = create_dtype(NULL, false);
    _BEGIN_CPP {
      _GET_METADATA(metadata, out, out);
      metadata->fromFormat(format_str, as_array, use_generic);
      return out;
    } _END_CPP_CLEANUP(create_dtype_format, out, destroy_dtype(&out));
  }

  dtype_t create_dtype_1darray(const char* subtype,
			       const size_t precision,
			       const size_t length, const char* units,
			       const bool use_generic) {
    dtype_t out = create_dtype(NULL, false);
    _BEGIN_CPP {
      _GET_METADATA(metadata, out, out);
      metadata->fromNDArray(subtype, precision, 1, &length,
			    units, use_generic);
      return out;
    } _END_CPP_CLEANUP(create_dtype_1darray, out, destroy_dtype(&out));
  }

  dtype_t create_dtype_ndarray(const char* subtype,
			       const size_t precision,
			       const size_t ndim, const size_t* shape,
			       const char* units,
			       const bool use_generic) {
    dtype_t out = create_dtype(NULL, false);
    _BEGIN_CPP {
      _GET_METADATA(metadata, out, out);
      metadata->fromNDArray(subtype, precision, ndim, shape,
			    units, use_generic);
      return out;
    } _END_CPP_CLEANUP(create_dtype_ndarray, out, destroy_dtype(&out));
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
	ygglog_throw_error_c("create_dtype_json_array: %d items expected, but the items parameter is NULL.", nitems);
      }
      _GET_METADATA(metadata, out, out);
      metadata->fromType("array", (use_generic || nitems == 0));
      if (nitems > 0) {
	metadata->SetSchemaValue(
	  "items", rapidjson::Value(rapidjson::kArrayType).Move());
	for (size_t i = 0; i < nitems; i++) {
	  if (items[i].metadata == NULL) {
	    ygglog_throw_error_c("create_dtype_json_array: Item metadata %d is NULL", i);
	  }
	  metadata->addItem(*((Metadata*)(items[i].metadata)));
	  destroy_dtype(&(items[i]));
	}
      }
      return out;
    } _END_CPP_CLEANUP(create_dtype_json_array, out,
		       destroy_dtype(&out));
  }
  dtype_t create_dtype_json_object(const size_t nitems, char** keys,
				   dtype_t* values,
				   const bool use_generic=true) {
    dtype_t out = create_dtype(NULL, false);
    _BEGIN_CPP {
      if ((nitems > 0) && ((keys == NULL) || (values == NULL))) {
	ygglog_throw_error_c("create_dtype_json_object: %d items expected, but the keys and/or values parameter is NULL.", nitems);
      }
      _GET_METADATA(metadata, out, out);
      metadata->fromType("object", use_generic);
      if (nitems > 0) {
	metadata->SetSchemaValue(
	  "properties", rapidjson::Value(rapidjson::kObjectType).Move());
	for (size_t i = 0; i < nitems; i++) {
	  if (values[i].metadata == NULL) {
	    ygglog_throw_error_c("create_dtype_json_array: Value metadata %d is NULL", i);
	  }
	  metadata->addMember(keys[i],
			      *((Metadata*)(values[i].metadata)));
	  destroy_dtype(&(values[i]));
	}
      }
      return out;
    } _END_CPP_CLEANUP(create_dtype_json_object, out,
		       destroy_dtype(&out));
  }
  dtype_t create_dtype_ply(const bool use_generic) {
    return create_dtype_default("ply", use_generic);
  }
  dtype_t create_dtype_obj(const bool use_generic) {
    return create_dtype_default("obj", use_generic);
  }
  dtype_t create_dtype_ascii_table(const char *format_str,
				   const int as_array,
				   const bool use_generic) {
    return create_dtype_format(format_str, as_array, use_generic);
  }
  dtype_t create_dtype_pyobj(const char* type, const bool use_generic) {
    return create_dtype_default(type, use_generic);
  }
  dtype_t create_dtype_pyinst(const char*, // class_name,
			      dtype_t args_dtype,
			      dtype_t kwargs_dtype,
			      const bool use_generic) {
    dtype_t out = create_dtype(NULL, false);
    _BEGIN_CPP {
      _GET_METADATA(metadata, out, out);
      metadata->fromType("instance", use_generic);
      if (!is_empty_dtype(args_dtype)) {
	metadata->SetSchemaMetadata("args",
				    *((Metadata*)(args_dtype.metadata)));
	destroy_dtype(&args_dtype);
      }
      if (!is_empty_dtype(kwargs_dtype)) {
	metadata->SetSchemaMetadata("kwargs",
				    *((Metadata*)(kwargs_dtype.metadata)));
	destroy_dtype(&kwargs_dtype);
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
      _GET_METADATA(metadata_src, dtype, out);
      _GET_METADATA(metadata_dst, out, out);
      metadata_dst->fromMetadata(*metadata_src);
      return out;
    } _END_CPP_CLEANUP(copy_dtype, out, destroy_dtype(&out));
  }

  int dtype_uses_generic(dtype_t dtype) {
    _BEGIN_CPP {
      _GET_METADATA(metadata, dtype, 0);
      return static_cast<int>(metadata->isGeneric());
    } _END_CPP(dtype_uses_generic, -1);
  }

  void display_dtype(const dtype_t dtype, const char* indent) {
    _BEGIN_CPP {
      _GET_METADATA(metadata, dtype,);
      metadata->Display(indent);
    } _END_CPP(display_dtype, );
  }

  ////////////////////////////////////////////
  // OBJWAVEFRONT OBJECT WRAPPER
  ////////////////////////////////////////////

  obj_t init_obj() {
    obj_t x;
    x.obj = NULL;
    return x;
  }

  void set_obj(obj_t* x, void* obj, int copy) {
    if (x == NULL)
      return;
    if (copy && obj != NULL) {
      rapidjson::ObjWavefront* objw = (rapidjson::ObjWavefront*)obj;
      rapidjson::ObjWavefront* cpy = new rapidjson::ObjWavefront(*objw);
      x->obj = cpy;
    } else {
      x->obj = obj;
    }
  }

  void free_obj(obj_t *p) {
    if (p != NULL) {
      if (p->obj != NULL) {
	rapidjson::ObjWavefront* obj = (rapidjson::ObjWavefront*)(p->obj);
	p->obj = NULL;
	delete obj;
      }
    }
  }

  obj_t copy_obj(obj_t src) {
    obj_t out = init_obj();
    set_obj(&out, src.obj, 1);
    return out;
  }

  void display_obj_indent(obj_t p, const char* indent) {
    if (p.obj == NULL) {
      printf("%sNULL\n", indent);
    } else {
      rapidjson::ObjWavefront* obj = (rapidjson::ObjWavefront*)(p.obj);
      std::string s = obj->as_string();
      std::string s_indent(indent);
      size_t orig_size = s.size(), j = 0;
      for (size_t i = 0; i < orig_size; i++) {
	if (s[j] == '\n') {
	  s.insert(j + 1, s_indent);
	  j += s_indent.size();
	}
	j++;
      }
      printf("%s%s\n", indent, s.c_str());
    }
  }
  void display_obj(obj_t p) {
    return display_obj_indent(p, "");
  }

  int nelements_obj(obj_t p, const char* name) {
    if (p.obj == NULL) {
      ygglog_error_c("nelements_obj: ObjWavefront object is NULL.");
      return -1;
    }
    rapidjson::ObjWavefront* obj = (rapidjson::ObjWavefront*)(p.obj);
    size_t N = obj->count_elements(std::string(name));
    return static_cast<int>(N);
  }

  ////////////////////////////////////////////
  // PLY OBJECT WRAPPER
  ////////////////////////////////////////////

  ply_t init_ply() {
    ply_t x;
    x.obj = NULL;
    return x;
  }

  void set_ply(ply_t* x, void* obj, int copy) {
    if (x == NULL)
      return;
    if (copy && obj != NULL) {
      rapidjson::Ply* objw = (rapidjson::Ply*)obj;
      rapidjson::Ply* cpy = new rapidjson::Ply(*objw);
      x->obj = cpy;
    } else {
      x->obj = obj;
    }
  }

  void free_ply(ply_t *p) {
    if (p != NULL) {
      if (p->obj != NULL) {
	rapidjson::Ply* obj = (rapidjson::Ply*)(p->obj);
	p->obj = NULL;
	delete obj;
      }
    }
  }

  ply_t copy_ply(ply_t src) {
    ply_t out = init_ply();
    set_ply(&out, src.obj, 1);
    return out;
  }

  void display_ply_indent(ply_t p, const char* indent) {
    if (p.obj == NULL) {
      printf("%sNULL\n", indent);
    } else {
      rapidjson::Ply* obj = (rapidjson::Ply*)(p.obj);
      std::string s = obj->as_string();
      std::string s_indent(indent);
      size_t orig_size = s.size(), j = 0;
      for (size_t i = 0; i < orig_size; i++) {
	if (s[j] == '\n') {
	  s.insert(j + 1, s_indent);
	  j += s_indent.size();
	}
	j++;
      }
      printf("%s%s\n", indent, s.c_str());
    }
  }

  void display_ply(ply_t p) {
    return display_ply_indent(p, "");
  }

  int nelements_ply(ply_t p, const char* name) {
    if (p.obj == NULL) {
      ygglog_error_c("nelements_ply: Ply object is NULL.");
      return -1;
    }
    rapidjson::Ply* ply = (rapidjson::Ply*)(p.obj);
    size_t N = ply->count_elements(std::string(name));
    return static_cast<int>(N);
  }
  
  int init_python_API() {
    try {
#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
      rapidjson::init_python_API();
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
    } catch(...) {
      ygglog_error_c("init_python_API: C++ exception thrown.");
      return 1;
    }
    return 0;
  }
  
}
