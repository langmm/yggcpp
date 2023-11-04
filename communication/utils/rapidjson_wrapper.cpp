#include "rapidjson_wrapper.hpp"

#ifdef WRAP_RAPIDJSON_FOR_DLL

#include "rapidjson/document.h"
#include "rapidjson/prettywriter.h"

using namespace rapidjson;

#define RJV_WRAP_DEC(name, argsT, args, type, mods)	\
  type WValue::name argsT mods {			\
    return val_->name args;				\
  }
#define RJD_WRAP_DEC(name, argsT, args, type, mods)	\
  type WDocument::name argsT mods {			\
    return doc_->name args;				\
  }
#define RJV_WRAP_DEC_RETV(name, argsT, args, mods)		\
  WValue WValue::name argsT mods {				\
    return WValue(&(val_->name args));				\
  }
#define RJV_WRAP_DEC_RETV_CONST(name, argsT, args, mods)	\
  const WValue WValue::name argsT const mods {			\
    return WValue(&(val_->name args));				\
  }
#define RJD_WRAP_DEC_RETD(name, argsT, args, mods)		\
  WDocument WDocument::name argsT mods {			\
    return WDocument(&(doc_->name args));			\
  }
#define RJD_WRAP_DEC_RETV(name, argsT, args, mods)		\
  WValue WDocument::name argsT mods {				\
    return WValue(&(doc_->name args));				\
  }
#define RJD_WRAP_DEC_RETD_CONST(name, argsT, args, mods)	\
  const WDocument WDocument::name argsT const mods {		\
    return WDocument(&(doc_->name args));			\
  }

WValue::WValue(Type type) :
  val_(new rapidjson::Value(type)), created_val(true) {}
WValue::WValue(const rapidjson::Value* val) :
  WValue(const_cast<rapidjson::Value*>(val)) {}
WValue::WValue(rapidjson::Value* val) :
  val_(val), created_val(false) {}
WValue::WValue(rapidjson::Value& val) :
  WValue(&val) {}
WValue::WValue(rapidjson::Document* val) :
  WValue(dynamic_cast<rapidjson::Value*>(val)) {}
WValue& WValue::CopyFrom(const rapidjson::Document& rhs,
			 WValue::Allocator& allocator,
			 bool copyConstStrings) {
  val_->CopyFrom(rhs, allocator, copyConstStrings);
  return *this;
}
WValue& WValue::CopyFrom(const rapidjson::Value& rhs,
			 WValue::Allocator& allocator,
			 bool copyConstStrings) {
  val_->CopyFrom(rhs, allocator, copyConstStrings);
  return *this;
}
bool WValue::operator==(const WValue& rhs) const {
  return ((*val_) == (*rhs.val_));
}

  RJV_WRAP_DEC_RETV(CopyFrom, (const WValue& rhs,
			       WValue::Allocator& allocator,
			       bool copyConstStrings),
		    (*(rhs.val_), allocator, copyConstStrings), );
  RJV_WRAP_DEC_RETV(Swap, (WValue rhs), (*(rhs.val_)), );
  RJV_WRAP_DEC_RETV(PushBack, (WValue& value,
			       WValue::Allocator& allocator),
		    (*(value.val_), allocator), );
  template <typename Handler>
  RJV_WRAP_DEC(Accept, (Handler& handler, bool skip_yggdrasil),
	       (handler, skip_yggdrasil), bool, const);
  RJV_WRAP_DEC(GetUint, (), (), unsigned, const);
  RJV_WRAP_DEC(GetNElements, (), (), SizeType, const);
  RJV_WRAP_DEC_RETV_CONST(GetShape, (), (), );
  // String methods
  RJV_WRAP_DEC(IsString, (), (), bool, const);
  RJV_WRAP_DEC_RETV(SetString, (const WValue::Ch* s, SizeType length,
				WValue::Allocator& allocator),
		    (s, length, allocator), );
  RJV_WRAP_DEC(GetString, (), (), const WValue::Ch*, const);
  RJV_WRAP_DEC(GetStringLength, (), (), SizeType, const);
  // Templated methods
  template <typename T>
  RJV_WRAP_DEC(Is, (), <T>(), bool, const);
  template <typename T>
  RJV_WRAP_DEC(IsScalar, (), <T>(), bool, const);
  template <typename T>
  RJV_WRAP_DEC(Is1DArray, (), <T>(), bool, const);
  template <typename T>
  RJV_WRAP_DEC(IsNDArray, (), <T>(), bool, const);
  template<typename T>
  RJV_WRAP_DEC_RETV(Set, (const T& data,
			  WValue::AllocatorType& allocator),
		    (data, allocator), );
  template<typename T>
  RJV_WRAP_DEC_RETV(Set1DArray, (const T* x, SizeType len,
				 WValue::Allocator& allocator),
		    (x, len, allocator), );
  template<typename T>
  RJV_WRAP_DEC_RETV(SetNDArray, (const T* x, SizeType shape[],
				 SizeType ndim,
				 WValue::Allocator& allocator),
		    (x, shape, ndim, allocator), );
  template<typename T>
  RJV_WRAP_DEC(Get, (T& data), (data), void, );
  template<typename T>
  RJV_WRAP_DEC(GetScalarValue, (T& data), (data), void, );
  // Array methods
  RJV_WRAP_DEC(IsArray, (), (), bool, const);
  RJV_WRAP_DEC_RETV(SetArray, (), (), );
  RJV_WRAP_DEC(Size, (), (), rapidjson::SizeType, const);
  RJV_WRAP_DEC(Empty, (), (), bool, const);
  RJV_WRAP_DEC_RETV(Reserve, (SizeType newCapacity,
			      WValue::Allocator &allocator),
		    (newCapacity, allocator), );
  RJV_WRAP_DEC(Erase, (WValue::ConstValueIterator pos), (pos),
	       WValue::ValueIterator, );
  RJV_WRAP_DEC(Begin, (), (), WValue::ValueIterator, );
  RJV_WRAP_DEC(End, (), (), WValue::ValueIterator, );
  RJV_WRAP_DEC(Begin, (), (), WValue::ConstValueIterator, const);
  RJV_WRAP_DEC(End, (), (), WValue::ConstValueIterator, const);
  // RJV_WRAP_DEC_RETV(operator[], (SizeType index), (index), );
  // RJV_WRAP_DEC_RETV_CONST(operator[], (SizeType index), (index), );
  RJV_WRAP_DEC(operator[], (SizeType index), (index),
	       rapidjson::Value&, );
  RJV_WRAP_DEC(operator[], (SizeType index), (index),
	       const rapidjson::Value&, const );
  // Object methods
  RJV_WRAP_DEC(IsObject, (), (), bool, const);
  RJV_WRAP_DEC_RETV(SetObject, (), (), );
  RJV_WRAP_DEC(MemberCount, (), (), rapidjson::SizeType, const);
  RJV_WRAP_DEC(HasMember, (const Ch* name), (name), bool, const);
  // RJV_WRAP_DEC_RETV(operator[], (const Ch* name), (name), );
  // RJV_WRAP_DEC_RETV_CONST(operator[], (const Ch* name), (name), );
  RJV_WRAP_DEC(operator[], (const Ch* name), (name),
	       rapidjson::Value&, );
  RJV_WRAP_DEC(operator[], (const Ch* name), (name),
	       const rapidjson::Value&, const);
  RJV_WRAP_DEC_RETV(AddMember, (rapidjson::Value& name,
				rapidjson::Value& value,
				WValue::Allocator& allocator),
		    (name, value, allocator), );
  RJV_WRAP_DEC(RemoveMember, (const Ch* name), (name), bool, );

std::ostream & operator << (std::ostream &out, const WValue& p) {
  return out << p.val_;
}

WDocument::WDocument(Type type) :
  WValue((rapidjson::Value*)nullptr),
  doc_(new rapidjson::Document(type)), created_doc(true)
{ val_ = doc_; }
WDocument::WDocument(rapidjson::Document* doc) :
  WValue(doc), created_doc(false) {}
WDocument::WDocument(rapidjson::Document& doc) :
  WDocument(&doc) {}

WDocument& WDocument::CopyFrom(const rapidjson::Document& rhs,
			       WDocument::Allocator& allocator,
			       bool copyConstStrings) {
  doc_->CopyFrom(rhs, allocator, copyConstStrings);
  return *this;
}

  RJD_WRAP_DEC_RETV(CopyFrom, (const WDocument& rhs,
			       WDocument::Allocator& allocator,
			       bool copyConstStrings),
		    (*(rhs.doc_), allocator, copyConstStrings), );
  RJD_WRAP_DEC_RETD(Swap, (WDocument rhs), (*(rhs.doc_)), );
  RJD_WRAP_DEC(GetAllocator, (), (), WDocument::Allocator&, );
  RJD_WRAP_DEC(Normalize, (const WValue& schema,
			   StringBuffer* error),
	       (*(schema.val_), error), bool, );
  RJD_WRAP_DEC_RETD(Parse, (const WValue::Ch* str, size_t length),
		    (str, length), );
  RJD_WRAP_DEC(HasParseError, (), (), bool, const);

#undef RJV_WRAP_DEC
#undef RJD_WRAP_DEC
#undef RJV_WRAP_DEC_RETV
#undef RJV_WRAP_DEC_RETV_CONST
#undef RJD_WRAP_DEC_RETV
#undef RJD_WRAP_DEC_RETD
#undef RJD_WRAP_DEC_RETD_CONST

#define SPECIALIZE_BASE(type, set_type)					\
  template bool WValue::Is<type>() const;				\
  template WValue WValue::Set<set_type>(const set_type& data, WValue::Allocator& allocator); \
  template void WValue::Get<type>(type& data)
#define SPECIALIZE(type)						\
  SPECIALIZE_BASE(type, type)
#define SPECIALIZE_SCALAR(type)						\
  SPECIALIZE(type);							\
  template bool WValue::IsScalar<type>() const;				\
  template bool WValue::Is1DArray<type>() const;			\
  template bool WValue::IsNDArray<type>() const;			\
  template WValue WValue::Set1DArray<type>(const type* x, SizeType len, WValue::Allocator& allocator); \
  template WValue WValue::SetNDArray<type>(const type* x, SizeType shape[], SizeType ndim, WValue::Allocator& allocator); \
  template void WValue::GetScalarValue<type>(type& data)

// default rapidjson
SPECIALIZE(bool);
SPECIALIZE_SCALAR(int);
SPECIALIZE_SCALAR(unsigned);
#ifdef _MSC_VER
SPECIALIZE_SCALAR(long);
SPECIALIZE_SCALAR(unsigned long);
#endif
SPECIALIZE_SCALAR(int64_t);
SPECIALIZE_SCALAR(uint64_t);
SPECIALIZE_SCALAR(float);
SPECIALIZE_SCALAR(double);
typedef const typename WValue::Ch* StringType;
SPECIALIZE_BASE(StringType, StringType);
#ifdef RAPIDJSON_HAS_STDSTRING
SPECIALIZE(std::basic_string<typename WValue::Ch>);
#endif
// ValueType::Array
// ValueType::ConstArray
// ValueType::Object
// ValueType::ConstObject

// yggdrasil rapidjson
SPECIALIZE_SCALAR(int8_t);
SPECIALIZE_SCALAR(int16_t);
SPECIALIZE_SCALAR(uint8_t);
SPECIALIZE_SCALAR(uint16_t);
SPECIALIZE_SCALAR(std::complex<float>);
SPECIALIZE_SCALAR(std::complex<double>);
#ifdef YGGDRASIL_LONG_DOUBLE_AVAILABLE
SPECIALIZE_SCALAR(long double);
SPECIALIZE_SCALAR(std::complex<long double>);
#endif
#ifdef YGGDRASIL_DISABLE_PYTHON_C_API
SPECIALIZE(PyObject*)
#endif
SPECIALIZE(rapidjson::Ply);
SPECIALIZE(rapidjson::ObjWavefront);
// vector
// map

#endif // WRAP_RAPIDJSON_FOR_DLL

/*
template<typename T>
void rapidjson::Value_Set(rapidjson::Value& v, const T& data,
			  RAPIDJSON_DEFAULT_ALLOCATOR& allocator) {
  v.Set(data, allocator);
}
void rapidjson::Value_SetString(rapidjson::Value& v,
				const char* s,
				rapidjson::SizeType length,
				RAPIDJSON_DEFAULT_ALLOCATOR& allocator) {
  v.SetString(s, length, allocator);
};
void rapidjson::Value_CopyFrom(rapidjson::Value& v,
			       const rapidjson::Document& data,
			       RAPIDJSON_DEFAULT_ALLOCATOR& allocator,
			       bool copyConstStrings) {
  v.CopyFrom(data, allocator, copyConstStrings);
}
template<typename T>
void rapidjson::Value_Set1DArray(rapidjson::Value& v,
				 const T* x, rapidjson::SizeType len,
				 RAPIDJSON_DEFAULT_ALLOCATOR& allocator) {
  v.Set1DArray(x, len, allocator);
}
template<typename T>
void rapidjson::Value_SetNDArray(rapidjson::Value& v,
				 const T* x, rapidjson::SizeType shape[],
				 rapidjson::SizeType ndim,
				 RAPIDJSON_DEFAULT_ALLOCATOR& allocator) {
  v.SetNDArray(x, shape, ndim, allocator);
}
rapidjson::Document* rapidjson::Document_new(Type type) {
  return new rapidjson::Document(type);
}
void rapidjson::Document_delete(rapidjson::Document*& d) {
  delete d;
  d = nullptr;
}
RAPIDJSON_DEFAULT_ALLOCATOR rapidjson::Document_GetAllocator(rapidjson::Document& d) {
  return d.GetAllocator();
}
rapidjson::SizeType rapidjson::Document_Size(const rapidjson::Document& d) {
  return d.Size();
}
void rapidjson::Document_PushBack(rapidjson::Document& d,
				  rapidjson::Value& v,
				  RAPIDJSON_DEFAULT_ALLOCATOR& allocator) {
  d.PushBack(v, allocator);
}
*/

