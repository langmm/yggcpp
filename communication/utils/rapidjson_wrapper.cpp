#include "rapidjson_wrapper.hpp"

#ifdef WRAP_RAPIDJSON_FOR_DLL

#include "rapidjson/document.h"
#include "rapidjson/prettywriter.h"
#include "rapidjson/schema.h"

using namespace rapidjson;

#define RJV_WRAP_DEC(name, argsT, args, type, mods)	\
  type WValue::name argsT mods {			\
    return val_->name args;				\
  }
#define RJV_WRAP_DEC_TEMP(name, argsT, args, type, mods)	\
  template <typename T>						\
  type WValue::name argsT mods {				\
    return val_->name<T> args;					\
  }
#define RJD_WRAP_DEC(name, argsT, args, type, mods)	\
  type WDocument::name argsT mods {			\
    return doc_->name args;				\
  }
#define RJV_WRAP_DEC_RETV(name, argsT, args, mods)		\
  WValue WValue::name argsT mods {				\
    return WValue(&(val_->name args));				\
  }
#define RJV_WRAP_DEC_RETSV(name, argsT, args, mods)		\
  WValue& WValue::name argsT mods {				\
    val_->name args;						\
    return *this;						\
  }
#define RJV_WRAP_DEC_RETV_CONST(name, argsT, args, mods)		\
  const WValue WValue::name argsT const mods {				\
    return WValue(const_cast<RJ_WNS::Value*>(&(val_->name args)));	\
  }
#define RJD_WRAP_DEC_RETSD(name, argsT, args, mods)		\
  WDocument& WDocument::name argsT mods {			\
    doc_->name args;						\
    return *this;						\
  }
#define RJD_WRAP_DEC_RETV(name, argsT, args, mods)		\
  WValue WDocument::name argsT mods {				\
    return WValue(&(doc_->name args));				\
  }
#define RJD_WRAP_DEC_RETSV(name, argsT, args, mods)		\
  WValue& WDocument::name argsT mods {				\
    doc_->name args;						\
    return *this;						\
  }
#define RJV_WRAP_SG(name, type)				\
  RJV_WRAP_DEC(Get ## name, (), (), type, const);	\
  RJV_WRAP_DEC_RETSV(Set ## name, (type x), (x), );	\
  RJV_WRAP_DEC(Is ## name, (), (), bool, const)

WValue::WValue(Type type) :
  val_(new RJ_WNS::Value(type)), created_val(true) {}
WValue::WValue(const RJ_WNS::Value* val) :
  WValue(const_cast<RJ_WNS::Value*>(val)) {}
WValue::WValue(RJ_WNS::Value* val) :
  val_(val), created_val(false) {}
WValue::WValue(RJ_WNS::Value& val) :
  WValue(&val) {}
WValue::WValue(RJ_WNS::Document* val) :
  WValue(dynamic_cast<RJ_WNS::Value*>(val)) {}
WValue::~WValue() {
  if (created_val) {
    delete val_;
  }
  val_ = nullptr;
}
#if RAPIDJSON_HAS_CXX11_RVALUE_REFS
WValue::WValue(WValue&& rhs) :
  val_(nullptr), created_val(false) {
  std::swap(val_, rhs.val_);
  std::swap(created_val, rhs.created_val);
}
WValue& WValue::operator=(WValue&& rhs) {
  return *this = rhs.Move();
}

#endif
WValue& WValue::operator=(WValue& rhs) {
  std::swap(val_, rhs.val_);
  std::swap(created_val, rhs.created_val);
  return *this;
}
WValue& WValue::CopyFrom(const RJ_WNS::Document& rhs,
			 WValue::Allocator& allocator,
			 bool copyConstStrings) {
  val_->CopyFrom(rhs, allocator, copyConstStrings);
  return *this;
}
WValue& WValue::CopyFrom(const RJ_WNS::Value& rhs,
			 WValue::Allocator& allocator,
			 bool copyConstStrings) {
  val_->CopyFrom(rhs, allocator, copyConstStrings);
  return *this;
}
WValue& WValue::Swap(RJ_WNS::Value& rhs) {
  val_->Swap(rhs);
  return *this;
}

bool WValue::operator==(const WValue& rhs) const {
  return ((*val_) == (*rhs.val_));
}

  RJV_WRAP_DEC_RETSV(CopyFrom, (const WValue& rhs,
				WValue::Allocator& allocator,
				bool copyConstStrings),
		     (*(rhs.val_), allocator, copyConstStrings), );
  RJV_WRAP_DEC_RETSV(Swap, (WValue& rhs), (*(rhs.val_)), );
  RJV_WRAP_DEC_RETSV(PushBack, (WValue& value,
				WValue::Allocator& allocator),
		     (*(value.val_), allocator), );
  template <typename Handler>
  RJV_WRAP_DEC(Accept, (Handler& handler, bool skip_yggdrasil),
	       (handler, skip_yggdrasil), bool, const);
  RJV_WRAP_DEC_RETSV(SetNull, (), (), );
  RJV_WRAP_DEC(IsNull, (), (), bool, const);
  RJV_WRAP_SG(Bool, bool);
  RJV_WRAP_SG(Int, int);
  RJV_WRAP_SG(Uint, unsigned);
  RJV_WRAP_SG(Int64, int64_t);
  RJV_WRAP_SG(Uint64, uint64_t);
  RJV_WRAP_SG(Double, double);
  RJV_WRAP_SG(Float, float);
  RJV_WRAP_DEC(GetNElements, (), (), SizeType, const);
  RJV_WRAP_DEC_RETV_CONST(GetShape, (), (), );
  // String methods
  RJV_WRAP_DEC(IsString, (), (), bool, const);
  RJV_WRAP_DEC_RETSV(SetString, (const WValue::Ch* s, SizeType length,
				 WValue::Allocator& allocator),
		     (s, length, allocator), );
  RJV_WRAP_DEC(GetString, (), (), const WValue::Ch*, const);
  RJV_WRAP_DEC(GetStringLength, (), (), SizeType, const);
  // Templated methods
  RJV_WRAP_DEC_TEMP(Is, (), (), bool, const);
  RJV_WRAP_DEC_TEMP(IsScalar, (), (), bool, const);
  RJV_WRAP_DEC_TEMP(Is1DArray, (), (), bool, const);
  RJV_WRAP_DEC_TEMP(IsNDArray, (), (), bool, const);
  template<typename T>
  RJV_WRAP_DEC_RETSV(Set, (const T& data,
			   WValue::AllocatorType& allocator),
		     (data, allocator), );
  template<typename T>
  RJV_WRAP_DEC_RETSV(Set1DArray, (const T* x, SizeType len,
				  WValue::Allocator& allocator),
		     (x, len, allocator), );
  template<typename T>
  RJV_WRAP_DEC_RETSV(SetNDArray, (const T* x, SizeType shape[],
				  SizeType ndim,
				  WValue::Allocator& allocator),
		     (x, shape, ndim, allocator), );
  template<typename T>
  RJV_WRAP_DEC(Get, (T& data), (data), void, const);
  template<typename T>
  RJV_WRAP_DEC(GetScalarValue, (T& data), (data), void, const);
  // Array methods
  RJV_WRAP_DEC(IsArray, (), (), bool, const);
  RJV_WRAP_DEC_RETSV(SetArray, (), (), );
  RJV_WRAP_DEC(Size, (), (), rapidjson::SizeType, const);
  RJV_WRAP_DEC(Empty, (), (), bool, const);
  RJV_WRAP_DEC_RETSV(Reserve, (SizeType newCapacity,
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
	       RJ_WNS::Value&, );
  RJV_WRAP_DEC(operator[], (SizeType index), (index),
	       const RJ_WNS::Value&, const );
  // Object methods
  RJV_WRAP_DEC(IsObject, (), (), bool, const);
  RJV_WRAP_DEC_RETSV(SetObject, (), (), );
  RJV_WRAP_DEC(MemberCount, (), (), rapidjson::SizeType, const);
  RJV_WRAP_DEC(HasMember, (const WValue::Ch* name), (name), bool, const);
  template <typename T>
  RAPIDJSON_DISABLEIF_RETURN(
    (internal::NotExpr<
     internal::IsSame<typename internal::RemoveConst<T>::Type,
     typename WValue::Ch> >),
    (RJ_WNS::Value&)) WValue::operator[](T* name)
  { return (*val_)[name]; }
  template <typename T>
  RAPIDJSON_DISABLEIF_RETURN(
    (internal::NotExpr<
     internal::IsSame<typename internal::RemoveConst<T>::Type,
     typename WValue::Ch> >),
    (const RJ_WNS::Value&)) WValue::operator[](T* name) const
  { return const_cast<WValue&>(*this)[name]; }
  // RJV_WRAP_DEC_RETV(operator[], (const Ch* name), (name), );
  // RJV_WRAP_DEC_RETV_CONST(operator[], (const Ch* name), (name), );
  // RJV_WRAP_DEC(operator[], (const Ch* name), (name),
  // 	       RJ_WNS::Value&, );
  // RJV_WRAP_DEC(operator[], (const Ch* name), (name),
  // 	       const RJ_WNS::Value&, const);
  RJV_WRAP_DEC_RETSV(AddMember, (RJ_WNS::Value& name,
				 RJ_WNS::Value& value,
				 WValue::Allocator& allocator),
		     (name, value, allocator), );
  RJV_WRAP_DEC(RemoveMember, (const WValue::Ch* name), (name), bool, );

namespace rapidjson {
std::ostream & operator << (std::ostream &out, const WValue& p) {
  return out << *(p.val_);
}
}

WDocument::WDocument(Type type) :
  WValue((RJ_WNS::Value*)nullptr),
  doc_(new RJ_WNS::Document(type)), created_doc(true)
{ val_ = doc_; }
WDocument::WDocument(RJ_WNS::Document* doc) :
  WValue(doc), doc_(doc), created_doc(false) {}
WDocument::WDocument(RJ_WNS::Document& doc) :
  WDocument(&doc) {}
WDocument::~WDocument() {
  if (created_doc) {
    delete doc_;
  }
  doc_ = nullptr;
  val_ = nullptr;
}
#if RAPIDJSON_HAS_CXX11_RVALUE_REFS
WDocument::WDocument(WDocument&& rhs) :
  WValue(std::forward<WValue>(rhs)),
  doc_(nullptr), created_doc(false) {
  std::swap(doc_, rhs.doc_);
  std::swap(created_doc, rhs.created_doc);
}
WDocument& WDocument::operator=(WDocument&& rhs) {
  return *this = rhs.Move();
}
#endif
WDocument& WDocument::operator=(WDocument& rhs) {
  WValue::operator=(std::forward<WValue>(rhs));
  std::swap(doc_, rhs.doc_);
  std::swap(created_doc, rhs.created_doc);
  return *this;
}

WDocument& WDocument::CopyFrom(const RJ_WNS::Document& rhs,
			       WDocument::Allocator& allocator,
			       bool copyConstStrings) {
  doc_->CopyFrom(rhs, allocator, copyConstStrings);
  return *this;
}
WDocument& WDocument::Swap(RJ_WNS::Document& rhs) {
  doc_->Swap(rhs);
  return *this;
}

  RJD_WRAP_DEC_RETSV(CopyFrom, (const WDocument& rhs,
				WDocument::Allocator& allocator,
				bool copyConstStrings),
		    (*(rhs.doc_), allocator, copyConstStrings), );
  RJD_WRAP_DEC_RETSV(Swap, (WDocument& rhs), (*(rhs.doc_)), );
  RJD_WRAP_DEC(GetAllocator, (), (), WDocument::Allocator&, );
  RJD_WRAP_DEC(Normalize, (const WValue& schema,
			   StringBuffer* error),
	       (*(schema.val_), error), bool, );
  RJD_WRAP_DEC_RETSD(Parse, (const WValue::Ch* str, size_t length),
		     (str, length), );
  RJD_WRAP_DEC(HasParseError, (), (), bool, const);

#undef RJV_WRAP_DEC
#undef RJD_WRAP_DEC
#undef RJV_WRAP_DEC_TEMP
#undef RJV_WRAP_DEC_RETV
#undef RJV_WRAP_DEC_RETSV
#undef RJV_WRAP_DEC_RETV_CONST
#undef RJD_WRAP_DEC_RETV
#undef RJD_WRAP_DEC_RETSV
#undef RJD_WRAP_DEC_RETSD

// Explicitly instantiate template specializations

#define SPECIALIZE_ACCEPT(type)			\
  template bool WValue::Accept<type>(type& handler, bool skip_yggdrasil) const

SPECIALIZE_ACCEPT(rapidjson::PrettyWriter<rapidjson::StringBuffer>);
SPECIALIZE_ACCEPT(rapidjson::Writer<rapidjson::StringBuffer>);
SPECIALIZE_ACCEPT(rapidjson::SchemaEncoder);

#undef SPECIALIZE_ACCEPT

template RAPIDJSON_DISABLEIF_RETURN(
  (internal::NotExpr<
   internal::IsSame<typename internal::RemoveConst<const WValue::Ch>::Type,
   typename WValue::Ch> >),
  (RJ_WNS::Value&)) WValue::operator[]<const WValue::Ch>(const WValue::Ch* name);
template RAPIDJSON_DISABLEIF_RETURN(
  (internal::NotExpr<
   internal::IsSame<typename internal::RemoveConst<const WValue::Ch>::Type,
   typename WValue::Ch> >),
  (const RJ_WNS::Value&)) WValue::operator[]<const WValue::Ch>(const WValue::Ch* name) const;

#define SPECIALIZE_BASE(type, set_type)					\
  template bool WValue::Is<type>() const;				\
  template WValue& WValue::Set<set_type>(const set_type& data, WValue::Allocator& allocator); \
  template void WValue::Get<type>(type& data) const
#define SPECIALIZE(type)						\
  SPECIALIZE_BASE(type, type)
#define SPECIALIZE_SCALAR(type)						\
  SPECIALIZE(type);							\
  template bool WValue::IsScalar<type>() const;				\
  template bool WValue::Is1DArray<type>() const;			\
  template bool WValue::IsNDArray<type>() const;			\
  template WValue& WValue::Set1DArray<type>(const type* x, SizeType len, WValue::Allocator& allocator); \
  template WValue& WValue::SetNDArray<type>(const type* x, SizeType shape[], SizeType ndim, WValue::Allocator& allocator); \
  template void WValue::GetScalarValue<type>(type& data) const

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

#undef SPECIALIZE_SCALAR
#undef SPECIALIZE
#undef SPECIALIZE_BASE

#endif // WRAP_RAPIDJSON_FOR_DLL
