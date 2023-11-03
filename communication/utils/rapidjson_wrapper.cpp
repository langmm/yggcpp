#include "rapidjson_wrapper.hpp"

#include "rapidjson/document.h"

#ifdef WRAP_RAPIDJSON_FOR_DLL

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
#define RJD_WRAP_DEC_RETD_CONST(name, argsT, args, mods)	\
  const WDocument WDocument::name argsT const mods {		\
    return WDocument(&(doc_->name args));			\
  }

WValue::WValue(Type type) :
  val_(new rapidjson::Value(type)), created_val(true) {}
WValue::WValue(rapidjson::Document* val) :
  WValue(dynamic_cast<rapidjson::Value*>(val)) {}
WValue& WValue::CopyFrom(const rapidjson::Document& rhs,
			 Allocator& allocator,
			 bool copyConstStrings) {
  val_->CopyFrom(rhs, allocator, copyConstStrings);
  return *this;
}

  RJV_WRAP_DEC_RETV(CopyFrom, (const WValue& rhs, Allocator& allocator,
			       bool copyConstStrings),
		    (*(rhs.val_), allocator, copyConstStrings), );
  RJV_WRAP_DEC_RETV(Swap, (WValue rhs), (*(rhs.val_)), );
  RJV_WRAP_DEC_RETV(PushBack, (WValue& value, Allocator& allocator),
		    (*(value.val_), allocator), );
  RJV_WRAP_DEC(GetUint, (), (), unsigned, const);
  RJV_WRAP_DEC(GetNElements, (), (), SizeType, const);
  RJV_WRAP_DEC_RETV_CONST(GetShape, (), (), );
  // String methods
  RJV_WRAP_DEC(IsString, (), (), bool, const);
  RJV_WRAP_DEC_RETV(SetString, (const Ch* s, SizeType length,
				Allocator& allocator),
		    (s, length, allocator), );
  RJV_WRAP_DEC(GetString, (), (), const Ch*, const);
  RJV_WRAP_DEC(GetStringLength, (), (), SizeType, const);
  // Templated methods
  template <typename T>
  RJV_WRAP_DEC(Is, (), (), bool, const);
  template <typename T>
  RJV_WRAP_DEC(IsScalar, (), <T>(), bool, const);
  template <typename T>
  RJV_WRAP_DEC(Is1DArray, (), <T>(), bool, const);
  template <typename T>
  RJV_WRAP_DEC(IsNDArray, (), <T>(), bool, const);
  template<typename T>
  RJV_WRAP_DEC_RETV(Set, (const T& data, AllocatorType& allocator),
		    (data, allocator), );
  template<typename T>
  RJV_WRAP_DEC_RETV(Set1DArray, (const T* x, SizeType len,
				 Allocator& allocator),
		    (x, len, allocator), );
  template<typename T>
  RJV_WRAP_DEC_RETV(SetNDArray, (const T* x, SizeType shape[],
				 SizeType ndim, Allocator& allocator),
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
			      Allocator &allocator),
		    (newCapacity, allocator), );
  RJV_WRAP_DEC(Erase, (ConstValueIterator pos), (pos), ValueIterator, );
  RJV_WRAP_DEC(Begin, (), (), ValueIterator, );
  RJV_WRAP_DEC(End, (), (), ValueIterator, );
  RJV_WRAP_DEC(Begin, (), (), ConstValueIterator, const);
  RJV_WRAP_DEC(End, (), (), ConstValueIterator, const);
  RJV_WRAP_DEC_RETV(operator[], (SizeType index), (index), );
  RJV_WRAP_DEC_RETV_CONST(operator[], (SizeType index), (index), );

std::ostream & operator << (std::ostream &out, const WValue& p) {
  return out << p.val_;
}

WDocument::WDocument(Type type) :
  WValue(nullptr), doc_(new rapidjson::Document(type)), created_doc(true)
{ val_ = doc_; }

WDocument& WDocument::CopyFrom(const rapidjson::Document& rhs,
			       Allocator& allocator,
			       bool copyConstStrings) {
  doc_->CopyFrom(rhs, allocator, copyConstStrings);
  return *this;
}

  RJD_WRAP_DEC_RETD(CopyFrom, (const WDocument& rhs,
			       Allocator& allocator,
			       bool copyConstStrings),
		    (*(rhs.doc_), allocator, copyConstStrings), );
  RJD_WRAP_DEC_RETD(Swap, (WDocument rhs), (*(rhs.doc_)), );
  RJD_WRAP_DEC(GetAllocator, (), (), Allocator&, );

#undef RJV_WRAP_DEC
#undef RJD_WRAP_DEC
#undef RJV_WRAP_DEC_RETV
#undef RJV_WRAP_DEC_RETV_CONST
#undef RJD_WRAP_DEC_RETD
#undef RJD_WRAP_DEC_RETD_CONST

#endif // WRAP_RAPIDJSON_FOR_DLL
