#include "rapidjson_wrapper.hpp"

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

WValue::WValue() : val_(new rapidjson::Value()), created_val(true) {}

  RJV_WRAP_DEC(CopyFrom, (const WValue& rhs, Allocator& allocator,
			  bool copyConstStrings = false),
	       (*(rhs->val_), allocator, copyConstStrings),
	       rapidjson::Value&, );
  RJV_WRAP_DEC(Swap, (WValue& rhs), (*(rhs->val_)), rapidjson::Value&, );
  RJV_WRAP_DEC(PushBack, (WValue& value, Allocator& allocator),
	       (*(value->val_), allocator), rapidjson::Value&, );
  RJV_WRAP_DEC(GetNElements, (), (), SizeType, const);
  RJV_WRAP_DEC(GetShape, (), (), const rapidjson::Value&, const);
  // String methods
  RJV_WRAP_DEC(IsString, (), (), bool, const);
  RJV_WRAP_DEC(SetString, (const Ch* s, SizeType length,
			   Allocator& allocator),
	       (s, length, allocator), rapidjson::Value&, );
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
  RJV_WRAP_DEC(Set, (const T& data, AllocatorType& allocator),
	       (data, allocator), rapidjson::Value&, );
  template<typename T>
  RJV_WRAP_DEC(Set1DArray, (const T* x, SizeType len,
			    Allocator& allocator),
	       (x, len, allocator), rapidjson::Value&, );
  template<typename T>
  RJV_WRAP_DEC(SetNDArray, (const T* x, SizeType shape[], SizeType ndim,
			    Allocator& allocator),
	       (x, shape, ndim, allocator), rapidjson::Value&, );
  template<typename T>
  RJV_WRAP_DEC(Get, (T& data), (data), void, );
  template<typename T>
  RJV_WRAP_DEC(GetScalarValue, (T& data), (data), void, );
  // Array methods
  RJV_WRAP_DEC(IsArray, (), (), bool, const);
  RJV_WRAP_DEC(SetArray, (), (), rapidjson::Value&, );
  RJV_WRAP_DEC(Size, (), (), rapidjson::SizeType, const);
  RJV_WRAP_DEC(Empty, (), (), bool, const);
  RJV_WRAP_DEC(Reserve, (SizeType newCapacity, Allocator &allocator),
	       (newCapacity, allocator), rapidjson::Value&, );
  RJV_WRAP_DEC(Erase, (ConstValueIterator pos), (pos), ValueIterator, );
  RJV_WRAP_DEC(Begin, (), (), ValueIterator, );
  RJV_WRAP_DEC(End, (), (), ValueIterator, );
  RJV_WRAP_DEC(Begin, (), (), ConstValueIterator, const);
  RJV_WRAP_DEC(End, (), (), ConstValueIterator, const);

WDocument::WDocument() :
  WValue(nullptr), doc_(new rapidjson::Document()), created_doc(true)
{ val_ = doc_; }

  RJD_WRAP_DEC(Swap, (WDocument& rhs), (*(rhs->doc_)),
	       rapidjson::Document&, );
  RJD_WRAP_DEC(GetAllocator, (), (), Allocator&, );

#endif // WRAP_RAPIDJSON_FOR_DLL
