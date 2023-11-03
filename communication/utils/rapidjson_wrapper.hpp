#pragma once

#include "rapidjson/rapidjson.h"

// #if defined(_WINDOWS) && !(defined(YggInterface_EXPORTS) || defined(YggInterface_py_EXPORTS) || defined(RAPIDJSON_FORCE_IMPORT_ARRAY))
// #define WRAP_RAPIDJSON_FOR_DLL
// #endif

#ifdef WRAP_RAPIDJSON_FOR_DLL

RAPIDJSON_NAMESPACE_BEGIN

// Forward declarations
class Value;
class Document;
class CrtAllocator;
template <typename BaseAllocator>
class MemoryPoolAllocator;
class VarArgList;
class StringBuffer;

#ifndef RAPIDJSON_DEFAULT_ALLOCATOR
#define RAPIDJSON_DEFAULT_ALLOCATOR RAPIDJSON_NAMESPACE::MemoryPoolAllocator< RAPIDJSON_NAMESPACE::CrtAllocator >
#endif

#define RJV_WRAP_DEC(name, argsT, args, type, mods)	\
  type name argsT mods
#define RJD_WRAP_DEC(name, argsT, args, type, mods)	\
  RJV_WRAP_DEC(name, argsT, args, type, mods)

// Helper classes for use in headers
class WValue {
public:
  typedef RAPIDJSON_DEFAULT_ALLOCATOR Allocator;
  typedef RAPIDJSON_DEFAULT_ALLOCATOR AllocatorType;
  typedef WValue ValueType;
  typedef rapidjson::Value* ValueIterator;
  typedef const rapidjson::Value* ConstValueIterator;
  WValue();
  WValue(rapidjson::Value* val) :
    val_(val), created_val(false) {}
  WValue(rapidjson::Value& val) : WValue(&val) {}
  ~WValue();
  bool created_val;
  rapidjson::Value* val_;
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
};
class WDocument : public WValue {
public:
  WDocument();
  WDocument(rapidjson::Document* doc) :
    WValue(doc), created_doc(false) {}
  WDocument(rapidjson::Document& doc) : WDocument(&doc) {}
  RJD_WRAP_DEC(Swap, (WDocument& rhs), (*(rhs->doc_)),
	       rapidjson::Document&, );
  RJD_WRAP_DEC(GetAllocator, (), (), Allocator&, );
  bool created_doc;
  rapidjson::Document* doc_;
};

RAPIDJSON_NAMESPACE_END

#else // WRAP_RAPIDJSON_FOR_DLL

#include "rapidjson/document.h"
#include "rapidjson/writer.h"
#include "rapidjson/schema.h"
#include "rapidjson/internal/meta.h"

RAPIDJSON_NAMESPACE_BEGIN

typedef Value WValue;
typedef Document WDocument;

RAPIDJSON_NAMESPACE_END

#endif // WRAP_RAPIDJSON_FOR_DLL


