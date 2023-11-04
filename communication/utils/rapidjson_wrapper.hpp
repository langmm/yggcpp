#pragma once

#include <vector>
#include "rapidjson/rapidjson.h"
#include "rapidjson/internal/meta.h"
#include "rapidjson/stringbuffer.h"
#include "rapidjson/allocators.h"

#if defined(_MSC_VER)
// _WINDOWS) && !(defined(YggInterface_EXPORTS) || defined(YggInterface_py_EXPORTS) || defined(RAPIDJSON_FORCE_IMPORT_ARRAY))
#define WRAP_RAPIDJSON_FOR_DLL
#endif

#ifdef WRAP_RAPIDJSON_FOR_DLL

#define RJ_VAL(body)				\
  (*((body).val_))
#define RJ_DOC(body)				\
  (*((body).doc_))
#define RJ_WVAL(body)				\
  (rapidjson::WValue(&(body)))
#define RJ_WDOC(body)				\
  (rapidjson::WDocument(&(body)))

#include <ostream> // required for ostream

#ifdef _DEBUG
#undef _DEBUG
#include "Python.h"
#define _DEBUG
#else
#include "Python.h"
#endif


RAPIDJSON_NAMESPACE_BEGIN

// Forward declarations

template <typename Encoding, typename Allocator>
class GenericValue;
template <typename Encoding, typename Allocator, typename StackAllocator>
class GenericDocument;

class CrtAllocator;
template <typename BaseAllocator>
class MemoryPoolAllocator;
class VarArgList;

#ifndef RAPIDJSON_DEFAULT_ALLOCATOR
#define RAPIDJSON_DEFAULT_ALLOCATOR RAPIDJSON_NAMESPACE::MemoryPoolAllocator< RAPIDJSON_NAMESPACE::CrtAllocator >
#endif

#ifndef RAPIDJSON_DEFAULT_STACK_ALLOCATOR
#define RAPIDJSON_DEFAULT_STACK_ALLOCATOR RAPIDJSON_NAMESPACE::CrtAllocator
#endif

typedef GenericValue<UTF8<>, RAPIDJSON_DEFAULT_ALLOCATOR> Value;
typedef GenericDocument<UTF8<>,  RAPIDJSON_DEFAULT_ALLOCATOR,
			RAPIDJSON_DEFAULT_STACK_ALLOCATOR> Document;

#define RJV_WRAP_DEC(name, argsT, args, type, mods)	\
  type name argsT mods
#define RJD_WRAP_DEC(name, argsT, args, type, mods)	\
  RJV_WRAP_DEC(name, argsT, args, type, mods)
#define RJV_WRAP_DEC_RETV(name, argsT, args, mods)	\
  RJV_WRAP_DEC(name, argsT, args, WValue, mods)
#define RJV_WRAP_DEC_RETV_CONST(name, argsT, args, mods)	\
  RJV_WRAP_DEC(name, argsT, args, const WValue, const mods)
#define RJD_WRAP_DEC_RETD(name, argsT, args, mods)	\
  RJV_WRAP_DEC(name, argsT, args, WDocument, mods)
#define RJD_WRAP_DEC_RETV(name, argsT, args, mods)	\
  RJV_WRAP_DEC(name, argsT, args, WValue, mods)
#define RJD_WRAP_DEC_RETD_CONST(name, argsT, args, mods)	\
  RJV_WRAP_DEC(name, argsT, args, const WDocument, const mods)

// Helper classes for use in headers
class WValue {
public:
  typedef RAPIDJSON_DEFAULT_ALLOCATOR Allocator;
  typedef RAPIDJSON_DEFAULT_ALLOCATOR AllocatorType;
  typedef WValue ValueType;
  typedef rapidjson::Value* ValueIterator;
  typedef const rapidjson::Value* ConstValueIterator;
  typedef char Ch;
  WValue(Type type=rapidjson::kNullType);
  WValue(const rapidjson::Value* val);
  WValue(rapidjson::Value* val);
  WValue(rapidjson::Value& val);
  WValue(rapidjson::Document* val);
  ~WValue();
  WValue& CopyFrom(const rapidjson::Value& rhs,
		   Allocator& allocator,
		   bool copyConstStrings = false);
  WValue& CopyFrom(const rapidjson::Document& rhs,
		   Allocator& allocator,
		   bool copyConstStrings = false);
  bool operator==(const WValue& rhs) const;
  rapidjson::Value* val_;
  bool created_val;

  RJV_WRAP_DEC_RETV(CopyFrom, (const WValue& rhs, Allocator& allocator,
			  bool copyConstStrings = false),
		    (*(rhs.val_), allocator, copyConstStrings), );
  RJV_WRAP_DEC_RETV(Swap, (WValue rhs), (*(rhs.val_)), );
  RJV_WRAP_DEC_RETV(PushBack, (WValue& value, Allocator& allocator),
		    (*(value.val_), allocator), );
  template <typename Handler>
  RJV_WRAP_DEC(Accept, (Handler& handler, bool skip_yggdrasil=false),
	       (handler, skip_yggdrasil), bool, const);
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
  RJV_WRAP_DEC(Is, (), <T>(), bool, const);
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

  friend std::ostream & operator << (std::ostream &out, const WValue& p);
};

std::ostream & operator << (std::ostream &out, const WValue& p);

class WDocument : public WValue {
public:
  typedef RAPIDJSON_DEFAULT_ALLOCATOR Allocator;
  typedef RAPIDJSON_DEFAULT_ALLOCATOR AllocatorType;
  typedef WValue ValueType;
  WDocument(Type type=rapidjson::kNullType);
  WDocument(rapidjson::Document* doc);
  WDocument(rapidjson::Document& doc);

  using ValueType::CopyFrom;
  using ValueType::Swap;
  
  WDocument& CopyFrom(const rapidjson::Document& rhs,
		      Allocator& allocator,
		      bool copyConstStrings = false);
  
  RJD_WRAP_DEC_RETV(CopyFrom, (const WDocument& rhs,
			       Allocator& allocator,
			       bool copyConstStrings = false),
		    (*(rhs.doc_), allocator, copyConstStrings), );
  RJD_WRAP_DEC_RETD(Swap, (WDocument rhs), (*(rhs.doc_)), );
  RJD_WRAP_DEC(GetAllocator, (), (), Allocator&, );
  RJD_WRAP_DEC(Normalize, (const WValue& schema,
			   StringBuffer* error=NULL),
	       (*(schema.val_), error), bool, );
  RJD_WRAP_DEC_RETD(Parse, (const Ch* str, size_t length),
		    (str, length), );
  RJD_WRAP_DEC(HasParseError, (), (), bool, const);
  
  rapidjson::Document* doc_;
  bool created_doc;
};

#undef RJV_WRAP_DEC
#undef RJD_WRAP_DEC
#undef RJV_WRAP_DEC_RETV
#undef RJV_WRAP_DEC_RETV_CONST
#undef RJD_WRAP_DEC_RETV
#undef RJD_WRAP_DEC_RETD
#undef RJD_WRAP_DEC_RETD_CONST

RAPIDJSON_NAMESPACE_END

#else // WRAP_RAPIDJSON_FOR_DLL

#define RJ_VAL(body)				\
  body
#define RJ_DOC(body)				\
  body
#define RJ_WVAL(body)				\
  body
#define RJ_WDOC(body)				\
  body

#include "rapidjson/document.h"
#include "rapidjson/writer.h"
#include "rapidjson/schema.h"

RAPIDJSON_NAMESPACE_BEGIN

typedef Value WValue;
typedef Document WDocument;

RAPIDJSON_NAMESPACE_END

#endif // WRAP_RAPIDJSON_FOR_DLL


