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
#define RJ_WNS RAPIDJSON_NAMESPACE
// #define WValue Value
// #define WDocument Document

#include <ostream> // required for ostream

#ifdef _DEBUG
#undef _DEBUG
#include "Python.h"
#define _DEBUG
#else
#include "Python.h"
#endif


RAPIDJSON_NAMESPACE_BEGIN

#ifndef RAPIDJSON_DEFAULT_ALLOCATOR
#define RAPIDJSON_DEFAULT_ALLOCATOR RAPIDJSON_NAMESPACE::MemoryPoolAllocator< RAPIDJSON_NAMESPACE::CrtAllocator >
#endif

#ifndef RAPIDJSON_DEFAULT_STACK_ALLOCATOR
#define RAPIDJSON_DEFAULT_STACK_ALLOCATOR RAPIDJSON_NAMESPACE::CrtAllocator
#endif

// Forward declarations
// namespace wrap {

template <typename Encoding, typename Allocator>
class GenericValue;
template <typename Encoding, typename Allocator, typename StackAllocator>
class GenericDocument;

class CrtAllocator;
template <typename BaseAllocator>
class MemoryPoolAllocator;
class VarArgList;

typedef GenericValue<UTF8<>, RAPIDJSON_DEFAULT_ALLOCATOR> Value;
typedef GenericDocument<UTF8<>,  RAPIDJSON_DEFAULT_ALLOCATOR,
			RAPIDJSON_DEFAULT_STACK_ALLOCATOR> Document;

#define RJV_WRAP_DEC(name, argsT, args, type, mods)	\
  type name argsT mods
#define RJV_WRAP_DEC_TEMP(name, argsT, args, type, mods)	\
  template <typename T>						\
  type name argsT mods
#define RJD_WRAP_DEC(name, argsT, args, type, mods)	\
  RJV_WRAP_DEC(name, argsT, args, type, mods)
#define RJV_WRAP_DEC_RETV(name, argsT, args, mods)	\
  RJV_WRAP_DEC(name, argsT, args, WValue, mods)
#define RJV_WRAP_DEC_RETSV(name, argsT, args, mods)	\
  RJV_WRAP_DEC(name, argsT, args, WValue&, mods)
#define RJV_WRAP_DEC_RETV_CONST(name, argsT, args, mods)	\
  RJV_WRAP_DEC(name, argsT, args, const WValue, const mods)
#define RJD_WRAP_DEC_RETSD(name, argsT, args, mods)	\
  RJV_WRAP_DEC(name, argsT, args, WDocument&, mods)
#define RJD_WRAP_DEC_RETV(name, argsT, args, mods)	\
  RJV_WRAP_DEC(name, argsT, args, WValue, mods)
#define RJD_WRAP_DEC_RETSV(name, argsT, args, mods)	\
  RJV_WRAP_DEC(name, argsT, args, WValue&, mods)
#define RJV_WRAP_SG(name, type)				\
  RJV_WRAP_DEC(Get ## name, (), (), type, const);	\
  RJV_WRAP_DEC_RETSV(Set ## name, (type x), (x), );	\
  RJV_WRAP_DEC(Is ## name, (), (), bool, const)

class WDocument;

// Helper classes for use in headers
class WValue {
private:
  WValue(const WValue&) = delete;
  WValue& operator=(const WValue&) = delete;
public:
  typedef RAPIDJSON_DEFAULT_ALLOCATOR Allocator;
  typedef RAPIDJSON_DEFAULT_ALLOCATOR AllocatorType;
  typedef WValue ValueType;
  typedef RJ_WNS::Value* ValueIterator;
  typedef const RJ_WNS::Value* ConstValueIterator;
  typedef char Ch;
  WValue(Type type=rapidjson::kNullType);
  WValue(const RJ_WNS::Value* val);
  WValue(RJ_WNS::Value* val);
  WValue(RJ_WNS::Value& val);
  WValue(RJ_WNS::Document* val);
  ~WValue();
#if RAPIDJSON_HAS_CXX11_RVALUE_REFS
  WValue(WValue&& rhs);
  WValue& operator=(WValue&& rhs);
#endif // RAPIDJSON_HAS_CXX11_RVALUE_REFS
  WValue& operator=(WValue& rhs);
  WValue& Move() { return *this; }
  WValue& CopyFrom(const RJ_WNS::Value& rhs,
		   Allocator& allocator,
		   bool copyConstStrings = false);
  WValue& CopyFrom(const RJ_WNS::Document& rhs,
		   Allocator& allocator,
		   bool copyConstStrings = false);
  WValue& Swap(RJ_WNS::Value& rhs);
  bool operator==(const WValue& rhs) const;
  RJ_WNS::Value* val_;
  bool created_val;


  RJV_WRAP_DEC_RETSV(CopyFrom, (const WValue& rhs, Allocator& allocator,
				bool copyConstStrings = false),
		     (*(rhs.val_), allocator, copyConstStrings), );
  RJV_WRAP_DEC_RETSV(Swap, (WValue& rhs), (*(rhs.val_)), );
  RJV_WRAP_DEC_RETSV(PushBack, (WValue& value, Allocator& allocator),
		     (*(value.val_), allocator), );
  template <typename Handler>
  RJV_WRAP_DEC(Accept, (Handler& handler, bool skip_yggdrasil=false),
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
  RJV_WRAP_DEC_RETSV(SetString, (const Ch* s, SizeType length,
				 Allocator& allocator),
		     (s, length, allocator), );
  RJV_WRAP_DEC(GetString, (), (), const Ch*, const);
  RJV_WRAP_DEC(GetStringLength, (), (), SizeType, const);
  // Templated methods
  RJV_WRAP_DEC_TEMP(Is, (), <T>(), bool, const);
  RJV_WRAP_DEC_TEMP(IsScalar, (), <T>(), bool, const);
  RJV_WRAP_DEC_TEMP(Is1DArray, (), <T>(), bool, const);
  RJV_WRAP_DEC_TEMP(IsNDArray, (), <T>(), bool, const);
  template<typename T>
  RJV_WRAP_DEC_RETSV(Set, (const T& data, AllocatorType& allocator),
		     (data, allocator), );
  template<typename T>
  RJV_WRAP_DEC_RETSV(Set1DArray, (const T* x, SizeType len,
				  Allocator& allocator),
		     (x, len, allocator), );
  template<typename T>
  RJV_WRAP_DEC_RETSV(SetNDArray, (const T* x, SizeType shape[],
				  SizeType ndim, Allocator& allocator),
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
	       RJ_WNS::Value&, );
  RJV_WRAP_DEC(operator[], (SizeType index), (index),
	       const RJ_WNS::Value&, const );
  // Object methods
  RJV_WRAP_DEC(IsObject, (), (), bool, const);
  RJV_WRAP_DEC_RETSV(SetObject, (), (), );
  RJV_WRAP_DEC(MemberCount, (), (), rapidjson::SizeType, const);
  RJV_WRAP_DEC(HasMember, (const Ch* name), (name), bool, const);
  // RJV_WRAP_DEC_RETV(operator[], (const Ch* name), (name), );
  // RJV_WRAP_DEC_RETV_CONST(operator[], (const Ch* name), (name), );
  template <typename T>
  RAPIDJSON_DISABLEIF_RETURN(
    (internal::NotExpr<
     internal::IsSame<typename internal::RemoveConst<T>::Type, Ch> >),
    (RJ_WNS::Value&)) operator[](T* name);
  template <typename T>
  RAPIDJSON_DISABLEIF_RETURN(
    (internal::NotExpr<
     internal::IsSame<typename internal::RemoveConst<T>::Type, Ch> >),
    (const RJ_WNS::Value&)) operator[](T* name) const;
  // RJV_WRAP_DEC(operator[], (const Ch* name), (name),
  // 	       RJ_WNS::Value&, );
  // RJV_WRAP_DEC(operator[], (const Ch* name), (name),
  // 	       const RJ_WNS::Value&, const);
  RJV_WRAP_DEC_RETSV(AddMember, (RJ_WNS::Value& name,
				 RJ_WNS::Value& value,
				 WValue::Allocator& allocator),
		     (name, value, allocator), );
  RJV_WRAP_DEC(RemoveMember, (const Ch* name), (name), bool, );

  friend std::ostream & operator << (std::ostream &out,
				     const WValue& p);
};

class WDocument : public WValue {
private:
  WDocument(const WDocument&) = delete;
  WDocument& operator=(const WDocument&) = delete;
public:
  typedef RAPIDJSON_DEFAULT_ALLOCATOR Allocator;
  typedef RAPIDJSON_DEFAULT_ALLOCATOR AllocatorType;
  typedef WValue ValueType;
  WDocument(Type type=rapidjson::kNullType);
  WDocument(RJ_WNS::Document* doc);
  WDocument(RJ_WNS::Document& doc);
  ~WDocument();
#if RAPIDJSON_HAS_CXX11_RVALUE_REFS
  WDocument(WDocument&& rhs);
  WDocument& operator=(WDocument&& rhs);
#endif // RAPIDJSON_HAS_CXX11_RVALUE_REFS
  WDocument& operator=(WDocument& rhs);
  WDocument& Move() { return *this; }

  using ValueType::CopyFrom;
  using ValueType::Swap;
  
  WDocument& CopyFrom(const RJ_WNS::Document& rhs,
		      Allocator& allocator,
		      bool copyConstStrings = false);
  WDocument& Swap(RJ_WNS::Document& rhs);
  
  RJD_WRAP_DEC_RETSV(CopyFrom, (const WDocument& rhs,
				Allocator& allocator,
				bool copyConstStrings = false),
		    (*(rhs.doc_), allocator, copyConstStrings), );
  RJD_WRAP_DEC_RETSV(Swap, (WDocument& rhs), (*(rhs.doc_)), );
  RJD_WRAP_DEC(GetAllocator, (), (), Allocator&, );
  RJD_WRAP_DEC(Normalize, (const WValue& schema,
			   StringBuffer* error=NULL),
	       (*(schema.val_), error), bool, );
  RJD_WRAP_DEC_RETSD(Parse, (const Ch* str, size_t length),
		     (str, length), );
  RJD_WRAP_DEC(HasParseError, (), (), bool, const);
  
  RJ_WNS::Document* doc_;
  bool created_doc;
};

#undef RJV_WRAP_DEC
#undef RJD_WRAP_DEC
#undef RJV_WRAP_DEC_TEMP
#undef RJV_WRAP_DEC_RETV
#undef RJV_WRAP_DEC_RETSV
#undef RJV_WRAP_DEC_RETV_CONST
#undef RJD_WRAP_DEC_RETV
#undef RJD_WRAP_DEC_RETSV
#undef RJD_WRAP_DEC_RETSD

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
#define RJ_WNS RAPIDJSON_NAMESPACE

#include "rapidjson/document.h"
#include "rapidjson/writer.h"
#include "rapidjson/prettywriter.h"
#include "rapidjson/schema.h"

RAPIDJSON_NAMESPACE_BEGIN

typedef Value WValue;
typedef Document WDocument;

RAPIDJSON_NAMESPACE_END

#endif // WRAP_RAPIDJSON_FOR_DLL
