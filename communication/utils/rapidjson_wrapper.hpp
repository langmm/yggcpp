#pragma once

#include <vector>
#include <string>
#include <iostream>
#include "rapidjson/rapidjson.h"
#include "rapidjson/internal/meta.h"
#include "rapidjson/stringbuffer.h"
#include "rapidjson/allocators.h"
#include "rapidjson/reader.h"
#include "rapidjson/writer.h"
#include "rapidjson/va_list.h"
#include "rapidjson/units.h"
#include "rapidjson/ply.h"
#include "rapidjson/obj.h"
#include "rapidjson/helpers.h"

#define WStringRefType StringRefType
#define WValue Value
#define WDocument Document
#define WSchemaDocument SchemaDocument
#define WSchemaValidator SchemaValidator
#define WSchemaNormalizer SchemaNormalizer
#define WSchemaEncoder SchemaEncoder
#define WPrettyWriter PrettyWriter

#if defined(_MSC_VER)
// _WINDOWS) && !(defined(YggInterface_EXPORTS) || defined(YggInterface_py_EXPORTS) || defined(RAPIDJSON_FORCE_IMPORT_ARRAY))
#define WRAP_RAPIDJSON_FOR_DLL
#endif

#ifdef WRAP_RAPIDJSON_FOR_DLL

#define RJ_WNS wrap::RAPIDJSON_NAMESPACE

#include <ostream> // required for ostream

#ifdef _DEBUG
#undef _DEBUG
#include "Python.h"
#define _DEBUG
#else
#include "Python.h"
#endif

#ifndef RAPIDJSON_DEFAULT_ALLOCATOR
#define RAPIDJSON_DEFAULT_ALLOCATOR ::RAPIDJSON_NAMESPACE::MemoryPoolAllocator< ::RAPIDJSON_NAMESPACE::CrtAllocator >
#endif

#ifndef RAPIDJSON_DEFAULT_STACK_ALLOCATOR
#define RAPIDJSON_DEFAULT_STACK_ALLOCATOR ::RAPIDJSON_NAMESPACE::CrtAllocator
#endif

// Forward declarations
namespace wrap {
  namespace rapidjson {
    // document.h classes
    template<typename CharType>
    struct GenericStringRef;
    template <typename Encoding, typename Allocator>
    class GenericValue;
    template <typename Encoding, typename Allocator, typename StackAllocator>
    class GenericDocument;
    template <typename Encoding, typename Allocator>
    class GenericMember;
    template <bool Const, typename Encoding, typename Allocator>
    class GenericMemberIterator;

    typedef GenericStringRef<char> StringRefType;
    typedef GenericValue<
      ::rapidjson::UTF8<>, RAPIDJSON_DEFAULT_ALLOCATOR> Value;
    typedef GenericDocument<
      ::rapidjson::UTF8<>, RAPIDJSON_DEFAULT_ALLOCATOR,
      RAPIDJSON_DEFAULT_STACK_ALLOCATOR> Document;
    
    // schema.h classes
    template <typename ValueType, typename Allocator>
    class GenericSchemaDocument;
    template <typename SchemaDocumentType, typename OutputHandler,
	      typename StateAllocator>
    class GenericSchemaValidator;
    template <typename SchemaDocumentType, typename OutputHandler,
	      typename StateAllocator>
    class GenericSchemaNormalizer;
    template<typename Encoding, typename Allocator, typename StackAllocator>
    class GenericSchemaEncoder;
  
    typedef GenericSchemaDocument<
      Value, ::rapidjson::CrtAllocator> SchemaDocument;
    typedef GenericSchemaValidator<
      SchemaDocument,
      ::rapidjson::BaseReaderHandler<::rapidjson::UTF8<> >,
      ::rapidjson::CrtAllocator> SchemaValidator;
    typedef GenericSchemaNormalizer<
      SchemaDocument,
      ::rapidjson::BaseReaderHandler<::rapidjson::UTF8<> >,
      ::rapidjson::CrtAllocator> SchemaNormalizer;
    typedef GenericSchemaEncoder<
      ::rapidjson::UTF8<>, RAPIDJSON_DEFAULT_ALLOCATOR,
      RAPIDJSON_DEFAULT_STACK_ALLOCATOR> SchemaEncoder;

    // prettywriter.h classes
    template<typename OutputStream, typename SourceEncoding,
	     typename TargetEncoding, typename StackAllocator,
	     unsigned writeFlags>
    class PrettyWriter;

  }
}

RAPIDJSON_NAMESPACE_BEGIN

/*!
  @brief Initialize the Python interpreter.
 */
void initialize_python(const std::string error_prefix="");

/*!
  @brief Finalize the Python interpreter.
 */
void finalize_python(const std::string error_prefix="");

// Forward declarations
class WDocument;
class WMember;
template<bool Const>
class WGenericMemberIterator;
typedef WGenericMemberIterator<false> WMemberIterator;
typedef WGenericMemberIterator<true> WConstMemberIterator;

#define WRAP_CONSTRUCTOR(cls, argsT, args)		\
  cls argsT
#define WRAP_METHOD(cls, name, argsT, args, type, mods)	\
  type name argsT mods
#define WRAP_METHOD_TEMP(cls, name, argsT, args, type, mods)	\
  template <typename T>						\
  WRAP_METHOD(cls, name, argsT, args, type, mods)
#define WRAP_METHOD_SELF(cls, name, argsT, args, mods)	\
  WRAP_METHOD(cls, name, argsT, args, cls&, mods)
#define WRAP_METHOD_SELF_CAST(cls, name, argsT, args, type, mods)	\
  WRAP_METHOD(cls, name, argsT, args, type&, mods)
#define WRAP_METHOD_CAST(cls, name, argsT, args, type, mods)	\
  WRAP_METHOD(cls, name, argsT, args, type, mods)
#define WRAP_METHOD_CAST_CONST(cls, name, argsT, args, type, mods)	\
  WRAP_METHOD(cls, name, argsT, args, const type, const mods)
#define WRAP_SET_GET(name, type)					\
  WRAP_CONSTRUCTOR(explicit WValue, (type x), (x));			\
  WRAP_METHOD(WValue, Get ## name, (), (), type, const);		\
  WRAP_METHOD_SELF(WValue, Set ## name, (type x), (x), );		\
  WRAP_METHOD(WValue, Is ## name, (), (), bool, const)
#define WRAP_METHOD_ITER(cls, name, argsT, args, mods)	\
  WRAP_METHOD(cls, name, argsT, args, cls, mods)
#define WRAP_GET_STRING(name)			\
  static const WValue Get ## name ## String();


////////////////////////////////////////////////////////////////////
// WrapperBase
////////////////////////////////////////////////////////////////////

template<typename T>
class WrapperBase {
private:
  WrapperBase(const WrapperBase&) = delete;
  WrapperBase& operator=(const WrapperBase&) = delete;
public:
  typedef T BaseType;
  WrapperBase(T* val, bool created=false, bool iteration=false) :
    val_(val), created_val(created), iter(iteration) {}
  WrapperBase(T& val) :
    WrapperBase(&val) {}
  T* val_;
  bool created_val;
  bool iter;
};

#define WRAPPER_BASE(base)			\
  WrapperBase<RJ_WNS::base>
#define WRAPPER_CLASS(base)			\
  W ## base : public WRAPPER_BASE(base)
#define WRAPPER_METHODS_PRIV_(cls)		\
private:					\
 cls(const cls&) = delete;			\
 cls& operator=(const cls&) = delete
#define WRAPPER_METHODS_OPS_(cls)		\
 cls* operator->() const;			\
 cls& operator++();				\
 cls operator++(int);				\
 bool operator==(const cls& rhs) const;		\
 bool operator!=(const cls& rhs) const
#define WRAPPER_METHODS_EMPTY_CONSTRUCTOR(cls)	\
 cls()
#define WRAPPER_METHODS_MOVE_CONSTRUCTOR(cls, base)	\
  cls(base&& val)
#define WRAPPER_METHODS_(cls, base)		\
public:						\
 typedef base BaseType;				\
 cls(base* val, bool iterator=false);		\
 cls(base& val);				\
 cls(cls&& rhs);				\
 ~cls();					\
 cls& operator=(cls&& rhs);			\
 cls& operator=(cls& rhs);			\
 cls& Move()
#define WRAPPER_METHODS(base)					\
  WRAPPER_METHODS_PRIV_(W ## base);				\
  WRAPPER_METHODS_(W ## base, RJ_WNS::base);			\
  WRAPPER_METHODS_EMPTY_CONSTRUCTOR(W ## base);			\
  WRAPPER_METHODS_MOVE_CONSTRUCTOR(W ## base, RJ_WNS::base);	\
  WRAPPER_METHODS_OPS_(W ## base)
#define WRAPPER_METHODS_NO_EMPTY_CONSTRUCTOR(base)	\
  WRAPPER_METHODS_(W ## base, RJ_WNS::base)

#define WRAP_HANDLER_METHOD(cls, name, argsT, args)	\
  WRAP_METHOD(cls, name, argsT, args, bool, )
#define WRAP_HANDLER_METHODS(cls)					\
  WRAP_HANDLER_METHOD(cls, Null, (), ());				\
  WRAP_HANDLER_METHOD(cls, Bool, (bool b), (b));			\
  WRAP_HANDLER_METHOD(cls, Int, (int i), (i));				\
  WRAP_HANDLER_METHOD(cls, Uint, (unsigned u), (u));			\
  WRAP_HANDLER_METHOD(cls, Int64, (int64_t i64), (i64));		\
  WRAP_HANDLER_METHOD(cls, Uint64, (uint64_t u64), (u64));		\
  WRAP_HANDLER_METHOD(cls, Double, (double d), (d));			\
  WRAP_HANDLER_METHOD(cls, String, (const Ch* str, SizeType length,	\
				    bool copy = false),			\
		      (str, length, copy));				\
  WRAP_HANDLER_METHOD(cls, StartObject, (), ());			\
  WRAP_HANDLER_METHOD(cls, Key, (const Ch* str, SizeType length,	\
				 bool copy = false),			\
		      (str, length, copy));				\
  WRAP_HANDLER_METHOD(cls, EndObject, (SizeType memberCount = 0),	\
		      (memberCount));					\
  WRAP_HANDLER_METHOD(cls, StartArray, (), ());				\
  WRAP_HANDLER_METHOD(cls, EndArray, (SizeType memberCount = 0),	\
		      (memberCount));					\
  WRAP_HANDLER_METHOD(cls, YggdrasilString,				\
		      (const Ch* str, SizeType length,			\
		       bool copy, WDocument* schema),			\
		      (str, length, copy, *(schema->val_)));		\
  WRAP_HANDLER_METHOD(cls, YggdrasilStartObject,			\
		      (WDocument* schema),				\
		      (*(schema->val_)));				\
  WRAP_HANDLER_METHOD(cls, YggdrasilEndObject,				\
		      (SizeType memberCount = 0), (memberCount));	\
  template<typename SchemaValueType>					\
  bool YggdrasilString(const Ch* str, SizeType length,			\
		       bool copy, SchemaValueType& schema) {		\
    WDocument doc;							\
    schema.Accept(doc);							\
    doc.FinalizeFromStack();						\
    return YggdrasilString(str, length, copy, &doc);			\
  }									\
  template<typename SchemaValueType>					\
  bool YggdrasilStartObject(SchemaValueType& schema) {			\
    WDocument doc;							\
    schema.Accept(doc);							\
    doc.FinalizeFromStack();						\
    return YggdrasilStartObject(&doc);					\
  }


////////////////////////////////////////////////////////////////////
// WStringRefType
////////////////////////////////////////////////////////////////////

class WRAPPER_CLASS(StringRefType) {
  WRAPPER_METHODS_NO_EMPTY_CONSTRUCTOR(StringRefType);
  typedef char Ch;
  typedef char CharType;
  WRAP_CONSTRUCTOR(explicit WStringRefType, (const CharType* str), (str));
  WRAP_CONSTRUCTOR(WStringRefType, (const CharType* str, SizeType len),
		   (str, len));
  WRAP_CONSTRUCTOR(WStringRefType, (const WStringRefType& rhs),
		   (*(rhs.val_)));
  template<SizeType N>
  WStringRefType(const CharType (&str)[N]) :
    WStringRefType(&(str[0]), N) {}
  operator const Ch *() const;
};

////////////////////////////////////////////////////////////////////
// WValue
////////////////////////////////////////////////////////////////////

#define INDEX_RTYPE WValue&
#define INDEX_METHOD			\
  childRef(&tmp)
#define INDEX_METHOD_CONST			\
  const_cast<WValue*>(this)->childRef(const_cast<RJ_WNS::Value*>(&tmp))

class WRAPPER_CLASS(Value) {
public:
  typedef RAPIDJSON_DEFAULT_ALLOCATOR Allocator;
  typedef RAPIDJSON_DEFAULT_ALLOCATOR AllocatorType;
  typedef WValue ValueType;
  typedef WValue ValueIterator;
  typedef WValue ConstValueIterator;
  typedef WMemberIterator MemberIterator;
  typedef WConstMemberIterator ConstMemberIterator;
  typedef char Ch;
  typedef rapidjson::units::Units UnitsType;
  
  std::vector<WValue> refs;
  WValue& childRef(RJ_WNS::Value* x);
  
  WRAPPER_METHODS(Value);
  WRAP_CONSTRUCTOR(explicit WValue, (Type type), (type));
  WRAP_CONSTRUCTOR(WValue,
		   (const Ch* str, SizeType len, Allocator& allocator),
		   (str, len, allocator));
  WRAP_CONSTRUCTOR(WValue, (const Ch* str, SizeType len), (str, len));
  WRAP_CONSTRUCTOR(WValue, (const std::string& s, Allocator& allocator),
		   (s, allocator));
  WRAP_CONSTRUCTOR(WValue,
		   (const WValue& rhs, Allocator& allocator,
		    bool copyConstStrings = false),
		   (*(rhs.val_), allocator, copyConstStrings));
  WRAP_CONSTRUCTOR(explicit WValue,
		   (PyObject* pyobj, Allocator& allocator),
		   (pyobj, allocator));
  WRAP_CONSTRUCTOR(explicit WValue,
		   (const ObjWavefront& x, Allocator& allocator),
		   (x, allocator));
  WRAP_CONSTRUCTOR(explicit WValue,
		   (const Ply& x, Allocator& allocator),
		   (x, allocator));
  WValue(RJ_WNS::Document* val);
  
  WRAP_METHOD_SELF(WValue, CopyFrom,
		   (const WValue& rhs, Allocator& allocator,
		    bool copyConstStrings = false),
		   (*(rhs.val_), allocator, copyConstStrings), );
  WRAP_METHOD_SELF(WValue, Swap, (WValue& rhs), (*(rhs.val_)), );
  WRAP_METHOD_SELF(WValue, Swap, (WValue&& rhs), (*(rhs.val_)), );
  WRAP_METHOD_SELF(WValue, PushBack,
		   (WValue& value, Allocator& allocator),
		   (*(value.val_), allocator), );
  template <typename Handler>
  WRAP_METHOD(WValue, Accept, (Handler& handler, bool skip_yggdrasil=false),
	       (handler, skip_yggdrasil), bool, const);
  WRAP_METHOD_SELF(WValue, SetNull, (), (), );
  WRAP_METHOD(WValue, IsNull, (), (), bool, const);
  WRAP_SET_GET(Bool, bool);
  WRAP_SET_GET(Int, int);
  WRAP_SET_GET(Uint, unsigned);
  WRAP_SET_GET(Int64, int64_t);
  WRAP_SET_GET(Uint64, uint64_t);
  WRAP_SET_GET(Double, double);
  WRAP_SET_GET(Float, float);
  WRAP_METHOD(WValue, GetNElements, (), (), SizeType, const);
  WRAP_METHOD_CAST_CONST(WValue, GetShape, (), (), WValue, );
  // String methods
  WRAP_METHOD(WValue, IsString, (), (), bool, const);
  WRAP_METHOD_SELF(WValue, SetString, (const Ch* s, SizeType length,
				       Allocator& allocator),
		   (s, length, allocator), );
  WRAP_METHOD(WValue, GetString, (), (), const Ch*, const);
  WRAP_METHOD(WValue, GetStringLength, (), (), SizeType, const);
  // Templated methods
  WRAP_METHOD_TEMP(WValue, Is, (), (), bool, const);
  WRAP_METHOD_TEMP(WValue, IsScalar, (), (), bool, const);
  WRAP_METHOD_TEMP(WValue, Is1DArray, (), (), bool, const);
  WRAP_METHOD_TEMP(WValue, IsNDArray, (), (), bool, const);
  template<typename T>
  WRAP_METHOD(WValue, Get, (T& data), (data), void, const);
  template<typename T>
  WRAP_METHOD_SELF(WValue, Set, (const T& data, Allocator& allocator),
		   (data, allocator), );
  template<typename T>
  WRAP_METHOD_SELF(WValue, Set, (const T& data), (data), );
  // Scalar methods
  WRAP_METHOD_TEMP(WValue, GetScalar, (), (), T, const);
  WRAP_METHOD_TEMP(WValue, GetScalar,
		   (const UnitsType data_units),
		   (data_units), T, const);
  WRAP_METHOD_TEMP(WValue, GetScalar, (const Ch* units_str),
		   (units_str), T, const);
  template<typename T>
  WRAP_METHOD(WValue, GetScalar, (T& data), (data), void, const);
  template<typename T>
  WRAP_METHOD(WValue, GetScalar,
	      (T& data, const UnitsType data_units),
	      (data, data_units), void, const);
  template<typename T>
  WRAP_METHOD(WValue, GetScalar, (T& data, const Ch* units_str),
	      (data, units_str), void, const);
  template<typename T>
  WRAP_METHOD(WValue, GetScalarValue, (T& data), (data), void, const);
  template<typename T>
  WRAP_METHOD_SELF(WValue, SetScalar,
		   (const T data, Allocator& allocator),
		   (data, allocator), );
  template<typename T>
  WRAP_METHOD_SELF(WValue, SetScalar,
		   (const T data, const Ch* units_str,
		    Allocator& allocator),
		   (data, units_str, allocator), );
  template<typename T>
  WRAP_METHOD_SELF(WValue, SetScalar,
		   (const T data, const Ch* units_str,
		    const SizeType units_len, Allocator& allocator),
		   (data, units_str, units_len, allocator), );
  WRAP_METHOD_SELF(WValue, SetScalar,
		   (const Ch* data, const SizeType precision,
		    Allocator& allocator,
		    const Ch* encoding=NULL,
		    SizeType encoding_len=0),
		   (data, precision, allocator,
		    encoding, encoding_len), );
  // 1DArray
  template<typename T>
  WRAP_METHOD(WValue, Get1DArray,
	      (T*& data, SizeType& nelements,
	       Allocator& allocator,
	       const UnitsType data_units = UnitsType()),
	      (data, nelements, allocator, data_units), void, const);
  template<typename T>
  WRAP_METHOD(WValue, Get1DArray,
	      (T*& data, SizeType& nelements,
	       Allocator& allocator, const Ch* units_str),
	      (data, nelements, allocator, units_str), void, const);
  WRAP_METHOD_TEMP(WValue, Get1DArray,
		   (SizeType& nelements, Allocator& allocator,
		    const UnitsType data_units = UnitsType()),
		   (nelements, allocator, data_units), T*, const);
  WRAP_METHOD_TEMP(WValue, Get1DArray,
		   (SizeType& nelements, Allocator& allocator,
		    const Ch* units_str),
		   (nelements, allocator, units_str), T*, const);
  template<typename T>
  WRAP_METHOD_SELF(WValue, Set1DArray, (const T* x, SizeType len,
					Allocator& allocator),
		   (x, len, allocator), );
  template<typename T>
  WRAP_METHOD_SELF(WValue, Set1DArray, (const T* x, SizeType len,
					const Ch* units_str,
					Allocator& allocator),
		   (x, len, units_str, allocator), );
  template<typename T>
  WRAP_METHOD_SELF(WValue, Set1DArray, (const T* x, SizeType len,
					const Ch* units_str,
					SizeType units_len,
					Allocator& allocator),
		   (x, len, units_str, units_len, allocator), );
  // NDArray
  template<typename T>
  WRAP_METHOD(WValue, GetNDArray,
	      (T*& data, SizeType*& shape, SizeType& ndim,
	       Allocator& allocator,
	       const UnitsType data_units = UnitsType()),
	      (data, shape, ndim, allocator, data_units), void, const);
  template<typename T>
  WRAP_METHOD(WValue, GetNDArray,
	      (T*& data, SizeType*& shape, SizeType& ndim,
	       Allocator& allocator, const Ch* units_str),
	      (data, shape, ndim, allocator, units_str), void, const); 
  WRAP_METHOD_TEMP(WValue, GetNDArray,
		   (SizeType*& shape, SizeType& ndim,
		    Allocator& allocator, const Ch* units_str),
		   (shape, ndim, allocator, units_str), T*, const);
  WRAP_METHOD_TEMP(WValue, GetNDArray,
		   (SizeType*& shape, SizeType& ndim,
		    Allocator& allocator,
		    const UnitsType data_units = UnitsType()),
		   (shape, ndim, allocator, data_units), T*, const);
  template<typename T>
  WRAP_METHOD_SELF(WValue, SetNDArray,
		   (const T* x, SizeType shape[],
		    SizeType ndim, Allocator& allocator),
		   (x, shape, ndim, allocator), );
  template<typename T>
  WRAP_METHOD_SELF(WValue, SetNDArray,
		   (const T* x, SizeType shape[],
		    SizeType ndim, const Ch* units_str,
		    Allocator& allocator),
		   (x, shape, ndim, units_str, allocator), );
  template<typename T>
  WRAP_METHOD_SELF(WValue, SetNDArray,
		   (const T* x, SizeType shape[],
		    SizeType ndim, const Ch* units_str,
		    SizeType units_len, Allocator& allocator),
		   (x, shape, ndim, units_str, units_len, allocator), );
  // Array methods
  WRAP_METHOD(WValue, IsArray, (), (), bool, const);
  WRAP_METHOD_SELF(WValue, SetArray, (), (), );
  WRAP_METHOD(WValue, Size, (), (), rapidjson::SizeType, const);
  WRAP_METHOD(WValue, Empty, (), (), bool, const);
  WRAP_METHOD_SELF(WValue, Reserve, (SizeType newCapacity,
				     Allocator &allocator),
		   (newCapacity, allocator), );
  WRAP_METHOD_ITER(WValue, Erase, (const WValue& pos), (pos.val_), );
  WRAP_METHOD_ITER(WValue, Begin, (), (), );
  WRAP_METHOD_ITER(WValue, End, (), (), );
  WRAP_METHOD_ITER(WValue, Begin, (), (), const);
  WRAP_METHOD_ITER(WValue, End, (), (), const);
  INDEX_RTYPE operator[](SizeType index);
  const INDEX_RTYPE operator[](SizeType index) const;
  // Object methods
  WRAP_METHOD(WValue, IsObject, (), (), bool, const);
  WRAP_METHOD_SELF(WValue, SetObject, (), (), );
  WRAP_METHOD(WValue, MemberCount, (), (), rapidjson::SizeType, const);
  WRAP_METHOD(WValue, HasMember, (const Ch* name), (name), bool, const);
  WRAP_METHOD(WValue, HasMember, (const WValue& name), (*(name.val_)),
	      bool, const);
  template <typename T>
  RAPIDJSON_DISABLEIF_RETURN(
    (internal::NotExpr<
     internal::IsSame<typename internal::RemoveConst<T>::Type, Ch> >),
    (INDEX_RTYPE)) operator[](T* name);
  template <typename T>
  RAPIDJSON_DISABLEIF_RETURN(
    (internal::NotExpr<
     internal::IsSame<typename internal::RemoveConst<T>::Type, Ch> >),
    (const INDEX_RTYPE)) operator[](T* name) const;
  WRAP_METHOD_SELF(WValue, AddMember, (WValue& name,
				       WValue& value,
				       WValue::Allocator& allocator),
		   (*(name.val_), *(value.val_), allocator), );
  WRAP_METHOD_SELF(WValue, AddMember, (WValue& name,
				       WValue&& value,
				       WValue::Allocator& allocator),
		   (*(name.val_), *(value.val_), allocator), );
  WRAP_METHOD_SELF(WValue, AddMember, (WValue&& name,
				       WValue& value,
				       WValue::Allocator& allocator),
		   (*(name.val_), *(value.val_), allocator), );
  WRAP_METHOD_SELF(WValue, AddMember, (WValue&& name,
				       WValue&& value,
				       WValue::Allocator& allocator),
		   (*(name.val_), *(value.val_), allocator), );
  WRAP_METHOD_SELF(WValue, AddMember, (const WValue& name,
				       WValue& value,
				       WValue::Allocator& allocator),
		   (*(name.val_), *(value.val_), allocator), );
  WRAP_METHOD_SELF(WValue, AddMember, (WStringRefType name,
				       WValue& value,
				       WValue::Allocator& allocator),
		   (*(name.val_), *(value.val_), allocator), );
  WRAP_METHOD_SELF(WValue, AddMember, (WStringRefType name,
				       WValue&& value,
				       WValue::Allocator& allocator),
		   (*(name.val_), *(value.val_), allocator), );
  WRAP_METHOD(WValue, RemoveMember, (const Ch* name), (name), bool, );
  WRAP_METHOD_CAST(WValue, MemberBegin, (), (),
		   MemberIterator, );
  WRAP_METHOD_CAST(WValue, MemberEnd, (), (),
		   MemberIterator, );
  WRAP_METHOD_CAST(WValue, MemberBegin, (), (),
		   ConstMemberIterator, const);
  WRAP_METHOD_CAST(WValue, MemberEnd, (), (),
		   ConstMemberIterator, const);
  WRAP_METHOD_CAST(WValue, FindMember, (const Ch* name), (name),
		   MemberIterator, );
  WRAP_METHOD_CAST(WValue, FindMember, (const Ch* name), (name),
		   ConstMemberIterator, const);
  WRAP_METHOD_CAST(WValue, FindMember, (const WValue& name),
		   (*(name.val_)), MemberIterator, );
  WRAP_METHOD_CAST(WValue, FindMember, (const WValue& name),
		   (*(name.val_)), ConstMemberIterator, const);
  // Python methods
  WRAP_METHOD(WValue, IsPythonClass, (), (), bool, const);
  WRAP_METHOD(WValue, IsPythonInstance, (), (), bool, const);
  WRAP_METHOD(WValue, IsPythonFunction, (), (), bool, const);
  WRAP_METHOD(WValue, GetPythonObjectRaw, (), (), PyObject*, const);
  WRAP_METHOD(WValue, SetPythonObjectRaw,
	      (PyObject* x, Allocator& allocator,
	       bool skipTitle=false, bool allowPickle=true),
	      (x, allocator, skipTitle, allowPickle), bool, );
  // Geometry methods
  WRAP_METHOD(WValue, IsObjWavefront, (), (), bool, const);
  WRAP_METHOD(WValue, IsPly, (), (), bool, const);
  WRAP_METHOD(WValue, GetObjWavefront, (), (),
	      ObjWavefront, const);
  WRAP_METHOD(WValue, GetObjWavefront,
	      (ObjWavefront& x), (x), void, const);
  WRAP_METHOD(WValue, GetPly, (), (), Ply, const);
  WRAP_METHOD(WValue, GetPly, (Ply& x), (x), void, const);
  WRAP_METHOD_SELF(WValue, SetObjWavefront,
		   (ObjWavefront x,
		    Allocator& allocator),
		   (x, allocator), );
  WRAP_METHOD_SELF(WValue, SetObj,
		   (ObjWavefront x,
		    Allocator& allocator),
		   (x, allocator), );
  WRAP_METHOD_SELF(WValue, SetPly,
		   (Ply x, Allocator& allocator),
		   (x, allocator), );
		   
  // Yggdrasil methods
  WRAP_METHOD(WValue, IsType, (const Ch* type), (type), bool, const);
  WRAP_METHOD(WValue, GetDataPtr, (bool& requires_freeing),
	      (requires_freeing), void*, const);
  WRAP_METHOD(WValue, GetNBytes, (), (), SizeType, const);
  WRAP_METHOD(WValue, SetDataPtr,
	      (const Ch* type, void*& value,
	       WValue::Allocator& allocator),
	      (type, value, allocator), bool, );
  WRAP_METHOD(WValue, IsSubType,
	      (const Ch* subtype, SizeType precision),
	      (subtype, precision), bool, const);
  WRAP_METHOD_SELF(WValue, SetYggdrasilString,
		   (const Ch* s, SizeType length,
		    Allocator& allocator, const WValue& schema),
		   (s, length, allocator, *(schema.val_)), );
  // Strings
  WRAP_GET_STRING(Type);
  WRAP_GET_STRING(Object);
  WRAP_GET_STRING(Array);
  WRAP_GET_STRING(Properties);
  WRAP_GET_STRING(Items);
  WRAP_GET_STRING(Null);
  WRAP_GET_STRING(Boolean);
  WRAP_GET_STRING(Number);
  WRAP_GET_STRING(Integer);
  WRAP_GET_STRING(String);
  WRAP_GET_STRING(Scalar);
  WRAP_GET_STRING(1DArray);
  WRAP_GET_STRING(NDArray);
  WRAP_GET_STRING(PythonClass);
  WRAP_GET_STRING(PythonFunction);
  WRAP_GET_STRING(PythonInstance);
  WRAP_GET_STRING(Obj);
  WRAP_GET_STRING(ObjWavefront);
  WRAP_GET_STRING(Ply);
  WRAP_GET_STRING(Schema);
  WRAP_GET_STRING(Any);
  WRAP_GET_STRING(Title);
  WRAP_GET_STRING(SubType);
  WRAP_GET_STRING(Precision);
  WRAP_GET_STRING(Units);
  WRAP_GET_STRING(Length);
  WRAP_GET_STRING(Shape);
  WRAP_GET_STRING(Args);
  WRAP_GET_STRING(Kwargs);
  WRAP_GET_STRING(Encoding);
  WRAP_GET_STRING(ASCIIEncoding);
  WRAP_GET_STRING(UCS4Encoding);
  WRAP_GET_STRING(UTF8Encoding);
  WRAP_GET_STRING(UTF16Encoding);
  WRAP_GET_STRING(UTF32Encoding);
  WRAP_GET_STRING(IntSubType);
  WRAP_GET_STRING(UintSubType);
  WRAP_GET_STRING(FloatSubType);
  WRAP_GET_STRING(ComplexSubType);
  WRAP_GET_STRING(StringSubType);
  WRAP_GET_STRING(NullSubType);

  friend std::ostream & operator << (std::ostream &out,
				     const WValue& p);
};

////////////////////////////////////////////////////////////////////
// WDocument
////////////////////////////////////////////////////////////////////

class WDocument : public WValue {
private:
  WDocument(const WDocument&) = delete;
  WDocument& operator=(const WDocument&) = delete;
public:
  typedef RAPIDJSON_DEFAULT_ALLOCATOR Allocator;
  typedef RAPIDJSON_DEFAULT_ALLOCATOR AllocatorType;
  typedef WValue ValueType;
  typedef RJ_WNS::Document BaseType;
  WDocument(RJ_WNS::Document* doc);
  WDocument(RJ_WNS::Document& doc);
  WDocument(RJ_WNS::Document&& doc);
  WDocument(Type type=rapidjson::kNullType);
#if RAPIDJSON_HAS_CXX11_RVALUE_REFS
  WDocument(WDocument&& rhs);
  WDocument& operator=(WDocument&& rhs);
#endif // RAPIDJSON_HAS_CXX11_RVALUE_REFS
  WDocument& operator=(WDocument& rhs);
  WDocument& Move() { return *this; }

  using ValueType::CopyFrom;
  using ValueType::Swap;
  
  WRAP_METHOD_SELF_CAST(WDocument, CopyFrom,
			(const WDocument& rhs,
			 WDocument::Allocator& allocator,
			 bool copyConstStrings = false),
			(*(rhs.val_), allocator, copyConstStrings),
			WValue, );
  WRAP_METHOD_SELF_CAST(WDocument, Swap, (WDocument& rhs), (*(rhs.val_)),
			WValue, );
  WRAP_METHOD(WDocument, GetAllocator, (), (), Allocator&, );
  WRAP_METHOD(WDocument, Normalize, (const WValue& schema,
			   StringBuffer* error=NULL),
	      (*(schema.val_), error), bool, );
  WRAP_METHOD_SELF(WDocument, Parse, (const Ch* str, size_t length),
		   (str, length), );
  WRAP_METHOD_SELF(WDocument, Parse, (const Ch* str), (str), );
  WRAP_METHOD(WDocument, HasParseError, (), (), bool, const);
  WRAP_METHOD(WDocument, CountVarArgs, (WValue& schema, bool set),
	      (*(schema.val_), set), size_t, const);
  WRAP_METHOD(WDocument, SetVarArgs, (WValue& schema, VarArgList& ap),
	      (*(schema.val_), ap), bool, const);
  WRAP_METHOD(WDocument, GetVarArgs, (WValue& schema, VarArgList& ap),
	      (*(schema.val_), ap), bool, );
  WRAP_METHOD(WDocument, FinalizeFromStack, (), (), void, );
  template<typename InputStream>
  WRAP_METHOD_SELF(WDocument, ParseStream, (InputStream& is), (is), );
  WRAP_HANDLER_METHODS(WDocument);
};

////////////////////////////////////////////////////////////////////
// WMember
////////////////////////////////////////////////////////////////////

#define MEMBER							\
  RJ_WNS::GenericMember<UTF8<>, RAPIDJSON_DEFAULT_ALLOCATOR>

class WMember {
private:
  WMember(const WMember&) = delete;
public:
  typedef MEMBER BaseType;
  WValue name;
  WValue value;
  WMember(MEMBER& rhs);
  WMember(MEMBER&& rhs);
  WMember(WMember&& rhs);
  WMember& operator=(WMember&& rhs);
  WMember& operator=(WMember& rhs);
  const WMember* operator->() const;
};

#undef MEMBER

////////////////////////////////////////////////////////////////////
// WGenericMemberIterator
////////////////////////////////////////////////////////////////////

#define MEMBER_ITERATOR(C)						\
  RJ_WNS::GenericMemberIterator<C, UTF8<>, RAPIDJSON_DEFAULT_ALLOCATOR>

template<bool Const>
class WGenericMemberIterator : public WrapperBase<MEMBER_ITERATOR(Const)> {
  WRAPPER_METHODS_(WGenericMemberIterator, MEMBER_ITERATOR(Const));
  WRAPPER_METHODS_EMPTY_CONSTRUCTOR(WGenericMemberIterator);
  WRAPPER_METHODS_MOVE_CONSTRUCTOR(WGenericMemberIterator,
				   MEMBER_ITERATOR(Const));
  friend class WValue;
  template <bool> friend class WGenericMemberIterator;

  typedef WMember PlainType;
  typedef typename internal::MaybeAddConst<Const,PlainType>::Type ValueType;
  
  typedef WGenericMemberIterator Iterator;
  typedef WGenericMemberIterator<true> ConstIterator;
  typedef WGenericMemberIterator<false> NonConstIterator;
  typedef MEMBER_ITERATOR(true) ConstBaseType;
  typedef MEMBER_ITERATOR(false) NonConstBaseType;

  typedef ValueType      value_type;
  typedef ValueType *    pointer;
  typedef ValueType &    reference;
  typedef std::ptrdiff_t difference_type;

  typedef pointer         Pointer;
  typedef reference       Reference;
  typedef difference_type DifferenceType;
  
  WRAP_CONSTRUCTOR(WGenericMemberIterator,
		   (const NonConstIterator& it), (*(it.val_)));
  WRAP_CONSTRUCTOR(WGenericMemberIterator,
		   (const NonConstBaseType&& it), (&it));
  WRAP_METHOD_CAST(WGenericMemberIterator, operator*, (), (),
		   const WMember, const);
  WRAP_METHOD_CAST(WGenericMemberIterator, operator->, (), (),
		   const WMember, const);
  
  WRAP_METHOD_SELF(WGenericMemberIterator, operator=,
		   (const NonConstIterator& it), (it.val_), );
  WRAP_METHOD_SELF(WGenericMemberIterator, operator++, (), (), );
  WRAP_METHOD_SELF(WGenericMemberIterator, operator--, (), (), );
  WRAP_METHOD(WGenericMemberIterator, operator++,
	      (int x), (x), Iterator, );
  WRAP_METHOD(WGenericMemberIterator, operator--,
	      (int x), (x), Iterator, );
  WRAP_METHOD_CAST(WGenericMemberIterator, operator+,
		   (DifferenceType n), (n),
		   WGenericMemberIterator, const);
  WRAP_METHOD_CAST(WGenericMemberIterator, operator-,
		   (DifferenceType n), (n),
		   WGenericMemberIterator, const);

#define ITERATOR_COMP_OP(op)						\
  template<bool Const1, bool Const2>					\
  friend bool operator op(const WGenericMemberIterator<Const1>& lhs,	\
			  const WGenericMemberIterator<Const2>& rhs)
  ITERATOR_COMP_OP(==);
  ITERATOR_COMP_OP(!=);
  ITERATOR_COMP_OP(<=);
  ITERATOR_COMP_OP(>=);
  ITERATOR_COMP_OP(< );
  ITERATOR_COMP_OP(> );
#undef ITERATOR_COMP_OP
  
};

#define ITERATOR_COMP_OP_(op, C1, C2)				  \
  template<>							  \
  bool operator op<C1, C2>(const WGenericMemberIterator<C1>& lhs, \
			   const WGenericMemberIterator<C2>& rhs)

#define ITERATOR_COMP_OP(op)					\
  template<bool Const1, bool Const2>				\
  bool operator op(const WGenericMemberIterator<Const1>& lhs,	\
		   const WGenericMemberIterator<Const2>& rhs);	\
  ITERATOR_COMP_OP_(op, true, true);				\
  ITERATOR_COMP_OP_(op, false, false);				\
  ITERATOR_COMP_OP_(op, true, false);				\
  ITERATOR_COMP_OP_(op, false, true)
  
ITERATOR_COMP_OP(==);
ITERATOR_COMP_OP(!=);
ITERATOR_COMP_OP(<=);
ITERATOR_COMP_OP(>=);
ITERATOR_COMP_OP(< );
ITERATOR_COMP_OP(> );
#undef ITERATOR_COMP_OP
#undef ITERATOR_COMP_OP_

#undef MEMBER_ITERATOR

typedef WGenericMemberIterator<false> WMemberIterator;
typedef WGenericMemberIterator<true> WConstMemberIterator;

////////////////////////////////////////////////////////////////////
// WSchemaDocument
////////////////////////////////////////////////////////////////////

class WRAPPER_CLASS(SchemaDocument) {
  WRAPPER_METHODS_NO_EMPTY_CONSTRUCTOR(SchemaDocument);
  WRAP_CONSTRUCTOR(WSchemaDocument, (WDocument& d),
		   (*((RJ_WNS::Document*)d.val_)));
  WRAP_CONSTRUCTOR(WSchemaDocument, (WValue& d), (*(d.val_)));
};

////////////////////////////////////////////////////////////////////
// WSchemaValidator
////////////////////////////////////////////////////////////////////

class WRAPPER_CLASS(SchemaValidator) {
  WRAPPER_METHODS_NO_EMPTY_CONSTRUCTOR(SchemaValidator);
  typedef RAPIDJSON_DEFAULT_ALLOCATOR Allocator;
  typedef RAPIDJSON_DEFAULT_ALLOCATOR AllocatorType;
  WRAP_CONSTRUCTOR(WSchemaValidator, (WSchemaDocument& s),
		   (*((RJ_WNS::SchemaDocument*)s.val_)));
  WRAP_METHOD(WSchemaValidator, GenerateData, (WDocument& d),
	      (*((RJ_WNS::Document*)d.val_)), bool, );
};

////////////////////////////////////////////////////////////////////
// WSchemaNormalizer
////////////////////////////////////////////////////////////////////

class WRAPPER_CLASS(SchemaNormalizer) {
  WRAPPER_METHODS_NO_EMPTY_CONSTRUCTOR(SchemaNormalizer);
  typedef RAPIDJSON_DEFAULT_ALLOCATOR Allocator;
  typedef RAPIDJSON_DEFAULT_ALLOCATOR AllocatorType;
  WRAP_CONSTRUCTOR(WSchemaNormalizer, (WSchemaDocument& s),
		   (*((RJ_WNS::SchemaDocument*)s.val_)));
  WRAP_METHOD(WSchemaNormalizer, GenerateData, (WDocument& d),
	      (*((RJ_WNS::Document*)d.val_)), bool, );
  WRAP_METHOD(WSchemaNormalizer, Compare, (const WValue& d),
	      (*((const RJ_WNS::Value*)d.val_)), bool, );
  WRAP_METHOD(WSchemaNormalizer, GetErrorMsg,
	      (WValue& err, Allocator& allocator),
	      (*(err.val_), allocator), bool, const);
};

////////////////////////////////////////////////////////////////////
// WSchemaEncoder
////////////////////////////////////////////////////////////////////

class WRAPPER_CLASS(SchemaEncoder) {
  WRAPPER_METHODS_NO_EMPTY_CONSTRUCTOR(SchemaEncoder);
  typedef char Ch;
  typedef RAPIDJSON_DEFAULT_ALLOCATOR Allocator;
  typedef RAPIDJSON_DEFAULT_ALLOCATOR AllocatorType;
  typedef RAPIDJSON_DEFAULT_STACK_ALLOCATOR StackAllocator;
  WRAP_CONSTRUCTOR(WSchemaEncoder,
		   (bool minimal = false,
		    Allocator* allocator = 0,
		    StackAllocator* stackAllocator = 0,
		    size_t stackCapacity = 1024),
		   (minimal, allocator, stackAllocator, stackCapacity));
  WRAP_HANDLER_METHODS(WSchemaEncoder);
  WRAP_METHOD_CAST(WSchemaEncoder, GetSchema, (), (), WDocument, );
};

////////////////////////////////////////////////////////////////////
// WPrettyWriter
////////////////////////////////////////////////////////////////////

enum PrettyFormatOptions {
  kFormatDefault = 0,         //!< Default pretty formatting.
  kFormatSingleLineArray = 1  //!< Format arrays on a single line.
};

#define WRAPPED_WRITER						\
  RJ_WNS::PrettyWriter<OutputStream, SourceEncoding,		\
		       TargetEncoding, StackAllocator,		\
		       writeFlags>

template<typename OutputStream, typename SourceEncoding = UTF8<>,
	 typename TargetEncoding = UTF8<>,
	 typename StackAllocator = CrtAllocator,
	 unsigned writeFlags = kWriteDefaultFlags>
class WPrettyWriter : public WrapperBase<WRAPPED_WRITER> {
  WRAPPER_METHODS_(WPrettyWriter, WRAPPED_WRITER);
  // typedef CrtAllocator StackAllocator;
  typedef Writer<OutputStream, SourceEncoding, TargetEncoding,
		 StackAllocator, writeFlags> Base;
  typedef typename SourceEncoding::Ch Ch;
  WRAPPER_METHODS_MOVE_CONSTRUCTOR(WPrettyWriter, WRAPPED_WRITER);
  WRAP_CONSTRUCTOR(WPrettyWriter,
		   (OutputStream& os, StackAllocator* allocator = 0,
		    size_t levelDepth = Base::kDefaultLevelDepth),
		   (os, allocator, levelDepth));
  WRAP_CONSTRUCTOR(WPrettyWriter,
		   (StackAllocator* allocator = 0,
		    size_t levelDepth = Base::kDefaultLevelDepth),
		   (allocator, levelDepth));
  WRAP_METHOD_SELF(WPrettyWriter, SetIndent,
		   (Ch indentChar, unsigned indentCharCount),
		   (indentChar, indentCharCount), );
  WRAP_METHOD_SELF(WPrettyWriter, SetFormatOptions,
		   (PrettyFormatOptions options), (options), );
  WRAP_HANDLER_METHODS(WPrettyWriter);
  WRAP_METHOD(WPrettyWriter, SetYggdrasilMode,
	      (bool readable), (readable), void, );
};

#undef WRAPPED_WRITER

// Cleanup macros

#undef WRAP_CONSTRUCTOR
#undef WRAP_METHOD
#undef WRAP_METHOD_TEMP
#undef WRAP_METHOD_SELF
#undef WRAP_METHOD_SELF_CAST
#undef WRAP_METHOD_CAST
#undef WRAP_METHOD_CAST_CONST
#undef WRAP_SET_GET
#undef WRAP_METHOD_ITER
#undef WRAP_GET_STRING
#undef WRAPPER_CLASS
#undef WRAPPER_BASE
#undef WRAPPER_METHODS_
#undef WRAPPER_METHODS_OPS_
#undef WRAPPER_METHODS_PRIV_
#undef WRAPPER_METHODS_EMPTY_CONSTRUCTOR
#undef WRAPPER_METHODS_MOVE_CONSTRUCTOR
#undef WRAPPER_METHODS
#undef WRAPPER_METHODS_NO_OP
#undef WRAPPER_METHODS_NO_EMPTY_CONSTRUCTOR
#undef WRAP_HANDLER_METHOD
#undef WRAP_HANDLER_METHODS

RAPIDJSON_NAMESPACE_END

#else // WRAP_RAPIDJSON_FOR_DLL

// #define RJ_WNS RAPIDJSON_NAMESPACE

#include "rapidjson/document.h"
#include "rapidjson/prettywriter.h"
#include "rapidjson/schema.h"

// RAPIDJSON_NAMESPACE_BEGIN

// typedef Value WValue;
// typedef Document WDocument;

// RAPIDJSON_NAMESPACE_END

#endif // WRAP_RAPIDJSON_FOR_DLL

namespace communication {
  namespace utils {
    typedef bool (*filterFunc)(const rapidjson::Document&);
    typedef bool (*transformFunc)(rapidjson::Document&);

    /*!
      @brief Determine if numpy arrays are enabled.
    */
    bool numpy_arrays_imported();

  }
}
