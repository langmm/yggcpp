#pragma once

#if defined(RESTINSTALLED) && defined(__MINGW32__)
// Ensure winsock2.h is included before windows.h included by curl
#include <winsock2.h>
#endif

#include "YggInterface_export.h"

#define YGGDRASIL_PYGIL_BEGIN YggInterface::utils::global_PyGILState();
#define YGGDRASIL_PYGIL_END YggInterface::utils::global_PyGILState(true);
#define YGGDRASIL_PYGIL_ALLOW_THREADS_BEGIN YggInterface::utils::global_PyThreadState();
#define YGGDRASIL_PYGIL_ALLOW_THREADS_END YggInterface::utils::global_PyThreadState(true);

#include <vector>
#include <string>
#include <iostream>
#include "logging.hpp"
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
#include "rapidjson/error/en.h"

#define WStringRefType StringRefType
#define WValue Value
#define WDocument Document
#define WSchemaDocument SchemaDocument
#define WSchemaValidator SchemaValidator
#define WSchemaNormalizer SchemaNormalizer
#define WSchemaEncoder SchemaEncoder
#define WPrettyWriter PrettyWriter

#ifdef WRAP_RAPIDJSON_FOR_DLL

#define RJ_WNS wrap::RAPIDJSON_NAMESPACE

#include <ostream> // required for ostream

#ifdef _DEBUG
#undef _DEBUG
#include <Python.h>
#define _DEBUG
#else
#include <Python.h>
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
 * @brief Acquire/release the Python GIL.
 * @param[in] release If true, the Python GIL will be released by the
 *   current thread if it has already been acquired. If false, the GIL
 *   will be acquired by the current thread if it has not already been
 *   acquired.
 * @returns true if successful, false otherwise.
 */
bool global_PyGILState(bool release = false);

/*!
 * @brief Save/restore the global Python thread state.
 * @param[in] restore If true, the global thread state will be restored
 *   if one has been saved. If false, the global thread state will be
 *   saved if one does not already exist.
 * @returns true if successful, false otherwise.
 */
bool global_PyThreadState(bool restore = false);

/*!
  @brief Import the numpy C API.
  @return error message
 */
std::string init_numpy_API();
/*!
  @brief Initialize the Python interpreter.
  @param[in] error_prefix Prefix to add to error message describine
    the context from which the function was called.
  @param[in] dont_raise If true, the error will not be thrown.
 */
std::string initialize_python(const std::string error_prefix="",
			      bool dont_raise=false);

/*!
  @brief Finalize the Python interpreter.
  @param[in] error_prefix Prefix to add to error message describine
    the context from which the function was called.
 */
void finalize_python(const std::string error_prefix="");

/*!
  @brief Import a Python function or class.
  @param[in] module_name Name of the module containing the desired
    function or class.
  @param[in] class_name Name of desired Python function or class.
  @param[in] error_prefix Prefix to add to error message describine
    the context from which the function was called.
  @param[in] ignore_error If true, no error will be thrown and NULL will
    be returned.
  @return The Python function or class. NULL indicates an error.
 */
PyObject* import_python_class(const char* module_name,
			      const char* class_name,
			      const std::string error_prefix="",
			      const bool ignore_error=false);

/*!
  @brief Import a Python function or class.
  @param[in] module_class String containing the name of the module
    a function or class should be imported from and the name of the
    function or class.
  @param[in] error_prefix Prefix to add to error message describine
    the context from which the function was called.
  @param[in] ignore_error If true, no error will be thrown and NULL will
    be returned.
  @return The Python function or class. NULL indicates an error.
 */
PyObject* import_python_object(const char* module_class,
			       const std::string error_prefix="",
			       const bool ignore_error=false);

// Forward declarations
class WValue;
class WDocument;
class WMember;
template<typename T, bool Const>
class IteratorWrapperBase;
typedef IteratorWrapperBase<WValue, false> WValueIterator;
typedef IteratorWrapperBase<WValue, true> WConstValueIterator;
typedef IteratorWrapperBase<WMember, false> WMemberIterator;
typedef IteratorWrapperBase<WMember, true> WConstMemberIterator;

#define MEMBER							\
  RJ_WNS::GenericMember<UTF8<>, RAPIDJSON_DEFAULT_ALLOCATOR>
#define MEMBER_ITERATOR(C)						\
  RJ_WNS::GenericMemberIterator<C, UTF8<>, RAPIDJSON_DEFAULT_ALLOCATOR>
#define WRAP_MEMBER_ITERATOR(x)				\
  val_->MemberBegin() + (const_cast<RJ_WNS::Value::Member*>(x.ptr_) - &(*(val_->MemberBegin())))
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
#define WRAP_METHOD_CAST_VITER(cls, name, argsT, args, type, mods)	\
  WRAP_METHOD(cls, name, argsT, args, type, mods)
#define WRAP_METHOD_CAST_MITER(cls, name, argsT, args, type, mods)	\
  WRAP_METHOD(cls, name, argsT, args, type, mods)
#define WRAP_METHOD_CAST_CONST(cls, name, argsT, args, type, mods)	\
  WRAP_METHOD(cls, name, argsT, args, const type, const mods)
#define WRAP_SET_GET(name, type)					\
  WRAP_CONSTRUCTOR(explicit WValue, (type x), (x));			\
  WRAP_METHOD(WValue, Get ## name, (), (), type, const);		\
  WRAP_METHOD_SELF(WValue, Set ## name, (type x), (x), );		\
  WRAP_METHOD(WValue, Is ## name, (), (), bool, const)
#define WRAP_GET_STRING(name)			\
  static const WValue Get ## name ## String();
#define WRAP_STATIC(cls, name, argsT, args, type, mods)	\
  static WRAP_METHOD(cls, name, argsT, args, type, mods)
#define WRAP_STATIC_CAST(cls, name, argsT, args, type, mods)	\
  static WRAP_METHOD(cls, name, argsT, args, type, mods)
#define WRAP_STATIC_CAST_CONST(cls, name, argsT, args, type, mods)	\
  static WRAP_METHOD(cls, name, argsT, args, const type, mods)


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
  WrapperBase(T* val, bool created=false) :
    val_(val), created_val(created) {}
  WrapperBase(T& val) :
    WrapperBase(&val) {}
  WrapperBase(WrapperBase<T>&& rhs) :
    WrapperBase(nullptr) {
    std::swap(val_, rhs.val_);
    std::swap(created_val, rhs.created_val);
  }
  ~WrapperBase();
  WrapperBase<T>& operator=(WrapperBase<T>& rhs) {
    std::swap(val_, rhs.val_);
    std::swap(created_val, rhs.created_val);
    return *this;
  }
  T* val_;
  bool created_val;
};

#define WRAPPER_CLASS(base)			\
  W ## base : public WrapperBase<RJ_WNS::base>
#define WRAPPER_METHODS_MOVE_CONSTRUCTOR(cls, base)	\
  cls(base&& val)
#define WRAPPER_METHODS_STRICT(cls, base)	\
 cls(base* val);				\
 cls(base& val);				\
 cls(cls&& rhs);				\
 cls& operator=(cls& rhs)
#define WRAPPER_METHODS_BASE(cls, base)		\
public:						\
 typedef base BaseType;				\
 cls& operator=(cls&& rhs);			\
 cls& Move()
#define WRAPPER_METHODS(base)				\
  WRAPPER_METHODS_BASE(W ## base, RJ_WNS::base);	\
  WRAPPER_METHODS_STRICT(W ## base, RJ_WNS::base)

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
  WRAPPER_METHODS(StringRefType);
  typedef char Ch;
  typedef char CharType;
  WRAP_CONSTRUCTOR(explicit WStringRefType, (const CharType* str), (str));
  WRAP_CONSTRUCTOR(WStringRefType, (const CharType* str, SizeType len),
		   (str, len));
  WRAP_CONSTRUCTOR(WStringRefType, (const WStringRefType& rhs),
		   (*(rhs.val_)));
  template<SizeType N>
  WStringRefType(const CharType (&str)[N]) :
    WStringRefType(&(str[0]), N-1) {}
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
private:
  WValue(const WValue&) = delete;
  WValue& operator=(const WValue&) = delete;
public:
  typedef RAPIDJSON_DEFAULT_ALLOCATOR Allocator;
  typedef RAPIDJSON_DEFAULT_ALLOCATOR AllocatorType;
  typedef WValue ValueType;
  typedef WValueIterator ValueIterator;
  typedef WConstValueIterator ConstValueIterator;
  typedef WMemberIterator MemberIterator;
  typedef WConstMemberIterator ConstMemberIterator;
  typedef char Ch;
  typedef rapidjson::units::Units UnitsType;

  WValue(RJ_WNS::Value* x, WValue* parent=nullptr) :
    WrapperBase(x), parent_(parent), vrefs(), mrefs() {}
  WValue(RJ_WNS::Value& x, WValue* parent=nullptr) :
    WrapperBase(x), parent_(parent), vrefs(), mrefs() {}
  WValue(WValue&& rhs) :
    WrapperBase(std::forward<WrapperBase<RJ_WNS::Value> >(rhs)),
    parent_(nullptr),
    vrefs(), mrefs() {
    std::swap(parent_, rhs.parent_);
    std::swap(vrefs, rhs.vrefs);
    std::swap(mrefs, rhs.mrefs);
  }
  WValue& operator=(WValue& rhs);
  ~WValue();
  WValue* parent_;
  std::vector<WValue*> vrefs;
  std::vector<WMember*> mrefs;
  
 public:
  bool operator==(const WValue& rhs) const;
  bool operator!=(const WValue& rhs) const;

  WValue& childRef(RJ_WNS::Value* x);
  WMember& childRef(MEMBER* x);
  WValue* _getPtr();
  const WValue* _getPtr() const;
  WValue* operator & ();
  const WValue* operator & () const;
  
  // operator RJ_WNS::Value& () { return *val_; }
  // operator const RJ_WNS::Value& () const { return *val_; }
  WValue& CopyInto(WValue& rhs, Allocator& allocator,
		   bool copyConstStrings = false) const;
  RJ_WNS::Value& CopyInto(RJ_WNS::Value& rhs, Allocator& allocator,
			  bool copyConstStrings = false) const;
  WValue& CopyFrom(const RJ_WNS::Value& rhs, Allocator& allocator,
		   bool copyConstStrings = false);

  WRAPPER_METHODS_BASE(WValue, RJ_WNS::Value);
  
  // Static methods
  WRAP_STATIC_CAST_CONST(WValue, YggSubTypeString,
			 (enum YggSubType subtype),
			 (subtype), WValue, );

  WRAP_CONSTRUCTOR(explicit WValue, (Type type=kNullType), (type));
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
  WRAP_METHOD(WValue, GetType, (), (), rapidjson::Type, const);
  WRAP_METHOD_SELF(WValue, SetNull, (), (), );
  WRAP_METHOD(WValue, IsNull, (), (), bool, const);
  WRAP_METHOD(WValue, IsFalse, (), (), bool, const);
  WRAP_METHOD(WValue, IsTrue, (), (), bool, const);
  WRAP_METHOD(WValue, IsLosslessDouble, (), (), bool, const);
  WRAP_METHOD(WValue, IsLosslessFloat, (), (), bool, const);
  WRAP_SET_GET(Bool, bool);
  WRAP_SET_GET(Int, int);
  WRAP_SET_GET(Uint, unsigned);
  WRAP_SET_GET(Int64, int64_t);
  WRAP_SET_GET(Uint64, uint64_t);
  WRAP_SET_GET(Double, double);
  WRAP_SET_GET(Float, float);
  WRAP_METHOD(WValue, IsNumber, (), (), bool, const);
  WRAP_METHOD(WValue, GetNElements, (), (), SizeType, const);
  WRAP_METHOD_CAST_CONST(WValue, GetShape, (), (), WValue, );
  WRAP_METHOD_CAST_CONST(WValue, GetUnits, (), (), WValue, );
  WRAP_METHOD(WValue, GetElement,
	      (const SizeType index, WValue& dst, Allocator& allocator),
	      (index, *(dst.val_), allocator), bool, const);
  WRAP_METHOD(WValue, GetSubArray,
	      (const SizeType index, const SizeType dim,
	       WValue& dst, Allocator& allocator),
	      (index, dim, *(dst.val_), allocator), bool, const);
  // String methods
  WRAP_METHOD(WValue, IsString, (), (), bool, const);
  WRAP_METHOD_SELF(WValue, SetString, (const Ch* s, SizeType length,
				       Allocator& allocator),
		   (s, length, allocator), );
  WRAP_METHOD_SELF(WValue, SetString, (const Ch* s, SizeType length),
		   (s, length), );
  WRAP_METHOD_SELF(WValue, SetString, (WStringRefType s), (*(s.val_)), );
  WRAP_METHOD(WValue, GetString, (), (), const Ch*, const);
  WRAP_METHOD(WValue, GetStringLength, (), (), SizeType, const);
  // Templated methods
  WRAP_METHOD_TEMP(WValue, Is, (), (), bool, const);
  WRAP_METHOD_TEMP(WValue, IsScalar, (), (), bool, const);
  WRAP_METHOD_TEMP(WValue, Is1DArray, (), (), bool, const);
  WRAP_METHOD_TEMP(WValue, IsNDArray, (), (), bool, const);
  WRAP_METHOD_TEMP(WValue, Get, (), (), T, const);
  template<typename T>
  WRAP_METHOD(WValue, Get, (T& data), (data), void, const);
  template<typename T>
  WRAP_METHOD_SELF(WValue, Set, (const T& data, Allocator& allocator),
		   (data, allocator), );
  template<typename T>
  WRAP_METHOD_SELF(WValue, Set, (const T& data), (data), );
  // Scalar methods
  WRAP_METHOD(WValue, IsScalar, (), (), bool, const);
  WRAP_METHOD(WValue, IsScalar, (const Ch* subT), (subT), bool, const);
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
  WRAP_METHOD(WValue, Is1DArray, (), (), bool, const);
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
  WRAP_METHOD(WValue, IsNDArray, (), (), bool, const);
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
  WRAP_METHOD_SELF(WValue, SetNDArray,
		   (const Ch* x, SizeType precision, SizeType shape[],
		    SizeType ndim, Allocator& allocator,
		    const Ch* encoding=NULL, SizeType encoding_len=0),
		   (x, precision, shape, ndim, allocator,
		    encoding, encoding_len), );
  template<typename T, SizeType N>
  WValue& SetNDArray(const T (&x)[N], Allocator& allocator) {
    SizeType shape[1] = { N };
    return SetNDArray(&(x[0]), shape, 1, allocator);
  }
  template<typename T, SizeType M, SizeType N>
  WValue& SetNDArray(const T (&x)[M][N], Allocator& allocator) {
    SizeType shape[2] = { M, N };
    return SetNDArray(&(x[0][0]), shape, 2, allocator);
  }
  template<typename T, SizeType L, SizeType M, SizeType N>
  WValue& SetNDArray(const T (&x)[L][M][N], Allocator& allocator) {
    SizeType shape[3] = { L, M, N };
    return SetNDArray(&(x[0][0][0]), shape, 3, allocator);
  }
  // Array methods
  WRAP_METHOD(WValue, IsArray, (), (), bool, const);
  WRAP_METHOD_SELF(WValue, SetArray, (), (), );
  WRAP_METHOD(WValue, Size, (), (), rapidjson::SizeType, const);
  WRAP_METHOD(WValue, Capacity, (), (), rapidjson::SizeType, const);
  WRAP_METHOD(WValue, Empty, (), (), bool, const);
  WRAP_METHOD_SELF(WValue, Reserve, (SizeType newCapacity,
				     Allocator &allocator),
		   (newCapacity, allocator), );
  WRAP_METHOD(WValue, Clear, (), (), void, );
  WRAP_METHOD_CAST_VITER(WValue, Erase, (ConstValueIterator pos),
			 (pos.val_), ValueIterator, );
  WRAP_METHOD_CAST_VITER(WValue, Begin, (), (),
			 ValueIterator, );
  WRAP_METHOD_CAST_VITER(WValue, End, (), (),
			 ValueIterator, );
  WRAP_METHOD_CAST_VITER(WValue, Begin, (), (),
			 ConstValueIterator, const);
  WRAP_METHOD_CAST_VITER(WValue, End, (), (),
			 ConstValueIterator, const);
  WRAP_METHOD(WValue, Contains, (const WValue& x),
	      (*(x.val_)), bool, const);
  WRAP_METHOD_SELF(WValue, PopBack, (), (), );
  INDEX_RTYPE operator[](SizeType index);
  const INDEX_RTYPE operator[](SizeType index) const;
  // Object methods
  WRAP_METHOD(WValue, IsObject, (), (), bool, const);
  WRAP_METHOD_SELF(WValue, SetObject, (), (), );
  WRAP_METHOD(WValue, MemberCount, (), (), rapidjson::SizeType, const);
  WRAP_METHOD(WValue, MemberCapacity, (), (), rapidjson::SizeType, const);
  WRAP_METHOD(WValue, ObjectEmpty, (), (), bool, const);
#if RAPIDJSON_HAS_STDSTRING
  WRAP_METHOD(WValue, HasMember, (const std::basic_string<Ch>& name), (name), bool, const);
#endif
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
#if RAPIDJSON_HAS_STDSTRING
  WRAP_METHOD(WValue, RemoveMember, (const std::basic_string<Ch>& name),
	      (name), bool, );
#endif
  WRAP_METHOD(WValue, RemoveAllMembers, (), (), void, );
  WRAP_METHOD_CAST_MITER(WValue, MemberBegin, (), (),
			 MemberIterator, );
  WRAP_METHOD_CAST_MITER(WValue, MemberEnd, (), (),
			 MemberIterator, );
  WRAP_METHOD_CAST_MITER(WValue, MemberBegin, (), (),
			 ConstMemberIterator, const);
  WRAP_METHOD_CAST_MITER(WValue, MemberEnd, (), (),
			 ConstMemberIterator, const);
  WRAP_METHOD_CAST_MITER(WValue, FindMember, (const Ch* name),
			 (name), MemberIterator, );
  WRAP_METHOD_CAST_MITER(WValue, FindMember, (const Ch* name),
			 (name), ConstMemberIterator, const);
  WRAP_METHOD_CAST_MITER(WValue, FindMember, (const WValue& name),
			 (*(name.val_)), MemberIterator, );
  WRAP_METHOD_CAST_MITER(WValue, FindMember, (const WValue& name),
			 (*(name.val_)), ConstMemberIterator,
			 const);
#if RAPIDJSON_HAS_STDSTRING
  WRAP_METHOD_CAST_MITER(WValue, FindMember,
			 (const std::basic_string<Ch>& name), (name),
			 WValue::ConstMemberIterator, const);
#endif
  // *const_cast<RJ_WNS::Value::Member*>(x->val_)))
  WRAP_METHOD_CAST_MITER(WValue, EraseMember,
			 (WValue::ConstMemberIterator pos),
			 (WRAP_MEMBER_ITERATOR(pos)),
			 WValue::ConstMemberIterator, );
  WRAP_METHOD_CAST_MITER(WValue, EraseMember,
			 (WValue::ConstMemberIterator first,
			  WValue::ConstMemberIterator last),
			 (WRAP_MEMBER_ITERATOR(first),
			  WRAP_MEMBER_ITERATOR(last)),
			 WValue::ConstMemberIterator, );
  WRAP_METHOD(WValue, EraseMember, (const Ch* name), (name), bool, );
#if RAPIDJSON_HAS_STDSTRING
  WRAP_METHOD(WValue, EraseMember, (const std::basic_string<Ch>& name),
	      (name), bool, );
#endif
  WRAP_METHOD(WValue, EraseMember, (const WValue& name),
	      (*(name.val_)), bool, );
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
  WRAP_METHOD(WValue, IsYggdrasil, (), (), bool, const);
  WRAP_METHOD(WValue, IsSchema, (), (), bool, const);
  WRAP_METHOD_SELF(WValue, SetSchema,
		   (Allocator& allocator),
		   (allocator), );
  WRAP_METHOD_SELF(WValue, SetSchema,
		   (const WValue& x, Allocator& allocator),
		   (*(rhs.val_), allocator), );
  WRAP_METHOD(WValue, RawAssignSchema, (WValue& rhs),
	      (*(rhs.val_)), void, );
  WRAP_METHOD(WValue, DestroySchema, (), (), void, );
  WRAP_METHOD(WValue, HasSchema, (), (), bool, const);
  WRAP_METHOD(WValue, HasSchemaNested, (), (), bool, const);
  WRAP_METHOD(WValue, HasUnits, (), (), bool, const);
  WRAP_METHOD(WValue, HasTitle, (), (), bool, const);
  WRAP_METHOD(WValue, HasPrecision, (), (), bool, const);
  WRAP_METHOD(WValue, HasEncoding, (), (), bool, const);
  WRAP_METHOD_CAST_CONST(WValue, GetTitle, (), (), WValue, );
  WRAP_METHOD(WValue, GetPrecision, (), (), SizeType, const);
  WRAP_METHOD_CAST_CONST(WValue, GetEncoding, (), (), WValue, );
  WRAP_METHOD(WValue, RequiresPython, (), (), bool, const);
  WRAP_METHOD(WValue, IsType, (const Ch* type), (type), bool, const);
  WRAP_METHOD_CAST_CONST(WValue, GetYggType, (), (), WValue, );
  WRAP_METHOD(WValue, GetSubTypeCode, (), (), enum YggSubType, const);
  WRAP_METHOD_CAST_CONST(WValue, GetSubType, (), (), WValue, );
  WRAP_METHOD(WValue, GetSubType, (SizeType &length), (length),
	      const Ch*, const);
  WRAP_METHOD(WValue, GetSubTypeNumpyType, (WValue& enc), (*(enc.val_)),
	      int, const);
  WRAP_METHOD(WValue, SetUnits, (const std::basic_string<Ch> units),
	      (units), bool, );
  WRAP_METHOD(WValue, SetUnits,
	      (const Ch* units_str, const SizeType units_len=0),
	      (units_str, units_len), bool, );
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
  WRAP_METHOD(WValue, AddSchemaMember,
	      (const WValue& key, const WValue& value),
	      (*(key.val_), *(value.val_)), void, );
  WRAP_METHOD(WValue, AddSchemaMember,
	      (const WValue& key, unsigned int value),
	      (*(key.val_), value), void, );
  WRAP_METHOD(WValue, AddSchemaMember,
	      (const WValue& key, const Ch* str, SizeType str_len),
	      (*(key.val_), str, str_len), void, );
					
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
  WDocument* _getPtr() { return this; }
  const WDocument* _getPtr() const { return this; }
  WDocument* operator & () { return this; }
  const WDocument* operator & () const { return this; }
  

  using ValueType::CopyFrom;
  using ValueType::CopyInto;
  using ValueType::Swap;
  
  WRAP_METHOD_SELF_CAST(WDocument, Swap, (WDocument& rhs), (*(rhs.val_)),
			WDocument, );
			// WValue, );
  WRAP_METHOD(WDocument, GetAllocator, (), (), Allocator&, );
  WRAP_METHOD(WDocument, Normalize, (const WValue& schema,
			   StringBuffer* error=NULL),
	      (*(schema.val_), error), bool, );
  WRAP_METHOD_SELF(WDocument, Parse, (const Ch* str, size_t length),
		   (str, length), );
  WRAP_METHOD_SELF(WDocument, Parse, (const Ch* str), (str), );
  WRAP_METHOD_SELF(WDocument, Parse,
		   (const WValue::Ch* str, WDocument& schema),
		   (str, *(schema.val_)), );
  WRAP_METHOD(WDocument, HasParseError, (), (), bool, const);
  WRAP_METHOD(WDocument, GetParseError, (), (), ParseErrorCode, const);
  WRAP_METHOD(WDocument, GetErrorOffset, (), (), size_t, const);
  WRAP_METHOD(WDocument, CountVarArgs, (WValue& schema, bool set),
	      (*(schema.val_), set), size_t, const);
  WRAP_METHOD(WDocument, SetVarArgs, (WValue& schema, VarArgList& ap),
	      (*(schema.val_), ap), bool, const);
  WRAP_METHOD(WDocument, SetVarArgs, (WValue* schema, ...),
	      (*(schema.val_), ap), bool, const);
  WRAP_METHOD(WDocument, SetVarArgsRealloc,
	      (WValue& schema, VarArgList& ap),
	      (*(schema.val_), ap), bool, const);
  WRAP_METHOD(WDocument, SetVarArgsRealloc, (WValue* schema, ...),
	      (*(schema.val_), ap), bool, const);
  WRAP_METHOD(WDocument, GetVarArgs, (WValue& schema, VarArgList& ap),
	      (*(schema.val_), ap), bool, );
  WRAP_METHOD(WDocument, GetVarArgs, (WValue* schema, ...),
	      (*(schema.val_), ap), bool, );
  WRAP_METHOD(WDocument, RawNumber,
	      (const Ch* str, SizeType length, bool copy),
	      (str, length, copy), bool, );
  WRAP_METHOD(WDocument, FromYggdrasilString,
	      (const Ch* str, SizeType length, bool copy),
	      (str, length, copy), bool, );
  WRAP_METHOD(WDocument, WasFinalized, (), (), bool, const);
  WRAP_METHOD(WDocument, ConsolidateStack, (), (), void, );
  WRAP_METHOD(WDocument, FinalizeFromStack, (), (), void, );
  template<typename InputStream>
  WRAP_METHOD_SELF(WDocument, ParseStream, (InputStream& is), (is), );
  WRAP_HANDLER_METHODS(WDocument);
};

////////////////////////////////////////////////////////////////////
// WMember
////////////////////////////////////////////////////////////////////

class WMember {
private:
  WMember(const WMember&) = delete;
public:
  typedef MEMBER BaseType;
  WValue name;
  WValue value;
  BaseType* val_;
  WValue* parent_;
  WMember(BaseType* rhs, WValue* parent=nullptr);
  WMember(WMember&& rhs);
  WMember& operator=(WMember&& rhs);
  WMember& operator=(WMember& rhs);
  WMember* _getPtr() { return this; }
  const WMember* _getPtr() const { return this; }
};

////////////////////////////////////////////////////////////////////
// IteratorWrapperBase
////////////////////////////////////////////////////////////////////

template<typename T, bool Const>
class IteratorWrapperBase {
public:
  typedef T PlainType;
  typedef typename PlainType::BaseType PlainTypeBase;
  typedef typename internal::MaybeAddConst<Const,PlainType>::Type ValueType;
  typedef typename internal::MaybeAddConst<Const,PlainTypeBase>::Type BaseValueType;
  typedef IteratorWrapperBase Iterator;
  typedef IteratorWrapperBase<T, true> ConstIterator;
  typedef IteratorWrapperBase<T, false> NonConstIterator;
  
  typedef ValueType      value_type;
  typedef ValueType *    pointer;
  typedef ValueType &    reference;
  typedef std::ptrdiff_t difference_type;
  typedef std::random_access_iterator_tag iterator_category;

  typedef pointer         Pointer;
  typedef reference       Reference;
  typedef difference_type DifferenceType;

  typedef BaseValueType * BasePointer;
  typedef BaseValueType & BaseReference;

  IteratorWrapperBase() :
    ptr_(nullptr), parent_(nullptr) {}
  
  IteratorWrapperBase(BasePointer p, WValue* parent) :
    ptr_(p), parent_(parent) {}
  IteratorWrapperBase(BaseReference p, WValue* parent) :
    ptr_(&p), parent_(parent) {}
  
  IteratorWrapperBase(const NonConstIterator & it) :
    ptr_(it.ptr_), parent_(it.parent_) {}
  Iterator& operator=(const NonConstIterator & it) {
    ptr_ = it.ptr_;
    parent_ = it.parent_;
    return *this;
  }

#define ITERATOR_INC_OP(op)						\
  IteratorWrapperBase<T, Const>& operator op ## op();			\
  IteratorWrapperBase<T, Const> operator op ## op(int);			\
  IteratorWrapperBase<T, Const> operator op(DifferenceType n) const;	\
  IteratorWrapperBase<T, Const>& operator op ## =(DifferenceType n);
  ITERATOR_INC_OP(+)
  ITERATOR_INC_OP(-)
#undef ITERATOR_INC_OP
  
#define ITERATOR_COMP_OP(op)						\
  template<bool Const1>							\
  bool operator op(const IteratorWrapperBase<T, Const1>& rhs) const {	\
    return ptr_ op rhs.ptr_;						\
  }
  ITERATOR_COMP_OP(==)
  ITERATOR_COMP_OP(!=)
  ITERATOR_COMP_OP(<=)
  ITERATOR_COMP_OP(>=)
  ITERATOR_COMP_OP(<)
  ITERATOR_COMP_OP(>)
#undef ITERATOR_COMP_OP

  Reference operator*() const;
  Pointer   operator->() const;
  Reference operator[](DifferenceType n) const;
  
  BasePointer ptr_;
  WValue* parent_;
  
};

typedef IteratorWrapperBase<WValue, false> WValueIterator;
typedef IteratorWrapperBase<WValue, true> WConstValueIterator;
typedef IteratorWrapperBase<WMember, false> WMemberIterator;
typedef IteratorWrapperBase<WMember, true> WConstMemberIterator;

////////////////////////////////////////////////////////////////////
// WSchemaDocument
////////////////////////////////////////////////////////////////////

class WRAPPER_CLASS(SchemaDocument) {
  WRAPPER_METHODS(SchemaDocument);
  WRAP_CONSTRUCTOR(WSchemaDocument, (WDocument& d),
		   (*((RJ_WNS::Document*)d.val_)));
  WRAP_CONSTRUCTOR(WSchemaDocument, (WValue& d), (*(d.val_)));
};

////////////////////////////////////////////////////////////////////
// WSchemaValidator
////////////////////////////////////////////////////////////////////

class WRAPPER_CLASS(SchemaValidator) {
  WRAPPER_METHODS(SchemaValidator);
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
  WRAPPER_METHODS(SchemaNormalizer);
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
  WRAPPER_METHODS(SchemaEncoder);
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
  WRAPPER_METHODS_BASE(WPrettyWriter, WRAPPED_WRITER);
  WRAPPER_METHODS_STRICT(WPrettyWriter, WRAPPED_WRITER);
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
#undef WRAP_METHOD_CAST_VITER
#undef WRAP_METHOD_CAST_MITER
#undef WRAP_METHOD_CAST_CONST
#undef WRAP_SET_GET
#undef WRAP_STATIC
#undef WRAP_STATIC_CAST
#undef WRAP_STATIC_CAST_CONST
#undef WRAP_GET_STRING
#undef WRAPPER_CLASS
#undef WRAPPER_BASE
#undef WRAPPER_METHODS_MOVE_CONSTRUCTOR
#undef WRAPPER_METHODS_BASE
#undef WRAPPER_METHODS_STRICT
#undef WRAPPER_METHODS
#undef WRAP_HANDLER_METHOD
#undef WRAP_HANDLER_METHODS
#undef MEMBER
#undef MEMBER_ITERATOR


RAPIDJSON_NAMESPACE_END

#else // WRAP_RAPIDJSON_FOR_DLL

#include "rapidjson/pyrj_c.h"
#include "rapidjson/document.h"
#include "rapidjson/prettywriter.h"
#include "rapidjson/schema.h"

#endif // WRAP_RAPIDJSON_FOR_DLL

namespace YggInterface {
  namespace utils {
    typedef bool (*filterFunc)(const rapidjson::Document&);
    typedef bool (*transformFunc)(rapidjson::Document&);

    /**
     * @brief Base class for filters that can be used to exclude messages
     */
    class FilterBase {
    private:
      FilterBase(const FilterBase&) = delete;
      FilterBase& operator=(const FilterBase&) = delete;
    public:
      /** @brief Constructor */
      YGG_API FilterBase();
      /** @brief Destructor */
      YGG_API virtual ~FilterBase();
      /**
       * @brief Apply the filter to a message
       * @param[in] doc Message to run filter on
       * @return true if message filtered, false otherwise
       */
      YGG_API virtual bool operator()(const rapidjson::Document& doc);
      /**
       * @brief Return a copy of this filter
       * @return Copy
       */
      YGG_API virtual FilterBase* copy() const;
      /**
       * @brief Get the wrapped Python function/callable if present.
       * @return Python object if wrapped, NULL otherwise.
       */
      virtual PyObject* getPython() const { return NULL; }
    };
    /**
     * @brief Base class for transforms that can be applied to messages
     */
    class TransformBase {
    private:
      TransformBase(const TransformBase&) = delete;
      TransformBase& operator=(const TransformBase&) = delete;
    public:
      /** @brief Constructor */
      YGG_API TransformBase();
      /** @brief Destructor */
      YGG_API virtual ~TransformBase();
      /**
       * @brief Apply the transform to a message
       * @param[in,out] doc Message to transform
       * @return true if successful, false otherwise
       */
      YGG_API virtual bool operator()(rapidjson::Document& doc);
      /**
       * @brief Return a copy of this transform
       * @return Copy
       */
      YGG_API virtual TransformBase* copy() const;
      /**
       * @brief Get the wrapped Python function/callable if present.
       * @return Python object if wrapped, NULL otherwise.
       */
      virtual PyObject* getPython() const { return NULL; }
    };

    /**
     * @brief Filter based on a C++ function
     */
    class FilterClass : public FilterBase {
    private:
      FilterClass(const FilterClass&) = delete;
      FilterClass& operator=(const FilterClass&) = delete;
    public:
      /**
       * @brief Constructor
       * @param[in] func C++ function to use as a filter
       */
      YGG_API FilterClass(filterFunc func);
      /**
       * @brief Apply the filter to a message
       * @param[in] doc Message to filter
       * @return true if message filtered, false otherwise
       */
      YGG_API bool operator()(const rapidjson::Document& doc) override;
      /** \copydoc YggInterface::utils::FilterBase::copy */
      YGG_API FilterBase* copy() const override;
      filterFunc func_; /**< Filter function */
    };
    /**
     * @brief Transform based on a C++ function
     */
    class TransformClass : public TransformBase {
    private:
      TransformClass(const TransformClass&) = delete;
      TransformClass& operator=(const TransformClass&) = delete;
    public:
      /**
       * @brief Constructor
       * @param[in] func C++ function to use as a transform
       */
      YGG_API TransformClass(const transformFunc& func);
      /**
       * @brief Apply the transform to a message
       * @param[in,out] doc Message to transform
       * @return true if successful, false otherwise
       */
      YGG_API bool operator()(rapidjson::Document& doc) override;
      /** \copydoc YggInterface::utils::TransformBase::copy */
      YGG_API TransformBase* copy() const override;
      transformFunc func_; /**< Transform function */
    };

    /**
     * @brief Base function for utilizing a Python function
     */
    class PyBaseFunc {
    private:
      PyBaseFunc(const PyBaseFunc&) = delete;
      PyBaseFunc& operator=(const PyBaseFunc&) = delete;
    public:
      /**
       * @brief Constructor
       * @param[in] func Python function
       */
      YGG_API PyBaseFunc(const PyObject* func);
      /**
       * @brief Destructor
       */
      YGG_API virtual ~PyBaseFunc();
    protected:
      /**
       * @brief Call the Python function on document
       * @param[in] doc Input document to pass as an argument
       * @param[out] out Document to store the result in
       * @return true if successful, false otherwise
       */
      YGG_API bool _call(const rapidjson::Document& doc,
			 rapidjson::Document* out=nullptr);
#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
      PyObject* func_; /**< Python function */
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
    };
    /**
     * @brief Filter based on a Python function
     */
    class PyFilterClass : public FilterBase, public PyBaseFunc {
    private:
      PyFilterClass(const PyFilterClass&) = delete;
      PyFilterClass& operator=(const PyFilterClass&) = delete;
    public:
      /**
       * @brief Constructor
       * @param[in] func Python function to use as a filter
       */
      YGG_API PyFilterClass(const PyObject* func);
      /**
       * @brief Apply the filter to a message
       * @param[in] doc Message to filter
       * @return true if message filtered, false otherwise
       */
      YGG_API bool operator()(const rapidjson::Document& doc) override;
      /** \copydoc YggInterface::utils::FilterBase::copy */
      YGG_API FilterBase* copy() const override;
#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
      /** \copydoc YggInterface::utils::FilterBase::getPython */
      PyObject* getPython() const override { return this->func_; }
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
    };
    /**
     * @brief Transform based on a Python function
     */
    class PyTransformClass : public TransformBase, public PyBaseFunc {
    private:
      PyTransformClass(const PyTransformClass&) = delete;
      PyTransformClass& operator=(const PyTransformClass&) = delete;
    public:
      /**
       * @brief Constructor
       * @param[in] func Python function to use as a transform
       */
      YGG_API PyTransformClass(const PyObject* func);
      /**
       * @brief Apply the transform to a message
       * @param[in,out] doc Message to transform
       * @return true if successful, false otherwise
       */
      YGG_API bool operator()(rapidjson::Document& doc) override;
      /** \copydoc YggInterface::utils::TransformBase::copy */
      YGG_API TransformBase* copy() const override;
#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
      /** \copydoc YggInterface::utils::FilterBase::getPython */
      PyObject* getPython() const override { return this->func_; }
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
    };
#ifdef YGG_LINK_PYTHON_TO_CPP
// definitions are in the header and don't need to be exported
#define YGG_RJ_API
#else
#define YGG_RJ_API YGG_API
#endif

#ifdef YGG_LINK_PYTHON_TO_CPP
    /*! @brief Global copy of rapidjson import of numpy PyArray_API */
    YGG_RJ_API extern int rapidjson_NUMPY_IMPORTED;
#endif

    /*!
     * @brief Acquire/release the Python GIL.
     * @param[in] release If true, the Python GIL will be released by the
     *   current thread if it has already been acquired. If false, the GIL
     *   will be acquired by the current thread if it has not already been
     *   acquired.
     * @returns true if successful, false otherwise.
     */
    YGG_RJ_API bool global_PyGILState(bool release = false);

    /*!
     * @brief Save/restore the global Python thread state.
     * @param[in] restore If true, the global thread state will be restored
     *   if one has been saved. If false, the global thread state will be
     *   saved if one does not already exist.
     * @returns true if successful, false otherwise.
     */
    YGG_RJ_API bool global_PyThreadState(bool restore = false);
    
    /*!
      @brief Initialize the Python API.
      @param[in] error_prefix Prefix to add to error message describine
        the context from which the function was called.
      @return true if successful, false otherwise.
     */
    YGG_RJ_API bool initialize_python(const std::string error_prefix="");
    
    /*!
      @brief Finalize the Python API.
      @param[in] error_prefix Prefix to add to error message describine
        the context from which the function was called.
      @return true if successful, false otherwise.
     */
    YGG_RJ_API bool finalize_python(const std::string error_prefix="");
    
    /*!
      @brief Import numpy arrays.
      @return true if successful, false otherwise.
     */
    YGG_RJ_API bool import_numpy_arrays();

    /*!
      @brief Determine if numpy arrays are enabled.
      @return true if successful, false otherwise.
    */
    YGG_RJ_API bool numpy_arrays_imported();

    /*!
      @brief Import a Python function or class.
      @param[in] module Name of the module containing the desired function
        or class.
      @param[in] element Name of desired Python function or class.
      @param[in] error_prefix Prefix to add to error message describine
        the context from which the function was called.
      @param[in] ignore_error If true, no error will be logged.
      @return The Python function or class. NULL indicates an error.
     */
    YGG_RJ_API PyObject* import_python_element(const std::string& module,
					       const std::string& element,
					       const std::string error_prefix="",
					       const bool ignore_error=false);

    /*!
      @brief Import a Python function or class.
      @param[in] module_class String containing the name of the module
        a function or class should be imported from and the name of the
	function or class.
      @param[in] error_prefix Prefix to add to error message describine
        the context from which the function was called.
      @param[in] ignore_error If true, no error will be logged.
      @return The Python function or class. NULL indicates an error.
     */
    YGG_RJ_API PyObject* import_python_object(const std::string& module_class,
					      const std::string error_prefix="",
					      const bool ignore_error=false);
    

  }
}

#if defined(YGG_LINK_PYTHON_TO_CPP) && !defined(RAPIDJSON_WRAPPER_DEFS_)
#define RAPIDJSON_WRAPPER_DEFS_ inline
#include "utils/rapidjson_wrapper.defs"
#endif // WRAP_RAPIDJSON_FOR_DLL
