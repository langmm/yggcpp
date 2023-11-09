#define RAPIDJSON_FORCE_IMPORT_ARRAY
#include "rapidjson/pyrj_c.h"
#include "rapidjson_wrapper.hpp"

#define DO_NOTHING()
#define UNPACK_BKTS(...)			\
  __VA_ARGS__
  // UNPACK_MACRO X
#define ADD_BKTS_T(X)			\
  template <UNPACK_MACRO X>
#define ADD_BKTS(X)				\
  <UNPACK_MACRO X>

#ifdef WRAP_RAPIDJSON_FOR_DLL

namespace wrap {
  namespace rapidjson {
    namespace internal {
      using namespace ::rapidjson::internal;
    }
  }
  using namespace ::rapidjson;
  using ::rapidjson::ObjWavefront;
#include "rapidjson/document.h"
#include "rapidjson/prettywriter.h"
#include "rapidjson/schema.h"
#include "rapidjson/pyrj.h"
#undef UTF8
}

void rapidjson::initialize_python(const std::string error_prefix) {
  RJ_WNS::initialize_python(error_prefix);
}
void rapidjson::finalize_python(const std::string error_prefix) {
  RJ_WNS::finalize_python(error_prefix);
}

using namespace rapidjson;

#define WRAP_CONSTRUCTOR_TEMP(cls, tempT, temp, argsT, args)		\
  ADD_BKTS_T(tempT)							\
  cls ADD_BKTS(temp)::cls argsT :					\
    WrapperBase<cls::BaseType>(new cls ADD_BKTS(temp)::BaseType args, true) {}
#define WRAP_CONSTRUCTOR(cls, argsT, args)		\
  cls::cls argsT :					\
    WrapperBase<cls::BaseType>(new cls::BaseType args, true) {}
#define WRAP_METHOD(cls, name, argsT, args, type, mods)	\
  type cls::name argsT mods {				\
    return ((cls::BaseType*)(this->val_))->name args;	\
  }
#define WRAP_METHOD_TEMP(cls, name, argsT, args, type, mods)	\
  template <typename T>						\
  type cls::name argsT mods {					\
    return ((cls::BaseType*)(this->val_))->name<T> args;	\
  }
#define WRAP_METHOD_TEMP_ARGS(cls, name, tempT, temp, argsT, args, type, mods) \
  ADD_BKTS_T(tempT)							\
  type cls ADD_BKTS(temp)::name argsT mods {				\
    return ((cls::BaseType*)(this->val_))->name args;			\
  }
#define WRAP_METHOD_TEMP_ARGS_SELF(cls, name, tempT, temp, argsT, args, mods) \
  ADD_BKTS_T(tempT)							\
  cls ADD_BKTS(temp)& cls ADD_BKTS(temp)::name argsT mods {		\
    ((cls::BaseType*)(this->val_))->name args;				\
    return *this;							\
  }
#define WRAP_METHOD_SELF(cls, name, argsT, args, mods)		\
  cls& cls::name argsT mods {					\
    ((cls::BaseType*)(this->val_))->name args;			\
    return *this;						\
  }
#define WRAP_METHOD_SELF_CAST(cls, name, argsT, args, type, mods)	\
  type& cls::name argsT mods {						\
    ((cls::BaseType*)(this->val_))->name args;				\
    return *this;							\
  }
#define WRAP_METHOD_CAST(cls, name, argsT, args, type, mods)	\
  type cls::name argsT mods {					\
    return type(((cls::BaseType*)(this->val_))->name args);	\
  }
#define WRAP_METHOD_CAST_CONST(cls, name, argsT, args, type, mods)	\
  const type cls::name argsT const mods {				\
    const type::BaseType& res = ((cls::BaseType*)(this->val_))->name args; \
    return type(*const_cast<type::BaseType*>(&res));			\
  }
#define WRAP_METHOD_CAST_CONST_PTR(cls, name, argsT, args, type, mods)	\
  const type cls::name argsT const mods {				\
    const type::BaseType* res = ((cls::BaseType*)(this->val_))->name args; \
    return type(*const_cast<type::BaseType*>(res));			\
  }
#define WRAP_SET_GET(name, type)					\
  WRAP_CONSTRUCTOR(WValue, (type x), (x));				\
  WRAP_METHOD(WValue, Get ## name, (), (), type, const);		\
  WRAP_METHOD_SELF(WValue, Set ## name, (type x), (x), );		\
  WRAP_METHOD(WValue, Is ## name, (), (), bool, const)
#define WRAP_METHOD_ITER(cls, name, argsT, args, mods)			\
  cls cls::name argsT mods {						\
    return cls(const_cast<cls::BaseType*>(this->val_)->name args, true); \
  }
#define WRAP_GET_STRING(name)						\
  const WValue WValue::Get ## name ## String() {			\
    return WValue(const_cast<WValue::BaseType*>(&(WValue::BaseType::Get ## name ## String())));	\
  }

////////////////////////////////////////////////////////////////////
// WrapperBase
////////////////////////////////////////////////////////////////////

#define WRAPPER_METHODS_OPS_(cls)					\
  cls* cls::operator->() const {					\
    RAPIDJSON_ASSERT(iter);						\
    return const_cast<cls*>(this);					\
  }									\
  cls& cls::operator++() {						\
    RAPIDJSON_ASSERT(iter);;						\
    val_++;								\
    return *this;							\
  }									\
  cls cls::operator++(int) {						\
    cls::BaseType* out = val_;						\
    ++(*this);								\
    return cls(out);							\
  }									\
  bool cls::operator==(const cls& rhs) const {				\
    if (iter)								\
      return (val_ == rhs.val_);					\
    return ((*val_) == (*rhs.val_));					\
  }									\
  bool cls::operator!=(const cls& rhs) const {				\
    return !(*this == rhs);						\
  }
#define WRAPPER_METHODS_EMPTY_CONSTRUCTOR_BASE(cls, base, tempT, temp, macroB, macroT, macro) \
  macroT(tempT)								\
  cls macro(temp)::cls() :						\
    WrapperBase<macroB base>(new macroB base, true) {}
#define WRAPPER_METHODS_EMPTY_CONSTRUCTOR_TEMP(cls, base, tempT, temp)	\
  WRAPPER_METHODS_EMPTY_CONSTRUCTOR_BASE(cls, base, tempT, temp, UNPACK_BKTS, ADD_BKTS_T, ADD_BKTS)
#define WRAPPER_METHODS_EMPTY_CONSTRUCTOR(cls, base)			\
  WRAPPER_METHODS_EMPTY_CONSTRUCTOR_BASE(cls, base, , , ,		\
					 DO_NOTHING, DO_NOTHING)
#define WRAPPER_METHODS_MOVE_CONSTRUCTOR_BASE(cls, base, tempT, temp, macroB, macroT, macro) \
  macroT(tempT)								\
  cls macro(temp)::cls(macroB base&& val) :				\
    WrapperBase<macroB base>(new macroB base(std::forward<macroB base>(val)), true) {}
#define WRAPPER_METHODS_MOVE_CONSTRUCTOR_TEMP(cls, base, tempT, temp)	\
  WRAPPER_METHODS_MOVE_CONSTRUCTOR_BASE(cls, base, tempT, temp, UNPACK_BKTS, ADD_BKTS_T, ADD_BKTS)
#define WRAPPER_METHODS_MOVE_CONSTRUCTOR(cls, base)			\
  WRAPPER_METHODS_MOVE_CONSTRUCTOR_BASE(cls, base, , , ,		\
					DO_NOTHING, DO_NOTHING)
#define WRAPPER_METHODS_BASE(cls, base, tempT, temp, macroB, macroT, macro) \
  macroT(tempT)								\
  cls macro(temp)::cls(macroB base* val, bool iterator) :		\
  WrapperBase<macroB base>(val, false, iterator) {}			\
    macroT(tempT)							\
    cls macro(temp)::cls(macroB base& val) :				\
  WrapperBase<macroB base>(val) {}					\
  macroT(tempT)								\
  cls macro(temp)::cls(cls macro(temp)&& rhs) :				\
  WrapperBase<macroB base>((macroB base*)nullptr) {			\
    std::swap(this->val_, rhs.val_);					\
    std::swap(this->created_val, rhs.created_val);			\
    std::swap(this->iter, rhs.iter);					\
  }									\
  macroT(tempT)								\
  cls macro(temp)::~cls() {						\
    if (this->created_val)						\
      delete this->val_;						\
    this->val_ = nullptr;						\
  }									\
  macroT(tempT)								\
  cls macro(temp)& cls macro(temp)::operator=(cls macro(temp)&& rhs) {	\
    return *this = rhs.Move();						\
  }									\
  macroT(tempT)								\
  cls macro(temp)& cls macro(temp)::operator=(cls macro(temp)& rhs) {	\
    std::swap(this->val_, rhs.val_);					\
    std::swap(this->created_val, rhs.created_val);			\
    std::swap(this->iter, rhs.iter);					\
    return *this;							\
  }									\
  macroT(tempT)								\
  cls macro(temp)& cls macro(temp)::Move() {				\
    return *this;							\
  }
#define WRAPPER_METHODS_(cls, base)		\
  WRAPPER_METHODS_BASE(cls, base, , , , DO_NOTHING, DO_NOTHING)
#define WRAPPER_METHODS_TEMP(cls, base, tempT, temp)		\
  WRAPPER_METHODS_BASE(cls, base, tempT, temp,			\
		       UNPACK_BKTS, ADD_BKTS_T, ADD_BKTS)

#define WRAPPER_METHODS(base)						\
  WRAPPER_METHODS_(W ## base, RJ_WNS::base);				\
  WRAPPER_METHODS_EMPTY_CONSTRUCTOR(W ## base, RJ_WNS::base);		\
  WRAPPER_METHODS_MOVE_CONSTRUCTOR(W ## base, RJ_WNS::base);		\
  WRAPPER_METHODS_OPS_(W ## base)
#define WRAPPER_METHODS_NO_EMPTY_CONSTRUCTOR(base)	\
  WRAPPER_METHODS_(W ## base, RJ_WNS::base)

// #define _Args(...) __VA_ARGS__
// #define STRIP_PARENS(X) X
// #define PACK_MACRO_(X) STRIP_PARENS( _Args X )
// #define PACK_MACRO(...) PACK_MACRO_((__VA_ARGS__))
// #define UNPACK_MACRO(...) __VA_ARGS__

#define WRAP_HANDLER_METHOD_TEMP(cls, name, argsT, args, tempT, temp)	\
  WRAP_METHOD_TEMP_ARGS(cls, name, tempT, temp, argsT, args, bool, )
#define WRAP_HANDLER_METHODS_TEMP(cls, tempT, temp)			\
  WRAP_HANDLER_METHOD_TEMP(cls, Null, (), (),				\
			   tempT, temp);				\
  WRAP_HANDLER_METHOD_TEMP(cls, Bool, (bool b), (b),			\
			   tempT, temp);				\
  WRAP_HANDLER_METHOD_TEMP(cls, Int, (int i), (i),			\
			   tempT, temp);				\
  WRAP_HANDLER_METHOD_TEMP(cls, Uint, (unsigned u), (u),		\
			   tempT, temp);				\
  WRAP_HANDLER_METHOD_TEMP(cls, Int64, (int64_t i64), (i64),		\
			   tempT, temp);				\
  WRAP_HANDLER_METHOD_TEMP(cls, Uint64, (uint64_t u64), (u64),		\
			   tempT, temp);				\
  WRAP_HANDLER_METHOD_TEMP(cls, Double, (double d), (d),		\
			   tempT, temp);				\
  WRAP_HANDLER_METHOD_TEMP(cls, String,					\
			   (const Ch* str, SizeType length, bool copy),	\
			   (str, length, copy),				\
			   tempT, temp);				\
  WRAP_HANDLER_METHOD_TEMP(cls, StartObject, (), (),			\
			   tempT, temp);				\
  WRAP_HANDLER_METHOD_TEMP(cls, Key, (const Ch* str, SizeType length,	\
				      bool copy),			\
			   (str, length, copy),				\
			   tempT, temp);				\
  WRAP_HANDLER_METHOD_TEMP(cls, EndObject, (SizeType memberCount),	\
			   (memberCount),				\
			   tempT, temp);				\
  WRAP_HANDLER_METHOD_TEMP(cls, StartArray, (), (),			\
			   tempT, temp);				\
  WRAP_HANDLER_METHOD_TEMP(cls, EndArray, (SizeType memberCount),	\
			   (memberCount),				\
			   tempT, temp);				\
  WRAP_HANDLER_METHOD_TEMP(cls, YggdrasilString,			\
			   (const Ch* str, SizeType length,		\
			    bool copy, WDocument* schema),		\
			   (str, length, copy, *(schema->val_)),	\
			   tempT, temp);				\
  WRAP_HANDLER_METHOD_TEMP(cls, YggdrasilStartObject,			\
			   (WDocument* schema),				\
			   (*(schema->val_)),				\
			   tempT, temp);				\
  WRAP_HANDLER_METHOD_TEMP(cls, YggdrasilEndObject,			\
			   (SizeType memberCount), (memberCount),	\
			   tempT, temp)
  
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
  WRAP_HANDLER_METHOD(cls, String,					\
		      (const Ch* str, SizeType length, bool copy),	\
		      (str, length, copy));				\
  WRAP_HANDLER_METHOD(cls, StartObject, (), ());			\
  WRAP_HANDLER_METHOD(cls, Key, (const Ch* str, SizeType length,	\
				 bool copy),				\
		      (str, length, copy));				\
  WRAP_HANDLER_METHOD(cls, EndObject, (SizeType memberCount),		\
		      (memberCount));					\
  WRAP_HANDLER_METHOD(cls, StartArray, (), ());				\
  WRAP_HANDLER_METHOD(cls, EndArray, (SizeType memberCount),		\
		      (memberCount));					\
  WRAP_HANDLER_METHOD(cls, YggdrasilString,				\
		      (const Ch* str, SizeType length,			\
		       bool copy, WDocument* schema),			\
		      (str, length, copy, *(schema->val_)));		\
  WRAP_HANDLER_METHOD(cls, YggdrasilStartObject, (WDocument* schema),	\
		      (*(schema->val_)));				\
  WRAP_HANDLER_METHOD(cls, YggdrasilEndObject,				\
		      (SizeType memberCount), (memberCount))

////////////////////////////////////////////////////////////////////
// WStringRefType
////////////////////////////////////////////////////////////////////

  WRAPPER_METHODS_NO_EMPTY_CONSTRUCTOR(StringRefType);
  WRAP_CONSTRUCTOR(WStringRefType, (const CharType* str), (str));
  WRAP_CONSTRUCTOR(WStringRefType, (const CharType* str, SizeType len),
		   (str, len));
  WRAP_CONSTRUCTOR(WStringRefType, (const WStringRefType& rhs),
		   (*(rhs.val_)));

WStringRefType::operator const Ch *() const {
  return (const Ch*)(*(this->val_));
}

////////////////////////////////////////////////////////////////////
// WValue
////////////////////////////////////////////////////////////////////

WValue& WValue::childRef(RJ_WNS::Value* x) {
  for (std::vector<WValue>::iterator it = refs.begin(); it != refs.end(); it++) {
    if (x == it->val_)
      return (*it);
  }
  refs.emplace_back(x);
  return refs[refs.size() - 1];
}

  WRAPPER_METHODS(Value)
  WRAP_CONSTRUCTOR(WValue, (Type type), (type));
  WRAP_CONSTRUCTOR(WValue,
		   (const WValue::Ch* str, SizeType len,
		    WValue::Allocator& allocator),
		   (str, len, allocator));
  WRAP_CONSTRUCTOR(WValue, (const Ch* str, SizeType len), (str, len));
  WRAP_CONSTRUCTOR(WValue, (const std::string& s, Allocator& allocator),
		   (s, allocator));
  WRAP_CONSTRUCTOR(WValue,
		   (const WValue& rhs, WValue::Allocator& allocator,
		    bool copyConstStrings),
		   (*(rhs.val_), allocator, copyConstStrings));
  WRAP_CONSTRUCTOR(WValue,
		   (PyObject* pyobj, Allocator& allocator),
		   (pyobj, allocator));
  WRAP_CONSTRUCTOR(WValue,
		   (const ObjWavefront& x, Allocator& allocator),
		   (x, allocator));
  WRAP_CONSTRUCTOR(WValue,
		   (const Ply& x, Allocator& allocator),
		   (x, allocator));

WValue::WValue(RJ_WNS::Document* val) :
  WValue(dynamic_cast<RJ_WNS::Value*>(val)) {}

  WRAP_METHOD_SELF(WValue, CopyFrom, (const WValue& rhs,
				      WValue::Allocator& allocator,
				      bool copyConstStrings),
		   (*(rhs.val_), allocator, copyConstStrings), );
  WRAP_METHOD_SELF(WValue, Swap, (WValue& rhs), (*(rhs.val_)), );
  WRAP_METHOD_SELF(WValue, Swap, (WValue&& rhs), (*(rhs.val_)), );
  WRAP_METHOD_SELF(WValue, PushBack, (WValue& value,
				      WValue::Allocator& allocator),
		   (*(value.val_), allocator), );
  template <typename Handler>
  WRAP_METHOD(WValue, Accept, (Handler& handler, bool skip_yggdrasil),
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
  WRAP_METHOD_SELF(WValue, SetString,
		   (const WValue::Ch* s, SizeType length,
		    WValue::Allocator& allocator),
		   (s, length, allocator), );
  WRAP_METHOD(WValue, GetString, (), (), const WValue::Ch*, const);
  WRAP_METHOD(WValue, GetStringLength, (), (), SizeType, const);
  // Templated methods
  WRAP_METHOD_TEMP(WValue, Is, (), (), bool, const);
  WRAP_METHOD_TEMP(WValue, IsScalar, (), (), bool, const);
  WRAP_METHOD_TEMP(WValue, Is1DArray, (), (), bool, const);
  WRAP_METHOD_TEMP(WValue, IsNDArray, (), (), bool, const);
  template<typename T>
  WRAP_METHOD(WValue, Get, (T& data), (data), void, const);
  template<typename T>
  WRAP_METHOD_SELF(WValue, Set, (const T& data,
				 WValue::Allocator& allocator),
		   (data, allocator), );
  template<typename T>
  WRAP_METHOD_SELF(WValue, Set, (const T& data), (data), );
  // Scalar methods
  WRAP_METHOD_TEMP(WValue, GetScalar, (), (), T, const);
  WRAP_METHOD_TEMP(WValue, GetScalar,
		   (const WValue::UnitsType data_units),
		   (data_units), T, const);
  WRAP_METHOD_TEMP(WValue, GetScalar, (const WValue::Ch* units_str),
		   (units_str), T, const);
  template<typename T>
  WRAP_METHOD(WValue, GetScalar, (T& data), (data), void, const);
  template<typename T>
  WRAP_METHOD(WValue, GetScalar,
	      (T& data, const WValue::UnitsType data_units),
	      (data, data_units), void, const);
  template<typename T>
  WRAP_METHOD(WValue, GetScalar, (T& data, const WValue::Ch* units_str),
	      (data, units_str), void, const);
  template<typename T>
  WRAP_METHOD(WValue, GetScalarValue, (T& data), (data), void, const);
  template<typename T>
  WRAP_METHOD_SELF(WValue, SetScalar,
		   (const T data, WValue::Allocator& allocator),
		   (data, allocator), );
  template<typename T>
  WRAP_METHOD_SELF(WValue, SetScalar,
		   (const T data, const WValue::Ch* units_str,
		    WValue::Allocator& allocator),
		   (data, units_str, allocator), );
  template<typename T>
  WRAP_METHOD_SELF(WValue, SetScalar,
		   (const T data, const WValue::Ch* units_str,
		    const SizeType units_len,
		    WValue::Allocator& allocator),
		   (data, units_str, units_len, allocator), );
  WRAP_METHOD_SELF(WValue, SetScalar,
		   (const WValue::Ch* data, const SizeType precision,
		    WValue::Allocator& allocator,
		    const WValue::Ch* encoding,
		    SizeType encoding_len),
		   (data, precision, allocator,
		    encoding, encoding_len), );
  // 1DArray
  template<typename T>
  WRAP_METHOD(WValue, Get1DArray,
	      (T*& data, SizeType& nelements,
	       WValue::Allocator& allocator,
	       const WValue::UnitsType data_units),
	      (data, nelements, allocator, data_units), void, const);
  template<typename T>
  WRAP_METHOD(WValue, Get1DArray,
	      (T*& data, SizeType& nelements,
	       WValue::Allocator& allocator, const WValue::Ch* units_str),
	      (data, nelements, allocator, units_str), void, const);
  WRAP_METHOD_TEMP(WValue, Get1DArray,
		   (SizeType& nelements, WValue::Allocator& allocator,
		    const WValue::UnitsType data_units),
		   (nelements, allocator, data_units), T*, const);
  WRAP_METHOD_TEMP(WValue, Get1DArray,
		   (SizeType& nelements, WValue::Allocator& allocator,
		    const WValue::Ch* units_str),
		   (nelements, allocator, units_str), T*, const);
  template<typename T>
  WRAP_METHOD_SELF(WValue, Set1DArray, (const T* x, SizeType len,
					WValue::Allocator& allocator),
		   (x, len, allocator), );
  template<typename T>
  WRAP_METHOD_SELF(WValue, Set1DArray, (const T* x, SizeType len,
					const WValue::Ch* units_str,
					WValue::Allocator& allocator),
		   (x, len, units_str, allocator), );
  template<typename T>
  WRAP_METHOD_SELF(WValue, Set1DArray, (const T* x, SizeType len,
					const WValue::Ch* units_str,
					SizeType units_len,
					WValue::Allocator& allocator),
		   (x, len, units_str, units_len, allocator), );
  // NDArray
  template<typename T>
  WRAP_METHOD(WValue, GetNDArray,
	      (T*& data, SizeType*& shape, SizeType& ndim,
	       WValue::Allocator& allocator,
	       const WValue::UnitsType data_units),
	      (data, shape, ndim, allocator, data_units), void, const);
  template<typename T>
  WRAP_METHOD(WValue, GetNDArray,
	      (T*& data, SizeType*& shape, SizeType& ndim,
	       WValue::Allocator& allocator, const WValue::Ch* units_str),
	      (data, shape, ndim, allocator, units_str), void, const); 
  WRAP_METHOD_TEMP(WValue, GetNDArray,
		   (SizeType*& shape, SizeType& ndim,
		    WValue::Allocator& allocator,
		    const WValue::Ch* units_str),
		   (shape, ndim, allocator, units_str), T*, const);
  WRAP_METHOD_TEMP(WValue, GetNDArray,
		   (SizeType*& shape, SizeType& ndim,
		    WValue::Allocator& allocator,
		    const WValue::UnitsType data_units),
		   (shape, ndim, allocator, data_units), T*, const);
  template<typename T>
  WRAP_METHOD_SELF(WValue, SetNDArray, (const T* x, SizeType shape[],
					SizeType ndim,
					WValue::Allocator& allocator),
		   (x, shape, ndim, allocator), );
  template<typename T>
  WRAP_METHOD_SELF(WValue, SetNDArray,
		   (const T* x, SizeType shape[],
		    SizeType ndim, const WValue::Ch* units_str,
		    WValue::Allocator& allocator),
		   (x, shape, ndim, units_str, allocator), );
  template<typename T>
  WRAP_METHOD_SELF(WValue, SetNDArray,
		   (const T* x, SizeType shape[],
		    SizeType ndim, const WValue::Ch* units_str,
		    SizeType units_len, WValue::Allocator& allocator),
		   (x, shape, ndim, units_str, units_len, allocator), );
  // Array methods
  WRAP_METHOD(WValue, IsArray, (), (), bool, const);
  WRAP_METHOD_SELF(WValue, SetArray, (), (), );
  WRAP_METHOD(WValue, Size, (), (), rapidjson::SizeType, const);
  WRAP_METHOD(WValue, Empty, (), (), bool, const);
  WRAP_METHOD_SELF(WValue, Reserve, (SizeType newCapacity,
				     WValue::Allocator &allocator),
		   (newCapacity, allocator), );
  WRAP_METHOD_ITER(WValue, Erase, (const WValue& pos), (pos.val_), );
  WRAP_METHOD_ITER(WValue, Begin, (), (), );
  WRAP_METHOD_ITER(WValue, End, (), (), );
  WRAP_METHOD_ITER(WValue, Begin, (), (), const);
  WRAP_METHOD_ITER(WValue, End, (), (), const);
  WValue& WValue::operator[](SizeType index) {
    RJ_WNS::Value& tmp = this->val_->operator[](index);
    return childRef(&tmp);
  }
  const WValue& WValue::operator[](SizeType index) const {
    const RJ_WNS::Value& tmp = this->val_->operator[](index);
    return const_cast<WValue*>(this)->childRef(const_cast<RJ_WNS::Value*>(&tmp));
  }
  // Object methods
  WRAP_METHOD(WValue, IsObject, (), (), bool, const);
  WRAP_METHOD_SELF(WValue, SetObject, (), (), );
  WRAP_METHOD(WValue, MemberCount, (), (), rapidjson::SizeType, const);
  WRAP_METHOD(WValue, HasMember, (const WValue::Ch* name), (name), bool, const);
  WRAP_METHOD(WValue, HasMember, (const WValue& name), (*(name.val_)),
	      bool, const);
  template <typename T>
  RAPIDJSON_DISABLEIF_RETURN(
    (internal::NotExpr<
     internal::IsSame<typename internal::RemoveConst<T>::Type,
     typename WValue::Ch> >),
    (WValue&)) WValue::operator[](T* name) {
    RJ_WNS::Value& tmp = this->val_->operator[](name);
    return childRef(&tmp);
  }
  template <typename T>
  RAPIDJSON_DISABLEIF_RETURN(
    (internal::NotExpr<
     internal::IsSame<typename internal::RemoveConst<T>::Type,
     typename WValue::Ch> >),
    (const WValue&)) WValue::operator[](T* name) const {
    const RJ_WNS::Value& tmp = this->val_->operator[](name);
    return const_cast<WValue*>(this)->childRef(const_cast<RJ_WNS::Value*>(&tmp));
  }
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
  WRAP_METHOD(WValue, RemoveMember, (const WValue::Ch* name),
	      (name), bool, );
  WRAP_METHOD_CAST(WValue, MemberBegin, (), (),
		   WValue::MemberIterator, );
  WRAP_METHOD_CAST(WValue, MemberEnd, (), (),
		   WValue::MemberIterator, );
  WRAP_METHOD_CAST(WValue, MemberBegin, (), (),
		   WValue::ConstMemberIterator, const);
  WRAP_METHOD_CAST(WValue, MemberEnd, (), (),
		   WValue::ConstMemberIterator, const);
  WRAP_METHOD_CAST(WValue, FindMember, (const Ch* name), (name),
		   WValue::MemberIterator, );
  WRAP_METHOD_CAST(WValue, FindMember, (const Ch* name), (name),
		   WValue::ConstMemberIterator, const);
  WRAP_METHOD_CAST(WValue, FindMember, (const WValue& name),
		   (*(name.val_)), WValue::MemberIterator, );
  WRAP_METHOD_CAST(WValue, FindMember, (const WValue& name),
		   (*(name.val_)), WValue::ConstMemberIterator, const);
  // Python methods
  WRAP_METHOD(WValue, IsPythonClass, (), (), bool, const);
  WRAP_METHOD(WValue, IsPythonInstance, (), (), bool, const);
  WRAP_METHOD(WValue, IsPythonFunction, (), (), bool, const);
  WRAP_METHOD(WValue, GetPythonObjectRaw, (), (), PyObject*, const);
  WRAP_METHOD(WValue, SetPythonObjectRaw,
	      (PyObject* x, WValue::Allocator& allocator,
	       bool skipTitle, bool allowPickle),
	      (x, allocator, skipTitle, allowPickle), bool, );
  // Geometry methods
  WRAP_METHOD(WValue, IsObjWavefront, (), (), bool, const);
  WRAP_METHOD(WValue, IsPly, (), (), bool, const);
  WRAP_METHOD(WValue, GetObjWavefront, (), (),
	      rapidjson::ObjWavefront, const);
  WRAP_METHOD(WValue, GetObjWavefront,
	      (rapidjson::ObjWavefront& x), (x), void, const);
  WRAP_METHOD(WValue, GetPly, (), (), rapidjson::Ply, const);
  WRAP_METHOD(WValue, GetPly, (rapidjson::Ply& x), (x), void, const);
  WRAP_METHOD_SELF(WValue, SetObjWavefront,
		   (rapidjson::ObjWavefront x,
		    WValue::Allocator& allocator),
		   (x, allocator), );
  WRAP_METHOD_SELF(WValue, SetObj,
		   (rapidjson::ObjWavefront x,
		    WValue::Allocator& allocator),
		   (x, allocator), );
  WRAP_METHOD_SELF(WValue, SetPly,
		   (rapidjson::Ply x, WValue::Allocator& allocator),
		   (x, allocator), );
  // Yggdrasil methods
  WRAP_METHOD(WValue, IsType, (const WValue::Ch* type), (type), bool, const);
  WRAP_METHOD(WValue, GetDataPtr, (bool& requires_freeing),
	      (requires_freeing), void*, const);
  WRAP_METHOD(WValue, GetNBytes, (), (), SizeType, const);
  WRAP_METHOD(WValue, SetDataPtr,
	      (const WValue::Ch* type, void*& value,
	       WValue::Allocator& allocator),
	      (type, value, allocator), bool, );
  WRAP_METHOD(WValue, IsSubType,
	      (const WValue::Ch* subtype, SizeType precision),
	      (subtype, precision), bool, const);
  WRAP_METHOD_SELF(WValue, SetYggdrasilString,
		   (const WValue::Ch* s, SizeType length,
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

namespace rapidjson {
  std::ostream & operator << (std::ostream &out, const WValue& p) {
    return ::wrap::rapidjson::operator<<(out, *(p.val_));
  }
}

////////////////////////////////////////////////////////////////////
// WDocument
////////////////////////////////////////////////////////////////////

WDocument::WDocument(RJ_WNS::Document* doc) :
  WValue(doc) {}
WDocument::WDocument(RJ_WNS::Document& doc) :
  WDocument(&doc) {}
WDocument::WDocument(RJ_WNS::Document&& doc) :
  WValue(new RJ_WNS::Document(std::forward<RJ_WNS::Document>(doc)))
{ created_val = true; }
WDocument::WDocument(Type type) :
  WDocument(new RJ_WNS::Document(type))
{ created_val = true; }
#if RAPIDJSON_HAS_CXX11_RVALUE_REFS
WDocument::WDocument(WDocument&& rhs) :
  WValue(std::forward<WValue>(rhs)) {}
WDocument& WDocument::operator=(WDocument&& rhs) {
  return *this = rhs.Move();
}
#endif
WDocument& WDocument::operator=(WDocument& rhs) {
  WValue::operator=(std::forward<WValue>(rhs));
  return *this;
}

  WRAP_METHOD_SELF_CAST(WDocument, CopyFrom,
			(const WDocument& rhs,
			 WDocument::Allocator& allocator,
			 bool copyConstStrings),
			(*(rhs.val_), allocator, copyConstStrings),
			WValue, );
  WRAP_METHOD_SELF_CAST(WDocument, Swap, (WDocument& rhs), (*(rhs.val_)),
			WValue, );
  WRAP_METHOD(WDocument, GetAllocator, (), (), WDocument::Allocator&, );
  WRAP_METHOD(WDocument, Normalize, (const WValue& schema,
				     StringBuffer* error),
	      (*(schema.val_), error), bool, );
  WRAP_METHOD_SELF(WDocument, Parse,
		   (const WValue::Ch* str, size_t length),
		   (str, length), );
  WRAP_METHOD_SELF(WDocument, Parse, (const WValue::Ch* str), (str), );
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

template WDocument& WDocument::ParseStream<StringStream>(StringStream& is);

// bool WDocument::SetVarArgs(WValue& schema, ...) {
//   size_t nargs = val->CountVarArgs(*(schema.val_), true);
//   RAPIDJSON_BEGIN_VAR_ARGS(ap, schema, &nargs, false);
//   bool out = val->SetVarArgs(*(schema.val_), ap);
//   RAPIDJSON_END_VAR_ARGS(ap);
//   return out;
// }

////////////////////////////////////////////////////////////////////
// WMember
////////////////////////////////////////////////////////////////////

#define MEMBER							\
  RJ_WNS::GenericMember<UTF8<>, RAPIDJSON_DEFAULT_ALLOCATOR>

WMember::WMember(MEMBER& rhs) :
  name(rhs.name), value(rhs.value) {}
WMember::WMember(MEMBER&& rhs) :
  name(rhs.name), value(rhs.value) {}
WMember::WMember(WMember&& rhs) :
  name(std::move(rhs.name)), value(std::move(rhs.value)) {}
WMember& WMember::operator=(WMember&& rhs) {
  return *this = static_cast<WMember&>(rhs);
}
WMember& WMember::operator=(WMember& rhs) {
  if (this != &rhs) {
    name = rhs.name;
    value = rhs.value;
  }
  return *this;
}
const WMember* WMember::operator->() const {
  return const_cast<WMember*>(this);
}

#undef MEMBER

////////////////////////////////////////////////////////////////////
// WGenericMemberIterator
////////////////////////////////////////////////////////////////////

#define MEMBER_ITERATOR(C)						\
  RJ_WNS::GenericMemberIterator<C, UTF8<>, RAPIDJSON_DEFAULT_ALLOCATOR>  

  WRAPPER_METHODS_TEMP(WGenericMemberIterator, (MEMBER_ITERATOR(Const)),
		       (bool Const), (Const));
  WRAPPER_METHODS_MOVE_CONSTRUCTOR_TEMP(WGenericMemberIterator,
					(MEMBER_ITERATOR(Const)),
					(bool Const), (Const));
  WRAPPER_METHODS_EMPTY_CONSTRUCTOR_TEMP(WGenericMemberIterator,
					 (MEMBER_ITERATOR(Const)),
					 (bool Const), (Const));
  WRAP_CONSTRUCTOR_TEMP(WGenericMemberIterator,
			(bool Const), (Const),
			(const WGenericMemberIterator::NonConstIterator& it),
			(*(it.val_)));
  template<bool Const>
  WRAP_METHOD_CAST_CONST(WGenericMemberIterator<Const>, operator*, (), (),
			 WMember, );
  template<bool Const>
  WRAP_METHOD_CAST_CONST_PTR(WGenericMemberIterator<Const>, operator->, (), (),
			     WMember, );
  template<bool Const>
  WRAP_METHOD_SELF(WGenericMemberIterator<Const>, operator=,
		   (const WGenericMemberIterator::NonConstIterator& it),
		   (*(it.val_)), );
  template<bool Const>
  WRAP_METHOD_SELF(WGenericMemberIterator<Const>, operator++, (), (), );
  template<bool Const>
  WRAP_METHOD_SELF(WGenericMemberIterator<Const>, operator--, (), (), );
  template<bool Const>
  WRAP_METHOD_CAST(WGenericMemberIterator<Const>, operator++, (int x), (x),
		   typename WGenericMemberIterator<Const>::Iterator, );
  template<bool Const>
  WRAP_METHOD_CAST(WGenericMemberIterator<Const>, operator--, (int x), (x),
		   typename WGenericMemberIterator<Const>::Iterator, );
  template<bool Const>
  WRAP_METHOD_CAST(WGenericMemberIterator<Const>, operator+,
		   (DifferenceType n), (n),
		   typename WGenericMemberIterator<Const>::Iterator, const);
  template<bool Const>
  WRAP_METHOD_CAST(WGenericMemberIterator<Const>, operator-,
		   (DifferenceType n), (n),
		   typename WGenericMemberIterator<Const>::Iterator, const);

#undef MEMBER_ITERATOR

#define ITERATOR_COMP_OP_(op, C1, C2)					\
  namespace rapidjson {							\
    template<>								\
    bool operator op<C1,C2>(const WGenericMemberIterator<C1>& lhs,	\
			    const WGenericMemberIterator<C2>& rhs) {	\
      return (*(lhs.val_) op *(rhs.val_));				\
    }									\
  }
#define ITERATOR_COMP_OP(op)			\
  ITERATOR_COMP_OP_(op, true, true);		\
  ITERATOR_COMP_OP_(op, false, false);		\
  ITERATOR_COMP_OP_(op, true, false);		\
  ITERATOR_COMP_OP_(op, false, true)
/*
#define ITERATOR_COMP_OP(op)						\
  template<bool Const1, bool Const2>				\
  bool operator op(const WGenericMemberIterator<Const1>& lhs,	\
		   const WGenericMemberIterator<Const2>& rhs) {	\
    return (*(lhs.val_) op *(rhs.val_));			\
  }
*/
ITERATOR_COMP_OP(==);
ITERATOR_COMP_OP(!=);
ITERATOR_COMP_OP(<=);
ITERATOR_COMP_OP(>=);
ITERATOR_COMP_OP(< );
ITERATOR_COMP_OP(> );
#undef ITERATOR_COMP_OP
#undef ITERATOR_COMP_OP_

namespace rapidjson {
  template class WGenericMemberIterator<true>;
  template class WGenericMemberIterator<false>;
}

////////////////////////////////////////////////////////////////////
// WSchemaDocument
////////////////////////////////////////////////////////////////////


  WRAPPER_METHODS_NO_EMPTY_CONSTRUCTOR(SchemaDocument);
  WRAP_CONSTRUCTOR(WSchemaDocument, (WDocument& d),
		   (*((RJ_WNS::Document*)d.val_)));
  WRAP_CONSTRUCTOR(WSchemaDocument, (WValue& d), (*(d.val_)));

////////////////////////////////////////////////////////////////////
// WSchemaValidator
////////////////////////////////////////////////////////////////////

  WRAPPER_METHODS_NO_EMPTY_CONSTRUCTOR(SchemaValidator);
  WRAP_CONSTRUCTOR(WSchemaValidator, (WSchemaDocument& s),
		   (*((RJ_WNS::SchemaDocument*)s.val_)));
  WRAP_METHOD(WSchemaValidator, GenerateData, (WDocument& d),
	      (*((RJ_WNS::Document*)d.val_)), bool, );

////////////////////////////////////////////////////////////////////
// WSchemaNormalizer
////////////////////////////////////////////////////////////////////

  WRAPPER_METHODS_NO_EMPTY_CONSTRUCTOR(SchemaNormalizer);
  WRAP_CONSTRUCTOR(WSchemaNormalizer, (WSchemaDocument& s),
		   (*((RJ_WNS::SchemaDocument*)s.val_)));
  WRAP_METHOD(WSchemaNormalizer, GenerateData, (WDocument& d),
	      (*((RJ_WNS::Document*)d.val_)), bool, );
  WRAP_METHOD(WSchemaNormalizer, Compare, (const WValue& d),
	      (*((const RJ_WNS::Value*)d.val_)), bool, );
  WRAP_METHOD(WSchemaNormalizer, GetErrorMsg,
	      (WValue& err, WSchemaNormalizer::Allocator& allocator),
	      (*(err.val_), allocator), bool, const);

////////////////////////////////////////////////////////////////////
// WSchemaEncoder
////////////////////////////////////////////////////////////////////

  WRAPPER_METHODS_NO_EMPTY_CONSTRUCTOR(SchemaEncoder);
  WRAP_CONSTRUCTOR(WSchemaEncoder,
		   (bool minimal,
		    WSchemaEncoder::Allocator* allocator,
		    WSchemaEncoder::StackAllocator* stackAllocator,
		    size_t stackCapacity),
		   (minimal, allocator, stackAllocator, stackCapacity));
  WRAP_HANDLER_METHODS(WSchemaEncoder);
  WRAP_METHOD_CAST(WSchemaEncoder, GetSchema, (), (), WDocument, );

////////////////////////////////////////////////////////////////////
// WPrettyWriter
////////////////////////////////////////////////////////////////////

#define WRAPPED_WRITER						\
  RJ_WNS::PrettyWriter<OutputStream, SourceEncoding,		\
		       TargetEncoding, StackAllocator,		\
		       writeFlags>
#define WRAPPED_WRITER_TEMP_ARGS_T				\
  (typename OutputStream, typename SourceEncoding,		\
   typename TargetEncoding, typename StackAllocator,		\
   unsigned writeFlags)
#define WRAPPED_WRITER_TEMP_ARGS				\
  (OutputStream, SourceEncoding,				\
   TargetEncoding, StackAllocator,				\
   writeFlags)
#define WRAPPED_WRITER_TEMP_T					\
  template <typename OutputStream, typename SourceEncoding,	\
	    typename TargetEncoding, typename StackAllocator,	\
	    unsigned writeFlags>
#define WRAPPED_WRITER_TEMP						\
  <OutputStream, SourceEncoding,					\
    TargetEncoding, StackAllocator,					\
    writeFlags>

  WRAPPER_METHODS_TEMP(WPrettyWriter, (WRAPPED_WRITER),
		       WRAPPED_WRITER_TEMP_ARGS_T,
		       WRAPPED_WRITER_TEMP_ARGS);
  WRAPPER_METHODS_MOVE_CONSTRUCTOR_TEMP(WPrettyWriter, (WRAPPED_WRITER),
					WRAPPED_WRITER_TEMP_ARGS_T,
					WRAPPED_WRITER_TEMP_ARGS);
  WRAP_CONSTRUCTOR_TEMP(WPrettyWriter,
			WRAPPED_WRITER_TEMP_ARGS_T,
			WRAPPED_WRITER_TEMP_ARGS,
			(OutputStream& os, StackAllocator* allocator,
			 size_t levelDepth),
			(os, allocator, levelDepth));
  WRAP_CONSTRUCTOR_TEMP(WPrettyWriter,
			WRAPPED_WRITER_TEMP_ARGS_T,
			WRAPPED_WRITER_TEMP_ARGS,
			(StackAllocator* allocator, size_t levelDepth),
			(allocator, levelDepth));
  WRAP_METHOD_TEMP_ARGS_SELF(WPrettyWriter, SetIndent,
			     WRAPPED_WRITER_TEMP_ARGS_T,
			     WRAPPED_WRITER_TEMP_ARGS,
			     (WPrettyWriter::Ch indentChar,
			      unsigned indentCharCount),
			     (indentChar, indentCharCount), );
  WRAP_METHOD_TEMP_ARGS_SELF(WPrettyWriter, SetFormatOptions,
			     WRAPPED_WRITER_TEMP_ARGS_T,
			     WRAPPED_WRITER_TEMP_ARGS,
			     (PrettyFormatOptions options),
			     ((RJ_WNS::PrettyFormatOptions)options), );
  WRAP_HANDLER_METHODS_TEMP(WPrettyWriter, WRAPPED_WRITER_TEMP_ARGS_T,
			    WRAPPED_WRITER_TEMP_ARGS);
  WRAP_METHOD_TEMP_ARGS(WPrettyWriter, SetYggdrasilMode,
			WRAPPED_WRITER_TEMP_ARGS_T,
			WRAPPED_WRITER_TEMP_ARGS,
			(bool readable), (readable), void, );

namespace rapidjson {
  template class WPrettyWriter<rapidjson::StringBuffer>;
}

#undef WRAPPED_WRITER
#undef WRAPPED_WRITER_TEMP_T
#undef WRAPPED_WRITER_TEMP
#undef WRAPPED_WRITER_TEMP_ARGS
#undef WRAPPED_WRITER_TEMP_ARGS_T

// Cleanup macros

#undef WRAP_CONSTRUCTOR
#undef WRAP_CONSTRUCTOR_TEMP
#undef WRAP_METHOD
#undef WRAP_METHOD_TEMP
#undef WRAP_METHOD_TEMP_ARGS
#undef WRAP_METHOD_TEMP_ARGS_SELF
#undef WRAP_METHOD_SELF
#undef WRAP_METHOD_SELF_CAST
#undef WRAP_METHOD_CAST
#undef WRAP_METHOD_CAST_CONST
#undef WRAP_SET_GET
#undef WRAP_METHOD_ITER
#undef WRAP_GET_STRING
#undef WRAPPER_METHODS_OPS_
#undef WRAPPER_METHODS_EMPTY_CONSTRUCTOR
#undef WRAPPER_METHODS_EMPTY_CONSTRUCTOR_TEMP
#undef WRAPPER_METHODS_EMPTY_CONSTRUCTOR_BASE
#undef WRAPPER_METHODS_MOVE_CONSTRUCTOR
#undef WRAPPER_METHODS_MOVE_CONSTRUCTOR_TEMP
#undef WRAPPER_METHODS_MOVE_CONSTRUCTOR_BASE
#undef WRAPPER_METHODS_
#undef WRAPPER_METHODS_BASE
#undef WRAPPER_METHODS_TEMP
#undef WRAPPER_METHODS
#undef WRAPPER_METHODS_NO_EMPTY_CONSTRUCTOR
#undef WRAP_HANDLER_METHOD
#undef WRAP_HANDLER_METHODS
#undef WRAP_HANDLER_METHOD_TEMP
#undef WRAP_HANDLER_METHODS_TEMP

// Explicitly instantiate template specializations

#define SPECIALIZE_ACCEPT(type)					       \
  template bool WValue::Accept<type>(type& handler,		       \
				     bool skip_yggdrasil) const
#define SPECIALIZE_ACCEPT_WRAP(type)		\
  SPECIALIZE_ACCEPT(::wrap::rapidjson::type)

SPECIALIZE_ACCEPT(PrettyWriter<rapidjson::StringBuffer>);
SPECIALIZE_ACCEPT(Writer<rapidjson::StringBuffer>);
SPECIALIZE_ACCEPT(SchemaEncoder);
SPECIALIZE_ACCEPT(Document);

#undef SPECIALIZE_ACCEPT
#undef SPECIALIZE_ACCEPT_WRAP

template RAPIDJSON_DISABLEIF_RETURN(
  (internal::NotExpr<
   internal::IsSame<typename internal::RemoveConst<const WValue::Ch>::Type,
   typename WValue::Ch> >),
  (WValue&)) WValue::operator[]<const WValue::Ch>(const WValue::Ch* name);
template RAPIDJSON_DISABLEIF_RETURN(
  (internal::NotExpr<
   internal::IsSame<typename internal::RemoveConst<const WValue::Ch>::Type,
   typename WValue::Ch> >),
  (const WValue&)) WValue::operator[]<const WValue::Ch>(const WValue::Ch* name) const;

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
  template void WValue::GetScalarValue<type>(type& data) const;		\
  template void WValue::GetScalar<type>(type& data) const;		\
  template void WValue::GetScalar<type>(type& data,			\
					const WValue::UnitsType data_units) const; \
  template void WValue::GetScalar<type>(type& data,			\
					const WValue::Ch* units_str) const; \
  template type WValue::GetScalar<type>() const;			\
  template type WValue::GetScalar<type>(const WValue::UnitsType data_units) const; \
  template type WValue::GetScalar<type>(const WValue::Ch* units_str) const; \
  template WValue& WValue::SetScalar<type>(const type data,		\
					   WValue::Allocator& allocator); \
  template WValue& WValue::SetScalar<type>(const type data,		\
					   const WValue::Ch* units_str,	\
					   WValue::Allocator& allocator); \
  template WValue& WValue::SetScalar<type>(const type data,		\
					   const WValue::Ch* units_str,	\
					   const SizeType units_len,	\
					   WValue::Allocator& allocator); \
  template void WValue::Get1DArray<type>(type*& data, SizeType& nelements, \
					 WValue::Allocator& allocator,	\
					 const WValue::UnitsType data_units) const; \
  template void WValue::Get1DArray<type>(type*& data, SizeType& nelements, \
					 WValue::Allocator& allocator,	\
					 const WValue::Ch* units_str) const;	\
  template type* WValue::Get1DArray<type>(SizeType& nelements,		\
					  WValue::Allocator& allocator,	\
					  const WValue::UnitsType data_units) const; \
  template type* WValue::Get1DArray<type>(SizeType& nelements,		\
					  WValue::Allocator& allocator,	\
					  const WValue::Ch* units_str) const; \
  template WValue& WValue::Set1DArray<type>(const type* x, SizeType len, \
					    WValue::Allocator& allocator); \
  template WValue& WValue::Set1DArray<type>(const type* x, SizeType len, \
					    const WValue::Ch* units_str, \
					    WValue::Allocator& allocator); \
  template WValue& WValue::Set1DArray<type>(const type* x, SizeType len, \
					    const WValue::Ch* units_str, \
					    SizeType units_len,		\
					    WValue::Allocator& allocator); \
  template void WValue::GetNDArray<type>(type*& data, SizeType*& shape,	\
					 SizeType& ndim,		\
					 WValue::Allocator& allocator,	\
					 const WValue::UnitsType data_units) const; \
  template void WValue::GetNDArray<type>(type*& data, SizeType*& shape,	\
					 SizeType& ndim,		\
					 WValue::Allocator& allocator,	\
					 const WValue::Ch* units_str) const; \
  template type* WValue::GetNDArray<type>(SizeType*& shape,		\
					  SizeType& ndim,		\
					  WValue::Allocator& allocator,	\
					  const WValue::UnitsType data_units) const; \
  template type* WValue::GetNDArray<type>(SizeType*& shape,		\
					  SizeType& ndim,		\
					  WValue::Allocator& allocator,	\
					  const WValue::Ch* units_str) const; \
  template WValue& WValue::SetNDArray<type>(const type* x,		\
					    SizeType shape[],		\
					    SizeType ndim,		\
					    WValue::Allocator& allocator); \
  template WValue& WValue::SetNDArray<type>(const type* x,		\
					    SizeType shape[],		\
					    SizeType ndim,		\
					    const WValue::Ch* units_str, \
					    WValue::Allocator& allocator); \
  template WValue& WValue::SetNDArray<type>(const type* x,		\
					    SizeType shape[],		\
					    SizeType ndim,		\
					    const WValue::Ch* units_str, \
					    SizeType units_len,		\
					    WValue::Allocator& allocator)
#define SPECIALIZE_BUILTIN(type)			\
  SPECIALIZE(type);					\
  template WValue& WValue::Set<type>(const type& data)
  
#define SPECIALIZE_SCALAR_BUILTIN(type)			\
  SPECIALIZE_SCALAR(type);				\
  template WValue& WValue::Set<type>(const type& data)

// default rapidjson
SPECIALIZE_BUILTIN(bool);
SPECIALIZE_SCALAR_BUILTIN(int);
SPECIALIZE_SCALAR_BUILTIN(unsigned);
#ifdef _MSC_VER
SPECIALIZE_SCALAR_BUILTIN(long);
SPECIALIZE_SCALAR_BUILTIN(unsigned long);
#endif
SPECIALIZE_SCALAR_BUILTIN(int64_t);
SPECIALIZE_SCALAR_BUILTIN(uint64_t);
SPECIALIZE_SCALAR_BUILTIN(float);
SPECIALIZE_SCALAR_BUILTIN(double);
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

bool communication::utils::numpy_arrays_imported() {
  bool out = false;
#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
  if (rapidjson_ARRAY_API)
    out = true;
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
  return out;
}
