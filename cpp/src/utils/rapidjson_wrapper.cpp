// defined(YggInterface_py_EXPORTS)
#ifndef YGG_LINK_PYTHON_TO_CPP
#define RAPIDJSON_FORCE_IMPORT_ARRAY
#endif // YGG_LINK_PYTHON_TO_CPP
#include "rapidjson/pyrj_c.h"
#include "utils/rapidjson_wrapper.hpp"

#define DO_NOTHING()
#define UNPACK_BKTS(...)			\
  __VA_ARGS__
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

std::string rapidjson::init_numpy_API() {
  return RJ_WNS::init_numpy_API();
}
std::string rapidjson::initialize_python(const std::string error_prefix,
					 bool dont_raise) {
  return RJ_WNS::initialize_python(error_prefix, dont_raise);
}
void rapidjson::finalize_python(const std::string error_prefix) {
  RJ_WNS::finalize_python(error_prefix);
}
PyObject* rapidjson::import_python_class(const char* module_name,
					 const char* class_name,
					 const std::string error_prefix,
					 const bool ignore_error) {
  return RJ_WNS::import_python_class(module_name, class_name,
				     error_prefix, ignore_error);
}
PyObject* rapidjson::import_python_object(const char* module_class,
					  const std::string error_prefix,
					  const bool ignore_error) {
  return RJ_WNS::import_python_object(module_class,
				      error_prefix, ignore_error);
}

using namespace rapidjson;

#define MEMBER							\
  RJ_WNS::GenericMember<UTF8<>, RAPIDJSON_DEFAULT_ALLOCATOR>
#define MEMBER_ITERATOR(C)						\
  RJ_WNS::GenericMemberIterator<C, UTF8<>, RAPIDJSON_DEFAULT_ALLOCATOR>

#define WRAP_CONSTRUCTOR_TEMP(cls, tempT, temp, argsT, args)		\
  ADD_BKTS_T(tempT)							\
  cls ADD_BKTS(temp)::cls argsT :					\
  WrapperBase<cls ADD_BKTS(temp)::BaseType>(new cls ADD_BKTS(temp)::BaseType args, true) {}
#define WRAP_CONSTRUCTOR(cls, argsT, args)		\
  cls::cls argsT :					\
  WrapperBase<cls::BaseType>(new cls::BaseType args, true) {}
#define WRAP_CONSTRUCTOR_VAL(cls, argsT, args)			\
  cls::cls argsT :						\
  WrapperBase<cls::BaseType>(new cls::BaseType args, true),	\
    parent_(nullptr), vrefs(), mrefs() {}
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
#define WRAP_METHOD_CAST_VITER(cls, name, argsT, args, type, mods)	\
  type cls::name argsT mods {						\
    return type(((cls::BaseType*)(this->val_))->name args,		\
		const_cast<cls*>(this));				\
  }
#define WRAP_METHOD_CAST_MITER(cls, name, argsT, args, type, mods)	\
  type cls::name argsT mods {						\
    return type(*(((cls::BaseType*)(this->val_))->name args),		\
		const_cast<cls*>(this));				\
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
#define WRAP_GET_STRING(name)						\
  const WValue WValue::Get ## name ## String() {			\
    return WValue(const_cast<WValue::BaseType*>(&(WValue::BaseType::Get ## name ## String())));	\
  }

////////////////////////////////////////////////////////////////////
// WrapperBase
////////////////////////////////////////////////////////////////////

template<typename T>
WrapperBase<T>::~WrapperBase() {
  if (created_val)
    delete val_;
  val_ = nullptr;
}

#define WRAPPER_CLASS(base)						\
  namespace rapidjson {							\
    template class WrapperBase<RJ_WNS::base>;				\
  }
#define WRAPPER_CLASS_TEMP(base)					\
  namespace rapidjson {							\
    template class WrapperBase<UNPACK_BKTS base >;			\
  }
#define WRAPPER_METHODS_MOVE_CONSTRUCTOR_BASE(cls, base, tempT, temp, macroB, macroT, macro) \
  macroT(tempT)								\
  cls macro(temp)::cls(macroB base&& val) :				\
    WrapperBase<macroB base>(new macroB base(std::forward<macroB base>(val)), true) {}
#define WRAPPER_METHODS_MOVE_CONSTRUCTOR_TEMP(cls, base, tempT, temp)	\
  WRAPPER_METHODS_MOVE_CONSTRUCTOR_BASE(cls, base, tempT, temp, UNPACK_BKTS, ADD_BKTS_T, ADD_BKTS)
#define WRAPPER_METHODS_MOVE_CONSTRUCTOR(cls, base)			\
  WRAPPER_METHODS_MOVE_CONSTRUCTOR_BASE(cls, base, , , ,		\
					DO_NOTHING, DO_NOTHING)
#define WRAPPER_METHODS_STRICT(cls, base, tempT, temp, macroB, macroT, macro) \
  macroT(tempT)								\
  cls macro(temp)::cls(macroB base* val) :				\
  WrapperBase<macroB base>(val, false) {}				\
  macroT(tempT)								\
  cls macro(temp)::cls(macroB base& val) :				\
  WrapperBase<macroB base>(val) {}					\
  macroT(tempT)								\
  cls macro(temp)::cls(cls macro(temp)&& rhs) :				\
  WrapperBase<macroB base>(std::forward<WrapperBase<macroB base>>(rhs)) {} \
  macroT(tempT)								\
  cls macro(temp)& cls macro(temp)::operator=(cls macro(temp)& rhs) {	\
    WrapperBase<macroB base>::operator=(rhs);				\
    return *this;							\
  }
  
#define WRAPPER_METHODS_BASE(cls, base, tempT, temp, macroB, macroT, macro) \
  macroT(tempT)								\
  cls macro(temp)& cls macro(temp)::operator=(cls macro(temp)&& rhs) {	\
    return *this = rhs.Move();						\
  }									\
  macroT(tempT)								\
  cls macro(temp)& cls macro(temp)::Move() {				\
    return *this;							\
  }
#define WRAPPER_METHODS_TEMP(cls, base, tempT, temp)		\
  WRAPPER_METHODS_BASE(cls, base, tempT, temp,			\
		       UNPACK_BKTS, ADD_BKTS_T, ADD_BKTS)	\
  WRAPPER_METHODS_STRICT(cls, base, tempT, temp,			\
			 UNPACK_BKTS, ADD_BKTS_T, ADD_BKTS)
#define WRAPPER_METHODS(base)						\
  WRAPPER_METHODS_BASE(W ## base, RJ_WNS::base, , , , DO_NOTHING, DO_NOTHING) \
  WRAPPER_METHODS_STRICT(W ## base, RJ_WNS::base, , , , DO_NOTHING, DO_NOTHING)

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

  WRAPPER_CLASS(StringRefType);
  WRAPPER_METHODS(StringRefType);
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

WRAPPER_CLASS(Value);

bool WValue::operator==(const WValue& rhs) const {
  return ((*val_) == (*rhs.val_));
}
bool WValue::operator!=(const WValue& rhs) const {
  return !(*this == rhs);
}

WValue& WValue::childRef(RJ_WNS::Value* x) {
  if (parent_)
    return parent_->childRef(x);
  for (std::vector<WValue>::iterator it = vrefs.begin(); it != vrefs.end(); it++) {
    if (x == it->val_)
      return (*it);
  }
  vrefs.emplace_back(x); //, this);
  return vrefs[vrefs.size() - 1];
}
WMember& WValue::childRef(MEMBER* x) {
  if (parent_)
    return parent_->childRef(x);
  for (std::vector<WMember>::iterator it = mrefs.begin(); it != mrefs.end(); it++) {
    if (x == it->val_)
      return (*it);
  }
  mrefs.emplace_back(x); //, this);
  return mrefs[mrefs.size() - 1];
}
WValue* WValue::_getPtr() {
  return this;
}
const WValue* WValue::_getPtr() const {
  return this;
}
WValue* WValue::operator & () {
  if (parent_)
    return childRef(val_)._getPtr();
  return this;
}
const WValue* WValue::operator & () const {
  if (parent_)
    return const_cast<WValue*>(parent_)->childRef(const_cast<RJ_WNS::Value*>(val_))._getPtr();
  return this;
}

WValue& WValue::CopyInto(WValue& rhs, Allocator& allocator,
			 bool copyConstStrings) const {
  return rhs.CopyFrom(*this, allocator, copyConstStrings);
}
RJ_WNS::Value& WValue::CopyInto(RJ_WNS::Value& rhs, Allocator& allocator,
				bool copyConstStrings) const {
  return rhs.CopyFrom(*val_, allocator, copyConstStrings);
}
WValue& WValue::CopyFrom(const RJ_WNS::Value& rhs, Allocator& allocator,
			 bool copyConstStrings) {
  val_->CopyFrom(rhs, allocator, copyConstStrings);
  return *this;
}

  WRAPPER_METHODS_BASE(WValue, RJ_WNS::Value,
		       , , , DO_NOTHING, DO_NOTHING)
  
  WRAP_CONSTRUCTOR_VAL(WValue, (Type type), (type));
  WRAP_CONSTRUCTOR_VAL(WValue,
		       (const WValue::Ch* str, SizeType len,
			WValue::Allocator& allocator),
		       (str, len, allocator));
  WRAP_CONSTRUCTOR_VAL(WValue, (const Ch* str, SizeType len), (str, len));
  WRAP_CONSTRUCTOR_VAL(WValue,
		       (const std::string& s, Allocator& allocator),
		       (s, allocator));
  WRAP_CONSTRUCTOR_VAL(WValue,
		       (const WValue& rhs, WValue::Allocator& allocator,
			bool copyConstStrings),
		       (*(rhs.val_), allocator, copyConstStrings));
  WRAP_CONSTRUCTOR_VAL(WValue,
		       (PyObject* pyobj, Allocator& allocator),
		       (pyobj, allocator));
  WRAP_CONSTRUCTOR_VAL(WValue,
		       (const ObjWavefront& x, Allocator& allocator),
		       (x, allocator));
  WRAP_CONSTRUCTOR_VAL(WValue,
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
  WRAP_METHOD(WValue, GetType, (), (), rapidjson::Type, const);
  WRAP_METHOD_SELF(WValue, SetNull, (), (), );
  WRAP_METHOD(WValue, IsNull, (), (), bool, const);
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
  WRAP_METHOD(WValue, GetElement,
	      (const SizeType index, WValue& dst, Allocator& allocator),
	      (index, *(dst.val_), allocator), bool, const);
  WRAP_METHOD(WValue, GetSubArray,
	      (const SizeType index, const SizeType dim,
	       WValue& dst, Allocator& allocator),
	      (index, dim, *(dst.val_), allocator), bool, const);
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
  WRAP_METHOD(WValue, IsScalar, (), (), bool, const);
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
  WRAP_METHOD(WValue, Is1DArray, (), (), bool, const);
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
  WRAP_METHOD(WValue, IsNDArray, (), (), bool, const);
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
  WRAP_METHOD_SELF(WValue, SetNDArray,
		   (const Ch* x, SizeType precision, SizeType shape[],
		    SizeType ndim, Allocator& allocator,
		    const Ch* encoding, SizeType encoding_len),
		   (x, precision, shape, ndim, allocator,
		    encoding, encoding_len), );
  // Array methods
  WRAP_METHOD(WValue, IsArray, (), (), bool, const);
  WRAP_METHOD_SELF(WValue, SetArray, (), (), );
  WRAP_METHOD(WValue, Size, (), (), rapidjson::SizeType, const);
  WRAP_METHOD(WValue, Empty, (), (), bool, const);
  WRAP_METHOD_SELF(WValue, Reserve, (SizeType newCapacity,
				     WValue::Allocator &allocator),
		   (newCapacity, allocator), );
  WRAP_METHOD_CAST_VITER(WValue, Erase, (WValue::ConstValueIterator pos),
			 (pos.ptr_), WValue::ValueIterator, );
  WRAP_METHOD_CAST_VITER(WValue, Begin, (), (),
			 WValue::ValueIterator, );
  WRAP_METHOD_CAST_VITER(WValue, End, (), (),
			 WValue::ValueIterator, );
  WRAP_METHOD_CAST_VITER(WValue, Begin, (), (),
			 WValue::ConstValueIterator, const);
  WRAP_METHOD_CAST_VITER(WValue, End, (), (),
			 WValue::ConstValueIterator, const);
  WRAP_METHOD(WValue, Contains, (const WValue& x),
	      (*(x.val_)), bool, const);
  INDEX_RTYPE WValue::operator[](SizeType index) {
    RJ_WNS::Value& tmp = this->val_->operator[](index);
    return INDEX_METHOD;
  }
  const INDEX_RTYPE WValue::operator[](SizeType index) const {
    const RJ_WNS::Value& tmp = this->val_->operator[](index);
    return INDEX_METHOD_CONST;
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
    (INDEX_RTYPE)) WValue::operator[](T* name) {
    RJ_WNS::Value& tmp = this->val_->operator[](name);
    return INDEX_METHOD;
  }
  template <typename T>
  RAPIDJSON_DISABLEIF_RETURN(
    (internal::NotExpr<
     internal::IsSame<typename internal::RemoveConst<T>::Type,
     typename WValue::Ch> >),
    (const INDEX_RTYPE)) WValue::operator[](T* name) const {
    const RJ_WNS::Value& tmp = this->val_->operator[](name);
    return INDEX_METHOD_CONST;
  }
  WRAP_METHOD_SELF(WValue, AddMember, (WValue& name,
				       WValue& value,
				       WValue::Allocator& allocator),
		   (*(name.val_), *(value.val_), allocator), );
  WRAP_METHOD_SELF(WValue, AddMember, (WValue& name,
				       WValue&& value,
				       WValue::Allocator& allocator),
		   (*(name.val_), value.val_->Move(), allocator), );
  WRAP_METHOD_SELF(WValue, AddMember, (WValue&& name,
				       WValue& value,
				       WValue::Allocator& allocator),
		   (name.val_->Move(), *(value.val_), allocator), );
  WRAP_METHOD_SELF(WValue, AddMember, (WValue&& name,
				       WValue&& value,
				       WValue::Allocator& allocator),
		   (name.val_->Move(), value.val_->Move(), allocator), );
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
		   (*(name.val_), value.val_->Move(), allocator), );
  WRAP_METHOD(WValue, RemoveMember, (const WValue::Ch* name),
	      (name), bool, );
  WRAP_METHOD_CAST_MITER(WValue, MemberBegin, (), (),
			 WValue::MemberIterator, );
  WRAP_METHOD_CAST_MITER(WValue, MemberEnd, (), (),
			 WValue::MemberIterator, );
  WRAP_METHOD_CAST_MITER(WValue, MemberBegin, (), (),
			 WValue::ConstMemberIterator, const);
  WRAP_METHOD_CAST_MITER(WValue, MemberEnd, (), (),
			 WValue::ConstMemberIterator, const);
  WRAP_METHOD_CAST_MITER(WValue, FindMember, (const Ch* name),
			 (name), WValue::MemberIterator, );
  WRAP_METHOD_CAST_MITER(WValue, FindMember, (const Ch* name),
			 (name), WValue::ConstMemberIterator,
			 const);
  WRAP_METHOD_CAST_MITER(WValue, FindMember, (const WValue& name),
			 (*(name.val_)), WValue::MemberIterator, );
  WRAP_METHOD_CAST_MITER(WValue, FindMember, (const WValue& name),
			 (*(name.val_)),
			 WValue::ConstMemberIterator, const);
#if RAPIDJSON_HAS_STDSTRING
  WRAP_METHOD_CAST_MITER(WValue, FindMember,
			 (const std::basic_string<Ch>& name), (name),
			 WValue::ConstMemberIterator, const);
#endif
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
  WRAP_METHOD(WValue, IsSchema, (), (), bool, const);
  WRAP_METHOD_SELF(WValue, SetSchema,
		   (Allocator& allocator),
		   (allocator), );
  WRAP_METHOD_SELF(WValue, SetSchema,
		   (const WValue& x, Allocator& allocator),
		   (*(x.val_), allocator), );
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
  WRAP_METHOD(WValue, AddSchemaMember,
	      (const WValue& key, const WValue& value),
	      (*(key.val_), *(value.val_)), void, );
  WRAP_METHOD(WValue, AddSchemaMember,
	      (const WValue& key, unsigned int value),
	      (*(key.val_), value), void, );
  WRAP_METHOD(WValue, AddSchemaMember,
	      (const WValue& key, const WValue::Ch* str, SizeType str_len),
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
  WRAP_METHOD(WDocument, GetParseError, (), (), ParseErrorCode, const);
  WRAP_METHOD(WDocument, GetErrorOffset, (), (), size_t, const);
  WRAP_METHOD(WDocument, CountVarArgs, (WValue& schema, bool set),
	      (*(schema.val_), set), size_t, const);
bool WDocument::SetVarArgs(WValue& schema, VarArgList& ap) const {
  return val_->ApplyVarArgs(*(schema.val_), ap, kSetVarArgsFlag, this);
}
bool WDocument::SetVarArgs(WValue* schema, ...) const {
  size_t nargs = CountVarArgs(*schema, true);
  RAPIDJSON_BEGIN_VAR_ARGS(ap, schema, &nargs, false);
  bool out = val_->ApplyVarArgs(*(schema->val_), ap,
				kSetVarArgsFlag, this);
  RAPIDJSON_END_VAR_ARGS(ap);
  return out;
}
bool WDocument::SetVarArgsRealloc(WValue& schema, VarArgList& ap) const {
  return val_->ApplyVarArgs(*(schema.val_), ap, kSetVarArgsFlag, this);
}
bool WDocument::SetVarArgsRealloc(WValue* schema, ...) const {
  size_t nargs = CountVarArgs(*schema, true);
  RAPIDJSON_BEGIN_VAR_ARGS(ap, schema, &nargs, true);
  bool out = val_->ApplyVarArgs(*(schema->val_), ap,
				kSetVarArgsFlag, this);
  RAPIDJSON_END_VAR_ARGS(ap);
  return out;
}
bool WDocument::GetVarArgs(WValue& schema, VarArgList& ap) {
  return val_->ApplyVarArgs(*(schema.val_), ap, kGetVarArgsFlag, this);
}
bool WDocument::GetVarArgs(WValue* schema, ...) {
  size_t nargs = CountVarArgs(*schema, false);
  RAPIDJSON_BEGIN_VAR_ARGS(ap, schema, &nargs, false);
  bool out = val_->ApplyVarArgs(*(schema->val_), ap,
				kGetVarArgsFlag, this);
  RAPIDJSON_END_VAR_ARGS(ap);
  return out;
}
  WRAP_METHOD(WDocument, FinalizeFromStack, (), (), void, );
  template<typename InputStream>
  WRAP_METHOD_SELF(WDocument, ParseStream, (InputStream& is), (is), );
  WRAP_HANDLER_METHODS(WDocument);

template WDocument& WDocument::ParseStream<StringStream>(StringStream& is);

////////////////////////////////////////////////////////////////////
// WMember
////////////////////////////////////////////////////////////////////

WMember::WMember(WMember::BaseType* rhs, WValue* parent) :
  name(&(rhs->name), parent), value(&(rhs->value), parent),
  val_(rhs), parent_(parent) {}
WMember::WMember(WMember&& rhs) :
  name(std::move(rhs.name)),
  value(std::move(rhs.value)),
  val_(std::move(rhs.val_)), parent_(std::move(rhs.parent_)) {}
WMember& WMember::operator=(WMember&& rhs) {
  return *this = static_cast<WMember&>(rhs);
}
WMember& WMember::operator=(WMember& rhs) {
  if (this != &rhs) {
    name = rhs.name;
    value = rhs.value;
    val_ = rhs.val_;
    parent_ = rhs.parent_;
  }
  return *this;
}

////////////////////////////////////////////////////////////////////
// IteratorWrapperBase
////////////////////////////////////////////////////////////////////

#define ITERATOR_INC_OP(op)						\
  template<typename T, bool Const>					\
  IteratorWrapperBase<T, Const>& IteratorWrapperBase<T, Const>::operator op ## op() { \
    ptr_ op ## op;							\
    return *this;							\
  }									\
  template<typename T, bool Const>					\
  IteratorWrapperBase<T, Const> IteratorWrapperBase<T, Const>::operator op ## op(int) { \
    IteratorWrapperBase<T, Const> old(ptr_, parent_);			\
    op ## op (*this);							\
    return old;								\
  }									\
  template<typename T, bool Const>					\
  IteratorWrapperBase<T, Const> IteratorWrapperBase<T, Const>::operator op(DifferenceType n) const { \
    return IteratorWrapperBase<T, Const>(ptr_ op n, parent_);		\
  }									\
  template<typename T, bool Const>					\
  IteratorWrapperBase<T, Const>& IteratorWrapperBase<T, Const>::operator op ## =(DifferenceType n) { \
    ptr_ op ## =(n);							\
    return *this;							\
  }
ITERATOR_INC_OP(+)
ITERATOR_INC_OP(-)
#undef ITERATOR_INC_OP

template<typename T, bool Const>
typename IteratorWrapperBase<T, Const>::Reference IteratorWrapperBase<T, Const>::operator*() const {
  return parent_->childRef(const_cast<typename IteratorWrapperBase<T, Const>::PlainTypeBase*>(ptr_));
}
template<typename T, bool Const>
typename IteratorWrapperBase<T, Const>::Pointer IteratorWrapperBase<T, Const>::operator->() const {
  return parent_->childRef(const_cast<typename IteratorWrapperBase<T, Const>::PlainTypeBase*>(ptr_))._getPtr();
}
template<typename T, bool Const>
typename IteratorWrapperBase<T, Const>::Reference IteratorWrapperBase<T, Const>::operator[](typename IteratorWrapperBase<T, Const>::DifferenceType n) const {
  return parent_->childRef(const_cast<typename IteratorWrapperBase<T, Const>::PlainTypeBase*>(&(ptr_[n])));
}

namespace rapidjson {
  template class IteratorWrapperBase<WValue, true>;
  template class IteratorWrapperBase<WValue, false>;
  template class IteratorWrapperBase<WMember, true>;
  template class IteratorWrapperBase<WMember, false>;
}

////////////////////////////////////////////////////////////////////
// WSchemaDocument
////////////////////////////////////////////////////////////////////

  WRAPPER_CLASS(SchemaDocument);
  WRAPPER_METHODS(SchemaDocument);
  WRAP_CONSTRUCTOR(WSchemaDocument, (WDocument& d),
		   (*((RJ_WNS::Document*)d.val_)));
  WRAP_CONSTRUCTOR(WSchemaDocument, (WValue& d), (*(d.val_)));

////////////////////////////////////////////////////////////////////
// WSchemaValidator
////////////////////////////////////////////////////////////////////

  WRAPPER_CLASS(SchemaValidator);
  WRAPPER_METHODS(SchemaValidator);
  WRAP_CONSTRUCTOR(WSchemaValidator, (WSchemaDocument& s),
		   (*((RJ_WNS::SchemaDocument*)s.val_)));
  WRAP_METHOD(WSchemaValidator, GenerateData, (WDocument& d),
	      (*((RJ_WNS::Document*)d.val_)), bool, );

////////////////////////////////////////////////////////////////////
// WSchemaNormalizer
////////////////////////////////////////////////////////////////////

  WRAPPER_CLASS(SchemaNormalizer);
  WRAPPER_METHODS(SchemaNormalizer);
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

  WRAPPER_CLASS(SchemaEncoder);
  WRAPPER_METHODS(SchemaEncoder);
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

  WRAPPER_CLASS_TEMP((RJ_WNS::PrettyWriter<rapidjson::StringBuffer>))
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
#undef WRAP_METHOD_CAST_VITER
#undef WRAP_METHOD_CAST_MITER
#undef WRAP_METHOD_CAST_CONST
#undef WRAP_SET_GET
#undef WRAP_GET_STRING
#undef WRAPPER_METHODS_MOVE_CONSTRUCTOR
#undef WRAPPER_METHODS_MOVE_CONSTRUCTOR_TEMP
#undef WRAPPER_METHODS_MOVE_CONSTRUCTOR_BASE
#undef WRAPPER_METHODS_BASE
#undef WRAPPER_METHODS_STRICT
#undef WRAPPER_METHODS_TEMP
#undef WRAPPER_METHODS
#undef WRAP_HANDLER_METHOD
#undef WRAP_HANDLER_METHODS
#undef WRAP_HANDLER_METHOD_TEMP
#undef WRAP_HANDLER_METHODS_TEMP
#undef MEMBER
#undef MEMBER_ITERATOR

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
#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
typedef PyObject* _PyObject_yggptr;
SPECIALIZE(_PyObject_yggptr);
#endif
SPECIALIZE(rapidjson::Ply);
SPECIALIZE(rapidjson::ObjWavefront);
// vector
// map

#undef SPECIALIZE_SCALAR
#undef SPECIALIZE
#undef SPECIALIZE_BASE

#else // WRAP_RAPIDJSON_FOR_DLL
#include "rapidjson/pyrj.h"
#endif // WRAP_RAPIDJSON_FOR_DLL

#ifndef YGG_LINK_PYTHON_TO_CPP
#define RAPIDJSON_WRAPPER_DEFS_
#include "utils/rapidjson_wrapper.defs"
#endif

using namespace YggInterface::utils;

FilterBase::FilterBase() {}
FilterBase::~FilterBase() {}
bool FilterBase::operator()(const rapidjson::Document&) {
  ygglog_throw_error("FilterBase: operator() must be overridden");
  return false;
}
FilterBase* FilterBase::copy() const {
  ygglog_throw_error("FilterBase: copy must be overriden");
  return nullptr;
}

TransformBase::TransformBase() {}
TransformBase::~TransformBase() {}
bool TransformBase::operator()(rapidjson::Document&) {
  ygglog_throw_error("TransformBase: operator() must be overridden");
  return false;
}
TransformBase* TransformBase::copy() const {
  ygglog_throw_error("TransformBase: copy must be overriden");
  return nullptr;
}

FilterClass::FilterClass(filterFunc func) :
  FilterBase(), func_(func) {}
bool FilterClass::operator()(const rapidjson::Document& doc) {
  return func_(doc);
}
FilterBase* FilterClass::copy() const {
  return new FilterClass(func_);
}

TransformClass::TransformClass(const transformFunc& func) :
  TransformBase(), func_(func) {}
bool TransformClass::operator()(rapidjson::Document& doc) {
  return func_(doc);
}
TransformBase* TransformClass::copy() const {
  return new TransformClass(func_);
}

PyFilterClass::PyFilterClass(const PyObject* func) :
  FilterBase(), PyBaseFunc(func) {}
bool PyFilterClass::operator()(const rapidjson::Document& doc) {
  return PyBaseFunc::_call(doc);
}
FilterBase* PyFilterClass::copy() const {
#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
  return new PyFilterClass(func_);
#else // YGGDRASIL_DISABLE_PYTHON_C_API
  ygglog_throw_error("PyFilterClass: Python disabled");
  return nullptr;
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
}

PyTransformClass::PyTransformClass(const PyObject* func) :
  TransformBase(), PyBaseFunc(func) {}
bool PyTransformClass::operator()(rapidjson::Document& doc) {
  return PyBaseFunc::_call(doc, &doc);
}
TransformBase* PyTransformClass::copy() const {
#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
  return new PyTransformClass(func_);
#else // YGGDRASIL_DISABLE_PYTHON_C_API
  ygglog_throw_error("PyTransformClass: Python disabled");
  return nullptr;
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
}

#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
PyBaseFunc::PyBaseFunc(const PyObject* func) :
  func_(const_cast<PyObject*>(func)) {
  if (func_ == NULL) {
    ygglog_throw_error("PyBaseFunc: Provided Python function is NULL");
  }
  Py_INCREF(func_);
}
PyBaseFunc::~PyBaseFunc() { Py_DECREF(func_); }
bool PyBaseFunc::_call(const rapidjson::Document& doc,
		       rapidjson::Document* out) {
  bool res = false;
  PyGILState_STATE gstate;
  std::string err;
  PyObject *pyDoc = NULL, *args = NULL, *resPy = NULL;
  gstate = PyGILState_Ensure();
  pyDoc = doc.GetPythonObjectRaw();
  if (pyDoc == NULL) {
    err = "PyBaseFunc: Could not convert input document to Python";  // GCOV_EXCL_LINE
    goto cleanup;  // GCOV_EXCL_LINE
  }
  args = PyTuple_Pack(1, pyDoc);
  if (args == NULL) {
    err = "PyBaseFunc: Failed to create arguments tuple";  // GCOV_EXCL_LINE
    goto cleanup;  // GCOV_EXCL_LINE
  }
  resPy = PyObject_Call(func_, args, NULL);
  Py_CLEAR(args);
  if (resPy == NULL) {
    err = "PyBaseFunc: Error in function call";
    goto cleanup;
  }
  if (out != nullptr) {
    Py_CLEAR(pyDoc);
    pyDoc = resPy;
    Py_INCREF(Py_True);
    resPy = Py_True;
  }
  if (!PyBool_Check(resPy)) {
    err = "PyBaseFunc: Result is not a boolean.";
    goto cleanup;
  }
  res = (resPy == Py_True);
  Py_CLEAR(resPy);
  if (out != nullptr &&
      !out->SetPythonObjectRaw(pyDoc, out->GetAllocator())) {
    err = "PyBaseFunc: Error setting document from Python";  // GCOV_EXCL_LINE
    goto cleanup;  // GCOV_EXCL_LINE
  }
 cleanup:
  Py_XDECREF(pyDoc);
  Py_XDECREF(args);
  Py_XDECREF(resPy);
  PyGILState_Release(gstate);
  if (!err.empty())
    ygglog_throw_error(err.c_str());
  return res;
}
#else
PyBaseFunc::PyBaseFunc(const PyObject*) {
  ygglog_throw_error("PyBaseFunc: Python API disabled");
}
PyBaseFunc::~PyBaseFunc() {}
bool PyBaseFunc::_call(const rapidjson::Document&,
		       rapidjson::Document*) {
  ygglog_throw_error("PyBaseFunc: Python API disabled");
  return false;
}
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
