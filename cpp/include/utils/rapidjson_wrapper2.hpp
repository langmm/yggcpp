
// LINES AFTER THIS WERE GENERATED AND SHOULD NOT BE MODIFIED DIRECTLY
//====================================================================
#include <type_traits>


namespace wrap {
  namespace rapidjson {
  class Value;
  class Document;
  template< bool Const >
class GenericArray;
  template< bool Const >
class GenericObject;
}
}

template<class T>
struct is_wrapper_class {
  static const bool value = false;
  typedef T type;
};
template<class T>
struct is_wrapped_class {
  static const bool value = false;
  typedef T type;
};
template<class T>
constexpr bool is_wrapper_class_v = is_wrapper_class<T>::value;
template<class T>
constexpr bool is_wrapped_class_v = is_wrapped_class<T>::value;

template<typename T,
         std::enable_if_t<!is_wrapper_class<T>::value, bool> = true >
T _unwrap(T x) {
  return x;
}
template<typename T,
         std::enable_if_t<is_wrapper_class<T>::value, bool> = true >
typename is_wrapper_class<T>::type _unwrap(T x) {
  return x.val_;
}
template<typename T,
         std::enable_if_t<!is_wrapped_class<T>::value, bool> = true >
T _wrap(T x) {
  return x;
}
template<typename T,
         std::enable_if_t<is_wrapped_class<T>::value, bool> = true >
typename is_wrapped_class<T>::type _wrap(T x) {
  return typename is_wrapped_class<T>::type(x);
}

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
  template<typename V>  V _wrap_return(V x);  template<typename V>  V _wrap_arg(V x);  T* val_;
  bool created_val;
};

namespace rapidjson {
  class Value : public WrapperBase< wrap::rapidjson::Value > {
public:
  typedef WrapperBase< wrap::rapidjson::Value > BaseType;
  Value(Value* val, bool created=false) :
    val_(val), created_val(created) {}
  Value(Value& val) :
    Value(&val) {}
  Value(Value&& rhs) :
    Value(nullptr) {
    std::swap(val_, rhs.val_);
    std::swap(created_val, rhs.created_val);
  }  ~Value();
  Value& operator=(Value& rhs) {
    std::swap(val_, rhs.val_);
    std::swap(created_val, rhs.created_val);
    return *this;
  }
  Value* val_;
  bool created_val;
    
    bool IsObject() const;
    
    bool IsArray()  const;
    
    Value SetObject();
    
    Object GetObject();
    
    ConstObject GetObject() const;
    
    Value SetArray();
    
    Array GetArray();
    
    ConstArray GetArray() const;
    
    bool Is1DArray() const;
    
    bool IsNDArray() const;
    
    Value();
    
    explicit Value(int (i));
    
    explicit Value(unsigned (u));
    
    explicit Value(int64_t (i64));
    
    explicit Value(uint64_t (u64));
    
    explicit Value(double (d));
    
    explicit Value(float (f));
    
    Value(const char* (s), unsigned (length));
    
    Value(Array (a));
    
    Value(Object (o));
    ~Value();
    
    Value& operator=(Value& (rhs));
    
    Value& operator=(Value&& (rhs));
    
    Value Swap(Value& (other));
    
    bool operator==(const char* (rhs)) const;
    
    bool operator==(const std::basic_string< char >& (rhs)) const;
    
    bool IsNull()   const;
    
    bool IsFalse()  const;
    
    bool IsTrue()   const;
    
    bool IsBool()   const;
    
    bool IsNumber() const;
    
    bool IsInt()    const;
    
    bool IsUint()   const;
    
    bool IsInt64()  const;
    
    bool IsUint64() const;
    
    bool IsDouble() const;
    
    bool IsString() const;
    
    bool IsLosslessDouble() const;
    
    bool IsFloat() const;
    
    bool IsLosslessFloat() const;
    
    Value SetNull();
    
    bool GetBool() const;
    
    Value SetBool(bool (b));
    
    unsigned MemberCount() const;
    
    unsigned MemberCapacity() const;
    
    bool ObjectEmpty() const;
    
    Value& operator[](const std::basic_string< char >& (name));
    
    const Value& operator[](const std::basic_string< char >& (name)) const;
    
    bool HasMember(const char* (name)) const;
    
    bool HasMember(const std::basic_string< char >& (name)) const;
    
    void RemoveAllMembers();
    
    bool RemoveMember(const char* (name));
    
    bool RemoveMember(const std::basic_string< char >& (name));
    
    bool EraseMember(const char* (name));
    
    bool EraseMember(const std::basic_string< char >& (name));
    
    Object GetObj();
    
    ConstObject GetObj() const;
    
    unsigned Size() const;
    
    unsigned Capacity() const;
    
    bool Empty() const;
    
    void Clear();
    
    Value& operator[](unsigned (index));
    
    const Value& operator[](unsigned (index)) const;
    
    Value PopBack();
    
    bool Contains(const Value& (x)) const;
    
    int GetInt() const;
    
    unsigned GetUint() const;
    
    int64_t GetInt64() const;
    
    uint64_t GetUint64() const;
    
    double GetDouble() const;
    
    float GetFloat() const;
    
    Value SetInt(int (i));
    
    Value SetUint(unsigned (u));
    
    Value SetInt64(int64_t (i64));
    
    Value SetUint64(uint64_t (u64));
    
    Value SetDouble(double (d));
    
    Value SetFloat(float (f));
    
    const char* GetString() const;
    
    unsigned GetStringLength() const;
    
    Value SetString(const char* (s), unsigned (length));
    
    static const Value YggSubTypeString(YggSubType (subtype));
    
    void DestroySchema();
    
    bool HasSchema() const;
    
    bool HasUnits() const;
    
    bool SetUnits(const std::basic_string< char > (units));
    
    bool SetUnits(const char* (units_str), const unsigned (units_len)=0);
    
    const Value GetUnits() const;
    
    bool HasSchemaNested() const;
    
    void RawAssignSchema(Value& (rhs));
    
    void AddSchemaMember(const Value& (key), const Value& (value));
    
    void AddSchemaMember(const Value& (key), unsigned int (value));
    
    void AddSchemaMember(const Value& (key), const char* (str), unsigned (str_len));
    
    const Value GetYggType() const;
    
    bool RequiresPython() const;
    
    bool IsYggdrasil() const;
    
    bool HasTitle() const;
    
    const Value GetTitle() const;
    
    bool IsSubType(const char* (subtype), unsigned (precision)) const;
    
    bool IsType(const char* (type)) const;
    
    bool IsScalar() const;
    
    bool IsScalar(const char* (subT)) const;
    
    bool IsPythonClass() const;
    
    bool IsPythonFunction() const;
    
    bool IsPythonInstance() const;
    
    bool IsObjWavefront() const;
    
    bool IsPly() const;
    
    bool IsSchema() const;
    
    YggSubType GetSubTypeCode() const;
    
    int GetSubTypeNumpyType(Value& (enc)) const;
    
    const Value GetSubType() const;
    
    const char* GetSubType(unsigned& (length)) const;
    
    bool HasPrecision() const;
    
    unsigned GetPrecision() const;
    
    bool HasEncoding() const;
    
    const Value GetEncoding() const;
    
    const Value GetShape() const;
    
    unsigned GetNElements() const;
    
    void* GetDataPtr(bool& (requires_freeing)) const;
    
    unsigned GetNBytes() const;
    
    Object GetSchema();
};
template<>
struct is_wrapper_class<Value> {
  static const bool value = true;
  typedef Value type;
};
template<>
struct is_wrapped_class<Value> {
  static const bool value = true;
  typedef Value type;
};

  class Document : public  : public Value<Encoding, Allocator>< Encoding, Allocator > {
public:
  typedef  : public Value<Encoding, Allocator>< Encoding, Allocator > BaseType;
  Document(Document* val, bool created=false) :
    val_(val), created_val(created) {}
  Document(Document& val) :
    Document(&val) {}
  Document(Document&& rhs) :
    Document(nullptr) {
    std::swap(val_, rhs.val_);
    std::swap(created_val, rhs.created_val);
  }  ~Document();
  Document& operator=(Document& rhs) {
    std::swap(val_, rhs.val_);
    std::swap(created_val, rhs.created_val);
    return *this;
  }
  Document* val_;
  bool created_val;
    
    bool StartObject();
    
    bool EndObject(unsigned (memberCount));
    
    bool StartArray();
    
    bool EndArray(unsigned (elementCount));
    
    bool YggdrasilEndObject(unsigned (memberCount));
    ~Document();
    
    Document& operator=(Document&& (rhs));
    
    Document Swap(Document& (rhs));
    
    bool Null();
    
    bool Bool(bool (b));
    
    bool Int(int (i));
    
    bool Uint(unsigned (i));
    
    bool Int64(int64_t (i));
    
    bool Uint64(uint64_t (i));
    
    bool Double(double (d));
    
    bool RawNumber(const char* (str), unsigned (length), bool (copy));
    
    bool FromYggdrasilString(const char* (str), unsigned (length), bool (copy));
    
    bool WasFinalized() const;
    
    void ConsolidateStack();
    
    void FinalizeFromStack();
    
    bool String(const char* (str), unsigned (length), bool (copy));
    
    bool Key(const char* (str), unsigned (length), bool (copy));
    
    Document Parse(const char* (str), Document& (schema));
};
template<>
struct is_wrapper_class<Document> {
  static const bool value = true;
  typedef Document type;
};
template<>
struct is_wrapped_class<Document> {
  static const bool value = true;
  typedef Document type;
};

  template< bool Const >
class GenericArray : public WrapperBase< wrap::rapidjson::GenericArray > {
public:
  typedef WrapperBase< wrap::rapidjson::GenericArray > BaseType;
  GenericArray(GenericArray* val, bool created=false) :
    val_(val), created_val(created) {}
  GenericArray(GenericArray& val) :
    GenericArray(&val) {}
  GenericArray(GenericArray&& rhs) :
    GenericArray(nullptr) {
    std::swap(val_, rhs.val_);
    std::swap(created_val, rhs.created_val);
  }  ~GenericArray();
  GenericArray& operator=(GenericArray& rhs) {
    std::swap(val_, rhs.val_);
    std::swap(created_val, rhs.created_val);
    return *this;
  }
  GenericArray* val_;
  bool created_val;
    
    GenericArray(const GenericArray& (rhs));
    
    GenericArray& operator=(const GenericArray& (rhs));
    ~GenericArray();
    
    unsigned Size() const;
    
    unsigned Capacity() const;
    
    bool Empty() const;
    
    void Clear() const;
    
    Value& operator[](unsigned (index)) const;
    
    GenericArray Reserve(unsigned (newCapacity), AllocatorType& (allocator)) const;
    
    GenericArray PushBack(Value& (value), AllocatorType& (allocator)) const;
    
    GenericArray PushBack(Value&& (value), AllocatorType& (allocator)) const;
    
    GenericArray PopBack() const;
};
template<>
struct is_wrapper_class<GenericArray> {
  static const bool value = true;
  typedef GenericArray type;
};
template<>
struct is_wrapped_class<GenericArray> {
  static const bool value = true;
  typedef GenericArray type;
};

  template< bool Const >
class GenericObject : public WrapperBase< wrap::rapidjson::GenericObject > {
public:
  typedef WrapperBase< wrap::rapidjson::GenericObject > BaseType;
  GenericObject(GenericObject* val, bool created=false) :
    val_(val), created_val(created) {}
  GenericObject(GenericObject& val) :
    GenericObject(&val) {}
  GenericObject(GenericObject&& rhs) :
    GenericObject(nullptr) {
    std::swap(val_, rhs.val_);
    std::swap(created_val, rhs.created_val);
  }  ~GenericObject();
  GenericObject& operator=(GenericObject& rhs) {
    std::swap(val_, rhs.val_);
    std::swap(created_val, rhs.created_val);
    return *this;
  }
  GenericObject* val_;
  bool created_val;
    
    GenericObject(const GenericObject& (rhs));
    
    GenericObject& operator=(const GenericObject& (rhs));
    ~GenericObject();
    
    unsigned MemberCount() const;
    
    unsigned MemberCapacity() const;
    
    bool ObjectEmpty() const;
    
    Value& operator[](const std::basic_string< char >& (name)) const;
    
    GenericObject MemberReserve(unsigned (newCapacity), AllocatorType& (allocator)) const;
    
    bool HasMember(const char* (name)) const;
    
    bool HasMember(const std::basic_string< char >& (name)) const;
    
    GenericObject AddMember(Value& (name), Value& (value), AllocatorType& (allocator)) const;
    
    GenericObject AddMember(Value& (name), std::basic_string< char >& (value), AllocatorType& (allocator)) const;
    
    GenericObject AddMember(Value&& (name), Value&& (value), AllocatorType& (allocator)) const;
    
    GenericObject AddMember(Value&& (name), Value& (value), AllocatorType& (allocator)) const;
    
    GenericObject AddMember(Value& (name), Value&& (value), AllocatorType& (allocator)) const;
    
    void RemoveAllMembers();
    
    bool RemoveMember(const char* (name)) const;
    
    bool RemoveMember(const std::basic_string< char >& (name)) const;
    
    bool EraseMember(const char* (name)) const;
    
    bool EraseMember(const std::basic_string< char >& (name)) const;
};
template<>
struct is_wrapper_class<GenericObject> {
  static const bool value = true;
  typedef GenericObject type;
};
template<>
struct is_wrapped_class<GenericObject> {
  static const bool value = true;
  typedef GenericObject type;
};

}
