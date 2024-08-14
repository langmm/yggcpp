
# LINES AFTER THIS WERE GENERATED AND SHOULD NOT BE MODIFIED DIRECTLY
#####################################################################
module YggInterface
using CxxWrap
const exclude = [:__init__, :eval, :include]
module librapidjson
  using CxxWrap
  import Base: |, convert, promote_rule
  @wrapmodule(() -> "libYggInterface_julia.dylib",:define_module_rapidjson)
  function __init__()
    @initcxx
  end
  Value(i::CxxLong) = _construct(i)
  Value(u::CxxULong) = _construct(u)
  Value(i64::Core.Int64) = _construct(i64)
  Value(u64::Core.UInt64) = _construct(u64)
  Value(s::Cstring, length::CxxULong) = _construct(s, length)
  function convert(::Type{ <: String }, x::Value)
    if (!IsString(x))
      throw("Value is not a String")
    end
    return convert(String, GetString(x))
  end
  function Value(y::String)
    x = Value()
    SetString(x, y)
    return x
  end
  function SetValue(x::Value, y::String)
    SetString(x, y)
  end
  function convert(::Type{ <: Core.Int64 }, x::Value)
    if (!IsInt64(x))
      throw("Value is not a Int64")
    end
    return convert(Core.Int64, GetInt64(x))
  end
  function Value(y::Core.Int64)
    x = Value()
    SetInt64(x, y)
    return x
  end
  function SetValue(x::Value, y::Core.Int64)
    SetInt64(x, y)
  end
  function convert(::Type{ <: Core.UInt64 }, x::Value)
    if (!IsUint64(x))
      throw("Value is not a Uint64")
    end
    return convert(Core.UInt64, GetUint64(x))
  end
  function Value(y::Core.UInt64)
    x = Value()
    SetUint64(x, y)
    return x
  end
  function SetValue(x::Value, y::Core.UInt64)
    SetUint64(x, y)
  end
  function convert(::Type{ <: CxxLong }, x::Value)
    if (!IsInt(x))
      throw("Value is not a Int")
    end
    return convert(CxxLong, GetInt(x))
  end
  function Value(y::CxxLong)
    x = Value()
    SetInt(x, y)
    return x
  end
  function SetValue(x::Value, y::CxxLong)
    SetInt(x, y)
  end
  function convert(::Type{ <: CxxULong }, x::Value)
    if (!IsUint(x))
      throw("Value is not a Uint")
    end
    return convert(CxxULong, GetUint(x))
  end
  function Value(y::CxxULong)
    x = Value()
    SetUint(x, y)
    return x
  end
  function SetValue(x::Value, y::CxxULong)
    SetUint(x, y)
  end
  function convert(::Type{ <: Float32 }, x::Value)
    if (!IsFloat(x))
      throw("Value is not a Float")
    end
    return convert(Float32, GetFloat(x))
  end
  function Value(y::Float32)
    x = Value()
    SetFloat(x, y)
    return x
  end
  function SetValue(x::Value, y::Float32)
    SetFloat(x, y)
  end
  function convert(::Type{ <: Float64 }, x::Value)
    if (!IsDouble(x))
      throw("Value is not a Double")
    end
    return convert(Float64, GetDouble(x))
  end
  function Value(y::Float64)
    x = Value()
    SetDouble(x, y)
    return x
  end
  function SetValue(x::Value, y::Float64)
    SetDouble(x, y)
  end
  function extract(x::Value)
    if (IsString(x))
      return convert(String, x)
    end
    if (IsInt64(x))
      return convert(Core.Int64, x)
    end
    if (IsUint64(x))
      return convert(Core.UInt64, x)
    end
    if (IsInt(x))
      return convert(CxxLong, x)
    end
    if (IsUint(x))
      return convert(CxxULong, x)
    end
    if (IsFloat(x))
      return convert(Float32, x)
    end
    if (IsDouble(x))
      return convert(Float64, x)
    end
    return x
  end
  function convert(::Type{ <: String }, x::Document)
    if (!IsString(x))
      throw("Document is not a String")
    end
    return convert(String, GetString(x))
  end
  function Document(y::String)
    x = Document()
    SetString(x, y)
    return x
  end
  function SetDocument(x::Document, y::String)
    SetString(x, y)
  end
  function convert(::Type{ <: Core.Int64 }, x::Document)
    if (!IsInt64(x))
      throw("Document is not a Int64")
    end
    return convert(Core.Int64, GetInt64(x))
  end
  function Document(y::Core.Int64)
    x = Document()
    SetInt64(x, y)
    return x
  end
  function SetDocument(x::Document, y::Core.Int64)
    SetInt64(x, y)
  end
  function convert(::Type{ <: Core.UInt64 }, x::Document)
    if (!IsUint64(x))
      throw("Document is not a Uint64")
    end
    return convert(Core.UInt64, GetUint64(x))
  end
  function Document(y::Core.UInt64)
    x = Document()
    SetUint64(x, y)
    return x
  end
  function SetDocument(x::Document, y::Core.UInt64)
    SetUint64(x, y)
  end
  function convert(::Type{ <: CxxLong }, x::Document)
    if (!IsInt(x))
      throw("Document is not a Int")
    end
    return convert(CxxLong, GetInt(x))
  end
  function Document(y::CxxLong)
    x = Document()
    SetInt(x, y)
    return x
  end
  function SetDocument(x::Document, y::CxxLong)
    SetInt(x, y)
  end
  function convert(::Type{ <: CxxULong }, x::Document)
    if (!IsUint(x))
      throw("Document is not a Uint")
    end
    return convert(CxxULong, GetUint(x))
  end
  function Document(y::CxxULong)
    x = Document()
    SetUint(x, y)
    return x
  end
  function SetDocument(x::Document, y::CxxULong)
    SetUint(x, y)
  end
  function convert(::Type{ <: Float32 }, x::Document)
    if (!IsFloat(x))
      throw("Document is not a Float")
    end
    return convert(Float32, GetFloat(x))
  end
  function Document(y::Float32)
    x = Document()
    SetFloat(x, y)
    return x
  end
  function SetDocument(x::Document, y::Float32)
    SetFloat(x, y)
  end
  function convert(::Type{ <: Float64 }, x::Document)
    if (!IsDouble(x))
      throw("Document is not a Double")
    end
    return convert(Float64, GetDouble(x))
  end
  function Document(y::Float64)
    x = Document()
    SetDouble(x, y)
    return x
  end
  function SetDocument(x::Document, y::Float64)
    SetDouble(x, y)
  end
  function extract(x::Document)
    if (IsString(x))
      return convert(String, x)
    end
    if (IsInt64(x))
      return convert(Core.Int64, x)
    end
    if (IsUint64(x))
      return convert(Core.UInt64, x)
    end
    if (IsInt(x))
      return convert(CxxLong, x)
    end
    if (IsUint(x))
      return convert(CxxULong, x)
    end
    if (IsFloat(x))
      return convert(Float32, x)
    end
    if (IsDouble(x))
      return convert(Float64, x)
    end
    return x
  end
  export YggSubType, YggEncodingType, Value, Document
  
end
using .librapidjson
for name in names(librapidjson; all=true)
  (name in exclude || !isdefined(librapidjson, name)) && continue
  startswith(string(name), "#") && continue
  startswith(string(name), "__cxxwrap") && continue
  @eval import .librapidjson: $name
  @eval export $name
end
module libYggInterface
  using CxxWrap
  import Base: |, convert, promote_rule
  @wrapmodule(() -> "libYggInterface_julia.dylib",:define_module_YggInterface)
  function __init__()
    @initcxx
  end
  convert(::Type{ CxxLong }, x::HeadFlags) = reinterpret(CxxLong, x)
  convert(::Type{ Core.Int64 }, x::COMM_FLAG) = reinterpret(Core.Int64, x)
  |(a::COMM_FLAG, b::COMM_FLAG) = (reinterpret(Core.Int64, a) | reinterpret(Core.Int64, b))
  |(a::HeadFlags, b::HeadFlags) = (reinterpret(CxxLong, a) | reinterpret(CxxLong, b))
  YggComm(nme::String, dirn::DIRECTION; flgs::Union{Core.Int64, COMM_FLAG}=0, type::COMM_TYPE=DEFAULT_COMM, wraptype::COMM_TYPE=NULL_COMM) = _construct(convert(StdString, nme), dirn, (convert(Core.Int64, flgs) | convert(Core.Int64, COMM_FLAG_INTERFACE)), type, wraptype)
  export COMM_TYPE, DIRECTION, CLEANUP_MODE, COMM_FLAG, LANGUAGE, HeadFlags, HEAD_RESET_MODE, SIGNON_STATUS, THREAD_STATUS, FORK_TYPE, FUNCTION_FLAGS, Comm_t, ygg_cleanup, YggComm, recv
  atexit( ygg_cleanup )
end
using .libYggInterface
for name in names(libYggInterface; all=true)
  (name in exclude || !isdefined(libYggInterface, name)) && continue
  startswith(string(name), "#") && continue
  startswith(string(name), "__cxxwrap") && continue
  @eval import .libYggInterface: $name
  @eval export $name
end
  function recv(self::Union{ YggComm }, not_generic::Core.Bool=false)
    data = Document()
    flag = recv(self, CxxRef(data), not_generic)
    return flag, extract(data)
  end
end