#ifdef INVALID_EMBED
#include "utils/embedded_r.hpp"
#include "utils/regex.hpp"

using namespace YggInterface::utils;

#ifdef YGG_EMBED_R
EMBEDED_LANGUAGE_DEFN_ENABLED(EmbeddedR, YGG_EMBED_R_TYPE, R_LANGUAGE, .R)
bool EmbeddedR::initialize_main() {
  R = (void*)(new RInside());
  if (check_error("initialize")) return false;
  return true;
}
bool EmbeddedR::finalize_main() {
  if (R) {
    RInside* Rcast = (RInside*)R;
    R = NULL;
    delete Rcast;
  }
  if (check_error("finalize")) return false;
  return true;
}
bool EmbeddedR::get_error(std::string&) const {
  return true;
}
bool EmbeddedR::preserve_embedded(void*&) const {
  bool out = false;
  // if (x) {
  // }
  out = true;
 cleanup:
  if (check_error("preserve_embedded")) return false;
  if (!out)
    log_error() << "preserve_embedded: Preservation failed" << std::endl;
  return out;
}
bool EmbeddedR::free_embedded(void*&) const {
  bool out = false;
  // if (x) {
  // }
  out = true;
 cleanup:
  if (check_error("free_embedded")) return false;
  if (!out)
    log_error() << "free_embedded: Free failed" << std::endl;
  return out;
}
std::string EmbeddedR::string_embedded(const void* x) const {
  std::string out;
  if (x) {
    SEXP* x_cast = (SEXP*)x;
    R().assign(x_cast[0], "temp");
    out = Rcpp::as<std::string>(R().parseEval("string(temp)"));
  }
  return out;
}
bool EmbeddedR::eval(const std::string& expr,
		     rapidjson::Document& result) const {
  SEXP ret_R;
  if (R().parseEval(expr, ret_R))
    return false;
  const void* ret = (const void*)(&ret_R);
  return convert_from(&ret, result, result.GetAllocator());
}
bool EmbeddedR::convert_to(const rapidjson::Value& v_in,
			   void*& v_out0, bool dont_preserve) const {
  v_out0 = NULL;
  YGG_EMBED_R_TYPE v_out = NULL;
#define CONVERT(check, set)				\
  if (check) {						\
    v_out = set;					\
    if (!v_out) {					\
      log_error() << "convert_to: Conversion to r failed" << std::endl; \
      check_error("convert_to");			\
      return false;					\
    }							\
  }
  // TODO: array, object, 1darray, ndarray, units
#define CONVERT_SCALAR(jtype, ctype)					\
  CONVERT(v_in.Is<ctype>(), R()jl_box_ ## jtype(v_in.Get<ctype>()))
  CONVERT_SCALAR(float64, double)
  else CONVERT_SCALAR(float32, float)
  else CONVERT_SCALAR(int64, int64_t)
  else CONVERT_SCALAR(int32, int32_t)
  else CONVERT_SCALAR(int16, int16_t)
  else CONVERT_SCALAR(int8, int8_t)
  else CONVERT_SCALAR(uint64, uint64_t)
  else CONVERT_SCALAR(uint32, uint32_t)
  else CONVERT_SCALAR(uint16, uint16_t)
  else CONVERT_SCALAR(uint8, uint8_t)
  else CONVERT(v_in.IsString(), jl_cstr_to_string(v_in.GetString()))
  else {
    log_error() << "convert_to: Unsupported type: " << v_in << std::endl;
    return false;
  }
#undef CONVERT_SCALAR
#undef CONVERT
  if (check_error("convert_to")) return false;
  v_out0 = (void*)v_out;
  if (!preserve_embedded(v_out0))
    return false;
  log_debug() << "convert_to: " << v_in << " -> " <<
    string_embedded(v_out0) << std::endl;
  return true;
}
bool EmbeddedR::convert_from(const void*& v_in0,
				 rapidjson::Value& v_out,
				 rapidjson::Value::AllocatorType& allocator) const {
  YGG_EMBED_R_TYPE v_in = (YGG_EMBED_R_TYPE)v_in0;
#define CONVERT(check, set)					\
  if (check) {							\
    set;							\
    if (check_error("convert_from")) {				\
      log_error() << "convert_from: Conversion from R failed" << std::endl; \
      return false;						\
    }								\
  }
#define CONVERT_SCALAR(jtype, ctype)			\
  CONVERT(jl_typeis(v_in, jl_ ## jtype ## _type),	\
	  v_out.Set<ctype>(jl_unbox_ ## jtype(v_in), allocator))
  CONVERT_SCALAR(float64, double)
  else CONVERT_SCALAR(float32, float)
  else CONVERT_SCALAR(int64, int64_t)
  else CONVERT_SCALAR(int32, int32_t)
  else CONVERT_SCALAR(int16, int16_t)
  else CONVERT_SCALAR(int8, int8_t)
  else CONVERT_SCALAR(uint64, uint64_t)
  else CONVERT_SCALAR(uint32, uint32_t)
  else CONVERT_SCALAR(uint16, uint16_t)
  else CONVERT_SCALAR(uint8, uint8_t)
  else CONVERT(jl_typeis(v_in, jl_string_type),
	       v_out.Set(jl_string_ptr(v_in), allocator))
  else {
    log_error() << "convert_from: Unsupported type: \"" <<
      jl_typeof_str(v_in) << "\"" << std::endl;
    return false;
  }
#undef CONVERT_SCALAR
#undef CONVERT
  if (check_error("convert_from")) return false;
  log_debug() << "convert_from: " << string_embedded(v_in0) <<
    " -> " << v_out << std::endl;
  return true;
}
void* EmbeddedR::load_function(const std::string& name) const {
  void* out = NULL;
  jl_module_t* mod = NULL;
  std::string eval_msg, func_name;
  std::vector<std::string> parts = split(name, "::", 2, true);
  if (parts.size() != 2 && parts.size() != 3) {
    log_error() << "load_function: Failed to decifer function location from \"" << name << "\"" << std::endl;
    goto cleanup;
  }
  eval_msg = "include(\"" + parts[0] + "\")";
  jl_eval_string(eval_msg.c_str());
  if (check_error("load_function")) return NULL;
  if (parts.size() == 3) {
    mod = (jl_module_t*)jl_eval_string(parts[1].c_str());
    func_name = parts[2];
  } else {
    mod = jl_main_module;
    func_name = parts[1];
  }
  if (!mod) goto cleanup;
  out = (void*)jl_get_function(mod, func_name.c_str());
  if (out)
    preserve_embedded(out);
 cleanup:
  if (check_error("load_function")) return NULL;
  if (!out)
    log_error() << "load_function: Failed to load function \"" << name << "\"" << std::endl;
  return out;
}
bool EmbeddedR::call_function(void* func,
				  const rapidjson::Document& args,
				  rapidjson::Document& result) const {
  bool out = false;
  jl_value_t** args_ = NULL;
  jl_value_t* result_ = NULL;
  size_t i = 0, nargs = 0;
  if (!func)
    goto cleanup;
  if (args.IsArray()) {
    nargs = static_cast<size_t>(args.Size());
  } else {
    nargs = 1;
  }
  args_ = (jl_value_t**)malloc(nargs * sizeof(jl_value_t*));
  if (!args_)
    goto cleanup;
  for (i = 0; i < nargs; i++) {
    args_[i] = NULL;
  }
  if (args.IsArray()) {
    for (i = 0; i < nargs; i++) {
      void* iarg = NULL;
      if (!convert_to(args[i], iarg)) {
	log_error() << "call_function: Error converting argument " << i <<
	  " to R value" << std::endl;
	goto cleanup;
      }
      args_[i] = (jl_value_t*)iarg;
    }
  } else {
    void* iarg = NULL;
    if (!convert_to(args, iarg)) {
      log_error() << "call_function: Error converting argument to R value" << std::endl;
      goto cleanup;
    }
    args_[0] = (jl_value_t*)iarg;
  }
  {
    JL_GC_PUSHARGS(args_, nargs);
    std::cerr << "before jl_call nargs = " << nargs << std::endl;
    result_ = jl_call((jl_function_t*)func, args_, nargs);
    std::cerr << "after jl_call" << std::endl;
    JL_GC_POP();
  }
  if (!result_) {
    log_error() << "call_function: Error in jl_call" << std::endl;
    goto cleanup;
  }
  {
    JL_GC_PUSH1(&result_);
    const void* result_void = (const void*)result_;
    out = convert_from(result_void, result, result.GetAllocator());
    JL_GC_POP();
  }
 cleanup:
  if (args_) {
    for (size_t i = 0; i < nargs; i++) {
      if (args_[i]) {
	void* iarg = args_[i];
	if (!free_embedded(iarg))
	  out = false;
	args_[i] = NULL;
      }
    }
    free(args_);
  }
  if (check_error("call_function")) return false;
  if (!out)
    log_error() << "call_function: Failed to call function" << std::endl;
  return out;
}
#else
EMBEDED_LANGUAGE_DEFN_DISABLED(EmbeddedR, YGG_EMBED_R_TYPE, R_LANGUAGE, .R)
#endif
#endif
