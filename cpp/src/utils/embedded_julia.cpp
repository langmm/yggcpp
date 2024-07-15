#include "utils/embedded_julia.hpp"
#include "utils/regex.hpp"

using namespace YggInterface::utils;

#ifdef YGG_EMBED_JULIA
EMBEDED_LANGUAGE_DEFN_ENABLED(EmbeddedJulia, YGG_EMBED_JULIA_TYPE, JULIA_LANGUAGE, .jl)
jl_value_t* EmbeddedJulia::deref_(jl_value_t* ref_x) const {
  // TODO: Check type?
  jl_function_t * getindex = jl_get_function(jl_base_module, "getindex");
  jl_value_t* x = jl_call1(getindex, ref_x);
  return x;
}
bool EmbeddedJulia::initialize_main() {
  jl_init();
  refs = (void*)jl_eval_string("refs = IdDict()");
  if (check_error("initialize_main")) return false;
  return true;
}
bool EmbeddedJulia::finalize_main() {
  jl_atexit_hook(0);
  refs = NULL; // TODO: Does anything else need done?
  if (check_error("finalize_main")) return false;
  return true;
}
bool EmbeddedJulia::initialize_thread() {
  if (jl_get_pgcstack() == NULL)
    jl_adopt_thread();
  if (check_error("initialize_thread")) return false;
  return true;
}
bool EmbeddedJulia::finalize_thread() {
  jl_wakeup_thread(0); // TODO: propogate error?
  if (check_error("finalize_thread")) return false;
  return true;
}
bool EmbeddedJulia::get_error(std::string& message) const {
  jl_value_t* exception = jl_exception_occurred();
  if (!exception)
    return false;
  message = (const char*)jl_unbox_voidpointer(jl_eval_string("pointer(sprint(showerror, ccall(:jl_exception_occurred, Any, ())))"));
  return true;
}
bool EmbeddedJulia::preserve_embedded(void*& x) const {
  bool out = false;
  if (x) {
    jl_value_t* var = (jl_value_t*)x;
    jl_function_t* setindex = jl_get_function(jl_base_module, "setindex!");
    if (!setindex) goto cleanup;
    jl_datatype_t* reft = (jl_datatype_t*)jl_eval_string("Base.RefValue{Any}");
    if (!reft) goto cleanup;
    JL_GC_PUSH1(&var);
    jl_value_t* rvar = jl_new_struct(reft, var);
    if (!rvar) {
      JL_GC_POP();
      goto cleanup;
    }
    jl_call3(setindex, (jl_value_t*)refs, rvar, rvar);
    JL_GC_POP();
    x = (void*)rvar;
  }
  out = true;
 cleanup:
  if (check_error("preserve_embedded")) return false;
  if (!out)
    log_error() << "preserve_embedded: Preservation failed" << std::endl;
  return out;
}
bool EmbeddedJulia::free_embedded(void*& x) const {
  bool out = false;
  if (x) {
    jl_value_t* rvar = (jl_value_t*)x;
    jl_function_t* fdelete = jl_get_function(jl_base_module, "delete!");
    if (!fdelete) goto cleanup;
    jl_call2(fdelete, (jl_value_t*)refs, rvar);
    x = nullptr;
  }
  out = true;
 cleanup:
  if (check_error("free_embedded")) return false;
  if (!out)
    log_error() << "free_embedded: Free failed" << std::endl;
  return out;
}
std::string EmbeddedJulia::string_embedded(const void* x) const {
  std::string out;
  if (x) {
    jl_function_t* jl_string = jl_get_function(jl_base_module, "string");
    jl_value_t* result = jl_call1(jl_string, (jl_value_t*) x);
    JL_GC_PUSH1(&result);
    out = std::string(jl_string_ptr(result));
    JL_GC_POP();
  }
  return out;
}
bool EmbeddedJulia::eval(const std::string& expr,
			 rapidjson::Document& result) const {
  const void* ret = (const void*)jl_eval_string(expr.c_str());
  return convert_from(ret, result, result.GetAllocator());
}
bool EmbeddedJulia::convert_to(const rapidjson::Value& v_in,
			       void*& v_out0,
			       bool dont_preserve) const {
  v_out0 = NULL;
  YGG_EMBED_JULIA_TYPE v_out = NULL;
#define CONVERT(check, set)				\
  if (check) {						\
    v_out = set;					\
    if (!v_out) {					\
      log_error() << "convert_to: Conversion to julia failed" << std::endl; \
      check_error("convert_to");			\
      return false;					\
    }							\
  }
  // TODO: array, object, 1darray, ndarray, units
#define CONVERT_SCALAR(jtype, ctype)					\
  CONVERT(v_in.Is<ctype>(), jl_box_ ## jtype(v_in.Get<ctype>()))
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
  if ((!dont_preserve) && (!preserve_embedded(v_out0)))
    return false;
  log_debug() << "convert_to: " << v_in << " -> " <<
    string_embedded(v_out0) << std::endl;
  return true;
}
bool EmbeddedJulia::convert_from(const void*& v_in0,
				 rapidjson::Value& v_out,
				 rapidjson::Value::AllocatorType& allocator) const {
  YGG_EMBED_JULIA_TYPE v_in = (YGG_EMBED_JULIA_TYPE)v_in0;
#define CONVERT(check, set)					\
  if (check) {							\
    set;							\
    if (check_error("convert_from")) {				\
      log_error() << "convert_from: Conversion from Julia failed" << std::endl; \
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
void* EmbeddedJulia::load_function(const std::string& name) const {
  void* out = NULL;
  jl_module_t* mod = NULL;
  std::string eval_msg, func_name;
  std::vector<std::string> parts = split(name, "::", 2, true);
  std::vector<std::string> ext_parts;
  if (parts.size() != 2 && parts.size() != 3) {
    log_error() << "load_function: Failed to decifer function location from \"" << name << "\"" << std::endl;
    goto cleanup;
  }
  ext_parts = split(parts[0], ".", 1, true);
  if (ext_parts.size() == 1 || ext_parts[0].size() == 0)
    parts[0] += ext;
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
bool EmbeddedJulia::call_function(void* func,
				  const rapidjson::Document& args,
				  rapidjson::Document& result) const {
  bool out = false;
  jl_value_t** args_ = NULL;
  jl_value_t* result_ = NULL;
  size_t i = 0, nargs = 0;
  if (!func)
    return false;
  if (args.IsArray()) {
    nargs = static_cast<size_t>(args.Size());
  } else {
    nargs = 1;
  }
  JL_GC_PUSHARGS(args_, nargs);
  // args_ = (jl_value_t**)malloc(nargs * sizeof(jl_value_t*));
  // if (!args_)
  //   goto cleanup;
  // for (i = 0; i < nargs; i++) {
  //   args_[i] = NULL;
  // }
  if (args.IsArray()) {
    for (i = 0; i < nargs; i++) {
      void* iarg = NULL;
      if (!convert_to(args[i], iarg, true)) {
	log_error() << "call_function: Error converting argument " << i <<
	  " to Julia value" << std::endl;
	goto cleanup;
      }
      args_[i] = (jl_value_t*)iarg;
      // args_[i] = deref_((jl_value_t*)iarg);
    }
  } else {
    void* iarg = NULL;
    if (!convert_to(args, iarg, true)) {
      log_error() << "call_function: Error converting argument to Julia value" << std::endl;
      goto cleanup;
    }
    args_[0] = (jl_value_t*)iarg;
    // args_[0] = deref_((jl_value_t*)iarg);
  }
  {
    jl_value_t* func_r = NULL;
    JL_GC_PUSH1(&func_r);
    func_r = deref_((jl_value_t*)func);
    result_ = jl_call((jl_function_t*)func_r, args_, nargs);
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
  JL_GC_POP();
  // if (args_) {
  //   for (size_t i = 0; i < nargs; i++) {
  //     if (args_[i]) {
  // 	void* iarg = args_[i];
  // 	if (!free_embedded(iarg))
  // 	  out = false;
  // 	args_[i] = NULL;
  //     }
  //   }
  //   free(args_);
  // }
  if (check_error("call_function")) return false;
  if (!out)
    log_error() << "call_function: Failed to call function" << std::endl;
  return out;
}
#else
EMBEDED_LANGUAGE_DEFN_DISABLED(EmbeddedJulia, YGG_EMBED_JULIA_TYPE, JULIA_LANGUAGE, .jl)
#endif
