#pragma once
#include "utils/tools.hpp"
#include "utils/rapidjson_wrapper.hpp"
#include "utils/logging.hpp"
#include "utils/enums_utils.hpp"

// TODO: Pass error state to finalize?

#define EMBEDED_LANGUAGE_DECL(cls, emT)					\
  public:								\
  /** \copydoc YggInterface::utils::EmbeddedLanguageBase::EmbeddedLanguageBase */ \
  YGG_API cls();							\
  /** \copydoc YggInterface::utils::EmbeddedLanguageBase::is_enabled */	\
  YGG_API bool is_enabled() const override;				\
  /** \copydoc YggInterface::utils::EmbeddedLanguageBase::initialize_main */	\
  YGG_API bool initialize_main() override;				\
  /** \copydoc YggInterface::utils::EmbeddedLanguageBase::finalize_main */	\
  YGG_API bool finalize_main() override;				\
  /** \copydoc YggInterface::utils::EmbeddedLanguageBase::get_error */	\
  YGG_API bool get_error(std::string& message) const override;	\
  /** \copydoc YggInterface::utils::EmbeddedLanguageBase::preserve_embedded */ \
  YGG_API bool preserve_embedded(void*& x) const override;		\
  /** \copydoc YggInterface::utils::EmbeddedLanguageBase::free_embedded */ \
  YGG_API bool free_embedded(void*& x) const override;			\
  /** \copydoc YggInterface::utils::EmbeddedLanguageBase::string_embedded */ \
  YGG_API std::string string_embedded(const void* x) const override;	\
  /** \copydoc YggInterface::utils::EmbeddedLanguageBase::eval */	\
  YGG_API bool eval(const std::string& expr,				\
		    rapidjson::Document& result) const override;	\
  /** \copydoc YggInterface::utils::EmbeddedLanguageBase::convert_to */	\
  YGG_API bool convert_to(const rapidjson::Value& v_in, void*& v_out,\
			  bool dont_preserve = false) const;		\
  /** \copydoc YggInterface::utils::EmbeddedLanguageBase::convert_from */ \
  YGG_API bool convert_from(const void*& v_in, rapidjson::Value& v_out,\
			    rapidjson::Value::AllocatorType& allocator) const; \
  /** \copydoc YggInterface::utils::EmbeddedLanguageBase::load_function */ \
  YGG_API void* load_function(const std::string& name) const override;	\
  /** \copydoc YggInterface::utils::EmbeddedLanguageBase::call_function */ \
  YGG_API bool call_function(void* func, const rapidjson::Document& args,\
			     rapidjson::Document& result) const override
#define EMBEDED_LANGUAGE_DEFN_ENABLED(cls, emT, lang, ext)		\
  cls::cls() : EmbeddedLanguageBase(lang, #ext) {}			\
  bool cls::is_enabled() const {					\
    std::string envvar = env_dont_use();				\
    char* temp = NULL;							\
    if (!envvar.empty()) {						\
      temp = std::getenv(envvar.c_str());				\
    }									\
    std::string envval;							\
    if (temp) {								\
      envval.assign(temp);						\
    }									\
    if (envval == "1" || utils::str_toupper(envval) == "TRUE") {	\
      return false;							\
    }									\
    return true;							\
  }
#define LOG_DISABLED()						\
  log_error() << logInst() << " is disabled" << std::endl
#define EMBEDED_LANGUAGE_DEFN_DISABLED(cls, emT, lang, ext)		\
  cls::cls() : EmbeddedLanguageBase(lang, #ext) {}			\
  bool cls::is_enabled() const { return false; }			\
  bool cls::initialize_main() {						\
    LOG_DISABLED();							\
    return false;							\
  }									\
  bool cls::finalize_main() {						\
    LOG_DISABLED();							\
    return false;							\
  }									\
  bool cls::get_error(std::string&) const {				\
    LOG_DISABLED();							\
    return true;							\
  }									\
  bool cls::preserve_embedded(void*&) const {				\
    LOG_DISABLED();							\
    return false;							\
  }									\
  bool cls::free_embedded(void*&) const {				\
    LOG_DISABLED();							\
    return false;							\
  }									\
  std::string cls::string_embedded(const void*) const {			\
    LOG_DISABLED();							\
    return "";								\
  }									\
  bool cls::eval(const std::string&, rapidjson::Document&) const {	\
    LOG_DISABLED();							\
    return false;							\
  }									\
  bool cls::convert_to(const rapidjson::Value&, void*&, bool) const {	\
    LOG_DISABLED();							\
    return false;							\
  }									\
  bool cls::convert_from(const void*&, rapidjson::Value&,		\
			 rapidjson::Value::AllocatorType&) const {	\
    LOG_DISABLED();							\
    return false;							\
  }									\
  void* cls::load_function(const std::string&) const {			\
    LOG_DISABLED();							\
    return NULL;							\
  }									\
  bool cls::call_function(void*, const rapidjson::Document&,		\
			  rapidjson::Document&) const {			\
    LOG_DISABLED();							\
    return false;							\
  }
// #undef LOG_DISABLED

namespace YggInterface {
  namespace utils {
    /**
     * @brief Base class for embeded languages.
     */
    class EmbeddedLanguageBase : public YggInterface::utils::LogBase {
    private:
      EmbeddedLanguageBase(const EmbeddedLanguageBase& other) = delete;
      EmbeddedLanguageBase& operator=(const EmbeddedLanguageBase&) = delete;
    public:
      /**
       * @brief Initializer
       * @param[in] name Name of the embeded language.
       * @param[in] ext Extension for language files.
       */
      YGG_API EmbeddedLanguageBase(const LANGUAGE& name,
				   const std::string& ext="");
      /** \copydoc YggInterface::utils::LogBase::logClass */
      std::string logClass() const override { return "EmbeddedLanguageBase"; }
      /** \copydoc YggInterface::utils::LogBase::logInst */
      std::string logInst() const override;
      /**
       * @brief Determine if the embedded language is enabled.
       * @returns true if the embedded language is enabled, false otherwise.
       */
      YGG_API virtual bool is_enabled() const;
      /**
       * @brief Get the environment variable that can be used to disable
       *   use of this embedded language.
       * @returns Environment variable name.
       */
      YGG_API virtual std::string env_dont_use() const;
      /**
       * @brief Check if the current thread is the thread that initialized
       *   this embedded language.
       * @returns true if the current thread initialized the language,
       *   false otherwise.
       */
      YGG_API bool onParentThread() const;
      /**
       * @brief Get any flags that should be set for functions loaded
       *   from libraries written in this embedded language.
       * @returns Bitwise function flags.
       */
      YGG_API virtual int functionFlags() const;
      /**
       * @brief Get any flags that should be set for comms calling
       *   functions loaded from libraries written in this embedded
       *   language.
       * @returns Bitwise comm flags.
       */
      YGG_API virtual int64_t commFlags() const;
      /**
       * @brief Disable the embedded language by setting an environment
       *   variable so that it can't be initialized again.
       * @returns Previous state of the language as enabled (true) or not
       *   (false).
       */
      YGG_API virtual bool disable() const;
      /**
       * @brief Enable the embedded language (if possibly) by setting an
       *   environment variable.
       * @returns Previous state of the language as enabled (true) or not
       *   (false).
       */
      YGG_API virtual bool enable() const;
      /**
       * @brief Initialize the embeded language if it has not already
       *   been initialized.
       * @param[in] on_thread If true, the embedded language is
       *   initialized for the current thread via initialize_thread. If
       *   false, initialize_main is used instead.
       * @returns true if language is successfully initialized, false
       *   otherwise.
       */
      YGG_API virtual bool initialize(bool on_thread = false);
      /**
       * @brief Finalize the embeded language on the main thread.
       * @param[in] on_thread If true, the embedded language is
       *   finalized for the current thread via finalize_thread. If
       *   false, finalize_main is used instead.
       * @returns true if language is successfully finalized, false
       *   otherwise.
       */
      YGG_API virtual bool finalize(bool on_thread = false);
      /**
       * @brief Initialize the embeded language on the main thread.
       * @returns true if language is successfully initialized, false
       *   otherwise.
       */
      YGG_API virtual bool initialize_main() {
	log_error() << "initialize of base class called, must be overridden" << std::endl; // GCOVR_EXCL_START
	return false; // GCOVR_EXCL_STOP
      }
      /**
       * @brief Finalize the embeded language on the main thread.
       * @returns true if language is successfully finalized, false
       *   otherwise.
       */
      YGG_API virtual bool finalize_main() {
	log_error() << "finalize of base class called, must be overridden" << std::endl;  // GCOVR_EXCL_START
	return false; // GCOVR_EXCL_STOP
      }
      /**
       * @brief Check the number of functions using this language on the
       *   current thread.
       * @param[in] action The number that the function count for this
       *   language should be incremented by.
       * @returns The function count after performing the requested action.
       */
      YGG_API virtual int function_count(int action = 0);
      /**
       * @brief Initialize the embeded language on a C++ thread.
       * @returns true if language is successfully initialized, false
       *   otherwise.
       */
      YGG_API virtual bool initialize_thread();
      /**
       * @brief Finalize the embeded language on a C++ thread.
       * @returns true if language is successfully initialized, false
       *   otherwise.
       */
      YGG_API virtual bool finalize_thread();
      /**
       * @brief Check and log any errors in the embedded language.
       * @param[in] context Context that should be added to the emitted
       *   error message if an error occurred.
       * @returns true if there is an error, false otherwise.
       */
      YGG_API virtual bool check_error(const std::string& context) const;
      /**
       * @brief Get any error in the embedded language
       * @param[out] message String to store the error message in
       * @returns true if there is an error, false otherwise.
       */
      YGG_API virtual bool get_error(std::string& message) const {
	UNUSED(message); // GCOVR_EXCL_START
	log_error() << "get_error of base class called, must be overridden" << std::endl;
	return true; // GCOVR_EXCL_STOP
      }
      /**
       * @brief Preserve resources associated with an embedded object so
       *   they are not garbage collected by the embedded language.
       * @param[in,out] x Embedded variable to preserve.
       * @returns true if successful, false otherwise.
       */
      YGG_API virtual bool preserve_embedded(void*& x) const {
	UNUSED(x); // GCOVR_EXCL_START
	log_error() << "preserve_embedded of base class called, must be overridden" << std::endl;
	return false; // GCOVR_EXCL_STOP
      }
      /**
       * @brief Release resources associated with an embedded object.
       * @param[in,out] x Embedded variable to release.
       * @returns true if successful, false otherwise.
       */
      YGG_API virtual bool free_embedded(void*& x) const {
	UNUSED(x); // GCOVR_EXCL_START
	log_error() << "free_embedded of base class called, must be overridden" << std::endl;
	return false; // GCOVR_EXCL_STOP
      }
      /**
       * @brief Get a string respresentation of an embedded object.
       * @param[in] x Embedded variable.
       * @returns String representation.
       */
      YGG_API std::string string_embedded(void* x) const {
	const void* x_noconst = const_cast<const void*>(x);
	return string_embedded(x_noconst);
      }
      /**
       * @brief Get a string respresentation of an embedded object.
       * @param[in] x Embedded variable.
       * @returns String representation.
       */
      YGG_API virtual std::string string_embedded(const void* x) const {
	UNUSED(x); // GCOVR_EXCL_START
	log_error() << "string_embedded of base class called, must be overridden" << std::endl;
	return "";
      }
      /**
       * @brief Evaluate an expression in the embedded language.
       * @param[in] expr Expression to evaluate.
       * @param[out] result Document where expresssion result should be
       *   stored.
       * @returns true if expression evaluation was successful, false
       *   otherwise.
       */
      YGG_API virtual bool eval(const std::string& expr,
				rapidjson::Document& result) const {
	UNUSED(expr); // GCOVR_EXCL_START
	UNUSED(result);
	log_error() << "eval of base class called, must be overridden" << std::endl;
	return false; // GCOVR_EXCL_STOP
      }
      /**
       * @brief Convert from a rapidjson document to a native embeded type.
       * @param[in] v_in Value to convert.
       * @param[out] v_out Native embeded type.
       * @param[in] dont_preserve If true, don't preserve the returned
       *   value against cleanup.
       * @returns true if the conversion was successful, false otherwise.
       */
      YGG_API bool convert_to(const rapidjson::Value& v_in,
			      void*& v_out,
			      bool dont_preserve=false) const {
	UNUSED(v_in); // GCOVR_EXCL_START
	UNUSED(v_out);
	UNUSED(dont_preserve);
	log_error() << "convert_to of base class called, must be overridden" << std::endl;
	return false; // GCOVR_EXCL_STOP
      }
      /**
       * @brief Convert from a native embeddd type to a rapidjson document.
       * @param[in] v_in Native embeded type.
       * @param[out] v_out Converted document.
       * @param[in] allocator rapidjson Allocator that should be used if
       *   required when assigning to v_out.
       * @returns true if the conversion was successful, false otherwise.
       */
      YGG_API bool convert_from(const void*& v_in,
				rapidjson::Value& v_out,
				rapidjson::Value::AllocatorType& allocator) const {
	UNUSED(v_in); // GCOVR_EXCL_START
	UNUSED(v_out);
	UNUSED(allocator);
	log_error() << "convert_from of base class called, must be overridden" << std::endl;
	return false; // GCOVR_EXCL_STOP
      }
      /**
       * @brief Load a function based on a string.
       * @param[in] name Description of where a function can be located.
       * @returns Pointer to the loaded function. Returns NULL if the function
       *   could not be loaded.
       */
      YGG_API virtual void* load_function(const std::string& name) const {
	UNUSED(name); // GCOVR_EXCL_START
	log_error() << "load_function of base class called, must be overridden" << std::endl;
	return NULL; // GCOVR_EXCL_STOP
      }
      /**
       * @brief Call a function in the embedded language
       * @param[in] func Pointer to the function to call.
       * @param[in] args Document containing function arguments.
       * @param[out] result Document to store the result from the function
       *   call in.
       * @returns true if the call is successful, false otherwise.
       */
      YGG_API virtual bool call_function(void* func,
					 const rapidjson::Document& args,
					 rapidjson::Document& result) const {
	UNUSED(func); // GCOVR_EXCL_START
	UNUSED(args);
	UNUSED(result);
	log_error() << "call_function of base class called, must be overridden" << std::endl;
	return false; // GCOVR_EXCL_STOP
      }
      
      LANGUAGE language;     /**< Enum of the embeded language. */
      std::string ext;       /**< Language extension. */
      std::string thread_id; /**< Thread that initialized the language. */
    };

  }
}
