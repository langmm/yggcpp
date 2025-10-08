#pragma once
#include "utils/rapidjson_wrapper.hpp"


namespace YggInterface {
  namespace utils {

    enum HandlerContextObjectFlag {
      HandlerContextObjectFlagFalse = 0,
      HandlerContextObjectFlagTrue = 1,
      HandlerContextObjectFlagInstance = 2
    };

    template<typename T>
    struct HandlerContextBase {
      T* object;
      const char* key;
      SizeType keyLength;
      HandlerContextObjectFlag isObject;
      bool keyValuePairs;
      bool copiedKey;
    };
    
    enum DatetimeMode {
      DM_NONE = 0,
      // Formats
      DM_ISO8601 = 1<<0,      // Bidirectional ISO8601 for datetimes, dates and times
      DM_UNIX_TIME = 1<<1,    // Serialization only, "Unix epoch"-based number of seconds
      // Options
      DM_ONLY_SECONDS = 1<<4, // Truncate values to the whole second, ignoring micro seconds
      DM_IGNORE_TZ = 1<<5,    // Ignore timezones
      DM_NAIVE_IS_UTC = 1<<6, // Assume naive datetime are in UTC timezone
      DM_SHIFT_TO_UTC = 1<<7, // Shift to/from UTC
      DM_MAX = 1<<8
    };
    
    
#define DATETIME_MODE_FORMATS_MASK 0x0f // 0b00001111 in C++14
    
    
    static inline int
    datetime_mode_format(unsigned mode) {
      return mode & DATETIME_MODE_FORMATS_MASK;
    }
    
    
    static inline bool
    valid_datetime_mode(int mode) {
      int format = datetime_mode_format(mode);
      return (mode >= 0 && mode < DM_MAX
	      && (format <= DM_UNIX_TIME)
	      && (mode == 0 || format > 0));
    }
    
    
    static int
    days_per_month(int year, int month) {
      assert(month >= 1);
      assert(month <= 12);
      if (month == 1 || month == 3 || month == 5 || month == 7
	  || month == 8 || month == 10 || month == 12) {
        return 31;
      } else if (month == 4 || month == 6 || month == 9 || month == 11) {
        return 30;
      } else if (year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)) {
        return 29;
      } else {
        return 28;
      }
    }
    
    
    enum UuidMode {
      UM_NONE = 0,
      UM_CANONICAL = 1<<0, // 4-dashed 32 hex chars: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
      UM_HEX = 1<<1,       // canonical OR 32 hex chars in a row
      UM_MAX = 1<<2
    };
    
    
    enum NumberMode {
      NM_NONE = 0,
      NM_NAN = 1<<0,     // allow "not-a-number" values
      NM_DECIMAL = 1<<1, // serialize Decimal instances, deserialize floats as Decimal
      NM_NATIVE = 1<<2,  // use faster native C library number handling
      NM_MAX = 1<<3
    };
    
    
    enum BytesMode {
      BM_NONE = 0,
      BM_UTF8 = 1<<0,             // try to convert to UTF-8
      BM_SCALAR = 1<<1,           // Encode as a yggdrasil scalar
      BM_MAX = 1<<2
    };
    
    
    enum ParseMode {
      PM_NONE = 0,
      PM_COMMENTS = 1<<0,         // Allow one-line // ... and multi-line /* ... */ comments
      PM_TRAILING_COMMAS = 1<<1,  // allow trailing commas at the end of objects and arrays
      PM_MAX = 1<<2
    };
    
    
    enum WriteMode {
      WM_COMPACT = 0,
      WM_PRETTY = 1<<0,            // Use PrettyWriter
      WM_SINGLE_LINE_ARRAY = 1<<1, // Format arrays on a single line
      WM_MAX = 1<<2
    };
    
    
    enum IterableMode {
      IM_ANY_ITERABLE = 0,        // Default, any iterable is dumped as JSON array
      IM_ONLY_LISTS = 1<<0,       // Only list instances are dumped as JSON arrays
      IM_MAX = 1<<1
    };
    
    
    enum MappingMode {
      MM_ANY_MAPPING = 0,                // Default, any mapping is dumped as JSON object
      MM_ONLY_DICTS = 1<<0,              // Only dict instances are dumped as JSON objects
      MM_COERCE_KEYS_TO_STRINGS = 1<<1,  // Convert keys to strings
      MM_SKIP_NON_STRING_KEYS = 1<<2,    // Ignore non-string keys
      MM_SORT_KEYS = 1<<3,               // Sort keys
      MM_MAX = 1<<4
    };
    
    
    enum YggdrasilMode {
      YM_BASE64 = 0,              // Default, yggdrasil extension types are base 64 encoded
      YM_READABLE = 1<<0,         // Encode yggdrasil extension types in a readable format
      YM_PICKLE = 1<<2,           // Pickle unsupported objects
      YM_MAX = 1<<3
    };
    
    static int SIZE_OF_SIZE_T = sizeof(size_t);


    template<typename T>
    struct WrappedHandler {
      typedef T WrapT
      typedef char Ch;
      typedef HandlerContextBase<WrapT> HandlerContext;
      WrapT* decoderStartObject;
      WrapT* decoderEndObject;
      WrapT* decoderEndArray;
      WrapT* decoderString;
      WrapT* sharedKeys;
      WrapT* root;
      WrapT* objectHook;
      WrapT* wrapped_true;
      WrapT* wrapped_false;
      unsigned datetimeMode;
      unsigned uuidMode;
      unsigned numberMode;
      std::vector<HandlerContext> stack;

      WrappedHandler(WrapT* decoder,
		     WrapT* hook,
		     unsigned dm,
		     unsigned um,
		     unsigned nm)
        : decoderStartObject(NULL),
          decoderEndObject(NULL),
          decoderEndArray(NULL),
          decoderString(NULL),
          root(NULL),
          objectHook(hook),
          datetimeMode(dm),
          uuidMode(um),
          numberMode(nm)
        {
	  stack.reserve(128);
	  if (decoder != NULL) {
	    assert(!objectHook);

	    if (wrapped_hasattr(decoder, "start_object")) {
	      decoderStartObject = wrapped_getattr(decoder, "start_object");
	    }
	    
	    if (wrapped_hasattr(decoder, "end_object")) {
	      decoderEndObject = wrapped_getattr(decoder, "end_object");
	    }
	    if (wrapped_hasattr(decoder, "end_array")) {
	      decoderEndArray = wrapped_getattr(decoder, "end_array");
	    }
	    if (wrapped_hasattr(decoder, "string")) {
	      decoderString = wrapped_getattr(decoder, "string");
	    }
	  }
	  sharedKeys = wrapped_map();
        }
      
      ~WrappedHandler() {
        while (!stack.empty()) {
	  const HandlerContext& ctx = stack.back();
	  if (ctx.copiedKey)
	    wrapped_free((void*) ctx.key);
	  if (ctx.object != NULL)
	    wrapped_dec(ctx.object);
	  stack.pop_back();
        }
	wrapped_cleanup(decoderStartObject);
        wrapped_cleanup(decoderEndObject);
        wrapped_cleanup(decoderEndArray);
        wrapped_cleanup(decoderString);
        wrapped_cleanup(sharedKeys);
      }

      static inline
      void wrapped_inc(WrapT*& x) {
	UNUSED(x);
      }
      static inline
      void wrapped_dec(WrapT*& x) {
	UNUSED(x);
      }
      static inline
      void wrapped_setref(WrapT*& dst, WrapT*& src) {
	UNUSED(dst);
	UNUSED(src);
      }
      static inline
      void wrapped_cleanup(WrapT*& x) {
	UNUSED(x);
      }
      static inline
      void* wrapped_malloc(SizeType size) {
	UNUSED(size);
	return NULL;
      }
      static inline
      void wrapped_free(void* key) {
	UNUSED(key);
      }
      static inline
      void wrapped_error(const std::string& msg) {
	UNUSED(msg);
      }
      static inline
      bool wrapped_hasattr(WrapT*& x, const std::string& name) {
	UNUSED(x);
	UNUSED(name);
	return false;
      }
      static inline
      WrapT* wrapped_getattr(WrapT*& x, const std::string& name) {
	UNUSED(x);
	UNUSED(name);
	return NULL;
      }
      static inline
      WrapT* wrapped_map() { return NULL; }
      static inline
      bool wrapped_map_check(WrapT* x) {
	UNUSED(x);
	return false;
      }
      static inline
      bool wrapped_map_set(WrapT* map, WrapT* key, WrapT* val) {
	UNUSED(map);
	UNUSED(key);
	UNUSED(val);
	return false;
      }
      static inline
      bool wrapped_map_setdefault(WrapT* map, WrapT* key, WrapT* val) {
	UNUSED(map);
	UNUSED(key);
	UNUSED(val);
	return false;
      }
      static inline
      WrapT* wrapped_map_get(WrapT* map, WrapT* key, WrapT* val) {
	UNUSED(map);
	UNUSED(key);
	UNUSED(val);
	return false;
      }
      static inline
      bool wrapped_array_check(WrapT* x) {
	UNUSED(x);
	return false;
      }
      static inline
      WrapT* wrapped_array() { return NULL; }
      static inline
      WrapT* wrapped_tuple(std::initializer_list<WrapT*> list) {
	UNUSED(list);
	return NULL;
      }
      static inline
      SizeType wrapped_array_size(WrapT* arr) {
	UNUSED(arr);
	return 0;
      }
      static inline
      bool wrapped_array_set(WrapT* arr, SizeType idx, WrapT* item) {
	UNUSED(arr);
	UNUSED(idx);
	UNUSED(item);
	return false;
      }
      static inline
      bool wrapped_array_append(WrapT* arr, WrapT* val) {
	UNUSED(arr);
	UNUSED(val);
	return false;
      }
      static inline
      WrapT* wrapped_string(const char* str, SizeType leng) {
	UNUSED(str);
	UNUSED(len);
	return NULL;
      }
      static inline
      WrapT* wrapped_function_call(WrapT* func, WrapT* args) {
	UNUSED(func);
	UNUSED(args);
      }

      bool Handle(WrapT* value) {
        if (root) {
	  const HandlerContext& current = stack.back();

	  if (current.isObject) {
	    WrapT* key = wrapped_string(current.key, current.keyLength);
	    if (key == NULL) {
	      wrapped_dec(value);
	      return false;
	    }

	    WrapT* shared_key = wrapped_setdefault(sharedKeys, key, key);
	    if (shared_key == NULL) {
	      wrapped_dec(key);
	      wrapped_dec(value);
	      return false;
	    }
	    wrapped_inc(shared_key);
	    wrapped_dec(key);
	    key = shared_key;
	    
	    bool rc = false;
	    if (current.keyValuePairs) {
	      WrapT* pair = wrapped_tuple({key, value});
	      
	      wrapped_dec(key);
	      wrapped_dec(value);
	      if (pair == NULL) {
		return false;
	      }
	      rc = wrapped_array_append(current.object, pair);
	      wrapped_dec(pair);
	    } else {
	      rc = wrapped_map_set(current.object, key, value);
	      wrapped_dec(key);
	      wrapped_dec(value);
	    }
	    
	    if (!rc) {
	      return false;
	    }
	  } else {
	    wrapped_array_append(current.object, value);
	    wrapped_dec(value);
	  }
        } else {
	  root = value;
        }
        return true;
      }

      bool Key(const char* str, SizeType length, bool copy) {
        HandlerContext& current = stack.back();
	
        // This happens when operating in stream mode and kParseInsituFlag is not set: we
        // must copy the incoming string in the context, and destroy the duplicate when
        // the context gets reused for the next dictionary key
	
        if (current.key && current.copiedKey) {
	  wrapped_free(current.key);
	  current.key = NULL;
        }

        if (copy) {
	  char* copied_str = (char*) wrapped_malloc(length+1);
	  if (copied_str == NULL)
	    return false;
	  memcpy(copied_str, str, length+1);
	  str = copied_str;
	  assert(!current.key);
        }
	
        current.key = str;
        current.keyLength = length;
        current.copiedKey = copy;

        return true;
      }
      
      bool StartObject(bool yggdrasilInstance=false) {
        WrapT* mapping;
        bool key_value_pairs;

        if (decoderStartObject != NULL) {
	  mapping = wrapped_function_call(decoderStartObject, NULL);
	  if (mapping == NULL)
	    return false;
	  key_value_pairs = wrapped_array_check(mapping);
	  if (!wrapped_map_check(mapping) && !key_value_pairs) {
	    wrapped_dec(mapping);
	    wrapped_error("start_object() must return a mapping or a list instance");
	    return false;
	  }
        } else {
	  mapping = wrapped_map();
	  if (mapping == NULL) {
	    return false;
	  }
	  key_value_pairs = false;
        }

        if (!Handle(mapping)) {
	  return false;
        }

        HandlerContext ctx;
        ctx.isObject = HandlerContextObjectFlagTrue;
	if (yggdrasilInstance)
	  ctx.isObject = HandlerContextObjectFlagInstance;
        ctx.keyValuePairs = key_value_pairs;
        ctx.object = mapping;
        ctx.key = NULL;
        ctx.copiedKey = false;
        wrapped_inc(mapping);

        stack.push_back(ctx);

        return true;
      }

      bool EndObject(SizeType, bool yggdrasilInstance=false) {
        const HandlerContext& ctx = stack.back();
	
        if (ctx.copiedKey)
	  wrapped_free((void*) ctx.key);
	
        WrapT* mapping = ctx.object;
	
	bool isInstance = false;
	if (yggdrasilInstance && stack.size() > 0)
	  isInstance = (ctx.isObject == HandlerContextObjectFlagInstance);
        stack.pop_back();

        if (objectHook == NULL && decoderEndObject == NULL &&
	    !(yggdrasilInstance && isInstance)) {
	  wrapped_dec(mapping);
	  return true;
        }

        WrapT* replacement = NULL;
	if (yggdrasilInstance && isInstance) {
	  // TODO: Replace when schema?
	  replacement = dict2instance(mapping);
	} else if (decoderEndObject != NULL) {
	  replacement = wrapped_function_call(decoderEndObject, mapping, NULL);
        } else /* if (objectHook != NULL) */ {
	  replacement = wrapped_function_call(objectHook, mapping, NULL);
        }

        wrapped_dec(mapping);
        if (replacement == NULL)
	  return false;

        if (!stack.empty()) {
	  HandlerContext& current = stack.back();
	  
	  if (current.isObject) {
	    WrapT* key = wrapped_string(current.key,
					current.keyLength);
	    if (key == NULL) {
	      wrapped_dec(replacement);
	      return false;
	    }

	    WrapT* shared_key = wrapped_map_setdefault(sharedKeys, key, key);
	    if (shared_key == NULL) {
	      wrapped_dec(key);
	      wrapped_dec(replacement);
	      return false;
	    }
	    wrapped_inc(shared_key);
	    wrapped_dec(key);
	    key = shared_key;
	    
	    bool rc = false;
	    if (current.keyValuePairs) {
	      WrapT* pair = wrapped_tuple({key, replacement});
	      
	      wrapped_dec(key);
	      wrapped_dec(replacement);
	      if (pair == NULL) {
		return false;
	      }

	      SizeType listLen = wrapped_array_size(current.object);
	      
	      rc = wrapped_array_set(current.object, listLen - 1, pair);
	      
	      if (!rc) {
		wrapped_dec(pair);
		return false;
	      }
	    } else {
	      rc = wrapped_map_set(current.object, key, replacement);
	      wrapped_dec(key);
	      wrapped_dec(replacement);
	      if (!rc) {
		return false;
	      }
	    }
	  } else {
	    SizeType listLen = wrapped_array_size(current.object);
	    bool rc = wrapped_array_set(current.object, listLen - 1, replacement);
	    if (!rc) {
	      wrapped_dec(replacement);
	      return false;
	    }
	  }
        } else {
	  wrapped_setref(root, replacement);
        }
	
        return true;
      }

      bool StartArray() {
        WrapT* list = wrapped_array();
        if (list == NULL) {
	  return false;
        }

        if (!Handle(list)) {
	  return false;
        }

        HandlerContext ctx;
        ctx.isObject = HandlerContextObjectFlagFalse;
        ctx.object = list;
        ctx.key = NULL;
        ctx.copiedKey = false;
        wrapped_inc(list);
	
        stack.push_back(ctx);
	
        return true;
      }

      bool EndArray(SizeType) {
        const HandlerContext& ctx = stack.back();

        if (ctx.copiedKey)
	  wrapped_free((void*) ctx.key);
	
        WrapT* sequence = ctx.object;
        stack.pop_back();

        WrapT* replacement = NULL;
        if (decoderEndArray == NULL) {
	  if (IsStructuredArray(sequence)) {
	    replacement = GetStructuredArray(sequence);
	  } else {
	    wrapped_dec(sequence);
	    return true;
	  }
        } else {
	  replacement = wrapped_function_call(decoderEndArray, sequence,
					      NULL);
	}
        wrapped_dec(sequence);
        if (replacement == NULL)
	  return false;
	
        if (!stack.empty()) {
	  const HandlerContext& current = stack.back();
	  
	  if (current.isObject) {
	    WrapT* key = wrapped_string(current.key,
					current.keyLength);
	    if (key == NULL) {
	      wrapped_dec(replacement);
	      return false;
	    }
	    
	    bool rc = wrapped_map_set(current.object, key, replacement);

	    wrapped_dec(key);
	    wrapped_dec(replacement);

	    if (!rc) {
	      return false;
	    }
	  } else {
	    SizeType listLen = wrapped_array_size(current.object);
	    bool rc = wrapped_array_set(current.object, listLen - 1, replacement);

	    if (!rc) {
	      wrapped_dec(replacement);
	      return false;
	    }
	  }
        } else {
	  wrapped_setref(root, replacement);
        }
	
        return true;
      }

      bool NaN() {
        if (!(numberMode & NM_NAN)) {
	  wrapped_error("Out of range float values are not JSON compliant");
	  return false;
        }

        WrapT* value;
        if (numberMode & NM_DECIMAL) {
	  value = wrapped_function_call(decimal_type, nan_string_value, NULL);
        } else {
	  value = wrapped_double("nan");
        }

        if (value == NULL)
	  return false;

        return Handle(value);
      }

      bool Infinity(bool minus) {
        if (!(numberMode & NM_NAN)) {
	  wrapped_error("Out of range float values are not JSON compliant");
	  return false;
        }

        WrapT* value;
        if (numberMode & NM_DECIMAL) {
	  value = wrapped_function_call(decimal_type,
					minus
					? "-Infinity"
					: "+Infinity", NULL);
        } else {
	  value = wrapped_double(minus
				 ? "-Infinity"
				 : "+Infinity");
        }

        if (value == NULL)
            return false;
	
        return Handle(value);
      }

      bool Null() {
        WrapT* value = Py_None;
        wrapped_inc(value);

        return Handle(value);
      }

      bool Bool(bool b) {
        WrapT* value = b ? wrapped_true : wrapped_false;
        wrapped_inc(value);
	
        return Handle(value);
      }

      bool Int(int i) {
        WrapT* value = PyLong_FromLong(i);
        return Handle(value);
      }

      bool Uint(unsigned i) {
        WrapT* value = PyLong_FromUnsignedLong(i);
        return Handle(value);
      }

      bool Int64(int64_t i) {
        WrapT* value = PyLong_FromLongLong(i);
        return Handle(value);
      }

      bool Uint64(uint64_t i) {
        WrapT* value = PyLong_FromUnsignedLongLong(i);
        return Handle(value);
      }

      bool Double(double d) {
        WrapT* value = PyFloat_FromDouble(d);
        return Handle(value);
      }

      bool RawNumber(const char* str, SizeType length, bool) {
        WrapT* value;
        bool isFloat = false;

        for (int i = length - 1; i >= 0; --i) {
            // consider it a float if there is at least one non-digit character,
            // it may be either a decimal number or +-infinity or nan
            if (!isdigit(str[i]) && str[i] != '-') {
                isFloat = true;
                break;
            }
        }

        if (isFloat) {

            if (numberMode & NM_DECIMAL) {
                WrapT* pystr = wrapped_string(str, length);
                if (pystr == NULL)
                    return false;
                value = wrapped_function_call(decimal_type, pystr, NULL);
                wrapped_dec(pystr);
            } else {
                std::string zstr(str, length);

                value = float_from_string(zstr.c_str(), length);
            }

        } else {
            std::string zstr(str, length);

            value = PyLong_FromString(zstr.c_str(), NULL, 10);
        }

        if (value == NULL) {
            PyErr_SetString(PyExc_ValueError,
                            isFloat
                            ? "Invalid float value"
                            : "Invalid integer value");
            return false;
        } else {
            return Handle(value);
        }
    }

#define digit(idx) (str[idx] - '0')

    bool IsIso8601Date(const char* str, int& year, int& month, int& day) {
        // we've already checked that str is a valid length and that 5 and 8 are '-'
        if (!isdigit(str[0]) || !isdigit(str[1]) || !isdigit(str[2]) || !isdigit(str[3])
            || !isdigit(str[5]) || !isdigit(str[6])
            || !isdigit(str[8]) || !isdigit(str[9])) return false;

        year = digit(0)*1000 + digit(1)*100 + digit(2)*10 + digit(3);
        month = digit(5)*10 + digit(6);
        day = digit(8)*10 + digit(9);

        return year > 0 && month <= 12 && day <= days_per_month(year, month);
    }

    bool IsIso8601Offset(const char* str, int& tzoff) {
        if (!isdigit(str[1]) || !isdigit(str[2]) || str[3] != ':'
            || !isdigit(str[4]) || !isdigit(str[5])) return false;

        int hofs = 0, mofs = 0, tzsign = 1;
        hofs = digit(1)*10 + digit(2);
        mofs = digit(4)*10 + digit(5);

        if (hofs > 23 || mofs > 59) return false;

        if (str[0] == '-') tzsign = -1;
        tzoff = tzsign * (hofs * 3600 + mofs * 60);
        return true;
    }

    bool IsIso8601Time(const char* str, SizeType length,
                       int& hours, int& mins, int& secs, int& usecs, int& tzoff) {
        // we've already checked that str is a minimum valid length, but nothing else
        if (!isdigit(str[0]) || !isdigit(str[1]) || str[2] != ':'
            || !isdigit(str[3]) || !isdigit(str[4]) || str[5] != ':'
            || !isdigit(str[6]) || !isdigit(str[7])) return false;

        hours = digit(0)*10 + digit(1);
        mins = digit(3)*10 + digit(4);
        secs = digit(6)*10 + digit(7);

        if (hours > 23 || mins > 59 || secs > 59) return false;

        if (length == 8 || (length == 9 && str[8] == 'Z')) {
            // just time
            return true;
        }


        if (length == 14 && (str[8] == '-' || str[8] == '+')) {
            return IsIso8601Offset(&str[8], tzoff);
        }

        // at this point we need a . AND at least 1 more digit
        if (length == 9 || str[8] != '.' || !isdigit(str[9])) return false;

        int usecLength;
        if (str[length - 1] == 'Z') {
            usecLength = length - 10;
        } else if (str[length - 3] == ':') {
            if (!IsIso8601Offset(&str[length - 6], tzoff)) return false;
            usecLength = length - 15;
        } else {
            usecLength = length - 9;
        }

        if (usecLength > 9) return false;

        switch (usecLength) {
            case 9: if (!isdigit(str[17])) { return false; }
		RAPIDJSON_DELIBERATE_FALLTHROUGH;
            case 8: if (!isdigit(str[16])) { return false; }
		RAPIDJSON_DELIBERATE_FALLTHROUGH;
            case 7: if (!isdigit(str[15])) { return false; }
		RAPIDJSON_DELIBERATE_FALLTHROUGH;
            case 6: if (!isdigit(str[14])) { return false; }
		usecs += digit(14);
		RAPIDJSON_DELIBERATE_FALLTHROUGH;
            case 5: if (!isdigit(str[13])) { return false; }
		usecs += digit(13)*10;
		RAPIDJSON_DELIBERATE_FALLTHROUGH;
            case 4: if (!isdigit(str[12])) { return false; }
		usecs += digit(12)*100;
		RAPIDJSON_DELIBERATE_FALLTHROUGH;
            case 3: if (!isdigit(str[11])) { return false; }
		usecs += digit(11)*1000;
		RAPIDJSON_DELIBERATE_FALLTHROUGH;
            case 2: if (!isdigit(str[10])) { return false; }
		usecs += digit(10)*10000;
		RAPIDJSON_DELIBERATE_FALLTHROUGH;
            case 1: if (!isdigit(str[9])) { return false; }
		usecs += digit(9)*100000;
        }

        return true;
    }

    bool IsIso8601(const char* str, SizeType length,
                   int& year, int& month, int& day,
                   int& hours, int& mins, int &secs, int& usecs, int& tzoff) {
        year = -1;
        month = day = hours = mins = secs = usecs = tzoff = 0;

        // Early exit for values that are clearly not valid (too short or too long)
        if (length < 8 || length > 35) return false;

        bool isDate = str[4] == '-' && str[7] == '-';

        if (!isDate) {
            return IsIso8601Time(str, length, hours, mins, secs, usecs, tzoff);
        }

        if (length == 10) {
            // if it looks like just a date, validate just the date
            return IsIso8601Date(str, year, month, day);
        }
        if (length > 18 && (str[10] == 'T' || str[10] == ' ')) {
            // if it looks like a date + time, validate date + time
            return IsIso8601Date(str, year, month, day)
                && IsIso8601Time(&str[11], length - 11, hours, mins, secs, usecs, tzoff);
        }
        // can't be valid
        return false;
    }

    bool HandleIso8601(const char* str, SizeType length,
                       int year, int month, int day,
                       int hours, int mins, int secs, int usecs, int tzoff) {
        // we treat year 0 as invalid and thus the default when there is no date
        bool hasDate = year > 0;

        if (length == 10 && hasDate) {
            // just a date, handle quickly
            return Handle(PyDate_FromDate(year, month, day));
        }

        bool isZ = str[length - 1] == 'Z';
        bool hasOffset = !isZ && (str[length - 6] == '-' || str[length - 6] == '+');

        WrapT* value;

        if ((datetimeMode & DM_NAIVE_IS_UTC || isZ) && !hasOffset) {
            if (hasDate) {
                value = PyDateTimeAPI->DateTime_FromDateAndTime(
                    year, month, day, hours, mins, secs, usecs, timezone_utc,
                    PyDateTimeAPI->DateTimeType);
            } else {
                value = PyDateTimeAPI->Time_FromTime(
                    hours, mins, secs, usecs, timezone_utc, PyDateTimeAPI->TimeType);
            }
        } else if (datetimeMode & DM_IGNORE_TZ || (!hasOffset && !isZ)) {
            if (hasDate) {
                value = PyDateTime_FromDateAndTime(year, month, day,
                                                   hours, mins, secs, usecs);
            } else {
                value = PyTime_FromTime(hours, mins, secs, usecs);
            }
        } else if (!hasDate && datetimeMode & DM_SHIFT_TO_UTC && tzoff) {
            PyErr_Format(PyExc_ValueError,
                         "Time literal cannot be shifted to UTC: %s", str);
            value = NULL;
        } else if (!hasDate && datetimeMode & DM_SHIFT_TO_UTC) {
            value = PyDateTimeAPI->Time_FromTime(
                hours, mins, secs, usecs, timezone_utc, PyDateTimeAPI->TimeType);
        } else {
            WrapT* offset = PyDateTimeAPI->Delta_FromDelta(0, tzoff, 0, 1,
                                                              PyDateTimeAPI->DeltaType);
            if (offset == NULL) {
                value = NULL;
            } else {
                WrapT* tz = wrapped_function_call(timezone_type, offset, NULL);
                wrapped_dec(offset);
                if (tz == NULL) {
                    value = NULL;
                } else {
                    if (hasDate) {
                        value = PyDateTimeAPI->DateTime_FromDateAndTime(
                            year, month, day, hours, mins, secs, usecs, tz,
                            PyDateTimeAPI->DateTimeType);
                        if (value != NULL && datetimeMode & DM_SHIFT_TO_UTC) {
                            WrapT* asUTC = PyObject_CallMethodObjArgs(
                                value, astimezone_name, timezone_utc, NULL);
                            wrapped_dec(value);
                            if (asUTC == NULL) {
                                value = NULL;
                            } else {
                                value = asUTC;
                            }
                        }
                    } else {
                        value = PyDateTimeAPI->Time_FromTime(hours, mins, secs, usecs, tz,
                                                             PyDateTimeAPI->TimeType);
                    }
                    wrapped_dec(tz);
                }
            }
        }

        if (value == NULL)
            return false;

        return Handle(value);
    }

#undef digit

    bool IsUuid(const char* str, SizeType length) {
        if (uuidMode == UM_HEX && length == 32) {
            for (int i = length - 1; i >= 0; --i)
                if (!isxdigit(str[i]))
                    return false;
            return true;
        } else if (length == 36
                   && str[8] == '-' && str[13] == '-'
                   && str[18] == '-' && str[23] == '-') {
            for (int i = length - 1; i >= 0; --i)
                if (i != 8 && i != 13 && i != 18 && i != 23 && !isxdigit(str[i]))
                    return false;
            return true;
        }
        return false;
    }

    bool HandleUuid(const char* str, SizeType length) {
        WrapT* pystr = wrapped_string(str, length);
        if (pystr == NULL)
            return false;

        WrapT* value = wrapped_function_call(uuid_type, pystr, NULL);
        wrapped_dec(pystr);

        if (value == NULL)
            return false;
        else
            return Handle(value);
    }

    bool String(const char* str, SizeType length, bool copy) {
	if (isYggdrasilString(str, length, copy)) {
	    Document x;
	    if (!x.FromYggdrasilString(str, length, copy))
		return false;
	    x.FinalizeFromStack();
	    return x.Accept(*this);
	}
        WrapT* value;

        if (datetimeMode != DM_NONE) {
            int year, month, day, hours, mins, secs, usecs, tzoff;

            if (IsIso8601(str, length, year, month, day,
                          hours, mins, secs, usecs, tzoff))
                return HandleIso8601(str, length, year, month, day,
                                     hours, mins, secs, usecs, tzoff);
        }

        if (uuidMode != UM_NONE && IsUuid(str, length))
            return HandleUuid(str, length);

        value = wrapped_string(str, length);
        if (value == NULL)
            return false;

        if (decoderString != NULL) {
            WrapT* replacement = wrapped_function_call(decoderString, value,
                                                                 NULL);
            wrapped_dec(value);
            if (replacement == NULL)
                return false;
            value = replacement;
        }

        return Handle(value);
    }

    template <typename YggSchemaValueType>
    bool YggdrasilString(const char* str, SizeType length, bool, YggSchemaValueType& schema) {
	WrapT* value = NULL;
	RAPIDJSON_DEFAULT_ALLOCATOR allocator;
	Value* x = new Value(str, length, allocator, schema);
	if (x->HasUnits()) {
	    WrapT* type = NULL;
	    if (x->IsScalar()) {
		type = (WrapT*)&Quantity_Type;
	    } else {
		type = (WrapT*)&QuantityArray_Type;
	    }
	    RAPIDJSON_DEFAULT_ALLOCATOR allocator;
	    WrapT* arr = x->GetPythonObjectRaw();
	    WrapT* units = wrapped_string(x->GetUnits().GetString(),
					  x->GetUnits().GetStringLength());
	    if (arr != NULL && units != NULL) {
		WrapT* args = PyTuple_Pack(2, arr, units);
		if (args != NULL) {
		    value = PyObject_Call(type, args, NULL);
		    wrapped_dec(args);
		}
	    }
	    Py_XDECREF(arr);
	    Py_XDECREF(units);
	} else if (x->IsPly()) {
	    PlyObject* v = (PlyObject*) Ply_Type.tp_alloc(&Ply_Type, 0);
	    value = (WrapT*)v;
	    v->ply = new Ply();
	    x->GetPly(*v->ply);
	} else if (x->IsObjWavefront()) {
	    ObjWavefrontObject* v = (ObjWavefrontObject*) ObjWavefront_Type.tp_alloc(&ObjWavefront_Type, 0);
	    value = (WrapT*)v;
	    v->obj = new ObjWavefront();
	    x->GetObjWavefront(*v->obj);
	} else {
	    value = x->GetPythonObjectRaw();
	}
	delete x;
	if (value)
	    return Handle(value);
	return false;
    }

    template <typename YggSchemaValueType>
    bool YggdrasilStartObject(YggSchemaValueType& schema) {
	if (!schema.IsObject())
	    return false;
	typename YggSchemaValueType::ConstMemberIterator vs = schema.FindMember(YggSchemaValueType::GetTypeString());
	if ((vs != schema.MemberEnd()) &&
	    ((vs->value == YggSchemaValueType::GetPythonInstanceString()) ||
	     (vs->value == YggSchemaValueType::GetSchemaString())))
	    return StartObject((vs->value == YggSchemaValueType::GetPythonInstanceString()));
	return false;
    }

    WrapT* dict2instance(WrapT* x) {
	WrapT* cls_name = NULL;
	WrapT* args = NULL;
	WrapT* kwargs = NULL;
	WrapT* class_key = PyUnicode_FromString("class");
	WrapT* args_key = PyUnicode_FromString("args");
	WrapT* kwargs_key = PyUnicode_FromString("kwargs");
	if (PyDict_CheckExact(x)) {
	    cls_name = PyDict_GetItem(x, class_key);
	    args = PyDict_GetItem(x, args_key);
	    kwargs = PyDict_GetItem(x, kwargs_key);
	    Py_XINCREF(cls_name);
	    Py_XINCREF(args);
	    Py_XINCREF(kwargs);
	} else {
	    cls_name = PyObject_GetItem(x, class_key);
	    args = PyObject_GetItem(x, args_key);
	    kwargs = PyObject_GetItem(x, kwargs_key);
	}
	wrapped_dec(class_key);
	wrapped_dec(args_key);
	wrapped_dec(kwargs_key);
	if (cls_name == NULL) {
	    Py_XDECREF(args);
	    Py_XDECREF(kwargs);
	    return NULL;
	}
	if (args == NULL)
	    args = PyTuple_New(0);
	else {
	    WrapT* args_list = args;
	    args = PyList_AsTuple(args_list);
	    wrapped_dec(args_list);
	}
	if (args == NULL) {
	    wrapped_dec(cls_name);
	    Py_XDECREF(kwargs);
	    return NULL;
	}
	if (kwargs == NULL)
	    kwargs = wrapped_map();
	if (kwargs == NULL) {
	    wrapped_dec(cls_name);
	    wrapped_dec(args);
	    return NULL;
	}
	WrapT* cls = import_python_object(PyUnicode_AsUTF8(cls_name),
					     "dict2instance: ", true);
	wrapped_dec(cls_name);
	if (cls == NULL) {
	    wrapped_dec(args);
	    wrapped_dec(kwargs);
	    return NULL;
	}
	WrapT* inst = PyObject_Call(cls, args, kwargs);
	wrapped_dec(cls);
	wrapped_dec(args);
	wrapped_dec(kwargs);
	return inst;
    }

    bool YggdrasilEndObject(SizeType memberCount) {
	return EndObject(memberCount, true);
    }
};
