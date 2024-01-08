#pragma once

// Platform specific
#ifdef _WIN32
#include "regex.hpp"
#else
#include "regex.hpp"
#endif
#include "constants.hpp"
#include "enums.hpp"
#include "rapidjson_wrapper.hpp"

#include <cstring>
#include <cstdlib>


#include "tools.hpp"
#include "logging.hpp"

namespace YggInterface {
  namespace communicator {
    class Comm_t;
  }
}


namespace YggInterface {
namespace utils {

/*!
  @brief Split header and body of message.
  @param[in] buf const char* Message that should be split.
  @param[out] head const char** pointer to buffer where the extracted header
  should be stored.
  @param[out] headsiz size_t reference to memory where size of extracted header
  should be stored.
  @returns: int 0 if split is successful, -1 if there was an error.
*/
int split_head_body(const char *buf,
                    const char **head, size_t *headsiz);

template <typename ValueT>
std::string document2string(ValueT& rhs, const char* indent="");

/*!
  @brief Copy data from one buffer to another.
  @tparam T Type of data in the buffers.
  @param[in,out] dst Destination buffer that data will be copied into.
  @param[in] dst_len Size of destination buffer.
  @param[in] src Source buffer that data will be copied from.
  @param[in] src_len Size of source buffer.
  @param[in] allow_realloc If true, dst can be reallocated if it is not
    large enough to accomodate the data from src.
  @returns If successful, the size of the data copied will be returned.
    If not successful, the negative size of the source data will be
    returned.
 */
template <typename T>
long copyData(T*& dst, const size_t dst_len,
	      const T* src, const size_t src_len,
	      bool allow_realloc);

/**
 * @brief Class for holding generic data
 */
class Metadata : public YggInterface::utils::LogBase {
private:
  Metadata(const Metadata&) = delete;
  Metadata& operator=(const Metadata&) = delete;
public:
  /*!
   * @brief Constructor
   */
  Metadata();
  /*!
   * @brief Destructor
   */
  virtual ~Metadata() {
    if (raw_schema) resetRawSchema();
    reset_filters();
    reset_transforms();
  }
  // Metadata(Metadata& rhs);
#if RAPIDJSON_HAS_CXX11_RVALUE_REFS
    /*!
     * @brief Copy constructor
     * @param[in] rhs The Metadata instance to copy
     */
  Metadata(Metadata&& rhs) noexcept ;
  /*!
   * @brief Assignment operator
   * @param[in] rhs The Metadata instance to assign to this instance
   * @return The new instance
   */
  Metadata& operator=(Metadata&& rhs) noexcept ;
#endif // RAPIDJSON_HAS_CXX11_RVALUE_REFS
  /*!
   * @brief Assignment operator
   * @param[in] rhs The Metadata instance to assign to this instance
   * @return The new instance
   */
  Metadata& operator=(Metadata& rhs);
  /*!
   * @brief Equality operator
   * @param[in] rhs The instance to compare to
   * @return True if they are the same
   */
  bool operator==(const Metadata& rhs) const;
  /*!
   * @brief Inequality operator
   * @param[in] rhs The instance to compare to
   * @return True if they are not the same
   */
  bool operator!=(const Metadata& rhs) const;
  /*! \copydoc YggInterface::utils::LogBase::logClass */
  std::string logClass() const override { return "Metadata"; }
  /*! \copydoc YggInterface::utils::LogBase::logInst */
  std::string logInst() const override;
  /*!
   * @brief Move this instance to another instance
   * @return Pointer to this instance
   */
  virtual Metadata& Move() { return *this; }
  /*!
   * @brief Copy metadata from the input to this instance
   * @param[in] rhs Metqdata instance to copy from
   * @return Always returns true
   */
  bool CopyFrom(const Metadata& rhs);
  /*!
   * @brief Initialize this instance
   * @param[in] use_generic If true then initialize with generic values
   */
  bool _init(bool use_generic = false);
  /*!
   * @brief Reset this instance's schema
   */
  void resetRawSchema();
  /*!
   * @brief Reset this instance's filters to an empty state
   */
  void reset_filters();
  /*!
   * @brief Reset this instance's transforms to an empty state
   */
  void reset_transforms();
  /*!
   * @brief Reset this instance to an empty state
   */
  void reset();
  /*!
   * @brief Copy data from the given schema into this instance
   * @param[in] new_schema The schema to copy from
   * @param[in] isMetadata If true then only copy the schema into the metadata of this instance
   * @param[in] use_generic If true then initialize the schema with generic values first
   * @return true if successful, false otherwise.
   */
  bool fromSchema(const rapidjson::Value& new_schema,
		  bool isMetadata = false, bool use_generic = false);
  /*!
   * @brief Normalize the schema
   * @return true if successful, false otherwise.
   */
  bool Normalize();
  /*!
   * @brief Copy data from the input schema into this instance
   * @param[in] schemaStr The scema to copy in string form
   * @param[in] use_generic If true then initialize the schema with generic values first
   * @return true if successful, false otherwise.
   */
  bool fromSchema(const std::string& schemaStr, bool use_generic = false);
  /*
   * @brief Template function for setting member values from the given data
   * @tparam T The datatype of the input data
   * @param[in] data The data
   * @return true if successful, false otherwise.
   */
  // template<typename T>
  // bool fromData(const T& data) {
  //   rapidjson::Document d;
  //   d.Set(data, d.GetAllocator());
  //   bool has_type = hasType();
  //   bool out = fromData(d);
  //   if (out && !has_type)
  //     out = setAllowWrapped();
  //   return out;
  // }
  /*!
   * @brief Copy the schema from the input document
   * @param[in] data The document to copy from
   * @param[in] before_transforms If true, the the pre-transform schema
   * @return true if successful, false otherwise.
   */
  bool fromData(const rapidjson::Document& data,
		bool before_transforms=false);
  /*!
   * @brief Set the schema type
   * @param[in] type String representation of the type
   * @param[in] use_generic If true then initialize with generic values (only if dont_init is false)
   * @param[in] dont_init If true, then initialize the schema
   * @return true if successful, false otherwise.
   */
  bool fromType(const std::string& type, bool use_generic=false,
		bool dont_init = false);
  /*!
   * @brief Set the data type to scalar and initialize.
   * @param[in] subtype The subtype of the data
   * @param[in] precision The precision of any numeric data
   * @param[in] units The units of the data
   * @param[in] use_generic If true then initialize with generic values
   * @return true if successful, false otherwise.
   */
  bool fromScalar(const std::string& subtype, size_t precision,
		  const char* units=NULL, bool use_generic=false);
  /*!
   * @brief Set the data type to ndarray and initialize
   * @param[in] subtype The subtype of the data
   * @param[in] precision The precision of any numeric data
   * @param[in] ndim The number of dimensions in the array
   * @param[in] shape Pointer array of the sizes of each dimension, the number of elements in shape
   *              must be equal to ndim.
   * @param[in] units The units of the data
   * @param[in] use_generic If true then initialize with generic values
   * @return true if successful, false otherwise.
   */
  bool fromNDArray(const std::string& subtype, size_t precision,
		   const size_t ndim=0, const size_t* shape=NULL,
		   const char* units=NULL, bool use_generic=false);
  /*!
   * @brief Initialize the ndarray structure
   * @param[in] subtype The subtype of the data
   * @param[in] precision The precision of any numeric data
   * @param[in] ndim The number of dimensions in the array
   * @param[in] shape Pointer array of the sizes of each dimension, the number of elements in shape
   *              must be equal to ndim.
   * @param[in] units The units of the data
   * @param[in] use_generic If true then initialize with generic values
   * @param[in] subSchema Pointer to any sub-schema of this instance
   * @return true if successful, false otherwise.
   */
  bool _fromNDArray(const std::string& subtype, size_t precision,
		    const size_t ndim=0, const size_t* shape=NULL,
		    const char* units=NULL, bool use_generic=false,
		    rapidjson::Value* subSchema = NULL);
  /*!
   * @brief Initialize the instance from a format string
   * @param[in] format_str The format string to use
   * @param[in] as_array If true then set the internal element type to array, rather than scalar
   * @param[in] field_names Set of names describing fields in the format
   *   string.
   * @param[in] field_units Set of units describing fields in the format
   *   string.
   * @param[in] use_generic If true then initialize with generic values
   * @return true if successful, false otherwise.
   */
  bool fromFormat(const std::string& format_str, bool as_array = false,
		  const std::vector<std::string>& field_names = {},
		  const std::vector<std::string>& field_units = {},
		  bool use_generic = false);
  /*!
   * @brief Set the metadata from the given Metadata object
   * @param[in] other The Metadata object to use
   * @param[in] use_generic If true then initialize with generic values
   * @return true if successful, false otherwise.
   */
  bool fromMetadata(const Metadata& other, bool use_generic = false);
  /*!
   * @brief Set the metadata from the given char* array
   * @param[in] head The char* array to get the data from
   * @param[in] headsiz The size of header data.
   * @param[in] use_generic If true then initialize with generic values
   * @return true if successful, false otherwise.
   */
  bool fromMetadata(const char* head, const size_t headsiz,
		    bool use_generic = false);
  /*!
   * @brief Set the metadata from the given string
   * @param[in] head The string to get the data from
   * @param[in] use_generic If true then initialize with generic values
   * @return true if successful, false otherwise.
   */
  bool fromMetadata(const std::string& head, bool use_generic = false);
  /*!
   * @brief Set the metadata from the encoded schema
   * @param[in] document The encoded schema
   * @param[in] use_generic If true then initialize with generic values
   * @return true if successful, false otherwise.
   */
  bool fromEncode(const rapidjson::Value& document,
		  bool use_generic = false);
  /*!
   * @brief Set the metadata from the encoded schema in a Python object
   * @param[in] pyobj The encoded schema
   * @param[in] use_generic If true then initialize with generic values
   * @return true if successful, false otherwise.
   */
  bool fromEncode(PyObject* pyobj, bool use_generic = false);
  /*!
   * @brief Add a filter to those used to select messages.
   * @param[in] new_filter New filter to add.
   */
  void addFilter(const FilterBase* new_filter);
  /*!
   * @brief Add a filter to those used to select messages.
   * @param[in] new_filter Python function to add as a filter.
   */
  void addFilter(const PyObject* new_filter);
  /*!
   * @brief Add a filter to those used to select messages.
   * @param[in] new_filter Function to add as a filter.
   */
  void addFilter(filterFunc new_filter);
  /*!
   * @brief Add a transform to those used to modify messages.
   * @param[in] new_transform New transform to add.
   */
  void addTransform(const TransformBase* new_transform);
  /*!
   * @brief Add a transform to those used to modify messages.
   * @param[in] new_transform Python function to add as a transform.
   */
  void addTransform(const PyObject* new_transform);
  /*!
   * @brief Add a transform to those used to modify messages.
   * @param[in] new_transform Function to add as a transform.
   */
  void addTransform(const transformFunc& new_transform);
  /*!
   * @brief Set the filters used to select messages.
   * @param[in] new_filters New filters.
   */
  template<typename T>
  bool setFilters(const std::vector<T>& new_filters) {
    reset_filters();
    for (typename std::vector<T>::const_iterator it = new_filters.cbegin();
	 it != new_filters.cend(); it++)
      addFilter(*it);
    return true;
  }
  /*!
   * @brief Set the transforms used to select messages.
   * @param[in] new_transforms New transforms.
   */
  template<typename T>
  bool setTransforms(const std::vector<T>& new_transforms) {
    reset_transforms();
    for (typename std::vector<T>::const_iterator it = new_transforms.cbegin();
	 it != new_transforms.cend(); it++)
      addTransform(*it);
    return true;
  }
  /*!
   * @brief Get an allocator for the metadata.
   * @return An allocator
   */
  RAPIDJSON_DEFAULT_ALLOCATOR& GetAllocator();
  /*!
   * @brief Determine if the schema is a generic one
   * @return True if the schema is generci
   */
  bool isGeneric() const;
  /*!
   * @brief Set the schema to be generic
   */
  bool setGeneric();
  /*!
   * @brief Set the schema to allow wrapping as an array
   */
  bool setAllowWrapped();
  /*!
   * @brief Determine if the metadata was from a format string
   * @return True if it was set from a format string
   */
  int isFormatArray() const;
  /*!
   * @brief Determine if the metadata is empty
   * @return True if it is empty
   */
  bool empty() const;
  /*!
   * @brief Determine if the schema has a type
   * @return True if the schema has a type
   */
  bool hasType() const;
  /*!
   * @brief Determine if the schema has a subtype
   * @return True if the schema has a subtype
   */
  bool hasSubtype() const;
  /*!
   * @brief Get the type of the schema
   * @return The type of the schema, or "" if there is none.
   */
  const char* typeName() const;
  /*!
   * Get the subtype of the schema
   * @return The subtype of the schema, or "" if there is none.
   */
  const char* subtypeName() const;
  /*!
   * @brief Initialize the schema based on the metadata
   */
  rapidjson::Value* initSchema();
  /*!
   * @brief Allocate and initialize the metadata
   */
  void initMeta();
  /*!
   * @brief Add an item to the subschema from the given Metadata
   * @param[in] other The metadata containing the item
   * @param[in] subSchema The subschema to use, if null then use the internal schema
   * @return Always returns true
   */
  bool addItem(const Metadata& other,
	       rapidjson::Value* subSchema=nullptr);
  /*!
   * @brief Add a member to the subschema from the Metadata
   * @param[in] name The name of the member to add
   * @param[in] other The Meatdata containing the member
   * @param[in] subSchema The subschema to use, if null then use the internal schema
   * @return Always returns true
   */
  bool addMember(const std::string& name, const Metadata& other,
		 rapidjson::Value* subSchema=nullptr);
  /*!
   * @brief Get the metadata
   * @return The metadata
   */
  rapidjson::Value* getMeta();
  /*!
   * @brief Get a const version of the metadata
   * @return The metadata (const)
   */
  const rapidjson::Value* getMeta() const;
  /*!
   * @brief Get the schema
   * @return The schema
   */
  rapidjson::Value* getSchema(bool required=false);
  /*!
   * @brief Get a const version of the schema
   * @return The schema (const)
   */
  const rapidjson::Value* getSchema(bool required=false) const;
  /*!
   * @brief Set the value of the given item. If the item does not exist it will be added to the subschema.
   * @param[in] name The name of the item to set the value for
   * @param[in] x The value to assign to the item
   * @param[in] subSchema The subschema to use.
   */
  bool SetValue(const std::string& name, rapidjson::Value& x,
		rapidjson::Value& subSchema);
#define GET_METHOD_(type_out, method)					\
  bool Get ## method(const std::string& name,				\
		     type_out& out,					\
		     const rapidjson::Value& subSchema			\
		     ) const;						\
  bool Get ## method ## Optional(const std::string& name,		\
				 type_out& out,				\
				 type_out defV,				\
				 const rapidjson::Value& subSchema	\
				 ) const;				\
  bool GetMeta ## method(const std::string& name,			\
			     type_out& out) const;			\
  bool GetMeta ## method ## Optional(const std::string& name,		\
				     type_out& out,			\
				     type_out defV			\
				     ) const;				\
  bool GetSchema ## method(const std::string& name,			\
			   type_out& out,				\
			   const rapidjson::Value* subSchema = NULL	\
			   ) const;					\
  bool GetSchema ## method ## Optional(const std::string& name,		\
				       type_out& out,			\
				       type_out defV,			\
				       const rapidjson::Value* subSchema = NULL) const
#define SET_VECTOR_METHOD_(type_in, method, setargs)			\
  bool SetVector ## method(const std::string& name,			\
			   const std::vector<type_in>& x,		\
			   rapidjson::Value& subSchema);		\
  bool SetMetaVector ## method(const std::string& name,			\
			       const std::vector<type_in>& x);		\
  bool SetSchemaVector ## method(const std::string& name,		\
				 const std::vector<type_in>& x,		\
				 rapidjson::Value* subSchema = NULL)
#define SET_METHOD_(type_in, method, setargs)				\
  bool Set ## method(const std::string& name, type_in x,		\
		     rapidjson::Value& subSchema);			\
  bool SetMeta ## method(const std::string& name, type_in x);		\
  bool SetSchema ## method(const std::string& name, type_in x,		\
			   rapidjson::Value* subSchema = NULL)
#define GET_SET_METHOD_(type_in, type_out, type_vect, method, setargs)	\
  GET_METHOD_(type_out, method);					\
  SET_METHOD_(type_in, method, setargs);				\
  SET_VECTOR_METHOD_(type_vect, method, setargs)
  GET_SET_METHOD_(int, int, int, Int, (x));
  GET_SET_METHOD_(uint64_t, uint64_t, uint64_t, Uint, (x));
  GET_SET_METHOD_(bool, bool, bool, Bool, (x));
  GET_SET_METHOD_(const std::string&, const char*, std::string, String,
		  (x.c_str(), (rapidjson::SizeType)(x.size()),
		   metadata.GetAllocator()));
  GET_METHOD_(unsigned, Uint);
  GET_METHOD_(int32_t, Uint);
  GET_METHOD_(std::string, String);
#undef GET_SET_METHOD_
#undef GET_METHOD_
#undef SET_METHOD_
#undef SET_VECTOR_METHOD_
  /*!
   * @brief Set the value of the named metadata item
   * @param[in] name The name of the item to set
   * @param[in] x The value to set the item to
   */
  bool SetMetaValue(const std::string& name, rapidjson::Value& x);
  /*!
   * @brief Set the value of the named schema item
   * @param[in] name The name of the item to set
   * @param[in] x The value to give to the item
   * @param[in] subSchema The subschema to use
   */
  bool SetSchemaValue(const std::string& name, rapidjson::Value& x,
		      rapidjson::Value* subSchema = NULL);
  /*!
   * @brief Set the named schema item from the given Metadata instnace
   * @param[in] name The name of the item to set the value for
   * @param[in] other Metadata containing the value for the item
   */
  bool SetSchemaMetadata(const std::string& name,
			 const Metadata& other);
  /*!
   * @brief Set the ID for the metadata item
   * @param[in] name The name of the item to set the ID for
   * @param[in] id the id to use
   */
  bool SetMetaID(const std::string& name, const char** id=NULL);
  /*!
   * @brief Set the ID for the metadata item
   * @param[in] name The name of the item to set the ID for
   * @param[in] id the id to use
   */
  bool SetMetaID(const std::string& name, std::string& id);
  /*!
   * @brief Check if the last message was filtered. This also resets the
   *   state to unfiltered for the next message.
   * @return true if the last message was filtered.
   */
  bool checkFilter();
  /*!
   * @brief Evaluate filters for a message.
   * @param[in] msg Message to pass through filters.
   * @return true if message should be filtered.
   */
  bool filter(const rapidjson::Document& msg);
  /*!
   * @brief Evaluate transforms for a message.
   * @param[in,out] msg Message to transform.
   * @return true if successful, false otherwise.
   */
  bool transform(rapidjson::Document& msg);
  /*!
   * @brief Deserialize a message into a variable argument list.
   * @param[in] data Message.
   * @param[out] ap Variable argument list.
   * @return 0 if succesful
   */
  int deserialize_args(const rapidjson::Document& data,
		       rapidjson::VarArgList& ap);
  /*!
   * @brief Update the metaschema and message following deserialization.
   * @param[in,out] d Message.
   * @return 0 if succesful
   */
  int deserialize_updates(rapidjson::Document& d);
  /*!
   * @brief Deserialize a message into a rapidjson document.
   * @param[in] buf Message buffer to deserialize.
   * @param[out] data Destination document.
   * @param[in] temporary If true, this is a temporary deserialization
   *   and the datatype should not be updated.
   */
  int deserialize(const char* buf, rapidjson::Document& data,
		  bool temporary = false);
  /*!
   * @brief Deserialize the buffer into an arg list
   * @param[in] buf The document to be parsed
   * @param[in] nargs The number of arguments in the va_arg list
   * @param[in] allow_realloc If set to true, then allow the buffer to be resized to fit the data
   * @param[out] ... The va_arg list of items to deserialize
   * @return 0 if successful
   */
  int deserialize(const char* buf, size_t nargs, int allow_realloc, ...);
  /*!
   * @brief Deserialize the buffer into an arg list
   * @param[in] buf The document to be parsed
   * @param[out] ap The va_arg list to use
   * @return 0 if successful
   */
  int deserialize(const char* buf, rapidjson::VarArgList& ap);
  /*!
   * @brief Create a rapidjson document from a variable argument list.
   * @param[out] data Destination document.
   * @param[in] ap Variable argument list.
   * @return The number of variable arguments added to the document.
   */
  int serialize_args(rapidjson::Document& data,
		     rapidjson::VarArgList& ap);
  /*!
   * @brief Update the metaschema and message prior to serialization.
   * @param[in,out] d Message.
   * @return The number of elements
   */
  int serialize_updates(rapidjson::Document& d);
  /*!
   * @brief Serialize a rapidjson document into a buffer.
   * @param[out] buf Destination buffer (assumed to be reallocatable).
   * @param[out] buf_siz Finalize size of destination buffer.
   * @param[in] data Document to serialize.
   * @param[in] temporary If true, this is a temporary serialization and
   *   the datatype should not be updated.
   */
  int serialize(char **buf, size_t *buf_siz,
		const rapidjson::Document& data,
		bool temporary = false);
  /*!
   * @brief Serialize the arg list into the buffer
   * @param[out] buf Buffer for the document
   * @param[in, out] buf_siz The size of the buffer
   * @param[in] nargs The number of arguments
   * @param[in] ... The arguments
   * @return The size of buf
   */
  int serialize(char **buf, size_t *buf_siz, size_t nargs, ...);
  /*!
   * @brief Serialize the arg list into the buffer
   * @param[out] buf Buffer for the document
   * @param[in, out] buf_siz The size of the buffer
   * @param[in] ap The va_arg list
   * @return The size of buf
   */
  int serialize(char **buf, size_t *buf_siz,
		rapidjson::VarArgList& ap);
  /*!
   * @brief Write the metadata to the terminal.
   * @param[in] indent The indentation to use for different levels
   */
  virtual void Display(const char* indent="") const;
  rapidjson::Document metadata;            /**! The metadata */
  Metadata* raw_schema;                    /**! Metadata for messages prior to transformation */
  std::vector<FilterBase*> filters;        /**! Filters for messages */
  std::vector<TransformBase*> transforms;  /**! Transformations for messages */
  bool skip_last;                          /**! Whether the last message was filtered */
};

/*!
 * @brief Subtype of Metadata used for storing message information
 */
class YGG_API Header : public Metadata {
public:
  Header(const Header&) = delete;
  Header& operator=(const Header&) = delete;
  /*!
   * @brief Constructor
   * @param[in] own_data If set to true, then the header istance owns its own data
   */
  explicit Header(bool own_data=false);
  /*!
   * @brief Constructor from a communicator
   * @param[in] buf Buffer containing the message to send
   * @param[in] len The size of the buffer in bytes
   * @param[in] comm The communicator to use.
   */
  Header(const char* buf, const size_t &len,
	 YggInterface::communicator::Comm_t* comm);
  /*!
   * @brief Constructor for receiving data
   * @param[in] buf The buffer where the data will be put
   * @param[in] len The current size of the buffer in bytes
   * @param[in] allow_realloc If true then buf can be resized to fit the incoming data
   */
  Header(char*& buf, const size_t &len, bool allow_realloc);
  ~Header() override;
#if RAPIDJSON_HAS_CXX11_RVALUE_REFS
  /*!
   * @brief Copy constructor
   * @param[in] rhs The Header instance to copy
   */
  Header(Header&& rhs) noexcept ;
  /*!
   * @brief Assignment operator
   * @param[in] rhs The Header instance to move to this one
   * @return The new instance
   */
  Header& operator=(Header&& rhs) noexcept ;
#endif // RAPIDJSON_HAS_CXX11_RVALUE_REFS
  /*!
   * @brief Assignment operator
   * @param[in] rhs The Header instance to move to this one
   * @return The new instance
   */
  Header& operator=(Header& rhs);
  /*!
   * @brief Equality operator
   * @param[in] rhs The instance to compare this one to
   * @return True if the Headers are the same
   */
  bool operator==(const Header& rhs) const;
  /*!
   * @brief Inequality operator
   * @param[in] rhs The Header instance to compare this one to
   * @return True if they are different
   */
  bool operator!=(const Header& rhs) const;
  /*!
   * @brief Move this instance to another
   * @return Pointer to this instance
   */
  Header& Move() override { return *this; }
  /*!
   * @brief Get the size of this instance in bytes
   * @return The size in bytes
   */
  size_t size() const { return size_curr; }
  /*!
   * @brief Get a pointer to the beginning of the data
   * @return Pointer to the beginning of the data
   */
  const char* c_str() const { return data[0]; }
  std::string logClass() const override { return "Header"; }
  /*!
    @brief Reset the header, clearing any data/flags.
    @param[in] mode Method that should be used to reset the header.
      HEAD_RESET_COMPLETE
        All parameters should be reset and the buffer should be freed.
      HEAD_RESET_KEEP_BUFFER
        All parameters should be reset except those describing the
	buffer state and the buffer should not be freed.
      HEAD_RESET_OWN_DATA
        All parameters should be reset and the buffer should be freed.
	Parameters will then be set so that the header is responsible
	for freeing the buffer.
      HEAD_RESET_DROP_DATA
        All parameters should be reset, but the buffer will not be freed
	and its pointer will not be stored.
   */
  void reset(HEAD_RESET_MODE mode = HEAD_RESET_COMPLETE);
  /*!
    @brief Copy data and header directly from another Header instance.
    @param[in] rhs Header to copy from.
    @param[in] keep_buffer If true, the existing buffer will be preserved
      and data from rhs will be copied into it.
    @returns true if successful, false otherwise.
   */
  bool RawAssign(const Header& rhs, bool keep_buffer=false);
  /*!
    @brief Reallocate the buffer as necessary.
    @param[in] size_new Size to reallocate the buffer to accomodate.
    @returns Size of allocated buffer on success, -size_new on failure.
   */
  long reallocData(const size_t size_new);
  /*!
    @brief Copy data into the buffer.
    @param[in] msg Data to copy.
    @param[in] msg_siz Size of data to copy.
    @returns If successful, the size of the data copied will be returned.
      If not successful, the negative size of the source data will be
      returned.
   */
  long copyData(const char* msg, const size_t msg_siz);
  /*!
    @brief Move parameters from another header into this one.
    @param[in] rhs Header to move parameters from. It will be reset to
      the original state except that the buffer will be preserved if it
      is not transfered to this header (in the case that this header
      does not own its buffer).
    @returns true if successful, false otherwise.
   */
  bool MoveFrom(Header& rhs);
  /*!
    @brief Copy parameters and data from another header.
    @param[in] rhs Header to copy parameters/data from.
    @returns true if successful, false otherwise.
   */
  bool CopyFrom(const Header& rhs);

  /*!
    @brief Set flags based on a message's contents.
    @param[in] msg Message.
    @param[in] msg_len Message size.
   */
  void setMessageFlags(const char* msg, const size_t msg_len);

  /*!
    @brief Set parameters for sending a message.
    @param[in] metadata0 Pointer to metadata object
    @param[in] msg Buffer containing message to be sent.
    @param[in] len Size of message in buffer.
    @param[in] comm_flags Bit flags describing the communicator that will
      send the message.
    @returns true on success, false on failure.
  */
  bool for_send(Metadata* metadata0, const char* msg, const size_t len,
		int comm_flags);
  /*!
    @brief Format data and set flags for the message.
    @param[in] dont_advance If true, the position in the buffer will
      not be updated.
    @returns -1 on error, size of message otherwise.
   */
  int on_send(bool dont_advance=false);
  /*!
    @brief Set parameters for receiving a message.
    @param[in] buf Message containing header.
    @param[in] buf_siz Size of buffer containing message.
    @param[in] allow_realloc If true, the buffer can be resized to
      receive message larger than buf_siz.
   */
  void for_recv(char*& buf, size_t buf_siz, bool allow_realloc);
  /*!
    @brief Copy data and set flags based on received message.
    @param[in] msg Buffer containing message received.
    @param[in] msg_siz Size of message in buffer.
    @returns -1 on error, size of message otherwise.
   */
  long on_recv(const char* msg, const size_t& msg_siz);
  /*!
   * @brief Reset and format the given buffer
   * @param[in, out] buffer The buffer to format
   * @param[in] metaOnly If true, then only work on the metadata
   */
  bool formatBuffer(rapidjson::StringBuffer& buffer, bool metaOnly=false);
  /*!
   * @brief Write this instance to the terminal
   * @param[in] indent What to use for indentation of the different levels
   */
  void Display(const char* indent="") const override;
  /*!
   * @brief Format the data into the buffer
   * @return The current size of the buffer
   */
  int format();
  /*!
   * @brief Finish up after the last of the data are received.
   * @return true if successful, false otherwise.
   */
  bool finalize_recv();
  /*!
   * @brief Get the message data
   * @return Pointer to the message data
   */
  char* data_msg() const {
    if (!data) {
      assert((!data_) && (!offset));
      return data_;
    }
    if (!data[0]) {
      assert(!offset);
      return data[0];
    }
    return data[0] + static_cast<long>(offset);
  }
  
  char* data_;       /**! Internal data storage */
  char** data;       /**! Internal data storage */
  size_t size_data;  /**! Size of the data */
  size_t size_buff;  /**! Size of the buffer */
  size_t size_curr;  /**! Current size */
  size_t size_head;  /**! Size of the header */
  size_t size_max;   /**! Maximum size for a single header */
  size_t size_msg;   /**! Size of the message */
  size_t size_raw;   /**! Size of raw data */
  uint16_t flags;    /**! Internal flags */
  size_t offset;     /**! Offset for finding data */
};

}
}


// Local Variables:
// mode: c++
// End:
