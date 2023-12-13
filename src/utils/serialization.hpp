#pragma once

// Platform specific
#ifdef _WIN32
#include "regex.hpp"
#else
#include "regex.hpp"
#endif
#include "constants.hpp"
#include "enums.hpp"

#include "rapidjson/document.h"
#include "rapidjson/writer.h"
#include "rapidjson/prettywriter.h"
#include "rapidjson/stringbuffer.h"
#include "rapidjson/schema.h"
#include "rapidjson/va_list.h"
#include <cstring>
#include <cstdlib>


#include "logging.hpp"

#define STRLEN_RJ(var)				\
  static_cast<rapidjson::SizeType>(strlen(var))


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
long copyData(char*& dst, const size_t dst_len,
	      const char* src, const size_t src_len,
	      bool allow_realloc);

/**
 * @brief Class for holding generic data
 */
class Metadata {
public:
    Metadata(const Metadata&) = delete;
    Metadata& operator=(const Metadata&) = delete;
    /*!
     * @brief Constructor
     */
  Metadata();
  virtual ~Metadata() = default;
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
  /*!
   *@brief  Move this instance to another instance
   * @return Pointer to this instance
   */
  virtual Metadata& Move() { return *this; }
  /*!
   * @brief Copy metadata from the input to this instance
   * @param[in] rhs Metqdata instance to copy from
   * @return Always returns true
   */
  bool CopyFrom(const Metadata& rhs) {
    metadata.CopyFrom(rhs.metadata, GetAllocator(), true);
    _update_schema();
    return true;
  }
  /*!
   * @brief Initialize this instance
   * @param[in] use_generic If true then initialize with generic values
   */
  void _init(bool use_generic = false);
  /*!
   * @brief Reset this instance to an empty state
   */
  void reset();
  /*!
   * @brief Copy data from the given schema into this instance
   * @param[in] new_schema The schema to copy from
   * @param[in] isMetadata If true then only copy the schema into the metadata of this instance
   * @param[in] use_generic If true then initialize the schema with generic values first
   */
  void fromSchema(const rapidjson::Value& new_schema,
		  bool isMetadata = false, bool use_generic = false);
  /*!
   * @brief Normalize the schema
   */
  void Normalize();
  /*!
   * @brief Copy data from the input schema into this instance
   * @param[in] schemaStr The scema to copy in string form
   * @param[in] use_generic If true then initialize the schema with generic values first
   */
  void fromSchema(const std::string& schemaStr, bool use_generic = false);
  /*!
   * @brief Template function for setting member values from the given data
   * @tparam T The datatype of the input data
   * @param[in] data The data
   */
  template<typename T>
  void fromData(const T& data) {
    rapidjson::Document d;
    d.Set(data, d.GetAllocator());
    fromData(d, true);
  }
  /*!
   * @brief Copy the schema from the input document
   * @param[in] data THe document to copy from
   * @param[in] indirect If true, then allow indirect (wrapped) data
   */
  void fromData(const rapidjson::Document& data, bool indirect=false);
  /*!
   * @brief Set the schema type
   * @param[in] type String representation of the type
   * @param[in] use_generic If true then initialize with generic values (only if dont_init is false)
   * @param[in] dont_init If true, then initialize the schema
   */
  void fromType(const std::string& type, bool use_generic=false,
		bool dont_init = false);
  /*!
   * @brief Set the data type to scalar and initialize.
   * @param[in] subtype The subtype of the data
   * @param[in] precision The precision of any numeric data
   * @param[in] units The units of the data
   * @param[in] use_generic If true then initialize with generic values
   */
  void fromScalar(const std::string& subtype, size_t precision,
		  const char* units=nullptr, bool use_generic=false);
  /*!
   * @brief Set the data type to ndarray and initialize
   * @param[in] subtype The subtype of the data
   * @param[in] precision The precision of any numeric data
   * @param[in] ndim The number of dimensions in the array
   * @param[in] shape Pointer array of the sizes of each dimension, the number of elements in shape
   *              must be equal to ndim.
   * @param[in] units The units of the data
   * @param[in] use_generic If true then initialize with generic values
   */
  void fromNDArray(const std::string& subtype, size_t precision,
		   const size_t ndim=0, const size_t* shape=nullptr,
		   const char* units=nullptr, bool use_generic=false);
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
   */
  void _fromNDArray(const std::string& subtype, size_t precision,
		    const size_t ndim=0, const size_t* shape=nullptr,
		    const char* units=nullptr, bool use_generic=false,
		    rapidjson::Value* subSchema = nullptr);
  /*!
   * @brief Initialize the instance from a format string
   * @param[in] format_str The format string to use
   * @param[in] as_array If true then set the internal element type to array, rather than scalar
   * @param[in] use_generic If true then initialize with generic values
   */
  void fromFormat(const std::string& format_str,
		  bool as_array = false, bool use_generic = false);
  /*!
   * @brief Set the metadata from the given Metadata object
   * @param[in] other The Metadata object to use
   * @param[in] use_generic If true then initialize with generic values
   */
  void fromMetadata(const Metadata& other, bool use_generic = false);
  /*!
   * @brief Set the metadata from the given char* array
   * @param[in] head The char* array to get the data from
   * @param[in] headsiz The ise of head.
   * @param[in] use_generic If true then initialize with generic values
   */
  void fromMetadata(const char* head, const size_t headsiz,
		    bool use_generic = false);
  /*!
   * @brief Set the metadata from the given string
   * @param[in] head The string to get the data from
   * @param[in] use_generic If true then initialize with generic values
   */
  void fromMetadata(const std::string& head, bool use_generic = false);
  /*!
   * @brief Set the metadata from the encoded schema
   * @param[in] document The encoded schema
   * @param[in] use_generic If true then initialize with generic values
   */
  void fromEncode(const rapidjson::Value& document,
		  bool use_generic = false);
  /*!
   * @brief Get an allocator for the metadata.
   * @return An allocator
   */
  rapidjson::Document::AllocatorType& GetAllocator();
  /*!
   * @brief Determine if the schema is a generic one
   * @return True if the schema is generci
   */
  bool isGeneric() const;
  /*!
   * @brief Set the schema to be generic
   */
  void setGeneric();
  /*!
   * @brief Determine if the metadata was from a format string
   * @return True if it was set from a format string
   */
  bool isFormatArray() const;
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
  void initSchema();
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
  rapidjson::Value& getMeta();
  /*!
   * @brief Get a const version of the metadata
   * @return The metadata (const)
   */
  const rapidjson::Value& getMeta() const;
  /*!
   * @brief Get the schema
   * @return The schema
   */
  rapidjson::Value& getSchema();
  /*!
   * @brief Get a const version of the schema
   * @return The schema (const)
   */
  const rapidjson::Value& getSchema() const;
  /*!
   * @brief Set the value of the given item. If the item does not exist it will be added to the subschema.
   * @param[in] name The name of the item to set the value for
   * @param[in] x The value to assign to the item
   * @param[in] subSchema The subschema to use.
   */
  void SetValue(const std::string& name, rapidjson::Value& x,
		rapidjson::Value& subSchema);
#define GET_SET_METHOD_(type_in, type_out, method, setargs)		\
  type_out Get ## method(const std::string& name,			\
			 const rapidjson::Value& subSchema) const;	\
  type_out Get ## method ## Optional(const std::string& name,		\
				     type_out defV,			\
				     const rapidjson::Value& subSchema	\
				     ) const;				\
  void Set ## method(const std::string& name, type_in x,			\
		     rapidjson::Value& subSchema);			\
  type_out GetMeta ## method(const std::string& name) const;		\
  type_out GetMeta ## method ## Optional(const std::string& name,	\
					 type_out defV) const;		\
  void SetMeta ## method(const std::string& name, type_in x);		\
  type_out GetSchema ## method(const std::string& name,			\
			       rapidjson::Value* subSchema = NULL	\
			       ) const;					\
  type_out GetSchema ## method ## Optional(const std::string& name,	\
					   type_out defV,		\
					   rapidjson::Value* subSchema = NULL) const; \
  void SetSchema ## method(const std::string& name, type_in x,		\
			   rapidjson::Value* subSchema = NULL)
  GET_SET_METHOD_(int, int, Int, (x));
  GET_SET_METHOD_(uint64_t, uint64_t, Uint, (x));
  GET_SET_METHOD_(bool, bool, Bool, (x));
  GET_SET_METHOD_(const std::string&, const char*, String,
		  (x.c_str(), (rapidjson::SizeType)(x.size()),
		   metadata.GetAllocator()));
#undef GET_SET_METHOD_
  /*!
   * @brief Set the value of the named metadata item
   * @param[in] name The name of the item to set
   * @param[in] x The value to set the item to
   */
  void SetMetaValue(const std::string& name, rapidjson::Value& x);
  /*!
   * @brief Set the value of the named schema item
   * @param[in] name The name of the item to set
   * @param[in] x The value to give to the item
   * @param[in] subSchema The subschema to use
   */
  void SetSchemaValue(const std::string& name, rapidjson::Value& x,
		      rapidjson::Value* subSchema = nullptr);
  /*!
   * @brief Set the named schema item from the given Metadata instnace
   * @param[in] name The name of the item to set the value for
   * @param[in] other Metadata containing the value for the item
   */
  void SetSchemaMetadata(const std::string& name,
			 const Metadata& other);
  /*!
   * @brief Set the ID for the metadata item
   * @param[in] name The name of the item to set the ID for
   * @param[in] id the id to use
   */
  void SetMetaID(const std::string& name, const char** id=nullptr);
  /*!
   * @brief Set the ID for the metadata item
   * @param[in] name The name of the item to set the ID for
   * @param[in] id the id to use
   */
  void SetMetaID(const std::string& name, std::string& id);
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
  rapidjson::Document metadata;   /**! The metadata */
  rapidjson::Value* schema;       /**! The schema */
 private:
  void _update_schema();
};

/*!
 * @brief Header subtype of Metadata
 */
class Header : public Metadata {
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
  */
  void for_send(Metadata* metadata0, const char* msg, const size_t len,
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
  void formatBuffer(rapidjson::StringBuffer& buffer, bool metaOnly=false);
  /*!
   * @brief Write this instance to the terminal
   * @param[in] indent What to use for indentation of the different levels
   */
  void Display(const char* indent="") const override;
  /*!
   * @brief Format the data into the buffer
   * @return The current size of the buffer
   */
  size_t format();
  /*!
   * @brief Finish up after the last of the data are received.
   */
  void finalize_recv();
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
