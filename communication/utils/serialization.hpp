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
#include <stdlib.h>


#include "logging.hpp"

#define STRLEN_RJ(var)				\
  static_cast<rapidjson::SizeType>(strlen(var))


namespace communication {
  namespace communicator {
    class Comm_t;
  }
}


namespace communication {
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

class Metadata {
private:
  Metadata(const Metadata&) = delete;
  Metadata& operator=(const Metadata&) = delete;
public:
  Metadata();
  virtual ~Metadata() {}
  // Metadata(Metadata& rhs);
#if RAPIDJSON_HAS_CXX11_RVALUE_REFS
  Metadata(Metadata&& rhs);
  Metadata& operator=(Metadata&& rhs);
#endif // RAPIDJSON_HAS_CXX11_RVALUE_REFS
  Metadata& operator=(Metadata& rhs);
  bool operator==(const Metadata& rhs) const;
  bool operator!=(const Metadata& rhs) const;
  Metadata& Move() { return *this; }
  bool CopyFrom(const Metadata& rhs) {
    metadata.CopyFrom(rhs.metadata, GetAllocator(), true);
    _update_schema();
    return true;
  }
  void _init(bool use_generic = false);
  void reset();
  void fromSchema(const rapidjson::Value& new_schema,
		  bool isMetadata = false, bool use_generic = false);
  void Normalize();
  void fromSchema(const std::string schemaStr, bool use_generic = false);
  template<typename T>
  void fromData(const T& data) {
    rapidjson::Document d;
    d.Set(data, d.GetAllocator());
    fromData(d, true);
  }
  void fromData(const rapidjson::Document& data, bool indirect=false);
  void fromType(const std::string type, bool use_generic=false,
		bool dont_init = false);
  void fromScalar(const std::string subtype, size_t precision,
		  const char* units=NULL, bool use_generic=false);
  void fromNDArray(const std::string subtype, size_t precision,
		   const size_t ndim=0, const size_t* shape=NULL,
		   const char* units=NULL, bool use_generic=false);
  void _fromNDArray(const std::string subtype, size_t precision,
		    const size_t ndim=0, const size_t* shape=NULL,
		    const char* units=NULL, bool use_generic=false,
		    rapidjson::Value* subSchema = NULL);
  void fromFormat(const std::string& format_str,
		  bool as_array = false, bool use_generic = false);
  void fromMetadata(const Metadata& other, bool use_generic = false);
  void fromMetadata(const char* head, const size_t headsiz,
		    bool use_generic = false);
  void fromMetadata(const std::string& head, bool use_generic = false);
  void fromEncode(const rapidjson::Value& document,
		  bool use_generic = false);
  void fromEncode(PyObject* pyobj, bool use_generic = false);
  rapidjson::Document::AllocatorType& GetAllocator();
  bool isGeneric() const;
  void setGeneric();
  bool isFormatArray() const;
  bool empty() const;
  bool hasType() const;
  bool hasSubtype() const;
  const char* typeName() const;
  const char* subtypeName() const;
  void initSchema();
  void initMeta();
  bool addItem(const Metadata& other,
	       rapidjson::Value* subSchema=nullptr);
  bool addMember(const std::string name, const Metadata& other,
		 rapidjson::Value* subSchema=nullptr);
  rapidjson::Value& getMeta();
  const rapidjson::Value& getMeta() const;
  rapidjson::Value& getSchema();
  const rapidjson::Value& getSchema() const;
  void SetValue(const std::string name, rapidjson::Value& x,
		rapidjson::Value& subSchema);
#define GET_SET_METHOD_(type_in, type_out, method, setargs)		\
  type_out Get ## method(const std::string name,			\
			 const rapidjson::Value& subSchema) const;	\
  type_out Get ## method ## Optional(const std::string name,		\
				     type_out defV,			\
				     const rapidjson::Value& subSchema	\
				     ) const;				\
  void Set ## method(const std::string name, type_in x,			\
		     rapidjson::Value& subSchema);			\
  type_out GetMeta ## method(const std::string name) const;		\
  type_out GetMeta ## method ## Optional(const std::string name,	\
					 type_out defV) const;		\
  void SetMeta ## method(const std::string name, type_in x);		\
  type_out GetSchema ## method(const std::string name,			\
			       rapidjson::Value* subSchema = NULL	\
			       ) const;					\
  type_out GetSchema ## method ## Optional(const std::string name,	\
					   type_out defV,		\
					   rapidjson::Value* subSchema = NULL) const; \
  void SetSchema ## method(const std::string name, type_in x,		\
			   rapidjson::Value* subSchema = NULL)
  GET_SET_METHOD_(int, int, Int, (x));
  GET_SET_METHOD_(uint64_t, uint64_t, Uint, (x));
  GET_SET_METHOD_(bool, bool, Bool, (x));
  GET_SET_METHOD_(const std::string&, const char*, String,
		  (x.c_str(), (rapidjson::SizeType)(x.size()),
		   metadata.GetAllocator()));
#undef GET_SET_METHOD_
  void SetMetaValue(const std::string name, rapidjson::Value& x);
  void SetSchemaValue(const std::string name, rapidjson::Value& x,
		      rapidjson::Value* subSchema = NULL);
  void SetSchemaMetadata(const std::string name,
			 const Metadata& other);
  void SetMetaID(const std::string name, const char** id=NULL);
  void SetMetaID(const std::string name, std::string& id);
  int deserialize(const char* buf, size_t nargs, int allow_realloc, ...);
  int deserialize(const char* buf, rapidjson::VarArgList& ap);
  int serialize(char **buf, size_t *buf_siz, size_t nargs, ...);
  int serialize(char **buf, size_t *buf_siz,
		rapidjson::VarArgList& ap);
  void Display(const char* indent="") const;
  rapidjson::Document metadata;
  rapidjson::Value* schema;
 private:
  void _update_schema();
};

class Header : public Metadata {
private:
  Header(const Header&) = delete;
  Header& operator=(const Header&) = delete;
public:
  Header(bool own_data=false);
  Header(const char* buf, const size_t &len,
	 communication::communicator::Comm_t* comm);
  Header(char*& buf, const size_t &len, bool allow_realloc);
  ~Header() override;
#if RAPIDJSON_HAS_CXX11_RVALUE_REFS
  Header(Header&& rhs);
  Header& operator=(Header&& rhs);
#endif // RAPIDJSON_HAS_CXX11_RVALUE_REFS
  Header& operator=(Header& rhs);
  bool operator==(const Header& rhs) const;
  bool operator!=(const Header& rhs) const;
  Header& Move() { return *this; }
  size_t size() const { return size_curr; }
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
    @param[in] msg_Siz Size of data to copy.
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

  void formatBuffer(rapidjson::StringBuffer& buffer, bool metaOnly=false);
  void Display(const char* indent="") const;
  size_t format();
  void finalize_recv();
  char* data_msg() {
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
  
  char* data_;
  char** data;
  size_t size_data;
  size_t size_buff;
  size_t size_curr;
  size_t size_head;
  size_t size_max;
  size_t size_msg;
  size_t size_raw;
  uint16_t flags;
  size_t offset;
};

}
}


// Local Variables:
// mode: c++
// End:
