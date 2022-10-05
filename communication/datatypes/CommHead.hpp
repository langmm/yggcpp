#pragma once

#include <map>
#include "utils/Address.hpp"
#include "rapidjson/document.h"
#include "rapidjson/writer.h"
#include "rapidjson/stringbuffer.h"

namespace communication {
namespace datatypes {
/*! @brief Bit flags. */
#define HEAD_FLAG_VALID      0x00000001  //!< Set if the header is valid.
#define HEAD_FLAG_MULTIPART  0x00000002  //!< Set if the header is for a multipart message
#define HEAD_TYPE_IN_DATA    0x00000004  //!< Set if the type is stored with the data during serialization
#define HEAD_AS_ARRAY        0x00000008  //!< Set if messages will be serialized arrays

enum fields {
    ADDRESS, ID, REQUEST_ID, RESPONSE_ADDRESS, ZMQ_REPLY, ZMQ_REPLY_WORKER, MODEL
};
const std::map<fields, std::string> string_fields = {{ADDRESS,          "address"},
                                                     {ID,               "id"},
                                                     {REQUEST_ID,       "request_id"},
                                                     {RESPONSE_ADDRESS, "response_address"},
                                                     {ZMQ_REPLY,        "zmq_reply"},
                                                     {ZMQ_REPLY_WORKER, "zmq_reply_worker"},
                                                     {MODEL,            "model"}};

class CommHead {
public:
    explicit CommHead(utils::Address *adr = nullptr, const std::string &id = "");

    CommHead(const char *buf, const size_t &buf_siz);

    ~CommHead();

    size_t bodysiz; //!< Size of body.
    size_t bodybeg; //!< Start of body in header.
    int flags; //!< Bit flags encoding the status of the header.
    int nargs_populated; //!< Number of arguments populated during deserialization.
    //
    size_t size; //!< Size of incoming message.
    utils::Address *address; //!< Address that message will comm in on.
    std::string id; //!< Unique ID associated with this message.
    utils::Address *response_address; //!< Response address.
    std::string request_id; //!< Request id.
    utils::Address *zmq_reply; //!< Reply address for ZMQ sockets.
    utils::Address *zmq_reply_worker; //!< Reply address for worker socket.
    std::string model; //!< Name of model that sent the header.
    // These should be removed once JSON fully implemented
    int serializer_type; //!< Code indicating the type of serializer.
    std::string format_str; //!< Format string for serializer.
    std::string field_names; //!< String containing field names.
    std::string field_units; //!< String containing field units.
    //
    //DTYPE dtype;
private:
    bool update_header_from_doc(rapidjson::Value &head_doc);
};

}
}