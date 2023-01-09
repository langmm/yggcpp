#include "CommHead.hpp"
#include "utils/logging.hpp"
#include "utils/tools.hpp"

using namespace communication::datatypes;
using namespace communication::utils;

CommHead::CommHead(utils::Address* adr, const std::string &id): address(adr), id(id){
    // Parameters set during read
    bodysiz = 0;
    bodybeg = 0;
    flags = HEAD_FLAG_VALID;
    nargs_populated = 0;
    // Parameters sent in header
    //out.size = size;
    response_address = nullptr;
    request_id = "";
    zmq_reply = nullptr;
    zmq_reply_worker = nullptr;
    model = "";
    // Parameters that will be removed
    serializer_type = -1;
    format_str = "";
    // Parameters used for type
    //dtype = T_ANY;
}

CommHead::CommHead(const char *buf, const size_t &buf_siz) {
    int ret;
    char *head = nullptr;
    size_t headsiz;
    try {
        // Split header/body
        ret = split_head_body(buf, buf_siz, &head, &headsiz);
        if (ret < 0) {
            ygglog_error << "parse_comm_header: Error splitting head and body.";
            flags &= ~HEAD_FLAG_VALID;
            if (head != nullptr)
                free(head);
            return;
        }
        bodybeg = headsiz + 2*strlen(MSG_HEAD_SEP);
        bodysiz = buf_siz - bodybeg;
        // Handle raw data without header
        if (headsiz == 0) {
            flags &= ~HEAD_FLAG_MULTIPART;
            size = bodysiz;
            free(head);
            return;
        }
        // Parse header
        rapidjson::Document head_doc;
        head_doc.Parse(head, headsiz);
        if (!(head_doc.IsObject()))
            ygglog_throw_error("parse_comm_header: Parsed header document is not an object.");
        if (head_doc.HasMember("datatype")) {
            dtype = new DataType(type_from_header_doc(head_doc), false);
        } else if (head_doc.HasMember("type_in_data")) {
            dtype = nullptr;
        } else {
            dtype = create_dtype_direct();
        }
        if (!(update_header_from_doc(head_doc))) {
            ygglog_error << "parse_comm_header: Error updating header from JSON doc.";
            flags &= ~HEAD_FLAG_VALID;
            delete dtype;
            dtype = nullptr;
            free(head);
            return;
        }
        free(head);
    } catch(...) {
        ygglog_error << "parse_comm_header: C++ exception thrown.";
        flags &= ~HEAD_FLAG_VALID;
        if (head != nullptr)
            free(head);
    }
}

bool CommHead::update_header_from_doc(rapidjson::Value &head_doc) {
    // Type
    if (!(head_doc.IsObject())) {
        ygglog_error << "update_header_from_doc: head document must be an object.";
        return false;
    }
    // Size
    if (!(head_doc.HasMember("size"))) {
        ygglog_error << "update_header_from_doc: No size information in the header.";
        return false;
    }
    if (!(head_doc["size"].IsInt())) {
        ygglog_error << "update_header_from_doc: Size is not integer.";
        return false;
    }
    size = (size_t)(head_doc["size"].GetInt());
    if (bodysiz < size) {
        flags |= HEAD_FLAG_MULTIPART;
    } else {
        flags &= ~HEAD_FLAG_MULTIPART;
    }
    // Flag specifying that type is in data
    if (head_doc.HasMember("type_in_data")) {
        if (!(head_doc["type_in_data"].IsBool())) {
            ygglog_error << "update_header_from_doc: type_in_data is not boolean.";
            return false;
        }
        if (head_doc["type_in_data"].GetBool()) {
            flags |= HEAD_TYPE_IN_DATA;
        } else {
            flags &= ~HEAD_TYPE_IN_DATA;
        }
    }
    // String fields


    for (const auto& n : string_fields) {
        if (head_doc.HasMember(n.second.c_str())) {
            if (!(head_doc[n.second.c_str()].IsString())) {
                ygglog_error << "update_header_from_doc: '" << n.second << "' is not a string.";
                return false;
            }
            const std::string value(head_doc[n.second.c_str()].GetString());
            if (value.size() > COMMBUFFSIZ) {
                ygglog_error << "update_header_from_doc: Size of value for key '" << n.second << "' ("
                             << value.size() << ") exceeds size of target buffer (" << COMMBUFFSIZ << ").";
                return false;
            }
            switch (n.first) {
                case ADDRESS:
                    if (address != nullptr)
                        delete address;
                    address = new Address(value);
                    break;
                case ID:
                    id = value;
                    break;
                case REQUEST_ID:
                    request_id = value;
                    break;
                case RESPONSE_ADDRESS:
                    if (response_address != nullptr)
                        delete response_address;
                    response_address = new Address(value);
                    break;
                case ZMQ_REPLY:
                    if (zmq_reply != nullptr)
                        delete zmq_reply;
                    zmq_reply = new Address(value);
                    break;
                case ZMQ_REPLY_WORKER:
                    if (zmq_reply_worker != nullptr)
                        delete zmq_reply_worker;
                    zmq_reply_worker = new Address(value);
                    break;
                case MODEL:
                    model = value;
                    break;
            }
        }
    }

    // Return
    return true;
}

CommHead::~CommHead() {
    if (dtype != nullptr)
        delete dtype;
    if (address != nullptr)
        delete address;
    if (response_address != nullptr)
        delete response_address;
    if (zmq_reply != nullptr)
        delete zmq_reply;
    if (zmq_reply_worker != nullptr)
        delete zmq_reply_worker;
}
