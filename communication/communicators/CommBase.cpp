#include "CommBase.hpp"


using namespace communication::communicator;
using namespace communication::utils;
using namespace communication::datatypes;


Comm_t::Comm_t(Address *address, communication::Direction dirn, const communication::comm_type &t,
               DataType *datatype, int flgs) : address(address), type(t), _valid(false), direction(direction), flags(flgs) {
    name = "";

    flags |= COMM_FLAG_VALID;
    if (direction == NONE)
        flags &= ~COMM_FLAG_VALID;

    datatype = complete_dtype(datatype, false);
    if (datatype == nullptr)
        EXCEPTION;
    maxMsgSize = YGG_MSG_MAX;
    const_flags = 0;
    thread_id = get_thread_id();
    char *allow_threading = getenv("YGG_THREADING");
    if (allow_threading != nullptr)
        flags |= COMM_ALLOW_MULTIPLE_COMMS;

    info = nullptr;
    msgBufSize = 0;
    index_in_register = -1;
    last_send = nullptr;
}

Comm_t::Comm_t(const std::string &name, communication::Direction direction, const communication::comm_type &t,
               DataType *datatype) : Comm_t(new Address(), direction, t, datatype) {
    std::string full_name;
    if (!name.empty()) {
        full_name = name;
        if (full_name.size() > COMM_NAME_SIZE)
            full_name.resize(COMM_NAME_SIZE);
        if (direction != NONE) {
            if (direction == SEND) {
                full_name += "_OUT";
            } else if (direction == RECV) {
                full_name += "_IN";
            }
        }
        char *model_name = getenv("YGG_MODEL_NAME");
        char *addr = std::getenv(full_name.c_str());
        if (addr == nullptr && model_name != nullptr) {
            std::string prefix(model_name);
            prefix += ":";
            if (prefix.size() > COMM_NAME_SIZE)
                prefix.resize(COMM_NAME_SIZE);
            if (full_name.rfind(prefix, 0) != 0) {
                prefix += full_name;
                full_name = prefix;
                addr = std::getenv(full_name.c_str());
            }
        }
        if (addr == nullptr) {
            std::string temp_name(full_name);
            size_t loc;
            while ((loc = temp_name.find(":")) != std::string::npos) {
                temp_name.replace(loc, 1, "__COLON__");
            }
            addr = getenv(temp_name.c_str());
        }
        ygglog_debug("init_comm_base: model_name = %s, full_name = %s, address = %s",
                     model_name, full_name.c_str(), addr);
        this->name = full_name;
        if (addr != nullptr) {
            this->address->address(addr);
        }
        this->name = name;
    } else {
        flags &= ~COMM_FLAG_VALID;
    }

    if (!this->address->valid() && t != SERVER_COMM && t != CLIENT_COMM) {
        ygglog_error("init_comm_base: %s not registered as environment variable.\n",
                     full_name.c_str());
        flags &= ~COMM_FLAG_VALID;
    }
    ygglog_debug("init_comm_base(%s): Done", name.c_str());

}

/*Comm_t::Comm_t(const communication::Comm_t *comm, comm_type type) {
    this->type = type;
    name =  comm->name; //!< Comm name.

    *address = *(comm->address);
    direction = comm->direction;
    flags = comm->flags;
    const_flags = comm->const_flags;
    info = comm->info;
    *datatype = *(comm->datatype);
    maxMsgSize = comm->maxMsgSize;
    msgBufSize = comm->msgBufSize;
    index_in_register = comm->index_in_register;
    *last_send = *(comm->last_send);
    thread_id = comm->thread_id;
    _valid = comm->_valid;
}*/

Comm_t::~Comm_t() {
    ygglog_debug("~CommBase: Started");
    if (last_send != nullptr)
        delete last_send;
    if (datatype != nullptr)
        delete datatype;
    if (address != nullptr)
        delete address;
    ygglog_debug("~CommBase: Finished");
}

bool Comm_t::check_size(const size_t &len) const {
    // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
    UNUSED(data);
#endif
    // Make sure you aren't sending a message that is too big
    if (len > YGG_MSG_MAX) {
        ygglog_error("comm_base_send(%s): message too large for single packet (YGG_MSG_MAX=%d, len=%d)",
                     name.c_str(), YGG_MSG_MAX, len);
        return false;
    }
    return true;
}
