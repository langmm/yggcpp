#include "comms.hpp"

using namespace communication::communicator;
using namespace communication::utils;


Comm_t::Comm_t(Address *address, DIRECTION dirn, const COMM_TYPE &t, int flgs) :
        address(address), type(t), direction(dirn), flags(flgs) {
    name = "";

    flags |= COMM_FLAG_VALID;
    if (direction == NONE)
        flags &= ~COMM_FLAG_VALID;

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

Comm_t::Comm_t(const std::string &name, DIRECTION direction, const COMM_TYPE &t) :
        Comm_t(new Address(), direction, t) {
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
        ygglog_debug << "init_comm_base: model_name = " << model_name << ", full_name = " << full_name
                     << ", address = " << addr;
        this->name = full_name;
        if (addr != nullptr) {
            this->address->address(addr);
            if (this->address->valid())
                flags |= COMM_FLAG_VALID;
        }
        this->name = name;
    } else {
        flags &= ~COMM_FLAG_VALID;
    }

    if (!this->address->valid() && t != SERVER_COMM && t != CLIENT_COMM) {
        ygglog_error << "init_comm_base: " << full_name << " not registered as environment variable.\n";
        flags &= ~COMM_FLAG_VALID;
    }
    ygglog_debug << "init_comm_base(" << name << "): Done";

}

/*Comm_t::Comm_t(const communication::Comm_t *comm, COMM_TYPE type) {
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
}*/

Comm_t::~Comm_t() {
    ygglog_debug << "~CommBase: Started";
    if (last_send != nullptr)
        delete last_send;
    if (address != nullptr)
        delete address;
    ygglog_debug << "~CommBase: Finished";
}

bool Comm_t::check_size(const size_t &len) const {
    // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
    UNUSED(data);
#endif
    // Make sure you aren't sending a message that is too big
    if (len > YGG_MSG_MAX) {
        ygglog_error << "comm_base_send(" << name << "): message too large for single packet (YGG_MSG_MAX="
                     << YGG_MSG_MAX << ", len=" << len << ")";
        return false;
    }
    return true;
}

/*int free_comm(comm_t* x) {
    auto temp = static_cast<Comm_t*>(x->comm);
    if (temp != nullptr) {
        delete temp;
        temp = nullptr;
        free(x);
    }
    return 0;
}*/
//comm_t* empty_comm() {
//    comm_t* comm;
//
//}
Comm_t* communication::communicator::new_Comm_t(const DIRECTION dir, const COMM_TYPE type, const std::string &name, char* address) {
    switch(type) {
        case NULL_COMM:
            break;
        case IPC_COMM:
            return new IPCComm(name, (address == nullptr) ? nullptr : new Address(address), dir);
        case ZMQ_COMM:
            return new ZMQComm(name, (address == nullptr) ? nullptr : new Address(address), dir);
        case SERVER_COMM:
            return new ServerComm(name, (address == nullptr) ? nullptr : new Address(address), dir);
        case CLIENT_COMM:
            return new ClientComm(name, (address == nullptr) ? nullptr : new Address(address));
        case MPI_COMM:
            //std::string adr;
            return new MPIComm(name, (address == nullptr) ? nullptr : new Address(address), dir);
    }
    return nullptr;
}
/*comm_t* new_comm(char* address, const DIRECTION dir, const COMM_TYPE type) {
    auto comm = (comm_t*)malloc(sizeof(comm_t));
    comm->comm = new_Comm_t(dir, type, "", address);
    return comm;
}
comm_t* init_comm(const char* name, const DIRECTION dir, const COMM_TYPE type) {
    auto comm = (comm_t*)malloc(sizeof(comm_t));
    comm->comm = new_Comm_t(dir, type, name);
    return comm;
}*/

int Comm_t::send(const dtype_t *dtype) {
    rapidjson::Document *doc = static_cast<rapidjson::Document*>(dtype->schema);
    rapidjson::StringBuffer buffer;
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
    doc->Accept(writer);
    return this->send(buffer.GetString(), buffer.GetSize());
}

long Comm_t::recv(dtype_t *dtype) {
    char *data = (char *) malloc(sizeof(char) * YGG_MSG_MAX);
    long size = recv(data, YGG_MSG_MAX, true);
    if (dtype->schema == nullptr)
        dtype->schema = (void *) new rapidjson::Document();
    static_cast<rapidjson::Document *>(dtype->schema)->Parse(data);
    free(data);
    return size;
}

/*int send(const comm_t* x, const char *data, const size_t &len) {
    auto comm = (Comm_t*)x->comm;
    return comm->send(data, len);
}
long recv(comm_t* x, char **data, const size_t &len, bool allow_realloc) {
    auto comm = (Comm_t*)x->comm;
    return comm->recv(data, len, allow_realloc);
}
int comm_nmsg(const comm_t* x) {
    auto comm = (Comm_t*)x->comm;
    return comm->comm_nmsg();
}
 */