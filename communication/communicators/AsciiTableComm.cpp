#include "AsciiTableComm.hpp"

using namespace communication::communicator;
using namespace communication::utils;
using namespace communication::datatypes;

unsigned AsciiTableComm::_yggAsciiTablesCreated = 0;

AsciiTableComm::AsciiTableComm(const std::string &name, const Direction &direct, DataType* datatype) : CommBase<asciiTable_t, int>(name, direct, ASCII_TABLE_COMM, datatype) {
    init(name, direct, datatype);
}

AsciiTableComm::~AsciiTableComm() {
    if (handle != nullptr) {
        handle->close();
        delete handle;
    }
}

void AsciiTableComm::init(const std::string &name, const Direction &direct,  DataType* datatype) {
    flags |= COMM_FLAG_FILE;
    address->address(name);
    handle = (asciiTable_t*)(dtype_ascii_table(datatype));
    int flag = 0;
    if (direction == SEND) {
        flag = handle->update(address->address().c_str(), "w");
    } else {
        flag = handle->update(address->address().c_str(), "r");
    }
    if (flag != 0) {
        ygglog_error("init_ascii_table_comm: Could not set asciiTable address.");
        return;
    }
    // Simplify received formats
    if (direction == RECV) {
        flag = simplify_formats(handle->format_str, YGG_MSG_MAX);
        if (flag < 0) {
            ygglog_error("init_ascii_table_comm: Failed to simplify recvd format.");
            return;
        }
    }
    // Open the table
    flag = handle->open();
    if (flag != 0) {
        ygglog_error("init_ascii_table_comm: Could not open %s", name.c_str());
        flags &= ~COMM_FLAG_VALID;
        return;
    }
    // Write format to file if "send"
    if (direction == SEND)
        handle->writeformat();
}

int AsciiTableComm::new_ascii_table_address() {
    name = "tempASCIITable." + std::to_string(_yggAsciiTablesCreated);
    init(name, direction, datatype);
    _yggAsciiTablesCreated++;
    return 0;
}

int AsciiTableComm::new_ascii_table_array_address() {
    name = "tempASCIITableArray." + std::to_string(_yggAsciiTablesCreated);
    init(name, direction, datatype);
    _yggAsciiTablesCreated++;
    return 0;
}

int AsciiTableComm::comm_nmsg() const {
    // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
    UNUSED(x);
#endif
    // TODO: Count lines in table.
    return 0;
}

int AsciiTableComm::send(const char *data, const size_t &len) {
    if (is_eof(data))
        return 0;
    // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
    UNUSED(len);
#endif
    return handle->writeline_full(data);
}

long AsciiTableComm::recv(char **data, const size_t &len, bool allow_realloc) {
    return handle->readline_full_realloc(data, len, allow_realloc);
}
