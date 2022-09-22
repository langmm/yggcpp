#include "AsciiFileComm.hpp"

using namespace communication::communicator;
using namespace communication::utils;
using namespace communication::datatypes;

unsigned AsciiFileComm::_yggAsciiFilesCreated = 0;

AsciiFileComm::AsciiFileComm(const std::string &name, const Direction &direct, DataType *datatype) :
        CommBase<asciiFile_t, int>(name, direct, ASCII_FILE_COMM, datatype) {
    init(name, direct, datatype);
}

AsciiFileComm::~AsciiFileComm() {
    if (handle != nullptr) {
        handle->close();
        delete handle;
    }
}
void AsciiFileComm::init(const std::string &name, const Direction &direct, DataType* datatype) {
    // Don't check base validity since address is name
    flags |= COMM_FLAG_FILE;
    address->address(name);

    if (direction == SEND) {
        handle = new asciiFile_t(address->address().c_str(), "w", nullptr, nullptr);
    } else {
        handle =  new asciiFile_t(address->address().c_str(), "r", nullptr, nullptr);
    }
    int ret = handle->open();
    if (ret != 0) {
        ygglog_error("init_ascii_file_comm: Could not open %s", name.c_str());
        flags &= ~COMM_FLAG_VALID;
    }
}

void AsciiFileComm::new_ascii_file_address() {
    name = "temp" + std::to_string(_yggAsciiFilesCreated);
    init(name, direction, datatype);
}

int AsciiFileComm::comm_nmsg() const {
    // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
    UNUSED(x);
#endif
    // TODO: Count lines in file.
    return 0;
}

int AsciiFileComm::send(const char *data, const size_t &len) {
    if (is_eof(data))
        return 0;
    // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
    UNUSED(len);
#endif
    return handle->writeline_full(data);
}

long AsciiFileComm::recv(char **data, const size_t &len, bool allow_realloc) {
    if (allow_realloc)
        return handle->readline_full(data, (size_t*)(&len));
    return handle->readline_full_norealloc(data[0], len);
}