#pragma once
#include "CommBase.hpp"
#include "datatypes/AsciiFile.hpp"
namespace communication {
namespace communicator {

class AsciiFileComm : public CommBase<datatypes::asciiFile_t, int> {
public:
    AsciiFileComm(const std::string &name, Direction &direct, datatypes::DataType *datatype);

    ~AsciiFileComm();

    int send(const char *data, const size_t &len) override;

    long recv(char **data, const size_t &len, bool allow_realloc) override;

    int comm_nmsg() const override;

private:
    static unsigned _yggAsciiFilesCreated;

    void init(const std::string &name, Direction &direct, datatypes::DataType *datatype);

    void new_ascii_file_address();
};

}
} // communication
