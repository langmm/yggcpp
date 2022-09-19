#pragma once
#include "CommBase.hpp"
#include "datatypes/AsciiTable.hpp"

namespace communication {
namespace communicator {

class AsciiTableComm  : public CommBase<datatypes::asciiTable_t, int>{
public:
    AsciiTableComm(const std::string &name, Direction &direct, datatypes::DataType* datatype);

    ~AsciiTableComm();
    int send(const char* data, const size_t &len) override;
    long recv(char** data, const size_t &len, bool allow_realloc) override;
    int comm_nmsg() const override;
private:
    void init(const std::string &name, Direction &direct, datatypes::DataType* datatype);
    int new_ascii_table_address();
    int new_ascii_table_array_address();
    static unsigned _yggAsciiTablesCreated;
};

} // communication
