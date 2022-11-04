#pragma once
#include <string>

#define COMM_ADDRESS_SIZE 500

namespace communication {
namespace utils {

class Address {
public:
    Address(const std::string &adr = "");

    Address(const char *adr);

    Address(Address *adr);

    const std::string &address() const;

    int key() const;

    void address(const std::string &addr);

    bool operator==(const Address &adr);

    bool valid() const;

private:
    std::string _address;
    int _key;
    bool _valid;

};
}
}
