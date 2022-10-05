#include "Address.hpp"

using namespace communication::utils;

Address::Address(const std::string &adr) {
    address(adr);
}

Address::Address(char *adr) {
    address(adr);
}

Address::Address(Address *adr) {
    address(adr->address());
}

void Address::address(const std::string &addr) {
    _address = addr;
    if (_address.size() > COMM_ADDRESS_SIZE)
        _address.resize(COMM_ADDRESS_SIZE);

    _key = stoi(addr);
    if (!_address.empty())
        _valid = true;
    else
        _valid = false;
}

const std::string &Address::address() const {
    return _address;
}

int Address::key() const {
    return _key;
}

bool Address::operator==(const Address *adr) {
    return this->_address == adr->_address;
}

bool Address::operator==(const Address &adr) {
    return this->_address == adr._address;
}

bool Address::valid() const {
    return _valid;
}
