#include "Address.hpp"
#include <algorithm>

using namespace communication::utils;

Address::Address(const std::string &adr) {
    address(adr);
}

Address::Address(const char *adr) {
    std::string sadr;
    if (adr != NULL)
	sadr.assign(adr);
    address(sadr);
}

Address::Address(Address *adr) : Address(adr->address()){}

void Address::address(const std::string &addr) {
    _address = addr;
    _address.erase(std::remove_if(_address.begin(), _address.end(), ::isspace), _address.end());
    if (_address.size() > COMM_ADDRESS_SIZE)
        _address.resize(COMM_ADDRESS_SIZE);

    try {
        _key = std::stoi(addr);
    } catch (...) {
        _key = 0;
    }
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

bool Address::operator==(const Address &adr) {
    return this->_address == adr._address;
}

bool Address::valid() const {
    return _valid;
}
