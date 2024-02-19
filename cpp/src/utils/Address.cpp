#include "utils/Address.hpp"
#include <algorithm>

using namespace YggInterface::utils;

Address::Address(const std::string &adr): _key(-1), _valid(false) {
    address(adr);
}

Address::Address(const char *adr): _key(-1), _valid(false) {
    std::string sadr;
    if (adr != nullptr)
        sadr.assign(adr);
    address(sadr);
}

Address::Address(const Address& adr): _valid(false) {
    address(adr.address());
}

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

Address& Address::operator=(const Address& adr) {
    address(adr.address());
    return *this;
}

bool Address::valid() const {
    return _valid;
}
