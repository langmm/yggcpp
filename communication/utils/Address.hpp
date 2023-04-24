#pragma once
#include <string>
#include <ostream>

#define COMM_ADDRESS_SIZE 500

namespace communication {
namespace utils {
static std::string blank = "";
class Address {
public:
    Address(const std::string &adr = blank);

    Address(const char *adr);

    Address(Address *adr);

    const std::string &address() const;

    int key() const;

    void address(const std::string &addr);

    bool operator==(const Address &adr);

    bool valid() const;
    friend std::ostream &operator<<(std::ostream &out, const Address &addr) {
        out << addr._address;
        return out;
    }

#ifdef WITH_PYTHON
    std::string print() {return _address;}
#endif
private:
    std::string _address = "";
    int _key;
    bool _valid;

};
}
}
