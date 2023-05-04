#pragma once
#include <string>
#include <ostream>

#define COMM_ADDRESS_SIZE 500

namespace communication {
namespace utils {
static std::string blank = "";

/**
 * Class for holding an address, used by the communicators
 */
class Address {
public:
    /**
     * Create a new instance of the Address class with the given input
     * @param adr The address to use, as a std::string, defaults to empy string.
     */
    Address(const std::string &adr = blank);

    /**
     * Create a new instance of the Address class with the given input
     * @param adr The address to use, as a char*
     */
    Address(const char *adr);

    /**
     * Copy constructor of the Address class
     * @param adr The instance to copy
     */
    Address(Address *adr);

    /**
     * Get the address from the class as a std::string
     * @return the current address
     */
    const std::string &address() const;

    /**
     * Get the key to this instance as in int
     * @return int
     */
    int key() const;

    /**
     * Set the current address
     * @param addr The new address to use
     */
    void address(const std::string &addr);

    /**
     * Equality comparison for the class
     * @param adr The instance to compare this one to
     * @return bool
     */
    bool operator==(const Address &adr);

    /**
     * Whether or not this instance is has a valid address
     * @return bool
     */
    bool valid() const;
    friend std::ostream &operator<<(std::ostream &out, const Address &addr) {
        out << addr._address;
        return out;
    }

#ifdef WITH_PYTHON
    /**
     * Print the address to the screen from python.
     * @return
     */
    std::string print() {return _address;}
#endif
private:
    std::string _address = "";  // the address
    int _key;                   // the unique key
    bool _valid;                // validity flag

};
}
}
