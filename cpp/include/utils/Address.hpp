#pragma once
#include "YggInterface_export.h"
#include <string>
#include <ostream>

#define COMM_ADDRESS_SIZE 500

namespace YggInterface {
namespace utils {
static std::string blank;

/**
 * @brief Class for holding an address, used by the communicators
 */
class Address {
public:
    /**
     * @brief Create a new instance of the Address class with the given input
     * @param adr The address to use, as a std::string, defaults to empy string.
     */
    YGG_API explicit Address(const std::string &adr = blank);

    /**
     * @brief Create a new instance of the Address class with the given input
     * @param adr The address to use, as a char*
     */
    YGG_API explicit Address(const char *adr);

    /**
     * @brief Copy constructor of the Address class
     * @param adr The instance to copy
     */
    YGG_API Address(const Address &adr);
    /**
     * @brief Copy constructor of the Address class
     * @param adr The instance to copy
     */
    YGG_API explicit Address(const Address* adr);

    /**
     * @brief Get the address from the class as a std::string
     * @return the current address
     */
    YGG_API const std::string &address() const;

    /**
     * @brief Get the key to this instance as in int
     * @return int
     */
    YGG_API int key() const;

    /**
     * @brief Set the current address
     * @param addr The new address to use
     */
    YGG_API void address(const std::string &addr);

    /**
     * @brief Equality comparison for the class
     * @param adr The instance to compare this one to
     * @return bool
     */
    YGG_API bool operator==(const Address &adr);

    /**
     * @brief Assignment operator
     * @param[in] adr Address to copy
     * @return A reference to this instance
     */
    YGG_API Address& operator=(const Address& adr);
    /**
     * @brief Whether or not this instance is has a valid address
     * @return bool
     */
    YGG_API bool valid() const;

    /**
     * @brief Invalidate and clear the address
     */
    void invalidate() {
        _valid = false;
        _address.clear();
    }
    /**
     * @brief Output stream operator
     * @param[in,out] out Output stream
     * @param[in] addr Address to output to the stream
     * @return Output stream
     */
    friend std::ostream &operator<<(std::ostream &out, const Address &addr) {
        out << addr._address;
        return out;
    }

private:
    std::string _address;       //!< the address
    int _key{};                 //!< the unique key
    bool _valid;                //!< validity flag

};

  static Address blankAddress;  //!< empty address singleton
  
}
}
