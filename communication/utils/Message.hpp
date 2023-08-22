#pragma once
#include "serialization.hpp"

namespace communication {
  namespace utils {

    class Message {
    public:
      Message(const char* data, const size_t &len);
      Message(const char* data, const size_t &len,
	      const Header& header);
      Message(Message&& rhs);
      Message& operator=(Message&& rhs);
      size_t size() const { return msg.size(); }
      const char* c_str() const { return msg.c_str(); }
      std::string msg;
      Header head;
    };

  }
}

// Local Variables:
// mode: c++
// End:
