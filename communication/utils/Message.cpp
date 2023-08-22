#include "Message.hpp"

using namespace communication::utils;

Message::Message(const char* data, const size_t &len) :
  msg(data, len), head() {}
Message::Message(const char* data, const size_t &len,
		 const Header& header) :
  msg(data, len), head() {
  head.CopyFrom(header);
}
Message::Message(Message&& rhs) :
  msg(std::forward<std::string>(rhs.msg)),
  head(std::forward<Header>(rhs.head)) {}
Message& Message::operator=(Message&& rhs) {
  this->~Message();
  new (this) Message(std::move(rhs));
  return *this;
}
