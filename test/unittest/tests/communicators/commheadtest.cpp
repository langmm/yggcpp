#include "../../unittest.hpp"
#include "utils/Address.hpp"
#include "communicators/CommHead.hpp"

using namespace communication;
using namespace communication::datatypes;

TEST(CommHead, Constructor) {
    utils::Address *adr = new utils::Address("this.is.a.test");
    CommHead cha(adr);
    EXPECT_EQ(cha.flags, HEAD_FLAG_VALID);
}
