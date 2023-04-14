#include "../../unittest.hpp"
#include "utils/serialization.hpp"

using namespace communication::utils;

// TODO: Add tests for HEADER

TEST(Header, Constructor) {
  Header cha;
  EXPECT_EQ(cha.flags, HEAD_FLAG_VALID);
}
