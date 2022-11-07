#include <sstream>
#include "unittest.hpp"
#include "datatypes/ValueArray1D.hpp"

using namespace communication;
using namespace communication::datatypes;

namespace {
TEST(VALUEARRAY, constructor) {
    std::vector<double> v = {2.2,3.3,4.4};
    ValueArray1D<double> val(v);
    ValueArray1D<float> val2("pixels");

}

}