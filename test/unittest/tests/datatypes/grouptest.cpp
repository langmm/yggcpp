#include <sstream>
#include <random>
#include "../unittest.hpp"
#include "datatypes/ValueGroup.hpp"
#include "datatypes/ValueArray1D.hpp"
#include "datatypes/Value.hpp"


using namespace communication;
using namespace communication::datatypes;

const std::string validValue = "valuegroup\n"
                               "10\n"
                               "3,3,3,4,4,3,3,5,2,1\n"
                               "8,32,16,64,16,64,16,4,8,0\n"
                               "0,1,1,1,0,0,1,1,0,1\n"
                               "end_header\n"
                               "item\n"
                               "scalarvalue\n"
                               "type 3\n"
                               "precision 8\n"
                               "unit pixel\n"
                               " value 22\n"
                               "item\n"
                               "scalararray\n"
                               " type 3\n"
                               " precision 32\n"
                               " dim1 1\n"
                               " dims 6\n"
                               " unit None\n"
                               " value 276,303,836,204,912,8\n"
                               "item\n"
                               "scalararray\n"
                               " type 3\n"
                               " precision 16\n"
                               " dim1 1\n"
                               " dims 5\n"
                               " unit None\n"
                               " value 410,493,793,600,646\n"
                               "item\n"
                               "scalararray\n"
                               " type 4\n"
                               " precision 64\n"
                               " dim1 1\n"
                               " dims 2\n"
                               " unit None\n"
                               " value -38,-75\n"
                               "item\n"
                               "scalarvalue\n"
                               "type 4\n"
                               "precision 16\n"
                               "unit None\n"
                               " value -11\n"
                               "item\n"
                               "scalarvalue\n"
                               "type 3\n"
                               "precision 64\n"
                               "unit None\n"
                               " value 601\n"
                               "item\n"
                               "scalararray\n"
                               " type 3\n"
                               " precision 16\n"
                               " dim1 1\n"
                               " dims 5\n"
                               " unit None\n"
                               " value 591,68,733,944,366\n"
                               "item\n"
                               "scalararray\n"
                               " type 5\n"
                               " precision 4\n"
                               " dim1 1\n"
                               " dims 3\n"
                               " unit None\n"
                               " value (-476.678650,695.015137),(539.314453,56.244991),(341.013641,-68.527588)\n"
                               "item\n"
                               "scalarvalue\n"
                               "type 2\n"
                               "precision 8\n"
                               "unit None\n"
                               " value -815.601681889522752\n"
                               "item\n"
                               "scalararray\n"
                               " type 1\n"
                               " precision 0\n"
                               " dim1 1\n"
                               " dims 6\n"
                               " unit None\n"
                               " value | | fa37JncCHryDsbzay|y4cBWDxS22JjzhMaiR|rV41mt|zxlYvKWrO72tK0LK0e1zLOZ|2nOXpPIhMFSv|8kP07U20o0J90xA0GW\n"
                               "end_valuegroup\n";
namespace {
std::random_device rd;
std::mt19937 mt(rd());
std::uniform_real_distribution<double> dist(-1000, 1000);
std::uniform_real_distribution<double> choice(0, 1000);

ValueItem* make_value();
ValueItem* make_array();

class ValueGroupTest : public ::testing::Test {
public:
    static std::vector<ValueItem *> items;
    ValueGroupTest() {
        size_t count = static_cast<size_t>(choice(mt))%10 + 3;
        items.reserve(count);
        for (auto i = 0; i < count; i++) {
            if (static_cast<int>(choice(mt)) % 2 == 0) {
                items.push_back(make_value());
            } else {
                items.push_back(make_array());
            }
        }
    }

    ~ValueGroupTest() {
        items.clear();
    }
};

std::vector<ValueItem *> ValueGroupTest::items = {};



std::string random_string() {
    size_t len = static_cast<size_t>(choice(mt)) %20 + 5;
    auto randchar = []() -> char
    {
        const char charset[] =
        "0123456789"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "abcdefghijklmnopqrstuvwxyz";
        const size_t max_index = (sizeof(charset) - 1);
        return charset[ rand() % max_index ];
    };
    std::string str(len,0);
    std::generate_n( str.begin(), len, randchar );
    return str;
}

ValueItem* make_array() {
    size_t len = static_cast<size_t>(choice(mt)) % 5 + 2;
    switch (static_cast<int>(choice(mt)) % 15) {
        case 0: {
            std::vector<int8_t> val(len);
            for (auto i = 0; i < len; i++)
                val[i] = static_cast<int8_t>(dist(mt)) % 255;
            return new ValueArray1D<int8_t>(val);
        }
        case 1: {
            std::vector<int16_t> val(len);
            for (auto i = 0; i < len; i++)
                val[i] = static_cast<int16_t>(dist(mt));
            return new ValueArray1D<int16_t>(val);
        }
        case 2: {
            std::vector<int32_t> val(len);
            for (auto i = 0; i < len; i++)
                val[i] = static_cast<int32_t>(dist(mt));
            return new ValueArray1D<int32_t>(val);
        }
        case 3: {
            std::vector<int64_t> val(len);
            for (auto i = 0; i < len; i++)
                val[i] = static_cast<int64_t>(dist(mt));
            return new ValueArray1D<int64_t>(val);
        }
        case 4: {
            std::vector<uint8_t> val(len);
            for (auto i = 0; i < len; i++)
                val[i] = static_cast<uint8_t>(abs(dist(mt))) % 255;
            return new ValueArray1D<uint8_t>(val);
        }
        case 5: {
            std::vector<uint16_t> val(len);
            for (auto i = 0; i < len; i++)
                val[i] = static_cast<uint16_t>(abs(dist(mt)));
            return new ValueArray1D<uint16_t>(val);
        }
        case 6: {
            std::vector<uint32_t> val(len);
            for (auto i = 0; i < len; i++)
                val[i] = static_cast<uint32_t>(abs(dist(mt)));
            return new ValueArray1D<uint32_t>(val);
        }
        case 7: {
            std::vector<uint64_t> val(len);
            for (auto i = 0; i < len; i++)
                val[i] = static_cast<uint64_t>(abs(dist(mt)));
            return new ValueArray1D<uint64_t>(val);
        }
        case 8: {
            std::vector<float> val(len);
            for (auto i = 0; i < len; i++)
                val[i] = static_cast<float>(dist(mt));
            return new ValueArray1D<float>(val);
        }
        case 9: {
            std::vector<double> val(len);
            for (auto i = 0; i < len; i++)
                val[i] = dist(mt);
            return new ValueArray1D<double>(val);
        }
        case 10: {
            std::vector<long double> val(len);
            for (auto i = 0; i < len; i++)
                val[i] = static_cast<long double>(dist(mt));
            return new ValueArray1D<long double>(val);
        }
        case 11: {
            std::vector<complex_float_t> val(len);
            for (auto i = 0; i < len; i++)
                val[i] = {static_cast<float>(dist(mt)), static_cast<float>(dist(mt))};
            return new ValueArray1D<complex_float_t>(val);
        }
        case 12: {
            std::vector<complex_double_t> val(len);
            for (auto i = 0; i < len; i++)
                val[i] = {dist(mt), dist(mt)};
            return new ValueArray1D<complex_double_t>(val);
        }
        case 13: {
            std::vector<complex_long_double_t> val(len);
            for (auto i = 0; i < len; i++)
                val[i] = {static_cast<long double>(dist(mt)), static_cast<long double>(dist(mt))};
            return new ValueArray1D<complex_long_double_t>(val);
        }
        case 14: {
            std::vector<std::string> val(len);
            for (auto i = 0; i < len; i++)
                val[i] = random_string();
            return new ValueArray1D<std::string>(val);
        }
    }
    return nullptr;
}

ValueItem* make_value() {
    switch (static_cast<int>(choice(mt)) % 15) {
        case 0:
            return new Value<int8_t>(static_cast<int8_t>(dist(mt))%255);
        case 1:
            return new Value<int16_t>(static_cast<int16_t>(dist(mt)));
        case 2:
            return new Value<int32_t>(static_cast<int32_t>(dist(mt)));
        case 3:
            return new Value<int64_t>(static_cast<int64_t>(dist(mt)));
        case 4:
            return new Value<uint8_t>(static_cast<uint8_t>(abs(dist(mt)))%255);
        case 5:
            return new Value<uint16_t>(static_cast<uint16_t>(abs(dist(mt))));
        case 6:
            return new Value<uint32_t>(static_cast<uint32_t>(abs(dist(mt))));
        case 7:
            return new Value<uint64_t>(static_cast<uint64_t>(abs(dist(mt))));
        case 8:
            return new Value<float>(static_cast<float>(dist(mt)));
        case 9:
            return new Value<double>(dist(mt));
        case 10:
            return new Value<long double>(static_cast<long double>(dist(mt)));
        case 11: {
            complex_float_t val = {static_cast<float>(dist(mt)), static_cast<float>(dist(mt))};
            return new Value<complex_float_t>(val);
        }
        case 12: {
            complex_double_t val = {dist(mt), dist(mt)};
            return new Value<complex_double_t>(val);
        }
        case 13: {
            complex_long_double_t val = {static_cast<long double>(dist(mt)), static_cast<long double>(dist(mt))};
            return new Value<complex_long_double_t>(val);
        }
        case 14: {
            std::string str = random_string();
            return new Value<std::string>(str);
        }
    }
    return nullptr;
}


TEST_F(ValueGroupTest, constructor) {
    ValueGroup vg(ValueGroupTest::items);
    EXPECT_EQ(vg.size(), ValueGroupTest::items.size());
    ValueGroup vg2;
    EXPECT_EQ(vg2.size(), 0);
}

TEST_F(ValueGroupTest, misc) {
    ValueGroup vg(ValueGroupTest::items);
    vg.display("");
    EXPECT_EQ(vg.nargs_exp(), 0);
    EXPECT_EQ(vg.getType(), T_GROUP);
    EXPECT_EQ(vg[1000], nullptr);
}

TEST_F(ValueGroupTest, addItem) {
    ValueGroup vg;
    for (auto i = 0; i < ValueGroupTest::items.size(); i++) {
        vg.addItem(ValueGroupTest::items[i]);
        EXPECT_EQ(vg.size(), i + 1);
    }
}

TEST_F(ValueGroupTest, insertItem) {
    ValueGroup vg;
    vg.insertItem(ValueGroupTest::items[0], 1000);
    EXPECT_EQ(vg.size(), 1);
    EXPECT_EQ(vg[0], ValueGroupTest::items[0]);
    vg.insertItem(ValueGroupTest::items[ValueGroupTest::items.size() - 1], 0);
    EXPECT_EQ(vg.size(), 2);
    EXPECT_EQ(vg[1], ValueGroupTest::items[0]);
    EXPECT_EQ(vg[0], ValueGroupTest::items[ValueGroupTest::items.size() - 1]);
}

TEST_F(ValueGroupTest, removeItem) {
    ValueGroup vg(ValueGroupTest::items);
    EXPECT_EQ(vg.size(), ValueGroupTest::items.size());
    vg.removeItem(1000);
    EXPECT_EQ(vg.size(), ValueGroupTest::items.size());
    vg.removeItem(1);
    EXPECT_EQ(vg.size(), ValueGroupTest::items.size() - 1);
    EXPECT_EQ(vg[1], ValueGroupTest::items[2]);
    vg.removeItem(0);
    EXPECT_EQ(vg.size(), ValueGroupTest::items.size() - 2);
    EXPECT_EQ(vg[0], ValueGroupTest::items[2]);
}

TEST_F(ValueGroupTest, pop) {
    ValueGroup vg(ValueGroupTest::items);
    EXPECT_EQ(vg.pop(1000), nullptr);
    ValueItem* vi = vg.pop(1);
    EXPECT_EQ(vg.size(), ValueGroupTest::items.size() - 1);
    EXPECT_EQ(vi, ValueGroupTest::items[1]);
}

TEST_F(ValueGroupTest, getItemTypes) {
    ValueGroup vg(ValueGroupTest::items);
    vg.getItemTypes();
    EXPECT_EQ(vg.size(), vg.types.size());
    EXPECT_EQ(vg.size(), vg.prec.size());
    EXPECT_EQ(vg.size(), vg.vtypes.size());
}

TEST_F(ValueGroupTest, readwrite) {
    ValueGroup vg(ValueGroupTest::items);
    std::stringstream ss;
    ss << vg;
    std::string s = ss.str();
    ValueGroup vg2;
    ss >> vg2;
    EXPECT_TRUE(vg == vg2);

    ss.str("");
    std::string invalidValue = validValue;
    std::string repl = "valuegroup";
    invalidValue.replace(invalidValue.find(repl), repl.size(), "valuegrop");
    ss << invalidValue;
    ValueGroup vg1;
    EXPECT_THROW(ss >> vg1, std::invalid_argument);
    invalidValue = validValue;
    repl = "end_header";
    ss.str("");
    invalidValue.replace(invalidValue.find(repl), repl.size(), "endheader");
    ss << invalidValue;
    EXPECT_THROW(ss >> vg1, std::invalid_argument);
    invalidValue = validValue;
    repl = "scalararray";
    ss.str("");
    invalidValue.replace(invalidValue.find(repl), repl.size(), "scalarrray");
    ss << invalidValue;
    EXPECT_THROW(ss >> vg1, std::invalid_argument);
    invalidValue = validValue;
    repl = "item";
    ss.str("");
    invalidValue.replace(invalidValue.find(repl), repl.size(), "itm");
    ss << invalidValue;
    EXPECT_THROW(ss >> vg1, std::invalid_argument);
}

TEST_F(ValueGroupTest, equality) {
    ValueGroup vg(ValueGroupTest::items);
    ValueGroup vg1;
    std::stringstream ss;
    ss << vg;
    ss >> vg1;
    EXPECT_TRUE(vg == vg1);
    vg1.removeItem(0);
    EXPECT_TRUE(vg != vg1);
    ss.str("");
    ss << vg;
    ss >> vg1;
    ValueItem* vt = vg1.pop_front();
    EXPECT_TRUE(vg != vg1);
    vg1.insertItem(vt, 1000);
    EXPECT_TRUE(vg != vg1);
}
}