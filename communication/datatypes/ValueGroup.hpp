#pragma once
#include "Value.hpp"
#include "utils/tools.hpp"
#include <vector>

#define T_INT_STR "i"
#define T_FLOAT_STR "f"
#define T_BOOL_STR "b"
#define T_STRING_STR "s"
#define T_COMP_STR "x"
#define T_UINT_STR "u"
#define T_BYTES_STR "y"
#define T_UNICODE_STR "z"
//namespace rapidjson {
//class Document;
//}
namespace communication {
namespace datatypes {


class ValueGroup : public DataType {
public:
    explicit ValueGroup(const std::string &type="valuegroup") : DataType(), _type(type) {}


    ~ValueGroup();
    void addItem(ValueItem* item);
    void insertItem(ValueItem* item, const long& idx);
    void removeItem(const long& idx);
    ValueItem* pop(const long& idx);
    ValueItem* pop_fron() {return pop(0);}
    ValueItem* pop_back() {
        auto r = items.back();
        items.pop_back();
        return r;
    }
    void getItemTypes();
    ValueItem* operator[](const long& idx);

    void display(const std::string& indent) const override;
    int nargs_exp() const override {return 0;}
    DTYPE getType() const override {return T_GROUP;}

    std::ostream& write(std::ostream &out) override;
    std::istream& read(std::istream &in) override;
    friend std::ostream & operator << (std::ostream &out, ValueGroup &vg);
    friend std::istream & operator >> (std::istream &in,  ValueGroup &vg);

protected:
    std::vector<ValueItem*> items;
    std::vector<SUBTYPE> types;
    std::vector<VTYPE> vtypes;
    std::vector<uint8_t> prec;

    const std::string _type;
private:

    bool typesCalculated = false;
};


} // communication
} // datatype
