#pragma once
#include "Value.hpp"
#include <vector>
namespace communication {
namespace datatypes {


class ValueGroup {
public:
    ValueGroup() = default;

    ~ValueGroup();
    void addItem(ValueItem* item);
    void insertItem(ValueItem* item, const long& idx);
    void removeItem(const long& idx);
    ValueItem* pop(const long& idx);
    ValueItem* pop_fron() {return pop(0);}
    ValueItem* pop_back() {};
    std::string getItemTypes();
    ValueItem* operator[](const long& idx);
private:
    std::vector<ValueItem*> items;

};

} // communication
} // datatype
