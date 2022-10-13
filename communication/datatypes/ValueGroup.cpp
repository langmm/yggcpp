#include "ValueGroup.hpp"
#include "value_t.h"

namespace communication {
namespace datatypes {
ValueGroup::~ValueGroup() {
    for (auto a : items)
        delete a;
    items.clear();
}

void ValueGroup::addItem(ValueItem *item) {
    items.push_back(item);
}

void ValueGroup::insertItem(ValueItem *item, const long &idx) {
    if (idx >= items.size())
        items.push_back(item);
    else
        items.insert(items.begin() + idx, item);
}

void ValueGroup::removeItem(const long &idx) {
    if (idx >= items.size())
        return;
    delete items[idx];
    items.erase(items.begin() + idx);
}

ValueItem* ValueGroup::pop(const long& idx) {
    if (idx >= items.size())
        return nullptr;
    auto item = items[idx];
    items.erase(items.begin() + idx);
    return item;
}

ValueItem* ValueGroup::operator[](const long &idx) {
    if (idx >= items.size())
        return nullptr;
    return items[idx];
}

std::string ValueGroup::getItemTypes() {
    std::string types;
    for (auto i : items) {
        switch (i->type) {
            case T_INT:
                types += "i";
                break;
            case T_FLOAT:
                types += "f";
                break;
            case T_BOOLEAN:
                types += "b";
                break;
            case T_STRING:
                types += "s";
                break;
            case T_COMPLEX:
                types += "c";
                break;
            case T_UINT:
                types += "u";
                break;
        }
    }
    return types;
}
} // communication
} // datatype

value_group_t* init_valgroup() {
    auto grp = (value_group_t*)malloc(sizeof(value_group_t));
    auto vg = new communication::datatypes::ValueGroup();
    grp->obj = vg;
    return grp;
}
