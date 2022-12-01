#include "ValueGroup.hpp"
#include <algorithm>
#include "dtype_t.h"
#include "Value.hpp"

namespace communication {
namespace datatypes {
ValueGroup::~ValueGroup() {
    for (auto a : items)
        delete a;
    items.clear();
    typesCalculated = false;
}

void ValueGroup::display(const std::string &indent) const {
    printf("%s%-15s = %s\n", indent.c_str(), "type", "GROUP");
    printf("%s%-15s = %zu\n", indent.c_str(), "number", items.size());
    for (auto i : items) {
        printf("%s  %-15s\n", indent.c_str(), "Item");
        i->display(indent + "  ");
    }
}

void ValueGroup::addItem(ValueItem *item) {
    items.push_back(item);
    typesCalculated = false;
}

void ValueGroup::insertItem(ValueItem *item, const long &idx) {
    if (idx >= items.size())
        items.push_back(item);
    else
        items.insert(items.begin() + idx, item);
    typesCalculated = false;
}

void ValueGroup::removeItem(const long &idx) {
    if (idx >= items.size())
        return;
    delete items[idx];
    items.erase(items.begin() + idx);
    typesCalculated = false;
}

ValueItem* ValueGroup::pop(const long& idx) {
    if (idx >= items.size())
        return nullptr;
    auto item = items[idx];
    items.erase(items.begin() + idx);
    typesCalculated = false;
    return item;
}

ValueItem* ValueGroup::operator[](const long &idx) {
    if (idx >= items.size())
        return nullptr;
    typesCalculated = false;
    return items[idx];
}

void ValueGroup::getItemTypes() {
    types.clear();
    prec.clear();
    vtypes.clear();
    types.reserve(items.size());
    prec.reserve(items.size());
    vtypes.reserve(items.size());
    for (auto i : items) {
        types.emplace_back(i->type);
        prec.emplace_back(i->getPrecision());
        vtypes.emplace_back(i->vtype);
    }
    typesCalculated = true;
}

std::ostream& ValueGroup::write(std::ostream &out) {
    if (!typesCalculated)
        getItemTypes();
    out << _type << std::endl << items.size() << std::endl;
    communication::utils::join(types, out);
    out << std::endl;
    communication::utils::join(prec, out);
    out << std::endl;
    communication::utils::join(vtypes, out);
    out << std::endl << "end_header" << std::endl;

    for (auto i : items) {
        out << "item" << std::endl;
        out << i << std::endl;
    }
    out << "end_" << _type << std::endl;
    return out;
}
std::istream& ValueGroup::read(std::istream &in) {
    std::string word;
    in >> std::ws;
    in >> word;
    if (word != _type)
        throw std::invalid_argument("Invalid format, expected " + _type + " but got " + word + "instead");
    int count;
    in >> count;
    communication::utils::parse<SUBTYPE>(types, count, in);
    communication::utils::parse<uint8_t>(prec, count, in);
    communication::utils::parse<VTYPE>(vtypes, count, in);
    items.clear();
    items.reserve(count);
    in >> word;
    if (word != "end_header")
        throw std::invalid_argument("Invalid format, expected end_header but got " + word + "instead");
    communication::datatypes::ValueItem* vi;
    for (auto i = 0; i < count; i++) {
        in >> word;
        if (word != "item")
            throw std::invalid_argument("Invalid format, expected item but got " + word + "instead");
        switch (vtypes[i]) {
            case T_ARRAY1D:
                vi = createArray(types[i], prec[i], count, "");
                break;
            case T_SCALABLE:
                vi = createValue(types[i], prec[i], "");
                break;
        }
        vi->read(in);
        items.emplace_back(vi);
    }
    return in;
}

inline std::ostream & operator << (std::ostream &out, ValueGroup &vg) {
    return vg.write(out);
}
inline std::istream & operator >> (std::istream &in, ValueGroup &vg) {
    return vg.read(in);
}

} // communication
} // datatype

dtype_t* init_valgroup() {
    auto grp = (dtype_t*)malloc(sizeof(dtype_t));
    auto vg = new communication::datatypes::ValueGroup();
    grp->obj = vg;
    grp->type = T_GROUP;
    return grp;
}
