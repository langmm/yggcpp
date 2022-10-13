#include "Value.hpp"
#include "ValueGroup.hpp"
#include "dtype_t.h"
namespace communication {
namespace datatypes {
void set_unit(char* u, const char* unit, const size_t &unit_size) {
    if (unit_size != 0) {
        u = (char*)malloc(sizeof(char) * unit_size);
        memcpy(u, unit, unit_size);
    } else {
        u = nullptr;
    }
}

} // communication
} // datatype

value_t* Int_value(int &value, const char* unit, const size_t &unit_size) {
    auto val_t = (value_t*)malloc(sizeof(value_t));
    auto v = (int*)malloc(sizeof(int));
    *v = value;
    val_t->obj = v;
    val_t->type = T_INT;
    communication::datatypes::set_unit(val_t->unit, unit, unit_size);
    return val_t;
}
value_t* Bool_value(bool value, const char* unit, const size_t &unit_size) {
    auto val_t = (value_t*)malloc(sizeof(value_t));
    auto v = (bool*)malloc(sizeof(bool));
    *v = value;
    val_t->obj = v;
    val_t->type = T_BOOLEAN;
    communication::datatypes::set_unit(val_t->unit, unit, unit_size);
    return val_t;
}
value_t* String_value(char* value, const size_t &size, const char* unit, const size_t &unit_size) {
    auto val_t = (value_t*)malloc(sizeof(value_t));
    auto v = (char*)malloc(sizeof(char) * size);
    memcpy(v, value, size);
    val_t->obj = v;
    val_t->type = T_INT;
    communication::datatypes::set_unit(val_t->unit, unit, unit_size);
    return val_t;
}
value_t* Number_value() {
    return nullptr;
}
value_t* Float_value(float &value, const char* unit, const size_t &unit_size) {
    auto val_t = (value_t*)malloc(sizeof(value_t));
    auto v = (float*)malloc(sizeof(float));
    *v = value;
    val_t->obj = v;
    val_t->type = T_INT;
    communication::datatypes::set_unit(val_t->unit, unit, unit_size);
    return val_t;
}
value_t* UInt_value(uint& value, const char* unit, const size_t &unit_size) {
    auto val_t = (value_t*)malloc(sizeof(value_t));
    auto v = (unsigned int*)malloc(sizeof(uint));
    *v = value;
    val_t->obj = v;
    val_t->type = T_INT;
    communication::datatypes::set_unit(val_t->unit, unit, unit_size);
    return val_t;
}
value_t* ComplexFloat_value(complex_float_t& value, const char* unit, const size_t &unit_size) {
    auto val_t = (value_t*)malloc(sizeof(value_t));
    auto v = (complex_float_t*)malloc(sizeof(complex_float_t));
    *v = value;
    val_t->obj = v;
    val_t->type = T_INT;
    communication::datatypes::set_unit(val_t->unit, unit, unit_size);
    return val_t;
}
value_t* Bytes_value() {
    return nullptr;
}
value_t* Unicode_value(char* value, const char* unit, const size_t &unit_size) {
    return nullptr;
}

void add_VtoGroup(value_t* val, value_group_t* grp) {
    communication::datatypes::ValueItem* v;
    switch (val->type) {
        case T_INT:
            v = new communication::datatypes::Value<int>(val);
            break;
        case T_BOOLEAN:
            v = new communication::datatypes::Value<bool>(val);
            break;
        case T_FLOAT:
            v = new communication::datatypes::Value<float>(val);
            break;
        case T_UINT:
            v = new communication::datatypes::Value<uint>(val);
            break;
        case T_STRING:
            v = new communication::datatypes::Value<std::string>(val);
            break;
        case T_COMPLEX:
            v = new communication::datatypes::Value<complex_float_t>(val);
            break;
    }
    static_cast<communication::datatypes::ValueGroup*>(grp->obj)->addItem(v);
}

dtype_t* create_dtype_scalar(SUBTYPE type, size_t precision=0,
                             const char* units=nullptr) {

}
