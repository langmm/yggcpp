#pragma once
#include <rapidjson/ply.h>
#include "Ply.h"
namespace communication {
namespace datatypes {

class YggPly : public rapidjson::Ply {
public:
    YggPly();
    explicit YggPly(const YggPly* src);
    explicit YggPly(const ply_t* ply);
    ~YggPly();
    void display_indent(const char* indent) const;
    void display() const;
    ply_t* toPlyt();
private:
    void clear();
};
}
}
