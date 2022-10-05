#pragma once
#include <rapidjson/ply.h>
namespace communication {
namespace datatypes {

class YggPly : public rapidjson::Ply {
public:
    YggPly();
    YggPly(const YggPly* src);
    ~YggPly();
    void display_indent(const char* indent) const;
    void display() const;

private:
    void clear();
};
}
}
