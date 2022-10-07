#pragma once
#include <vector>
#include <rapidjson/obj.h>
#include "Obj.h"

namespace communication {
namespace datatypes {

class YggObj : public rapidjson::ObjWavefront {
public:
    YggObj();

    explicit YggObj(const YggObj *src);

    explicit YggObj(const obj_t *obj);

    ~YggObj() override;

    obj_t *toObjt();
};
}
}
