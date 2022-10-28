#pragma once
#include "ValueGroup.hpp"

namespace communication {
namespace datatypes {

class FormattedData : public ValueGroup {
public:
    FormattedData() = delete;
    FormattedData(const std::string& format, bool as_array);

    void display(const std::string& indent) const override;
    int nargs_exp() const override {return 2;}
    DTYPE getType() const override {return T_FORMATTED;}

    ~FormattedData() = default;
};

} // communication
} // datatypes
