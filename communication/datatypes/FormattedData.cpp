#include "FormattedData.hpp"
#include "utils/regex.hpp"
#include "utils/logging.hpp"
#include "dtype_t.h"
#define CSafe(x)  \
  try          \
    {          \
      x;      \
    }          \
  catch(...)      \
    {          \
      ygglog_error << "C++ exception thrown.";    \
    }

namespace communication {
namespace datatypes {
FormattedData::FormattedData(const std::string &format_str, const bool as_array) : ValueGroup("formatteddata") {
    // Loop over string
    size_t mres;
    size_t sind, eind, beg = 0, end;
    char re_fmt[FMT_LEN];
    char re_fmt_eof[FMT_LEN];
    sprintf(re_fmt, "%%[^%s%s ]+[%s%s ]", "\t", "\n", "\t", "\n");
    sprintf(re_fmt_eof, "%%[^%s%s ]+", "\t", "\n");


    size_t iprecision = 0;
    while (beg < format_str.size()) {
        SUBTYPE isubtype;
        mres = utils::find_match(utils::RE_FMT, format_str, beg, sind, eind);
        if (mres == 0) {
            // Make sure it's not just a format string with no newline
            mres = utils::find_match(utils::RE_FMT_EOF, format_str, beg, sind, eind);
            if (mres <= 0) {
                beg++;
                continue;
            }
        }
        beg += sind;
        end = beg + (eind - sind);
        std::string ifmt = format_str.substr(beg, end);
        // String
        if (utils::find_match(utils::RE_STRING, ifmt, sind, eind)) {
            isubtype = T_STRING;
            utils::regex_replace(ifmt, utils::RE_STRLEN, "$2", 0);
            iprecision = 8 * std::stoi(ifmt);
            // Complex
        } else if (utils::find_match(utils::RE_COMPLEX, ifmt, sind, eind)) {
            isubtype = T_COMPLEX;
            iprecision = 8 * 2 * sizeof(double);
        }
            // Floats
        else if (utils::find_match(utils::RE_FLOAT, ifmt, sind, eind)) {
            isubtype = T_FLOAT;
            iprecision = 8 * sizeof(double);
        }
            // Integers
        else if (utils::find_match(utils::RE_CHAR, ifmt, sind, eind)) {
            isubtype = T_INT;
            iprecision = 8 * sizeof(char);
        } else if (utils::find_match(utils::RE_SHORT, ifmt, sind, eind)) {
            isubtype = T_INT;
            iprecision = 8 * sizeof(short);
        } else if (utils::find_match(utils::RE_LONG_LONG, ifmt, sind, eind) ||
                   utils::find_match(utils::RE_LONG_LONG2, ifmt, sind, eind)) {
            isubtype = T_INT;
            iprecision = 8 * sizeof(long long);
        } else if (utils::find_match(utils::RE_LONG, ifmt, sind, eind)) {
            isubtype = T_INT;
            iprecision = 8 * sizeof(long);
        } else if (utils::find_match(utils::RE_INT, ifmt, sind, eind)) {
            isubtype = T_INT;
            iprecision = 8 * sizeof(int);
        }
            // Unsigned integers
        else if (utils::find_match(utils::RE_UCHAR, ifmt, sind, eind)) {
            isubtype = T_UINT;
            iprecision = 8 * sizeof(unsigned char);
        } else if (utils::find_match(utils::RE_USHORT, ifmt, sind, eind)) {
            isubtype = T_UINT;
            iprecision = 8 * sizeof(unsigned short);
        } else if (utils::find_match(utils::RE_ULONG_LONG, ifmt, sind, eind) ||
                   utils::find_match(utils::RE_ULONG_LONG2, ifmt, sind, eind)) {
            isubtype = T_UINT;
            iprecision = 8 * sizeof(unsigned long long);
        } else if (utils::find_match(utils::RE_ULONG, ifmt, sind, eind)) {
            isubtype = T_UINT;
            iprecision = 8 * sizeof(unsigned long);
        } else if (utils::find_match(utils::RE_UINT, ifmt, sind, eind)) {
            isubtype = T_UINT;
            iprecision = 8 * sizeof(unsigned int);
        } else {
            utils::ygglog_throw_error("create_dtype_format_class: Could not parse format string: " + ifmt);
        }
        ygglog_debug << "isubtype = " << isubtype << " iprecision = " << iprecision << " ifmt = " << ifmt;
        if (as_array) {
            addItem(createArray(isubtype, iprecision, 0, ""));
        } else {
            addItem(createValue(isubtype, iprecision, ""));
        }
        beg = end;
    }

    //for (size_t i = 0; i < items.size(); i++) {
    //    delete items[i];
    //    items[i] = NULL;
    //}
}

void FormattedData::display(const std::string &indent) const {
    printf("%s%-15s = %s\n", indent.c_str(), "type", "FORMATTED");
    printf("%s%-15s = %zu\n", indent.c_str(), "number", items.size());
    for (auto i : items) {
        printf("%s  %-15s\n", indent.c_str(), "Item");
        i->display(indent + "  ");
    }
}

} // communication
} // datatypes

dtype_t* create_dtype_format(const char* format_str, const int as_array, const bool use_generic) {
    auto dt = (dtype_t*)malloc(sizeof(dtype_t));
    communication::datatypes::FormattedData* obj = nullptr;
    try {
        obj = new communication::datatypes::FormattedData(format_str, as_array);
        dt->obj = obj;
        dt->type = T_FORMATTED;
        return dt;
    } catch(...) {
        ygglog_error << "create_dtype_format: C++ exception thrown.";
        CSafe(delete obj)
        return nullptr;
    }
}
