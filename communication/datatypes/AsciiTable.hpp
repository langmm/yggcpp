#pragma once

#include <vector>
#include <cstdio>
#include "AsciiFile.hpp"
#include "utils/tools.hpp"

namespace communication {
namespace communicator {
class AsciiTableComm;
}

namespace datatypes {

namespace Metaschema {
class AsciiTableMetaschemaType;
}

/*! @brief Enumerated types to be used for interpreting formats. */
enum fmt_type {
    AT_STRING, AT_FLOAT, AT_DOUBLE, AT_COMPLEX,
    AT_SHORTSHORT, AT_SHORT, AT_INT, AT_LONG, AT_LONGLONG,
    AT_USHORTSHORT, AT_USHORT, AT_UINT, AT_ULONG, AT_ULONGLONG
};

struct columnDesc {
    fmt_type type;
    size_t size;
};

class asciiTable_t {
public:
    asciiTable_t(const char *filepath, const char *io_mode,
                 const char *format_str, const char *comment,
                 const char *column, const char *newline);

    ~asciiTable_t();

    int open();

    void close();

    int readline_full_realloc(char **buf, const size_t &len_buf, bool allow_realloc);

    int readline_full(char *buf, const size_t &len_buf);

    int writeline_full(const char *line);

    int vbytes_to_row(const char *line, va_list &ap);

    int vrow_to_bytes(char *buf, const size_t &buf_siz, va_list &ap);

    int vreadline(va_list &ap);

    int vwriteline(va_list &ap);

    int readline(asciiTable_t* t, ...);

    int writeline(asciiTable_t* t, ...);

    int writeformat();

    int discover_format_str();

    int set_ncols();

    int set_format_siz();

    int set_format_typ();

    int vbytes_to_array(const char *data, const size_t &data_siz, va_list &ap);

    int varray_to_bytes(char *data, const size_t &data_siz, va_list &ap);

    int bytes_to_array(char *data, size_t data_siz, ...);

    int array_to_bytes(char *data, const size_t data_siz, ...);

    void cleanup();

    size_t ncols() {return columns.size();}
    int update(const char *filepath, const char *io_mode);

private:
    friend communicator::AsciiTableComm;
    friend Metaschema::AsciiTableMetaschemaType;
    asciiFile_t f; //!< ASCII file structure.
    char format_str[LINE_SIZE_MAX]; //!< Format string for rows.
    char column[64]; //!< Character(s) used to seperate columns.
    std::vector<columnDesc> columns;
    //int *format_typ; //!< Array of ncols integers specifying column types.
    //int *format_siz; //!< Array of ncols sizes for elements in each column.
    int row_siz; //!< Size of an entire row in bytes.
    int status; //!< Negative if format_str has not been set yet
    int expected_cols;
};
int count_formats(const char* fmt_str);
}
} // communication
