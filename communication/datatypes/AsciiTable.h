#pragma once
#include <cstdarg>
#include "AsciiFile.h"

#define FMT_LEN 100

#ifdef __cplusplus
extern "C" {
#endif

/*! @brief Enumerated types to be used for interpreting formats. */
enum fmt_type { AT_STRING, AT_FLOAT, AT_DOUBLE, AT_COMPLEX,
                 AT_SHORTSHORT, AT_SHORT, AT_INT, AT_LONG, AT_LONGLONG,
                 AT_USHORTSHORT, AT_USHORT, AT_UINT, AT_ULONG, AT_ULONGLONG };

struct asciiTable {
    void* obj;
};

typedef struct asciiTable asciiTable_t;

int count_complex_formats(const char* fmt_str);
int count_formats(const char* fmt_str);
int simplify_formats(char *fmt_str, const size_t &fmt_len);
int at_open(asciiTable_t *t);
void at_close(asciiTable_t *t);
int at_readline_full_realloc(const asciiTable_t &t, char **buf,
                             const size_t &len_buf, const int &allow_realloc);
int at_readline_full(const asciiTable_t &t, char *buf, const size_t &len_buf);
int at_writeline_full(const asciiTable_t &t, const char* line);
int at_vbytes_to_row(const asciiTable_t &t, const char* line, va_list &ap);
int at_vrow_to_bytes(const asciiTable_t &t, char *buf, const size_t &buf_siz, va_list &ap);
int at_vreadline(const asciiTable_t &t, va_list &ap);
int at_vwriteline(const asciiTable_t &t, va_list &ap);
int at_readline(asciiTable_t t, ...);
int at_writeline(asciiTable_t t, ...);
int at_writeformat(const asciiTable_t &t);
int at_discover_format_str(asciiTable_t *t);
int at_set_ncols(asciiTable_t *t);
int at_set_format_siz(asciiTable_t *t);
int at_set_format_typ(asciiTable_t *t);
int at_vbytes_to_array(const asciiTable_t &t, char *data,
                       const size_t &data_siz, va_list &ap);
int at_varray_to_bytes(const asciiTable_t &t, char *data, const size_t &data_siz, va_list &ap);
int at_bytes_to_array(const asciiTable_t &t, char *data, size_t data_siz, ...);
int at_array_to_bytes(const asciiTable_t &t, char *data, size_t data_siz, ...);
void at_cleanup(asciiTable_t *t);
int at_update(asciiTable_t *t, const char *filepath, const char *io_mode);
asciiTable_t init_asciiTable(const char *filepath, const char *io_mode,
                             const char *format_str, const char *comment,
                             const char *column, const char *newline);

#ifdef __cplusplus
}
#endif