#include "AsciiTable.hpp"
#include "AsciiTable.h"
#include "utils/regex.hpp"
#include <cstring>
#include <memory>

#define FMT_LEN 100

namespace communication {
namespace datatypes {

int regex_replace_sub(char *buf, const size_t &len_buf,
                      const char *re, const char *rp,
                      const size_t &nreplace);

int simplify_formats(char *fmt_str, const size_t fmt_len) {
    const char * fmt_regex1 = "%([[:digit:]]+\\$)?[+-]?([ 0]|\'.{1})?-?[[:digit:]]*(\\.[[:digit:]]+)?([lhjztL]*)([eEfFgG])";
    // "%([[:digit:]]+\\$)?[+-]?([ 0]|\'.{1})?-?[[:digit:]]*(\\.[[:digit:]]+)?([lhjztL]*)([eEfFgG])";
    // "%([[:digit:]]+\\$)?[+-]?([ 0]|'.{1})?-?[[:digit:]]*(\\.[[:digit:]]+)?([lhjztL])*([eEfFgG])";
    int ret = regex_replace_sub(fmt_str, fmt_len, fmt_regex1,
                                "%$4$5", 0);
    if (ret > 0) {
        const char * fmt_regex2 = "%[lhjztL]*([fF])";
        ret = regex_replace_sub(fmt_str, fmt_len, fmt_regex2,
                                "%l$1", 0);
    }
/*#ifdef _WIN32
  if (ret > 0) {
    const char * fmt_regex3 = "%l64([du])";
    ret = regex_replace_sub(fmt_str, fmt_len, fmt_regex3, "%l$1", 0);
  }
#endif*/
    return ret;
}

AsciiTable::AsciiTable(const char *filepath, const char *io_mode,
                           const char *format_str, const char *comment,
                           const char *column, const char *newline) :
                           f(AsciiFile(filepath, io_mode, comment, newline)),
                           row_siz(0), status(0), expected_cols(0){
    columns.clear();
    strncpy(this->format_str, "\0", LINE_SIZE_MAX);
    f = AsciiFile(filepath, io_mode, comment, newline);
    // Set defaults for optional parameters
    if (column == nullptr)
        strncpy(this->column, "\t", 64);
    else
        strncpy(this->column, column, 64);
    // Guess format string from file
    if (format_str == nullptr) {
        if (strcmp(io_mode, "r") == 0) {
            status = discover_format_str();
        } else {
            status = -1;
        }
    } else {
        strncpy(this->format_str, format_str, LINE_SIZE_MAX);
    }
    // Get number of columns & types
    if (status >= 0)
        status = set_ncols();
    if (status >= 0)
        status = set_format_typ();
    /* printf("status = %d\n", t.status); */
    /* printf("format_str = %s\n", t.format_str); */
    /* printf("ncols = %d, row_siz = %d\n", t.ncols, t.row_siz); */
}

AsciiTable::~AsciiTable(){
    cleanup();
}

int AsciiTable::open(){
    return f.open();
}

void AsciiTable::close(){
    f.close();
}

int AsciiTable::readline_full_realloc(char** buf, const size_t &len_buf, const bool allow_realloc){
    // Read lines until there's one that's not a comment
    int ret = 0;
    bool com = true;
    size_t nread = LINE_SIZE_MAX;
    char *line = (char*)malloc(nread);
    if (line == nullptr) {
        utils::ygglog_error("at_readline_full_realloc: Failed to malloc line.");
        return -1;
    }
    while (com) {
        ret = f.readline_full(&line, &nread);
        if (ret < 0) {
            free(line);
            return ret;
        }
        com = f.is_comment(line);
    }
    if (ret > (int)len_buf) {
        if (allow_realloc) {
            utils::ygglog_debug("at_readline_full_realloc: reallocating buffer from %d to %d bytes.",
                         (int)len_buf, ret + 1);
            char *temp_buf = (char*)realloc(*buf, ret + 1);
            if (temp_buf == nullptr) {
                utils:: ygglog_error("at_readline_full_realloc: Failed to realloc buffer.");
                free(*buf);
                free(line);
                return -1;
            }
            *buf = temp_buf;
        } else {
            utils::ygglog_error("at_readline_full_realloc: line (%d bytes) is larger than destination buffer (%d bytes)",
                         ret, (int)len_buf);
            ret = -1;
            free(line);
            return ret;
        }
    }
    strncpy(*buf, line, len_buf);
    free(line);
    return ret;
}

int AsciiTable::readline_full(char* buf, const size_t &len_buf){
    return readline_full_realloc(&buf, len_buf, false);
}

int AsciiTable::writeline_full(const char* line){
    return f.writeline_full(line);
}

int AsciiTable::bytes_to_row(const char* line, va_list &ap){
    char fmt[LINE_SIZE_MAX];
    strncpy(fmt, format_str, LINE_SIZE_MAX);
    int sret = simplify_formats(fmt, LINE_SIZE_MAX);
    if (sret < 0) {
        utils::ygglog_debug("at_vbytes_to_row: simplify_formats returned %d", sret);
        return -1;
    }
    // Interpret line
    int ret = vsscanf(line, fmt, ap);
    if (ret != columns.size()) {
        utils::ygglog_error("at_vbytes_to_row: %d arguments filled, but %d were expected",
                     sret, columns.size());
        ret = -1;
    }
    return ret;
}

int AsciiTable::row_to_bytes(char *buf, const size_t &buf_siz, va_list &ap){
    return vsnprintf(buf, buf_siz, format_str, ap);
}

int AsciiTable::readline(va_list &ap){
    int ret;
    // Read lines until there's one that's not a comment
    size_t nread = LINE_SIZE_MAX;
    char *line = (char*)malloc(nread);
    if (line == nullptr) {
        utils::ygglog_error("at_vreadline: Failed to malloc line.");
        return -1;
    }

    if ((ret = readline_full(line, nread)) < 0) {
        free(line);
        return ret;
    }
    // Parse line

    if (bytes_to_row(line, ap) < 0)
        ret = -1;
    free(line);
    return ret;
}

int AsciiTable::writeline(va_list &ap){
    return vfprintf(f.fd, format_str, ap);
}

int AsciiTable::readline(AsciiTable* t, ...){
    va_list ap;
    va_start(ap, t); // might need to use last element in structure
    int ret = readline(ap);
    va_end(ap);
    return ret;
}

int AsciiTable::writeline(AsciiTable* t, ...){
    va_list ap;
    va_start(ap, t);
    int ret = writeline(ap);
    va_end(ap);
    return ret;
}

int AsciiTable::writeformat(){
    int ret;
    if (f.is_open()) {
        ret = (int)fwrite(f.comment, 1, strlen(f.comment), f.fd);
        if (ret < 0)
            return ret;
    }
    ret = f.writeline_full(format_str);
    return ret;
}

int AsciiTable::discover_format_str(){
    int ret = open();
    if (ret < 0)
        return ret;
    size_t nread = LINE_SIZE_MAX;
    char *line = (char*)malloc(nread);
    if (line == nullptr) {
        utils::ygglog_error("at_discover_format_str: Failed to malloc line.");
        return -1;
    }
    ret = -1;
    while (getline(&line, &nread, f.fd) >= 0) {
        if (f.is_comment(line)) {
            if (count_formats(line) > 0) {
                strncpy(format_str, line + strlen(f.comment), LINE_SIZE_MAX);
                ret = 0;
                break;
            }
        }
    }
    close();
    free(line);
    return ret;
}

int AsciiTable::set_ncols() {
    // Assumes that format_str already done
    expected_cols = count_formats(format_str);
    return expected_cols;
}

int AsciiTable::set_format_siz(){
    /* (*t).format_siz = (int*)malloc((*t).ncols*sizeof(int)); */
    int i = 0, typ, siz;
    row_siz = 0;
    for (auto &c : columns) {
        typ = c.type;
        switch(c.type) {
            case AT_STRING:
                siz = static_cast<int>(c.size);
                break; // TODO
            case AT_FLOAT:
                siz = sizeof(float);
                break;
            case AT_DOUBLE:
                siz = sizeof(double);
                break;
            case AT_COMPLEX:
                siz = 2 * sizeof(double);
                break;
            case AT_SHORTSHORT:
                siz = sizeof(char);
                break;
            case AT_SHORT:
                siz = sizeof(short);
                break;
            case AT_LONGLONG:
                siz = sizeof(long long);
                break;
            case AT_LONG:
                siz = sizeof(long);
                break;
            case AT_INT:
                siz = sizeof(int);
                break;
            case AT_USHORTSHORT:
                siz = sizeof(unsigned char);
                break;
            case AT_USHORT:
                siz = sizeof(unsigned short);
                break;
            case AT_ULONGLONG:
                siz = sizeof(unsigned long long);
                break;
            case AT_ULONG:
                siz = sizeof(unsigned long);
                break;
            case AT_UINT:
                siz = sizeof(unsigned int);
                break;
            default:
                siz = -1;
                break;
        }
        if (siz < 0) {
            utils::ygglog_error("at_set_format_siz: Could not set size for column %d with type %d", i, typ);
            return -1;
        }
        i++;
        c.size = siz;
        row_siz += siz;
        // printf("format_str = %s\n", t->format_str);
        // printf("col %d/%d siz = %d\n", i, (*t).ncols, siz);
    }
    return 0;
}

int AsciiTable::set_format_typ(){
    columns.clear();
    columns.resize(expected_cols);
    size_t beg = 0, end;
    int icol;
    char ifmt[FMT_LEN];

    // Loop over string
    icol = 0;
    int mres;
    size_t sind, eind;
    char re_fmt[FMT_LEN];
    sprintf(re_fmt, "%%[^%s%s]+[%s%s]",
            column, f.newline, column, f.newline);
    while (beg < strlen(format_str)) {
        mres = utils::find_match(re_fmt, format_str + beg, &sind, &eind);
        if (mres < 0) {
            utils::ygglog_error("at_set_format_typ: find_match returned %d", mres);
            return -1;
        } else if (mres == 0) {
            beg++;
            continue;
        }
        beg += sind;
        end = beg + (eind - sind);
        strncpy(ifmt, &(format_str)[beg], end-beg);
        ifmt[end-beg] = '\0';
        if (utils::find_match("%.*s", ifmt, &sind, &eind)) {
            columns[icol].type = AT_STRING;
            mres = regex_replace_sub(ifmt, FMT_LEN,
                                     "%(\\.)?([[:digit:]]*)s(.*)", "$2", 0);
            columns[icol].size = atoi(ifmt);
#ifdef _WIN32
            } else if (find_match("(%.*[fFeEgG]){2}j", ifmt, &sind, &eind)) {
#else
        } else if (utils::find_match("(\%.*[fFeEgG]){2}j", ifmt, &sind, &eind)) {
#endif
            /* columns[icol].type = AT_COMPLEX; */
            columns[icol].type = AT_DOUBLE;
            icol++;
            columns[icol].type = AT_DOUBLE;
        } else if (utils::find_match("%.*[fFeEgG]", ifmt, &sind, &eind)) {
            columns[icol].type = AT_DOUBLE;
            /* } else if (find_match("%.*l[fFeEgG]", ifmt, &sind, &eind)) { */
            /*   columns[icol].type = AT_DOUBLE; */
            /* } else if (find_match("%.*[fFeEgG]", ifmt, &sind, &eind)) { */
            /*   columns[icol].type = AT_FLOAT; */
        } else if (utils::find_match("%.*hh[id]", ifmt, &sind, &eind)) {
            columns[icol].type = AT_SHORTSHORT;
        } else if (utils::find_match("%.*h[id]", ifmt, &sind, &eind)) {
            columns[icol].type = AT_SHORT;
        } else if (utils::find_match("%.*ll[id]", ifmt, &sind, &eind)) {
            columns[icol].type = AT_LONGLONG;
        } else if (utils::find_match("%.*l64[id]", ifmt, &sind, &eind)) {
            columns[icol].type = AT_LONGLONG;
        } else if (utils::find_match("%.*l[id]", ifmt, &sind, &eind)) {
            columns[icol].type = AT_LONG;
        } else if (utils::find_match("%.*[id]", ifmt, &sind, &eind)) {
            columns[icol].type = AT_INT;
        } else if (utils::find_match("%.*hh[uoxX]", ifmt, &sind, &eind)) {
            columns[icol].type = AT_USHORTSHORT;
        } else if (utils::find_match("%.*h[uoxX]", ifmt, &sind, &eind)) {
            columns[icol].type = AT_USHORT;
        } else if (utils::find_match("%.*ll[uoxX]", ifmt, &sind, &eind)) {
            columns[icol].type = AT_ULONGLONG;
        } else if (utils::find_match("%.*l64[uoxX]", ifmt, &sind, &eind)) {
            columns[icol].type = AT_ULONGLONG;
        } else if (utils::find_match("%.*l[uoxX]", ifmt, &sind, &eind)) {
            columns[icol].type = AT_ULONG;
        } else if (utils::find_match("%.*[uoxX]", ifmt, &sind, &eind)) {
            columns[icol].type = AT_UINT;
        } else {
            utils::ygglog_error("at_set_format_typ: Could not parse format string: %s", ifmt);
            return -1;
        }
        beg = end;
        icol++;
    }
    return set_format_siz();
}

int AsciiTable::bytes_to_array(char* data, const size_t &data_siz, va_list &ap){
    // check size of array
    /* size_t data_siz = strlen(data); */
    if ((data_siz % row_siz) != 0) {
        utils::ygglog_error("at_vbytes_to_array: Data: %s", data);
        utils::ygglog_error("at_vbytes_to_array: Data size (%d) not an even number of rows (row size is %d)",
                     (int)data_siz, row_siz);
        return -1;
    }
    // Loop through
    size_t nrows = data_siz / row_siz;
    size_t cur_pos = 0, col_siz;
    //int i;
    //for (i = 0; i < t.ncols; i++) {
    for (auto c : columns) {
        char **temp;
        char *t2;
        temp = va_arg(ap, char**);
        col_siz = nrows * c.size;
        t2 = (char*)realloc(*temp, col_siz);
        if (t2 == nullptr) {
            utils::ygglog_error("at_vbytes_to_array: Failed to realloc temp var.");
            free(*temp);
            return -1;
        }
        *temp = t2;
        // C order memory
        /* for (int j = 0; j < nrows; j++) { */
        /*   memcpy(*temp + j*t.format_siz[i], data + j*t.row_siz + cur_pos, t.format_siz[i]); */
        /* } */
        /* cur_pos += t.format_siz[i]; */
        // F order memory
        memcpy(*temp, data+cur_pos, col_siz);
        cur_pos += col_siz;
        /* printf("col %d: cur_pos = %d, col_siz = %d, data = %s, raw_data = ", i, cur_pos, col_siz, *temp); */
        /* fwrite(*temp, col_siz, 1, stdout); */
        /* printf("\n"); */
    }
    return (int)nrows;
}

int AsciiTable::array_to_bytes(char* data, const size_t &data_siz, va_list &ap){
    int nrows = va_arg(ap, int);
    size_t msg_siz = nrows * row_siz;
    if (msg_siz > data_siz) {
        utils::ygglog_debug("at_varray_to_bytes: Message size (%d bytes) will exceed allocated buffer (%d bytes).",
                     msg_siz, (int)data_siz);
        return (int)msg_siz;
    }
    // Loop through
    int cur_pos = 0, col_siz;
    char *temp;
    //int i;
    for (auto c: columns) {
    //for (i = 0; i < t.ncols; i++) {
        col_siz = nrows * (int)c.size;
        temp = va_arg(ap, char*);
        memcpy(data + cur_pos, temp, col_siz);
        cur_pos += col_siz;
    }
    return cur_pos;
}

int AsciiTable::bytes_to_array(char* data, size_t data_siz, ...){
    va_list ap;
    va_start(ap, data_siz);
    int ret = bytes_to_array(data, data_siz, ap);
    va_end(ap);
    return ret;
}

int AsciiTable::array_to_bytes(char* data, size_t data_siz, ...){
    va_list ap;
    va_start(ap, data_siz);
    int ret = array_to_bytes(data, data_siz, ap);
    va_end(ap);
    return ret;
}

void AsciiTable::cleanup(){
    columns.clear();
    row_siz = 0;
}

int AsciiTable::update(const char* filepath, const char* io_mode){
    f.update(filepath, io_mode);
    int flag = 0;
    if ((strlen(format_str) == 0) && (strcmp(io_mode, "r") == 0))
        if ((flag = discover_format_str()) >= 0)
            if ((flag = set_ncols()) >= 0)
                flag = set_format_typ();

    status = flag;
    return flag;
}

int count_formats(const char* fmt_str) {
    const char * fmt_regex = "%([[:digit:]]+\\$)?[+-]?([ 0]|\'.{1})?-?[[:digit:]]*(\\.[[:digit:]]+)?[lhjztL]*(64)?[bcdeEufFgGosxX]";
    int ret = utils::count_matches(fmt_regex, fmt_str);
    /* printf("%d, %s\n", ret, fmt_str); */
    return ret;
}

}
}

GET_ITEM(asciiTable,AsciiTable)
GET_ITEMC(asciiTable,AsciiTable)
GET_ITEMP(asciiTable,AsciiTable)

int at_open(asciiTable_t *t) {
    auto x = get_asciiTable(t);
    if (x == nullptr)
        return -1;
    return x->open();
}

void at_close(asciiTable_t *t) {
    auto x = get_asciiTable(t);
    if (x == nullptr)
        return;
    x->close();
}

int at_readline_full_realloc(const asciiTable_t &t, char **buf,
                             const size_t &len_buf, const int &allow_realloc) {
    auto x = get_asciiTable(t);
    if (x == nullptr)
        return -1;
    return x->readline_full_realloc(buf, len_buf, allow_realloc);
}

int at_readline_full(const asciiTable_t &t, char *buf, const size_t &len_buf) {
    auto x = get_asciiTable(t);
    if (x == nullptr)
        return -1;
    return x->readline_full(buf, len_buf);
}

int at_writeline_full(const asciiTable_t &t, const char* line) {
    auto x = get_asciiTable(t);
    if (x == nullptr)
        return -1;
    return x->writeline_full(line);
}

int at_vbytes_to_row(const asciiTable_t &t, const char* line, va_list &ap) {
    auto x = get_asciiTable(t);
    if (x == nullptr)
        return -1;
    return x->bytes_to_row(line, ap);
}

int at_vrow_to_bytes(const asciiTable_t &t, char *buf, const size_t &buf_siz, va_list &ap) {
    auto x = get_asciiTable(t);
    if (x == nullptr)
        return -1;
    return x->row_to_bytes(buf, buf_siz, ap);
}

int at_vreadline(const asciiTable_t &t, va_list &ap) {
    auto x = get_asciiTable(t);
    if (x == nullptr)
        return -1;
    return x->readline(ap);
}

int at_vwriteline(const asciiTable_t &t, va_list &ap) {
    auto x = get_asciiTable(t);
    if (x == nullptr)
        return -1;
    return x->writeline(ap);
}

int at_readline(asciiTable_t t, ...) {
    auto x = get_asciiTable(t);
    if (x == nullptr)
        return -1;
    va_list ap;
    va_start(ap, t);
    int ret = x->readline(ap);
    va_end(ap);
    return ret;
}

int at_writeline(asciiTable_t t, ...) {
    auto x = get_asciiTable(t);
    if (x == nullptr)
        return -1;
    va_list ap;
    va_start(ap, t);
    int ret = x->writeline(ap);
    va_end(ap);
    return ret;
}

int at_writeformat(const asciiTable_t &t) {
    auto x = get_asciiTable(t);
    if (x == nullptr)
        return -1;
    return x->writeformat();
}

int at_discover_format_str(asciiTable_t *t) {
    auto x = get_asciiTable(t);
    if (x == nullptr)
        return -1;
    return x->discover_format_str();
}

int at_set_ncols(asciiTable_t *t) {
    auto x = get_asciiTable(t);
    if (x == nullptr)
        return -1;
    return x->set_ncols();
}

int at_set_format_siz(asciiTable_t *t) {
    auto x = get_asciiTable(t);
    if (x == nullptr)
        return -1;
    return x->set_format_siz();
}

int at_set_format_typ(asciiTable_t *t) {
    auto x = get_asciiTable(t);
    if (x == nullptr)
        return -1;
    return x->set_format_typ();
}

int at_vbytes_to_array(const asciiTable_t &t, char *data,
                       const size_t &data_siz, va_list &ap) {
    auto x = get_asciiTable(t);
    if (x == nullptr)
        return -1;
    return x->bytes_to_array(data, data_siz, ap);
}

int at_varray_to_bytes(const asciiTable_t &t, char *data, const size_t &data_siz, va_list &ap) {
    auto x = get_asciiTable(t);
    if (x == nullptr)
        return -1;
    return x->array_to_bytes(data, data_siz, ap);
}

int at_bytes_to_array(const asciiTable_t &t, char *data, size_t data_siz, ...) {
    auto x = get_asciiTable(t);
    if (x == nullptr)
        return -1;
    va_list ap;
    va_start(ap, data_siz);
    int ret = x->bytes_to_array(data, data_siz, ap);
    va_end(ap);
    return ret;
}

int at_array_to_bytes(const asciiTable_t &t, char *data, size_t data_siz, ...) {
    auto x = get_asciiTable(t);
    if (x == nullptr)
        return -1;
    va_list ap;
    va_start(ap, data_siz);
    int ret = x->array_to_bytes(data, data_siz, ap);
    va_end(ap);
    return ret;
}

void at_cleanup(asciiTable_t *t) {
    auto x = get_asciiTable(t);
    if (x == nullptr)
        return;
    x->cleanup();
    delete x;
    t->obj = nullptr;
}

int at_update(asciiTable_t *t, const char *filepath, const char *io_mode) {
    auto x = get_asciiTable(t);
    if (x == nullptr)
        return -1;
    return x->update(filepath, io_mode);
}

asciiTable_t init_asciiTable(const char *filepath, const char *io_mode,
                        const char *format_str, const char *comment,
                        const char *column, const char *newline) {
    asciiTable_t at;
    auto aft = new communication::datatypes::AsciiTable(filepath, io_mode, format_str,
                                                        comment, column, newline);
    at.obj = static_cast<void*>(aft);
    return at;
}

