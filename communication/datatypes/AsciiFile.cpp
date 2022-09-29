#include "AsciiFile.hpp"
#include "AsciiFile.h"
#include "utils/tools.hpp"

#include <cstring>

namespace communication {
namespace datatypes {

AsciiFile::AsciiFile(const char* filepath, const char* io_mode,
                     const char *comment, const char *newline) {
    fd = nullptr;
    update(filepath, io_mode);
    // Set defaults for optional parameters
    if (comment == nullptr)
        strncpy(this->comment, "# ", 64);
    else
        strncpy(this->comment, comment, 64);
    if (newline == nullptr)
        strncpy(this->newline, "\n", 64);
    else
        strncpy(this->newline, newline, 64);
}

bool AsciiFile::is_open() const {
    return fd == nullptr;
}

bool AsciiFile::open() {
    if (!is_open()) {
        fd = fopen(filepath, io_mode);
        if (fd != nullptr)
            return true;
    } else {
        return true;
    }
    return false;
}

void AsciiFile::close() {
    if (is_open()) {
        fclose(fd);
        fd = nullptr;
    }
}

bool AsciiFile::is_comment(const char *line) const {
    if (strncmp(line, comment, strlen(comment)) == 0)
        return true;
    return false;
}

int AsciiFile::readline_full_norealloc(char *line, size_t n) {
    if (is_open()) {
        if (fgets(line, (int) n, fd) == nullptr) {
            return -1;
        }
        int nread = (int) strlen(line);
        if ((nread < ((int) n - 1)) || (line[nread - 1] == '\n') || (feof(fd)))
            return nread;
    }
    return -1;
}

int AsciiFile::readline_full(char **line, size_t *n) {
    if (is_open()) {
        return (int) getline(line, n, fd);
    }
    return -1;
}

int AsciiFile::writeline_full(const char *line) {
    if (is_open())
        return (int) fwrite(line, 1, strlen(line), fd);
    return -1;
}

void AsciiFile::update(const char* fpath, const char* io_m) {
    filepath = fpath;
    strncpy(this->io_mode, io_m, 64);
}
}
}

GET_ITEM(asciiFile,AsciiFile)
GET_ITEMC(asciiFile,AsciiFile)
GET_ITEMP(asciiFile,AsciiFile)

int af_is_open(const asciiFile_t &t) {
    auto x = get_asciiFile(t);
    if (x == nullptr)
        return -1;
    return x->is_open();
}

int af_open(asciiFile_t *t) {
    auto x = get_asciiFile(t);
    if (x == nullptr)
        return -1;
    return x->open();
}

int af_close(asciiFile_t *t) {
    auto x = get_asciiFile(t);
    if (x == nullptr)
        return -1;
    return 0;
}

int af_is_comment(const asciiFile_t &t, const char *line) {
    auto x = get_asciiFile(t);
    if (x == nullptr)
        return -1;
    return x->is_comment(line);
}
int af_readline_full_norealloc(const asciiFile_t &t, char *line, const size_t &n) {
    auto x = get_asciiFile(t);
    if (x == nullptr)
        return -1;
    return x->readline_full_norealloc(line, n);
}
int af_readline_full(const asciiFile_t &t, char **line, size_t *n) {
    auto x = get_asciiFile(t);
    if (x == nullptr)
        return -1;
    return x->readline_full(line, n);
}
int af_writeline_full(const asciiFile_t &t, const char *line) {
    auto x = get_asciiFile(t);
    if (x == nullptr)
        return -1;
    return x->writeline_full(line);
}

int af_update(asciiFile_t *t, const char *filepath, const char *io_mode) {
    auto x = get_asciiFile(t);
    if (x == nullptr)
        return -1;
    x->update(filepath, io_mode);
    return 0;
}
asciiFile_t init_asciiFile(const char *filepath, const char *io_mode,
                           const char *comment, const char *newline) {
    asciiFile_t af;
    auto ac = new communication::datatypes::AsciiFile(filepath, io_mode, comment, newline);
    af.obj = static_cast<void*>(ac);
    return af;
}
