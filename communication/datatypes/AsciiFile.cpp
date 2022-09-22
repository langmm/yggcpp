#include "AsciiFile.hpp"
#include <cstring>

using namespace communication;
using namespace communication::datatypes;
asciiFile_t::asciiFile_t(const char *filepath, const char *io_mode,
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
bool asciiFile_t::is_open() const{
    return fd == nullptr;
}

bool asciiFile_t::open(){
    if (!is_open()) {
        fd = fopen(filepath, io_mode);
        if (fd != nullptr)
            return true;
    } else {
        return true;
    }
    return false;
}

void asciiFile_t::close() {
    if (is_open()) {
        fclose(fd);
        fd = nullptr;
    }
}

bool asciiFile_t::is_comment(const char *line) const {
    if (strncmp(line, comment, strlen(comment)) == 0)
        return true;
    return false;
}

int asciiFile_t::readline_full_norealloc(char *line, size_t n) {
    if (is_open()) {
        if (fgets(line, (int)n, fd) == nullptr) {
            return -1;
        }
        int nread = (int)strlen(line);
        if ((nread < ((int)n - 1)) || (line[nread - 1] == '\n') || (feof(fd)))
            return nread;
    }
    return -1;
}

int asciiFile_t::readline_full(char** line, size_t *n){
    if (is_open()) {
        return (int)getline(line, n, fd);
    }
    return -1;
}

int asciiFile_t::writeline_full(const char* line){
    if (is_open())
        return (int)fwrite(line, 1, strlen(line), fd);
    return -1;
}

void asciiFile_t::update(const char* fpath, const char* io_m){
    this->filepath = fpath;
    strncpy(this->io_mode, io_m, 64);
}

