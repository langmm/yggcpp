#pragma once
#include <cstdio>
#include <string>
#define LINE_SIZE_MAX 1024*2

namespace communication {
namespace datatypes {
class AsciiTable;

class AsciiFile {
public:
    AsciiFile() = delete;
    AsciiFile(const char *filepath, const char *io_mode,
              const char *comment, const char *newline);

    ~AsciiFile();

    bool is_open() const;

    bool open();

    void close();

    bool is_comment(const char *line) const;

    int readline_full_norealloc(char *line, size_t n);

    int readline_full(char **line, size_t *n);

    int writeline_full(const char *line);

    void update(const char *filepath, const char *io_mode);

private:
    friend AsciiTable;
    const char* filepath; //!< Full path to file.
    char io_mode[64]; //!< I/O mode. 'r' for read, 'w' for write.
    char comment[64]; //!< Character(s) indicating a comment.
    char newline[64]; //!< Character(s) indicating a newline.
    FILE *fd; //!< File identifier for ASCII file when open.
};

}
} // communication
