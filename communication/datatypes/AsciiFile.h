#pragma once

#include <cstdlib>

#ifdef __cplusplus
extern "C" {
#endif

struct asciiFile {
    void* obj;
};
typedef struct asciiFile asciiFile_t;

int af_is_open(const asciiFile_t &t);
int af_open(asciiFile_t *t);
int af_close(asciiFile_t *t);
int af_is_comment(const asciiFile_t &t, const char *line);
int af_readline_full_norealloc(const asciiFile_t &t, char *line, const size_t &n);
int af_readline_full(const asciiFile_t &t, char **line, size_t *n);
int af_writeline_full(const asciiFile_t &t, const char *line);
int af_update(asciiFile_t *t, const char *filepath, const char *io_mode);
asciiFile_t init_asciiFile(const char *filepath, const char *io_mode,
                           const char *comment, const char *newline);



#ifdef __cplusplus
}
#endif