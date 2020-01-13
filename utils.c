#include "utils.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void realloc_safe(void** ptr, size_t new_size, const char* func, int line) {
    if ((*ptr = realloc(*ptr, new_size)) == NULL) {
        fprintf(stderr, "%s:%d: Could not allocate %zu bytes of memory\n", func,
                line, new_size);
        exit(ENOMEM);
    }
    LOG("func=%s line=%d allocated=%zu\n", func, line, new_size);
}

void read_stdin(char** content, size_t* content_len) {
    char buf[BUF_LEN] = "";

    ssize_t effectivily_read = 0;
    while ((effectivily_read = read(0, buf, BUF_LEN)) > 0) {
        *content_len += effectivily_read;
        REALLOC_SAFE(content, *content_len);

        memcpy(*content + *content_len - effectivily_read, buf,
               effectivily_read);
    }
    if (effectivily_read == -1) {
        fprintf(stderr, "Error reading from stdin: errno=%s error=%d\n",
                strerror(errno), errno);
        exit(errno);
    }
    LOG("content_len=%zu\n", *content_len);
}

void read_file(const char path[], char** content, size_t* content_len) {
    FILE* file = NULL;

    if ((file = fopen(path, "r")) == NULL) {
        fprintf(stderr, "Could not open the file `%s`: errno=%d error=%s\n",
                path, errno, strerror(errno));
        exit(errno);
    }

    int ret = 0;
    if ((ret = fseek(file, 0, SEEK_END)) != 0) {
        fprintf(stderr,
                "Could not move the file cursor to the end of the file `%s`: "
                "errno=%d error=%s\n",
                path, errno, strerror(errno));
        exit(errno);
    }
    const size_t file_size = (size_t)ftell(file);

    rewind(file);

    REALLOC_SAFE(content, file_size + 1);
    (*content)[file_size] = '\0';

    const size_t bytes_read = fread(*content, 1, file_size, file);
    if (bytes_read != file_size) {
        fprintf(stderr,
                "Could not read whole file: bytes_read=%zu file_size=%zu\n",
                bytes_read, file_size);
        exit(EIO);
    }
    *content_len = bytes_read;

    fclose(file);
}

bool str_eq(const char* a, size_t a_len, const char* b, size_t b_len) {
    if (!a || !b) return false;
    if (a_len != b_len) return false;

    return memcmp(a, b, a_len) == 0;
}
