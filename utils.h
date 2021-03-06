#pragma once
#include <stdbool.h>
#include <stddef.h>

#include "config.h"

#define UNREACHABLE()                                                       \
    do {                                                                    \
        fprintf(stderr,                                                     \
                "%s:%d:Reached unreachable code in function %s. This is a " \
                "bug in the compiler.\n",                                   \
                __FILE__, __LINE__, __func__);                              \
        abort();                                                            \
    } while (0)

#ifdef WITH_LOGS
#define LOG(fmt, ...)                                          \
    do {                                                       \
        printf("%s:%d:" fmt, __func__, __LINE__, __VA_ARGS__); \
    } while (0)
#else
#ifndef WITH_LOGS
#define LOG(fmt, ...) \
    do {              \
    } while (0)
#endif
#endif

#define REALLOC_SAFE(ptr, new_size) \
    realloc_safe((void*)ptr, new_size, __func__, __LINE__)

#define BUF_LEN 100

void* realloc_safe(void** ptr, size_t new_size, const char* func, int line);
void read_file(const char path[], char** content, size_t* content_len);
void read_stdin(char** content, size_t* content_len);
bool str_eq(const char* restrict a, size_t a_len, const char* restrict b,
            size_t b_len);
