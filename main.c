#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* #include "buf.h" */

typedef double Value;

#define VALUES_MAX 256

typedef enum {
    OP_RETURN = 0,
    OP_CONSTANT = 1,
} OpCode;

typedef struct {
    const uint8_t* opcodes;
    size_t opcodes_len;
    const size_t* lines;
    size_t lines_len;
} Chunk;

static void read_file(const char path[], char** content, size_t* content_len) {
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
    const long file_size = ftell(file);

    rewind(file);

    *content = calloc(file_size + 1, 1);
    if (*content == NULL) {
        fprintf(stderr, "Could not allocate file content: errno=%d error=%s\n",
                errno, strerror(errno));
        exit(errno);
    }

    const size_t bytes_read = fread(*content, 1, file_size, file);
    *content_len = bytes_read;

    fclose(file);
}
static void dump(const Chunk* chunk, const uint8_t values[256]) {
    size_t i = 0;
    while (i < chunk->opcodes_len) {
        const uint8_t opcode = chunk->opcodes[i];
        const size_t line = chunk->lines[i];

        switch (opcode) {
            case OP_RETURN:
                printf("%zu:OP_RETURN\n", line);
                break;
            case OP_CONSTANT:
                i += 1;
                if (!(i < chunk->opcodes_len)) {
                    fprintf(stderr,
                            "%zu:Malformed opcode: missing operand for "
                            "OP_CONSTANT\n",
                            line);
                    exit(EINVAL);
                }
                const uint8_t value_index = chunk->opcodes[i];
                const Value value = values[value_index];
                printf("%zu:OP_CONSTANT: %f\n", line, value);
                break;
            default:
                fprintf(stderr, "%zu:Unknown opcode %hhu\n", line, opcode);
                exit(EINVAL);
        }
        i += 1;
    }
}

static void interpret(const Chunk* chunk, const uint8_t values[256]) {
    size_t i = 0;
    while (i < chunk->opcodes_len) {
        const uint8_t opcode = chunk->opcodes[i];
        const size_t line = chunk->lines[i];

        switch (opcode) {
            case OP_RETURN:
                break;
            case OP_CONSTANT:
                i += 1;
                if (!(i < chunk->opcodes_len)) {
                    fprintf(stderr,
                            "%zu:Malformed opcode: missing operand for "
                            "OP_CONSTANT\n",
                            line);
                    exit(EINVAL);
                }
                const uint8_t value_index = chunk->opcodes[i];
                const Value value = values[value_index];
                printf("OP_CONSTANT: %f\n", value);
                break;
            default:
                fprintf(stderr, "%zu:Unknown opcode %d\n", line, opcode);
                exit(EINVAL);
        }
        i += 1;
    }
}

int main(int argc, char* argv[]) {
    char* content = NULL;
    size_t content_len = 0;
    read_file(argv[1], &content, &content_len);

    printf("%s", content);
    const uint8_t opcodes[] = {OP_CONSTANT, 0, OP_RETURN};
    const size_t lines[] = {0, 1, 2};

    const Chunk chunk = {
        .opcodes = opcodes, .opcodes_len = 3, .lines = lines, .lines_len = 3};
    uint8_t values[VALUES_MAX] = {0};
    values[0] = 42;
    interpret(&chunk, values);
}
