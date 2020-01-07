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
    size_t ip;
} Chunk;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

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

static void dump(Chunk* chunk, const uint8_t values[256]) {
    while (chunk->ip < chunk->opcodes_len) {
        const uint8_t opcode = chunk->opcodes[chunk->ip];
        const size_t line = chunk->lines[chunk->ip];

        switch (opcode) {
            case OP_RETURN:
                printf("%zu:OP_RETURN\n", line);
                break;
            case OP_CONSTANT:
                chunk->ip += 1;
                if (!(chunk->ip < chunk->opcodes_len)) {
                    fprintf(stderr,
                            "%zu:Malformed opcode: missing operand for "
                            "OP_CONSTANT\n",
                            line);
                    exit(EINVAL);
                }
                const uint8_t value_index = chunk->opcodes[chunk->ip];
                const Value value = values[value_index];
                printf("%zu:OP_CONSTANT: %f\n", line, value);
                break;
            default:
                fprintf(stderr, "%zu:Unknown opcode %hhu\n", line, opcode);
                exit(EINVAL);
        }
        chunk->ip += 1;
    }
}

static void interpret(Chunk* chunk, const uint8_t values[256]) {
    while (chunk->ip < chunk->opcodes_len) {
        const uint8_t opcode = chunk->opcodes[chunk->ip];
        const size_t line = chunk->lines[chunk->ip];

        switch (opcode) {
            case OP_RETURN:
                break;
            case OP_CONSTANT:
                chunk->ip += 1;
                if (!(chunk->ip < chunk->opcodes_len)) {
                    fprintf(stderr,
                            "%zu:Malformed opcode: missing operand for "
                            "OP_CONSTANT\n",
                            line);
                    exit(EINVAL);
                }
                const uint8_t value_index = chunk->opcodes[chunk->ip];
                const Value value = values[value_index];
                printf("OP_CONSTANT: %f\n", value);
                break;
            default:
                fprintf(stderr, "%zu:Unknown opcode %d\n", line, opcode);
                exit(EINVAL);
        }
        chunk->ip += 1;
    }
}

int main(int argc, char* argv[]) {
    if (argc != 3) {
        printf("Usage: %s dump|run filename\n", argv[0]);
        return 0;
    }
    char* content = NULL;
    size_t content_len = 0;
    read_file(argv[2], &content, &content_len);

    const uint8_t opcodes[] = {OP_CONSTANT, 0, OP_RETURN};
    const size_t lines[] = {0, 1, 2};

    Chunk chunk = {
        .opcodes = opcodes, .opcodes_len = 3, .lines = lines, .lines_len = 3};
    uint8_t values[VALUES_MAX] = {0};
    values[0] = 42;

    if (strcmp(argv[1], "dump") == 0)
        dump(&chunk, values);
    else if (strcmp(argv[1], "run") == 0)
        interpret(&chunk, values);
    else
        printf("Usage: %s dump|run filename\n", argv[0]);
}
