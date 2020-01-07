#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* #include "buf.h" */

typedef double Value;

#define VALUES_MAX 256

#define STACK_MAX 256

typedef enum {
    OP_RETURN = 0,
    OP_CONSTANT = 1,
    OP_NEGATE = 2,
    OP_ADD = 3,
    OP_SUBTRACT = 4,
    OP_MULTIPLY = 5,
    OP_DIVIDE = 6,
} OpCode;

typedef struct {
    const uint8_t* opcodes;
    size_t opcodes_len;
    const size_t* lines;
    size_t lines_len;
    size_t ip;
    Value stack[STACK_MAX];
    uint8_t stack_len;
} Chunk;

static void stack_push(Chunk* chunk, Value v) {
    if (chunk->stack_len == (STACK_MAX - 1)) {
        fprintf(stderr, "%zu:Maximum stack size reached: %d\n",
                chunk->lines[chunk->ip], STACK_MAX);
        exit(ENOMEM);
    }
    chunk->stack_len += 1;
    chunk->stack[chunk->stack_len - 1] = v;
}

static Value stack_pop(Chunk* chunk) {
    if (chunk->stack_len == 0) {
        fprintf(stderr, "%zu:Cannot pop from an empty stack\n",
                chunk->lines[chunk->ip]);
        exit(EINVAL);
    }

    const Value value = chunk->stack[chunk->stack_len - 1];
    chunk->stack[chunk->stack_len - 1] = 0xaa;
    chunk->stack_len -= 1;

    return value;
}

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
            case OP_NEGATE:
                printf("%zu:OP_NEGATE\n", line);
                break;
            case OP_ADD:
                printf("%zu:OP_ADD\n", line);
                break;
            case OP_SUBTRACT:
                printf("%zu:OP_SUBTRACT\n", line);
                break;
            case OP_MULTIPLY:
                printf("%zu:OP_MULTIPLY\n", line);
                break;
            case OP_DIVIDE:
                printf("%zu:OP_DIVIDE\n", line);
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
            case OP_RETURN: {
                const Value value = stack_pop(chunk);
                printf("Stack size=%hhu top value=%f\n", chunk->stack_len,
                       value);
                return;
            }
            case OP_NEGATE: {
                const Value value = stack_pop(chunk);
                stack_push(chunk, -value);
                break;
            }
            case OP_ADD: {
                const Value rhs = stack_pop(chunk);
                const Value lhs = stack_pop(chunk);
                // TODO: Check for overflow
                stack_push(chunk, lhs + rhs);
                break;
            }
            case OP_SUBTRACT: {
                const Value rhs = stack_pop(chunk);
                const Value lhs = stack_pop(chunk);
                // TODO: Check for underflow
                stack_push(chunk, lhs - rhs);
                break;
            }
            case OP_MULTIPLY: {
                const Value rhs = stack_pop(chunk);
                const Value lhs = stack_pop(chunk);
                // TODO: Check for overflow
                stack_push(chunk, lhs * rhs);
                break;
            }
            case OP_DIVIDE: {
                const Value rhs = stack_pop(chunk);
                const Value lhs = stack_pop(chunk);
                // TODO: Check for 0
                stack_push(chunk, lhs / rhs);
                break;
            }
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
                stack_push(chunk, value);
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

    const uint8_t opcodes[] = {OP_CONSTANT, 0,      OP_NEGATE, OP_CONSTANT,
                               1,           OP_ADD, OP_RETURN};
    const size_t lines[] = {1, 2, 3, 4, 5, 6, 7};

    Chunk chunk = {.opcodes = opcodes,
                   .opcodes_len = sizeof(opcodes) / sizeof(opcodes[0]),
                   .lines = lines,
                   .lines_len = sizeof(lines) / sizeof(lines[0])};
    uint8_t values[VALUES_MAX] = {0xaa};
    values[0] = 42;
    values[1] = 2;

    if (strcmp(argv[1], "dump") == 0)
        dump(&chunk, values);
    else if (strcmp(argv[1], "run") == 0)
        interpret(&chunk, values);
    else
        printf("Usage: %s dump|run filename\n", argv[0]);
}
