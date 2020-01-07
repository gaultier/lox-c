#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* #include "buf.h" */

typedef struct {
    const char* source;
    size_t source_len;
    size_t line;
    size_t column;
    size_t pos;
} Lex;

typedef enum {
    // Single-character tokens.
    TOKEN_LEFT_PAREN,
    TOKEN_RIGHT_PAREN,
    TOKEN_LEFT_BRACE,
    TOKEN_RIGHT_BRACE,
    TOKEN_COMMA,
    TOKEN_DOT,
    TOKEN_MINUS,
    TOKEN_PLUS,
    TOKEN_SEMICOLON,
    TOKEN_SLASH,
    TOKEN_STAR,

    // One or two character tokens.
    TOKEN_BANG,
    TOKEN_BANG_EQUAL,
    TOKEN_EQUAL,
    TOKEN_EQUAL_EQUAL,
    TOKEN_GREATER,
    TOKEN_GREATER_EQUAL,
    TOKEN_LESS,
    TOKEN_LESS_EQUAL,

    // Literals.
    TOKEN_IDENTIFIER,
    TOKEN_STRING,
    TOKEN_NUMBER,

    // Keywords.
    TOKEN_AND,
    TOKEN_CLASS,
    TOKEN_ELSE,
    TOKEN_FALSE,
    TOKEN_FOR,
    TOKEN_FUN,
    TOKEN_IF,
    TOKEN_NIL,
    TOKEN_OR,
    TOKEN_PRINT,
    TOKEN_RETURN,
    TOKEN_SUPER,
    TOKEN_THIS,
    TOKEN_TRUE,
    TOKEN_VAR,
    TOKEN_WHILE,

    TOKEN_ERROR,
    TOKEN_EOF
} TokenType;

typedef struct {
    size_t line;
    size_t column;
    const char* source;
    size_t source_len;
    TokenType type;
} Token;

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

static void interpret_dummy(Chunk* chunk, const uint8_t values[256]) {
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

static void lex_scan_token(Lex* lex, Token* token) {
    while (lex->pos < lex->source_len) {
        const char c = lex->source[lex->pos];
        switch (c) {
            case '{': {
                token->line = lex->line;
                token->column = lex->column;
                token->type = TOKEN_LEFT_BRACE;
                token->source = &lex->source[lex->pos];
                token->source_len = 1;

                lex->pos += 1;
                lex->column += 1;
                break;
            }
            default:
                fprintf(stderr, "%zu:%zu:Unknown token `%c`\n", lex->line,
                        lex->column, c);
                exit(1);
        }
    }
}

static void compile(const char* source, size_t source_len) {
    Lex lex = {
        .source = source,
        .source_len = source_len,
        .line = 1,
        .column = 1,
        .pos = 0,
    };

    Token token = {0};
    lex_scan_token(&lex, &token);

    printf("%zu:%zu:type=%d source=`", token.line, token.column, token.type);
    fwrite(token.source, 1, token.source_len, stdout);
    printf("`\n");
}

static void interpret(const char* source, size_t source_len) {
    compile(source, source_len);
}

int main(int argc, char* argv[]) {
    if (argc != 3) {
        printf("Usage: %s dump|run filename\n", argv[0]);
        return 0;
    }
    char* source = NULL;
    size_t source_len = 0;
    read_file(argv[2], &source, &source_len);

    /*if (strcmp(argv[1], "dump") == 0)
        dump(&chunk, values);
    else */
    if (strcmp(argv[1], "run") == 0)
        interpret(source, source_len);
    else
        printf("Usage: %s dump|run filename\n", argv[0]);
}
