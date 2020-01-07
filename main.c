#include <ctype.h>
#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
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
    TOKEN_LEFT_PAREN = 1,
    TOKEN_RIGHT_PAREN = 2,
    TOKEN_LEFT_BRACE = 3,
    TOKEN_RIGHT_BRACE = 4,
    TOKEN_COMMA = 5,
    TOKEN_DOT = 6,
    TOKEN_MINUS = 7,
    TOKEN_PLUS = 8,
    TOKEN_SEMICOLON = 9,
    TOKEN_SLASH = 10,
    TOKEN_STAR = 11,

    // One or two character tokens.
    TOKEN_BANG = 12,
    TOKEN_BANG_EQUAL = 13,
    TOKEN_EQUAL = 14,
    TOKEN_EQUAL_EQUAL = 15,
    TOKEN_GREATER = 16,
    TOKEN_GREATER_EQUAL = 17,
    TOKEN_LESS = 18,
    TOKEN_LESS_EQUAL = 19,

    // Literals.
    TOKEN_IDENTIFIER = 20,
    TOKEN_STRING = 21,
    TOKEN_NUMBER = 22,

    // Keywords.
    TOKEN_AND = 23,
    TOKEN_CLASS = 24,
    TOKEN_ELSE = 25,
    TOKEN_FALSE = 26,
    TOKEN_FOR = 27,
    TOKEN_FUN = 28,
    TOKEN_IF = 29,
    TOKEN_NIL = 30,
    TOKEN_OR = 31,
    TOKEN_PRINT = 32,
    TOKEN_RETURN = 33,
    TOKEN_SUPER = 34,
    TOKEN_THIS = 35,
    TOKEN_TRUE = 36,
    TOKEN_VAR = 37,
    TOKEN_WHILE = 38,

    TOKEN_ERROR = 39,
    TOKEN_EOF = 40,
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
    const size_t file_size = (size_t)ftell(file);

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

static void lex_init_token(const Lex* lex, Token* token, TokenType type,
                           const Lex* start_lex) {
    token->line = start_lex->line;
    token->column = start_lex->column;
    token->type = type;
    token->source = &lex->source[start_lex->pos];
    token->source_len = lex->pos - start_lex->pos;
}

static void lex_init_token_err(const Lex* lex, Token* token, const char err[]) {
    token->line = lex->line;
    token->column = lex->column;
    token->type = TOKEN_ERROR;
    token->source = err;
    token->source_len = strlen(err);
}

static char lex_advance(Lex* lex) {
    lex->pos += 1;
    lex->column += 1;
    return lex->source[lex->pos - 1];
}

static char lex_peek(const Lex* lex) { return lex->source[lex->pos]; }

static bool lex_is_at_end(const Lex* lex) {
    return lex->pos == lex->source_len - 1;
}

static char lex_peek_next(const Lex* lex) {
    return lex_is_at_end(lex) ? '\0' : lex->source[lex->pos + 1];
}

static void lex_skip_until_char(Lex* lex, char c) {
    while (!lex_is_at_end(lex) && lex_peek(lex) != c) lex_advance(lex);
}

static bool lex_match(Lex* lex, char c) {
    if (lex_is_at_end(lex)) return false;

    if (lex->source[lex->pos] != c) return false;

    lex_advance(lex);
    return true;
}

static void lex_newline(Lex* lex) {
    lex->pos += 1;
    lex->column = 1;
    lex->line += 1;
}

static void lex_skip_whitespace(Lex* lex) {
    while (!lex_is_at_end(lex)) {
        const char c = lex_peek(lex);
        switch (c) {
            case ' ':
            case '\t':
            case '\r':
                lex_advance(lex);
                break;
            case '\n':
                lex_newline(lex);
                break;
            case '/':
                if (lex_peek_next(lex) == '/') lex_skip_until_char(lex, '\n');
                break;
            default:
                return;
        }
    }
}

static void lex_string(Lex* lex, Token* token) {
    const Lex start_lex = *lex;
    while (!lex_is_at_end(lex) && lex_peek(lex) != '"') {
        if (lex_peek(lex) == '\n')
            lex_newline(lex);
        else
            lex_advance(lex);
    }

    if (lex_is_at_end(lex)) {
        lex_init_token_err(lex, token, "Unterminated string");
        return;
    }
    lex_init_token(lex, token, TOKEN_STRING, &start_lex);

    // Consume closing quote
    lex_advance(lex);
}

static void lex_number(Lex* lex, Token* token, const Lex* start_lex) {
    while (!lex_is_at_end(lex) && isdigit(lex_peek(lex))) lex_advance(lex);

    lex_init_token(lex, token, TOKEN_NUMBER, start_lex);
}

static void lex_scan_token(Lex* lex, Token* token) {
    lex_skip_whitespace(lex);
    const Lex start_lex = *lex;

    if (lex_is_at_end(lex)) {
        lex_init_token(lex, token, TOKEN_EOF, &start_lex);
        return;
    }

    const char c = lex_advance(lex);

    if (isdigit(c)) {
        lex_number(lex, token, &start_lex);
        return;
    }

    switch (c) {
        case '{':
            lex_init_token(lex, token, TOKEN_LEFT_BRACE, &start_lex);
            return;
        case '}':
            lex_init_token(lex, token, TOKEN_RIGHT_BRACE, &start_lex);
            return;
        case '(':
            lex_init_token(lex, token, TOKEN_LEFT_PAREN, &start_lex);
            return;
        case ';':
            lex_init_token(lex, token, TOKEN_SEMICOLON, &start_lex);
            return;
        case ',':
            lex_init_token(lex, token, TOKEN_COMMA, &start_lex);
            return;
        case '.':
            lex_init_token(lex, token, TOKEN_DOT, &start_lex);
            return;
        case '-':
            lex_init_token(lex, token, TOKEN_MINUS, &start_lex);
            return;
        case '+':
            lex_init_token(lex, token, TOKEN_PLUS, &start_lex);
            return;
        case '*':
            lex_init_token(lex, token, TOKEN_STAR, &start_lex);
            return;
        case '/':
            lex_init_token(lex, token, TOKEN_SLASH, &start_lex);
            return;
        case '!':
            lex_init_token(lex, token,
                           lex_match(lex, '=') ? TOKEN_BANG_EQUAL : TOKEN_BANG,
                           &start_lex);
            return;
        case '=':
            lex_init_token(
                lex, token,
                lex_match(lex, '=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL,
                &start_lex);
            return;
        case '<':
            lex_init_token(lex, token,
                           lex_match(lex, '=') ? TOKEN_LESS_EQUAL : TOKEN_LESS,
                           &start_lex);
            return;
        case '>':
            lex_init_token(
                lex, token,
                lex_match(lex, '=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER,
                &start_lex);
            return;
        case '"':
            lex_string(lex, token);
            return;
        default: {
            char* err = NULL;
            asprintf(&err, "Unknown token `%c`", c);
            lex_init_token_err(lex, token, err);
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

    while (true) {
        Token token = {0};
        lex_scan_token(&lex, &token);

        printf("%zu:%zu:type=%d source=`", token.line, token.column,
               token.type);
        fwrite(token.source, 1, token.source_len, stdout);
        printf("`\n");
        if (token.type == TOKEN_EOF) return;
    }
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
