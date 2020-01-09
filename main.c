#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "buf.h"

#define UNREACHABLE()                                                       \
    do {                                                                    \
        fprintf(stderr,                                                     \
                "%s:%d:Reached unreachable code in function %s. This is a " \
                "bug in the compiler.\n",                                   \
                __FILE__, __LINE__, __func__);                              \
        abort();                                                            \
    } while (0);

#define LOG(fmt, ...)                                              \
    do {                                                           \
        if (getenv("LOX_DEBUG")) {                                 \
            printf("%s:%d:" fmt, __func__, __LINE__, __VA_ARGS__); \
            puts("");                                              \
        }                                                          \
    } while (0)

#define BUF_LEN 100

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
    TOKEN_COUNT,
} TokenType;

typedef struct {
    size_t line;
    size_t column;
    const char* source;
    size_t source_len;
    TokenType type;
} Token;

typedef enum {
    PREC_NONE = 0,
    PREC_ASSIGNMENT,  // =
    PREC_OR,          // or
    PREC_AND,         // and
    PREC_EQUALITY,    // == !=
    PREC_COMPARISON,  // < > <= >=
    PREC_TERM,        // + -
    PREC_FACTOR,      // * /
    PREC_UNARY,       // ! -
    PREC_CALL,        // . ()
    PREC_PRIMARY
} Precedence;

typedef enum {
    VAL_BOOL,
    VAL_NIL,
    VAL_NUMBER,
} ValueType;

typedef struct {
    ValueType type;
    union {
        bool boolean;
        double number;
    } as;
} Value;

#define BOOL_VAL(value) ((Value){VAL_BOOL, {.boolean = value}})
#define NIL_VAL ((Value){VAL_NIL, {.number = 0}})
#define NUMBER_VAL(value) ((Value){VAL_NUMBER, {.number = value}})

#define AS_BOOL(value) ((value).as.boolean)
#define AS_NUMBER(value) ((value).as.number)

#define IS_BOOL(value) ((value).type == VAL_BOOL)
#define IS_NIL(value) ((value).type == VAL_NIL)
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)

static void value_print(FILE* out, Value v) {
    switch (v.type) {
        case VAL_BOOL:
            fprintf(out, "%s", v.as.boolean ? "true" : "false");
            break;
        case VAL_NIL:
            fprintf(out, "nil");
            break;
        case VAL_NUMBER:
            fprintf(out, "%f", v.as.number);
            break;
        default:
            UNREACHABLE();
    }
}

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
    OP_NIL = 7,
    OP_TRUE = 8,
    OP_FALSE = 9,
} OpCode;

typedef struct {
    uint8_t* opcodes;
    size_t* lines;
    Value* constants;
    size_t ip;
    Value stack[STACK_MAX];
    uint8_t stack_len;
} Chunk;

#define VM_ERROR(line, fmt, value)         \
    do {                                   \
        fprintf(stderr, "%zu:" fmt, line); \
        value_print(stderr, value);        \
        exit(EINVAL);                      \
    } while (0)

static void vm_stack_push(Chunk* chunk, Value v) {
    if (chunk->stack_len == (STACK_MAX - 1)) {
        fprintf(stderr, "%zu:Maximum stack size reached: %d\n",
                chunk->lines[chunk->ip], STACK_MAX);
        exit(ENOMEM);
    }
    chunk->stack_len += 1;
    chunk->stack[chunk->stack_len - 1] = v;
}

static Value vm_stack_pop(Chunk* chunk) {
    if (chunk->stack_len == 0) {
        fprintf(stderr, "%zu:Cannot pop from an empty stack\n",
                chunk->lines[chunk->ip]);
        exit(EINVAL);
    }

    const Value value = chunk->stack[chunk->stack_len - 1];
    chunk->stack[chunk->stack_len - 1] = (Value){0};
    chunk->stack_len -= 1;

    return value;
}

static void read_stdin(char** content, size_t* content_len) {
    char buf[BUF_LEN] = "";

    ssize_t effectivily_read = 0;
    while ((effectivily_read = read(0, buf, BUF_LEN)) > 0) {
        *content_len += effectivily_read;
        *content = realloc(*content, *content_len);

        if (*content == NULL) {
            fprintf(stderr, "Could not allocate while reading from stdin\n");
            exit(ENOMEM);
        }

        memcpy(*content + *content_len - effectivily_read, buf,
               effectivily_read);
    }
    if (effectivily_read == -1) {
        fprintf(stderr, "Error reading from stdin: errno=%s error=%d\n",
                strerror(errno), errno);
        exit(errno);
    }
    LOG("content_len=%zu", *content_len);
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

static void vm_dump(Chunk* chunk) {
    while (chunk->ip < buf_size(chunk->opcodes)) {
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
                if (!(chunk->ip < buf_size(chunk->opcodes))) {
                    fprintf(stderr,
                            "%zu:Malformed opcode: missing operand for "
                            "OP_CONSTANT\n",
                            line);
                    exit(EINVAL);
                }
                const uint8_t value_index = chunk->opcodes[chunk->ip];
                const Value value = chunk->constants[value_index];

                printf("%zu:OP_CONSTANT:", line);
                value_print(stdout, value);
                puts("");
                break;
            case OP_NIL:
                printf("OP_NIL\n");
                break;
            case OP_TRUE:
                printf("OP_TRUE\n");
                break;
            case OP_FALSE:
                printf("OP_FALSE\n");
                break;
            default:
                fprintf(stderr, "%zu:Unknown opcode %hhu\n", line, opcode);
                exit(EINVAL);
        }
        chunk->ip += 1;
    }
}

static void vm_run_bytecode(Chunk* chunk) {
    while (chunk->ip < buf_size(chunk->opcodes)) {
        const uint8_t opcode = chunk->opcodes[chunk->ip];
        const size_t line = chunk->lines[chunk->ip];

        switch (opcode) {
            case OP_RETURN: {
                const Value value = vm_stack_pop(chunk);
                printf("Stack size=%hhu top value=", chunk->stack_len);
                value_print(stdout, value);
                puts("");
                return;
            }
            case OP_NEGATE: {
                const Value value = vm_stack_pop(chunk);

                if (!IS_NUMBER(value))
                    VM_ERROR(line, "Expected a number, got:", value);

                vm_stack_push(chunk, NUMBER_VAL(-AS_NUMBER(value)));
                break;
            }
            case OP_ADD: {
                const Value rhs = vm_stack_pop(chunk);
                if (!IS_NUMBER(rhs))
                    VM_ERROR(line, "Expected a number, got:", rhs);

                const Value lhs = vm_stack_pop(chunk);
                if (!IS_NUMBER(lhs))
                    VM_ERROR(line, "Expected a number, got:", lhs);

                // TODO: Check for overflow
                vm_stack_push(chunk,
                              NUMBER_VAL(AS_NUMBER(lhs) + AS_NUMBER(rhs)));
                break;
            }
            case OP_SUBTRACT: {
                const Value rhs = vm_stack_pop(chunk);
                if (!IS_NUMBER(rhs))
                    VM_ERROR(line, "Expected a number, got:", rhs);

                const Value lhs = vm_stack_pop(chunk);
                if (!IS_NUMBER(lhs))
                    VM_ERROR(line, "Expected a number, got:", rhs);

                // TODO: Check for underflow
                vm_stack_push(chunk,
                              NUMBER_VAL(AS_NUMBER(lhs) - AS_NUMBER(rhs)));
                break;
            }
            case OP_MULTIPLY: {
                const Value rhs = vm_stack_pop(chunk);
                const Value lhs = vm_stack_pop(chunk);
                // TODO: Check for overflow
                vm_stack_push(chunk,
                              NUMBER_VAL(AS_NUMBER(lhs) * AS_NUMBER(rhs)));
                break;
            }
            case OP_DIVIDE: {
                const Value rhs = vm_stack_pop(chunk);
                const Value lhs = vm_stack_pop(chunk);
                // TODO: Check for 0
                vm_stack_push(chunk,
                              NUMBER_VAL(AS_NUMBER(lhs) / AS_NUMBER(rhs)));
                break;
            }
            case OP_CONSTANT:
                chunk->ip += 1;
                if (!(chunk->ip < buf_size(chunk->opcodes))) {
                    fprintf(stderr,
                            "%zu:Malformed opcode: missing operand for "
                            "OP_CONSTANT\n",
                            line);
                    exit(EINVAL);
                }
                const uint8_t value_index = chunk->opcodes[chunk->ip];
                const Value value = chunk->constants[value_index];
                vm_stack_push(chunk, value);
                break;
            case OP_NIL:
                vm_stack_push(chunk, NIL_VAL);
                break;
            case OP_TRUE:
                vm_stack_push(chunk, BOOL_VAL(true));
                break;
            case OP_FALSE:
                vm_stack_push(chunk, BOOL_VAL(false));
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
    LOG("type=%d line=%zu column=%zu source=%.*s source_len=%zu", type,
        token->line, token->column, (int)token->source_len, token->source,
        token->source_len);
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
    return lex->pos == lex->source_len;
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

    if (!lex_is_at_end(lex) && lex_peek(lex) == '.' &&
        isdigit(lex_peek_next(lex))) {
        // Consume the dot
        lex_advance(lex);

        while (!lex_is_at_end(lex) && isdigit(lex_peek(lex))) lex_advance(lex);
    }

    lex_init_token(lex, token, TOKEN_NUMBER, start_lex);
}

static bool str_eq(const char* a, size_t a_len, const char* b, size_t b_len) {
    if (!a || !b) return false;
    if (a_len != b_len) return false;

    return memcmp(a, b, a_len) == 0;
}

static TokenType lex_identifier_type(const char* s, size_t s_len) {
    assert(s_len >= 1);

    switch (s[0]) {
        case 'a':
            if (str_eq("nd", 2, s + 1, s_len - 1))
                return TOKEN_AND;
            else
                break;
        case 'c':
            if (str_eq("lass", 4, s + 1, s_len - 1))
                return TOKEN_CLASS;
            else
                break;
        case 'e':
            if (str_eq("lse", 3, s + 1, s_len - 1))
                return TOKEN_ELSE;
            else
                break;
        case 'f':
            if (str_eq("alse", 4, s + 1, s_len - 1))
                return TOKEN_FALSE;
            else if (str_eq("or", 2, s + 1, s_len - 1))
                return TOKEN_FOR;
            else if (str_eq("un", 2, s + 1, s_len - 1))
                return TOKEN_FUN;
            else
                break;
        case 'i':
            if (str_eq("f", 1, s + 1, s_len - 1))
                return TOKEN_IF;
            else
                break;
        case 'n':
            if (str_eq("il", 2, s + 1, s_len - 1))
                return TOKEN_NIL;
            else
                break;
        case 'o':
            if (str_eq("r", 1, s + 1, s_len - 1))
                return TOKEN_OR;
            else
                break;
        case 'p':
            if (str_eq("rint", 4, s + 1, s_len - 1))
                return TOKEN_PRINT;
            else
                break;
        case 'r':
            if (str_eq("eturn", 5, s + 1, s_len - 1))
                return TOKEN_RETURN;
            else
                break;
        case 's':
            if (str_eq("uper", 4, s + 1, s_len - 1))
                return TOKEN_SUPER;
            else
                break;
        case 't':
            if (str_eq("his", 3, s + 1, s_len - 1))
                return TOKEN_THIS;
            else if (str_eq("rue", 3, s + 1, s_len - 1))
                return TOKEN_TRUE;
            else
                break;
        case 'v':
            if (str_eq("ar", 2, s + 1, s_len - 1))
                return TOKEN_VAR;
            else
                break;
        case 'w':
            if (str_eq("hile", 4, s + 1, s_len - 1))
                return TOKEN_WHILE;
            else
                break;
        default:
            break;
    }

    return TOKEN_IDENTIFIER;
}

static void lex_identifier(Lex* lex, Token* token, const Lex* start_lex) {
    while (!lex_is_at_end(lex) && isalnum(lex_peek(lex))) lex_advance(lex);

    const char* s = &start_lex->source[start_lex->pos];
    const size_t s_len = lex->pos - start_lex->pos;

    lex_init_token(lex, token, lex_identifier_type(s, s_len), start_lex);
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
    } else if (isalnum(c)) {
        lex_identifier(lex, token, &start_lex);
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
        case ')':
            lex_init_token(lex, token, TOKEN_RIGHT_PAREN, &start_lex);
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

typedef enum {
    PARSER_STATE_OK,
    PARSER_STATE_ERROR,
    PARSER_STATE_PANIC_MODE,
} ParserState;

typedef struct {
    ParserState state;
    Lex lex;
    Token current;
    Token previous;
    Chunk* chunk;
} Parser;

typedef void (*ParseFn)(Parser*);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

static void parse_grouping(Parser*);
static void parse_unary(Parser*);
static void parse_binary(Parser*);
static void parse_number(Parser*);
static void parse_literal(Parser*);

static const ParseRule rules[TOKEN_COUNT] = {
    [TOKEN_LEFT_PAREN] = {.prefix = parse_grouping},
    [TOKEN_MINUS] = {.prefix = parse_unary,
                     .infix = parse_binary,
                     .precedence = PREC_TERM},
    [TOKEN_PLUS] = {.infix = parse_binary, .precedence = PREC_TERM},
    [TOKEN_SLASH] = {.infix = parse_binary, .precedence = PREC_FACTOR},
    [TOKEN_STAR] = {.infix = parse_binary, .precedence = PREC_FACTOR},
    [TOKEN_NUMBER] = {.prefix = parse_number},
    [TOKEN_NIL] = {.prefix = parse_literal},
    [TOKEN_TRUE] = {.prefix = parse_literal},
    [TOKEN_FALSE] = {.prefix = parse_literal},
};

static void parse_error(Parser* parser, const char* err, size_t err_len) {
    if (parser->state == PARSER_STATE_OK) {
        parser->state = PARSER_STATE_ERROR;
    } else if (parser->state == PARSER_STATE_ERROR) {
        parser->state = PARSER_STATE_PANIC_MODE;
        return;
    }

    fprintf(stderr, "%zu:%zu:%.*s\n", parser->current.line,
            parser->current.column, (int)err_len, err);
}

static void parse_emit_byte(Parser* parser, uint8_t byte) {
    LOG("byte=%d", byte);
    buf_push(parser->chunk->lines, parser->current.line);
    buf_push(parser->chunk->opcodes, byte);
}

static void parse_advance(Parser* parser) {
    parser->previous = parser->current;

    while (true) {
        lex_scan_token(&parser->lex, &parser->current);
        if (parser->current.type != TOKEN_ERROR) return;

        parse_error(parser, parser->current.source, parser->current.source_len);
    }
}

static void parse_precedence(Parser* parser, Precedence precedence) {
    parse_advance(parser);

    const ParseFn prefix_rule = rules[parser->previous.type].prefix;
    if (!prefix_rule) {
        parse_error(parser, "Expected expression", 19);
        return;
    }

    prefix_rule(parser);

    while (precedence <= rules[parser->current.type].precedence) {
        parse_advance(parser);

        const ParseFn infix_rule = rules[parser->previous.type].infix;

        infix_rule(parser);
    }
}

static void parse_expect(Parser* parser, TokenType type, const char* err,
                         size_t err_len) {
    if (parser->current.type == type) {
        parse_advance(parser);
        return;
    }

    parse_error(parser, err, err_len);
}

static void parse_number(Parser* parser) {
    assert(parser->previous.type = TOKEN_NUMBER);

    const double number = strtod(parser->previous.source, NULL);
    const Value v = NUMBER_VAL(number);

    parse_emit_byte(parser, OP_CONSTANT);
    buf_push(parser->chunk->constants, v);
    parse_emit_byte(parser, buf_size(parser->chunk->constants) - 1);
}

static void parse_literal(Parser* parser) {
    switch (parser->previous.type) {
        case TOKEN_NIL:
            parse_emit_byte(parser, OP_NIL);
            break;
        case TOKEN_TRUE:
            parse_emit_byte(parser, OP_TRUE);
            break;
        case TOKEN_FALSE:
            parse_emit_byte(parser, OP_FALSE);
            break;
        default:
            UNREACHABLE();
    }
}

static void parse_expression(Parser* parser);

static void parse_grouping(Parser* parser) {
    parse_expression(parser);
    parse_expect(parser, TOKEN_RIGHT_PAREN, "Expected `)` after expression",
                 12);
}

static void parse_unary(Parser* parser) {
    const TokenType previousType = parser->previous.type;

    parse_precedence(parser, PREC_UNARY);

    if (previousType != TOKEN_MINUS) UNREACHABLE();
    parse_emit_byte(parser, OP_NEGATE);
}

static void parse_binary(Parser* parser) {
    const TokenType previousType = parser->previous.type;

    const ParseRule* const rule = &rules[previousType];
    parse_precedence(parser, rule->precedence + 1);

    switch (previousType) {
        case TOKEN_PLUS:
            parse_emit_byte(parser, OP_ADD);
            break;
        case TOKEN_MINUS:
            parse_emit_byte(parser, OP_SUBTRACT);
            break;
        case TOKEN_STAR:
            parse_emit_byte(parser, OP_MULTIPLY);
            break;
        case TOKEN_SLASH:
            parse_emit_byte(parser, OP_DIVIDE);
            break;
        default:
            UNREACHABLE();
    }
}

static void parse_expression(Parser* parser) {
    parse_precedence(parser, PREC_ASSIGNMENT);
}

static void parse_compile(const char* source, size_t source_len, Chunk* chunk) {
    Parser parser = {.lex =
                         {
                             .source = source,
                             .source_len = source_len,
                             .line = 1,
                             .column = 1,
                             .pos = 0,
                         },
                     .chunk = chunk};

    parse_advance(&parser);
    parse_expression(&parser);
    parse_expect(&parser, TOKEN_EOF, "Expected EOF", 12);

    parse_emit_byte(&parser, OP_RETURN);
}

static void vm_interpret(const char* source, size_t source_len) {
    Chunk chunk = {0};
    parse_compile(source, source_len, &chunk);
    vm_run_bytecode(&chunk);
}

int main(int argc, char* argv[]) {
    if (argc != 3) {
        printf("Usage: %s vm_dump|run filename\n", argv[0]);
        return 0;
    }
    char* source = NULL;
    size_t source_len = 0;
    if (strcmp(argv[2], "-") == 0)
        read_stdin(&source, &source_len);
    else
        read_file(argv[2], &source, &source_len);

    if (strcmp(argv[1], "vm_dump") == 0)
        vm_dump(NULL);  // FIXME
    else if (strcmp(argv[1], "run") == 0)
        vm_interpret(source, source_len);
    else
        printf("Usage: %s vm_dump|run filename\n", argv[0]);
}
