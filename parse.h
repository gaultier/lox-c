#pragma once

#include "config.h"
#include "lex.h"
#include "result.h"
#include "value.h"
#include "vm.h"

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
    PREC_PRIMARY,
    PREC_COUNT,
} Precedence;

extern const char precedence_str[PREC_COUNT][16];

#define LOCALS_MAX (UINT8_MAX + 1)

typedef struct {
    Token name;
    intmax_t depth;
} Local;

typedef struct {
    Local locals[LOCALS_MAX];
    int locals_len;
    intmax_t scope_depth;
} Compiler;

typedef enum {
    PARSER_STATE_OK = 0,
    PARSER_STATE_ERROR,
    PARSER_STATE_PANIC_MODE,
    PARSER_STATE_SYNCED,
} ParserState;

typedef struct {
    ParserState state;
    Lex lex;
    Token current;
    Token previous;
    Chunk* chunk;
    Compiler* compiler;
} Parser;

Result parse_compile(const char* source, size_t source_len, Chunk* chunk,
                     Vm* vm);
