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

#ifdef WITH_LOGS
static const char precedence_str[PREC_COUNT][16] = {
    [PREC_NONE] = "PREC_NONE",
    [PREC_ASSIGNMENT] = "PREC_ASSIGNMENT",
    [PREC_OR] = "PREC_OR",
    [PREC_AND] = "PREC_AND",
    [PREC_EQUALITY] = "PREC_EQUALITY",
    [PREC_COMPARISON] = "PREC_COMPARISON",
    [PREC_TERM] = "PREC_TERM",
    [PREC_FACTOR] = "PREC_FACTOR",
    [PREC_UNARY] = "PREC_UNARY",
    [PREC_CALL] = "PREC_CALL",
    [PREC_PRIMARY] = "PREC_PRIMARY",
};
#endif
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
} Parser;

Result parse_compile(const char* source, size_t source_len, Chunk* chunk,
                     Vm* vm);
