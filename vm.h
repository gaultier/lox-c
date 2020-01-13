#pragma once

#include <ctype.h>

#include "hashtab.h"
#include "result.h"
#include "value.h"

#define RETURN_IF_ERR(e)             \
    do {                             \
        const Result _e = e;         \
        if (_e != RES_OK) return _e; \
    } while (0);

#define VALUES_MAX 256

#define STACK_MAX 256

typedef enum {
    OP_RETURN = 0,
    OP_CONSTANT,
    OP_NEGATE,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_NOT,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_PRINT,
    OP_POP,
    OP_DEFINE_GLOBAL,
    OP_GET_GLOBAL,
    OP_COUNT,
} OpCode;

static const char opcode_str[OP_COUNT][17] = {
    [OP_RETURN] = "OP_RETURN",
    [OP_CONSTANT] = "OP_CONSTANT",
    [OP_NEGATE] = "OP_NEGATE",
    [OP_ADD] = "OP_ADD",
    [OP_SUBTRACT] = "OP_SUBTRACT",
    [OP_MULTIPLY] = "OP_MULTIPLY",
    [OP_DIVIDE] = "OP_DIVIDE",
    [OP_NIL] = "OP_NIL",
    [OP_TRUE] = "OP_TRUE",
    [OP_FALSE] = "OP_FALSE",
    [OP_NOT] = "OP_NOT",
    [OP_EQUAL] = "OP_EQUAL",
    [OP_GREATER] = "OP_GREATER",
    [OP_LESS] = "OP_LESS",
    [OP_PRINT] = "OP_PRINT",
    [OP_POP] = "OP_POP",
    [OP_DEFINE_GLOBAL] = "OP_DEFINE_GLOBAL",
    [OP_GET_GLOBAL] = "OP_GET_GLOBAL",
};

typedef struct {
    uint8_t* opcodes;
    size_t* lines;
    Value* constants;
} Chunk;

typedef struct {
    size_t ip;
    Value stack[STACK_MAX];
    uint8_t stack_len;
    Obj* objects;
    hashtab_t* globals;
} Vm;

void vm_repl();
Result vm_dump(Vm* vm, Chunk* chunk);
Result vm_interpret(const char* source, size_t source_len,
                    Result (*bytecode_fn)(Vm*, Chunk*));
Result vm_run_bytecode(Vm* vm, Chunk* chunk);
