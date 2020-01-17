#pragma once

#include <stdint.h>

#include "config.h"
#include "hashtab.h"
#include "result.h"
#include "value.h"

#define RETURN_IF_ERR(e)             \
    do {                             \
        const Result _e = e;         \
        if (_e != RES_OK) return _e; \
    } while (0);

#define VALUES_MAX (UINT8_MAX + 1)

#define STACK_MAX (UINT8_MAX + 1)

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
    OP_SET_GLOBAL,
    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_JUMP_IF_FALSE,
    OP_JUMP,
    OP_LOOP,
    OP_COUNT,
} OpCode;

extern const char opcode_str[OP_COUNT][17];

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

void vm_repl(void);
Result vm_dump(Vm* vm, Chunk* chunk);
Result vm_interpret(char* source, ssize_t source_len,
                    Result (*bytecode_fn)(Vm*, Chunk*));
Result vm_run_bytecode(Vm* vm, Chunk* chunk);
