#pragma once

#include <stdint.h>

#include "config.h"
#include "hashtab.h"
#include "result.h"
#include "value.h"

#define VALUES_MAX (UINT8_MAX + 1)

#define FRAMES_MAX 64

#define STACK_MAX (FRAMES_MAX * (UINT8_MAX + 1))

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
    OP_CALL,
    OP_COUNT,
} OpCode;

extern const char opcode_str[256][17];

typedef struct {
    ObjFunction* fn;
    uint8_t* ip;
    Value* slots;
} CallFrame;

typedef struct {
    Value stack[STACK_MAX];
    Value* stack_top;  // Points to one element past the end
    Obj* objects;
    hashtab_t* globals;

    CallFrame frames[FRAMES_MAX];
    int8_t frame_len;
} Vm;

void vm_repl(void);
Result vm_dump(Vm* vm);
Result vm_interpret(char* source, size_t source_len,
                    Result (*bytecode_fn)(Vm*));
Result vm_run_bytecode(Vm* vm);
