#define _POSIX_C_SOURCE 200809L

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <signal.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "buf.h"
#include "hashtab.h"
#include "lex.h"
#include "utils.h"

#define RETURN_IF_ERR(e)             \
    do {                             \
        const Result _e = e;         \
        if (_e != RES_OK) return _e; \
    } while (0);

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

#ifndef NDEBUG
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
    VAL_BOOL,
    VAL_NIL,
    VAL_NUMBER,
    VAL_OBJ,
} ValueType;

typedef enum {
    OBJ_STRING,
} ObjType;

struct Obj {
    ObjType type;
    struct Obj* next;
};

typedef struct Obj Obj;

#define OBJ_TYPE(value) (AS_OBJ(value)->type)

typedef struct {
    ValueType type;
    union {
        bool boolean;
        double number;
        Obj* obj;
    } as;
} Value;

typedef struct {
    Obj obj;
    size_t len;
    char s[];
} ObjString;

#define BOOL_VAL(value) ((Value){VAL_BOOL, {.boolean = value}})
#define NIL_VAL ((Value){VAL_NIL, {.number = 0}})
#define NUMBER_VAL(value) ((Value){VAL_NUMBER, {.number = value}})
#define OBJ_VAL(object) ((Value){VAL_OBJ, {.obj = (Obj*)object}})

#define AS_BOOL(value) ((value).as.boolean)
#define AS_NUMBER(value) ((value).as.number)
#define AS_OBJ(value) ((value).as.obj)
#define AS_STRING(value) ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString*)AS_OBJ(value))->s)

#define IS_BOOL(value) ((value).type == VAL_BOOL)
#define IS_NIL(value) ((value).type == VAL_NIL)
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)
#define IS_OBJ(value) ((value).type == VAL_OBJ)
#define IS_STRING(value) value_obj_is_type(value, OBJ_STRING)

static bool value_obj_is_type(Value v, ObjType type) {
    return IS_OBJ(v) && AS_OBJ(v)->type == type;
}

#define VALUES_MAX 256

#define STACK_MAX 256

typedef enum {
    RES_OK = 0,
    RES_PARSE_ERR,
    RES_RUN_ERR,
} Result;

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
        case VAL_OBJ:
            switch (AS_OBJ(v)->type) {
                case OBJ_STRING:
                    fprintf(out, "%.*s", (int)AS_STRING(v)->len, AS_CSTRING(v));
                    break;
                default:
                    UNREACHABLE();
            }

            break;
        default:
            UNREACHABLE();
    }
}

static void value_obj_free(Vm* vm) {
    Obj* obj = vm->objects;
    while (obj) {
        Obj* next = obj->next;
        free(obj);
        obj = next;
    }
}

static bool value_is_falsy(Value v) {
    return IS_NIL(v) || (IS_BOOL(v) && !AS_BOOL(v));
}

static bool value_eq(Value lhs, Value rhs) {
    if (lhs.type != rhs.type) return false;

    switch (lhs.type) {
        case VAL_BOOL:
            return AS_BOOL(lhs) == AS_BOOL(rhs);
        case VAL_NIL:
            return true;
        case VAL_NUMBER:
            return AS_NUMBER(lhs) == AS_NUMBER(rhs);
        case VAL_OBJ:
            if (AS_STRING(lhs)->len != AS_STRING(rhs)->len) return false;
            return memcmp(AS_STRING(lhs)->s, AS_STRING(rhs)->s,
                          AS_STRING(lhs)->len) == 0;

        default:
            UNREACHABLE();
    }
}

static ObjString* vm_obj_str_allocate(Vm* vm, size_t size) {
    ObjString* obj = NULL;
    REALLOC_SAFE(&obj, size);
    obj->obj.next = vm->objects;

    vm->objects = &obj->obj;

    return obj;
}

static ObjString* vm_make_string(Vm* vm, size_t s_len) {
    ObjString* os = vm_obj_str_allocate(vm, sizeof(ObjString) + s_len);
    os->len = s_len;
    LOG("allocated string size=%zu\n", os->len);
    os->obj.type = OBJ_STRING;

    return os;
}

static void vm_str_cat(Vm* vm, Value lhs, Value rhs, Value* res) {
    assert(IS_STRING(lhs));
    assert(IS_STRING(rhs));

    const char* const lhs_s = AS_CSTRING(lhs);
    const size_t lhs_len = AS_STRING(lhs)->len;
    const char* const rhs_s = AS_CSTRING(rhs);
    const size_t rhs_len = AS_STRING(rhs)->len;

    ObjString* const os = vm_make_string(vm, lhs_len + rhs_len);

    memcpy(os->s, lhs_s, lhs_len);
    memcpy(os->s + lhs_len, rhs_s, rhs_len);

    *res = OBJ_VAL(os);
}

#define VM_ERROR(line, fmt, value)         \
    do {                                   \
        fprintf(stderr, "%zu:" fmt, line); \
        value_print(stderr, value);        \
        return RES_RUN_ERR;                \
    } while (0)

static Result vm_stack_push(Vm* vm, Chunk* chunk, Value v) {
    if (vm->stack_len == (STACK_MAX - 1)) {
        fprintf(stderr, "%zu:Maximum stack size reached: %d\n",
                chunk->lines[vm->ip], STACK_MAX);
        return RES_RUN_ERR;
    }
    vm->stack_len += 1;
    vm->stack[vm->stack_len - 1] = v;

    return RES_OK;
}

static Result vm_stack_pop(Vm* vm, Chunk* chunk, Value* v) {
    if (vm->stack_len == 0) {
        fprintf(stderr, "%zu:Cannot pop from an empty stack\n",
                chunk->lines[vm->ip]);
        return RES_RUN_ERR;
    }

    *v = vm->stack[vm->stack_len - 1];
    vm->stack[vm->stack_len - 1] = (Value){0};
    vm->stack_len -= 1;

    return RES_OK;
}

static Result vm_read_constant_in_next_byte(Vm* vm, Chunk* chunk, Value* v) {
    const uint8_t opcode = chunk->opcodes[vm->ip];
    const size_t line = chunk->lines[vm->ip];

    vm->ip += 1;

    if (!(vm->ip < buf_size(chunk->opcodes))) {
        fprintf(stderr, "%zu:Malformed opcode: missing operand for %s\n", line,
                opcode_str[opcode]);
        return RES_RUN_ERR;
    }
    const uint8_t value_index = chunk->opcodes[vm->ip];
    *v = chunk->constants[value_index];
    LOG("constant index=%d\n", value_index);

    return RES_OK;
}

static Result vm_dump_opcode_1_operand(Vm* vm, Chunk* chunk) {
    Value value = {0};
    RETURN_IF_ERR(vm_read_constant_in_next_byte(vm, chunk, &value));

    const uint8_t opcode = chunk->opcodes[vm->ip];
    const size_t line = chunk->lines[vm->ip];
    printf("%zu:%s:", line, opcode_str[opcode]);
    value_print(stdout, value);
    puts("");

    return RES_OK;
}

static Result vm_dump(Vm* vm, Chunk* chunk) {
    while (vm->ip < buf_size(chunk->opcodes)) {
        const uint8_t opcode = chunk->opcodes[vm->ip];
        const size_t line = chunk->lines[vm->ip];

        switch (opcode) {
            case OP_RETURN:
            case OP_NEGATE:
            case OP_ADD:
            case OP_SUBTRACT:
            case OP_MULTIPLY:
            case OP_DIVIDE:
            case OP_NIL:
            case OP_TRUE:
            case OP_FALSE:
            case OP_NOT:
            case OP_EQUAL:
            case OP_LESS:
            case OP_GREATER:
            case OP_PRINT:
            case OP_POP:
                printf("%zu:%s\n", line, opcode_str[opcode]);
                break;
            case OP_CONSTANT:
            case OP_DEFINE_GLOBAL:
            case OP_GET_GLOBAL:
                RETURN_IF_ERR(vm_dump_opcode_1_operand(vm, chunk));
                break;
            default:
                fprintf(stderr, "%zu:Unknown opcode %hhu\n", line, opcode);
                return RES_RUN_ERR;
        }
        vm->ip += 1;
    }
    return RES_OK;
}

static Result vm_run_bytecode(Vm* vm, Chunk* chunk) {
    while (vm->ip < buf_size(chunk->opcodes)) {
        const uint8_t opcode = chunk->opcodes[vm->ip];
        const size_t line = chunk->lines[vm->ip];

        switch (opcode) {
            case OP_NEGATE: {
                Value value = {0};
                RETURN_IF_ERR(vm_stack_pop(vm, chunk, &value));

                if (!IS_NUMBER(value))
                    VM_ERROR(line, "Expected a number, got:", value);

                RETURN_IF_ERR(
                    vm_stack_push(vm, chunk, NUMBER_VAL(-AS_NUMBER(value))));
                break;
            }
            case OP_ADD: {
                Value rhs = {0};
                RETURN_IF_ERR(vm_stack_pop(vm, chunk, &rhs));
                Value lhs = {0};
                RETURN_IF_ERR(vm_stack_pop(vm, chunk, &lhs));

                if (IS_STRING(lhs) && IS_STRING(rhs)) {
                    Value v;
                    vm_str_cat(vm, lhs, rhs, &v);
                    RETURN_IF_ERR(vm_stack_push(vm, chunk, v));
                    break;
                }

                if (!IS_NUMBER(rhs))
                    VM_ERROR(line, "Expected a number, got:", rhs);

                if (!IS_NUMBER(lhs))
                    VM_ERROR(line, "Expected a number, got:", lhs);

                // TODO: Check for underflow/overflow
                RETURN_IF_ERR(vm_stack_push(
                    vm, chunk, NUMBER_VAL(AS_NUMBER(lhs) + AS_NUMBER(rhs))));
                break;
            }
            case OP_SUBTRACT: {
                Value rhs = {0};
                RETURN_IF_ERR(vm_stack_pop(vm, chunk, &rhs));
                if (!IS_NUMBER(rhs))
                    VM_ERROR(line, "Expected a number, got:", rhs);

                Value lhs = {0};
                RETURN_IF_ERR(vm_stack_pop(vm, chunk, &lhs));
                if (!IS_NUMBER(lhs))
                    VM_ERROR(line, "Expected a number, got:", lhs);

                // TODO: Check for underflow/overflow
                RETURN_IF_ERR(vm_stack_push(
                    vm, chunk, NUMBER_VAL(AS_NUMBER(lhs) - AS_NUMBER(rhs))));
                break;
            }
            case OP_MULTIPLY: {
                Value rhs = {0};
                RETURN_IF_ERR(vm_stack_pop(vm, chunk, &rhs));
                if (!IS_NUMBER(rhs))
                    VM_ERROR(line, "Expected a number, got:", rhs);

                Value lhs = {0};
                RETURN_IF_ERR(vm_stack_pop(vm, chunk, &lhs));
                if (!IS_NUMBER(lhs))
                    VM_ERROR(line, "Expected a number, got:", lhs);

                // TODO: Check for underflow/overflow
                RETURN_IF_ERR(vm_stack_push(
                    vm, chunk, NUMBER_VAL(AS_NUMBER(lhs) * AS_NUMBER(rhs))));
                break;
            }
            case OP_DIVIDE: {
                Value rhs = {0};
                RETURN_IF_ERR(vm_stack_pop(vm, chunk, &rhs));
                if (!IS_NUMBER(rhs))
                    VM_ERROR(line, "Expected a number, got:", rhs);

                Value lhs = {0};
                RETURN_IF_ERR(vm_stack_pop(vm, chunk, &lhs));
                if (!IS_NUMBER(lhs))
                    VM_ERROR(line, "Expected a number, got:", lhs);

                RETURN_IF_ERR(vm_stack_push(
                    vm, chunk, NUMBER_VAL(AS_NUMBER(lhs) / AS_NUMBER(rhs))));
                break;
            }
            case OP_CONSTANT: {
                Value v = {0};
                RETURN_IF_ERR(vm_read_constant_in_next_byte(vm, chunk, &v));
                RETURN_IF_ERR(vm_stack_push(vm, chunk, v));
            } break;
            case OP_NIL:
                RETURN_IF_ERR(vm_stack_push(vm, chunk, NIL_VAL));
                break;
            case OP_TRUE:
                RETURN_IF_ERR(vm_stack_push(vm, chunk, BOOL_VAL(true)));
                break;
            case OP_FALSE:
                RETURN_IF_ERR(vm_stack_push(vm, chunk, BOOL_VAL(false)));
                break;
            case OP_NOT: {
                Value v = {0};
                RETURN_IF_ERR(vm_stack_pop(vm, chunk, &v));
                RETURN_IF_ERR(
                    vm_stack_push(vm, chunk, BOOL_VAL(value_is_falsy(v))));
                break;
            }
            case OP_EQUAL: {
                Value rhs = {0};
                RETURN_IF_ERR(vm_stack_pop(vm, chunk, &rhs));
                Value lhs = {0};
                RETURN_IF_ERR(vm_stack_pop(vm, chunk, &lhs));
                RETURN_IF_ERR(
                    vm_stack_push(vm, chunk, BOOL_VAL(value_eq(lhs, rhs))));
                break;
            }
            case OP_LESS: {
                Value rhs = {0};
                RETURN_IF_ERR(vm_stack_pop(vm, chunk, &rhs));
                if (!IS_NUMBER(rhs))
                    VM_ERROR(line, "Expected a number, got:", rhs);

                Value lhs = {0};
                RETURN_IF_ERR(vm_stack_pop(vm, chunk, &lhs));
                if (!IS_NUMBER(lhs))
                    VM_ERROR(line, "Expected a number, got:", lhs);

                // TODO: Check for 0
                RETURN_IF_ERR(vm_stack_push(
                    vm, chunk, BOOL_VAL(AS_NUMBER(lhs) < AS_NUMBER(rhs))));
                break;
            }
            case OP_GREATER: {
                Value rhs = {0};
                RETURN_IF_ERR(vm_stack_pop(vm, chunk, &rhs));
                if (!IS_NUMBER(rhs))
                    VM_ERROR(line, "Expected a number, got:", rhs);

                Value lhs = {0};
                RETURN_IF_ERR(vm_stack_pop(vm, chunk, &lhs));
                if (!IS_NUMBER(lhs))
                    VM_ERROR(line, "Expected a number, got:", lhs);

                // TODO: Check for 0
                RETURN_IF_ERR(vm_stack_push(
                    vm, chunk, BOOL_VAL(AS_NUMBER(lhs) > AS_NUMBER(rhs))));
                break;
            }
            case OP_PRINT: {
                Value value = {0};
                RETURN_IF_ERR(vm_stack_pop(vm, chunk, &value));
                value_print(stdout, value);
                puts("");
            } break;
            case OP_POP: {
                Value value = {0};
                RETURN_IF_ERR(vm_stack_pop(vm, chunk, &value));
            } break;
            case OP_DEFINE_GLOBAL: {
                Value name = {0};
                RETURN_IF_ERR(vm_read_constant_in_next_byte(vm, chunk, &name));

                Value value = {0};
                RETURN_IF_ERR(vm_stack_pop(vm, chunk, &value));

                ht_insert(vm->globals, AS_CSTRING(name), AS_STRING(name)->len,
                          &value, sizeof(value));
                break;
            }
            case OP_GET_GLOBAL: {
                Value name = {0};
                RETURN_IF_ERR(vm_read_constant_in_next_byte(vm, chunk, &name));

                char* const s = AS_CSTRING(name);
                const size_t s_len = AS_STRING(name)->len;
                Value* value = ht_search(vm->globals, s, s_len);
                if (!value) {
                    fprintf(stderr, "%zu:Undefined variable %.*s\n", line,
                            (int)s_len, s);
                    return RES_RUN_ERR;
                }

                RETURN_IF_ERR(vm_stack_push(vm, chunk, *value));
                break;
            }
            default:
                fprintf(stderr, "%zu:Unknown opcode %s\n", line,
                        opcode_str[opcode]);
                return RES_RUN_ERR;
        }
        vm->ip += 1;
    }
    return RES_OK;
}

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

typedef void (*ParseFn)(Parser*, Vm*);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

static void parse_grouping(Parser*, Vm*);
static void parse_unary(Parser*, Vm*);
static void parse_binary(Parser*, Vm*);
static void parse_number(Parser*, Vm*);
static void parse_literal(Parser*, Vm*);
static void parse_string(Parser*, Vm*);
static void parse_variable(Parser*, Vm*);

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
    [TOKEN_BANG] = {.prefix = parse_unary},
    [TOKEN_BANG_EQUAL] = {.infix = parse_binary, .precedence = PREC_EQUALITY},
    [TOKEN_EQUAL_EQUAL] = {.infix = parse_binary, .precedence = PREC_EQUALITY},
    [TOKEN_GREATER] = {.infix = parse_binary, .precedence = PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {.infix = parse_binary,
                             .precedence = PREC_COMPARISON},
    [TOKEN_LESS] = {.infix = parse_binary, .precedence = PREC_COMPARISON},
    [TOKEN_LESS_EQUAL] = {.infix = parse_binary, .precedence = PREC_COMPARISON},
    [TOKEN_STRING] = {.prefix = parse_string},
    [TOKEN_IDENTIFIER] = {.prefix = parse_variable},
};

static void parse_error(Parser* parser, const char* err, size_t err_len) {
    if (parser->state == PARSER_STATE_OK) {
        LOG("new parser error, entering error mode err=`%.*s`\n", (int)err_len,
            err);
        parser->state = PARSER_STATE_ERROR;
    } else if (parser->state == PARSER_STATE_ERROR) {
        LOG("new parser error, entering panic mode err=`%.*s`\n", (int)err_len,
            err);
        parser->state = PARSER_STATE_PANIC_MODE;
        return;
    } else {
        LOG("new parser error in panic mode, skipping err=`%.*s`\n",
            (int)err_len, err);
        return;
    }

    fprintf(stderr, "%zu:%zu:%.*s\n", parser->current.line,
            parser->current.column, (int)err_len, err);
}

static void parse_emit_byte(Parser* parser, uint8_t byte) {
    LOG("byte=%d opcode=%s\n", byte, opcode_str[byte]);
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

static uint8_t parse_make_constant(Parser* parser, Value v) {
    parse_emit_byte(parser, OP_CONSTANT);
    buf_push(parser->chunk->constants, v);
    const size_t constant_i = buf_size(parser->chunk->constants) - 1;
    parse_emit_byte(parser, constant_i);

    LOG("new constant index=%zu\n", constant_i);

    return constant_i;
}

static uint8_t parse_make_identifier_constant(Parser* parser, Vm* vm) {
    ObjString* const os = vm_make_string(vm, parser->previous.source_len);
    memcpy(os->s, parser->previous.source, os->len);

    return parse_make_constant(parser, OBJ_VAL(os));
}

static void parse_precedence(Parser* parser, Precedence precedence, Vm* vm) {
    LOG("precedence=%s previous_type=%s current_type=%s\n",
        precedence_str[precedence], token_type_str[parser->previous.type],
        token_type_str[parser->current.type]);
    parse_advance(parser);
    LOG("precedence=%s previous_type=%s current_type=%s\n",
        precedence_str[precedence], token_type_str[parser->previous.type],
        token_type_str[parser->current.type]);

    const ParseFn prefix_rule = rules[parser->previous.type].prefix;
    if (!prefix_rule) {
        parse_error(parser, "Expected expression", 19);
        return;
    }

    prefix_rule(parser, vm);

    while (precedence <= rules[parser->current.type].precedence) {
        parse_advance(parser);

        const ParseFn infix_rule = rules[parser->previous.type].infix;

        infix_rule(parser, vm);
    }
}

static void parse_expect(Parser* parser, TokenType type, const char err[]) {
    if (parser->current.type == type) {
        parse_advance(parser);
        return;
    }

    parse_error(parser, err, strlen(err));
}

static void parse_number(Parser* parser, Vm* vm) {
    (void)vm;
    assert(parser->previous.type = TOKEN_NUMBER);

    const double number = strtod(parser->previous.source, NULL);
    const Value v = NUMBER_VAL(number);

    parse_make_constant(parser, v);
}

static void parse_string(Parser* parser, Vm* vm) {
    assert(parser->previous.type = TOKEN_STRING);

    ObjString* const os = vm_make_string(vm, parser->previous.source_len);
    memcpy(os->s, parser->previous.source, os->len);
    const Value v = OBJ_VAL(os);

    parse_emit_byte(parser, OP_CONSTANT);
    buf_push(parser->chunk->constants, v);
    parse_emit_byte(parser, buf_size(parser->chunk->constants) - 1);
}

static void parse_named_variable(Parser* parser, Vm* vm) {
    uint8_t arg = parse_make_identifier_constant(parser, vm);
    parse_emit_byte(parser, OP_GET_GLOBAL);
    parse_emit_byte(parser, arg);
}

static void parse_variable(Parser* parser, Vm* vm) {
    parse_named_variable(parser, vm);
}

static void parse_literal(Parser* parser, Vm* vm) {
    (void)vm;

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

static void parse_expression(Parser* parser, Vm* vm);

static void parse_grouping(Parser* parser, Vm* vm) {
    parse_expression(parser, vm);
    parse_expect(parser, TOKEN_RIGHT_PAREN, "Expected `)` after expression");
}

static void parse_unary(Parser* parser, Vm* vm) {
    const TokenType previousType = parser->previous.type;

    parse_precedence(parser, PREC_UNARY, vm);

    switch (previousType) {
        case TOKEN_MINUS:
            parse_emit_byte(parser, OP_NEGATE);
            break;
        case TOKEN_BANG:
            parse_emit_byte(parser, OP_NOT);
            break;
        default:
            UNREACHABLE();
    }
}

static void parse_binary(Parser* parser, Vm* vm) {
    const TokenType previousType = parser->previous.type;

    const ParseRule* const rule = &rules[previousType];
    parse_precedence(parser, rule->precedence + 1, vm);

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
        case TOKEN_EQUAL_EQUAL:
            parse_emit_byte(parser, OP_EQUAL);
            break;
        case TOKEN_BANG_EQUAL:
            parse_emit_byte(parser, OP_EQUAL);
            parse_emit_byte(parser, OP_NOT);
            break;
        case TOKEN_GREATER:
            parse_emit_byte(parser, OP_GREATER);
            break;
        case TOKEN_GREATER_EQUAL:
            parse_emit_byte(parser, OP_EQUAL);
            parse_emit_byte(parser, OP_NOT);
            break;
        case TOKEN_LESS:
            parse_emit_byte(parser, OP_LESS);
            break;
        case TOKEN_LESS_EQUAL:
            parse_emit_byte(parser, OP_LESS);
            parse_emit_byte(parser, OP_NOT);
            break;
        default:
            UNREACHABLE();
    }
}

static void parse_expression(Parser* parser, Vm* vm) {
    parse_precedence(parser, PREC_ASSIGNMENT, vm);
}

static bool parse_match(Parser* parser, TokenType type) {
    if (parser->current.type != type) return false;

    parse_advance(parser);
    return true;
}

static void parse_print_stmt(Parser* parser, Vm* vm) {
    parse_expression(parser, vm);
    parse_expect(parser, TOKEN_SEMICOLON,
                 "Expected terminating semicolon after expression");
    parse_emit_byte(parser, OP_PRINT);
}

static void parse_expr_stmt(Parser* parser, Vm* vm) {
    parse_expression(parser, vm);
    parse_expect(parser, TOKEN_SEMICOLON,
                 "Expected terminating semicolon after expression");
    parse_emit_byte(parser, OP_POP);
}

static void parse_statement(Parser* parser, Vm* vm) {
    if (parse_match(parser, TOKEN_PRINT)) {
        parse_print_stmt(parser, vm);
    } else {
        parse_expr_stmt(parser, vm);
    }
}

static void parse_sync(Parser* parser) {
    parser->state = PARSER_STATE_SYNCED;

    while (parser->current.type != TOKEN_EOF) {
        if (parser->previous.type == TOKEN_SEMICOLON) return;

        switch (parser->current.type) {
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_PRINT:
            case TOKEN_RETURN:
                return;

            default:
                // Do nothing.
                ;
        }

        parse_advance(parser);
    }
}

static uint8_t parse_variable_name(Parser* parser, Vm* vm, const char err[]) {
    parse_expect(parser, TOKEN_IDENTIFIER, err);

    return parse_make_identifier_constant(parser, vm);
}

static void parse_define_variable(Parser* parser, uint8_t global_i) {
    parse_emit_byte(parser, OP_DEFINE_GLOBAL);
    parse_emit_byte(parser, global_i);
}

static void parse_var_declaration(Parser* parser, Vm* vm) {
    const uint8_t global_i =
        parse_variable_name(parser, vm, "Expected variable name");

    if (parse_match(parser, TOKEN_EQUAL))
        parse_expression(parser, vm);
    else
        parse_emit_byte(parser, OP_NIL);

    parse_expect(parser, TOKEN_SEMICOLON,
                 "Expected semicolon after variable declaration");

    parse_define_variable(parser, global_i);
}

static void parse_declaration(Parser* parser, Vm* vm) {
    if (parse_match(parser, TOKEN_VAR))
        parse_var_declaration(parser, vm);
    else
        parse_statement(parser, vm);

    if (parser->state == PARSER_STATE_PANIC_MODE) parse_sync(parser);
}

static Result parse_compile(const char* source, size_t source_len, Chunk* chunk,
                            Vm* vm) {
    LOG("source_len=%zu source=`%.*s`\n", source_len, (int)source_len, source);

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

    while (!parse_match(&parser, TOKEN_EOF)) {
        parse_declaration(&parser, vm);
    }

    if (parser.state != PARSER_STATE_OK) return RES_PARSE_ERR;

    return RES_OK;
}

static Result vm_interpret(const char* source, size_t source_len,
                           Result (*bytecode_fn)(Vm*, Chunk*)) {
    Vm vm = {.globals = ht_init(100, NULL)};
    Chunk chunk = {0};
    Result result = RES_OK;

    if ((result = parse_compile(source, source_len, &chunk, &vm)) != RES_OK)
        goto cleanup;

    LOG("parsing successful%s\n", "");
    result = bytecode_fn(&vm, &chunk);

cleanup:
    value_obj_free(&vm);
    free((char*)source);

    return result;
}

void vm_repl_sig_quit(int signal) {
    printf("\nBye!\n");
    exit(signal);
}

static void vm_repl() {
    signal(SIGINT, vm_repl_sig_quit);

    Vm vm = {.globals = ht_init(100, NULL)};
    setvbuf(stdout, (char*)NULL, _IONBF, 0);

    while (true) {
        vm.ip = 0;

        char* source = NULL;
        size_t source_len = 0;
        size_t line_cap = 255;

        printf("> ");
        if ((source_len = getline(&source, &line_cap, stdin)) <= 0) {
            fprintf(stderr, "Could not read from stdin: errno=%s error=%d\n",
                    strerror(errno), errno);
            exit(errno);
        }

        Chunk chunk = {0};
        Result result = RES_OK;

        if ((result = parse_compile(source, source_len, &chunk, &vm)) != RES_OK)
            continue;

        LOG("parsing successful%s\n", "");
        vm_run_bytecode(&vm, &chunk);
    }
}

void cli_help(const char* argv[]) {
    printf("Usage: %s dump|run|repl [filename]\n", argv[0]);
    exit(0);
}

int main(int argc, const char* argv[]) {
    if (argc == 2 && strcmp(argv[1], "repl") == 0)
        vm_repl();
    else if (argc == 3) {
        char* source = NULL;
        size_t source_len = 0;
        if (strcmp(argv[2], "-") == 0)
            read_stdin(&source, &source_len);
        else
            read_file(argv[2], &source, &source_len);

        if (strcmp(argv[1], "dump") == 0)
            vm_interpret(source, source_len, vm_dump);
        else if (strcmp(argv[1], "run") == 0)
            return vm_interpret(source, source_len, vm_run_bytecode);
    } else
        cli_help(argv);
}
