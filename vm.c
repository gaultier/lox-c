#include "vm.h"

#include <assert.h>
#include <errno.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "buf.h"
#include "parse.h"
#include "utils.h"

const char opcode_str[OP_COUNT][17] = {
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
    [OP_SET_GLOBAL] = "OP_SET_GLOBAL",
    [OP_GET_LOCAL] = "OP_GET_LOCAL",
    [OP_SET_LOCAL] = "OP_SET_LOCAL",
    [OP_JUMP_IF_FALSE] = "OP_JUMP_IF_FALSE",
    [OP_JUMP] = "OP_JUMP",
    [OP_LOOP] = "OP_LOOP",
};

static void str_cat(Vm* vm, Value lhs, Value rhs, Value* res) {
    assert(IS_STRING(lhs));
    assert(IS_STRING(rhs));

    const char* const lhs_s = AS_CSTRING(lhs);
    const size_t lhs_len = AS_STRING(lhs)->len;
    const char* const rhs_s = AS_CSTRING(rhs);
    const size_t rhs_len = AS_STRING(rhs)->len;

    ObjString* const os = value_make_string(&vm->objects, lhs_len + rhs_len);

    memcpy(os->s, lhs_s, lhs_len);
    memcpy(os->s + lhs_len, rhs_s, rhs_len);

    *res = OBJ_VAL(os);
}

#define VM_ERROR(loc, fmt, value)                                \
    do {                                                         \
        fprintf(stderr, "%zu:%zu:" fmt, loc->line, loc->column); \
        value_print_err(value);                                  \
        fprintf(stderr, "\n");                                   \
        return RES_RUN_ERR;                                      \
    } while (0)

static void stack_log(Vm* vm) {
    for (intmax_t i = 0; i < vm->stack_len; i++) {
        LOG("stack[%jd]=", i);
        LOG_VALUE_LN(vm->stack[i]);
    }
}

static Result stack_push(Vm* vm, Chunk* chunk, Value v) {
    if (vm->stack_len == (STACK_MAX - 1)) {
        const Location* const loc = &chunk->locations[vm->ip];
        fprintf(stderr, "%zu:%zu:Maximum stack size reached: %d\n", loc->line,
                loc->column, STACK_MAX);
        return RES_RUN_ERR;
    }
    vm->stack_len += 1;
    vm->stack[vm->stack_len - 1] = v;

    LOG("pushed %s", "");
    LOG_VALUE_LN(v);

    return RES_OK;
}

static Result stack_peek_from_bottom_at(const Vm* vm, const Chunk* chunk,
                                        Value* v, intmax_t i) {
    if (vm->stack_len == 0 || !(i < vm->stack_len)) {
        const Location* const loc = &chunk->locations[vm->ip];
        fprintf(
            stderr,
            "%zu:%zu:Cannot peek in the stack at this location: stack_len=%d "
            "i=%jd\n",
            loc->line, loc->column, vm->stack_len, i);
        return RES_RUN_ERR;
    }

    *v = vm->stack[i];

    LOG("i=%jd v=", i);
    LOG_VALUE_LN(*v);

    return RES_OK;
}

static Result stack_peek_from_top_at(const Vm* vm, const Chunk* chunk, Value* v,
                                     intmax_t i) {
    return stack_peek_from_bottom_at(vm, chunk, v, vm->stack_len - i - 1);
}

static Result stack_pop(Vm* vm, Chunk* chunk, Value* v) {
    if (vm->stack_len == 0) {
        const Location* const loc = &chunk->locations[vm->ip];
        fprintf(stderr, "%zu:%zu:Cannot pop from an empty stack\n", loc->line,
                loc->column);
        return RES_RUN_ERR;
    }

    *v = vm->stack[vm->stack_len - 1];
    LOG("popped %s", "");
    LOG_VALUE_LN(*v);
    vm->stack[vm->stack_len - 1] = (Value){0};
    vm->stack_len -= 1;

    return RES_OK;
}

static Result read_next_byte(Vm* vm, Chunk* chunk, uint8_t* byte) {
    const uint8_t opcode = chunk->opcodes[vm->ip];
    const Location* const loc = &chunk->locations[vm->ip];

    vm->ip += 1;

    if (!(vm->ip < buf_size(chunk->opcodes))) {
        fprintf(stderr, "%zu:%zu:Malformed opcode: missing operand for %s\n",
                loc->line, loc->column, opcode_str[opcode]);
        return RES_RUN_ERR;
    }
    *byte = chunk->opcodes[vm->ip];

    return RES_OK;
}

static Result read_constant_in_next_byte(Vm* vm, Chunk* chunk, Value* v) {
    uint8_t value_index = 0;
    RETURN_IF_ERR(read_next_byte(vm, chunk, &value_index));
    *v = chunk->constants[value_index];
    LOG("constant index=%d\n", value_index);

    return RES_OK;
}

static Result read_u16(Vm* vm, Chunk* chunk, uint16_t* u16) {
    uint8_t b1 = 0;
    RETURN_IF_ERR(read_next_byte(vm, chunk, &b1));

    uint8_t b2 = 0;
    RETURN_IF_ERR(read_next_byte(vm, chunk, &b2));

    *u16 = (uint16_t)(b1 << 8) | b2;
    return RES_OK;
}

static Result dump_opcode_u8_operand(Vm* vm, Chunk* chunk) {
    const uint8_t opcode = chunk->opcodes[vm->ip];
    const Location* const loc = &chunk->locations[vm->ip];

    uint8_t b = 0;
    RETURN_IF_ERR(read_next_byte(vm, chunk, &b));

    printf("%zu:%zu:%s:%d\n", loc->line, loc->column, opcode_str[opcode], b);

    return RES_OK;
}

static Result dump_opcode_u16_operand(Vm* vm, Chunk* chunk) {
    const uint8_t opcode = chunk->opcodes[vm->ip];
    const Location* const loc = &chunk->locations[vm->ip];

    uint16_t u16 = 0;
    RETURN_IF_ERR(read_u16(vm, chunk, &u16));
    printf("%zu:%zu:%s:%d\n", loc->line, loc->column, opcode_str[opcode], u16);

    return RES_OK;
}

Result vm_dump(Vm* vm, Chunk* chunk) {
    while (vm->ip < buf_size(chunk->opcodes)) {
        const uint8_t opcode = chunk->opcodes[vm->ip];
        const Location* const loc = &chunk->locations[vm->ip];

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
                printf("%zu:%zu:%s\n", loc->line, loc->column,
                       opcode_str[opcode]);
                break;
            case OP_CONSTANT:
            case OP_DEFINE_GLOBAL:
            case OP_GET_GLOBAL:
            case OP_SET_GLOBAL:
            case OP_GET_LOCAL:
            case OP_SET_LOCAL:
                RETURN_IF_ERR(dump_opcode_u8_operand(vm, chunk));
                break;
            case OP_JUMP_IF_FALSE:
            case OP_JUMP:
            case OP_LOOP:
                RETURN_IF_ERR(dump_opcode_u16_operand(vm, chunk));
                break;
            default:
                fprintf(stderr, "%zu:%zu:Unknown opcode %hhu\n", loc->line,
                        loc->column, opcode);
                return RES_RUN_ERR;
        }
        vm->ip += 1;
    }
    return RES_OK;
}

Result vm_run_bytecode(Vm* vm, Chunk* chunk) {
    while (vm->ip < buf_size(chunk->opcodes)) {
        const uint8_t opcode = chunk->opcodes[vm->ip];
        const Location* const loc = &chunk->locations[vm->ip];

        switch (opcode) {
            case OP_NEGATE: {
                Value value = {0};
                RETURN_IF_ERR(stack_pop(vm, chunk, &value));

                if (!IS_NUMBER(value))
                    VM_ERROR(loc, "Negation: expected a number, got:", value);

                RETURN_IF_ERR(
                    stack_push(vm, chunk, NUMBER_VAL(-AS_NUMBER(value))));
                break;
            }
            case OP_ADD: {
                Value rhs = {0};
                RETURN_IF_ERR(stack_pop(vm, chunk, &rhs));
                Value lhs = {0};
                RETURN_IF_ERR(stack_pop(vm, chunk, &lhs));

                if (IS_STRING(lhs) && IS_STRING(rhs)) {
                    Value v = {0};
                    str_cat(vm, lhs, rhs, &v);

                    RETURN_IF_ERR(stack_push(vm, chunk, v));
                    break;
                }

                if ((IS_STRING(lhs) && !IS_STRING(rhs)))
                    VM_ERROR(
                        loc,
                        "Addition: cannot concatenate a non-string type, got:",
                        rhs);

                if ((IS_STRING(rhs) && !IS_STRING(lhs)))
                    VM_ERROR(
                        loc,
                        "Addition: cannot concatenate a non-string type, got:",
                        lhs);

                if (!IS_NUMBER(lhs))
                    VM_ERROR(loc, "Addition: expected a number, got:", lhs);

                if (!IS_NUMBER(rhs))
                    VM_ERROR(loc, "Addition: expected a number, got:", rhs);

                // TODO: Check for underflow/overflow
                RETURN_IF_ERR(stack_push(
                    vm, chunk, NUMBER_VAL(AS_NUMBER(lhs) + AS_NUMBER(rhs))));
                break;
            }
            case OP_SUBTRACT: {
                Value rhs = {0};
                RETURN_IF_ERR(stack_pop(vm, chunk, &rhs));
                if (!IS_NUMBER(rhs))
                    VM_ERROR(loc, "Subtraction: expected a number, got:", rhs);

                Value lhs = {0};
                RETURN_IF_ERR(stack_pop(vm, chunk, &lhs));
                if (!IS_NUMBER(lhs))
                    VM_ERROR(loc, "Subtraction: expected a number, got:", lhs);

                // TODO: Check for underflow/overflow
                RETURN_IF_ERR(stack_push(
                    vm, chunk, NUMBER_VAL(AS_NUMBER(lhs) - AS_NUMBER(rhs))));
                break;
            }
            case OP_MULTIPLY: {
                Value rhs = {0};
                RETURN_IF_ERR(stack_pop(vm, chunk, &rhs));
                if (!IS_NUMBER(rhs))
                    VM_ERROR(loc,
                             "Multiplication: expected a number, got:", rhs);

                Value lhs = {0};
                RETURN_IF_ERR(stack_pop(vm, chunk, &lhs));
                if (!IS_NUMBER(lhs))
                    VM_ERROR(loc,
                             "Multiplication: expected a number, got:", lhs);

                // TODO: Check for underflow/overflow
                RETURN_IF_ERR(stack_push(
                    vm, chunk, NUMBER_VAL(AS_NUMBER(lhs) * AS_NUMBER(rhs))));
                break;
            }
            case OP_DIVIDE: {
                Value rhs = {0};
                RETURN_IF_ERR(stack_pop(vm, chunk, &rhs));
                if (!IS_NUMBER(rhs))
                    VM_ERROR(loc, "Division: expected a number, got:", rhs);

                Value lhs = {0};
                RETURN_IF_ERR(stack_pop(vm, chunk, &lhs));
                if (!IS_NUMBER(lhs))
                    VM_ERROR(loc, "Division: expected a number, got:", lhs);

                RETURN_IF_ERR(stack_push(
                    vm, chunk, NUMBER_VAL(AS_NUMBER(lhs) / AS_NUMBER(rhs))));
                break;
            }
            case OP_CONSTANT: {
                Value v = {0};
                RETURN_IF_ERR(read_constant_in_next_byte(vm, chunk, &v));
                RETURN_IF_ERR(stack_push(vm, chunk, v));
            } break;
            case OP_NIL:
                RETURN_IF_ERR(stack_push(vm, chunk, NIL_VAL));
                break;
            case OP_TRUE:
                RETURN_IF_ERR(stack_push(vm, chunk, BOOL_VAL(true)));
                break;
            case OP_FALSE:
                RETURN_IF_ERR(stack_push(vm, chunk, BOOL_VAL(false)));
                break;
            case OP_NOT: {
                Value v = {0};
                RETURN_IF_ERR(stack_pop(vm, chunk, &v));
                RETURN_IF_ERR(
                    stack_push(vm, chunk, BOOL_VAL(value_is_falsy(&v))));
                break;
            }
            case OP_EQUAL: {
                Value rhs = {0};
                RETURN_IF_ERR(stack_pop(vm, chunk, &rhs));
                Value lhs = {0};
                RETURN_IF_ERR(stack_pop(vm, chunk, &lhs));
                RETURN_IF_ERR(
                    stack_push(vm, chunk, BOOL_VAL(value_eq(lhs, rhs))));
                break;
            }
            case OP_LESS: {
                Value rhs = {0};
                RETURN_IF_ERR(stack_pop(vm, chunk, &rhs));
                if (!IS_NUMBER(rhs))
                    VM_ERROR(loc, "Comparison:expected a number, got:", rhs);

                Value lhs = {0};
                RETURN_IF_ERR(stack_pop(vm, chunk, &lhs));
                if (!IS_NUMBER(lhs))
                    VM_ERROR(loc, "Comparison:expected a number, got:", lhs);

                // TODO: Check for 0
                RETURN_IF_ERR(stack_push(
                    vm, chunk, BOOL_VAL(AS_NUMBER(lhs) < AS_NUMBER(rhs))));
                break;
            }
            case OP_GREATER: {
                Value rhs = {0};
                RETURN_IF_ERR(stack_pop(vm, chunk, &rhs));
                if (!IS_NUMBER(rhs))
                    VM_ERROR(loc, "Comparison:expected a number, got:", rhs);

                Value lhs = {0};
                RETURN_IF_ERR(stack_pop(vm, chunk, &lhs));
                if (!IS_NUMBER(lhs))
                    VM_ERROR(loc, "Comparison:expected a number, got:", lhs);

                // TODO: Check for 0
                RETURN_IF_ERR(stack_push(
                    vm, chunk, BOOL_VAL(AS_NUMBER(lhs) > AS_NUMBER(rhs))));
                break;
            }
            case OP_PRINT: {
                Value value = {0};
                RETURN_IF_ERR(stack_pop(vm, chunk, &value));
                value_print(value);
                puts("");
            } break;
            case OP_POP: {
                Value value = {0};
                RETURN_IF_ERR(stack_pop(vm, chunk, &value));
            } break;
            case OP_DEFINE_GLOBAL: {
                Value name = {0};
                RETURN_IF_ERR(read_constant_in_next_byte(vm, chunk, &name));

                Value value = {0};
                RETURN_IF_ERR(stack_pop(vm, chunk, &value));

                ht_insert(vm->globals, AS_CSTRING(name), AS_STRING(name)->len,
                          &value, sizeof(value));
                LOG("def global name=%.*s value=", (int)AS_STRING(name)->len,
                    AS_CSTRING(name));
                LOG_VALUE_LN(value);

                LOG("stack size: %d\n", vm->stack_len);
                break;
            }
            case OP_GET_GLOBAL: {
                Value name = {0};
                RETURN_IF_ERR(read_constant_in_next_byte(vm, chunk, &name));

                char* const s = AS_CSTRING(name);
                const size_t s_len = AS_STRING(name)->len;
                Value* value = ht_search(vm->globals, s, s_len);
                LOG("get global name=%.*s value=", (int)s_len, s);
                LOG_VALUE_LN(*value);

                if (!value) {
                    fprintf(stderr, "%zu:%zu:Undefined variable `%.*s`\n",
                            loc->line, loc->column, (int)s_len, s);
                    return RES_RUN_ERR;
                }

                RETURN_IF_ERR(stack_push(vm, chunk, *value));
                break;
            }
            case OP_SET_GLOBAL: {
                Value name = {0};
                RETURN_IF_ERR(read_constant_in_next_byte(vm, chunk, &name));

                char* const s = AS_CSTRING(name);
                const size_t s_len = AS_STRING(name)->len;
                Value* const value = ht_search(vm->globals, s, s_len);
                if (!value) {
                    fprintf(stderr, "%zu:%zu:Undefined variable `%.*s`\n",
                            loc->line, loc->column, (int)s_len, s);
                    return RES_RUN_ERR;
                }

                RETURN_IF_ERR(stack_peek_from_top_at(vm, chunk, value, 0));
                LOG("set global name=%.*s value=", (int)s_len, s);
                LOG_VALUE_LN(*value);

                break;
            }
            case OP_GET_LOCAL: {
                uint8_t local_index = 0;
                RETURN_IF_ERR(read_next_byte(vm, chunk, &local_index));

                Value v = {0};
                RETURN_IF_ERR(
                    stack_peek_from_bottom_at(vm, chunk, &v, local_index));
                RETURN_IF_ERR(stack_push(vm, chunk, v));
                LOG("OP_GET_LOCAL local_index=%d v=", local_index);
                LOG_VALUE_LN(v);

                stack_log(vm);
                break;
            }
            case OP_SET_LOCAL: {
                uint8_t local_index = 0;
                RETURN_IF_ERR(read_next_byte(vm, chunk, &local_index));

                Value v = {0};
                RETURN_IF_ERR(stack_peek_from_top_at(vm, chunk, &v, 0));
                vm->stack[local_index] = v;
                stack_log(vm);

                break;
            }
            case OP_JUMP_IF_FALSE: {
                uint16_t jump = 0;
                RETURN_IF_ERR(read_u16(vm, chunk, &jump));

                Value v = {0};
                RETURN_IF_ERR(stack_peek_from_top_at(vm, chunk, &v, 0));
                vm->ip += jump * value_is_falsy(&v);
            } break;
            case OP_JUMP: {
                uint16_t jump = 0;
                RETURN_IF_ERR(read_u16(vm, chunk, &jump));
                vm->ip += jump;
            } break;
            case OP_LOOP: {
                uint16_t jump = 0;
                RETURN_IF_ERR(read_u16(vm, chunk, &jump));
                vm->ip -= jump;
            } break;
            default:
                fprintf(stderr, "%zu:%zu:Unknown opcode %s\n", loc->line,
                        loc->column, opcode_str[opcode]);
                return RES_RUN_ERR;
        }
        vm->ip += 1;
    }
    return RES_OK;
}

static void value_obj_free(Vm* vm) {
    Obj* obj = vm->objects;
    while (obj) {
        Obj* next = obj->next;
        free(obj);
        obj = next;
    }
}

Result vm_interpret(char* source, size_t source_len,
                    Result (*bytecode_fn)(Vm*, Chunk*)) {
    Vm vm = {.globals = ht_init(100, NULL)};
    Chunk chunk = {0};
    Result result = RES_OK;

    if ((result = parser_compile(source, source_len, &chunk, &vm)) != RES_OK)
        goto cleanup;

    LOG("parsing successful%s\n", "");
    result = bytecode_fn(&vm, &chunk);

cleanup:
    value_obj_free(&vm);
    free(source);

    return result;
}

static void repl_sig_quit(int signal) {
    printf("\nBye!\n");
    exit(signal);
}

void vm_repl(void) {
    signal(SIGINT, repl_sig_quit);

    Vm vm = {.globals = ht_init(100, NULL)};
    setvbuf(stdout, (char*)NULL, _IONBF, 0);

    while (true) {
        vm.ip = 0;

        char* source = NULL;
        ssize_t source_len = 0;
        size_t line_cap = 255;

        printf("> ");
        if ((source_len = getline(&source, &line_cap, stdin)) <= 0) {
            fprintf(stderr, "Could not read from stdin: errno=%s error=%d\n",
                    strerror(errno), errno);
            exit(errno);
        }

        Chunk chunk = {0};
        Result result = RES_OK;

        if ((result = parser_compile(source, (size_t)source_len, &chunk,
                                     &vm)) != RES_OK)
            continue;

        LOG("parsing successful%s\n", "");
        vm_run_bytecode(&vm, &chunk);
    }
}
