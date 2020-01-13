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

static void vm_str_cat(Vm* vm, Value lhs, Value rhs, Value* res) {
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

    LOG("pushed %s", "");
    LOG_VALUE_LN(v);

    return RES_OK;
}

static Result vm_stack_peek(const Vm* vm, const Chunk* chunk, Value* v) {
    if (vm->stack_len == 0) {
        fprintf(stderr, "%zu:Cannot peek from an empty stack\n",
                chunk->lines[vm->ip]);
        return RES_RUN_ERR;
    }

    *v = vm->stack[vm->stack_len - 1];
    return RES_OK;
}

static Result vm_stack_pop(Vm* vm, Chunk* chunk, Value* v) {
    if (vm->stack_len == 0) {
        fprintf(stderr, "%zu:Cannot pop from an empty stack\n",
                chunk->lines[vm->ip]);
        return RES_RUN_ERR;
    }

    *v = vm->stack[vm->stack_len - 1];
    LOG("popped %s", "");
    LOG_VALUE_LN(*v);
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
    const uint8_t opcode = chunk->opcodes[vm->ip];
    const size_t line = chunk->lines[vm->ip];
    Value value = {0};
    RETURN_IF_ERR(vm_read_constant_in_next_byte(vm, chunk, &value));

    printf("%zu:%s:", line, opcode_str[opcode]);
    value_print(stdout, value);
    puts("");

    return RES_OK;
}

Result vm_dump(Vm* vm, Chunk* chunk) {
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
            case OP_SET_GLOBAL:
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

Result vm_run_bytecode(Vm* vm, Chunk* chunk) {
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
                    Value v = {0};
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
                    vm_stack_push(vm, chunk, BOOL_VAL(value_is_falsy(&v))));
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
                LOG("def global name=%.*s value=", (int)AS_STRING(name)->len,
                    AS_CSTRING(name));
                LOG_VALUE_LN(value);

                LOG("stack size: %d\n", vm->stack_len);
                break;
            }
            case OP_GET_GLOBAL: {
                Value name = {0};
                RETURN_IF_ERR(vm_read_constant_in_next_byte(vm, chunk, &name));

                char* const s = AS_CSTRING(name);
                const size_t s_len = AS_STRING(name)->len;
                Value* value = ht_search(vm->globals, s, s_len);
                LOG("get global name=%.*s value=", (int)s_len, s);
                LOG_VALUE_LN(*value);

                if (!value) {
                    fprintf(stderr, "%zu:Undefined variable %.*s\n", line,
                            (int)s_len, s);
                    return RES_RUN_ERR;
                }

                RETURN_IF_ERR(vm_stack_push(vm, chunk, *value));
                break;
            }
            case OP_SET_GLOBAL: {
                Value name = {0};
                RETURN_IF_ERR(vm_read_constant_in_next_byte(vm, chunk, &name));

                char* const s = AS_CSTRING(name);
                const size_t s_len = AS_STRING(name)->len;
                Value* const value = ht_search(vm->globals, s, s_len);
                if (!value) {
                    fprintf(stderr, "%zu:Undefined variable %.*s\n", line,
                            (int)s_len, s);
                    return RES_RUN_ERR;
                }

                RETURN_IF_ERR(vm_stack_peek(vm, chunk, value));
                LOG("set global name=%.*s value=", (int)s_len, s);
                LOG_VALUE_LN(*value);

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

static void value_obj_free(Vm* vm) {
    Obj* obj = vm->objects;
    while (obj) {
        Obj* next = obj->next;
        free(obj);
        obj = next;
    }
}

Result vm_interpret(const char* source, size_t source_len,
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

static void vm_repl_sig_quit(int signal) {
    printf("\nBye!\n");
    exit(signal);
}

void vm_repl() {
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
