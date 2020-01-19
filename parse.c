#include "parse.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "buf.h"
#include "lex.h"
#include "utils.h"

const char precedence_str[PREC_COUNT][16] = {
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

typedef void (*ParseFn)(Parser*, Vm*, bool);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

static void grouping(Parser*, Vm*, bool);
static void unary(Parser*, Vm*, bool);
static void binary(Parser*, Vm*, bool);
static void number(Parser*, Vm*, bool);
static void literal(Parser*, Vm*, bool);
static void string(Parser*, Vm*, bool);
static void variable(Parser*, Vm*, bool);
static void and (Parser*, Vm*, bool);
static void or (Parser*, Vm*, bool);
static void expression(Parser* parser, Vm* vm);
static void declaration(Parser* parser, Vm* vm);
static void statement(Parser* parser, Vm* vm);
static void var_declaration(Parser* parser, Vm* vm);

static const ParseRule rules[TOKEN_COUNT] = {
    [TOKEN_LEFT_PAREN] = {.prefix = grouping},
    [TOKEN_MINUS] = {.prefix = unary, .infix = binary, .precedence = PREC_TERM},
    [TOKEN_PLUS] = {.infix = binary, .precedence = PREC_TERM},
    [TOKEN_SLASH] = {.infix = binary, .precedence = PREC_FACTOR},
    [TOKEN_STAR] = {.infix = binary, .precedence = PREC_FACTOR},
    [TOKEN_NUMBER] = {.prefix = number},
    [TOKEN_NIL] = {.prefix = literal},
    [TOKEN_TRUE] = {.prefix = literal},
    [TOKEN_FALSE] = {.prefix = literal},
    [TOKEN_BANG] = {.prefix = unary},
    [TOKEN_BANG_EQUAL] = {.infix = binary, .precedence = PREC_EQUALITY},
    [TOKEN_EQUAL_EQUAL] = {.infix = binary, .precedence = PREC_EQUALITY},
    [TOKEN_GREATER] = {.infix = binary, .precedence = PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {.infix = binary, .precedence = PREC_COMPARISON},
    [TOKEN_LESS] = {.infix = binary, .precedence = PREC_COMPARISON},
    [TOKEN_LESS_EQUAL] = {.infix = binary, .precedence = PREC_COMPARISON},
    [TOKEN_STRING] = {.prefix = string},
    [TOKEN_IDENTIFIER] = {.prefix = variable},
    [TOKEN_AND] = {.infix = and, .precedence = PREC_AND},
    [TOKEN_OR] = {.infix = or, .precedence = PREC_OR},
};

static void error(Parser* parser, const Token* token, const char* err,
                  size_t err_len) {
    switch (parser->state) {
        case PARSER_STATE_OK:
            LOG("new parser error, entering error mode err=`%.*s`\n",
                (int)err_len, err);
            parser->state = PARSER_STATE_ERROR;

            fprintf(stderr, "%zu:%zu:%.*s, got: `%.*s`\n", token->line,
                    token->column, (int)err_len, err, (int)token->source_len,
                    token->source);
            return;
        case PARSER_STATE_ERROR:
            LOG("new parser error, entering panic mode err=`%.*s`\n",
                (int)err_len, err);
            parser->state = PARSER_STATE_PANIC_MODE;
            return;
        case PARSER_STATE_PANIC_MODE:
        case PARSER_STATE_SYNCED:
            LOG("new parser error in panic mode, skipping err=`%.*s`\n",
                (int)err_len, err);
            return;
        default:
            UNREACHABLE();
    }
}

static void error_str_nul(Parser* parser, const Token* token, const char* err) {
    error(parser, token, err, strlen(err));
}

static void emit_byte(Parser* parser, uint8_t byte) {
    LOG("byte=%d opcode=%s\n", byte, opcode_str[byte]);
    Location loc = {.line = parser->previous.line,
                    .column = parser->previous.column};
    buf_push(parser->chunk->locations, loc);
    buf_push(parser->chunk->opcodes, byte);
}

static void emit_byte2(Parser* parser, uint8_t byte1, uint8_t byte2) {
    LOG("opcode1=%s byte2=%d\n", opcode_str[byte1], byte2);
    Location loc = {.line = parser->previous.line,
                    .column = parser->previous.column};
    buf_push(parser->chunk->locations, loc);
    buf_push(parser->chunk->opcodes, byte1);
    buf_push(parser->chunk->locations, loc);
    buf_push(parser->chunk->opcodes, byte2);
}

static void advance(Parser* parser) {
    parser->previous = parser->current;

    while (true) {
        lex_scan_token(&parser->lex, &parser->current);
        if (parser->current.type != TOKEN_ERROR) return;

        error(parser, &parser->current, parser->current.source,
              parser->current.source_len);
    }
}

static bool peek(Parser* parser, TokenType type) {
    return (parser->current.type == type);
}

static bool match(Parser* parser, TokenType type) {
    if (!peek(parser, type)) return false;

    advance(parser);
    return true;
}

static uint8_t make_constant(Parser* parser, Value v) {
    buf_push(parser->chunk->constants, v);
    const size_t constant_i = buf_size(parser->chunk->constants) - 1;

    return (uint8_t)constant_i;
}

static uint8_t make_identifier_constant(Parser* parser, Vm* vm) {
    ObjString* const os =
        value_make_string(&vm->objects, parser->previous.source_len);
    memcpy(os->s, parser->previous.source, os->len);

    return make_constant(parser, OBJ_VAL(os));
}

static void precedence(Parser* parser, Precedence precedence, Vm* vm) {
    LOG("precedence=%s previous_type=%s current_type=%s\n",
        precedence_str[precedence], token_type_str[parser->previous.type],
        token_type_str[parser->current.type]);
    advance(parser);
    LOG("precedence=%s previous_type=%s current_type=%s\n",
        precedence_str[precedence], token_type_str[parser->previous.type],
        token_type_str[parser->current.type]);

    const ParseFn prefix_rule = rules[parser->previous.type].prefix;
    if (!prefix_rule) {
        error_str_nul(parser, &parser->previous, "Expected expression");
        return;
    }

    const bool canAssign = precedence <= PREC_ASSIGNMENT;
    LOG("precedence=%d canAssign=%d previous_type=%d\n", precedence, canAssign,
        parser->previous.type);
    prefix_rule(parser, vm, canAssign);
    LOG("precedence=%d canAssign=%d\n", precedence, canAssign);

    while (!peek(parser, TOKEN_EOF) &&
           (precedence <= rules[parser->current.type].precedence)) {
        LOG("precedence=%d\n", precedence);
        advance(parser);

        const ParseFn infix_rule = rules[parser->previous.type].infix;

        infix_rule(parser, vm, canAssign);
    }

    if (canAssign && peek(parser, TOKEN_EQUAL)) {
        error_str_nul(parser, &parser->previous,
                      "Expression cannot be assigned");
        return;
    }
}

static void expect(Parser* parser, TokenType type, const char err[]) {
    if (parser->current.type == type) {
        advance(parser);
        return;
    }

    error_str_nul(parser, &parser->current, err);
}

static void number(Parser* parser, Vm* vm, bool canAssign) {
    (void)canAssign;
    (void)vm;
    assert(parser->previous.type == TOKEN_NUMBER);

    const double number = strtod(parser->previous.source, NULL);
    const Value v = NUMBER_VAL(number);

    emit_byte2(parser, OP_CONSTANT, make_constant(parser, v));
}

static void string(Parser* parser, Vm* vm, bool canAssign) {
    (void)canAssign;
    assert(parser->previous.type = TOKEN_STRING);

    ObjString* const os =
        value_make_string(&vm->objects, parser->previous.source_len);
    memcpy(os->s, parser->previous.source, os->len);
    const Value v = OBJ_VAL(os);

    buf_push(parser->chunk->constants, v);
    emit_byte2(parser, OP_CONSTANT,
               (uint8_t)((intmax_t)buf_size(parser->chunk->constants) - 1));
}

static int resolve_local(Parser* parser, const Token* name) {
    for (int i = parser->compiler->locals_len - 1; i >= 0; i--) {
        const Local* const l = &parser->compiler->locals[i];
        if (str_eq(name->source, name->source_len, l->name.source,
                   l->name.source_len)) {
            if (l->depth == -1)
                error_str_nul(
                    parser, &parser->previous,
                    "Cannot read local variable in its own initializer");

            return i;
        }
    }
    return -1;
}

static void named_variable(Parser* parser, Vm* vm, bool canAssign) {
    const Token* const name = &parser->previous;

    int arg = resolve_local(parser, name);
    const bool is_local = arg >= 0;
    LOG("arg=%d is_local=%d name=`%.*s`\n", arg, is_local,
        (int)name->source_len, name->source);

    if (!is_local) arg = make_identifier_constant(parser, vm);

    if (canAssign && match(parser, TOKEN_EQUAL)) {
        expression(parser, vm);
        emit_byte2(parser, is_local ? OP_SET_LOCAL : OP_SET_GLOBAL,
                   (uint8_t)arg);
    } else
        emit_byte2(parser, is_local ? OP_GET_LOCAL : OP_GET_GLOBAL,
                   (uint8_t)arg);
}

static void variable(Parser* parser, Vm* vm, bool canAssign) {
    named_variable(parser, vm, canAssign);
}

static void literal(Parser* parser, Vm* vm, bool canAssign) {
    (void)canAssign;
    (void)vm;

    switch (parser->previous.type) {
        case TOKEN_NIL:
            emit_byte(parser, OP_NIL);
            break;
        case TOKEN_TRUE:
            emit_byte(parser, OP_TRUE);
            break;
        case TOKEN_FALSE:
            emit_byte(parser, OP_FALSE);
            break;
        default:
            UNREACHABLE();
    }
}

static void grouping(Parser* parser, Vm* vm, bool canAssign) {
    (void)canAssign;
    expression(parser, vm);
    expect(parser, TOKEN_RIGHT_PAREN, "Expected `)` after expression");
}

static void unary(Parser* parser, Vm* vm, bool canAssign) {
    (void)canAssign;
    const TokenType previousType = parser->previous.type;

    precedence(parser, PREC_UNARY, vm);

    switch (previousType) {
        case TOKEN_MINUS:
            emit_byte(parser, OP_NEGATE);
            break;
        case TOKEN_BANG:
            emit_byte(parser, OP_NOT);
            break;
        default:
            UNREACHABLE();
    }
}

static void binary(Parser* parser, Vm* vm, bool canAssign) {
    (void)canAssign;
    const TokenType previousType = parser->previous.type;

    const ParseRule* const rule = &rules[previousType];
    precedence(parser, rule->precedence + 1, vm);

    switch (previousType) {
        case TOKEN_PLUS:
            emit_byte(parser, OP_ADD);
            break;
        case TOKEN_MINUS:
            emit_byte(parser, OP_SUBTRACT);
            break;
        case TOKEN_STAR:
            emit_byte(parser, OP_MULTIPLY);
            break;
        case TOKEN_SLASH:
            emit_byte(parser, OP_DIVIDE);
            break;
        case TOKEN_EQUAL_EQUAL:
            emit_byte(parser, OP_EQUAL);
            break;
        case TOKEN_BANG_EQUAL:
            emit_byte(parser, OP_EQUAL);
            emit_byte(parser, OP_NOT);
            break;
        case TOKEN_GREATER:
            emit_byte(parser, OP_GREATER);
            break;
        case TOKEN_GREATER_EQUAL:
            emit_byte(parser, OP_LESS);
            emit_byte(parser, OP_NOT);
            break;
        case TOKEN_LESS:
            emit_byte(parser, OP_LESS);
            break;
        case TOKEN_LESS_EQUAL:
            emit_byte(parser, OP_GREATER);
            emit_byte(parser, OP_NOT);
            break;
        default:
            UNREACHABLE();
    }
}

static void expression(Parser* parser, Vm* vm) {
    precedence(parser, PREC_ASSIGNMENT, vm);
}

static void print_stmt(Parser* parser, Vm* vm) {
    expression(parser, vm);
    expect(parser, TOKEN_SEMICOLON,
           "Expected terminating semicolon after expression");
    emit_byte(parser, OP_PRINT);
}

static void expr_stmt(Parser* parser, Vm* vm) {
    expression(parser, vm);
    expect(parser, TOKEN_SEMICOLON,
           "Expected terminating semicolon after expression");
    emit_byte(parser, OP_POP);
}

static void begin_scope(Parser* parser) { parser->compiler->scope_depth += 1; }

static void block(Parser* parser, Vm* vm) {
    while (!peek(parser, TOKEN_EOF) && !peek(parser, TOKEN_RIGHT_BRACE)) {
        declaration(parser, vm);
    }

    expect(parser, TOKEN_RIGHT_BRACE, "Expected block closing `}`");
}

static void end_scope(Parser* parser) {
    parser->compiler->scope_depth -= 1;

    while (parser->compiler->locals_len > 0 &&
           parser->compiler->locals[parser->compiler->locals_len - 1].depth >
               parser->compiler->scope_depth) {
        emit_byte(parser, OP_POP);
        parser->compiler->locals_len -= 1;
    }
}

static intmax_t jump_emit(Parser* parser, uint8_t op) {
    emit_byte(parser, op);
    emit_byte(parser, UINT8_MAX);
    emit_byte(parser, UINT8_MAX);

    return (intmax_t)(buf_size(parser->chunk->opcodes) - 2);
}

static void jump_patch(Parser* parser, intmax_t offset) {
    const intmax_t jump =
        (intmax_t)buf_size(parser->chunk->opcodes) - offset - 2;
    assert(jump >= 0);

    if (jump > UINT16_MAX)
        error_str_nul(parser, &parser->previous,
                      "Reached jump limit for the `if` body");

    const uint16_t u16_jump = (uint16_t)jump;
    const uint8_t b1 = (u16_jump >> 8);
    const uint8_t b2 = (uint8_t)u16_jump;

    parser->chunk->opcodes[offset] = b1;
    parser->chunk->opcodes[offset + 1] = b2;
}

static void if_stmt(Parser* parser, Vm* vm) {
    expect(parser, TOKEN_LEFT_PAREN, "Expect `(` after `if`");
    expression(parser, vm);
    expect(parser, TOKEN_RIGHT_PAREN, "Expect `)` after `if`");

    const intmax_t then_jump = jump_emit(parser, OP_JUMP_IF_FALSE);
    emit_byte(parser, OP_POP);

    statement(parser, vm);
    const intmax_t else_jump = jump_emit(parser, OP_JUMP);

    jump_patch(parser, then_jump);

    emit_byte(parser, OP_POP);
    if (match(parser, TOKEN_ELSE)) statement(parser, vm);

    jump_patch(parser, else_jump);
}

static void and (Parser * parser, Vm* vm, bool canAssign) {
    (void)canAssign;

    const intmax_t end_jump = jump_emit(parser, OP_JUMP_IF_FALSE);
    emit_byte(parser, OP_POP);

    precedence(parser, PREC_AND, vm);

    jump_patch(parser, end_jump);
}

static void or (Parser * parser, Vm* vm, bool canAssign) {
    (void)canAssign;

    const intmax_t else_jump = jump_emit(parser, OP_JUMP_IF_FALSE);
    const intmax_t end_jump = jump_emit(parser, OP_JUMP);
    jump_patch(parser, else_jump);

    emit_byte(parser, OP_POP);

    precedence(parser, PREC_OR, vm);
    jump_patch(parser, end_jump);
}

static void emit_loop(Parser* parser, size_t loop_start) {
    emit_byte(parser, OP_LOOP);

    const intmax_t jump =
        (intmax_t)(buf_size(parser->chunk->opcodes) - loop_start + 2);
    assert(jump >= 0);

    if (jump > UINT16_MAX)
        error_str_nul(parser, &parser->previous,
                      "Reached jump limit for the for-loop body");

    const uint16_t u16_jump = (uint16_t)jump;
    const uint8_t b1 = (u16_jump >> 8);
    const uint8_t b2 = (uint8_t)u16_jump;

    emit_byte(parser, b1);
    emit_byte(parser, b2);
}

static void while_stmt(Parser* parser, Vm* vm) {
    const size_t loop_start = buf_size(parser->chunk->opcodes);

    expect(parser, TOKEN_LEFT_PAREN, "Expect `(` after `while`");
    expression(parser, vm);
    expect(parser, TOKEN_RIGHT_PAREN, "Expect `)` after `while`");

    const intmax_t exit_jump = jump_emit(parser, OP_JUMP_IF_FALSE);
    emit_byte(parser, OP_POP);

    statement(parser, vm);

    emit_loop(parser, loop_start);

    jump_patch(parser, exit_jump);
    emit_byte(parser, OP_POP);
}

static void for_stmt(Parser* parser, Vm* vm) {
    begin_scope(parser);
    expect(parser, TOKEN_LEFT_PAREN, "Expect `(` after `for`");

    if (match(parser, TOKEN_VAR))
        var_declaration(parser, vm);
    else if (match(parser, TOKEN_SEMICOLON)) {
    } else
        expr_stmt(parser, vm);

    size_t loop_start = buf_size(parser->chunk->opcodes);

    intmax_t exit_jump = -1;
    if (!match(parser, TOKEN_SEMICOLON)) {
        expression(parser, vm);
        expect(parser, TOKEN_SEMICOLON,
               "Expect `;` after for-loop stop condition");

        exit_jump = jump_emit(parser, OP_JUMP_IF_FALSE);
        emit_byte(parser, OP_POP);
    }

    if (!match(parser, TOKEN_RIGHT_PAREN)) {
        const intmax_t body_jump = jump_emit(parser, OP_JUMP);

        const size_t increment_start = buf_size(parser->chunk->opcodes);

        expression(parser, vm);
        emit_byte(parser, OP_POP);

        expect(parser, TOKEN_RIGHT_PAREN,
               "Expect `)` after for-loop stop condition");

        emit_loop(parser, loop_start);
        loop_start = increment_start;
        jump_patch(parser, body_jump);
    }

    statement(parser, vm);

    emit_loop(parser, loop_start);

    if (exit_jump != -1) {
        jump_patch(parser, exit_jump);
        emit_byte(parser, OP_POP);
    }

    end_scope(parser);
}

static void statement(Parser* parser, Vm* vm) {
    if (match(parser, TOKEN_PRINT)) {
        print_stmt(parser, vm);
    } else if (match(parser, TOKEN_LEFT_BRACE)) {
        begin_scope(parser);
        block(parser, vm);
        end_scope(parser);
    } else if (match(parser, TOKEN_IF)) {
        if_stmt(parser, vm);
    } else if (match(parser, TOKEN_WHILE)) {
        while_stmt(parser, vm);
    } else if (match(parser, TOKEN_FOR)) {
        for_stmt(parser, vm);
    } else {
        expr_stmt(parser, vm);
    }
}

static void sync(Parser* parser) {
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

        advance(parser);
    }
}

static void compiler_local_mark_initialized(Compiler* compiler) {
    compiler->locals[compiler->locals_len - 1].depth = compiler->scope_depth;
}

static void compiler_add_local(Parser* parser, const Token* name) {
    Compiler* const compiler = parser->compiler;

    if (compiler->locals_len == LOCALS_MAX - 1) {
        error_str_nul(parser, name,
                      "Reached the maximum number of locals: 256");
        return;
    }

    Local* const local = &compiler->locals[compiler->locals_len++];
    local->name = *name;
    local->depth = -1;
}

static void declare_variable(Parser* parser) {
    if (parser->compiler->scope_depth == 0) return;

    const Token* const name = &parser->previous;

    // Prevent shadowing in the same scope
    for (int i = parser->compiler->locals_len - 1; i >= 0; i--) {
        Local* const local = &parser->compiler->locals[i];
        LOG("local %.*s", (int)name->source_len, name->source);

        if (local->depth != -1 && local->depth < parser->compiler->scope_depth)
            break;

        if (str_eq(local->name.source, local->name.source_len, name->source,
                   name->source_len)) {
            error_str_nul(
                parser, name,
                "Variable shadowing: existing variable in the same scope");
            return;
        }
    }

    compiler_add_local(parser, name);
}

static uint8_t variable_name(Parser* parser, Vm* vm, const char err[]) {
    expect(parser, TOKEN_IDENTIFIER, err);

    declare_variable(parser);
    // Globals are resolved at runtime
    if (parser->compiler->scope_depth > 0) return 0;

    return make_identifier_constant(parser, vm);
}

static void define_variable(Parser* parser, uint8_t global_i) {
    // Skip locals
    if (parser->compiler->scope_depth > 0) {
        compiler_local_mark_initialized(parser->compiler);
        return;
    }
    emit_byte2(parser, OP_DEFINE_GLOBAL, global_i);
}

static void var_declaration(Parser* parser, Vm* vm) {
    const uint8_t global_i =
        variable_name(parser, vm, "Expected variable name");

    if (match(parser, TOKEN_EQUAL))
        expression(parser, vm);
    else
        emit_byte(parser, OP_NIL);

    expect(parser, TOKEN_SEMICOLON,
           "Expected semicolon after variable declaration");

    define_variable(parser, global_i);
}

static void declaration(Parser* parser, Vm* vm) {
    if (match(parser, TOKEN_VAR))
        var_declaration(parser, vm);
    else
        statement(parser, vm);

    if (parser->state == PARSER_STATE_PANIC_MODE) sync(parser);
}

Result parser_compile(const char* source, size_t source_len, Chunk* chunk,
                      Vm* vm) {
    LOG("source_len=%zu source=`%.*s`\n", source_len, (int)source_len, source);

    Compiler compiler = {.locals_len = 0, .scope_depth = 0};
    Parser parser = {.lex =
                         {
                             .source = source,
                             .source_len = source_len,
                             .line = 1,
                             .column = 1,
                             .pos = 0,
                         },
                     .chunk = chunk,
                     .compiler = &compiler};

    advance(&parser);

    while (!match(&parser, TOKEN_EOF)) {
        declaration(&parser, vm);
    }

    if (parser.state != PARSER_STATE_OK) return RES_PARSE_ERR;

    return RES_OK;
}

static void tok_dump(FILE* out, const Token* t) {
    fprintf(out, "%.*s", (int)t->source_len, t->source);
}

Result fmt(const char* source, size_t source_len) {
    Lex lex = {
        .line = 1, .column = 1, .source = source, .source_len = source_len};
    Token previous = {0};
    Token current = {0};

    FILE* const out = stdout;  // TODO: in-place

    while (true) {
        previous = current;
        lex_scan_token(&lex, &current);
        if (current.type == TOKEN_ERROR) return RES_PARSE_ERR;

        switch (current.type) {
            // Sometimes one space
            case TOKEN_LEFT_PAREN:
                switch (previous.type) {
                    case TOKEN_IF:
                    case TOKEN_WHILE:
                    case TOKEN_FOR:
                    case TOKEN_EQUAL:
                    case TOKEN_EQUAL_EQUAL:
                    case TOKEN_BANG_EQUAL:
                    case TOKEN_LESS:
                    case TOKEN_LESS_EQUAL:
                    case TOKEN_GREATER:
                    case TOKEN_GREATER_EQUAL:
                        fputs(" ", stdout);
                        break;
                    default:;
                }
                break;
            case TOKEN_RIGHT_PAREN:
                switch (previous.type) {
                    case TOKEN_NUMBER:
                    case TOKEN_IDENTIFIER:
                    case TOKEN_STRING:
                        break;
                    default:
                        fputs(" ", stdout);
                }
                break;
                // One space
            case TOKEN_OR:
            case TOKEN_AND:
            case TOKEN_EQUAL:
            case TOKEN_MINUS:
            case TOKEN_PLUS:
            case TOKEN_SLASH:
            case TOKEN_STAR:
                fputs(" ", stdout);
                break;
            // Sometimes one space
            case TOKEN_NUMBER:
            case TOKEN_IDENTIFIER:
            case TOKEN_STRING:
                switch (previous.type) {
                    case TOKEN_LEFT_PAREN:
                        break;
                    default:
                        fputs(" ", stdout);
                }
                break;
                // The end
            case TOKEN_EOF:
                fflush(out);
                return RES_OK;
                // No space
            case TOKEN_VAR:
            case TOKEN_SEMICOLON:
            case TOKEN_COMMA:
            case TOKEN_PRINT:
            case TOKEN_DOT:
            case TOKEN_BANG:
                break;
            default:
                fprintf(stderr, "Unknown token %s\n",
                        token_type_str[current.type]);
                UNREACHABLE();
        }

        tok_dump(out, &current);
    }
}
