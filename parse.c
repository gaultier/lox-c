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

static void parser_grouping(Parser*, Vm*, bool);
static void parser_unary(Parser*, Vm*, bool);
static void parser_binary(Parser*, Vm*, bool);
static void parser_number(Parser*, Vm*, bool);
static void parser_literal(Parser*, Vm*, bool);
static void parser_string(Parser*, Vm*, bool);
static void parser_variable(Parser*, Vm*, bool);
static void parser_expression(Parser* parser, Vm* vm);
static void parser_declaration(Parser* parser, Vm* vm);

static const ParseRule rules[TOKEN_COUNT] = {
    [TOKEN_LEFT_PAREN] = {.prefix = parser_grouping},
    [TOKEN_MINUS] = {.prefix = parser_unary,
                     .infix = parser_binary,
                     .precedence = PREC_TERM},
    [TOKEN_PLUS] = {.infix = parser_binary, .precedence = PREC_TERM},
    [TOKEN_SLASH] = {.infix = parser_binary, .precedence = PREC_FACTOR},
    [TOKEN_STAR] = {.infix = parser_binary, .precedence = PREC_FACTOR},
    [TOKEN_NUMBER] = {.prefix = parser_number},
    [TOKEN_NIL] = {.prefix = parser_literal},
    [TOKEN_TRUE] = {.prefix = parser_literal},
    [TOKEN_FALSE] = {.prefix = parser_literal},
    [TOKEN_BANG] = {.prefix = parser_unary},
    [TOKEN_BANG_EQUAL] = {.infix = parser_binary, .precedence = PREC_EQUALITY},
    [TOKEN_EQUAL_EQUAL] = {.infix = parser_binary, .precedence = PREC_EQUALITY},
    [TOKEN_GREATER] = {.infix = parser_binary, .precedence = PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {.infix = parser_binary,
                             .precedence = PREC_COMPARISON},
    [TOKEN_LESS] = {.infix = parser_binary, .precedence = PREC_COMPARISON},
    [TOKEN_LESS_EQUAL] = {.infix = parser_binary,
                          .precedence = PREC_COMPARISON},
    [TOKEN_STRING] = {.prefix = parser_string},
    [TOKEN_IDENTIFIER] = {.prefix = parser_variable},
};

static void parser_error(Parser* parser, const Token* token, const char* err,
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

static void parser_emit_byte(Parser* parser, uint8_t byte) {
    LOG("byte=%d opcode=%s\n", byte, opcode_str[byte]);
    buf_push(parser->chunk->lines, parser->current.line);
    buf_push(parser->chunk->opcodes, byte);
}

static void parser_emit_byte2(Parser* parser, uint8_t byte1, uint8_t byte2) {
    LOG("opcode1=%s byte2=%d\n", opcode_str[byte1], byte2);
    buf_push(parser->chunk->lines, parser->current.line);
    buf_push(parser->chunk->opcodes, byte1);
    buf_push(parser->chunk->lines, parser->current.line);
    buf_push(parser->chunk->opcodes, byte2);
}

static void parser_advance(Parser* parser) {
    parser->previous = parser->current;

    while (true) {
        lex_scan_token(&parser->lex, &parser->current);
        if (parser->current.type != TOKEN_ERROR) return;

        parser_error(parser, &parser->current, parser->current.source,
                     parser->current.source_len);
    }
}

static bool parser_peek(Parser* parser, TokenType type) {
    return (parser->current.type == type);
}

static bool parser_match(Parser* parser, TokenType type) {
    if (!parser_peek(parser, type)) return false;

    parser_advance(parser);
    return true;
}

static uint8_t parser_make_constant(Parser* parser, Value v) {
    buf_push(parser->chunk->constants, v);
    const size_t constant_i = buf_size(parser->chunk->constants) - 1;

    return constant_i;
}

static uint8_t parser_make_identifier_constant(Parser* parser, Vm* vm) {
    ObjString* const os =
        value_make_string(&vm->objects, parser->previous.source_len);
    memcpy(os->s, parser->previous.source, os->len);

    return parser_make_constant(parser, OBJ_VAL(os));
}

static void parser_precedence(Parser* parser, Precedence precedence, Vm* vm) {
    LOG("precedence=%s previous_type=%s current_type=%s\n",
        precedence_str[precedence], token_type_str[parser->previous.type],
        token_type_str[parser->current.type]);
    parser_advance(parser);
    LOG("precedence=%s previous_type=%s current_type=%s\n",
        precedence_str[precedence], token_type_str[parser->previous.type],
        token_type_str[parser->current.type]);

    const ParseFn prefix_rule = rules[parser->previous.type].prefix;
    if (!prefix_rule) {
        parser_error(parser, &parser->previous, "Expected expression", 19);
        return;
    }

    const bool canAssign = precedence <= PREC_ASSIGNMENT;
    LOG("precedence=%d canAssign=%d previous_type=%d\n", precedence, canAssign,
        parser->previous.type);
    prefix_rule(parser, vm, canAssign);
    LOG("precedence=%d canAssign=%d\n", precedence, canAssign);

    while (!parser_peek(parser, TOKEN_EOF) &&
           (precedence <= rules[parser->current.type].precedence)) {
        LOG("precedence=%d\n", precedence);
        parser_advance(parser);

        const ParseFn infix_rule = rules[parser->previous.type].infix;

        infix_rule(parser, vm, canAssign);
    }

    if (canAssign && parser_peek(parser, TOKEN_EQUAL)) {
        parser_error(parser, &parser->previous, "Expression cannot be assigned",
                     29);
        return;
    }
}

static void parser_expect(Parser* parser, TokenType type, const char err[]) {
    if (parser->current.type == type) {
        parser_advance(parser);
        return;
    }

    parser_error(parser, &parser->current, err, strlen(err));
}

static void parser_number(Parser* parser, Vm* vm, bool canAssign) {
    (void)canAssign;
    (void)vm;
    assert(parser->previous.type = TOKEN_NUMBER);

    const double number = strtod(parser->previous.source, NULL);
    const Value v = NUMBER_VAL(number);

    parser_emit_byte2(parser, OP_CONSTANT, parser_make_constant(parser, v));
}

static void parser_string(Parser* parser, Vm* vm, bool canAssign) {
    (void)canAssign;
    assert(parser->previous.type = TOKEN_STRING);

    ObjString* const os =
        value_make_string(&vm->objects, parser->previous.source_len);
    memcpy(os->s, parser->previous.source, os->len);
    const Value v = OBJ_VAL(os);

    buf_push(parser->chunk->constants, v);
    parser_emit_byte2(parser, OP_CONSTANT,
                      buf_size(parser->chunk->constants) - 1);
}

static int parser_resolve_local(Parser* parser, const Token* name) {
    for (int i = parser->compiler->locals_len - 1; i >= 0; i--) {
        const Local* const l = &parser->compiler->locals[i];
        if (str_eq(name->source, name->source_len, l->name.source,
                   l->name.source_len)) {
            if (l->depth == -1)
                parser_error(
                    parser, &parser->previous,
                    "Cannot read local variable in its own initializer", 50);

            return i;
        }
    }
    return -1;
}

static void parser_named_variable(Parser* parser, Vm* vm, bool canAssign) {
    const Token* const name = &parser->previous;

    int arg = parser_resolve_local(parser, name);
    const bool is_local = arg >= 0;
    LOG("arg=%d is_local=%d name=`%.*s`\n", arg, is_local,
        (int)name->source_len, name->source);

    if (!is_local) arg = parser_make_identifier_constant(parser, vm);

    if (canAssign && parser_match(parser, TOKEN_EQUAL)) {
        parser_expression(parser, vm);
        parser_emit_byte2(parser, is_local ? OP_SET_LOCAL : OP_SET_GLOBAL, arg);
    } else
        parser_emit_byte2(parser, is_local ? OP_GET_LOCAL : OP_GET_GLOBAL, arg);
}

static void parser_variable(Parser* parser, Vm* vm, bool canAssign) {
    parser_named_variable(parser, vm, canAssign);
}

static void parser_literal(Parser* parser, Vm* vm, bool canAssign) {
    (void)canAssign;
    (void)vm;

    switch (parser->previous.type) {
        case TOKEN_NIL:
            parser_emit_byte(parser, OP_NIL);
            break;
        case TOKEN_TRUE:
            parser_emit_byte(parser, OP_TRUE);
            break;
        case TOKEN_FALSE:
            parser_emit_byte(parser, OP_FALSE);
            break;
        default:
            UNREACHABLE();
    }
}

static void parser_grouping(Parser* parser, Vm* vm, bool canAssign) {
    (void)canAssign;
    parser_expression(parser, vm);
    parser_expect(parser, TOKEN_RIGHT_PAREN, "Expected `)` after expression");
}

static void parser_unary(Parser* parser, Vm* vm, bool canAssign) {
    (void)canAssign;
    const TokenType previousType = parser->previous.type;

    parser_precedence(parser, PREC_UNARY, vm);

    switch (previousType) {
        case TOKEN_MINUS:
            parser_emit_byte(parser, OP_NEGATE);
            break;
        case TOKEN_BANG:
            parser_emit_byte(parser, OP_NOT);
            break;
        default:
            UNREACHABLE();
    }
}

static void parser_binary(Parser* parser, Vm* vm, bool canAssign) {
    (void)canAssign;
    const TokenType previousType = parser->previous.type;

    const ParseRule* const rule = &rules[previousType];
    parser_precedence(parser, rule->precedence + 1, vm);

    switch (previousType) {
        case TOKEN_PLUS:
            parser_emit_byte(parser, OP_ADD);
            break;
        case TOKEN_MINUS:
            parser_emit_byte(parser, OP_SUBTRACT);
            break;
        case TOKEN_STAR:
            parser_emit_byte(parser, OP_MULTIPLY);
            break;
        case TOKEN_SLASH:
            parser_emit_byte(parser, OP_DIVIDE);
            break;
        case TOKEN_EQUAL_EQUAL:
            parser_emit_byte(parser, OP_EQUAL);
            break;
        case TOKEN_BANG_EQUAL:
            parser_emit_byte(parser, OP_EQUAL);
            parser_emit_byte(parser, OP_NOT);
            break;
        case TOKEN_GREATER:
            parser_emit_byte(parser, OP_GREATER);
            break;
        case TOKEN_GREATER_EQUAL:
            parser_emit_byte(parser, OP_LESS);
            parser_emit_byte(parser, OP_NOT);
            break;
        case TOKEN_LESS:
            parser_emit_byte(parser, OP_LESS);
            break;
        case TOKEN_LESS_EQUAL:
            parser_emit_byte(parser, OP_GREATER);
            parser_emit_byte(parser, OP_NOT);
            break;
        default:
            UNREACHABLE();
    }
}

static void parser_expression(Parser* parser, Vm* vm) {
    parser_precedence(parser, PREC_ASSIGNMENT, vm);
}

static void parser_print_stmt(Parser* parser, Vm* vm) {
    parser_expression(parser, vm);
    parser_expect(parser, TOKEN_SEMICOLON,
                  "Expected terminating semicolon after expression");
    parser_emit_byte(parser, OP_PRINT);
}

static void parser_expr_stmt(Parser* parser, Vm* vm) {
    parser_expression(parser, vm);
    parser_expect(parser, TOKEN_SEMICOLON,
                  "Expected terminating semicolon after expression");
    parser_emit_byte(parser, OP_POP);
}

static void parser_begin_scope(Parser* parser) {
    parser->compiler->scope_depth += 1;
}

static void parser_block(Parser* parser, Vm* vm) {
    while (!parser_peek(parser, TOKEN_EOF) &&
           !parser_peek(parser, TOKEN_RIGHT_BRACE)) {
        parser_declaration(parser, vm);
    }

    parser_expect(parser, TOKEN_RIGHT_BRACE, "Expected block closing `}`");
}

static void parser_end_scope(Parser* parser) {
    parser->compiler->scope_depth -= 1;

    while (parser->compiler->locals_len > 0 &&
           parser->compiler->locals[parser->compiler->locals_len - 1].depth >
               parser->compiler->scope_depth) {
        parser_emit_byte(parser, OP_POP);
        parser->compiler->locals_len -= 1;
    }
}

static void parser_statement(Parser* parser, Vm* vm) {
    if (parser_match(parser, TOKEN_PRINT)) {
        parser_print_stmt(parser, vm);
    } else if (parser_match(parser, TOKEN_LEFT_BRACE)) {
        parser_begin_scope(parser);
        parser_block(parser, vm);
        parser_end_scope(parser);
    } else {
        parser_expr_stmt(parser, vm);
    }
}

static void parser_sync(Parser* parser) {
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

        parser_advance(parser);
    }
}

static void compiler_local_mark_initialized(Compiler* compiler) {
    compiler->locals[compiler->locals_len - 1].depth = compiler->scope_depth;
}

static void compiler_add_local(Parser* parser, const Token* name) {
    Compiler* const compiler = parser->compiler;

    if (compiler->locals_len == LOCALS_MAX - 1) {
        parser_error(parser, name, "Reached the maximum number of locals: 256",
                     42);
        return;
    }

    Local* const local = &compiler->locals[compiler->locals_len++];
    local->name = *name;
    local->depth = -1;
}

static void parser_declare_variable(Parser* parser) {
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
            parser_error(
                parser, name,
                "Variable shadowing: existing variable in the same scope", 56);
            return;
        }
    }

    compiler_add_local(parser, name);
}

static uint8_t parser_variable_name(Parser* parser, Vm* vm, const char err[]) {
    parser_expect(parser, TOKEN_IDENTIFIER, err);

    parser_declare_variable(parser);
    // Globals are resolved at runtime
    if (parser->compiler->scope_depth > 0) return 0;

    return parser_make_identifier_constant(parser, vm);
}

static void parser_define_variable(Parser* parser, uint8_t global_i) {
    // Skip locals
    if (parser->compiler->scope_depth > 0) {
        compiler_local_mark_initialized(parser->compiler);
        return;
    }
    parser_emit_byte2(parser, OP_DEFINE_GLOBAL, global_i);
}

static void parser_var_declaration(Parser* parser, Vm* vm) {
    const uint8_t global_i =
        parser_variable_name(parser, vm, "Expected variable name");

    if (parser_match(parser, TOKEN_EQUAL))
        parser_expression(parser, vm);
    else
        parser_emit_byte(parser, OP_NIL);

    parser_expect(parser, TOKEN_SEMICOLON,
                  "Expected semicolon after variable declaration");

    parser_define_variable(parser, global_i);
}

static void parser_declaration(Parser* parser, Vm* vm) {
    if (parser_match(parser, TOKEN_VAR))
        parser_var_declaration(parser, vm);
    else
        parser_statement(parser, vm);

    if (parser->state == PARSER_STATE_PANIC_MODE) parser_sync(parser);
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

    parser_advance(&parser);

    while (!parser_match(&parser, TOKEN_EOF)) {
        parser_declaration(&parser, vm);
    }

    if (parser.state != PARSER_STATE_OK) return RES_PARSE_ERR;

    return RES_OK;
}

