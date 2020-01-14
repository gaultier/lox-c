#include "parse.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "buf.h"
#include "lex.h"
#include "utils.h"

typedef void (*ParseFn)(Parser*, Vm*, bool);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

static void parse_grouping(Parser*, Vm*, bool);
static void parse_unary(Parser*, Vm*, bool);
static void parse_binary(Parser*, Vm*, bool);
static void parse_number(Parser*, Vm*, bool);
static void parse_literal(Parser*, Vm*, bool);
static void parse_string(Parser*, Vm*, bool);
static void parse_variable(Parser*, Vm*, bool);
static void parse_expression(Parser* parser, Vm* vm);
static void parse_declaration(Parser* parser, Vm* vm);

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

static void parse_error(Parser* parser, const Token* token, const char* err,
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

static void parse_emit_byte(Parser* parser, uint8_t byte) {
    LOG("byte=%d opcode=%s\n", byte, opcode_str[byte]);
    buf_push(parser->chunk->lines, parser->current.line);
    buf_push(parser->chunk->opcodes, byte);
}

static void parse_emit_byte2(Parser* parser, uint8_t byte1, uint8_t byte2) {
    LOG("opcode1=%s byte2=%d\n", opcode_str[byte1], byte2);
    buf_push(parser->chunk->lines, parser->current.line);
    buf_push(parser->chunk->opcodes, byte1);
    buf_push(parser->chunk->lines, parser->current.line);
    buf_push(parser->chunk->opcodes, byte2);
}
static void parse_advance(Parser* parser) {
    parser->previous = parser->current;

    while (true) {
        lex_scan_token(&parser->lex, &parser->current);
        if (parser->current.type != TOKEN_ERROR) return;

        parse_error(parser, &parser->current, parser->current.source,
                    parser->current.source_len);
    }
}

static bool parse_peek(Parser* parser, TokenType type) {
    return (parser->current.type == type);
}

static bool parse_match(Parser* parser, TokenType type) {
    if (!parse_peek(parser, type)) return false;

    parse_advance(parser);
    return true;
}

static uint8_t parse_make_constant(Parser* parser, Value v) {
    buf_push(parser->chunk->constants, v);
    const size_t constant_i = buf_size(parser->chunk->constants) - 1;

    return constant_i;
}

static uint8_t parse_make_identifier_constant(Parser* parser, Vm* vm) {
    ObjString* const os =
        value_make_string(&vm->objects, parser->previous.source_len);
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
        parse_error(parser, &parser->previous, "Expected expression", 19);
        return;
    }

    const bool canAssign = precedence <= PREC_ASSIGNMENT;
    LOG("precedence=%d PREC_ASSIGNMENT=%d canAssign=%d\n", precedence,
        PREC_ASSIGNMENT, canAssign);
    prefix_rule(parser, vm, canAssign);

    while (precedence <= rules[parser->current.type].precedence) {
        parse_advance(parser);

        const ParseFn infix_rule = rules[parser->previous.type].infix;

        infix_rule(parser, vm, canAssign);
    }

    if (canAssign && parse_peek(parser, TOKEN_EQUAL)) {
        parse_error(parser, &parser->previous, "Expression cannot be assigned",
                    29);
        return;
    }
}

static void parse_expect(Parser* parser, TokenType type, const char err[]) {
    if (parser->current.type == type) {
        parse_advance(parser);
        return;
    }

    parse_error(parser, &parser->current, err, strlen(err));
}

static void parse_number(Parser* parser, Vm* vm, bool canAssign) {
    (void)canAssign;
    (void)vm;
    assert(parser->previous.type = TOKEN_NUMBER);

    const double number = strtod(parser->previous.source, NULL);
    const Value v = NUMBER_VAL(number);

    parse_emit_byte2(parser, OP_CONSTANT, parse_make_constant(parser, v));
}

static void parse_string(Parser* parser, Vm* vm, bool canAssign) {
    (void)canAssign;
    assert(parser->previous.type = TOKEN_STRING);

    ObjString* const os =
        value_make_string(&vm->objects, parser->previous.source_len);
    memcpy(os->s, parser->previous.source, os->len);
    const Value v = OBJ_VAL(os);

    buf_push(parser->chunk->constants, v);
    parse_emit_byte2(parser, OP_CONSTANT,
                     buf_size(parser->chunk->constants) - 1);
}

static void parse_named_variable(Parser* parser, Vm* vm, bool canAssign) {
    uint8_t arg = parse_make_identifier_constant(parser, vm);

    LOG("canAssign=%d\n", canAssign);
    if (canAssign && parse_match(parser, TOKEN_EQUAL)) {
        parse_expression(parser, vm);
        LOG("set global%s\n", "");
        parse_emit_byte2(parser, OP_SET_GLOBAL, arg);
    } else
        parse_emit_byte2(parser, OP_GET_GLOBAL, arg);
}

static void parse_variable(Parser* parser, Vm* vm, bool canAssign) {
    parse_named_variable(parser, vm, canAssign);
}

static void parse_literal(Parser* parser, Vm* vm, bool canAssign) {
    (void)canAssign;
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

static void parse_grouping(Parser* parser, Vm* vm, bool canAssign) {
    (void)canAssign;
    parse_expression(parser, vm);
    parse_expect(parser, TOKEN_RIGHT_PAREN, "Expected `)` after expression");
}

static void parse_unary(Parser* parser, Vm* vm, bool canAssign) {
    (void)canAssign;
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

static void parse_binary(Parser* parser, Vm* vm, bool canAssign) {
    (void)canAssign;
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
            parse_emit_byte(parser, OP_LESS);
            parse_emit_byte(parser, OP_NOT);
            break;
        case TOKEN_LESS:
            parse_emit_byte(parser, OP_LESS);
            break;
        case TOKEN_LESS_EQUAL:
            parse_emit_byte(parser, OP_GREATER);
            parse_emit_byte(parser, OP_NOT);
            break;
        default:
            UNREACHABLE();
    }
}

static void parse_expression(Parser* parser, Vm* vm) {
    parse_precedence(parser, PREC_ASSIGNMENT, vm);
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

static void parse_begin_scope(Parser* parser) {
    parser->compiler->scope_depth += 1;
}
static void parse_block(Parser* parser, Vm* vm) {
    while (!parse_peek(parser, TOKEN_EOF) &&
           !parse_peek(parser, TOKEN_RIGHT_BRACE)) {
        parse_declaration(parser, vm);
    }

    parse_expect(parser, TOKEN_RIGHT_BRACE, "Expected block closing `}`");
}

static void parse_end_scope(Parser* parser) {
    parser->compiler->scope_depth -= 1;
}

static void parse_statement(Parser* parser, Vm* vm) {
    if (parse_match(parser, TOKEN_PRINT)) {
        parse_print_stmt(parser, vm);
    } else if (parse_match(parser, TOKEN_LEFT_BRACE)) {
        parse_begin_scope(parser);
        parse_block(parser, vm);
        parse_end_scope(parser);
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

static void compiler_add_local(Parser* parser, const Token* name) {
    Compiler* const compiler = parser->compiler;

    if (compiler->locals_len == LOCALS_MAX - 1) {
        parse_error(parser, name, "Reached the maximum number of locals: 256",
                    42);
        return;
    }

    Local* const local = &compiler->locals[compiler->locals_len++];
    local->name = *name;
    local->depth = compiler->scope_depth;
}

static void parse_declare_variable(Parser* parser) {
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
            parse_error(
                parser, name,
                "Variable shadowing: existing variable in the same scope", 56);
            return;
        }
    }

    compiler_add_local(parser, name);
}

static uint8_t parse_variable_name(Parser* parser, Vm* vm, const char err[]) {
    parse_expect(parser, TOKEN_IDENTIFIER, err);

    parse_declare_variable(parser);
    if (parser->compiler->scope_depth > 0) return 0;

    return parse_make_identifier_constant(parser, vm);
}

static void parse_define_variable(Parser* parser, uint8_t global_i) {
    parse_emit_byte2(parser, OP_DEFINE_GLOBAL, global_i);
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

Result parse_compile(const char* source, size_t source_len, Chunk* chunk,
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

    parse_advance(&parser);

    while (!parse_match(&parser, TOKEN_EOF)) {
        parse_declaration(&parser, vm);
    }

    if (parser.state != PARSER_STATE_OK) return RES_PARSE_ERR;

    return RES_OK;
}

