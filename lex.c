#include "lex.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "utils.h"

const char token_type_str[TOKEN_COUNT + 1][20] = {
    [TOKEN_LEFT_PAREN] = "TOKEN_LEFT_PAREN",
    [TOKEN_RIGHT_PAREN] = "TOKEN_RIGHT_PAREN",
    [TOKEN_LEFT_BRACE] = "TOKEN_LEFT_BRACE",
    [TOKEN_RIGHT_BRACE] = "TOKEN_RIGHT_BRACE",
    [TOKEN_COMMA] = "TOKEN_COMMA",
    [TOKEN_DOT] = "TOKEN_DOT",
    [TOKEN_MINUS] = "TOKEN_MINUS",
    [TOKEN_PLUS] = "TOKEN_PLUS",
    [TOKEN_SEMICOLON] = "TOKEN_SEMICOLON",
    [TOKEN_SLASH] = "TOKEN_SLASH",
    [TOKEN_STAR] = "TOKEN_STAR",
    [TOKEN_BANG] = "TOKEN_BANG",
    [TOKEN_BANG_EQUAL] = "TOKEN_BANG_EQUAL",
    [TOKEN_EQUAL] = "TOKEN_EQUAL",
    [TOKEN_EQUAL_EQUAL] = "TOKEN_EQUAL_EQUAL",
    [TOKEN_GREATER] = "TOKEN_GREATER",
    [TOKEN_GREATER_EQUAL] = "TOKEN_GREATER_EQUAL",
    [TOKEN_LESS] = "TOKEN_LESS",
    [TOKEN_LESS_EQUAL] = "TOKEN_LESS_EQUAL",
    [TOKEN_IDENTIFIER] = "TOKEN_IDENTIFIER",
    [TOKEN_STRING] = "TOKEN_STRING",
    [TOKEN_NUMBER] = "TOKEN_NUMBER",
    [TOKEN_AND] = "TOKEN_AND",
    [TOKEN_CLASS] = "TOKEN_CLASS",
    [TOKEN_ELSE] = "TOKEN_ELSE",
    [TOKEN_FALSE] = "TOKEN_FALSE",
    [TOKEN_FOR] = "TOKEN_FOR",
    [TOKEN_FUN] = "TOKEN_FUN",
    [TOKEN_IF] = "TOKEN_IF",
    [TOKEN_NIL] = "TOKEN_NIL",
    [TOKEN_OR] = "TOKEN_OR",
    [TOKEN_PRINT] = "TOKEN_PRINT",
    [TOKEN_RETURN] = "TOKEN_RETURN",
    [TOKEN_SUPER] = "TOKEN_SUPER",
    [TOKEN_THIS] = "TOKEN_THIS",
    [TOKEN_TRUE] = "TOKEN_TRUE",
    [TOKEN_VAR] = "TOKEN_VAR",
    [TOKEN_WHILE] = "TOKEN_WHILE",
    [TOKEN_ERROR] = "TOKEN_ERROR",
    [TOKEN_EOF] = "TOKEN_EOF",
};

static void init_token(const Lex* lex, Token* token, TokenType type,
                       const Lex* start_lex) {
    token->line = start_lex->line;
    token->column = start_lex->column;
    token->type = type;
    token->source = &lex->source[start_lex->pos];
    token->source_len = lex->pos - start_lex->pos;
    LOG("type=%s line=%zu column=%zu source=`%.*s` source_len=%zu\n",
        token_type_str[type], token->line, token->column,
        (int)token->source_len, token->source, token->source_len);
}

static void init_token_err(const Lex* lex, Token* token, const char err[]) {
    token->line = lex->line;
    token->column = lex->column;
    token->type = TOKEN_ERROR;
    token->source = err;
    token->source_len = strlen(err);
}

static char advance(Lex* lex) {
    lex->pos += 1;
    lex->column += 1;
    return lex->source[lex->pos - 1];
}

static char peek(const Lex* lex) { return lex->source[lex->pos]; }

static bool is_at_end(const Lex* lex) { return lex->pos == lex->source_len; }

static char peek_next(const Lex* lex) {
    return (char)(is_at_end(lex) ? '\0' : lex->source[lex->pos + 1]);
}

static void skip_until_char(Lex* lex, char c) {
    while (!is_at_end(lex) && peek(lex) != c) {
        advance(lex);
    }
}

static bool match(Lex* lex, char c) {
    if (is_at_end(lex)) {
        return false;
    }

    if (lex->source[lex->pos] != c) {
        return false;
    }

    advance(lex);
    return true;
}

static void newline(Lex* lex) {
    lex->pos += 1;
    lex->column = 1;
    lex->line += 1;
}

static void skip_whitespace(Lex* lex) {
    while (!is_at_end(lex)) {
        const char c = peek(lex);
        switch (c) {
            case ' ':
            case '\t':
            case '\r':
                advance(lex);
                break;
            case '\n':
                newline(lex);
                break;
            case '/':
                if (peek_next(lex) == '/') {
                    skip_until_char(lex, '\n');
                } else {
                    return;
                }
                break;
            default:
                return;
        }
    }
}

static void string(Lex* lex, Token* token) {
    const Lex start_lex = *lex;
    while (!is_at_end(lex) && peek(lex) != '"') {
        if (peek(lex) == '\n') {
            newline(lex);
        } else {
            advance(lex);
        }
    }

    if (is_at_end(lex)) {
        init_token_err(lex, token, "Unterminated string");
        return;
    }
    init_token(lex, token, TOKEN_STRING, &start_lex);

    // Consume closing quote
    advance(lex);
}

static void number(Lex* lex, Token* token, const Lex* start_lex) {
    while (!is_at_end(lex) && isdigit(peek(lex))) {
        advance(lex);
    }

    if (!is_at_end(lex) && peek(lex) == '.' && isdigit(peek_next(lex))) {
        // Consume the dot
        advance(lex);

        while (!is_at_end(lex) && isdigit(peek(lex))) {
            advance(lex);
        }
    }

    init_token(lex, token, TOKEN_NUMBER, start_lex);
}

static TokenType identifier_type(const char* s, size_t s_len) {
    assert(s_len >= 1);

    switch (s[0]) {
        case 'a':
            if (str_eq("nd", 2, s + 1, s_len - 1)) {
                return TOKEN_AND;
            } else {
                break;
            }
        case 'c':
            if (str_eq("lass", 4, s + 1, s_len - 1)) {
                return TOKEN_CLASS;
            } else {
                break;
            }
        case 'e':
            if (str_eq("lse", 3, s + 1, s_len - 1)) {
                return TOKEN_ELSE;
            } else {
                break;
            }
        case 'f':
            if (str_eq("alse", 4, s + 1, s_len - 1)) {
                return TOKEN_FALSE;
            } else if (str_eq("or", 2, s + 1, s_len - 1)) {
                return TOKEN_FOR;
            } else if (str_eq("un", 2, s + 1, s_len - 1)) {
                return TOKEN_FUN;
            } else {
                break;
            }
        case 'i':
            if (str_eq("f", 1, s + 1, s_len - 1)) {
                return TOKEN_IF;
            } else {
                break;
            }
        case 'n':
            if (str_eq("il", 2, s + 1, s_len - 1)) {
                return TOKEN_NIL;
            } else {
                break;
            }
        case 'o':
            if (str_eq("r", 1, s + 1, s_len - 1)) {
                return TOKEN_OR;
            } else {
                break;
            }
        case 'p':
            if (str_eq("rint", 4, s + 1, s_len - 1)) {
                return TOKEN_PRINT;
            } else {
                break;
            }
        case 'r':
            if (str_eq("eturn", 5, s + 1, s_len - 1)) {
                return TOKEN_RETURN;
            } else {
                break;
            }
        case 's':
            if (str_eq("uper", 4, s + 1, s_len - 1)) {
                return TOKEN_SUPER;
            } else {
                break;
            }
        case 't':
            if (str_eq("his", 3, s + 1, s_len - 1)) {
                return TOKEN_THIS;
            } else if (str_eq("rue", 3, s + 1, s_len - 1)) {
                return TOKEN_TRUE;
            } else {
                break;
            }
        case 'v':
            if (str_eq("ar", 2, s + 1, s_len - 1)) {
                return TOKEN_VAR;
            } else {
                break;
            }
        case 'w':
            if (str_eq("hile", 4, s + 1, s_len - 1)) {
                return TOKEN_WHILE;
            } else {
                break;
            }
        default:
            break;
    }

    return TOKEN_IDENTIFIER;
}

static void identifier(Lex* lex, Token* token, const Lex* start_lex) {
    while (!is_at_end(lex) && isalnum(peek(lex))) {
        advance(lex);
    }

    const char* s = &start_lex->source[start_lex->pos];
    const size_t s_len = lex->pos - start_lex->pos;

    init_token(lex, token, identifier_type(s, s_len), start_lex);
}

void lex_scan_token(Lex* lex, Token* token) {
    skip_whitespace(lex);
    const Lex start_lex = *lex;

    if (is_at_end(lex)) {
        init_token(lex, token, TOKEN_EOF, &start_lex);
        return;
    }

    const char c = advance(lex);

    if (isdigit(c)) {
        number(lex, token, &start_lex);
        return;
    }
    if (isalnum(c)) {
        identifier(lex, token, &start_lex);
        return;
    }

    switch (c) {
        case '{':
            init_token(lex, token, TOKEN_LEFT_BRACE, &start_lex);
            return;
        case '}':
            init_token(lex, token, TOKEN_RIGHT_BRACE, &start_lex);
            return;
        case '(':
            init_token(lex, token, TOKEN_LEFT_PAREN, &start_lex);
            return;
        case ')':
            init_token(lex, token, TOKEN_RIGHT_PAREN, &start_lex);
            return;
        case ';':
            init_token(lex, token, TOKEN_SEMICOLON, &start_lex);
            return;
        case ',':
            init_token(lex, token, TOKEN_COMMA, &start_lex);
            return;
        case '.':
            init_token(lex, token, TOKEN_DOT, &start_lex);
            return;
        case '-':
            init_token(lex, token, TOKEN_MINUS, &start_lex);
            return;
        case '+':
            init_token(lex, token, TOKEN_PLUS, &start_lex);
            return;
        case '*':
            init_token(lex, token, TOKEN_STAR, &start_lex);
            return;
        case '/':
            init_token(lex, token, TOKEN_SLASH, &start_lex);
            return;
        case '!':
            init_token(lex, token,
                       match(lex, '=') ? TOKEN_BANG_EQUAL : TOKEN_BANG,
                       &start_lex);
            return;
        case '=':
            init_token(lex, token,
                       match(lex, '=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL,
                       &start_lex);
            return;
        case '<':
            init_token(lex, token,
                       match(lex, '=') ? TOKEN_LESS_EQUAL : TOKEN_LESS,
                       &start_lex);
            return;
        case '>':
            init_token(lex, token,
                       match(lex, '=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER,
                       &start_lex);
            return;
        case '"':
            string(lex, token);
            return;
        default: {
            char* err = NULL;
            REALLOC_SAFE(&err, 19);
            snprintf(err, 19, "Unknown token `%c`", c);
            init_token_err(lex, token, err);
        }
    }
}
