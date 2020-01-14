#pragma once
#include <ctype.h>
#include <stddef.h>
#include <stdint.h>

#include "config.h"

typedef struct {
    const char* source;
    size_t source_len;
    size_t line;
    size_t column;
    size_t pos;
} Lex;

typedef enum {
    // Single-character tokens.
    TOKEN_LEFT_PAREN = 1,
    TOKEN_RIGHT_PAREN,
    TOKEN_LEFT_BRACE,
    TOKEN_RIGHT_BRACE,
    TOKEN_COMMA,
    TOKEN_DOT,
    TOKEN_MINUS,
    TOKEN_PLUS,
    TOKEN_SEMICOLON,
    TOKEN_SLASH,
    TOKEN_STAR,

    // One or two character tokens.
    TOKEN_BANG,
    TOKEN_BANG_EQUAL,
    TOKEN_EQUAL,
    TOKEN_EQUAL_EQUAL,
    TOKEN_GREATER,
    TOKEN_GREATER_EQUAL,
    TOKEN_LESS,
    TOKEN_LESS_EQUAL,

    // Literals.
    TOKEN_IDENTIFIER,
    TOKEN_STRING,
    TOKEN_NUMBER,

    // Keywords.
    TOKEN_AND,
    TOKEN_CLASS,
    TOKEN_ELSE,
    TOKEN_FALSE,
    TOKEN_FOR,
    TOKEN_FUN,
    TOKEN_IF,
    TOKEN_NIL,
    TOKEN_OR,
    TOKEN_PRINT,
    TOKEN_RETURN,
    TOKEN_SUPER,
    TOKEN_THIS,
    TOKEN_TRUE,
    TOKEN_VAR,
    TOKEN_WHILE,

    TOKEN_ERROR,
    TOKEN_EOF,
    TOKEN_COUNT,
} TokenType;

#ifdef WITH_LOGS
static const char token_type_str[TOKEN_COUNT + 1][20] = {
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
#endif

typedef struct {
    size_t line;
    size_t column;
    const char* source;
    size_t source_len;
    TokenType type;
} Token;

void lex_scan_token(Lex* lex, Token* token);
