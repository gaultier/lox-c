
#pragma once
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>

#include "config.h"

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

ObjString* value_make_string(Obj** objects, size_t s_len);
bool value_obj_is_type(Value v, ObjType type);
ObjString* value_obj_str_allocate(Obj** objects, size_t size);
bool value_eq(Value lhs, Value rhs);
bool value_is_falsy(const Value* v);
void value_print(Value v);
void value_print_err(Value v);
