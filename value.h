
#pragma once
#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include "config.h"

typedef struct {
    size_t line;
    size_t column;
} Location;

typedef enum {
    TYPE_FUNCTION,
    TYPE_SCRIPT,
} FunctionType;

typedef enum {
    VAL_BOOL,
    VAL_NIL,
    VAL_NUMBER,
    VAL_OBJ,
} ValueType;

typedef enum {
    OBJ_STRING,
    OBJ_FUNCTION,
    OBJ_NATIVE,
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
    uint8_t* opcodes;
    Location* locations;
    Value* constants;
} Chunk;

typedef Value (*NativeFn)(Value* args, size_t arg_len);

typedef struct {
    Obj obj;
    NativeFn fn;
} ObjNative;

typedef struct {
    Obj obj;
    size_t len;
    char s[];
} ObjString;

typedef struct {
    Obj obj;
    uint8_t arity;
    Chunk chunk;
    size_t name_len;
    char name[];
} ObjFunction;

ObjFunction* obj_function_new(const char* name, size_t name_len);
#define BOOL_VAL(value) ((Value){.type = VAL_BOOL, {.boolean = value}})
#define NIL_VAL ((Value){.type = VAL_NIL, {.number = 0}})
#define NUMBER_VAL(value) ((Value){.type = VAL_NUMBER, {.number = value}})
#define OBJ_VAL(object) ((Value){.type = VAL_OBJ, {.obj = (Obj*)object}})

#define AS_BOOL(value) ((value).as.boolean)
#define AS_NUMBER(value) ((value).as.number)
#define AS_OBJ(value) ((value).as.obj)
#define AS_STRING(value) ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString*)AS_OBJ(value))->s)
#define AS_FN(value) ((ObjFunction*)AS_OBJ(value))
#define AS_NATIVE(value) ((ObjNative*)AS_OBJ(value))

#define IS_BOOL(value) ((value).type == VAL_BOOL)
#define IS_NIL(value) ((value).type == VAL_NIL)
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)
#define IS_OBJ(value) ((value).type == VAL_OBJ)
#define IS_STRING(value) value_obj_is_type(value, OBJ_STRING)
#define IS_FN(value) value_obj_is_type(value, OBJ_FUNCTION)
#define IS_NATIVE(value) value_obj_is_type(value, OBJ_NATIVE)

ObjString* value_make_string(Obj** objects, size_t s_len);
bool value_obj_is_type(Value v, ObjType type);
ObjString* value_obj_str_allocate(Obj** objects, size_t size);
bool value_eq(Value lhs, Value rhs);
bool value_is_falsy(const Value v);
const char* value_to_str(Value v);
const char* value_to_str_debug(Value v);
