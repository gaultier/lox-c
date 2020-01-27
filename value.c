#include "value.h"

#include <float.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils.h"

bool value_is_falsy(const Value v) {
    return IS_NIL(v) || (IS_BOOL(v) && AS_BOOL(v) == false);
}

bool value_eq(Value lhs, Value rhs) {
    if (lhs.type != rhs.type) {
        return false;
    }

    switch (lhs.type) {
        case VAL_BOOL:
            return AS_BOOL(lhs) == AS_BOOL(rhs);
        case VAL_NIL:
            return true;
        case VAL_NUMBER:
            return fabs(AS_NUMBER(lhs) - AS_NUMBER(rhs)) < DBL_EPSILON;
        case VAL_OBJ:
            if (!IS_STRING(lhs) || !IS_STRING(rhs)) {
                return false;
            }

            if (AS_STRING(lhs)->len != AS_STRING(rhs)->len) {
                return false;
            }
            return memcmp(AS_STRING(lhs)->s, AS_STRING(rhs)->s,
                          AS_STRING(lhs)->len) == 0;
    }
}

static ObjString* value_obj_str_allocate(Obj** objects, size_t size) {
    ObjString* obj = NULL;
    REALLOC_SAFE(&obj, size);
    obj->obj.next = *objects;

    *objects = &obj->obj;

    return obj;
}

ObjString* value_make_string(Obj** objects, size_t s_len) {
    ObjString* os = value_obj_str_allocate(objects, sizeof(ObjString) + s_len);
    os->len = s_len;
    LOG("allocated string size=%zu\n", os->len);
    os->obj.type = OBJ_STRING;

    return os;
}

bool value_obj_is_type(Value v, ObjType type) {
    return IS_OBJ(v) && AS_OBJ(v)->type == type;
}

ObjFunction* value_make_fn(const char* name, size_t name_len) {
    ObjFunction* fn = NULL;
    REALLOC_SAFE(&fn, sizeof(ObjFunction) + name_len);
    fn->obj.type = OBJ_FUNCTION;
    fn->arity = 0;
    fn->chunk = (Chunk){0};
    memcpy(fn->name, name, name_len);
    fn->name_len = name_len;
    return fn;
}

ObjNative* obj_function_native_new(NativeFn fn) {
    ObjNative* n = NULL;
    REALLOC_SAFE(&n, sizeof(ObjNative));
    n->obj.type = OBJ_NATIVE;
    n->fn = fn;

    return n;
}

const char* value_to_str(Value v) {
    static char str[UINT8_MAX + 1] = "";
    memset(str, 0, UINT8_MAX + 1);

    switch (v.type) {
        case VAL_BOOL:
            snprintf(str, UINT8_MAX, "%s", v.as.boolean ? "true" : "false");
            break;
        case VAL_NIL:
            snprintf(str, UINT8_MAX, "nil");
            break;
        case VAL_NUMBER:
            snprintf(str, UINT8_MAX, "%g", v.as.number);
            break;
        case VAL_OBJ:
            switch (AS_OBJ(v)->type) {
                case OBJ_STRING:
                    snprintf(str, UINT8_MAX, "%.*s", (int)AS_STRING(v)->len,
                             AS_CSTRING(v));
                    break;
                case OBJ_FUNCTION:
                    snprintf(str, UINT8_MAX, "fn@%.*s", (int)AS_FN(v)->name_len,
                             AS_FN(v)->name);
                    break;
                case OBJ_NATIVE:
                    snprintf(str, UINT8_MAX, "fn@<native>");
                    break;
            }
            break;
    }
    str[UINT8_MAX] = '\0';
    return str;
}

const char* value_to_str_debug(Value v) {
    static char str[UINT8_MAX + 1] = "";
    memset(str, 0, UINT8_MAX + 1);

    switch (v.type) {
        case VAL_BOOL:
            snprintf(str, UINT8_MAX, "boolean (%s)",
                     v.as.boolean ? "true" : "false");
            break;
        case VAL_NIL:
            snprintf(str, UINT8_MAX, "nil");
            break;
        case VAL_NUMBER:
            snprintf(str, UINT8_MAX, "number (%g)", v.as.number);
            break;
        case VAL_OBJ:
            switch (AS_OBJ(v)->type) {
                case OBJ_STRING:
                    snprintf(str, UINT8_MAX, "string (\"%.*s\")",
                             (int)AS_STRING(v)->len, AS_CSTRING(v));
                    break;
                case OBJ_FUNCTION:
                    snprintf(str, UINT8_MAX, "function (fn@%.*s)",
                             (int)AS_FN(v)->name_len, AS_FN(v)->name);
                    break;
                case OBJ_NATIVE:
                    snprintf(str, UINT8_MAX, "fn@<native>");
                    break;
            }
            break;
    }
    return str;
}
