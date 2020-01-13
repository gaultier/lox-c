#include "value.h"

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#include "utils.h"

void value_print(FILE* out, Value v) {
    switch (v.type) {
        case VAL_BOOL:
            fprintf(out, "%s", v.as.boolean ? "true" : "false");
            break;
        case VAL_NIL:
            fprintf(out, "nil");
            break;
        case VAL_NUMBER:
            fprintf(out, "%f", v.as.number);
            break;
        case VAL_OBJ:
            switch (AS_OBJ(v)->type) {
                case OBJ_STRING:
                    fprintf(out, "%.*s", (int)AS_STRING(v)->len, AS_CSTRING(v));
                    break;
                default:
                    UNREACHABLE();
            }

            break;
        default:
            UNREACHABLE();
    }
}

bool value_is_falsy(Value v) {
    return IS_NIL(v) || (IS_BOOL(v) && !AS_BOOL(v));
}

bool value_eq(Value lhs, Value rhs) {
    if (lhs.type != rhs.type) return false;

    switch (lhs.type) {
        case VAL_BOOL:
            return AS_BOOL(lhs) == AS_BOOL(rhs);
        case VAL_NIL:
            return true;
        case VAL_NUMBER:
            return AS_NUMBER(lhs) == AS_NUMBER(rhs);
        case VAL_OBJ:
            if (AS_STRING(lhs)->len != AS_STRING(rhs)->len) return false;
            return memcmp(AS_STRING(lhs)->s, AS_STRING(rhs)->s,
                          AS_STRING(lhs)->len) == 0;

        default:
            UNREACHABLE();
    }
}

ObjString* value_obj_str_allocate(Obj** objects, size_t size) {
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

