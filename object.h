#pragma once
#include "chunk.h"
#include "value.h"

typedef struct {
    Obj obj;
    uint8_t arity;
    Chunk chunk;
    size_t name_len;
    char name[];
} ObjFunction;
