#pragma once
#include <stdint.h>

#include "value.h"

typedef struct {
    uint8_t* opcodes;
    Location* locations;
    Value* constants;
} Chunk;
