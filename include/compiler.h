#ifndef CLOX_COMPILER_H
#define CLOX_COMPILER_H

#include <stdbool.h>

#include "object.h"
#include "vm.h"

bool compile(const char* source, Chunk* chunk);

#endif
