#ifndef CLOX_COMPILER_H
#define CLOX_COMPILER_H

#include <stdbool.h>

#include "object.h"
#include "vm.h"

ObjFunction* compile(const char* source);

void markCompilerRoots(void);
#endif
