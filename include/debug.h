#ifndef CLOX_DEBUG_H
#define CLOX_DEBUG_H

#include "chunk.h"

void disassembleChunk(Chunk* chunk, const char* name);

// disassemble the current instuction and return the offset to the next instruction
int disassembleInstruction(Chunk* chunk, int offset);

#endif
