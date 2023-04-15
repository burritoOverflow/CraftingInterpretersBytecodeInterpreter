#ifndef CLOX_CHUNK_H
#define CLOX_CHUNK_H

#include "common.h"

/*
    In this bytecode implementation, each instruction has a 1-byte
    operation code (opcode), corresponding to the type of instruction
    (add, subtract, multiply, etc.)
*/
typedef enum {
    OP_RETURN,
} OpCode;

// Dynamic array for the instructions
typedef struct {
    int count;
    int capacity;
    uint8_t* code;
} Chunk;

// create a new chunk
void initChunk(Chunk* chunk);

// allocate the Chunk
void writeChunk(Chunk* chunk, uint8_t byte);

// deallocate the chunk
void freeChunk(Chunk* chunk);

#endif