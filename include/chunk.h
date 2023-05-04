#ifndef CLOX_CHUNK_H
#define CLOX_CHUNK_H

#include "common.h"
#include "value.h"
/*
    In this bytecode implementation, each instruction has a 1-byte
    operation code (opcode), corresponding to the type of instruction
    (add, subtract, multiply, etc.)
*/
typedef enum {
    OP_CONSTANT,
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_POP,
    OP_GET_GLOBAL,
    OP_DEFINE_GLOBAL,
    OP_SET_GLOBAL,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_NOT,
    OP_NEGATE,
    OP_PRINT,
    OP_RETURN,
} OpCode;

// Dynamic array for the instructions
typedef struct {
    int count;
    int capacity;
    uint8_t* code;
    int* lines;  // store the line numbers in indices corresponding to the instructions
    ValueArray constants;
} Chunk;

// create a new chunk
void initChunk(Chunk* chunk);

// allocate the Chunk
void writeChunk(Chunk* chunk, uint8_t byte, int line);

// deallocate the chunk
void freeChunk(Chunk* chunk);

// add a constant to the chunk and return the index of the appended constant
int addConstant(Chunk* chunk, Value constant);

#endif
