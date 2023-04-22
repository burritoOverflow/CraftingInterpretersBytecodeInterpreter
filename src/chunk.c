#include <stdlib.h>

#include "chunk.h"
#include "memory.h"

// Start off completely empty
void initChunk(Chunk* chunk) {
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;
    chunk->lines = NULL;
    initValueArray(&chunk->constants);
}

void writeChunk(Chunk* chunk, uint8_t byte, int line) {
    // reallocate if necessary (additional chunk would reach capacity)
    if (chunk->capacity < chunk->count + 1) {
        const int oldCapacity = chunk->capacity;
        chunk->capacity = GROW_CAPACITY(oldCapacity);

        // allocate the arrays for the code and the line numbers
        chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
        chunk->lines = GROW_ARRAY(int, chunk->lines, oldCapacity, chunk->capacity);
    }

    chunk->code[chunk->count] = byte;
    chunk->lines[chunk->count] = line;
    chunk->count++;
}

// add a constant to the `chunk` and return the index where the constant was
// appended
int addConstant(Chunk* chunk, Value value) {
    writeValueArray(&chunk->constants, value);
    return chunk->constants.count - 1;
}

// Free the chunk's contents and set to a `zero state`
void freeChunk(Chunk* chunk) {
    FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
    // free the lines array
    FREE_ARRAY(int, chunk->lines, chunk->capacity);
    // set in a `null` state
    freeValueArray(&chunk->constants);
    initChunk(chunk);
}
