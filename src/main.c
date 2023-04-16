#include "chunk.h"
#include "common.h"
#include "debug.h"
#include "vm.h"

int main(int argc, const char* argv[]) {
    initVm();

    Chunk chunk;
    initChunk(&chunk);

    const int constant = addConstant(&chunk, 1.2);
    // first write the constant
    writeChunk(&chunk, OP_CONSTANT, 123);
    // then write the constant's index operand
    writeChunk(&chunk, constant, 123);
    writeChunk(&chunk, OP_NEGATE, 123);
    writeChunk(&chunk, OP_RETURN, 123);
    disassembleChunk(&chunk, "test chunk");
    interpret(&chunk);
    freeVm();
    freeChunk(&chunk);

    return 0;
}
