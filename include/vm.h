#ifndef CLOX_VM_H
#define CLOX_VM_H

#include "chunk.h"
#include "table.h"

#define STACK_MAX 256

typedef struct {
    Chunk* chunk;
    uint8_t* ip;             // instruction pointer
    Value stack[STACK_MAX];  // stack semantics are implemented
    Value* stackTop;  // points at the elem just past the elem containing the top value in the stack
    Table globals;    // storing global variables
    Table strings;    // for string interning
    Obj* objects;     // pointer to the head of the list of allocated objects
} VM;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
} InterpretResult;

// expose from module as `object` uses it directly
extern VM vm;

void initVm(void);

void freeVm(void);

InterpretResult interpret(const char* source);

void push(Value Value);

Value pop(void);

#endif
