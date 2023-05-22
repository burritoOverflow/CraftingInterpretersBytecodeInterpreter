#ifndef CLOX_VM_H
#define CLOX_VM_H

#include "object.h"
#include "table.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

// represents an ongoing function call
typedef struct {
    ObjFunction* function;  // function being executed
    uint8_t* ip;   // caller stores its own ip; on func return, VM jumps to `ip` of caller's
                   // `CallFrame` and resume from there
    Value* slots;  // points to the vm's stack at the first slot this function can use
} CallFrame;

typedef struct {
    CallFrame frames[FRAMES_MAX];
    int frameCount;          // store the current height of the callframe stack
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
