#ifndef CLOX_VM_H
#define CLOX_VM_H

#include "object.h"
#include "table.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

// represents an ongoing function call
typedef struct {
    ObjClosure* closure;  // function being executed
    uint8_t* ip;          // caller stores its own ip; on func return, VM jumps to `ip` of caller's
                          // `CallFrame` and resume from there
    Value* slots;         // points to the vm's stack at the first slot this function can use
} CallFrame;

typedef struct {
    CallFrame frames[FRAMES_MAX];
    int frameCount;          // store the current height of the callframe stack
    Value stack[STACK_MAX];  // stack semantics are implemented
    Value* stackTop;  // points at the elem just past the elem containing the top value in the stack
    Table globals;    // storing global variables
    Table strings;    // for string interning (see 20.5)
    ObjUpvalue* openUpvalues;  // head pointer to open upvalues (those upvalues that still
                               // reside on a local variable still on the stack)
    Obj* objects;              // pointer to the head of the list of allocated objects
    size_t bytesAllocated;     // number of bytes of managed memory the VM has allocated
    size_t nextGC;             // threshold that triggers next garbage collection

    int grayCount;
    int grayCapacity;
    Obj** grayStack;  // worklist for keeping track of gray objects (see 26.4.1) (set of objects we
                      // know about but haven't processed yet)
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
