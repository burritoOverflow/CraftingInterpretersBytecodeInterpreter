#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "compiler.h"
#include "debug.h"
#include "memory.h"
#include "object.h"
#include "vm.h"

VM vm;

static Value clockNative(int argCount, Value* args) {
    return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

// stack is empty when pointer is at the start of the array
void resetStack(void) {
    vm.stackTop = vm.stack;
    vm.frameCount = 0;
    vm.openUpvalues = NULL;
}

// output a formatted error
static void runtimeError(const char* format, ...) {
    va_list args;
    va_start(args, format);

    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    // show the stack trace when runtime error occurs
    for (int i = vm.frameCount - 1; i >= 0; i--) {
        CallFrame* frame = &vm.frames[i];
        ObjFunction* function = frame->closure->function;
        size_t instruction = frame->ip - function->chunk.code - 1;

        fprintf(stderr, "[line %d] in ", function->chunk.lines[instruction]);

        if (function->functionName == NULL) {
            fprintf(stderr, "script\n");
        } else {
            fprintf(stderr, "%s()\n", function->functionName->chars);
        }
    }

    resetStack();
}

static void defineNative(const char* name, NativeFn nativeFn) {
    push(OBJ_VAL(copyString(name, (int)strlen(name))));
    push(OBJ_VAL(newNativeFunction(nativeFn)));
    tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);
    pop();
    pop();
}

// set the initial state of the vm's struct members
void initVm(void) {
    resetStack();
    vm.objects = NULL;
    initTable(&vm.globals);
    initTable(&vm.strings);

    defineNative("clock", clockNative);
}

// free the contents of the vm's struct members
void freeVm(void) {
    freeTable(&vm.globals);
    freeTable(&vm.strings);
    freeObjects();
}

void push(Value value) {
    // recall the pointer points to the location of the next value to be added
    *vm.stackTop = value;
    vm.stackTop++;
}

Value pop(void) {
    // as above recall the pointer's location is the _next_ available slot in the
    // array
    vm.stackTop--;
    return *vm.stackTop;
}

// Get a value from the vm's stack (but do not pop it)
// distance determines the depth of the stack from where a Value is retrieved
static Value peek(int distance) {
    return vm.stackTop[-1 - distance];
}

// initialize the next CallFrame on the stack
// store the pointer to the function being called
// and point the frame's ip to the function's code
// slots pointer points to it's "window" in the stack
static bool call(ObjClosure* closure, int argCount) {
    if (argCount != closure->function->arity) {
        runtimeError("Expected %d arguments; got %d arguments.", closure->function->arity,
                     argCount);
        return false;
    }

    if (vm.frameCount == FRAMES_MAX) {
        runtimeError("Stack overflow.");
        return false;
    }

    CallFrame* callFrame = &vm.frames[vm.frameCount++];
    callFrame->closure = closure;
    callFrame->ip = closure->function->chunk.code;
    callFrame->slots = vm.stackTop - argCount - 1;
    return true;
}

static bool callValue(Value callee, int argCount) {
    if (IS_OBJ(callee)) {
        switch (OBJ_TYPE(callee)) {
            case OBJ_CLOSURE:
                return call(AS_CLOSURE(callee), argCount);

            case OBJ_NATIVE:
                // TODO - determine a legitimate cause of this error; for now a workaround to
                // compile with clang
#ifdef __clang__
                vm.stackTop -= argCount + 1;
                push((((ObjNative*)(callee).as.obj)->function(argCount, vm.stackTop - argCount)));
#elif __GNUC__
                const NativeFn native = AS_NATIVE(callee);
                const Value result = native(argCount, vm.stackTop - argCount);
                vm.stackTop -= argCount + 1;
                push(result);
#endif
                return true;
            default:
                break;  // non-callable object type
        }
    }

    runtimeError("Can only call functions and classes.");
    return false;
}

static ObjUpvalue* captureUpvalue(Value* local) {
    ObjUpvalue* prevUpvalue = NULL;
    ObjUpvalue* upvalue = vm.openUpvalues;

    while (upvalue != NULL && upvalue->location > local) {
        prevUpvalue = upvalue;
        upvalue = upvalue->next;
    }

    if (upvalue != NULL && upvalue->location == local) {
        return upvalue;
    }

    ObjUpvalue* createdUpvalue = newUpvalue(local);
    createdUpvalue->next = upvalue;

    if (prevUpvalue == NULL) {
        vm.openUpvalues = createdUpvalue;
    } else {
        prevUpvalue->next = createdUpvalue;
    }

    return createdUpvalue;
}

// close (move to heap) all open upvalues (local variables currently on the stack) found that points
// to that slot or slots above it on the stack (see discussion on 25.4.4)
static void closeUpvalues(Value* last) {
    while (vm.openUpvalues != NULL && vm.openUpvalues->location >= last) {
        ObjUpvalue* upvalue = vm.openUpvalues;
        upvalue->closed = *upvalue->location;
        upvalue->location = &upvalue->closed;
        vm.openUpvalues = upvalue->next;
    }
}

static bool isFalsey(Value value) {
    // nil and false are falsey; every other value is true
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

// pop strings off stack and push the concatenated result
static void concatenate(void) {
    ObjString* b = AS_STRING(pop());
    ObjString* a = AS_STRING(pop());

    const int length = a->length + b->length;
    char* chars = ALLOCATE(char, length + 1);
    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';

    // characters are already allocated; allow `result` to take ownership
    ObjString* result = takeString(chars, length);
    push(OBJ_VAL(result));
}

// reads and executes a bytecode instruction
static InterpretResult run(void) {
    CallFrame* frame = &vm.frames[vm.frameCount - 1];

// read the byte currently pointed to by the IP, and advance IP
#define READ_BYTE() (*frame->ip++)

// reads next byte from bytecode and uses that as an
// index in the Value's constant table
#define READ_CONSTANT() (frame->closure->function->chunk.constants.values[READ_BYTE()])

// read a 16bit operand from the chunk (build unsigned int from chunk)
#define READ_SHORT() (frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))

#define READ_STRING() AS_STRING(READ_CONSTANT())

// order is critical; left operand gets pushed before the right operand
// perform type checking and conversion
#define BINARY_OP(valueType, op)                          \
    do {                                                  \
        if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) { \
            runtimeError("Operands must be numbers");     \
            return INTERPRET_RUNTIME_ERROR;               \
        }                                                 \
        const double b = AS_NUMBER(pop());                \
        const double a = AS_NUMBER(pop());                \
        push(valueType(a op b));                          \
    } while (false)

    for (;;) {
// disassemble and print each instruction before executing it
#ifdef DEBUG_TRACE_EXECUTION
        printf("        ");
        for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
            printf("[ ");
            printValue(*slot);
            printf(" ]");
        }
        printf("\n");

        // convert `ip` back to a relative offset from the start of the bytecode
        // as `disassembleInstruction` takes an integer byte offset
        disassembleInstruction(&frame->closure->function->chunk,
                               (int)(frame->ip - frame->closure->function->chunk.code));
#endif

        // each condition implements the opcode's behavior
        switch (READ_BYTE()) {
            case OP_ADD: {
                if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
                    concatenate();
                } else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
                    const double b = AS_NUMBER(pop());
                    const double a = AS_NUMBER(pop());
                    push(NUMBER_VAL(a + b));
                } else {
                    runtimeError("Operands must be two numbers or two strings.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }

            case OP_SUBTRACT: {
                BINARY_OP(NUMBER_VAL, -);
                break;
            }

            case OP_MULTIPLY: {
                BINARY_OP(NUMBER_VAL, *);
                break;
            }

            case OP_DIVIDE: {
                BINARY_OP(NUMBER_VAL, /);
                break;
            }

            case OP_NOT:
                push(BOOL_VAL(isFalsey(pop())));
                break;

            case OP_NEGATE: {
                if (!IS_NUMBER(peek(0))) {
                    runtimeError("Operand must be a number");
                    return INTERPRET_RUNTIME_ERROR;
                }

                // convert, negate, and push the number
                push(NUMBER_VAL(-AS_NUMBER(pop())));
                break;
            }

            case OP_PRINT: {
                printValue(pop());
                printf("\n");
                break;
            }

            case OP_JUMP: {
                uint16_t offset = READ_SHORT();
                frame->ip += offset;
                break;
            }

            case OP_JUMP_IF_FALSE: {
                const uint16_t offset = READ_SHORT();

                // apply the jump offset to the ip, if the expression evaluates to false
                if (isFalsey(peek(0)))
                    frame->ip += offset;

                break;
            }

            case OP_LOOP: {
                const uint16_t offset = READ_SHORT();
                frame->ip -= offset;
                break;
            }

            case OP_CLOSURE: {
                ObjFunction* function = AS_FUNCTION(READ_CONSTANT());
                ObjClosure* closure = newClosure(function);
                push(OBJ_VAL(closure));

                // iterate through each upvalue the closure expects
                for (int i = 0; i < closure->upvalueCount; ++i) {
                    // see discussion in section 25.3.1
                    const uint8_t isLocal = READ_BYTE();
                    const uint8_t index = READ_BYTE();

                    if (isLocal) {
                        // closes over a local variable in the enclosing function
                        closure->upvalues[i] = captureUpvalue(frame->slots + index);
                    } else {
                        // capture an upvalue from the surrounding function
                        closure->upvalues[i] = frame->closure->upvalues[index];
                    }
                }

                break;
            }

            case OP_CLOSE_UPVALUE:
                closeUpvalues(vm.stackTop - 1);
                pop();
                break;

            case OP_RETURN: {
                const Value result = pop();
                closeUpvalues(frame->slots);
                vm.frameCount--;

                if (vm.frameCount == 0) {
                    // top-level code is done; exit interpreter
                    pop();
                    return INTERPRET_OK;
                }

                // otherwise, stack ends up right at the start of the returning function's stack
                // window (return value ends up on stack at the new, lower location)
                vm.stackTop = frame->slots;
                push(result);
                frame = &vm.frames[vm.frameCount - 1];

                break;
            }

            case OP_CALL: {
                const int argCount = READ_BYTE();

                if (!callValue(peek(argCount), argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }

                // update the cached pointer to the current frame
                frame = &vm.frames[vm.frameCount - 1];
                break;
            }

            case OP_CONSTANT: {
                // "loads" a constant (pg. 276)
                const Value constant = READ_CONSTANT();
                push(constant);
                break;
            }

            case OP_NIL:
                push(NIL_VAL);
                break;

            case OP_TRUE:
                push(BOOL_VAL(true));
                break;

            case OP_FALSE:
                push(BOOL_VAL(false));
                break;

            // expression evaluates the expression and discards the result
            case OP_POP:
                pop();
                break;

            case OP_GET_LOCAL: {
                uint8_t slot = READ_BYTE();
                // load value from index and push on stack where later instructions can find it
                // access the current frame's slots array (given numbered slot relative to the start
                // of that frame)
                push(frame->slots[slot]);
                break;
            }

            case OP_GET_GLOBAL: {
                ObjString* name = READ_STRING();
                Value value;
                if (!tableGet(&vm.globals, name, &value)) {
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(value);
                break;
            }

            case OP_SET_LOCAL: {
                // take the assigned value from the top of the stack and assigns it to the stack
                // slot pertaining to the local variable
                uint8_t slot = READ_BYTE();
                frame->slots[slot] = peek(0);
                break;
            }

            case OP_DEFINE_GLOBAL: {
                ObjString* name = READ_STRING();
                tableSet(&vm.globals, name, peek(0));
                pop();
                break;
            }

            case OP_SET_GLOBAL: {
                ObjString* name = READ_STRING();
                // set the value in the table; if did not previously exist, this is an error (set
                // requires an existing variable)
                if (tableSet(&vm.globals, name, peek(0))) {
                    tableDelete(&vm.globals, name);
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_GET_UPVALUE: {
                // operand is the index into the current func's upvalue array
                uint8_t slot = READ_BYTE();

                // so look up the corresponding upvalue and read the value in its' slot
                push(*frame->closure->upvalues[slot]->location);
                break;
            }

            case OP_SET_UPVALUE: {
                uint8_t slot = READ_BYTE();
                *frame->closure->upvalues[slot]->location = peek(0);
                break;
            }

            case OP_EQUAL: {
                const Value b = pop();
                const Value a = pop();

                // push the result of an equality check
                push(BOOL_VAL(valuesEqual(a, b)));
                break;
            }

            case OP_GREATER:
                BINARY_OP(BOOL_VAL, >);
                break;

            case OP_LESS:
                BINARY_OP(BOOL_VAL, <);
                break;
        }
    }

#undef READ_BYTE
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_STRING
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
    ObjFunction* function = compile(source);

    // compile-time error that's already been reported
    if (function == NULL)
        return INTERPRET_COMPILE_ERROR;

    // store the function on the stack
    push(OBJ_VAL(function));
    ObjClosure* closure = newClosure(function);
    pop();
    push(OBJ_VAL(closure));
    call(closure, 0);

    return run();
}
