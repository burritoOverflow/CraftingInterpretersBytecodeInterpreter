#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "compiler.h"
#include "debug.h"
#include "memory.h"
#include "object.h"
#include "vm.h"

VM vm;

// stack is empty when pointer is at the start of the array
void resetStack(void) {
    vm.stackTop = vm.stack;
}

// output a formatted error
static void runtimeError(const char* format, ...) {
    va_list args;
    va_start(args, format);

    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    const size_t instruction = vm.ip - vm.chunk->code - 1;

    // for reporting the line where the error occurred
    const int line = vm.chunk->lines[instruction];
    fprintf(stderr, "[line %d] in script\n", line);

    resetStack();
}

void initVm(void) {
    resetStack();
    vm.objects = NULL;
    initTable(&vm.strings);
}

void freeVm(void) {
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

static InterpretResult run(void) {
// read the byte currently pointed to by the IP, and advance IP
#define READ_BYTE() (*vm.ip++)

// reads next byte from bytecode and uses that as an
// index in the Value's constant table
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])

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
#ifdef DEBUG_TRACE_EXECUTION
        printf("        ");
        for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
            printf("[ ");
            printValue(*slot);
            printf(" ]");
        }
        printf("\n");
        disassembleInstruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
#endif

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

                push(NUMBER_VAL(AS_NUMBER(pop())));
                break;
            }

            case OP_RETURN: {
                printValue(pop());
                printf("\n");
                return INTERPRET_OK;
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

            case OP_EQUAL: {
                Value b = pop();
                Value a = pop();
                push(BOOL_VAL(valuesEqual(a, b)));
                break;

                case OP_GREATER:
                    BINARY_OP(BOOL_VAL, >);
                    break;

                case OP_LESS:
                    BINARY_OP(BOOL_VAL, <);
                    break;
            }
        }
    }

#undef READ_BYTE
#undef READ_CONSTANT
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
    Chunk chunk;
    initChunk(&chunk);

    // compiler will take the user's source and populate the chunk with
    // bytecode, if errors, discard the chunk
    if (!compile(source, &chunk)) {
        freeChunk(&chunk);
        return INTERPRET_COMPILE_ERROR;
    }

    vm.chunk = &chunk;
    vm.ip = vm.chunk->code;

    InterpretResult result = run();

    freeChunk(&chunk);
    return result;
}
