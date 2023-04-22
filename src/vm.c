#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
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

    // for reporting the line where the error occured
    const int line = vm.chunk->lines[instruction];
    fprintf(stderr, "[line %d] in script\n", line);

    resetStack();
}

void initVm(void) {
    resetStack();
}

void freeVm(void) {}

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
// distance determines the depth of the stack from where an Value is retrieved
static Value peek(int distance) {
    return vm.stackTop[-1 - distance];
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

        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
            case OP_ADD: {
                BINARY_OP(NUMBER_VAL, +);
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
