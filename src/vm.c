#include <stdio.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "vm.h"

VM vm;

// stack is empty when pointer is at the start of the array
void resetStack() {
    vm.stackTop = vm.stack;
}

void initVm() {
    resetStack();
}

void freeVm() {}

void push(Value value) {
    // recall the pointer points to the location of the next value to be added
    *vm.stackTop = value;
    vm.stackTop++;
}

Value pop() {
    // as above recall the pointer's location is the _next_ available slot in the array
    vm.stackTop--;
    return *vm.stackTop;
}

static InterpretResult run() {
// read the byte currently pointed to by the IP, and advance IP
#define READ_BYTE() (*vm.ip++)

// reads next byte from bytecode and uses that as an
// index in the Value's constant table
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])

// pre-processor macro magic; pass the operator as argument
// order is critical; left operand gets pushed before the right operand
#define BINARY_OP(op)     \
    do {                  \
        double b = pop(); \
        double a = pop(); \
        push(a op b);     \
    } while (false);

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
                BINARY_OP(+);
                break;
            }

            case OP_SUBTRACT: {
                BINARY_OP(-);
                break;
            }

            case OP_MULTIPLY: {
                BINARY_OP(*);
                break;
            }

            case OP_DIVIDE: {
                BINARY_OP(/);
                break;
            }

            case OP_NEGATE: {
                // add to the stack the negatated current value
                push(-pop());
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
    compile(source);
    return INTERPRET_OK;
}
