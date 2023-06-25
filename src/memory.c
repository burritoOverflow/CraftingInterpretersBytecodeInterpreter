#include <stdlib.h>

#include "compiler.h"
#include "memory.h"
#include "object.h"
#include "vm.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>
#endif

#define GC_HEAP_GROW_FACTOR 2

void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
    vm.bytesAllocated += newSize - oldSize;

    // "stress test" mode for garbage collector
    // when flag enabled, GC runs as often as possible (see 26.2.1)
    if (newSize > oldSize) {
#ifdef DEBUG_STRESS_GC
        collectGarbage();
#endif
    }

    if (vm.bytesAllocated > vm.nextGC) {
        collectGarbage();
    }

    // de-allocation is handled here as well
    if (newSize == 0) {
        free(pointer);
        return NULL;
    }

    // actual reallocation case
    void* result = realloc(pointer, newSize);
    if (result == NULL) {
        exit(1);
    }

    return result;
}

void markObject(Obj* object) {
    if (object == NULL) {
        return;
    }

    // graph need not be acyclic
    if (object->isMarked == true) {
        return;
    }

#ifdef DEBUG_LOG_GC
    printf("%p mark object ", (void*)object);
    printValue(OBJ_VAL(object));
    printf("\n");
#endif

    object->isMarked = true;

    // add to the worklist
    if (vm.grayCapacity < vm.grayCount + 1) {
        vm.grayCapacity = GROW_CAPACITY(vm.grayCapacity);

        // recall that this stack is not managed by the garbage collector so invoke `realloc`
        // instead of our implementation
        vm.grayStack = (Obj**)realloc(vm.grayStack, sizeof(Obj*) * vm.grayCapacity);

        // allocation failed; we'll just exit
        if (vm.grayStack == NULL) {
            exit(1);
        }
    }

    vm.grayStack[vm.grayCount++] = object;
}

// mark heap objects
void markValue(Value value) {
    if (IS_OBJ(value)) {
        markObject(AS_OBJ(value));
    }
}

static void markArray(ValueArray* valueArray) {
    for (int i = 0; i < valueArray->count; ++i) {
        markValue(valueArray->values[i]);
    }
}

static void blackenObject(Obj* object) {
#ifdef DEBUG_LOG_GC
    printf("%p blacken object ", (void*)object);
    printValue(OBJ_VAL(object));
    printf("\n");
#endif

    switch (object->type) {
        case OBJ_BOUND_METHOD: {
            ObjBoundMethod* boundMethod = (ObjBoundMethod*)object;
            markValue(boundMethod->receiver);
            markObject((Obj*)boundMethod->method);
            break;
        }

        case OBJ_CLASS: {
            ObjClass* klass = (ObjClass*)object;
            markObject((Obj*)klass->className);
            markTable(&klass->methods);
            break;
        }

        case OBJ_CLOSURE: {
            ObjClosure* closure = (ObjClosure*)object;
            markObject((Obj*)closure->function);

            for (int i = 0; i < closure->upvalueCount; ++i) {
                markObject((Obj*)closure->upvalues[i]);
            }

            break;
        }

        case OBJ_FUNCTION: {
            // first mark the function's name
            ObjFunction* function = (ObjFunction*)object;
            markObject((Obj*)function->functionName);

            // mark the references in the constant table
            markArray(&function->chunk.constants);
            break;
        }

        case OBJ_INSTANCE: {
            ObjInstance* instance = (ObjInstance*)object;
            markObject((Obj*)instance->klass);
            markTable(&instance->fields);
            break;
        }

        case OBJ_UPVALUE:
            markValue(((ObjUpvalue*)object)->closed);
            break;

        case OBJ_NATIVE:
        case OBJ_STRING:
            break;
    };
}

static void freeObject(Obj* object) {
#ifdef DEBUG_LOG_GC
    printf("%p free type %d\n", (void*)object, object->type);
#endif

    switch (object->type) {
        case OBJ_BOUND_METHOD: {
            FREE(ObjBoundMethod, object);
            break;
        }

        case OBJ_CLASS: {
            ObjClass* klass = (ObjClass*)object;
            freeTable(&klass->methods);
            FREE(ObjClass, object);
            break;
        }

        case OBJ_STRING: {
            ObjString* string = (ObjString*)object;
            FREE_ARRAY(char, string->chars, string->length + 1);
            FREE(ObjString, object);
            break;
        }

        case OBJ_FUNCTION: {
            // free the function itself
            ObjFunction* function = (ObjFunction*)object;
            // free the other memory owned by this function
            freeChunk(&function->chunk);
            FREE(OBJ_FUNCTION, object);
            break;
        }

        case OBJ_NATIVE:
            FREE(ObjNative, object);
            break;

        case OBJ_CLOSURE: {
            ObjClosure* closure = (ObjClosure*)object;
            // closure does not own the ObjUpvalue objects themselves, but does own the array of
            // pointers to those upvalues
            FREE_ARRAY(ObjUpvalue*, closure->upvalues, closure->upvalueCount);
            FREE(ObjClosure, object);
            break;
        }

        case OBJ_UPVALUE:
            // ObjUpvalue does not own the variable it references (only free the ObjUpvalue itself)
            FREE(ObjUpvalue, object);
            break;

        case OBJ_INSTANCE: {
            ObjInstance* instance = (ObjInstance*)object;
            freeTable(&instance->fields);
            FREE(ObjInstance, object);
            break;
        }
    }
}

// mark the locals or temporaries in the VM's stack
static void markRoots(void) {
    for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
        markValue(*slot);
    }

    // mark the vm's stack of CallFrames
    for (int i = 0; i < vm.frameCount; ++i) {
        markObject((Obj*)vm.frames[i].closure);
    }

    // mark the vm's list of open upvalues
    for (ObjUpvalue* upvalue = vm.openUpvalues; upvalue != NULL; upvalue = upvalue->next) {
        markObject((Obj*)upvalue);
    }

    markTable(&vm.globals);
    markCompilerRoots();
}

static void traceReferences(void) {
    while (vm.grayCount > 0) {
        Obj* object = vm.grayStack[--vm.grayCount];
        blackenObject(object);
    }
}

// free all unmarked objects, and mark the currently marked ones as unmarked
static void sweep(void) {
    Obj* previous = NULL;
    Obj* object = vm.objects;

    while (object != NULL) {
        // object is black, continue...
        if (object->isMarked) {
            // for the next collection we need every object to be white
            object->isMarked = false;
            previous = object;
            object = object->next;
        } else {
            // unlink from the list and free the object
            // (unreachable and thus can be claimed)
            Obj* unreached = object;
            object = object->next;

            // see diagram on 26.5 for clarification
            if (previous != NULL) {
                previous->next = object;
            } else {
                vm.objects = object;
            }

            freeObject(unreached);
        }
    }
}

void collectGarbage(void) {
#ifdef DEBUG_LOG_GC
    printf("-- gc begin\n");
    const size_t before = vm.bytesAllocated;
#endif

    markRoots();
    traceReferences();
    // remove references to unreachable strings before sweep is performed (26.5.1)
    tableRemoveWhite(&vm.strings);
    sweep();
    vm.nextGC = vm.bytesAllocated * GC_HEAP_GROW_FACTOR;

#ifdef DEBUG_LOG_GC
    printf("-- gc end\n");
    printf("   collected %zu bytes (from %zu to %zu) next at %zu\n", before - vm.bytesAllocated,
           before, vm.bytesAllocated, vm.nextGC);
#endif
}

// Traverse the VM's objects and delete each
void freeObjects(void) {
    Obj* object = vm.objects;
    while (object != NULL) {
        Obj* next = object->next;
        freeObject(object);
        object = next;
    }

    free(vm.grayStack);
}
