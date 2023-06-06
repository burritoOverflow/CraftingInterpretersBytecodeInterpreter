#include <stdlib.h>

#include "memory.h"
#include "object.h"
#include "vm.h"

void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
    // deallocation is handled here as well
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

static void freeObject(Obj* object) {
    switch (object->type) {
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
    }
}

// Traverse the VM's objects and delete each
void freeObjects(void) {
    Obj* object = vm.objects;
    while (object != NULL) {
        Obj* next = object->next;
        freeObject(object);
        object = next;
    }
}
