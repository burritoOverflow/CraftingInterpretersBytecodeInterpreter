#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "value.h"
#include "vm.h"

// Allocate an `Obj` and cast to the `type *` provided
#define ALLOCATE_OBJ(type, objectType) (type*)allocateObject(sizeof(type), objectType)

static Obj* allocateObject(size_t size, ObjType type) {
    Obj* object = (Obj*)reallocate(NULL, 0, size);
    object->type = type;
    object->isMarked = false;

    object->next = vm.objects;
    vm.objects = object;

#ifdef DEBUG_LOG_GC
    printf("%p allocate %zu for type %d\n", (void*)object, size, type);
#endif

    return object;
}

ObjClosure* newClosure(ObjFunction* function) {
    ObjUpvalue** upvalues = ALLOCATE(ObjUpvalue*, function->upvalueCount);
    for (int i = 0; i < function->upvalueCount; i++) {
        upvalues[i] = NULL;
    }

    ObjClosure* closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
    closure->function = function;
    closure->upvalues = upvalues;
    closure->upvalueCount = function->upvalueCount;

    return closure;
}

ObjFunction* newFunction(void) {
    ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
    function->arity = 0;
    function->upvalueCount = 0;
    function->functionName = NULL;
    initChunk(&function->chunk);
    return function;
}

ObjInstance* newInstance(ObjClass* klass) {
    ObjInstance* instance = ALLOCATE_OBJ(ObjInstance, OBJ_INSTANCE);
    instance->klass = klass;
    // each instance contains a table for the instance's fields.
    initTable(&instance->fields);
    return instance;
}

ObjNative* newNativeFunction(NativeFn nativeFn) {
    ObjNative* native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
    native->function = nativeFn;
    return native;
}

// allocate the `ObjString` object; precondition: `chars` have already been allocated
static ObjString* allocateObjString(char* chars, int length, uint32_t hash) {
    ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
    string->length = length;
    string->chars = chars;
    string->hash = hash;

    // see 26.7.2
    push(OBJ_VAL(string));

    // whenever a new (unique) string is created, add to the vm's strings table
    tableSet(&vm.strings, string, NIL_VAL);
    pop();

    return string;
}

ObjBoundMethod* newBoundMethod(Value receiver, ObjClosure* method) {
    ObjBoundMethod* boundMethod = ALLOCATE_OBJ(ObjBoundMethod, OBJ_BOUND_METHOD);
    boundMethod->receiver = receiver;
    boundMethod->method = method;
    return boundMethod;
}

ObjClass* newClass(ObjString* className) {
    ObjClass* klass = ALLOCATE_OBJ(ObjClass, OBJ_CLASS);
    initTable(&klass->methods);
    klass->className = className;
    return klass;
}

// FNV-1a hash: http://www.isthe.com/chongo/tech/comp/fnv/
static uint32_t hashString(const char* key, int length) {
    uint32_t hash = 2166136261u;
    for (int i = 0; i < length; ++i) {
        hash ^= (uint8_t)key[i];
        hash *= 16777619;
    }
    return hash;
}

// creates a new `ObjString` using the chars provided
// (newly allocated object owns the provided `chars`)
ObjString* takeString(char* chars, int length) {
    uint32_t hash = hashString(chars, length);
    ObjString* interned = tableFindString(&vm.strings, chars, length, hash);

    // if present, free the mem for the provided string
    if (interned != NULL) {
        FREE_ARRAY(char, chars, length + 1);
        return interned;
    }

    return allocateObjString(chars, length, hash);
}

// Allocate memory for an `ObjString`'s `chars` and create and return a newly constructed
// `ObjString` containing `chars` in the corresponding member
// used when cannot obtain ownership of `chars` provided
ObjString* copyString(const char* chars, int length) {
    uint32_t hash = hashString(chars, length);

    // if the string exists in the table, return a reference to the existing string
    ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
    if (interned != NULL)
        return interned;

    // otherwise, allocate length of `chars` and copy the contents to allocated mem
    char* heapChars = ALLOCATE(char, length + 1);
    memcpy(heapChars, chars, length);
    heapChars[length] = '\0';

    // create a  new `ObjString` with the newly allocated and assigned`chars` as the `chars` member
    return allocateObjString(heapChars, length, hash);
}

ObjUpvalue* newUpvalue(Value* slot) {
    ObjUpvalue* upvalue = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);
    upvalue->closed = NIL_VAL;
    upvalue->location = slot;
    upvalue->next = NULL;
    return upvalue;
}

static void printFunction(ObjFunction* function) {
    // top-level function
    if (function->functionName == NULL) {
        printf("<script>");
        return;
    }

    printf("<fn %s>", function->functionName->chars);
}

void printObject(Value value) {
    switch (OBJ_TYPE(value)) {
        case OBJ_BOUND_METHOD: {
            printFunction(AS_BOUND_METHOD(value)->method->function);
            break;
        }

        case OBJ_STRING:
            printf("%s", AS_CSTRING(value));
            break;

        case OBJ_FUNCTION:
            printFunction(AS_FUNCTION(value));
            break;

        case OBJ_INSTANCE:
            printf("%s instance", AS_INSTANCE(value)->klass->className->chars);
            break;

        case OBJ_NATIVE:
            printf("<native fn>");
            break;

        case OBJ_CLASS:
            printf("%s", AS_CLASS(value)->className->chars);
            break;

        case OBJ_CLOSURE:
            printFunction(AS_CLOSURE(value)->function);
            break;

        case OBJ_UPVALUE:
            printf("upvalue");
            break;
    }
}
