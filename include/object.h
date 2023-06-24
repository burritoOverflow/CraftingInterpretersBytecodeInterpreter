#ifndef CLOX_OBJECT_H
#define CLOX_OBJECT_H

#include "chunk.h"
#include "common.h"
#include "table.h"
#include "value.h"

#define OBJ_TYPE(value) (AS_OBJ(value)->type)

#define IS_CLASS(value) isObjType(value, OBJ_CLASS)
#define IS_CLOSURE(value) isObjType(value, OBJ_CLOSURE)
#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)
#define IS_INSTANCE(value) isObjType(value, OBJ_INSTANCE)
#define IS_NATIVE(value) isObjType(value, OBJ_NATIVE)
#define IS_STRING(value) isObjType(value, OBJ_STRING)

#define AS_CLASS(value) ((ObjClass*)AS_OBJ(value))
#define AS_CLOSURE(value) ((ObjClosure*)AS_OBJ(value))
#define AS_FUNCTION(value) ((ObjFunction*)AS_OBJ(value))
#define AS_INSTANCE(value) ((ObjInstance*)AS_OBJ(value))
#define AS_NATIVE(value) (((ObjNative*)AS_OBJ(value))->function)
#define AS_STRING(value) ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString*)AS_OBJ(value))->chars)

typedef enum {
    OBJ_CLASS,
    OBJ_STRING,
    OBJ_FUNCTION,
    OBJ_INSTANCE,
    OBJ_NATIVE,
    OBJ_CLOSURE,
    OBJ_UPVALUE
} ObjType;

struct Obj {
    ObjType type;  // designation for this object's type
    bool isMarked;
    struct Obj* next;  // intrusive list stores a ptr to the next object
};

typedef struct {
    Obj obj;    // functions are first-class, so they need to be Lox objects
    int arity;  // number of parameters the function expects
    int upvalueCount;
    Chunk chunk;              // each function has its own chunk
    ObjString* functionName;  // the name of the function
} ObjFunction;

typedef Value (*NativeFn)(int argCount, Value* args);

typedef struct {
    Obj obj;
    NativeFn function;
} ObjNative;

struct ObjString {
    Obj obj;        // strings contains the same state as Objects
    int length;     // the length of the string
    char* chars;    // the content of the string
    uint32_t hash;  // each object stores the hash code for its string
};

typedef struct ObjUpvalue {
    Obj obj;
    Value* location;  // reference to a variable, not the value itself (see 25.3)
    struct ObjUpvalue* next;
    Value closed;  // closed over variable moves into this field (where upvalues live on the heap)
} ObjUpvalue;

typedef struct {
    Obj obj;
    ObjFunction* function;
    ObjUpvalue** upvalues;  // each closure has an array of upvalues
    int upvalueCount;       // the number of upvalues for this closure (elements in the above array)
} ObjClosure;

typedef struct {
    Obj obj;
    ObjString* className;
    Table methods;
} ObjClass;

typedef struct {
    Obj obj;
    ObjClass* klass;
    Table fields;
} ObjInstance;

ObjClass* newClass(ObjString* className);

ObjClosure* newClosure(ObjFunction* function);

// create a new Lox function
ObjFunction* newFunction(void);

ObjInstance* newInstance(ObjClass* klass);

ObjNative* newNativeFunction(NativeFn nativeFn);

// Allocate a new `ObjString` and copy `chars` to its `chars` field
// does not take ownership of the `chars` provided
ObjString* copyString(const char* chars, int length);

ObjUpvalue* newUpvalue(Value* slot);

// Allocate a new `ObjString` and set `chars` to the provided `chars`
// claims ownership of the `chars` provided
ObjString* takeString(char* chars, int length);

void printObject(Value value);

static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif
