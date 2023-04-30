#ifndef CLOX_OBJECT_H
#define CLOX_OBJECT_H

#include "common.h"
#include "value.h"

#define OBJ_TYPE(value) (AS_OBJ(value)->type)

#define IS_STRING(value) isObjType(value, OBJ_STRING)

#define AS_STRING(value) ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString*)AS_OBJ(value))->chars)

typedef enum {
    OBJ_STRING,
} ObjType;

struct Obj {
    ObjType type;      // designation for this object's type
    struct Obj* next;  // intrusive list stores a ptr to the next object
};

struct ObjString {
    Obj obj;        // strings contains the same state as Objects
    int length;     // the length of the string
    char* chars;    // the content of the string
    uint32_t hash;  // each object stores the hash code for its string
};

// Allocate a new `ObjString` and copy `chars` to its `chars` field
// does not take ownership of the `chars` provided
ObjString* copyString(const char* chars, int length);

// Allocate a new `ObjString` and set `chars` to the provided `chars`
// claims ownership of the `chars` provided
ObjString* takeString(char* chars, int length);

void printObject(Value value);

static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif
