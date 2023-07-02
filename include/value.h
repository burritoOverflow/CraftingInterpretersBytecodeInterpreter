#ifndef CLOX_VALUE_H
#define CLOX_VALUE_H

#include <stdbool.h>

#include "common.h"

typedef struct Obj Obj;

typedef struct ObjString ObjString;

#ifdef NAN_BOXING
#include <string.h>

#define SIGN_BIT ((uint64_t)0x8000000000000000)

// see 'quiet NAN'
#define QNAN ((uint64_t)0x7ffc000000000000)

// two lowest bits of the mantissa are 'type tag'
#define TAG_NIL 1    // 01
#define TAG_FALSE 2  // 10
#define TAG_TRUE 3   // 11

typedef uint64_t Value;

// note the valid bit patterns above (30.3.4)
#define IS_BOOL(value) (((value) | 1) == TRUE_VAL)
#define IS_NIL(value) ((value) == NIL_VAL)
#define IS_NUMBER(value) (((value)&QNAN) != QNAN)
// check that both the sign and quiet Nan bits are set
#define IS_OBJ(value) (((value) & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT))

#define AS_BOOL(value) ((value) == TRUE_VAL)
#define AS_NUMBER(num) valueToNum(num)
// mask off the extra bits to get the Obj
#define AS_OBJ(value) ((Obj*)(uintptr_t)((value) & ~(SIGN_BIT | QNAN)))

#define BOOL_VAL(b) ((b) ? TRUE_VAL : FALSE_VAL)
#define FALSE_VAL ((Value)(uint64_t)(QNAN | TAG_FALSE))
#define TRUE_VAL ((Value)(uint64_t)(QNAN | TAG_TRUE))
// bitwise OR the quiet NAN bits and the type tag, cast to `Value`
#define NIL_VAL ((Value)(uint64_t)(QNAN | TAG_NIL))
#define NUMBER_VAL(num) numToValue(num)
// set all of the quiet NAN bits and the sign bit
#define OBJ_VAL(obj) (Value)(SIGN_BIT | QNAN | (uint64_t)(uintptr_t)(obj))

// see discussion around 'type punning' in section 30.3.3
static inline double valueToNum(Value value) {
    double num;
    memcpy(&num, &value, sizeof(Value));
    return num;
}

static inline Value numToValue(double num) {
    Value value;
    memcpy(&value, &num, sizeof(double));
    return value;
}

#else

typedef enum { VAL_BOOL, VAL_NIL, VAL_NUMBER, VAL_OBJ } ValueType;

typedef struct {
    ValueType type;
    union {
        bool boolean;
        double number;
        Obj* obj;
    } as;
} Value;

#define IS_BOOL(value) ((value).type == VAL_BOOL)
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)
#define IS_NIL(value) ((value).type == VAL_NIL)
#define IS_OBJ(value) ((value).type == VAL_OBJ)

// get the corresponding value from the union
#define AS_BOOL(value) ((value).as.boolean)
#define AS_NUMBER(value) ((value).as.number)
#define AS_OBJ(value) ((value).as.obj)

// set the corresponding value in the Value's union
#define BOOL_VAL(value) ((Value){VAL_BOOL, {.boolean = (value)}})
#define NIL_VAL ((Value){VAL_NIL, {.number = 0}})
#define NUMBER_VAL(value) ((Value){VAL_NUMBER, {.number = (value)}})
#define OBJ_VAL(object) ((Value){VAL_OBJ, {.obj = (Obj*)(object)}})

#endif  // NAN_BOXING

typedef struct {
    int capacity;
    int count;
    Value* values;
} ValueArray;

bool valuesEqual(Value a, Value b);

void initValueArray(ValueArray* array);

void writeValueArray(ValueArray* array, Value value);

void freeValueArray(ValueArray* array);

void printValue(Value value);

#endif
