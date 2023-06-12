#ifndef CLOX_TABLE_H
#define CLOX_TABLE_H

#include "common.h"
#include "value.h"

typedef struct {
    ObjString* key;
    Value value;
} Entry;

typedef struct {
    int count;
    int capacity;
    Entry* entries;
} Table;

void initTable(Table* table);

void freeTable(Table* table);

// Determine if a Value exists in the Table; if so set `value` to the retrieved value
bool tableGet(Table* table, ObjString* key, Value* value);

// add the provided `key` `val` pair to `table`; return true on success
bool tableSet(Table* table, ObjString* key, Value value);

// delete the corresponding key from the table' return true on success
bool tableDelete(Table* table, ObjString* key);

// copy all entries of one table to another
void tableAddAll(Table* src, Table* dest);

// locate an interned string in the table that matches the `chars` string provided
ObjString* tableFindString(Table* table, const char* chars, int length, uint32_t hash);

void tableRemoveWhite(Table* table);

void markTable(Table* table);

#endif
