#include <string.h>

#include "memory.h"
#include "table.h"
#include "value.h"

#define TABLE_MAX_CAPACITY 0.75

// set the initialized table to an "empty" state
void initTable(Table* table) {
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

// free the Entries in the table and set the table to an 'empty' state
void freeTable(Table* table) {
    FREE_ARRAY(Entry, table->entries, table->capacity);
    initTable(table);
}

// given a key and an array of buckets, determine which bucket the entry belongs in, and return the
// corresponding Entry
static Entry* findEntry(Entry* entries, int capacity, ObjString* key) {
    // map the key's hash code to an index in the array
    uint32_t index = key->hash % capacity;
    Entry* tombstone = NULL;

    // eventually find an empty bucket
    for (;;) {
        Entry* entry = &entries[index];

        if (entry->key == NULL) {
            if (IS_NIL(entry->value)) {
                // empty entry
                return tombstone != NULL ? tombstone : entry;
            } else {
                // tombstone found (value == true)
                if (tombstone == NULL)
                    tombstone = entry;
            }
        } else if (entry->key == key) {
            return entry;
        }

        // perform the linear probing, as the bucket has a different key
        index = (index + 1) % capacity;
    }
}

bool tableGet(Table* table, ObjString* key, Value* value) {
    if (table->count == 0)
        return false;

    // get a pointer to the bucket
    Entry* entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL)
        return false;

    // copy the value to the output parameter
    *value = entry->value;
    return true;
}

// modify `table` to capacity `capacity`
static void adjustCapacity(Table* table, int capacity) {
    // create a new entries array as replacement
    Entry* entries = ALLOCATE(Entry, capacity);

    for (int i = 0; i < capacity; ++i) {
        entries[i].key = NULL;
        entries[i].value = NIL_VAL;
    }

    // recalculate the size as it may change due to tombstones
    table->count = 0;

    for (int i = 0; i < table->capacity; ++i) {
        Entry* entry = &table->entries[i];
        if (entry->key == NULL)
            continue;

        Entry* dest = findEntry(entries, capacity, entry->key);

        dest->key = entry->key;
        dest->value = entry->value;
        table->count++;
    }

    FREE_ARRAY(Entry, table->entries, table->capacity);

    table->entries = entries;
    table->capacity = capacity;
}

// add `key` : `value` pair to table's entries; return true if the
// `key` is not already in the table's entries
bool tableSet(Table* table, ObjString* key, Value value) {
    // determine if the array size is adequate; allocate additional capacity if needed
    if (table->count + 1 > table->capacity * TABLE_MAX_CAPACITY) {
        const int newCapacity = GROW_CAPACITY(table->capacity);
        adjustCapacity(table, newCapacity);
    }

    Entry* entry = findEntry(table->entries, table->capacity, key);

    // no existing entry for this key
    const bool isNewKey = entry->key == NULL;

    // when replacing a tombstone with a new entry the bucket is already accounted for,
    // so we won't increment (if !IS_NIL, the entry is a tombstone)
    if (isNewKey && IS_NIL(entry->value))
        table->count++;

    entry->key = key;
    entry->value = value;
    return isNewKey;
}

// delete `key` from `table` and set the entry to be a tombstone
bool tableDelete(Table* table, ObjString* key) {
    if (table->count == 0)
        return false;

    // locate the entry that corresponds to the key
    Entry* entry = findEntry(table->entries, table->capacity, key);

    // nothing to delete
    if (entry->key == NULL)
        return false;

    // we'll use this as a tombstone: NULL key and true value
    entry->key = NULL;
    entry->value = BOOL_VAL(true);
    return true;
}

void tableAddAll(Table* src, Table* dest) {
    for (int i = 0; i < src->capacity; ++i) {
        Entry* entry = &src->entries[i];
        // when a non-empty bucket is found, copy the Entry from src->dest
        if (entry->key != NULL) {
            tableSet(dest, entry->key, entry->value);
        }
    }
}

// return an ObjString from `table` where `chars` matches the entity's `chars`
ObjString* tableFindString(Table* table, const char* chars, int length, uint32_t hash) {
    if (table->count == 0)
        return false;

    uint32_t index = hash % table->capacity;

    for (;;) {
        Entry* entry = &table->entries[index];

        if (entry->key == NULL) {
            // stop if we find an empty non-tombstone entry
            if (IS_NIL(entry->value))
                return NULL;
        } else if (entry->key->length == length && entry->key->hash == hash &&
                   memcmp(entry->key->chars, chars, length) == 0) {
            // match found
            return entry->key;
        }

        index = (index + 1) % table->capacity;
    }
}

// mark global variables
void markTable(Table* table) {
    for (int i = 0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        markObject((Obj*)entry->key);
        markValue(entry->value);
    }
}
