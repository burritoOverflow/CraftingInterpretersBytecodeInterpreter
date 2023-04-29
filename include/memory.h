#ifndef CLOX_MEMORY_H
#define CLOX_MEMORY_H

#include "common.h"
#include "object.h"

#define ALLOCATE(type, count) (type*)reallocate(NULL, 0, sizeof(type) * (count))

#define FREE(type, pointer) reallocate(pointer, sizeof(type), 0)

// either grow to 8 or twice the current capacity
#define GROW_CAPACITY(capacity) ((capacity) < 8 ? 8 : (capacity)*2)

// handles the work of getting the correct size of the array's element type and casting
// results (void *) back to a pointer of the correct type
#define GROW_ARRAY(type, pointer, oldCount, newCount) \
    reallocate(pointer, sizeof(type) * (oldCount), sizeof(type) * (newCount))

#define FREE_ARRAY(type, pointer, oldCount) reallocate(pointer, sizeof(type) * (oldCount), 0)

// Reallocate the pointed to from oldSize to newSize
void* reallocate(void* pointer, size_t oldSize, size_t newSize);

void freeObjects(void);

#endif
