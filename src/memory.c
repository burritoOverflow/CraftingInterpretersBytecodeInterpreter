#include <stdlib.h>

#include "memory.h"

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
