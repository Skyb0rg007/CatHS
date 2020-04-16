#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

// allocate memory and leak it
uint64_t* allocate(uint64_t sz) {
    void *p = malloc(sizeof(uint64_t) * sz);
    assert(p);
    return p;
}

// allocate memory, initialize it and leak it
uint64_t* allocate_and_memset(uint64_t sz, uint64_t init) {
    uint64_t i;
    uint64_t *p = malloc(sizeof(uint64_t) * sz);
    for (i = 0; i < sz; i++) {
        p[i] = init;
    }
    return p;
}

void print_int(int64_t number) {
    printf("%"PRId64"", number);
}

void print_line_int(int64_t number) {
    printf("%"PRId64"\n", number);
}

void print_string(uint64_t *string_arg) {
    char *string = (char *)string_arg;
    printf("%s", string);
}

void print_line_string(uint64_t *string_arg) {
    char *string = (char *)string_arg;
    printf("%s\n", string);
}
