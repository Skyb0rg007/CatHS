#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

// allocate memory and leak it
uint64_t* allocate(uint64_t length) {
        uint64_t *memory = malloc(sizeof(*memory) * length);
        return memory;
}

// allocate memory, initialize it and leak it
uint64_t* allocate_and_memset(uint64_t length, uint64_t inital_value) {
        uint64_t *memory = malloc(sizeof(*memory) * length);
        for (uint64_t i = 0; i < length; i++) {
                memory[i] = inital_value;
        }
        return memory;
}

void print_int(uint64_t number) {
        printf("%"PRId64"", (int64_t) number);
}

void print_line_int(uint64_t number) {
        printf("%"PRId64"\n", (int64_t) number);
}

void print_string(uint64_t *string_arg) {
        char *string = (char *) string_arg;
        printf("%s", string);
}

void print_line_string(uint64_t *string_arg) {
        char *string = (char *) string_arg;
        printf("%s\n", string);
}
