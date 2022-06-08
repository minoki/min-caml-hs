#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

extern void min_caml_start(char *, char *);

/* The calling convention for variadic functions is different on Darwin
   (see https://developer.apple.com/documentation/xcode/writing-arm64-code-for-apple-platforms).
   I chose to wrap "printf" in C rather than handling the difference in the assembly source. */
int min_caml_print_int_impl(int64_t x) {
    return printf("%" PRId64, x);
}

int main(int argc, char *argv[]) {
    char *hp, *sp;

    sp = alloca(1000000);
    hp = malloc(4000000);
    if (hp == NULL || sp == NULL) {
        fputs("malloc or alloca failed\n", stderr);
        return 1;
    }
    fprintf(stderr, "sp = %p, hp = %p\n", sp, hp);
    min_caml_start(sp, hp);

    return 0;
}
