#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

extern void min_caml_start(char *, char *);

/* The calling convention for variadic functions is different on Darwin
   (see https://developer.apple.com/documentation/xcode/writing-arm64-code-for-apple-platforms).
   I chose to wrap "printf" in C rather than handling the difference in the assembly source. */
int min_caml_print_int_impl(int64_t x) {
    return printf("%" PRId64, x);
}

int64_t min_caml_read_int_impl(void) {
    int64_t x;
    scanf("%" SCNd64, &x);
    return x;
}

double min_caml_read_float_impl(void) {
    double x;
    scanf("%lf", &x);
    return x;
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

    clock_t start_time = clock();
    min_caml_start(sp, hp);
    clock_t end_time = clock();

    if (argc > 1 && strcmp(argv[1], "--time") == 0) {
        fprintf(stderr, "elapsed: %g[s]\n", (double)(end_time - start_time) / (double)CLOCKS_PER_SEC);
    }

    return 0;
}
