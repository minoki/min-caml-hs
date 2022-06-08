#if defined(__APPLE__)
#define U(x) _##x
#else
#define U(x) x
#endif
        .text
        .align 2
        .global min_caml_print_newline
min_caml_print_newline:
        stp x29, x30, [sp, #-16]!
        stp x0, x1, [sp, #-16]!
        add x29, sp, #32
        mov x0, #'\n'
        bl U(putchar)
        ldp x0, x1, [sp], #16
        ldp x29, x30, [sp], #16
        ret
        .align 2
        .global min_caml_print_int
min_caml_print_int:
        stp x29, x30, [sp, #-16]!
        stp x0, x1, [sp, #-16]!
        add x29, sp, #32
        mov x0, x2
        bl U(min_caml_print_int_impl)
        ldp x0, x1, [sp], #16
        ldp x29, x30, [sp], #16
        ret
