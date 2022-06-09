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
        .global min_caml_print_byte
min_caml_print_byte:
        stp x29, x30, [sp, #-16]!
        stp x0, x1, [sp, #-16]!
        add x29, sp, #32
        mov x0, x2
        bl U(putchar)
        ldp x0, x1, [sp], #16
        ldp x29, x30, [sp], #16
        ret
        .global min_caml_create_array
min_caml_create_array:
        // x2: number of elements, x3: initial value
        mov x4, x1
        add x1, x1, x2, lsl #3 // x1 = x1 + x2 * 8
Lcreate_array_loop:
        cbz x2, Lcreate_array_exit
        sub x2, x2, #1
        str x3, [x4, x2, lsl #3]
        b Lcreate_array_loop
Lcreate_array_exit:
        mov x2, x4
        ret
        .global min_caml_create_float_array
min_caml_create_float_array:
        // x2: number of elements, d0: initial value
        mov x4, x1
        add x1, x1, x2, lsl #3 // x1 = x1 + x2 * 8
Lcreate_float_array_loop:
        cbz x2, Lcreate_float_array_exit
        sub x2, x2, #1
        str d0, [x4, x2, lsl #3]
        b Lcreate_float_array_loop
Lcreate_float_array_exit:
        mov x2, x4
        ret
        .global min_caml_truncate
min_caml_truncate:
        fcvtzs x2, d0
        ret
