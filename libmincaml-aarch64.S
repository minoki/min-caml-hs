#if defined(__APPLE__)
#define U(x) _##x
#else
#define U(x) x
#endif
        .text
        .align 2
        .global min_caml_print_newline
min_caml_print_newline:
        mov x0, #'\n'
        b U(putchar)
        .global min_caml_print_int
min_caml_print_int:
        b U(min_caml_print_int_impl)
        .global min_caml_print_byte
min_caml_print_byte:
        b U(putchar)
        .global min_caml_read_int
min_caml_read_int:
        b U(min_caml_read_int_impl)
        .global min_caml_read_float
min_caml_read_float:
        b U(min_caml_read_float_impl)
        .global min_caml_create_array
min_caml_create_array:
        // x0: number of elements, x1: initial value
        mov x2, x28
        add x28, x28, x0, lsl #3 // x28 = x28 + x0 * 8
Lcreate_array_loop:
        cbz x0, Lcreate_array_exit
        sub x0, x0, #1
        str x1, [x2, x0, lsl #3]
        b Lcreate_array_loop
Lcreate_array_exit:
        mov x0, x2
        ret
        .global min_caml_create_float_array
min_caml_create_float_array:
        // x0: number of elements, d0: initial value
        mov x1, x28
        add x28, x28, x0, lsl #3 // x28 = x28 + x0 * 8
Lcreate_float_array_loop:
        cbz x0, Lcreate_float_array_exit
        sub x0, x0, #1
        str d0, [x1, x0, lsl #3]
        b Lcreate_float_array_loop
Lcreate_float_array_exit:
        mov x0, x1
        ret
        .global min_caml_abs_float
min_caml_abs_float:
        fabs d0, d0
        ret
        .global min_caml_sqrt
min_caml_sqrt:
        fsqrt d0, d0
        ret
        .global min_caml_floor
min_caml_floor:
        frintm d0, d0
        ret
        .global min_caml_int_of_float
min_caml_int_of_float:
        .global min_caml_truncate
min_caml_truncate:
        fcvtzs x0, d0
        ret
        .global min_caml_float_of_int
min_caml_float_of_int:
        scvtf d0, x0
        ret
        .global min_caml_cos
min_caml_cos:
        b U(cos)
        .global min_caml_sin
min_caml_sin:
        b U(sin)
        .global min_caml_atan
min_caml_atan:
        b U(atan)
