# MinCaml implementation in Haskell

This is a clone of [MinCaml](https://github.com/esumii/min-caml) that targets AArch64.

Supported platforms:

* AArch64 Darwin (macOS)
* AArch64 Linux

Steps to run:

```
$ cabal build
$ cabal run -- min-caml <basename>
$ cc <basename>.s libmincaml-aarch64.S stub.c -lm
$ ./a.out
```

Features:

* [x] Lexer
* [x] Parser
* [x] Type inference
* [x] K-normalization
* [x] Alpha conversion
* [x] Beta reduction
* [x] Flatten lets
* [x] Inlining
* [x] Constant folding
* [x] Dead definition elimination
* [x] Closure conversion
* [x] Virtual machine code generation
* [x] Immediate operand
* [x] Register allocation
* [x] Assembly generation
