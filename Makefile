CFLAGS= -g -O2 -Wall

all: bin/min-caml

SOURCES = \
 src/AArch64/Asm.hs \
 src/AArch64/Emit.hs \
 src/AArch64/RegAlloc.hs \
 src/AArch64/Simm.hs \
 src/AArch64/Virtual.hs \
 src/Alpha.hs \
 src/Assoc.hs \
 src/Beta.hs \
 src/Closure.hs \
 src/ConstFold.hs \
 src/Elim.hs \
 src/Id.hs \
 src/Inline.hs \
 src/KNormal.hs \
 src/Lexer.x \
 src/Main.hs \
 src/MyPrelude.hs \
 src/Parser.y \
 src/Syntax.hs \
 src/Type.hs \
 src/Typing.hs

bin/min-caml: min-caml.cabal $(SOURCES)
	cabal install --installdir=bin --overwrite-policy=always min-caml

original/test/%.s: bin/min-caml original/test/%.ml
	bin/min-caml original/test/$*

original/test/%: original/test/%.s libmincaml-aarch64.S stub.c
	$(CC) $(CFLAGS) -o $@ $^ -lm

original/min-rt/min-rt.s: bin/min-caml original/min-rt/min-rt.ml
	bin/min-caml original/min-rt/min-rt

original/min-rt/min-rt.min-caml: original/min-rt/min-rt.s min-rt-globals.s libmincaml-aarch64.S stub.c
	$(CC) $(CFLAGS) -o $@ $^ -lm

original/min-rt/contest.ppm: original/min-rt/min-rt.min-caml original/min-rt/contest.sld
	original/min-rt/min-rt.min-caml < original/min-rt/contest.sld > $@

.PRECIOUS: original/test/%.s
.PHONY: all
