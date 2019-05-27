REM SIMPLEST OUTPUT
REM csmith --no-argc --no-arrays --no-bitfields --no-comma-operators --no-compound-assignment --no-consts --no-divs --no-embedded-assigns --no-pre-incr-operator --no-pre-decr-operator --no-post-incr-operator --no-post-decr-operator --no-unary-plus-operator --no-jumps --no-longlong --no-float --no-math64 --no-inline-function --no-muls --no-safe-math --no-packed-struct --no-pointers --no-structs --no-unions --no-volatiles --no-volatile-pointers --no-const-pointers --max-funcs 1 > test.c

REM REAL OUTPUT
csmith --no-argc --no-bitfields --no-longlong --no-float --no-math64 --no-safe-math --no-packed-struct --no-volatiles --no-volatile-pointers --no-consts --no-const-pointers > test.c
