xlang project:

Xlang compiler:
.x (source)
    -> .xtok                (tokenized source)          In JSON formate
    -> .xast                (parsed ast)                In JSON formate
    optimize
    -> .xtast               (typed / checked AST)       In JSON formate
    optimize (main)
    -> .xir                 (IR for x soruce)           In JSON formate
    optimize
    -> .xlir                (lowed x soruce)            In Specific formate

Xlang assembler (native):
.xlir (source)              (lowed x soruce)            In Specific formate
    -> .xlirtok             (tokenized lowed source)    In JSON formate
    -> .xlirast             (parsed + checked ast)      In JSON formate
    -> .asm                 (translate to nasm asm)     In NASM ASM formate
    -> .o


command:
sourcefiles includes:
.x, .xtok, .xast, .xtast, .xir, .xlir, (.xlirtok, .xlirast, .asm)

libs includes:
.o, .a, .lib, .dll, .so .....

dest includes:
.o, .a, .lib, .dll, .so .exe .....

xlang --target=native (-c <sourcefiles>) (-libs <libs>) -d <dest> (--include-runtime)
