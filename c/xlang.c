#include <stdio.h>
#include "libhaskell.h"

int main(int argc, char const *argv[])
{
    int a = add(1, 2);
    printf("Hello, World!%d\n", a);
    return 0;
}
