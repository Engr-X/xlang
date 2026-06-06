@file.class("String")
package xlang.util.string

import xlang.System


val NULL_CHAR: char = '\0'
val LINE_FEED: char = '\n'

fun strlen(str: pointer<char>) -> int
{
    var count: int = 0

    for (var ptr: pointer<char> = str; ptr.deref != NULL_CHAR; ptr++, count++);

    return count
}


fun streq(str1: pointer<char>, str2: pointer<char>) -> bool
{
    var ptr1: pointer<char> = str1
    var ptr2: pointer<char> = str2

    for (;
        ptr1.deref != NULL_CHAR && ptr2.deref != NULL_CHAR;
        ptr1++, ptr2++)
    {
        if ptr1.deref != ptr2.deref:
            return false
    }

    return ptr1.deref == ptr2.deref
}


fun strcpy(dest: pointer<char>, src: pointer<char>)
{
    var destPtr: pointer<char> = dest
    var srcPtr: pointer<char> = src

    for (;srcPtr.deref != NULL_CHAR; destPtr++, srcPtr++):
        destPtr.deref = srcPtr.deref

    destPtr.deref = NULL_CHAR
}


fun strcat(dest: pointer<char>, src: pointer<char>)
{
    val destLength: int = strlen(dest)
    strcpy(dest + destLength, src)
}
