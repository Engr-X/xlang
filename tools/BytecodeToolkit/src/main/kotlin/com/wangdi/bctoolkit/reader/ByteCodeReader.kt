package com.wangdi.bctoolkit.reader

import com.wangdi.bctoolkit.base.TypeRef
import org.objectweb.asm.ClassReader


class ByteCodeReader(private val bytes: ByteArray)
{
    private val reader: ClassReader = ClassReader(bytes)

    fun read(): JavaClass
    {
        val className = TypeRef(reader.className)
        val clazz = JavaClass(
            JavaPackage(className.getPackage()),
            mutableListOf(),
            className.getName())

        return clazz
    }
}
