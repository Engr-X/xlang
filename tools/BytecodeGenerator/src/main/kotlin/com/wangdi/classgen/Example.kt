package com.wangdi.classgen

import com.wangdi.classgen.base.Access
import com.wangdi.classgen.base.Type
import com.wangdi.classgen.base.TypeRef
import com.wangdi.classgen.generator.ClassEmitter
import org.objectweb.asm.Opcodes
import java.nio.file.Paths


object Example
{
    @JvmStatic
    fun main(args: Array<String>)
    {
        val gen = ClassEmitter(
            Opcodes.V1_8,
            mutableListOf(Access.Public, Access.Super),
            TypeRef("Square"),
        )

        val square = gen.newMethod(
            mutableListOf(Access.Static),
            "square",
            Type.INT32 to mutableListOf(Type.INT32)
        )
        square.Builder().apply {
            iload(0)
            iload(0)
            imul()
            ireturn()
        }.build()
        gen.addMethod(square)

        val stringArray = Type(TypeRef("java", "lang", "String"), 1)
        val printStream = Type(TypeRef("java", "io", "PrintStream"))
        val main = gen.newMethod(
            mutableListOf(Access.Public, Access.Static),
            "main",
            Type.VOID to mutableListOf(stringArray)
        )
        main.Builder().apply {
            getstatic(
                mutableListOf("java", "lang", "System", "out"),
                printStream
            )
            ipush(10)
            invokeStatic(
                mutableListOf("Square", "square"),
                Type.INT32 to mutableListOf(Type.INT32)
            )
            invokeVirtual(
                mutableListOf("java", "io", "PrintStream", "println"),
                Type.VOID to mutableListOf(Type.INT32)
            )
            `return`()
        }.build()
        gen.addMethod(main)

        gen.save(Paths.get("."))
    }

}
