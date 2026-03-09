package com.wangdi.bctoolkit.reader

import com.wangdi.bctoolkit.base.Access
import com.wangdi.bctoolkit.base.TypeRef
import org.objectweb.asm.ClassReader


class ByteCodeReader(bytes: ByteArray)
{
    private val reader: ClassReader = ClassReader(bytes)

    fun read(): JavaClass
    {
        val visitor: ClassVisitor = object : ClassVisitor(Opcodes.ASM9) {
            override fun visit(version: int, access: int, name: String, signature: String,
                    superName: String, interfaces: Array<String>)
            {
                System.out.println("class = " + name);
                System.out.println("access = " + access);

                if ((access & Opcodes.ACC_PUBLIC) != 0) {
                    System.out.println("public");
                }
                if ((access & Opcodes.ACC_FINAL) != 0) {
                    System.out.println("final");
                }
                if ((access & Opcodes.ACC_ABSTRACT) != 0) {
                    System.out.println("abstract");
                }
                if ((access & Opcodes.ACC_INTERFACE) != 0) {
                    System.out.println("interface");
                }
            }
        }
    }

    private fun getClassAccess(): MutableList<Access>
    {
        val 

        this.reader.accept( , 0);
    }
}
