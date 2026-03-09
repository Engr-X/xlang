package com.wangdi.bctoolkit.reader

import com.wangdi.bctoolkit.base.Access
import com.wangdi.bctoolkit.base.TypeRef
import org.objectweb.asm.ClassReader
import org.objectweb.asm.ClassVisitor
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes


class ByteCodeReader(bytes: ByteArray)
{
    companion object
    {
        private fun splitClassName(fullName: String): MutableList<String> = fullName.split(".").toMutableList()

        private val ACCESS_VALUE_MAP: MutableList<Pair<Int, Access>> = mutableListOf(
            Opcodes.ACC_PUBLIC to Access.Public,
            Opcodes.ACC_PRIVATE to Access.Private,
            Opcodes.ACC_PROTECTED to Access.Protected,

            Opcodes.ACC_FINAL to Access.Final)
    }

    private val reader: ClassReader = ClassReader(bytes)

    fun read(): JavaClass
    {
        var className: MutableList<String> = mutableListOf()
        var superClass: MutableList<String> = mutableListOf()
        var interfaceClass: MutableList<MutableList<String>> = mutableListOf()

        var accessList: MutableList<Access> = mutableListOf()

        object : ClassVisitor(Opcodes.ASM9) {
            override fun visit(
                version: Int,
                access: Int,
                name: String,
                signature: String,
                superName: String,
                interfaces: Array<String>)
            {
                className = splitClassName(name)
                superClass = splitClassName(superName)
                interfaceClass = interfaces.map { splitClassName(it) }.toMutableList()


                accessList = ACCESS_VALUE_MAP
                    .filterNot { (flag, _) -> (access and flag) == 0 }
                    .map { it.second }
                    .toMutableList()
            }
        }

        val classRef = TypeRef(className)

        return JavaClass(
            JavaPackage(classRef.getPackage()),
            accessList,
            classRef.getName(),
            superClass,
            interfaceClass
        )
    }

    fun readFunction()
    {
//        object : ClassVisitor(Opcodes.ASM9) {
//
//            override fun visitMethod(
//                access: Int,
//                name: String,
//                descriptor: String,
//                signature: String?,
//                exceptions: Array<out String>?
//            ): MethodVisitor?
//            {
//
//            }
//        }
    }
}
