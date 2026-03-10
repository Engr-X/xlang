package com.wangdi.bctoolkit.reader

import com.wangdi.bctoolkit.base.Access
import com.wangdi.bctoolkit.base.TypeRef
import org.objectweb.asm.ClassReader
import org.objectweb.asm.ClassVisitor
import org.objectweb.asm.FieldVisitor
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

        private fun parseType(fullName: String): TypeRef = TODO()

        private fun parseSignature(descriptor: String): Pair<TypeRef, MutableList<TypeRef>> = TODO()

        private fun parseAccess(raw: Int): Set<Access> = ACCESS_VALUE_MAP
            .filterNot { (flag, _) -> (raw and flag) == 0 }
            .map { it.second }
            .toHashSet()
    }

    private val reader: ClassReader = ClassReader(bytes)

    fun read(): JavaClass
    {
        var className: MutableList<String> = mutableListOf()
        var superClass: MutableList<String> = mutableListOf()
        var interfaceClass: MutableList<MutableList<String>> = mutableListOf()

        var accessList: Set<Access> = HashSet()

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
                accessList = parseAccess(access)
            }
        }

        val classRef = TypeRef(className)

        val clazz = JavaClass(
            JavaPackage(classRef.getPackage()),
            accessList.toHashSet(),
            classRef.getName(),
            superClass,
            interfaceClass
        )

        this.readField(clazz)
        this.readFunction(clazz)

        return clazz
    }

    fun readFunction(clazz: JavaClass)
    {
        val cv = object : ClassVisitor(Opcodes.ASM9) {
            override fun visitMethod(
                access: Int,
                name: String,
                descriptor: String,
                signature: String?,
                exceptions: Array<out String>?
            ): MethodVisitor? {

                val method = JavaMethod(clazz, parseAccess(access),
                    name, parseSignature(descriptor))

                clazz.addFunction(method)

                return super.visitMethod(access, name, descriptor, signature, exceptions)
            }
        }

        reader.accept(cv, 0)
    }

    fun readField(clazz: JavaClass)
    {
        val cv = object : ClassVisitor(Opcodes.ASM9) {
            override fun visitField(
                access: Int,
                name: String,
                descriptor: String,
                signature: String?,
                value: Any?
            ): FieldVisitor? {
                val field = JavaField(clazz,
                    parseAccess(access),parseType(descriptor), name)
                return super.visitField(access, name, descriptor, signature, value)
            }
        }
    }
}
