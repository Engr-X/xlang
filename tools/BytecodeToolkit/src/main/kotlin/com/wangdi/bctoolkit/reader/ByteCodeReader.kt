package com.wangdi.bctoolkit.reader

import com.wangdi.bctoolkit.base.Access
import com.wangdi.bctoolkit.base.TypeRef
import com.wangdi.bctoolkit.meta.OwnerTypeMetadata
import java.nio.file.Files
import java.nio.file.Path
import java.util.jar.JarFile
import org.objectweb.asm.AnnotationVisitor
import org.objectweb.asm.ClassReader
import org.objectweb.asm.ClassVisitor
import org.objectweb.asm.FieldVisitor
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes
import org.objectweb.asm.Type as AsmType


class ByteCodeReader(bytes: ByteArray)
{
    companion object
    {
        fun readClass(path: Path): JavaClass =
            ByteCodeReader(Files.readAllBytes(path)).read()

        private fun readArchiveClasses(
            path: Path,
            classEntryFilter: (String) -> Boolean
        ): List<JavaClass>
        {
            val classes = mutableListOf<JavaClass>()
            JarFile(path.toFile()).use { jar ->
                val entries = jar.entries()
                while (entries.hasMoreElements())
                {
                    val entry = entries.nextElement()
                    if (entry.isDirectory || !classEntryFilter(entry.name))
                        continue
                    jar.getInputStream(entry).use { input ->
                        classes.add(ByteCodeReader(input.readBytes()).read())
                    }
                }
            }

            return classes.sortedWith(compareBy({ it.toDto().packagePath.joinToString(".") }, { it.getName() }))
        }

        fun readJar(path: Path): List<JavaClass>
            = readArchiveClasses(path) { entryName -> entryName.endsWith(".class") }

        fun readJmod(path: Path): List<JavaClass>
            = readArchiveClasses(path) { entryName ->
                entryName.startsWith("classes/") && entryName.endsWith(".class")
            }

        private fun splitClassName(fullName: String): MutableList<String> = fullName.split("/").toMutableList()

        private val ACCESS_VALUE_MAP: MutableList<Pair<Int, Access>> = mutableListOf(
            Opcodes.ACC_PUBLIC to Access.Public,
            Opcodes.ACC_PRIVATE to Access.Private,
            Opcodes.ACC_PROTECTED to Access.Protected,
            Opcodes.ACC_STATIC to Access.Static,

            Opcodes.ACC_FINAL to Access.Final)

        private fun parseAsmType(asmType: AsmType): MutableList<String> = when (asmType.sort)
        {
            AsmType.VOID -> mutableListOf("void")
            AsmType.BOOLEAN -> mutableListOf("boolean")
            AsmType.CHAR -> mutableListOf("char")
            AsmType.BYTE -> mutableListOf("byte")
            AsmType.SHORT -> mutableListOf("short")
            AsmType.INT -> mutableListOf("int")
            AsmType.FLOAT -> mutableListOf("float")
            AsmType.LONG -> mutableListOf("long")
            AsmType.DOUBLE -> mutableListOf("double")
            AsmType.OBJECT -> splitClassName(asmType.internalName)
            AsmType.ARRAY ->
            {
                val base = parseAsmType(asmType.elementType)
                repeat(asmType.dimensions) { base.add("[]") }
                base
            }
            else -> mutableListOf("java", "lang", "Object")
        }

        private fun parseType(fullName: String): TypeRef =
            TypeRef(parseAsmType(AsmType.getType(fullName)))

        private fun parseSignature(descriptor: String): Pair<TypeRef, MutableList<TypeRef>>
        {
            val methodType: AsmType = AsmType.getMethodType(descriptor)
            val ret = TypeRef(parseAsmType(methodType.returnType))
            val params = methodType.argumentTypes
                .map { argType -> TypeRef(parseAsmType(argType)) }
                .toMutableList()

            return ret to params
        }

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

        val cv = object : ClassVisitor(Opcodes.ASM9) {
            override fun visit(
                version: Int,
                access: Int,
                name: String,
                signature: String?,
                superName: String?,
                interfaces: Array<String>)
            {
                className = splitClassName(name)
                superClass = if (superName == null)
                    mutableListOf("java", "lang", "Object")
                else
                    splitClassName(superName)
                interfaceClass = interfaces.map { splitClassName(it) }.toMutableList()
                accessList = parseAccess(access)
            }
        }
        reader.accept(cv, 0)

        val classRef = TypeRef(className)

        val clazz = JavaClass(
            JavaPackage(classRef.getPackage()),
            accessList.toHashSet(),
            classRef.getName().toMutableList(),
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
                val accessSet: Set<Access> = parseAccess(access)
                val sig: Pair<TypeRef, MutableList<TypeRef>> = parseSignature(descriptor)
                var ownerType: String = OwnerTypeMetadata.CLASS

                return object : MethodVisitor(Opcodes.ASM9)
                {
                    override fun visitAnnotation(descriptor: String?, visible: Boolean): AnnotationVisitor?
                    {
                        if (!OwnerTypeMetadata.isOwnerTypeDescriptor(descriptor))
                            return super.visitAnnotation(descriptor, visible)

                        return OwnerTypeMetadata.reader(ownerType) { parsed -> ownerType = parsed }
                    }

                    override fun visitEnd()
                    {
                        val method = JavaMethod(clazz, accessSet, name, sig, ownerType)
                        clazz.addFunction(method)
                        super.visitEnd()
                    }
                }
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
                val accessSet: Set<Access> = parseAccess(access)
                val fieldType: TypeRef = parseType(descriptor)
                var ownerType: String = OwnerTypeMetadata.CLASS

                return object : FieldVisitor(Opcodes.ASM9)
                {
                    override fun visitAnnotation(descriptor: String?, visible: Boolean): AnnotationVisitor?
                    {
                        if (!OwnerTypeMetadata.isOwnerTypeDescriptor(descriptor))
                            return super.visitAnnotation(descriptor, visible)

                        return OwnerTypeMetadata.reader(ownerType) { parsed -> ownerType = parsed }
                    }

                    override fun visitEnd()
                    {
                        val field = JavaField(clazz, accessSet, fieldType, name, ownerType)
                        clazz.addField(field)
                        super.visitEnd()
                    }
                }
            }
        }
        reader.accept(cv, 0)
    }
}
