package com.wangdi.classgen.artifact

import com.wangdi.classgen.base.Access
import com.wangdi.classgen.base.TypeRef
import com.wangdi.classgen.generator.MethodEmitter
import org.objectweb.asm.ClassWriter

import java.nio.file.Files
import java.nio.file.Path


abstract class ClassArtifact(
    protected val jvmTarget: Int,
    protected val access: MutableList<Access>,
    protected val clazz: TypeRef,
    protected val signature: MutableList<String> = mutableListOf())
{
    protected val methods: MutableList<MethodEmitter> = mutableListOf()

    protected val cw: ClassWriter = ClassWriter(ClassWriter.COMPUTE_FRAMES or ClassWriter.COMPUTE_MAXS)

    abstract fun init(cw: ClassWriter): ClassWriter

    fun addMethod(method: MethodEmitter): ClassArtifact = this.apply { this.methods.add(method) }

    fun save(outDir: Path)
    {
        this.init(cw)
        this.cw.visitEnd()
        val bytes: ByteArray = this.cw.toByteArray()
        val absDir: Path = outDir.toAbsolutePath().normalize()
        val outFile: Path = absDir.resolve("${this.clazz.getName()}.class")
        Files.createDirectories(outFile.parent)
        Files.write(outFile, bytes)
    }

    protected fun accessOf(): Int = this.access.fold(0) { acc, f -> acc or f.flag }

    protected fun genSignature(): String?
    {
        if (this.signature.isEmpty())
            return null

        val typeParams = this.signature.joinToString(separator = "") { tv -> "${tv}:L${TypeRef.OBJECT_FULL_NAME};" }

        return "<$typeParams>L${TypeRef.OBJECT_FULL_NAME};"
    }

    fun getCW(): ClassWriter = this.cw
}
