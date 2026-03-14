package com.wangdi.bctoolkit.generator.artifact

import com.wangdi.bctoolkit.base.Access
import com.wangdi.bctoolkit.base.TypeRef
import com.wangdi.bctoolkit.generator.MethodEmitter
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

    /**
     * Auto-generated baseline docs for init.
     * Describes the intent and behavior of this function.
     *
     * @param cw parameter from function signature.
     * @return return value of this function.
     */
    abstract fun init(cw: ClassWriter): ClassWriter

    /**
     * Auto-generated baseline docs for addMethod.
     * Describes the intent and behavior of this function.
     *
     * @param method parameter from function signature.
     * @return return value of this function.
     */
    fun addMethod(method: MethodEmitter): ClassArtifact = this.apply { this.methods.add(method) }

    /**
     * Auto-generated baseline docs for save.
     * Describes the intent and behavior of this function.
     *
     * @param outDir parameter from function signature.
     */
    fun save(outDir: Path)
    {
        this.init(cw)
        this.cw.visitEnd()
        val bytes: ByteArray = this.cw.toByteArray()
        val absDir: Path = this.clazz.getPackage()
            .fold(outDir) { acc, s -> acc.resolve(s) }
            .toAbsolutePath()
            .normalize()
        val outFile: Path = absDir.resolve("${this.clazz.getLastName()}.class")
        Files.createDirectories(outFile.parent)
        Files.write(outFile, bytes)
    }

    /**
     * Auto-generated baseline docs for accessOf.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    protected fun accessOf(): Int = this.access.fold(0) { acc, f -> acc or f.flag }

    /**
     * Auto-generated baseline docs for genSignature.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    protected fun genSignature(): String?
    {
        if (this.signature.isEmpty())
            return null

        val typeParams = this.signature.joinToString(separator = "") { tv -> "${tv}:L${TypeRef.OBJECT_FULL_NAME};" }

        return "<$typeParams>L${TypeRef.OBJECT_FULL_NAME};"
    }

    /**
     * Auto-generated baseline docs for getCW.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    fun getCW(): ClassWriter = this.cw
}

