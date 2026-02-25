package com.wangdi.classgen.generator

import com.wangdi.classgen.base.Access
import com.wangdi.classgen.artifact.ClassArtifact
import com.wangdi.classgen.base.TypeRef

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes.V1_8
import java.security.Signature
import kotlin.math.sin


class ClassGenerator(
    jvmTarget: Int,
    access: MutableList<Access>,
    clazz: TypeRef,
    signature: MutableList<String> = mutableListOf(),

    private val superClass: TypeRef = TypeRef.OBJECT,
    private val interfaces: MutableList<TypeRef> = ArrayList(),
) : ClassArtifact(jvmTarget, access, clazz, signature)
{
    override fun init(cw: ClassWriter): ClassWriter = this.cw.also {
        it.visit(
            this.jvmTarget,
            this.accessOf(),
            this.clazz.getFullName("/"),
            this.genSignature(),
            this.superClass.getFullName("/"),
            this.interfaces.map { impl -> impl.toString() }.toTypedArray())
    }

    private fun genSignature(): String?
    {
        if (this.signature.isEmpty())
            return null

        val typeParams = this.signature.joinToString(separator = "") { tv -> "${tv}:L${TypeRef.OBJECT_FULL_NAME};" }

        return "<$typeParams>L${TypeRef.OBJECT_FULL_NAME};"
    }
}
