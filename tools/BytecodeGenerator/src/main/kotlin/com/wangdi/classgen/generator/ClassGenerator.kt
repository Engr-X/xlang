package com.wangdi.classgen.generator

import com.wangdi.classgen.base.Access
import com.wangdi.classgen.artifact.ClassArtifact
import com.wangdi.classgen.base.TypeRef

import org.objectweb.asm.ClassWriter


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
}
