package com.wangdi.classgen.generator

import com.wangdi.classgen.artifact.Layout
import com.wangdi.classgen.base.Access
import com.wangdi.classgen.base.Type
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.MethodVisitor


class FunctionLayout(
    cw: ClassWriter,
    private val access: MutableList<Access>,
    private val name: String,
    private val funParams: Pair<Type, MutableList<Type>>,
    private val signature: MutableList<String> = mutableListOf(),
) : Layout(cw)
{
    private val mv: MethodVisitor = this.cw.visitMethod(
        this.accessOf(this.access),
        this.name,
        this.getDescription(),
        this.genSignature(this.signature),
        null
    )

    private fun getDescription(): String =
        "(${this.funParams.second.joinToString(separator = "") { it.getName() }})${this.funParams.first.getName()}"

    override fun generate(): Layout = this.apply {

    }
}
