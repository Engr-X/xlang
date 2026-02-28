package com.wangdi.classgen.generator

import com.wangdi.classgen.artifact.Layout
import com.wangdi.classgen.artifact.operation.OP
import com.wangdi.classgen.base.Access
import com.wangdi.classgen.base.Type

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor


class FunctionLayout(
    cw: ClassWriter,
    private val access: MutableList<Access>,
    private val name: String,
    private val funParams: Pair<Type, MutableList<Type>>,
    private val operations: MutableList<OP>,
    private val signature: MutableList<String> = mutableListOf(),
    private val exceptions: MutableList<Type> = mutableListOf(),
) : Layout(cw)
{
    private val mv: MethodVisitor = this.cw.visitMethod(
        this.accessOf(this.access),
        this.name,
        this.getDescription(),
        this.genSignature(this.signature),
        this.genExceptions(this.exceptions)
    )

    private val labels: MutableMap<Int, Label> = mutableMapOf()

    private fun getDescription(): String =
        "(${this.funParams.second.joinToString(separator = "") { it.getName() }})${this.funParams.first.getName()}"

    override fun generate(): Layout = this.apply {
        this.mv.visitCode()
        this.operations.forEach { it.addOp() }
        this.mv.visitMaxs(0, 0)
        this.mv.visitEnd()
    }
}
