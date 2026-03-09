package com.wangdi.bctoolkit.artifact.operation

import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes


private fun loadInsn(prefix: String, index: Int): String =
    if (index in 0..3) "${prefix}_${index}" else "$prefix $index"

// load reference from local variable slot onto the operand stack.
class ILoad(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val index: Int
) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitVarInsn(Opcodes.ILOAD, this.index)
    }

    override fun toString(tabs: Int): String = this.indent(tabs) + loadInsn("iload", this.index)
}


class LLoad(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val index: Int
) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitVarInsn(Opcodes.LLOAD, this.index)
    }

    override fun toString(tabs: Int): String = this.indent(tabs) + loadInsn("lload", this.index)
}


class FLoad(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val index: Int
) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitVarInsn(Opcodes.FLOAD, this.index)
    }

    override fun toString(tabs: Int): String = this.indent(tabs) + loadInsn("fload", this.index)
}


class DLoad(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val index: Int
) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitVarInsn(Opcodes.DLOAD, this.index)
    }

    override fun toString(tabs: Int): String = this.indent(tabs) + loadInsn("dload", this.index)
}


class ALoad(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val index: Int
) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitVarInsn(Opcodes.ALOAD, this.index)
    }

    override fun toString(tabs: Int): String = this.indent(tabs) + loadInsn("aload", this.index)
}
