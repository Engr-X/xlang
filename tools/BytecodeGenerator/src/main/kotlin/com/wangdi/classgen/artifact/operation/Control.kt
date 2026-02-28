package com.wangdi.classgen.artifact.operation

import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes


class OpBlock(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int,
    private val blockOps: MutableList<OP>
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        return this.mv.also {
            it.visitLabel(label)
            this.blockOps.forEach { op -> op.addOp() }
        }
    }

    override fun toString(tabs: Int): String {
        val pad = indent(tabs)
        if (this.blockOps.isEmpty())
            return "${pad}.L${this.blockId}"
        val body = this.blockOps.joinToString("\n") { it.toString(tabs + 1) }
        return "${pad}.L${this.blockId}\n${body}"
    }
}


class Goto(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        it.visitJumpInsn(Opcodes.GOTO, label)
    }

    override fun toString(tabs: Int): String = "${indent(tabs)}goto .L${this.blockId}"
}


class Return(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.RETURN)
    }

    override fun toString(tabs: Int): String = "${indent(tabs)}return"
}


class IReturn(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.IRETURN)
    }

    override fun toString(tabs: Int): String = "${indent(tabs)}ireturn"
}


class Pop(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.POP)
    }

    override fun toString(tabs: Int): String = "${indent(tabs)}pop"
}


class Dup(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.DUP)
    }

    override fun toString(tabs: Int): String = "${indent(tabs)}dup"
}


class Nop(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.NOP)
    }

    override fun toString(tabs: Int): String = "${indent(tabs)}nop"
}
