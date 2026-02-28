package com.wangdi.classgen.artifact.operation

import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes


private fun storeInsn(prefix: String, index: Int): String =
    if (index in 0..3) "${prefix}_${index}" else "${prefix} ${index}"

// store the value on the top of stack to pool with index
class IStore(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val index: Int
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitVarInsn(Opcodes.ISTORE, index)
    }

    override fun toString(tabs: Int): String = indent(tabs) + storeInsn("istore", this.index)
}


class LStore(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val index: Int
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitVarInsn(Opcodes.LSTORE, this.index)
    }

    override fun toString(tabs: Int): String = indent(tabs) + storeInsn("lstore", this.index)
}


class FStore(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val index: Int
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitVarInsn(Opcodes.FSTORE, this.index)
    }

    override fun toString(tabs: Int): String = indent(tabs) + storeInsn("fstore", this.index)
}


class DStore(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val index: Int
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitVarInsn(Opcodes.DSTORE, this.index)
    }

    override fun toString(tabs: Int): String = indent(tabs) + storeInsn("dstore", this.index)
}
