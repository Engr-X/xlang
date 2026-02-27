package com.wangdi.classgen.artifact.operation

import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes


// load reference from local variable slot onto the operand stack.
class LoadInt(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val index: Int
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitVarInsn(Opcodes.ILOAD, this.index)
    }
}


class LoadLong(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val index: Int
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitVarInsn(Opcodes.LLOAD, this.index)
    }
}


class LoadFloat(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val index: Int
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitVarInsn(Opcodes.FLOAD, this.index)
    }
}


class LoadDouble(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val index: Int
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitVarInsn(Opcodes.DLOAD, this.index)
    }
}


class LoadRef(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val index: Int
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitVarInsn(Opcodes.ALOAD, this.index)
    }
}
