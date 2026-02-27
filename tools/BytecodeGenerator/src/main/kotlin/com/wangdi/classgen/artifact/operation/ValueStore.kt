package com.wangdi.classgen.artifact.operation

import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes


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
}
