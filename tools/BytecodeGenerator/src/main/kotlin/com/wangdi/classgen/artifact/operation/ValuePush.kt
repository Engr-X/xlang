package com.wangdi.classgen.artifact.operation

import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes


// push value to the top of the stack

class IPush(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val value: Int
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        when (this.value)
        {
            -1 -> it.visitInsn(Opcodes.ICONST_M1)
            0 -> it.visitInsn(Opcodes.ICONST_0)
            1 -> it.visitInsn(Opcodes.ICONST_1)
            2 -> it.visitInsn(Opcodes.ICONST_2)
            3 -> it.visitInsn(Opcodes.ICONST_3)
            4 -> it.visitInsn(Opcodes.ICONST_4)
            5 -> it.visitInsn(Opcodes.ICONST_5)
            in Byte.MIN_VALUE..Byte.MAX_VALUE ->
                it.visitIntInsn(Opcodes.BIPUSH, value)
            in Short.MIN_VALUE..Short.MAX_VALUE ->
                it.visitIntInsn(Opcodes.SIPUSH, value)
            else ->
                it.visitLdcInsn(value)
        }
    }
}


class LPush(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val value: Long
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        when (this.value)
        {
            0L -> it.visitInsn(Opcodes.LCONST_0)
            1L -> it.visitInsn(Opcodes.LCONST_1)
            else -> it.visitLdcInsn(this.value) // ASM handles LDC2_W when needed
        }
    }
}


class FPush(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val value: Float
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        // prefer FCONST_0/1/2, otherwise use LDC.
        when (java.lang.Float.floatToIntBits(this.value))
        {
            java.lang.Float.floatToIntBits(0.0f) -> it.visitInsn(Opcodes.FCONST_0)
            java.lang.Float.floatToIntBits(1.0f) -> it.visitInsn(Opcodes.FCONST_1)
            java.lang.Float.floatToIntBits(2.0f) -> it.visitInsn(Opcodes.FCONST_2)
            else -> it.visitLdcInsn(this.value)
        }
    }
}


class DPush(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val value: Double
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        // prefer DCONST_0/1, otherwise use LDC (ASM will pick LDC2_W when needed).
        when (java.lang.Double.doubleToLongBits(this.value))
        {
            java.lang.Double.doubleToLongBits(0.0) -> it.visitInsn(Opcodes.DCONST_0)
            java.lang.Double.doubleToLongBits(1.0) -> it.visitInsn(Opcodes.DCONST_1)
            else -> it.visitLdcInsn(this.value)
        }
    }
}
