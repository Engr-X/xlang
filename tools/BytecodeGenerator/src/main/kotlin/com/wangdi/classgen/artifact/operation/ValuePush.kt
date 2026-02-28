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

    override fun toString(tabs: Int): String {
        val op = when (this.value)
        {
            -1 -> "iconst_m1"
            0 -> "iconst_0"
            1 -> "iconst_1"
            2 -> "iconst_2"
            3 -> "iconst_3"
            4 -> "iconst_4"
            5 -> "iconst_5"
            in Byte.MIN_VALUE..Byte.MAX_VALUE -> "bipush ${this.value}"
            in Short.MIN_VALUE..Short.MAX_VALUE -> "sipush ${this.value}"
            else -> "ldc ${this.value}"
        }
        return "${indent(tabs)}${op}"
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

    override fun toString(tabs: Int): String {
        val op = when (this.value)
        {
            0L -> "lconst_0"
            1L -> "lconst_1"
            else -> "ldc2_w ${this.value}"
        }
        return "${indent(tabs)}${op}"
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

    override fun toString(tabs: Int): String {
        val op = when (java.lang.Float.floatToIntBits(this.value))
        {
            java.lang.Float.floatToIntBits(0.0f) -> "fconst_0"
            java.lang.Float.floatToIntBits(1.0f) -> "fconst_1"
            java.lang.Float.floatToIntBits(2.0f) -> "fconst_2"
            else -> "ldc ${this.value}"
        }
        return "${indent(tabs)}${op}"
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

    override fun toString(tabs: Int): String {
        val op = when (java.lang.Double.doubleToLongBits(this.value))
        {
            java.lang.Double.doubleToLongBits(0.0) -> "dconst_0"
            java.lang.Double.doubleToLongBits(1.0) -> "dconst_1"
            else -> "ldc2_w ${this.value}"
        }
        return "${indent(tabs)}${op}"
    }
}
