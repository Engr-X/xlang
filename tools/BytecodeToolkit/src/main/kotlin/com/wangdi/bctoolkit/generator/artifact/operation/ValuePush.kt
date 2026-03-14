package com.wangdi.bctoolkit.generator.artifact.operation

import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes


// push value to the top of the stack
class IPush(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val value: Int
) : Instruction(mv, labels)
{
    private companion object {
        private val ICONST_TABLE: Map<Int, Int> = mutableMapOf(
            -1 to Opcodes.ICONST_M1,
            0 to Opcodes.ICONST_0,
            1 to Opcodes.ICONST_1,
            2 to Opcodes.ICONST_2,
            3 to Opcodes.ICONST_3,
            4 to Opcodes.ICONST_4,
            5 to Opcodes.ICONST_5
        )

        private val ICONST_NAME_TAbBLE: Map<Int, String> = mapOf(
            -1 to "iconst_m1",
            0 to "iconst_0",
            1 to "iconst_1",
            2 to "iconst_2",
            3 to "iconst_3",
            4 to "iconst_4",
            5 to "iconst_5"
        )
    }

    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also { mv ->
        ICONST_TABLE[this.value]?.let { opcode ->
            mv.visitInsn(opcode)
            return@also
        }

        when (this.value) {
            in Byte.MIN_VALUE..Byte.MAX_VALUE -> mv.visitIntInsn(Opcodes.BIPUSH, this.value)
            in Short.MIN_VALUE..Short.MAX_VALUE -> mv.visitIntInsn(Opcodes.SIPUSH, this.value)
            else -> mv.visitLdcInsn(this.value)
        }
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String {
        val op: String = ICONST_NAME_TAbBLE[this.value] ?: when (this.value) {
            in Byte.MIN_VALUE..Byte.MAX_VALUE -> "bipush ${this.value}"
            in Short.MIN_VALUE..Short.MAX_VALUE -> "sipush ${this.value}"
            else -> "ldc ${this.value}"
        }
        return "${this.indent(tabs)}$op"
    }
}


class LPush(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val value: Long
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        when (this.value)
        {
            0L -> it.visitInsn(Opcodes.LCONST_0)
            1L -> it.visitInsn(Opcodes.LCONST_1)
            else -> it.visitLdcInsn(this.value) // ASM handles LDC2_W when needed
        }
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String {
        val op: String = when (this.value)
        {
            0L -> "lconst_0"
            1L -> "lconst_1"
            else -> "ldc2_w ${this.value}"
        }
        return "${this.indent(tabs)}${op}"
    }
}


class FPush(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val value: Float
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
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

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String {
        val op: String = when (java.lang.Float.floatToIntBits(this.value))
        {
            java.lang.Float.floatToIntBits(0.0f) -> "fconst_0"
            java.lang.Float.floatToIntBits(1.0f) -> "fconst_1"
            java.lang.Float.floatToIntBits(2.0f) -> "fconst_2"
            else -> "ldc ${this.value}"
        }
        return "${this.indent(tabs)}${op}"
    }
}


class DPush(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val value: Double
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        // prefer DCONST_0/1, otherwise use LDC (ASM will pick LDC2_W when needed).
        when (java.lang.Double.doubleToLongBits(this.value))
        {
            java.lang.Double.doubleToLongBits(0.0) -> it.visitInsn(Opcodes.DCONST_0)
            java.lang.Double.doubleToLongBits(1.0) -> it.visitInsn(Opcodes.DCONST_1)
            else -> it.visitLdcInsn(this.value)
        }
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String {
        val op: String = when (java.lang.Double.doubleToLongBits(this.value))
        {
            java.lang.Double.doubleToLongBits(0.0) -> "dconst_0"
            java.lang.Double.doubleToLongBits(1.0) -> "dconst_1"
            else -> "ldc2_w ${this.value}"
        }
        return "${this.indent(tabs)}${op}"
    }
}


class APush(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val value: String
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitLdcInsn(this.value)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}ldc \"${this.value}\""
}

//class AConstNull(
//    mv: MethodVisitor,
//    labels: MutableMap<Int, Label>
//) : Instruction(mv, labels)
//{
//    override fun addOp(): MethodVisitor = this.mv.also {
//        it.visitInsn(Opcodes.ACONST_NULL)
//    }
//
//    override fun toString(tabs: Int): String = "${this.indent(tabs)}aconst_null"
//}

