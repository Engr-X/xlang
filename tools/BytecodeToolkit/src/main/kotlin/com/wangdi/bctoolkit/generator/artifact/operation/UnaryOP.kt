package com.wangdi.bctoolkit.generator.artifact.operation

import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes


// ---------- Numeric negation: -x ----------
class INeg(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.INEG)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}ineg"
}

class LNeg(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.LNEG)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}lneg"
}

class FNeg(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.FNEG)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}fneg"
}

class DNeg(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.DNEG)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}dneg"
}

// ---------- Unary plus: +x ----------
// No-op; emit nothing.
class UPlus(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}uplus"
}

