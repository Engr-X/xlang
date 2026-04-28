package com.diwang.bctoolkit.generator.artifact.operation

import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes


/**
 * Auto-generated baseline docs for loadInsn.
 * Describes the intent and behavior of this function.
 *
 * @param prefix parameter from function signature.
 * @param index parameter from function signature.
 * @return return value of this function.
 */
private fun loadInsn(prefix: String, index: Int): String =
    if (index in 0..3) "${prefix}_${index}" else "$prefix $index"

// load reference from local variable slot onto the operand stack.
class ILoad(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val index: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitVarInsn(Opcodes.ILOAD, this.index)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = this.indent(tabs) + loadInsn("iload", this.index)
}


class LLoad(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val index: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitVarInsn(Opcodes.LLOAD, this.index)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = this.indent(tabs) + loadInsn("lload", this.index)
}


class FLoad(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val index: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitVarInsn(Opcodes.FLOAD, this.index)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = this.indent(tabs) + loadInsn("fload", this.index)
}


class DLoad(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val index: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitVarInsn(Opcodes.DLOAD, this.index)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = this.indent(tabs) + loadInsn("dload", this.index)
}


class ALoad(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val index: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitVarInsn(Opcodes.ALOAD, this.index)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = this.indent(tabs) + loadInsn("aload", this.index)
}

