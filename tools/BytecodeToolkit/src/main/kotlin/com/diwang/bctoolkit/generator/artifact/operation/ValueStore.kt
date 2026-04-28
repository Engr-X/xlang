package com.diwang.bctoolkit.generator.artifact.operation

import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes


/**
 * Auto-generated baseline docs for storeInsn.
 * Describes the intent and behavior of this function.
 *
 * @param prefix parameter from function signature.
 * @param index parameter from function signature.
 * @return return value of this function.
 */
private fun storeInsn(prefix: String, index: Int): String =
    if (index in 0..3) "${prefix}_${index}" else "$prefix $index"

// store the value on the top of stack to pool with index
class IStore(
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
        it.visitVarInsn(Opcodes.ISTORE, index)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = this.indent(tabs) + storeInsn("istore", this.index)
}


class LStore(
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
        it.visitVarInsn(Opcodes.LSTORE, this.index)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = this.indent(tabs) + storeInsn("lstore", this.index)
}


class FStore(
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
        it.visitVarInsn(Opcodes.FSTORE, this.index)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = this.indent(tabs) + storeInsn("fstore", this.index)
}


class DStore(
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
        it.visitVarInsn(Opcodes.DSTORE, this.index)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = this.indent(tabs) + storeInsn("dstore", this.index)
}

class AStore(
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
        it.visitVarInsn(Opcodes.ASTORE, this.index)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = this.indent(tabs) + storeInsn("astore", this.index)
}

