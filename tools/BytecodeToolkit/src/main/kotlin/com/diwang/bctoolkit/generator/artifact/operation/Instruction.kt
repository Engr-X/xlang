package com.diwang.bctoolkit.generator.artifact.operation

import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor


open class Instruction(
    protected val mv: MethodVisitor,
    protected val labels: MutableMap<Int, Label>
)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    open fun addOp(): MethodVisitor = this.mv

    /**
     * Auto-generated baseline docs for indent.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    protected fun indent(tabs: Int): String = " ".repeat(tabs.coerceAtLeast(0) * 4)

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    open fun toString(tabs: Int = 0): String = "${this.indent(tabs)}empty op"

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun toString(): String = toString(0)
}

