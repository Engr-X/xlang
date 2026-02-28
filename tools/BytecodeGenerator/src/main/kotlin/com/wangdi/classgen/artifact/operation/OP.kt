package com.wangdi.classgen.artifact.operation

import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor


open class OP(
    protected val mv: MethodVisitor,
    protected val labels: MutableMap<Int, Label>
)
{
    open fun addOp(): MethodVisitor = this.mv

    protected fun indent(tabs: Int): String = " ".repeat(tabs.coerceAtLeast(0) * 4)

    open fun toString(tabs: Int = 0): String = "${indent(tabs)}empty op"

    override fun toString(): String = toString(0)
}
