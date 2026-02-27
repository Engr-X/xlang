package com.wangdi.classgen.artifact.operation

import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor


open class OP(
    protected val mv: MethodVisitor,
    protected val labels: MutableMap<Int, Label>
)
{
    open fun addOp(): MethodVisitor = this.mv
}
