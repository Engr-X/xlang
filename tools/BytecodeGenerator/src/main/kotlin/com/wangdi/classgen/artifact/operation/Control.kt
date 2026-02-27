package com.wangdi.classgen.artifact.operation

import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes


class Condition(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val thenBlock: MutableList<OP>,
    private val elseBlock: MutableList<OP>,
) : OP(mv, labels)
{
    private val hasThen: Boolean
        get() = this.thenBlock.isNotEmpty()

    private val hasElse: Boolean
        get() = this.elseBlock.isNotEmpty()


    override fun addOp(): MethodVisitor = this.mv.also {

        // English comment: if both branches are empty, just pop condition (IFEQ consumes it) and return.
        if (!this.hasThen && !this.hasElse)
        {
            val endL = Label()
            it.visitJumpInsn(Opcodes.IFEQ, endL) // consumes cond
            it.visitLabel(endL)
            return@also
        }

        // English comment: if only else branch exists, invert branch: if (cond != 0) goto end else emit else.
        if (!this.hasThen)
        {
            val endL = Label()
            it.visitJumpInsn(Opcodes.IFNE, endL) // true -> skip else (consumes cond)
            this.elseBlock.forEach { oP -> oP.addOp() }
            it.visitLabel(endL)
            return@also
        }

        // English comment: if only then branch exists, normal form: if (cond == 0) goto end else emit then.
        if (!this.hasElse)
        {
            val endL = Label()
            it.visitJumpInsn(Opcodes.IFEQ, endL) // false -> end (consumes cond)
            this.thenBlock.forEach { oP -> oP.addOp() }
            it.visitLabel(endL)
            return@also
        }

        // English comment: both branches exist, full if-else control flow.
        val elseL = Label()
        val endL  = Label()
        it.visitJumpInsn(Opcodes.IFEQ, elseL) // false -> else (consumes cond)
        this.thenBlock.forEach { oP -> oP.addOp() }
        it.visitJumpInsn(Opcodes.GOTO, endL)
        it.visitLabel(elseL)
        this.elseBlock.forEach { oP -> oP.addOp() }
        it.visitLabel(endL)
    }
}
