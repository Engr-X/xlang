package com.diwang.bctoolkit.generator.artifact.operation

import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes


class OpBlock(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int,
    private val blockOps: MutableList<Instruction>
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        return this.mv.also {
            it.visitLabel(label)
            this.blockOps.forEach { op -> op.addOp() }
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
        val pad: String = this.indent(tabs)
        if (this.blockOps.isEmpty())
            return "${pad}.L${this.blockId}"
        val body = this.blockOps.joinToString("\n") { it.toString(tabs + 1) }
        return "${pad}.L${this.blockId}\n${body}"
    }
}


class Goto(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        it.visitJumpInsn(Opcodes.GOTO, label)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}goto .L${this.blockId}"
}


// int if
@Suppress("ClassName")
class If_icmpeq(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        it.visitJumpInsn(Opcodes.IF_ICMPEQ, label)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}if_icmpeq .L${this.blockId}"
}


@Suppress("ClassName")
class If_icmpne(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        it.visitJumpInsn(Opcodes.IF_ICMPNE, label)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}if_icmpne .L${this.blockId}"
}


@Suppress("ClassName")
class If_icmplt(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        it.visitJumpInsn(Opcodes.IF_ICMPLT, label)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}if_icmplt .L${this.blockId}"
}


@Suppress("ClassName")
class If_icmple(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        it.visitJumpInsn(Opcodes.IF_ICMPLE, label)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}if_icmple .L${this.blockId}"
}


@Suppress("ClassName")
class If_icmpgt(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        it.visitJumpInsn(Opcodes.IF_ICMPGT, label)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}if_icmpgt .L${this.blockId}"
}


@Suppress("ClassName")
class If_icmpge(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        it.visitJumpInsn(Opcodes.IF_ICMPGE, label)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}if_icmpge .L${this.blockId}"
}


// long if
@Suppress("ClassName")
class If_lcmpeq(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        it.visitInsn(Opcodes.LCMP)
        it.visitJumpInsn(Opcodes.IFEQ, label)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}if_lcmpeq .L${this.blockId}"
}


@Suppress("ClassName")
class If_lcmpne(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        it.visitInsn(Opcodes.LCMP)
        it.visitJumpInsn(Opcodes.IFNE, label)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}if_lcmpne .L${this.blockId}"
}


@Suppress("ClassName")
class If_lcmplt(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        it.visitInsn(Opcodes.LCMP)
        it.visitJumpInsn(Opcodes.IFLT, label)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}if_lcmplt .L${this.blockId}"
}


@Suppress("ClassName")
class If_lcmple(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        it.visitInsn(Opcodes.LCMP)
        it.visitJumpInsn(Opcodes.IFLE, label)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}if_lcmple .L${this.blockId}"
}


@Suppress("ClassName")
class If_lcmpgt(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        it.visitInsn(Opcodes.LCMP)
        it.visitJumpInsn(Opcodes.IFGT, label)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}if_lcmpgt .L${this.blockId}"
}


@Suppress("ClassName")
class If_lcmpge(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        it.visitInsn(Opcodes.LCMP)
        it.visitJumpInsn(Opcodes.IFGE, label)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}if_lcmpge .L${this.blockId}"
}


// float if
@Suppress("ClassName")
class If_fcmpeq(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        it.visitInsn(Opcodes.FCMPL)
        it.visitJumpInsn(Opcodes.IFEQ, label)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}if_fcmpeq .L${this.blockId}"
}


@Suppress("ClassName")
class If_fcmpne(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        it.visitInsn(Opcodes.FCMPL)
        it.visitJumpInsn(Opcodes.IFNE, label)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}if_fcmpne .L${this.blockId}"
}


@Suppress("ClassName")
class If_fcmplt(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        it.visitInsn(Opcodes.FCMPG)
        it.visitJumpInsn(Opcodes.IFLT, label)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}if_fcmplt .L${this.blockId}"
}


@Suppress("ClassName")
class If_fcmple(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        it.visitInsn(Opcodes.FCMPG)
        it.visitJumpInsn(Opcodes.IFLE, label)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}if_fcmple .L${this.blockId}"
}


@Suppress("ClassName")
class If_fcmpgt(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        it.visitInsn(Opcodes.FCMPL)
        it.visitJumpInsn(Opcodes.IFGT, label)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}if_fcmpgt .L${this.blockId}"
}


@Suppress("ClassName")
class If_fcmpge(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        it.visitInsn(Opcodes.FCMPL)
        it.visitJumpInsn(Opcodes.IFGE, label)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}if_fcmpge .L${this.blockId}"
}


class Return(
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
        it.visitInsn(Opcodes.RETURN)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}return"
}


// double if
@Suppress("ClassName")
class If_dcmpeq(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        it.visitInsn(Opcodes.DCMPL)
        it.visitJumpInsn(Opcodes.IFEQ, label)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}if_dcmpeq .L${this.blockId}"
}


@Suppress("ClassName")
class If_dcmpne(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        it.visitInsn(Opcodes.DCMPL)
        it.visitJumpInsn(Opcodes.IFNE, label)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}if_dcmpne .L${this.blockId}"
}


@Suppress("ClassName")
class If_dcmplt(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        it.visitInsn(Opcodes.DCMPG)
        it.visitJumpInsn(Opcodes.IFLT, label)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}if_dcmplt .L${this.blockId}"
}


@Suppress("ClassName")
class If_dcmple(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        it.visitInsn(Opcodes.DCMPG)
        it.visitJumpInsn(Opcodes.IFLE, label)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}if_dcmple .L${this.blockId}"
}


@Suppress("ClassName")
class If_dcmpgt(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        it.visitInsn(Opcodes.DCMPL)
        it.visitJumpInsn(Opcodes.IFGT, label)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}if_dcmpgt .L${this.blockId}"
}


@Suppress("ClassName")
class If_dcmpge(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val blockId: Int
) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        val label: Label = this.labels.getOrPut(this.blockId) { Label() }
        it.visitInsn(Opcodes.DCMPL)
        it.visitJumpInsn(Opcodes.IFGE, label)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}if_dcmpge .L${this.blockId}"
}


// other
class IReturn(
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
        it.visitInsn(Opcodes.IRETURN)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}ireturn"
}


class LReturn(
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
        it.visitInsn(Opcodes.LRETURN)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}lreturn"
}


class FReturn(
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
        it.visitInsn(Opcodes.FRETURN)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}freturn"
}


class DReturn(
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
        it.visitInsn(Opcodes.DRETURN)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}dreturn"
}



class Pop(
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
        it.visitInsn(Opcodes.POP)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}pop"
}


class Dup(
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
        it.visitInsn(Opcodes.DUP)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}dup"
}


class Nop(
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
        it.visitInsn(Opcodes.NOP)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}nop"
}

