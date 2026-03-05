package com.wangdi.classgen.artifact.operation

import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes


// ---------- Int arithmetic ----------
class IAdd(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.IADD)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}iadd"
}


class ISub(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.ISUB)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}isub"
}


class IMul(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.IMUL)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}imul"
}


class IDiv(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.IDIV)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}idiv"
}


class IRem(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        // English comment: pop two ints, push remainder (a % b).
        it.visitInsn(Opcodes.IREM)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}irem"
}


// ---------- Long arithmetic ----------
class LAdd(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.LADD)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}ladd"
}


class LSub(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.LSUB)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}lsub"
}


class LMul(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.LMUL)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}lmul"
}


class LDiv(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        // English comment: pop two longs, push their quotient.
        it.visitInsn(Opcodes.LDIV)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}ldiv"
}


class LRem(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        // English comment: pop two longs, push remainder (a % b).
        it.visitInsn(Opcodes.LREM)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}lrem"
}


// ---------- Float arithmetic ----------
class FAdd(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.FADD)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}fadd"
}


class FSub(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.FSUB)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}fsub"
}


class FMul(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.FMUL)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}fmul"
}


class FDiv(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.FDIV)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}fdiv"
}


// ---------- Double arithmetic ----------
class DAdd(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.DADD)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}dadd"
}


class DSub(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.DSUB)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}dsub"
}


class DMul(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.DMUL)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}dmul"
}


class DDiv(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.DDIV)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}ddiv"
}
