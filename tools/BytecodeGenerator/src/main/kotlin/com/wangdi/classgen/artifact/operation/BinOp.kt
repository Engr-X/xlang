package com.wangdi.classgen.artifact.operation

import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes


// ---------- Int arithmetic ----------
class IAdd(mv: MethodVisitor, labels: MutableMap<Int, Label>) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.IADD)
    }
}


class ISub(mv: MethodVisitor, labels: MutableMap<Int, Label>) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.ISUB)
    }
}


class IMul(mv: MethodVisitor, labels: MutableMap<Int, Label>) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.IMUL)
    }
}


class IDiv(mv: MethodVisitor, labels: MutableMap<Int, Label>) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.IDIV)
    }
}


class IRem(mv: MethodVisitor, labels: MutableMap<Int, Label>) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        // English comment: pop two ints, push remainder (a % b).
        it.visitInsn(Opcodes.IREM)
    }
}


// ---------- Long arithmetic ----------
class LAdd(mv: MethodVisitor, labels: MutableMap<Int, Label>) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.LADD)
    }
}


class LSub(mv: MethodVisitor, labels: MutableMap<Int, Label>) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.LSUB)
    }
}


class LMul(mv: MethodVisitor, labels: MutableMap<Int, Label>) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.LMUL)
    }
}


class LDiv(mv: MethodVisitor, labels: MutableMap<Int, Label>) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        // English comment: pop two longs, push their quotient.
        it.visitInsn(Opcodes.LDIV)
    }
}


class LRem(mv: MethodVisitor, labels: MutableMap<Int, Label>) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        // English comment: pop two longs, push remainder (a % b).
        it.visitInsn(Opcodes.LREM)
    }
}


// ---------- Float arithmetic ----------
class FAdd(mv: MethodVisitor, labels: MutableMap<Int, Label>) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.FADD)
    }
}


class FSub(mv: MethodVisitor, labels: MutableMap<Int, Label>) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.FSUB)
    }
}


class FMul(mv: MethodVisitor, labels: MutableMap<Int, Label>) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.FMUL)
    }
}


class FDiv(mv: MethodVisitor, labels: MutableMap<Int, Label>) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.FDIV)
    }
}


// ---------- Double arithmetic ----------
class DAdd(mv: MethodVisitor, labels: MutableMap<Int, Label>) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.DADD)
    }
}


class DSub(mv: MethodVisitor, labels: MutableMap<Int, Label>) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.DSUB)
    }
}


class DMul(mv: MethodVisitor, labels: MutableMap<Int, Label>) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.DMUL)
    }
}


class DDiv(mv: MethodVisitor, labels: MutableMap<Int, Label>) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.DDIV)
    }
}
