package com.wangdi.classgen.artifact.operation

import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes


// ---------- Numeric negation: -x ----------
class INeg(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.INEG)
    }
}

class LNeg(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.LNEG)
    }
}

class FNeg(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.FNEG)
    }
}

class DNeg(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.DNEG)
    }
}


// ---------- Bitwise not: ~x ----------
// JVM has no I NOT/L NOT, implement as x ^ -1
class IBitNot(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        // English comment: bitwise not for int: x -> x ^ -1.
        it.visitLdcInsn(-1)
        it.visitInsn(Opcodes.IXOR)
    }
}

class LBitNot(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        // English comment: bitwise not for long: x -> x ^ -1L.
        it.visitLdcInsn(-1L)
        it.visitInsn(Opcodes.LXOR)
    }
}


// ---------- Logical not: !x ----------
// Assumption: boolean is represented as int 0/1.
// Then: !x == x ^ 1
class BNot(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        // English comment: logical not for 0/1 boolean: x -> x ^ 1.
        it.visitInsn(Opcodes.ICONST_1)
        it.visitInsn(Opcodes.IXOR)
    }
}


// ---------- Unary plus: +x ----------
// No-op; emit nothing.
class UPlus(mv: MethodVisitor, labels: MutableMap<Int, Label>) : OP(mv, labels)
