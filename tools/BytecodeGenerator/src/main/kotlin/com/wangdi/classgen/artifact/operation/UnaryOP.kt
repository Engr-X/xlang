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

    override fun toString(tabs: Int): String = "${indent(tabs)}ineg"
}

class LNeg(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.LNEG)
    }

    override fun toString(tabs: Int): String = "${indent(tabs)}lneg"
}

class FNeg(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.FNEG)
    }

    override fun toString(tabs: Int): String = "${indent(tabs)}fneg"
}

class DNeg(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>
) : OP(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.DNEG)
    }

    override fun toString(tabs: Int): String = "${indent(tabs)}dneg"
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

    override fun toString(tabs: Int): String {
        val pad = indent(tabs)
        return "${pad}ldc -1\n${pad}ixor"
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

    override fun toString(tabs: Int): String {
        val pad = indent(tabs)
        return "${pad}ldc2_w -1\n${pad}lxor"
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

    override fun toString(tabs: Int): String {
        val pad = indent(tabs)
        return "${pad}iconst_1\n${pad}ixor"
    }
}


// ---------- Unary plus: +x ----------
// No-op; emit nothing.
class UPlus(mv: MethodVisitor, labels: MutableMap<Int, Label>) : OP(mv, labels)
{
    override fun toString(tabs: Int): String = "${indent(tabs)}uplus"
}
