package com.wangdi.classgen.artifact.operation

import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes


// ---------- Numeric negation: -x ----------
class INeg(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>
) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.INEG)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}ineg"
}

class LNeg(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>
) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.LNEG)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}lneg"
}

class FNeg(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>
) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.FNEG)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}fneg"
}

class DNeg(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>
) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.DNEG)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}dneg"
}


// ---------- Bitwise not: ~x ----------
// JVM has no I NOT/L NOT, implement as x ^ -1
class IBitNot(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>
) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        // English comment: bitwise not for int: x -> x ^ -1.
        it.visitLdcInsn(-1)
        it.visitInsn(Opcodes.IXOR)
    }

    override fun toString(tabs: Int): String {
        val pad: String = this.indent(tabs)
        return "${pad}ldc -1\n${pad}ixor"
    }
}

class LBitNot(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>
) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        // English comment: bitwise not for long: x -> x ^ -1L.
        it.visitLdcInsn(-1L)
        it.visitInsn(Opcodes.LXOR)
    }

    override fun toString(tabs: Int): String {
        val pad: String = this.indent(tabs)
        return "${pad}ldc2_w -1\n${pad}lxor"
    }
}


// ---------- Unary plus: +x ----------
// No-op; emit nothing.
class UPlus(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun toString(tabs: Int): String = "${this.indent(tabs)}uplus"
}
