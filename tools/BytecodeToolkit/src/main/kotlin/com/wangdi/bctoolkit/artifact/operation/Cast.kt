package com.wangdi.bctoolkit.artifact.operation

import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes


// ---------- int casts ----------
class I2L(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.I2L)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}i2l"
}


class I2F(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.I2F)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}i2f"
}


class I2D(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.I2D)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}i2d"
}


// ---------- long casts ----------
class L2I(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.L2I)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}l2i"
}


class L2F(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.L2F)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}l2f"
}


class L2D(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.L2D)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}l2d"
}


// ---------- float casts ----------
class F2I(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.F2I)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}f2i"
}


class F2L(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.F2L)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}f2l"
}


class F2D(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.F2D)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}f2d"
}


// ---------- double casts ----------
class D2I(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.D2I)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}d2i"
}


class D2L(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.D2L)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}d2l"
}


class D2F(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.D2F)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}d2f"
}

