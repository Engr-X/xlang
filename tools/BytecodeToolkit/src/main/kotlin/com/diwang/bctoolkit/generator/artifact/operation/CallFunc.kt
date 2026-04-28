package com.diwang.bctoolkit.generator.artifact.operation

import com.wangdi.bctoolkit.base.Type

import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes


class InvokeStatic(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val fullName: MutableList<String>,
    private val signature: Pair<Type, MutableList<Type>>) : Instruction(mv, labels)
{
    private val name: String = this.fullName.last()

    private val owner: String = this.fullName.subList(0, this.fullName.size - 1).joinToString("/")

    private val desc: String = buildString {
        append('(')
        for (t in this@InvokeStatic.signature.second)
            append(t.getName())
        append(')')
        append(this@InvokeStatic.signature.first.getName())
    }


    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitMethodInsn(
            Opcodes.INVOKESTATIC,
            this.owner,
            this.name,
            this.desc,
            false
        )
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String =
        "${this.indent(tabs)}invokestatic ${this.owner}/${this.name}${this.desc}"
}


class InvokeSpecial(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val fullName: MutableList<String>,
    private val signature: Pair<Type, MutableList<Type>>) : Instruction(mv, labels)
{
    private val name: String = this.fullName.last()

    private val owner: String = this.fullName.subList(0, this.fullName.size - 1).joinToString("/")

    private val desc: String = buildString {
        append('(')
        for (t in this@InvokeSpecial.signature.second)
            append(t.getName())
        append(')')
        append(this@InvokeSpecial.signature.first.getName())
    }


    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitMethodInsn(
            Opcodes.INVOKESPECIAL,
            this.owner,
            this.name,
            this.desc,
            false
        )
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String =
        "${this.indent(tabs)}invokespecial ${this.owner}/${this.name}${this.desc}"
}


class InvokeVirtual(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val fullName: MutableList<String>,
    private val signature: Pair<Type, MutableList<Type>>) : Instruction(mv, labels)
{
    private val name: String = this.fullName.last()

    private val owner: String = this.fullName.subList(0, this.fullName.size - 1).joinToString("/")

    private val desc: String = buildString {
        append('(')
        for (t in this@InvokeVirtual.signature.second)
            append(t.getName())
        append(')')
        append(this@InvokeVirtual.signature.first.getName())
    }

    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitMethodInsn(
            Opcodes.INVOKEVIRTUAL,
            this.owner,
            this.name,
            this.desc,
            false
        )
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String =
        "${this.indent(tabs)}invokevirtual ${this.owner}/${this.name}${this.desc}"
}

