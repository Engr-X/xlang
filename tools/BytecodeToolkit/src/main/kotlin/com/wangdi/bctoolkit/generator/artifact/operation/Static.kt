package com.wangdi.bctoolkit.generator.artifact.operation

import com.wangdi.bctoolkit.base.Type
import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes


class GetStatic(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val fullName: MutableList<String>,
    private val type: Type
) : Instruction(mv, labels)
{
    private val name: String = this.fullName.last()

    private val owner: String = this.fullName.subList(0, this.fullName.size - 1).joinToString("/")

    private val desc: String = this.type.getName()

    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitFieldInsn(Opcodes.GETSTATIC, this.owner, this.name, this.desc)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String =
        "${this.indent(tabs)}getstatic ${this.owner}/${this.name} ${this.desc}"
}


class PutStatic(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val fullName: MutableList<String>,
    private val type: Type
) : Instruction(mv, labels)
{
    private val name: String = this.fullName.last()

    private val owner: String = this.fullName.subList(0, this.fullName.size - 1).joinToString("/")

    private val desc: String = this.type.getName()

    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitFieldInsn(Opcodes.PUTSTATIC, this.owner, this.name, this.desc)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String =
        "${this.indent(tabs)}putstatic ${this.owner}/${this.name} ${this.desc}"
}

