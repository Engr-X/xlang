package com.wangdi.classgen.artifact.operation

import com.wangdi.classgen.base.Type
import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes


class GetStatic(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val fullName: MutableList<String>,
    private val type: Type
) : OP(mv, labels)
{
    private val name: String = this.fullName.last()

    private val owner: String = this.fullName.subList(0, this.fullName.size - 1).joinToString("/")

    private val desc: String = this.type.getName()

    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitFieldInsn(Opcodes.GETSTATIC, this.owner, this.name, this.desc)
    }

    override fun toString(tabs: Int): String =
        "${indent(tabs)}getstatic ${this.owner}/${this.name} ${this.desc}"
}


class PutStatic(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val fullName: MutableList<String>,
    private val type: Type
) : OP(mv, labels)
{
    private val name: String = this.fullName.last()

    private val owner: String = this.fullName.subList(0, this.fullName.size - 1).joinToString("/")

    private val desc: String = this.type.getName()

    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitFieldInsn(Opcodes.PUTSTATIC, this.owner, this.name, this.desc)
    }

    override fun toString(tabs: Int): String =
        "${indent(tabs)}putstatic ${this.owner}/${this.name} ${this.desc}"
}
