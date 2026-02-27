package com.wangdi.classgen.artifact.operation

import com.wangdi.classgen.base.Type

import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes


class StaticCall(
    mv: MethodVisitor,
    labels: MutableMap<Int, Label>,
    private val fullName: MutableList<String>,
    private val signature: Pair<Type, MutableList<Type>>) : OP(mv, labels)
{
    private val name: String = this.fullName.last()

    private val owner: String = this.fullName.subList(0, this.fullName.size - 1).joinToString("/")

    private val desc: String = buildString {
        append('(')
        for (t in this@StaticCall.signature.second)
            append(t.getName())
        append(')')
        append(this@StaticCall.signature.first.getName())
    }


    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitMethodInsn(
            Opcodes.INVOKESTATIC,
            this.owner,
            this.name,
            this.desc,
            false
        )
    }
}
