package com.wangdi.bctoolkit.artifact.operation

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes
import kotlin.test.Test
import kotlin.test.assertEquals


class OpSmokeTest
{
    private fun newMv(): Pair<MethodVisitor, MutableMap<Int, Label>>
    {
        val cw = ClassWriter(ClassWriter.COMPUTE_FRAMES or ClassWriter.COMPUTE_MAXS)
        cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, "OpTest", null, "java/lang/Object", null)
        val mv = cw.visitMethod(Opcodes.ACC_PUBLIC or Opcodes.ACC_STATIC, "m", "()V", null, null)
        mv.visitCode()
        return mv to mutableMapOf()
    }

    @Test
    fun addOpDoesNotThrow()
    {
        val pair: Pair<MethodVisitor, MutableMap<Int, Label>> = newMv()
        val mv: MethodVisitor = pair.first
        val labels: MutableMap<Int, Label> = pair.second
        val ops: List<Instruction> = listOf(
            IAdd(mv, labels),
            IPush(mv, labels, 1),
            ALoad(mv, labels, 0),
            Return(mv, labels)
        )
        ops.forEach { it.addOp() }
    }

    @Test
    fun toStringMatchesExpected()
    {
        val pair: Pair<MethodVisitor, MutableMap<Int, Label>> = newMv()
        val mv: MethodVisitor = pair.first
        val labels: MutableMap<Int, Label> = pair.second
        assertEquals("iadd", IAdd(mv, labels).toString())
        assertEquals("iconst_1", IPush(mv, labels, 1).toString())
        assertEquals("aload_0", ALoad(mv, labels, 0).toString())
        assertEquals("return", Return(mv, labels).toString())
    }
}
