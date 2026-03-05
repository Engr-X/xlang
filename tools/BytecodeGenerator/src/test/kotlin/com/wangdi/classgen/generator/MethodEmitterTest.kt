package com.wangdi.classgen.generator

import com.wangdi.classgen.artifact.operation.OpBlock
import com.wangdi.classgen.base.Access
import com.wangdi.classgen.base.Type
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes
import kotlin.test.Test

class MethodEmitterTest
{
    @Test
    fun builderBuildsAndGenerate()
    {
        val cw = ClassWriter(ClassWriter.COMPUTE_FRAMES or ClassWriter.COMPUTE_MAXS)
        cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, "TestE", null, "java/lang/Object", null)

        val method = MethodEmitter(
            cw,
            mutableListOf(Access.Public, Access.Static),
            "m",
            Type.VOID to mutableListOf()
        )
        method.Builder().apply { `return`() }.build()
        method.generate()

        cw.visitEnd()
        cw.toByteArray()
    }

    @Test
    fun blockBuilderProducesBlockOp()
    {
        val cw = ClassWriter(ClassWriter.COMPUTE_FRAMES or ClassWriter.COMPUTE_MAXS)
        cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, "TestF", null, "java/lang/Object", null)

        val method = MethodEmitter(
            cw,
            mutableListOf(Access.Public, Access.Static),
            "m",
            Type.VOID to mutableListOf()
        )

        val blockBuilder = method.BlockBuilder(1)
        blockBuilder.nop()
        val block: OpBlock = blockBuilder.buildBlock() as OpBlock

        method.Builder().apply {
            block(block)
            `return`()
        }.build()

        method.generate()
        cw.visitEnd()
        cw.toByteArray()
    }
}
