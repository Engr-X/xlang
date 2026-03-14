package com.wangdi.bctoolkit.generator

import com.wangdi.bctoolkit.generator.artifact.operation.OpBlock
import com.wangdi.bctoolkit.base.Access
import com.wangdi.bctoolkit.base.Type
import xlang.annotation.OwnerTypeMetadata
import org.objectweb.asm.AnnotationVisitor
import org.objectweb.asm.ClassReader
import org.objectweb.asm.ClassVisitor
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes
import kotlin.test.Test
import kotlin.test.assertEquals

class MethodEmitterTest
{
    @Test
    /**
     * Auto-generated baseline docs for builderBuildsAndGenerate.
     * Describes the intent and behavior of this function.
     * @param Parameters are described by the function signature.`r`n * @return Return value follows the function signature.
     */
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
    /**
     * Auto-generated baseline docs for blockBuilderProducesBlockOp.
     * Describes the intent and behavior of this function.
     * @param Parameters are described by the function signature.`r`n * @return Return value follows the function signature.
     */
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

    @Test
    /**
     * Auto-generated baseline docs for methodWritesOwnerTypeMetadata.
     * Describes the intent and behavior of this function.
     * @param Parameters are described by the function signature.`r`n * @return Return value follows the function signature.
     */
    fun methodWritesOwnerTypeMetadata()
    {
        val cw = ClassWriter(ClassWriter.COMPUTE_FRAMES or ClassWriter.COMPUTE_MAXS)
        cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, "TestMetaM", null, "java/lang/Object", null)

        val method = MethodEmitter(
            cw,
            mutableListOf(Access.Public, Access.Static),
            "m",
            Type.VOID to mutableListOf()
        ).setOwnerType("xlang-top-level")

        method.Builder().apply { `return`() }.build()
        method.generate()

        cw.visitEnd()

        var ownerType: String? = null
        val reader = ClassReader(cw.toByteArray())
        val visitor = object : ClassVisitor(Opcodes.ASM9)
        {
            /**
             * Auto-generated baseline docs for visitMethod.
             * Describes the intent and behavior of this function.
             * @param Parameters are described by the function signature.`r`n * @return Return value follows the function signature.
             */
            override fun visitMethod(
                access: Int,
                name: String,
                descriptor: String?,
                signature: String?,
                exceptions: Array<out String>?
            ): MethodVisitor?
            {
                if (name != "m")
                    return super.visitMethod(access, name, descriptor, signature, exceptions)

                return object : MethodVisitor(Opcodes.ASM9)
                {
                    /**
                     * Auto-generated baseline docs for visitAnnotation.
                     * Describes the intent and behavior of this function.
                     * @param Parameters are described by the function signature.`r`n * @return Return value follows the function signature.
                     */
                    override fun visitAnnotation(descriptor: String?, visible: Boolean): AnnotationVisitor?
                    {
                        if (!OwnerTypeMetadata.isOwnerTypeDescriptor(descriptor))
                            return super.visitAnnotation(descriptor, visible)

                        return OwnerTypeMetadata.reader(OwnerTypeMetadata.CLASS) { parsed -> ownerType = parsed }
                    }
                }
            }
        }

        reader.accept(visitor, 0)

        assertEquals(OwnerTypeMetadata.TOP_LEVEL, ownerType)
    }

    @Test
    /**
     * Auto-generated baseline docs for classOwnerTypeSkipsMetadata.
     * Describes the intent and behavior of this function.
     * @param Parameters are described by the function signature.`r`n * @return Return value follows the function signature.
     */
    fun classOwnerTypeSkipsMetadata()
    {
        val cw = ClassWriter(ClassWriter.COMPUTE_FRAMES or ClassWriter.COMPUTE_MAXS)
        cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, "TestMetaMClass", null, "java/lang/Object", null)

        val method = MethodEmitter(
            cw,
            mutableListOf(Access.Public, Access.Static),
            "m",
            Type.VOID to mutableListOf()
        ).setOwnerType("xlang-class")

        method.Builder().apply { `return`() }.build()
        method.generate()
        cw.visitEnd()

        var foundOwnerTypeMetadata = false
        val reader = ClassReader(cw.toByteArray())
        val visitor = object : ClassVisitor(Opcodes.ASM9)
        {
            /**
             * Auto-generated baseline docs for visitMethod.
             * Describes the intent and behavior of this function.
             * @param Parameters are described by the function signature.`r`n * @return Return value follows the function signature.
             */
            override fun visitMethod(
                access: Int,
                name: String,
                descriptor: String?,
                signature: String?,
                exceptions: Array<out String>?
            ): MethodVisitor?
            {
                if (name != "m")
                    return super.visitMethod(access, name, descriptor, signature, exceptions)

                return object : MethodVisitor(Opcodes.ASM9)
                {
                    /**
                     * Auto-generated baseline docs for visitAnnotation.
                     * Describes the intent and behavior of this function.
                     * @param Parameters are described by the function signature.`r`n * @return Return value follows the function signature.
                     */
                    override fun visitAnnotation(descriptor: String?, visible: Boolean): AnnotationVisitor?
                    {
                        if (OwnerTypeMetadata.isOwnerTypeDescriptor(descriptor))
                            foundOwnerTypeMetadata = true
                        return super.visitAnnotation(descriptor, visible)
                    }
                }
            }
        }

        reader.accept(visitor, 0)
        assertEquals(false, foundOwnerTypeMetadata)
    }
}

