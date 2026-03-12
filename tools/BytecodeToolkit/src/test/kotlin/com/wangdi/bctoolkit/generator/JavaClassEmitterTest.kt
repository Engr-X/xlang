package com.wangdi.bctoolkit.generator

import com.wangdi.bctoolkit.base.Access
import com.wangdi.bctoolkit.base.Type
import com.wangdi.bctoolkit.base.TypeRef
import xlang.annotation.OwnerTypeMetadata
import org.objectweb.asm.AnnotationVisitor
import org.objectweb.asm.ClassReader
import org.objectweb.asm.ClassVisitor
import org.objectweb.asm.FieldVisitor
import org.objectweb.asm.Opcodes
import java.nio.file.Files
import java.nio.file.Path
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class JavaClassEmitterTest
{
    @Test
    fun saveCreatesClassFile()
    {
        val emitter = ClassEmitter(
            Opcodes.V1_8,
            mutableListOf(Access.Public, Access.Super),
            TypeRef("TestC")
        )

        val method: MethodEmitter = emitter.newMethod(
            mutableListOf(Access.Public, Access.Static),
            "m",
            Type.VOID to mutableListOf()
        )
        method.Builder().apply { `return`() }.build()
        emitter.addMethod(method)

        val outDir: Path = Files.createTempDirectory("bcg-test-c")
        emitter.save(outDir)
        assertTrue(Files.exists(outDir.resolve("TestC.class")))
    }

    @Test
    fun customInitAndClinitWork()
    {
        val emitter = ClassEmitter(
            Opcodes.V1_8,
            mutableListOf(Access.Public, Access.Super),
            TypeRef("TestD")
        )

        val clinit: ClinitEmitter = emitter.newClinit()
        clinit.Builder().apply { `return`() }.build()
        emitter.setClinit(clinit)

        val init: InitEmitter = emitter.newInit(Access.Public, mutableListOf())
        init.Builder().apply {
            aload(0)
            invokeSpecial(
                mutableListOf("java", "lang", "Object", "<init>"),
                Type.VOID to mutableListOf()
            )
            `return`()
        }.build()
        emitter.addInit(init)

        val outDir: Path = Files.createTempDirectory("bcg-test-d")
        emitter.save(outDir)
        assertTrue(Files.exists(outDir.resolve("TestD.class")))
    }

    @Test
    fun fieldWritesOwnerTypeMetadata()
    {
        val emitter = ClassEmitter(
            Opcodes.V1_8,
            mutableListOf(Access.Public, Access.Super),
            TypeRef("TestMetaF")
        )

        emitter.addAttribute(
            AttributeGenerator(
                emitter.getCW(),
                mutableListOf(Access.Public, Access.Static),
                "f",
                Type.INT32
            ).setOwnerType("xlang-top-level")
        )

        val outDir: Path = Files.createTempDirectory("bcg-test-meta-f")
        emitter.save(outDir)

        var ownerType: String? = null
        val classBytes = Files.readAllBytes(outDir.resolve("TestMetaF.class"))
        val reader = ClassReader(classBytes)
        val visitor = object : ClassVisitor(Opcodes.ASM9)
        {
            override fun visitField(
                access: Int,
                name: String?,
                descriptor: String?,
                signature: String?,
                value: Any?
            ): FieldVisitor?
            {
                if (name != "f")
                    return super.visitField(access, name, descriptor, signature, value)

                return object : FieldVisitor(Opcodes.ASM9)
                {
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
    fun fieldClassOwnerTypeSkipsMetadata()
    {
        val emitter = ClassEmitter(
            Opcodes.V1_8,
            mutableListOf(Access.Public, Access.Super),
            TypeRef("TestMetaFClass")
        )

        emitter.addAttribute(
            AttributeGenerator(
                emitter.getCW(),
                mutableListOf(Access.Public, Access.Static),
                "f",
                Type.INT32
            ).setOwnerType("xlang-class")
        )

        val outDir: Path = Files.createTempDirectory("bcg-test-meta-f-class")
        emitter.save(outDir)

        var foundOwnerTypeMetadata = false
        val classBytes = Files.readAllBytes(outDir.resolve("TestMetaFClass.class"))
        val reader = ClassReader(classBytes)
        val visitor = object : ClassVisitor(Opcodes.ASM9)
        {
            override fun visitField(
                access: Int,
                name: String?,
                descriptor: String?,
                signature: String?,
                value: Any?
            ): FieldVisitor?
            {
                if (name != "f")
                    return super.visitField(access, name, descriptor, signature, value)

                return object : FieldVisitor(Opcodes.ASM9)
                {
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
