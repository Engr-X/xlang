package com.wangdi.classgen.generator

import com.wangdi.classgen.base.Access
import com.wangdi.classgen.base.Type
import com.wangdi.classgen.base.TypeRef
import org.objectweb.asm.Opcodes
import java.nio.file.Files
import java.nio.file.Path
import kotlin.test.Test
import kotlin.test.assertTrue

class ClassEmitterTest
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
}
