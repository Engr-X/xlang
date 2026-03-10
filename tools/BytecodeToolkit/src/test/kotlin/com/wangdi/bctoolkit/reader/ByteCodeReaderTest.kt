package com.wangdi.bctoolkit.reader

import com.wangdi.bctoolkit.base.Access
import com.wangdi.bctoolkit.base.Type
import com.wangdi.bctoolkit.base.TypeRef
import com.wangdi.bctoolkit.generator.ClassEmitter
import org.objectweb.asm.Opcodes
import java.nio.file.Files
import java.nio.file.Path
import java.util.zip.ZipEntry
import java.util.zip.ZipOutputStream
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class ByteCodeReaderTest
{
    @Test
    fun readJmodReadsClassesUnderClassesDir()
    {
        val outDir: Path = Files.createTempDirectory("bctoolkit-jmod-read")
        val className = "JmodReadTestC"

        val emitter = ClassEmitter(
            Opcodes.V1_8,
            mutableListOf(Access.Public, Access.Super),
            TypeRef(className)
        )
        val method = emitter.newMethod(
            mutableListOf(Access.Public, Access.Static),
            "m",
            Type.VOID to mutableListOf()
        )
        method.Builder().apply { `return`() }.build()
        emitter.addMethod(method)
        emitter.save(outDir)

        val classFile = outDir.resolve("$className.class")
        val classBytes = Files.readAllBytes(classFile)
        val jmodFile = outDir.resolve("test.jmod")

        ZipOutputStream(Files.newOutputStream(jmodFile)).use { zip ->
            zip.putNextEntry(ZipEntry("classes/$className.class"))
            zip.write(classBytes)
            zip.closeEntry()

            zip.putNextEntry(ZipEntry("bin/readme.txt"))
            zip.write("ignored".toByteArray())
            zip.closeEntry()
        }

        val classes = ByteCodeReader.readJmod(jmodFile)
        assertEquals(1, classes.size)
        assertEquals(className, classes[0].toDto().name)
        assertTrue(classes[0].toDto().packagePath.isEmpty())
    }
}
