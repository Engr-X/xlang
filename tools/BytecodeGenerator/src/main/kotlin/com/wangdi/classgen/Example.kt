package com.wangdi.classgen

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes.*
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths


object GenSquare
{
    @JvmStatic
    fun main(args: Array<String>)
    {
        val cw = ClassWriter(ClassWriter.COMPUTE_FRAMES or ClassWriter.COMPUTE_MAXS)

        // target JVM bytecode version: V1_8 / V11 / V17 ...
        cw.visit(
            V1_8,
            ACC_PUBLIC or ACC_SUPER,
            "Square",
            null,
            "java/lang/Object",
            null
        )

        // public Square();
        run {
            val mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
            mv.visitCode()
            mv.visitVarInsn(ALOAD, 0)
            mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)
            mv.visitInsn(RETURN)
            mv.visitMaxs(0, 0) // COMPUTE_MAXS enabled
            mv.visitEnd()
        }

        // public static int add(int, int, long, Square);
        // descriptor: (IIJLSquare;)I
        run {
            val mv = cw.visitMethod(ACC_PUBLIC or ACC_STATIC, "add", "(IIJLSquare;)I", null, null)
            mv.visitCode()
            mv.visitVarInsn(ILOAD, 0)
            mv.visitVarInsn(ILOAD, 1)
            mv.visitInsn(IADD)
            mv.visitInsn(IRETURN)
            mv.visitMaxs(0, 0)
            mv.visitEnd()
        }

        // static void main(java.lang.String[]);
        // descriptor: ([Ljava/lang/String;)V
        run {
            val mv = cw.visitMethod(ACC_PUBLIC or ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null)
            mv.visitCode()

            mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
            mv.visitIntInsn(SIPUSH, 10000)
            mv.visitInsn(ICONST_2)
            mv.visitInsn(LCONST_1)
            mv.visitInsn(ACONST_NULL)
            mv.visitMethodInsn(INVOKESTATIC, "Square", "add", "(IIJLSquare;)I", false)
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(I)V", false)
            mv.visitInsn(RETURN)

            mv.visitMaxs(0, 0)
            mv.visitEnd()
        }

        cw.visitEnd()

        val bytes = cw.toByteArray()
        Files.write(Paths.get("Square.class"), bytes);
    }
}
