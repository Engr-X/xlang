package com.wangdi.classgen.base

import org.objectweb.asm.Opcodes


enum class Access(val flag: Int)
{
    Public(Opcodes.ACC_PUBLIC),
    Private(Opcodes.ACC_PRIVATE),
    Protected(Opcodes.ACC_PROTECTED),

    Static(Opcodes.ACC_STATIC),
    Final(Opcodes.ACC_FINAL),
    Abstract(Opcodes.ACC_ABSTRACT),

    Interface(Opcodes.ACC_INTERFACE),
    Enum(Opcodes.ACC_ENUM),
    Super(Opcodes.ACC_SUPER),
}
