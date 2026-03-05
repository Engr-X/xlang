package com.wangdi.classgen.generator

import com.wangdi.classgen.base.Access
import com.wangdi.classgen.base.Type

import org.objectweb.asm.ClassWriter


class ClinitEmitter(
    cw: ClassWriter,
) : MethodEmitter(
    cw,
    mutableListOf(Access.Static),
    "<clinit>",
    Type.VOID to mutableListOf(),
    mutableListOf())
{
    companion object
    {
        fun getEmpty(cw: ClassWriter): ClinitEmitter
        {
            val layout = ClinitEmitter(cw)
            layout.Builder().`return`().build()
            return layout
        }
    }
}
