package com.wangdi.bctoolkit.generator

import com.wangdi.bctoolkit.base.Access
import com.wangdi.bctoolkit.base.Type

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
