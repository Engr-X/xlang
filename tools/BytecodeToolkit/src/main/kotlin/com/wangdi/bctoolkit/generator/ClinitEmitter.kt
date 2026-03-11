package com.wangdi.bctoolkit.generator

import com.wangdi.bctoolkit.artifact.Emitter
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

    override fun generate(): Emitter = this.apply {
        // JVM requires <clinit>()V to terminate with RETURN on all fall-through paths.
        this.Builder().`return`().build()
        super.generate()
    }
}

