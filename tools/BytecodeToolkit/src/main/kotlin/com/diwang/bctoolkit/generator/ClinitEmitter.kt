package com.diwang.bctoolkit.generator

import com.wangdi.bctoolkit.generator.artifact.Emitter
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
        /**
         * Auto-generated baseline docs for getEmpty.
         * Describes the intent and behavior of this function.
         *
         * @param cw parameter from function signature.
         * @return return value of this function.
         */
        fun getEmpty(cw: ClassWriter): ClinitEmitter
        {
            val layout = ClinitEmitter(cw)
            layout.Builder().`return`().build()
            return layout
        }
    }

    /**
     * Auto-generated baseline docs for generate.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun generate(): Emitter = this.apply {
        // JVM requires <clinit>()V to terminate with RETURN on all fall-through paths.
        this.Builder().`return`().build()
        super.generate()
    }
}


