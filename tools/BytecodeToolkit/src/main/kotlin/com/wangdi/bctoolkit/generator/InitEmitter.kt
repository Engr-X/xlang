package com.wangdi.bctoolkit.generator

import com.wangdi.bctoolkit.base.Access
import com.wangdi.bctoolkit.base.Type
import org.objectweb.asm.ClassWriter


class InitEmitter(
    cw: ClassWriter,
    access: Access,
    params: MutableList<Type> = mutableListOf(),
    signature: MutableList<String> = mutableListOf(),
    exceptions: MutableList<Type> = mutableListOf(),
) : MethodEmitter(
    cw,
    mutableListOf(access),
    "<init>",
    Type.VOID to params,
    signature,
    exceptions
)
{
    companion object
    {
        fun getEmpty(cw: ClassWriter): InitEmitter
        {
            val layout = InitEmitter(cw, Access.Public)
            layout.Builder().apply {
                aload(0)
                invokeSpecial(
                    mutableListOf("java", "lang", "Object", "<init>"),
                    Type.VOID to mutableListOf()
                )
                `return`()
            }.build()
            return layout
        }
    }
}
