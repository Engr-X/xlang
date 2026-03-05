package com.wangdi.classgen.generator

import com.wangdi.classgen.base.Access
import com.wangdi.classgen.base.Type
import org.objectweb.asm.ClassWriter


class AttributeGenerator(
    private val cw: ClassWriter,
    private val access: MutableList<Access>,
    private val name: String,
    private val type: Type,
    private val signature: String? = null,
    private val value: Any? = null
)
{
    private fun accessOf(): Int = this.access.fold(0) { acc, f -> acc or f.flag }

    fun generate()
    {
        this.cw.visitField(
            this.accessOf(),
            this.name,
            this.type.getName(),
            this.signature,
            this.value).visitEnd()
    }
}