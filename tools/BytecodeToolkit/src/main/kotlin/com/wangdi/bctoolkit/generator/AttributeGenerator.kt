package com.wangdi.bctoolkit.generator

import com.wangdi.bctoolkit.base.Access
import com.wangdi.bctoolkit.base.Type
import xlang.annotation.OwnerTypeMetadata
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
    private var ownerType: String = OwnerTypeMetadata.CLASS

    private fun accessOf(): Int = this.access.fold(0) { acc, f -> acc or f.flag }

    fun setOwnerType(type: String): AttributeGenerator =
        this.apply { this.ownerType = OwnerTypeMetadata.normalize(type) }

    fun generate()
    {
        val fv = this.cw.visitField(
            this.accessOf(),
            this.name,
            this.type.getName(),
            this.signature,
            this.value)
        OwnerTypeMetadata.write(fv, this.ownerType)
        fv.visitEnd()
    }
}

