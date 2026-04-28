package com.diwang.bctoolkit.reader

import com.wangdi.bctoolkit.base.Access
import com.wangdi.bctoolkit.base.TypeRef
import xlang.annotation.OwnerTypeMetadata


class JavaMethod(
    private val belong: JavaClass,
    private val access: Set<Access>,
    private val name: String,
    private val funSignature: Pair<TypeRef, MutableList<TypeRef>>,
    private val ownerType: String = OwnerTypeMetadata.CLASS
)
{
    /**
     * Auto-generated baseline docs for isStatic.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    fun isStatic(): Boolean = this.access.contains(Access.Static)

    /**
     * Auto-generated baseline docs for toDto.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    fun toDto(): JavaMethodDto =
        JavaMethodDto(
            this.access.map { it.name }.sorted(),
            this.funSignature.first.getName(),
            this.name,
            this.funSignature.second.map { it.getName() },
            this.ownerType)

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun toString(): String =
        "${this.belong.getName()}:" +
                "${this.access.toList().sorted()} ${this.funSignature.first} " +
                "${this.name}(${this.funSignature.second.joinToString(", ")}) metadata(${this.ownerType})"
}

