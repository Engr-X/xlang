package com.diwang.bctoolkit.reader

import com.wangdi.bctoolkit.base.Access
import com.wangdi.bctoolkit.base.TypeRef
import xlang.annotation.OwnerTypeMetadata


class JavaField(
    private val belong: JavaClass,
    private val access: Set<Access>,
    private val type: TypeRef,
    private val name: String,
    private val ownerType: String = OwnerTypeMetadata.CLASS,
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
    fun toDto(): JavaFieldDto =
        JavaFieldDto(
            this.access.map { it.name }.sorted(),
            this.type.getName(),
            this.name,
            this.ownerType)

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun toString(): String = "${this.belong.getName()}:" +
            "${this.access.toList().sorted()} ${this.type} ${this.name} metadata(${this.ownerType})"
}

