package com.wangdi.bctoolkit.reader

import com.wangdi.bctoolkit.base.Access
import com.wangdi.bctoolkit.base.TypeRef
import xlang.annotation.OwnerTypeMetadata

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable


class JavaField(
    private val belong: JavaClass,
    private val access: Set<Access>,
    private val type: TypeRef,
    private val name: String,
    private val ownerType: String = OwnerTypeMetadata.CLASS,
)
{
    fun isStatic(): Boolean = this.access.contains(Access.Static)

    fun toDto(): JavaFieldDto =
        JavaFieldDto(
            this.access.map { it.name }.sorted(),
            this.type.getName(),
            this.name,
            this.ownerType)

    override fun toString(): String = "${this.belong.getName()}:" +
            "${this.access.toList().sorted()} ${this.type} ${this.name} metadata(${this.ownerType})"
}

@Serializable
data class JavaFieldDto(
    val access: List<String>,
    val type: List<String>,
    val name: String,
    @SerialName("owner_type") val ownerType: String)

