package com.wangdi.bctoolkit.reader

import com.wangdi.bctoolkit.base.Access
import com.wangdi.bctoolkit.base.TypeRef
import xlang.annotation.OwnerTypeMetadata

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable


class JavaMethod(
    private val belong: JavaClass,
    private val access: Set<Access>,
    private val name: String,
    private val funSignature: Pair<TypeRef, MutableList<TypeRef>>,
    private val ownerType: String = OwnerTypeMetadata.CLASS
)
{
    fun isStatic(): Boolean = this.access.contains(Access.Static)

    fun toDto(): JavaMethodDto =
        JavaMethodDto(
            this.access.map { it.name }.sorted(),
            this.funSignature.first.getName(),
            this.name,
            this.funSignature.second.map { it.getName() },
            this.ownerType)

    override fun toString(): String =
        "${this.belong.getName()}:" +
                "${this.access.toList().sorted()} ${this.funSignature.first} " +
                "${this.name}(${this.funSignature.second.joinToString(", ")}) metadata(${this.ownerType})"
}

@Serializable
data class JavaMethodDto(
    val access: List<String>,
    @SerialName("return_type") val returnType: List<String>,
    val name: String,
    @SerialName("param_types") val params: List<List<String>>,
    @SerialName("owner_type") val ownerType: String)

