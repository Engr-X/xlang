package com.wangdi.bctoolkit.reader

import com.wangdi.bctoolkit.base.Access

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable


class JavaClass(
    private val belong: JavaPackage,
    private val access: Set<Access>,
    private val name: MutableList<String>,
    private val superClass: MutableList<String> = mutableListOf("java", "lang", "Object"),
    private val interfaces: MutableList<MutableList<String>> = mutableListOf(),

    private val staticField: MutableList<JavaField> = mutableListOf(),
    private val instanceField: MutableList<JavaField> = mutableListOf(),

    private val staticMethod: MutableList<JavaMethod> = mutableListOf(),
    private val instanceMethod: MutableList<JavaMethod> = mutableListOf()
)
{
    fun addFunction(method: JavaMethod): JavaClass = this.apply {
        if (method.isStatic())
            this.staticMethod.add(method)
        else
            this.instanceMethod.add(method)
    }

    fun addField(field: JavaField): JavaClass = this.apply {
        if (field.isStatic())
            this.staticField.add(field)
        else
            this.instanceField.add(field)
    }

    fun toDto(): JavaClassDto =
        JavaClassDto(
            packagePath = this.belong.path.toList(),
            access = this.access.map { it.name }.sorted(),
            name = this.getName(),
            superClass = this.superClass.toList(),
            interfaces = this.interfaces.map { it.toList() },
            staticField = this.staticField.map { it.toDto() },
            instanceField = this.instanceField.map { it.toDto() },
            staticMethod = this.staticMethod.map { it.toDto() },
            instanceMethod = this.instanceMethod.map { it.toDto() })

    override fun toString(): String =
        "${this.belong.path.joinToString(".")}:${this.access.toList().sorted()} " +
                "${this.name.joinToString(".")} extends ${this.superClass.joinToString(".")} " +
                "implements [${this.interfaces.joinToString(", ") { it.joinToString(".") }}] " +
                "fields(static=${this.staticField.size}, instance=${this.instanceField.size}) " +
                "methods(static=${this.staticMethod.size}, instance=${this.instanceMethod.size})"

    fun getName(): String = this.name.last()
}

@Serializable
data class JavaClassDto(
    @SerialName("package") val packagePath: List<String>,
    val access: List<String>,
    @SerialName("class") val name: String,
    @SerialName("super_class") val superClass: List<String>,
    val interfaces: List<List<String>>,
    
    @SerialName("static_fields") val staticField: List<JavaFieldDto>,
    @SerialName("instance_fields") val instanceField: List<JavaFieldDto>,
    @SerialName("static_methods") val staticMethod: List<JavaMethodDto>,
    @SerialName("instance_methods") val instanceMethod: List<JavaMethodDto>)
