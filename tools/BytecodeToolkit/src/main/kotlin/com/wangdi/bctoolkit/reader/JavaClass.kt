package com.wangdi.bctoolkit.reader

import com.wangdi.bctoolkit.base.Access


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
    /**
     * Auto-generated baseline docs for addFunction.
     * Describes the intent and behavior of this function.
     *
     * @param method parameter from function signature.
     * @return return value of this function.
     */
    fun addFunction(method: JavaMethod): JavaClass = this.apply {
        if (method.isStatic())
            this.staticMethod.add(method)
        else
            this.instanceMethod.add(method)
    }

    /**
     * Auto-generated baseline docs for addField.
     * Describes the intent and behavior of this function.
     *
     * @param field parameter from function signature.
     * @return return value of this function.
     */
    fun addField(field: JavaField): JavaClass = this.apply {
        if (field.isStatic())
            this.staticField.add(field)
        else
            this.instanceField.add(field)
    }

    /**
     * Auto-generated baseline docs for toDto.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
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

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun toString(): String =
        "${this.belong.path.joinToString(".")}:${this.access.toList().sorted()} " +
                "${this.name.joinToString(".")} extends ${this.superClass.joinToString(".")} " +
                "implements [${this.interfaces.joinToString(", ") { it.joinToString(".") }}] " +
                "fields(static=${this.staticField.size}, instance=${this.instanceField.size}) " +
                "methods(static=${this.staticMethod.size}, instance=${this.instanceMethod.size})"

    /**
     * Auto-generated baseline docs for getName.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    fun getName(): String = this.name.last()
}

