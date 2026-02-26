package com.wangdi.classgen.base

class TypeRef(private val name: MutableList<String>)
{
    constructor(vararg name: String) : this(name.toMutableList())

    companion object
    {
        val OBJECT = TypeRef("java", "lang", "Object")
        val OBJECT_FULL_NAME: String = OBJECT.getFullName("/")
    }

    fun getName(): String = this.name.last()

    fun getFullName(separator: String): String = this.name.joinToString(separator = separator)
}
