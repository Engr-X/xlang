package com.wangdi.bctoolkit.reader

import com.wangdi.bctoolkit.base.Access


class JavaClass(
    private val belong: JavaPackage,
    private val access: Set<Access>,
    private val name: String,
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
}
