package com.wangdi.bctoolkit.reader

import com.wangdi.bctoolkit.base.Access


class JavaClass(
    private val belong: JavaPackage,
    private val access: MutableList<Access>,
    private val name: String,
    private val superClass: MutableList<String> = mutableListOf("java", "lang", "Object"),
    private val interfaces: MutableList<MutableList<String>> = mutableListOf(),

    private val staticField: MutableList<JavaField> = mutableListOf(),
    private val instanceField: MutableList<JavaField> = mutableListOf(),

    private val staticMethod: MutableList<JavaFunction> = mutableListOf(),
    private val instanceMethod: MutableList<JavaFunction> = mutableListOf()
)
{

}
