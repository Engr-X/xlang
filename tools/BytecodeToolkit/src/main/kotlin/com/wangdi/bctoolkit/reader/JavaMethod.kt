package com.wangdi.bctoolkit.reader

import com.wangdi.bctoolkit.base.Access
import com.wangdi.bctoolkit.base.TypeRef


class JavaMethod(
    private val belong: JavaClass,
    private val access: Set<Access>,
    private val name: String,
    private val funSignature: Pair<TypeRef, MutableList<TypeRef>>
)
{
    fun isStatic(): Boolean = this.access.contains(Access.Static)
}
