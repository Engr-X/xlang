package com.wangdi.bctoolkit.reader

import com.wangdi.bctoolkit.base.Access
import com.wangdi.bctoolkit.base.Type


class JavaField(
    private val belong: JavaClass,
    private val access: MutableList<Access>,
    private val type: Type,
    private val name: String,
)
{
}
