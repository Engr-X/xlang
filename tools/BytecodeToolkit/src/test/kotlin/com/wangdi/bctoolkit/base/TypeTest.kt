package com.wangdi.bctoolkit.base

import kotlin.test.Test
import kotlin.test.assertEquals

class TypeTest
{
    @Test
    fun getNameForPrimitiveInt()
    {
        assertEquals("I", Type.INT32.getName())
    }

    @Test
    fun getNameForObjectArray()
    {
        val t = Type(TypeRef("java", "lang", "String"), 1)
        assertEquals("[Ljava/lang/String;", t.getName())
    }
}
