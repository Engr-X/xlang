package com.diwang.bctoolkit.base

import kotlin.test.Test
import kotlin.test.assertEquals

class TypeTest
{
    @Test
    /**
     * Auto-generated baseline docs for getNameForPrimitiveInt.
     * Describes the intent and behavior of this function.
     * @param Parameters are described by the function signature.`r`n * @return Return value follows the function signature.
     */
    fun getNameForPrimitiveInt()
    {
        assertEquals("I", Type.INT32.getName())
    }

    @Test
    /**
     * Auto-generated baseline docs for getNameForObjectArray.
     * Describes the intent and behavior of this function.
     * @param Parameters are described by the function signature.`r`n * @return Return value follows the function signature.
     */
    fun getNameForObjectArray()
    {
        val t = Type(TypeRef("java", "lang", "String"), 1)
        assertEquals("[Ljava/lang/String;", t.getName())
    }
}

