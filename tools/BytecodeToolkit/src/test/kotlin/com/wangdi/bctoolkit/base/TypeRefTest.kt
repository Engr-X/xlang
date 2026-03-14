package com.wangdi.bctoolkit.base

import kotlin.test.Test
import kotlin.test.assertEquals

class TypeRefTest
{
    @Test
    /**
     * Auto-generated baseline docs for safeCreateEmptyUsesObject.
     * Describes the intent and behavior of this function.
     * @param Parameters are described by the function signature.`r`n * @return Return value follows the function signature.
     */
    fun safeCreateEmptyUsesObject()
    {
        val ref: TypeRef = TypeRef.safeCreate(mutableListOf())
        assertEquals("Object", ref.getLastName())
        assertEquals("java/lang/Object", ref.getFullName("/"))
    }

    @Test
    /**
     * Auto-generated baseline docs for safeCreateNonEmptyKeepsParts.
     * Describes the intent and behavior of this function.
     * @param Parameters are described by the function signature.`r`n * @return Return value follows the function signature.
     */
    fun safeCreateNonEmptyKeepsParts()
    {
        val ref: TypeRef = TypeRef.safeCreate(mutableListOf("a", "b"))
        assertEquals("b", ref.getLastName())
        assertEquals("a.b", ref.getFullName("."))
    }

    @Test
    /**
     * Auto-generated baseline docs for getNameReturnsLastPart.
     * Describes the intent and behavior of this function.
     * @param Parameters are described by the function signature.`r`n * @return Return value follows the function signature.
     */
    fun getNameReturnsLastPart()
    {
        val ref = TypeRef("x", "y", "Z")
        assertEquals("Z", ref.getLastName())
    }

    @Test
    /**
     * Auto-generated baseline docs for getFullNameUsesSeparator.
     * Describes the intent and behavior of this function.
     * @param Parameters are described by the function signature.`r`n * @return Return value follows the function signature.
     */
    fun getFullNameUsesSeparator()
    {
        val ref = TypeRef("x", "y", "Z")
        assertEquals("x/y/Z", ref.getFullName("/"))
        assertEquals("x.y.Z", ref.getFullName("."))
    }
}

