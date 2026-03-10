package com.wangdi.bctoolkit.base

import kotlin.test.Test
import kotlin.test.assertEquals

class TypeRefTest
{
    @Test
    fun safeCreateEmptyUsesObject()
    {
        val ref: TypeRef = TypeRef.safeCreate(mutableListOf())
        assertEquals("Object", ref.getLastName())
        assertEquals("java/lang/Object", ref.getFullName("/"))
    }

    @Test
    fun safeCreateNonEmptyKeepsParts()
    {
        val ref: TypeRef = TypeRef.safeCreate(mutableListOf("a", "b"))
        assertEquals("b", ref.getLastName())
        assertEquals("a.b", ref.getFullName("."))
    }

    @Test
    fun getNameReturnsLastPart()
    {
        val ref = TypeRef("x", "y", "Z")
        assertEquals("Z", ref.getLastName())
    }

    @Test
    fun getFullNameUsesSeparator()
    {
        val ref = TypeRef("x", "y", "Z")
        assertEquals("x/y/Z", ref.getFullName("/"))
        assertEquals("x.y.Z", ref.getFullName("."))
    }
}
