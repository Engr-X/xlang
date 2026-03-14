package com.wangdi.bctoolkit.generator.artifact

import com.wangdi.bctoolkit.base.Access
import com.wangdi.bctoolkit.base.Type
import com.wangdi.bctoolkit.base.TypeRef

import kotlin.collections.fold


abstract class Emitter
{
    /**
     * Auto-generated baseline docs for generate.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    open fun generate(): Emitter = this

    /**
     * Auto-generated baseline docs for accessOf.
     * Describes the intent and behavior of this function.
     *
     * @param access parameter from function signature.
     * @return return value of this function.
     */
    protected fun accessOf(access: MutableList<Access>): Int = access.fold(0) { acc, f -> acc or f.flag }

    /**
     * Auto-generated baseline docs for genSignature.
     * Describes the intent and behavior of this function.
     *
     * @param signature parameter from function signature.
     * @return return value of this function.
     */
    protected fun genSignature(signature: MutableList<String>): String?
    {
        if (signature.isEmpty())
            return null

        val typeParams = signature.joinToString(separator = "") { tv -> "${tv}:L${TypeRef.OBJECT_FULL_NAME};" }

        return "<$typeParams>L${TypeRef.OBJECT_FULL_NAME};"
    }

    /**
     * Auto-generated baseline docs for genExceptions.
     * Describes the intent and behavior of this function.
     *
     * @param exceptions parameter from function signature.
     * @return return value of this function.
     */
    protected fun genExceptions(exceptions: MutableList<Type>): Array<String> =
        exceptions.map { org.objectweb.asm.Type.getType(it.getName()).internalName }.toTypedArray()
}

