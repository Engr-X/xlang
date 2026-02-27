package com.wangdi.classgen.artifact

import com.wangdi.classgen.base.Access
import com.wangdi.classgen.base.Type
import com.wangdi.classgen.base.TypeRef

import org.objectweb.asm.ClassWriter

import kotlin.collections.fold


abstract class Layout(protected val cw: ClassWriter)
{
    abstract fun generate(): Layout

    protected fun accessOf(access: MutableList<Access>): Int = access.fold(0) { acc, f -> acc or f.flag }

    protected fun genSignature(signature: MutableList<String>): String?
    {
        if (signature.isEmpty())
            return null

        val typeParams = signature.joinToString(separator = "") { tv -> "${tv}:L${TypeRef.OBJECT_FULL_NAME};" }

        return "<$typeParams>L${TypeRef.OBJECT_FULL_NAME};"
    }

    protected fun genExceptions(exceptions: MutableList<Type>): Array<String> = exceptions.map { it.getName() }.toTypedArray()
}
