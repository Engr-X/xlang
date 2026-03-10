package com.wangdi.bctoolkit.meta

import org.objectweb.asm.AnnotationVisitor
import org.objectweb.asm.FieldVisitor
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes
import org.objectweb.asm.Type as AsmType


@Target(AnnotationTarget.FIELD, AnnotationTarget.FUNCTION)
@Retention(AnnotationRetention.BINARY)
annotation class Metadata(val value: String)


object OwnerTypeMetadata
{
    const val CLASS: String = "class"
    const val WRAPPED_CLASS: String = "wrapped-class"

    private const val CLASS_WRAPPED_ALIAS: String = "class-wrapped"
    private const val VALUE_KEY: String = "value"

    val DESC: String = AsmType.getDescriptor(Metadata::class.java)

    fun normalize(raw: String?): String = when (raw?.trim()?.lowercase())
    {
        null, "", CLASS -> CLASS
        WRAPPED_CLASS, CLASS_WRAPPED_ALIAS -> WRAPPED_CLASS
        else -> throw IllegalArgumentException("Unknown owner_type: $raw")
    }

    fun isOwnerTypeDescriptor(desc: String?): Boolean = desc == DESC

    fun visitAnnotationValue(name: String?, value: Any?, currentValue: String): String
    {
        if (name == VALUE_KEY && value is String)
            return normalize(value)

        return currentValue
    }

    fun write(fieldVisitor: FieldVisitor, ownerType: String)
    {
        val av = fieldVisitor.visitAnnotation(DESC, false) ?: return
        av.visit(VALUE_KEY, normalize(ownerType))
        av.visitEnd()
    }

    fun write(methodVisitor: MethodVisitor, ownerType: String)
    {
        val av = methodVisitor.visitAnnotation(DESC, false) ?: return
        av.visit(VALUE_KEY, normalize(ownerType))
        av.visitEnd()
    }

    fun reader(initialValue: String, onValue: (String) -> Unit): AnnotationVisitor =
        object : AnnotationVisitor(Opcodes.ASM9)
        {
            private var current: String = normalize(initialValue)

            override fun visit(name: String?, value: Any?)
            {
                current = visitAnnotationValue(name, value, current)
            }

            override fun visitEnd()
            {
                onValue(current)
            }
        }
}
