package xlang.annotation

import org.objectweb.asm.AnnotationVisitor
import org.objectweb.asm.FieldVisitor
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes


object OwnerTypeMetadata
{
    const val CLASS: String = "xlang-class"
    const val TOP_LEVEL: String = "xlang-top-level"

    private const val VALUE_KEY: String = "value"

    private const val DESC_CURRENT: String = "Lxlang/annotation/Metadata;"

    val DESC: String = DESC_CURRENT

    fun normalize(raw: String?): String = when (raw?.trim()?.lowercase())
    {
        null, "", CLASS -> CLASS
        TOP_LEVEL -> TOP_LEVEL
        else -> throw IllegalArgumentException("Unknown owner_type: $raw")
    }

    fun isOwnerTypeDescriptor(desc: String?): Boolean =
        desc == DESC_CURRENT

    fun visitAnnotationValue(name: String?, value: Any?, currentValue: String): String
    {
        if (name == VALUE_KEY && value is String)
            return normalize(value)

        return currentValue
    }

    fun write(fieldVisitor: FieldVisitor, ownerType: String)
    {
        val normalized = normalize(ownerType)
        if (normalized == CLASS)
            return

        val av = fieldVisitor.visitAnnotation(DESC_CURRENT, false) ?: return
        av.visit(VALUE_KEY, normalized)
        av.visitEnd()
    }

    fun write(methodVisitor: MethodVisitor, ownerType: String)
    {
        val normalized = normalize(ownerType)
        if (normalized == CLASS)
            return

        val av = methodVisitor.visitAnnotation(DESC_CURRENT, false) ?: return
        av.visit(VALUE_KEY, normalized)
        av.visitEnd()
    }

    fun reader(initialValue: String, onValue: (String) -> Unit): AnnotationVisitor =
        object : AnnotationVisitor(Opcodes.ASM9) {
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
