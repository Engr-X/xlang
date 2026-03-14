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

    /**
     * Auto-generated baseline docs for normalize.
     * Describes the intent and behavior of this function.
     *
     * @param raw parameter from function signature.
     * @return return value of this function.
     */
    fun normalize(raw: String?): String = when (raw?.trim()?.lowercase())
    {
        null, "", CLASS -> CLASS
        TOP_LEVEL -> TOP_LEVEL
        else -> throw IllegalArgumentException("Unknown owner_type: $raw")
    }

    /**
     * Auto-generated baseline docs for isOwnerTypeDescriptor.
     * Describes the intent and behavior of this function.
     *
     * @param desc parameter from function signature.
     * @return return value of this function.
     */
    fun isOwnerTypeDescriptor(desc: String?): Boolean =
        desc == DESC_CURRENT

    /**
     * Auto-generated baseline docs for visitAnnotationValue.
     * Describes the intent and behavior of this function.
     *
     * @param name parameter from function signature.
     * @param value parameter from function signature.
     * @param currentValue parameter from function signature.
     * @return return value of this function.
     */
    fun visitAnnotationValue(name: String?, value: Any?, currentValue: String): String
    {
        if (name == VALUE_KEY && value is String)
            return normalize(value)

        return currentValue
    }

    /**
     * Auto-generated baseline docs for write.
     * Describes the intent and behavior of this function.
     *
     * @param fieldVisitor parameter from function signature.
     * @param ownerType parameter from function signature.
     */
    fun write(fieldVisitor: FieldVisitor, ownerType: String)
    {
        val normalized = normalize(ownerType)
        if (normalized == CLASS)
            return

        val av = fieldVisitor.visitAnnotation(DESC_CURRENT, false) ?: return
        av.visit(VALUE_KEY, normalized)
        av.visitEnd()
    }

    /**
     * Auto-generated baseline docs for write.
     * Describes the intent and behavior of this function.
     *
     * @param methodVisitor parameter from function signature.
     * @param ownerType parameter from function signature.
     */
    fun write(methodVisitor: MethodVisitor, ownerType: String)
    {
        val normalized = normalize(ownerType)
        if (normalized == CLASS)
            return

        val av = methodVisitor.visitAnnotation(DESC_CURRENT, false) ?: return
        av.visit(VALUE_KEY, normalized)
        av.visitEnd()
    }

    /**
     * Auto-generated baseline docs for reader.
     * Describes the intent and behavior of this function.
     *
     * @param initialValue parameter from function signature.
     * @param onValue parameter from function signature.
     * @return return value of this function.
     */
    fun reader(initialValue: String, onValue: (String) -> Unit): AnnotationVisitor =
        object : AnnotationVisitor(Opcodes.ASM9) {
            private var current: String = normalize(initialValue)

            /**
             * Auto-generated baseline docs for visit.
             * Describes the intent and behavior of this function.
             *
             * @param name parameter from function signature.
             * @param value parameter from function signature.
             */
            override fun visit(name: String?, value: Any?)
            {
                current = visitAnnotationValue(name, value, current)
            }

            /**
             * Auto-generated baseline docs for visitEnd.
             * Describes the intent and behavior of this function.
             */
            override fun visitEnd()
            {
                onValue(current)
            }
        }
}

