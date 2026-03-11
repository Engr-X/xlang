package xlang.annotation

object OwnerTypeMetadata
{
    const val CLASS: String = "class"
    const val WRAPPED_CLASS: String = "wrapped-class"

    private const val CLASS_WRAPPED_ALIAS: String = "class-wrapped"

    const val DESC: String = "Lxlang/annotation/Metadata;"

    fun normalize(raw: String?): String = when (raw?.trim()?.lowercase())
    {
        null, "", CLASS -> CLASS
        WRAPPED_CLASS, CLASS_WRAPPED_ALIAS -> WRAPPED_CLASS
        else -> throw IllegalArgumentException("Unknown owner_type: $raw")
    }
}
