package xlang


abstract class Number() : Any()
{
    abstract fun toBool(): kotlin.Boolean

    abstract fun toInt(): kotlin.Int

    abstract fun toLong(): kotlin.Long

    abstract fun toFloat(): kotlin.Float

    abstract fun toDouble(): kotlin.Double

    fun toChar(): kotlin.Char = this.toInt().toChar()

    fun toByte(): Byte = this.toInt().toByte()

    fun toShort(): Short = this.toInt().toShort()
}
