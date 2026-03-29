package xlang


open class Int(private val value: kotlin.Int) : Number()
{
    override fun toBool(): Boolean = this.value != 0

    override fun toInt(): kotlin.Int = this.value

    override fun toLong(): Long = this.value.toLong()

    override fun toFloat(): Float = this.value.toFloat()

    override fun toDouble(): Double = this.value.toDouble()
}
