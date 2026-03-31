package xlang


open class Any : kotlin.Any(), Cloneable
{
    override fun clone(): Any = throw NotImplementedError("${this.javaClass.name}.clone() is not implemented.")
}
