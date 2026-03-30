package xlang


open class Any : kotlin.Any(), Cloneable
{
    override fun clone(): Any = throw NotImplementedError("${this.javaClass.name}.clone() is not implemented.")

    open fun <V : Any> add(other: V): V = throw NotImplementedError("${this.javaClass.name}.add() is not implemented.")

    open fun <V : Any> plus(other: V): V = this.clone().add(other)
}
