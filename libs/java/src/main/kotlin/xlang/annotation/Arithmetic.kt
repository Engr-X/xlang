package xlang.annotation


interface Arithmetic<T> : Cloneable
{
    override fun clone(): Arithmetic<T> = throw NotImplementedError("Clone method is not implemented")

    fun <V : T> add(a: Arithmetic<V>): Arithmetic<T> = this

    fun <V : T> plus(a: Arithmetic<V>): Arithmetic<T> = this.clone().add(a)
}
