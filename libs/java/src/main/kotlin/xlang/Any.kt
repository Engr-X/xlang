package xlang


open class Any : kotlin.Any(), Cloneable
{
    fun <V : Any> add(a: V): Unit = throw NotImplementedError("${this.javaClass}")

    fun <V : Any, R : Any> plus(a: V): R = 
}
