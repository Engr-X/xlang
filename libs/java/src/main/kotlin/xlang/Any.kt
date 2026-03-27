package xlang

import xlang.annotation.Arithmetic


open class Any : kotlin.Any(), Arithmetic<Any>
{
    override fun <V : Any> add(a: Arithmetic<V>) = throw NotImplementedError("Not yet implemented")
}
