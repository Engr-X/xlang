package xlang

import xlang.annotation.Arithmetic

class Any : kotlin.Any(), Arithmetic<Any>, Cloneable
{
    override fun clone(): Any = throw NotImplementedError("Clone method is not implemented")
}
