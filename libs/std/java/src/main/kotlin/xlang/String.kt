package xlang

import java.io.Serializable
import java.lang.StringIndexOutOfBoundsException
import kotlin.Int
import kotlin.math.min


class String(private val value: CharArray) : Any(),
    Serializable, Comparable<String>, CharSequence
{
    override val length: Int
        get() = this.value.size


    override fun compareTo(other: String): Int
    {
        val len1 = this.value.size

        val len2: Int = other.value.size

        for (i in 0..<min(len1, len2))
        {
            val c1 = this[i]
            val c2 = other[i]

            if (c1 != c2)
                return c1.code - c2.code
        }

        return len1 - len2
    }


    override fun get(index: Int): Char
    {
        if ((index < 0) || (index >= this.value.size))
            throw StringIndexOutOfBoundsException(index)

        return value[index]
    }

    override fun subSequence(startIndex: Int, endIndex: Int): CharSequence = TODO()
}
