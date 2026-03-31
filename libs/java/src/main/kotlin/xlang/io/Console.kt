@file:JvmName("Console")
package xlang.io

import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.PrintStream
import java.util.concurrent.locks.Lock
import java.util.concurrent.locks.ReentrantLock

import kotlin.concurrent.withLock

import xlang.annotation.Metadata


private const val TOP_LEVEL: String = "xlang-top-level"

@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun put(value: Boolean) { kotlin.io.print(value) }


@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun put(value: Char) { kotlin.io.print(value) }


@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun put(value: Byte) { kotlin.io.print(value) }


@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun put(value: Short) { kotlin.io.print(value) }


@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun put(value: Int) { kotlin.io.print(value) }


@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun put(value: Long) { kotlin.io.print(value) }


@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun put(value: Float) { kotlin.io.print(value) }


@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun put(value: Double) { kotlin.io.print(value) }


@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun put(value: String) { kotlin.io.print(value) }


@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun put(value: Any) { kotlin.io.print(value) }


@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun putln(value: Boolean) { kotlin.io.println(value) }


@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun putln(value: Char) { kotlin.io.println(value) }


@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun putln(value: Byte) { kotlin.io.println(value) }


@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun putln(value: Short) { kotlin.io.println(value) }


@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun putln(value: Int) { kotlin.io.println(value) }


@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun putln(value: Long) { kotlin.io.println(value) }


@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun putln(value: Float) { kotlin.io.println(value) }


@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun putln(value: Double) { kotlin.io.println(value) }


@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun putln(value: String) { println(value) }


@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun putln(value: Any) { println(value) }


private val LOCK: Lock = ReentrantLock()


@JvmOverloads
@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun print(vararg values: Boolean, sep: String = " ", end: String = "\n", file: PrintStream = System.out, flush: Boolean = false)
{
    val outputString: String = values.joinToString(separator = sep, postfix = end)

    LOCK.withLock {
        file.print(outputString)

        if (flush)
            file.flush()
    }
}


@JvmOverloads
@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun print(vararg values: Char, sep: String = " ", end: String = "\n", file: PrintStream = System.out, flush: Boolean = false)
{
    val outputString: String = values.joinToString(separator = sep, postfix = end)

    LOCK.withLock {
        file.print(outputString)

        if (flush)
            file.flush()
    }
}


@JvmOverloads
@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun print(vararg values: Byte, sep: String = " ", end: String = "\n", file: PrintStream = System.out, flush: Boolean = false)
{
    val outputString: String = values.joinToString(separator = sep, postfix = end)

    LOCK.withLock {
        file.print(outputString)

        if (flush)
            file.flush()
    }
}


@JvmOverloads
@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun print(vararg values: Short, sep: String = " ", end: String = "\n", file: PrintStream = System.out, flush: Boolean = false)
{
    val outputString: String = values.joinToString(separator = sep, postfix = end)

    LOCK.withLock {
        file.print(outputString)

        if (flush)
            file.flush()
    }
}


@JvmOverloads
@Metadata(TOP_LEVEL)
fun print(vararg values: Int, sep: String = " ", end: String = "\n", file: PrintStream = System.out, flush: Boolean = false)
{
    val outputString: String = values.joinToString(separator = sep, postfix = end)

    LOCK.withLock {
        file.print(outputString)

        if (flush)
            file.flush()
    }
}


@JvmOverloads
@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun print(vararg values: Long, sep: String = " ", end: String = "\n", file: PrintStream = System.out, flush: Boolean = false)
{
    val outputString: String = values.joinToString(separator = sep, postfix = end)

    LOCK.withLock {
        file.print(outputString)

        if (flush)
            file.flush()
    }
}


@JvmOverloads
@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun print(vararg values: Float, sep: String = " ", end: String = "\n", file: PrintStream = System.out, flush: Boolean = false)
{
    val outputString: String = values.joinToString(separator = sep, postfix = end)

    LOCK.withLock {
        file.print(outputString)

        if (flush)
            file.flush()
    }
}


@JvmOverloads
@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun print(vararg values: Double, sep: String = " ", end: String = "\n", file: PrintStream = System.out, flush: Boolean = false)
{
    val outputString: String = values.joinToString(separator = sep, postfix = end)

    LOCK.withLock {
        file.print(outputString)

        if (flush)
            file.flush()
    }
}


@JvmOverloads
@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun print(vararg values: String, sep: String = " ", end: String = "\n", file: PrintStream = System.out, flush: Boolean = false)
{
    val outputString: String = values.joinToString(separator = sep, postfix = end)

    LOCK.withLock {
        file.print(outputString)

        if (flush)
            file.flush()
    }
}


@JvmOverloads
@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun print(vararg values: Any, sep: String = " ", end: String = "\n", file: PrintStream = System.out, flush: Boolean = false)
{
    val outputString: String = values.joinToString(separator = sep, postfix = end)

    LOCK.withLock {
        file.print(outputString)

        if (flush)
            file.flush()
    }
}


private val STDIN_READER: BufferedReader = BufferedReader(InputStreamReader(System.`in`))


@JvmOverloads
@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun input(prompt: String = "", file: BufferedReader = STDIN_READER): String
{
    var b = 10;
    if (prompt.isNotEmpty())
        kotlin.io.print(prompt)

    return file.readLine()
}
