@file:JvmName("ConsoleXl")
package xlang.io

import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.PrintStream
import java.util.Objects
import java.util.concurrent.locks.Lock
import java.util.concurrent.locks.ReentrantLock
import kotlin.concurrent.withLock


private val LOCK: Lock = ReentrantLock()

fun print(vararg values: Boolean, sep: String = " ", end: String = "\n", file: PrintStream = System.out, flush: Boolean = false)
{
    val outputString: String = values.joinToString(separator = sep, postfix = end)

    LOCK.withLock {
        file.print(outputString)

        if (flush)
            file.flush()
    }
}

fun print(vararg values: Char, sep: String = " ", end: String = "\n", file: PrintStream = System.out, flush: Boolean = false)
{
    val outputString: String = values.joinToString(separator = sep, postfix = end)

    LOCK.withLock {
        file.print(outputString)

        if (flush)
            file.flush()
    }
}


fun print(vararg values: Byte, sep: String = " ", end: String = "\n", file: PrintStream = System.out, flush: Boolean = false)
{
    val outputString: String = values.joinToString(separator = sep, postfix = end)

    LOCK.withLock {
        file.print(outputString)

        if (flush)
            file.flush()
    }
}


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
fun print(vararg values: Int, sep: String = " ", end: String = "\n", file: PrintStream = System.out, flush: Boolean = false)
{
    val outputString: String = values.joinToString(separator = sep, postfix = end)

    LOCK.withLock {
        file.print(outputString)

        if (flush)
            file.flush()
    }
}

fun print(vararg values: Long, sep: String = " ", end: String = "\n", file: PrintStream = System.out, flush: Boolean = false)
{
    val outputString: String = values.joinToString(separator = sep, postfix = end)

    LOCK.withLock {
        file.print(outputString)

        if (flush)
            file.flush()
    }
}

fun print(vararg values: Float, sep: String = " ", end: String = "\n", file: PrintStream = System.out, flush: Boolean = false)
{
    val outputString: String = values.joinToString(separator = sep, postfix = end)

    LOCK.withLock {
        file.print(outputString)

        if (flush)
            file.flush()
    }
}

fun print(vararg values: Double, sep: String = " ", end: String = "\n", file: PrintStream = System.out, flush: Boolean = false)
{
    val outputString: String = values.joinToString(separator = sep, postfix = end)

    LOCK.withLock {
        file.print(outputString)

        if (flush)
            file.flush()
    }
}

fun print(vararg values: Objects, sep: String = " ", end: String = "\n", file: PrintStream = System.out, flush: Boolean = false)
{
    val outputString: String = values.joinToString(separator = sep, postfix = end)

    LOCK.withLock {
        file.print(outputString)

        if (flush)
            file.flush()
    }
}

fun input(prompt: String = "", file: InputStreamReader = InputStreamReader(System.`in`)): String
{
    val br = BufferedReader(file)
    return br.readLine()
}
