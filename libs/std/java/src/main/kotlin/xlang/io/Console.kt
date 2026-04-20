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


/**
 * Prints a boolean value without a trailing newline.
 * The text form follows xlang.String.valueOf(value) and is written to stdout.
 *
 * @param value boolean value to print
 */
@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun put(value: Boolean) { kotlin.io.print(value) }


/**
 * Prints one character without a trailing newline.
 *
 * @param value character value to print
 */
@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun put(value: Char) { kotlin.io.print(value) }


/**
 * Prints an int8 value without a trailing newline.
 * The text form follows xlang.String.valueOf(value).
 *
 * @param value int8 value to print
 */
@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun put(value: Byte) { kotlin.io.print(value) }


/**
 * Prints an int16 value without a trailing newline.
 * The text form follows xlang.String.valueOf(value).
 *
 * @param value int16 value to print
 */
@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun put(value: Short) { kotlin.io.print(value) }


/**
 * Prints an int32 value without a trailing newline.
 * The text form follows xlang.String.valueOf(value).
 *
 * @param value int32 value to print
 */
@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun put(value: Int) { kotlin.io.print(value) }


/**
 * Prints an int64 value without a trailing newline.
 * The text form follows xlang.String.valueOf(value).
 *
 * @param value int64 value to print
 */
@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun put(value: Long) { kotlin.io.print(value) }


/**
 * Prints a float32 value without a trailing newline.
 * The text form follows xlang.String.valueOf(value).
 *
 * @param value float32 value to print
 */
@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun put(value: Float) { kotlin.io.print(value) }


/**
 * Prints a float64 value without a trailing newline.
 * The text form follows xlang.String.valueOf(value).
 *
 * @param value float64 value to print
 */
@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun put(value: Double) { kotlin.io.print(value) }



@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun put(value: String) { kotlin.io.print(value) }



@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun put(value: xlang.String) { kotlin.io.print(value.toString()) }



@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun put(value: Any) { kotlin.io.print(value) }


/**
 * Prints a boolean value and then prints a newline.
 * The text form follows xlang.String.valueOf(value).
 *
 * @param value boolean value to print
 */
@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun putln(value: Boolean) { kotlin.io.println(value) }


/**
 * Prints one character and then prints a newline.
 *
 * @param value character value to print
 */
@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun putln(value: Char) { kotlin.io.println(value) }


/**
 * Prints an int8 value and then prints a newline.
 * The text form follows xlang.String.valueOf(value).
 *
 * @param value int8 value to print
 */
@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun putln(value: Byte) { kotlin.io.println(value) }


/**
 * Prints an int16 value and then prints a newline.
 * The text form follows xlang.String.valueOf(value).
 *
 * @param value int16 value to print
 */
@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun putln(value: Short) { kotlin.io.println(value) }


/**
 * Prints an int32 value and then prints a newline.
 * The text form follows xlang.String.valueOf(value).
 *
 * @param value int32 value to print
 */
@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun putln(value: Int) { kotlin.io.println(value) }


/**
 * Prints an int64 value and then prints a newline.
 * The text form follows xlang.String.valueOf(value).
 *
 * @param value int64 value to print
 */
@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun putln(value: Long) { kotlin.io.println(value) }


/**
 * Prints a float32 value and then prints a newline.
 * The text form follows xlang.String.valueOf(value).
 *
 * @param value float32 value to print
 */
@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun putln(value: Float) { kotlin.io.println(value) }


/**
 * Prints a float64 value and then prints a newline.
 * The text form follows xlang.String.valueOf(value).
 *
 * @param value float64 value to print
 */
@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun putln(value: Double) { kotlin.io.println(value) }


@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun putln(value: String) { println(value) }


@Metadata(TOP_LEVEL)
@SuppressWarnings("unused")
fun putln(value: xlang.String) { println(value.toString()) }


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
    if (prompt.isNotEmpty())
        kotlin.io.print(prompt)

    return file.readLine()
}
