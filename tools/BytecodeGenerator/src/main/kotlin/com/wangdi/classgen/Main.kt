package com.wangdi.classgen

import com.alibaba.fastjson2.JSONArray
import com.alibaba.fastjson2.JSONObject

import java.io.File
import java.nio.file.Path
import java.nio.file.Paths


private data class ArgContext(
    var jsonText: String? = null,
    var jsonPath: Path? = null,
    var outDir: Path = Paths.get("."),
    var showHelp: Boolean = false,
    val args: MutableList<String>,
)

private data class Options(
    val jsonText: String?,
    val jsonPath: Path?,
    val outDir: Path,
    val showHelp: Boolean
)

private fun readFile(path: Path): String = File(path.toString()).readText()

private fun nextValue(ctx: ArgContext, index: Int): String?
{
    if (index + 1 >= ctx.args.size)
    {
        ctx.showHelp = true
        return null
    }
    return ctx.args[index + 1]
}

private val handlers: Map<String, (ArgContext, Int) -> Int> = mapOf(
    "-h" to { ctx, i -> ctx.showHelp = true; i + 1 },
    "--help" to { ctx, i -> ctx.showHelp = true; i + 1 },

    "-s" to { ctx, i ->
        val value = nextValue(ctx, i) ?: return@to ctx.args.size
        ctx.jsonText = value
        i + 2
    },
    "--string" to { ctx, i ->
        val value = nextValue(ctx, i) ?: return@to ctx.args.size
        ctx.jsonText = value
        i + 2
    },
    "-f" to { ctx, i ->
        val value = nextValue(ctx, i) ?: return@to ctx.args.size
        ctx.jsonPath = Paths.get(value)
        i + 2
    },
    "--file" to { ctx, i ->
        val value = nextValue(ctx, i) ?: return@to ctx.args.size
        ctx.jsonPath = Paths.get(value)
        i + 2
    },
    "-o" to { ctx, i ->
        val value = nextValue(ctx, i) ?: return@to ctx.args.size
        ctx.outDir = Paths.get(value)
        i + 2
    },
    "--out" to { ctx, i ->
        val value = nextValue(ctx, i) ?: return@to ctx.args.size
        ctx.outDir = Paths.get(value)
        i + 2
    }
)

private fun parseArgs(args: Array<String>): Options
{
    val ctx = ArgContext(args = args.toMutableList())

    var i = 0

    while (i < args.size)
    {
        val handler: ((ArgContext, Int) -> Int)? = handlers[args[i]]

        if (handler == null)
        {
            System.err.println("Unknown argument: ${args[i]}")
            ctx.showHelp = true
            break
        }

        i = handler(ctx, i)

        if (ctx.showHelp)
            break
    }

    return Options(ctx.jsonText, ctx.jsonPath, ctx.outDir, ctx.showHelp)
}

private fun printHelp()
{
    val usage = """
        Usage:
            --string (-s) <json>            JSON string input (wrap in quotes)
            --file (-f) <path>              JSON file path
            --out (-o) <dir>                Output directory (default: .)
            --help (-h)                     Show help

        Examples:
            --string '{"class":["Square"],"methods":[]}'
            --string '{"classes":[{"class":["Square"],"methods":[]}]}'
            --file examples/Example.json
            --file examples/Example.json --out .
    """.trimIndent()
    println(usage)
}

private fun parseClassObjects(jsonText: String): List<JSONObject>
{
    val root: Any = JSONObject.parse(jsonText)

    val rootObj = root as? JSONObject
        ?: throw ClassCastException("Root JSON must be JSONObject, but was ${root::class.java.name}")

    val classesValue: Any = rootObj["classes"] ?: return listOf(rootObj)

    val classArray: JSONArray = classesValue as? JSONArray
        ?: throw ClassCastException("classes must be JSONArray, but was ${classesValue::class.java.name}")

    return classArray.map { item ->
        item as? JSONObject
            ?: throw ClassCastException("classes item must be JSONObject, but was ${item::class.java.name}")
    }
}

fun main(args: Array<String>)
{
    val opts: Options = parseArgs(args)

    if (opts.showHelp || (opts.jsonText == null && opts.jsonPath == null))
    {
        printHelp()
        return
    }

    if (opts.jsonText != null && opts.jsonPath != null)
    {
        System.err.println("Only one input method is allowed: --string or --file.")
        printHelp()
        return
    }

    val jsonText: String = opts.jsonText ?: readFile(opts.jsonPath!!)

    val classObjects: List<JSONObject> = parseClassObjects(jsonText)
    
    classObjects.forEach { classJson ->
        JsonAdapter(classJson).getClassEmitter().save(opts.outDir)
    }
}
