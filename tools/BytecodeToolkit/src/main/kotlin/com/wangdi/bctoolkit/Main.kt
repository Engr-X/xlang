package com.wangdi.bctoolkit

import com.alibaba.fastjson2.JSONArray
import com.alibaba.fastjson2.JSONObject
import com.wangdi.bctoolkit.reader.ByteCodeReader
import com.wangdi.bctoolkit.reader.JavaClassDto

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.charset.StandardCharsets

import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json


private data class ArgContext(
    var jsonText: String? = null,
    var jsonPath: Path? = null,
    var readPath: Path? = null,
    var outPath: Path? = null,
    var showHelp: Boolean = false,
    val args: MutableList<String>,
)

private data class Options(
    val jsonText: String?,
    val jsonPath: Path?,
    val readPath: Path?,
    val outPath: Path?,
    val showHelp: Boolean
)

@Serializable
private data class ClassesEnvelope(
    val classes: List<JavaClassDto>)

private val prettyJson: Json = Json {
    prettyPrint = true
    prettyPrintIndent = "    "
}

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
    "-r" to { ctx, i ->
        val value = nextValue(ctx, i) ?: return@to ctx.args.size
        ctx.readPath = Paths.get(value)
        i + 2
    },
    "--read" to { ctx, i ->
        val value = nextValue(ctx, i) ?: return@to ctx.args.size
        ctx.readPath = Paths.get(value)
        i + 2
    },
    "-o" to { ctx, i ->
        val value = nextValue(ctx, i) ?: return@to ctx.args.size
        ctx.outPath = Paths.get(value)
        i + 2
    },
    "--out" to { ctx, i ->
        val value = nextValue(ctx, i) ?: return@to ctx.args.size
        ctx.outPath = Paths.get(value)
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

    return Options(ctx.jsonText, ctx.jsonPath, ctx.readPath, ctx.outPath, ctx.showHelp)
}

private fun printHelp()
{
    val usage = """
        Usage:
            write mode (json -> class):
                --string (-s) <json>            JSON string input (wrap in quotes)
                --file (-f) <path>              JSON file path
                --out (-o) <dir>                Output directory (default: .)

            read mode (class -> json):
                --read (-r) <file.class|file.jar|file.jmod>
                                                Input class/jar/jmod file
                --out (-o) <file.json>          Write pretty JSON to file (optional)
                                                If omitted, print pretty JSON to stdout
                                                Output shape: {"classes":[...]}

            --help (-h)                         Show help

        Examples:
            --string '{"class":["Square"],"methods":[]}'
            --string '{"classes":[{"class":["Square"],"methods":[]}]}'
            --file examples/Example.json
            --file examples/Example.json --out .
            --read out/Square.class
            --read out/Square.class --out square.json
            --read out/app.jar
            --read out/app.jar --out app.json
            --read C:/Java/jmods/java.base.jmod --out java.base.json
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

private fun readOneClass(path: Path): JavaClassDto
{
    return ByteCodeReader.readClass(path).toDto()
}

private fun readFromJar(path: Path): List<JavaClassDto>
{
    return ByteCodeReader.readJar(path).map { it.toDto() }
}

private fun readFromJmod(path: Path): List<JavaClassDto>
{
    return ByteCodeReader.readJmod(path).map { it.toDto() }
}

private fun readClassDtos(path: Path): List<JavaClassDto>
{
    val fileName = path.fileName?.toString()?.lowercase() ?: ""

    return when
    {
        fileName.endsWith(".class") -> listOf(readOneClass(path))
        fileName.endsWith(".jar") -> readFromJar(path)
        fileName.endsWith(".jmod") -> readFromJmod(path)
        else -> throw IllegalArgumentException("Unsupported --read input: '$path' (only .class, .jar, or .jmod)")
    }
}

fun main(args: Array<String>)
{
    val opts: Options = parseArgs(args)
    val writeMode: Boolean = opts.jsonText != null || opts.jsonPath != null
    val readMode: Boolean = opts.readPath != null

    if (opts.showHelp || (!writeMode && !readMode))
    {
        printHelp()
        return
    }

    if (writeMode && readMode)
    {
        System.err.println("Only one mode is allowed at a time: write mode (-s/-f) or read mode (-r).")
        printHelp()
        return
    }

    if (opts.jsonText != null && opts.jsonPath != null)
    {
        System.err.println("Only one input method is allowed: --string or --file.")
        printHelp()
        return
    }

    if (readMode)
    {
        val readPath: Path = opts.readPath!!
        val classes = readClassDtos(readPath)
        val jsonText = prettyJson.encodeToString(ClassesEnvelope(classes))

        if (opts.outPath != null)
        {
            val outFile = opts.outPath.toAbsolutePath().normalize()
            outFile.parent?.let { Files.createDirectories(it) }
            Files.write(outFile, jsonText.toByteArray(StandardCharsets.UTF_8))
        }
        else
            println(jsonText)
        return
    }

    val jsonText: String = opts.jsonText ?: readFile(opts.jsonPath!!)
    val outDir: Path = opts.outPath ?: Paths.get(".")

    val classObjects: List<JSONObject> = parseClassObjects(jsonText)

    classObjects.forEach { classJson ->
        JsonAdapter(classJson).getClassEmitter().save(outDir)
    }
}
