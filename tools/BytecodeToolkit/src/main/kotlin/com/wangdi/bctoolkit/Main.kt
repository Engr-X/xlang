package com.wangdi.bctoolkit

import com.alibaba.fastjson2.JSONArray
import com.alibaba.fastjson2.JSONObject
import com.wangdi.bctoolkit.reader.ByteCodeReader
import com.wangdi.bctoolkit.reader.JavaClassDto

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.concurrent.Callable
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit

import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json


private data class ArgContext(
    var jsonText: String? = null,
    var jsonPath: Path? = null,
    var readPath: Path? = null,
    var outPath: Path? = null,
    var separateSize: Int? = null,
    var jobs: Int = 1,
    var debug: Boolean = false,
    var showHelp: Boolean = false,
    val args: MutableList<String>,
)

private data class Options(
    val jsonText: String?,
    val jsonPath: Path?,
    val readPath: Path?,
    val outPath: Path?,
    val separateSize: Int?,
    val jobs: Int,
    val debug: Boolean,
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

private fun parsePositiveInt(raw: String): Int?
{
    val n = raw.toIntOrNull()
    return if (n == null || n <= 0) null else n
}

private fun parseJobsValue(raw: String, ctx: ArgContext): Boolean
{
    val n = parsePositiveInt(raw)
    return if (n == null)
    {
        System.err.println("Invalid value for -j/--jobs: '$raw' (must be positive integer)")
        ctx.showHelp = true
        false
    }
    else
    {
        ctx.jobs = n
        true
    }
}

private val handlers: Map<String, (ArgContext, Int) -> Int> = mapOf(
    "-h" to { ctx, i -> ctx.showHelp = true; i + 1 },
    "--help" to { ctx, i -> ctx.showHelp = true; i + 1 },
    "--debug" to { ctx, i -> ctx.debug = true; i + 1 },
    "-debug" to { ctx, i -> ctx.debug = true; i + 1 },

    "-j" to { ctx, i ->
        val raw = nextValue(ctx, i) ?: return@to ctx.args.size
        if (parseJobsValue(raw, ctx)) i + 2 else ctx.args.size
    },
    "--jobs" to { ctx, i ->
        val raw = nextValue(ctx, i) ?: return@to ctx.args.size
        if (parseJobsValue(raw, ctx)) i + 2 else ctx.args.size
    },

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
    },
    "--separate" to { ctx, i ->
        val raw = nextValue(ctx, i) ?: return@to ctx.args.size
        val n = parsePositiveInt(raw)
        if (n == null)
        {
            System.err.println("Invalid value for --separate: '$raw' (must be positive integer)")
            ctx.showHelp = true
            ctx.args.size
        }
        else
        {
            ctx.separateSize = n
            i + 2
        }
    }
)

private fun parseArgs(args: Array<String>): Options
{
    val ctx = ArgContext(args = args.toMutableList())

    var i = 0

    while (i < args.size)
    {
        val arg = args[i]

        if (arg.startsWith("--separate="))
        {
            val raw = arg.substringAfter("=", "")
            val n = parsePositiveInt(raw)

            if (n == null)
            {
                System.err.println("Invalid value for --separate: '$raw' (must be positive integer)")
                ctx.showHelp = true
                break
            }

            ctx.separateSize = n
            i += 1
            continue
        }

        if (arg.startsWith("--jobs="))
        {
            val raw = arg.substringAfter("=", "")
            if (!parseJobsValue(raw, ctx))
                break
            i += 1
            continue
        }

        val handler: ((ArgContext, Int) -> Int)? = handlers[arg]

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

    return Options(ctx.jsonText, ctx.jsonPath, ctx.readPath, ctx.outPath, ctx.separateSize, ctx.jobs, ctx.debug, ctx.showHelp)
}

private fun printHelp()
{
    val usage = """
        Usage:
            write mode (json -> class):
                --string (-s) <json>            JSON string input (wrap in quotes)
                --file (-f) <path>              JSON file path
                --out (-o) <dir>                Output directory (default: .)
                -j <n>, --jobs <n>              Parallel workers for class generation

            read mode (class -> json):
                --read (-r) <file.class|file.jar|file.jmod>
                                                Input class/jar/jmod file
                --out (-o) <file.json>          Write pretty JSON to file (optional)
                                                If omitted, print pretty JSON to stdout
                                                Output shape: {"classes":[...]}
                --separate=<n>                  Split read output into files by class count
                --separate <n>                  Each file keeps at most n classes
                                                File names: out0.json, out1.json, ...
                --debug (-debug)                Print read progress: [current/total]: read <file>

            --help (-h)                         Show help

        Examples:
            --string '{"class":["Square"],"methods":[]}'
            --string '{"classes":[{"class":["Square"],"methods":[]}]}'
            --file examples/Example.json
            --file examples/Example.json --out .
            --file examples/Example.json --out . --jobs 8
            --read out/Square.class
            --read out/Square.class --out square.json
            --read out/app.jar
            --read out/app.jar --out app.json
            --read out/app.jar --out ./meta --separate=2
            --read out/app.jar --debug
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

private fun printReadProgress(current: Int, total: Int, fileText: String)
{
    println("[$current / $total]: read $fileText")
}

private fun readOneClass(path: Path, debug: Boolean = false): JavaClassDto
{
    if (debug)
        printReadProgress(1, 1, path.toString())

    return ByteCodeReader.readClass(path).toDto()
}

private fun readFromJar(path: Path, debug: Boolean = false): List<JavaClassDto>
{
    val cb: ((Int, Int, String) -> Unit)? = if (debug)
        { current, total, entryText -> printReadProgress(current, total, entryText) }
    else
        null

    return ByteCodeReader.readJar(path, cb).map { it.toDto() }
}

private fun readFromJmod(path: Path, debug: Boolean = false): List<JavaClassDto>
{
    val cb: ((Int, Int, String) -> Unit)? = if (debug)
        { current, total, entryText -> printReadProgress(current, total, entryText) }
    else
        null

    return ByteCodeReader.readJmod(path, cb).map { it.toDto() }
}

private fun readClassDtos(path: Path, debug: Boolean = false): List<JavaClassDto>
{
    val fileName = path.fileName?.toString()?.lowercase() ?: ""

    return when
    {
        fileName.endsWith(".class") -> listOf(readOneClass(path, debug))
        fileName.endsWith(".jar") -> readFromJar(path, debug)
        fileName.endsWith(".jmod") -> readFromJmod(path, debug)
        else -> throw IllegalArgumentException("Unsupported --read input: '$path' (only .class, .jar, or .jmod)")
    }
}

private fun resolveSeparateOutDir(outPath: Path?): Path
{
    if (outPath == null)
        return Paths.get(".")

    val outAbs = outPath.toAbsolutePath().normalize()
    val name = outAbs.fileName?.toString()?.lowercase() ?: ""

    return if (name.endsWith(".json"))
        outAbs.parent ?: Paths.get(".")
    else
        outAbs
}

private fun writeSeparatedReadJson(classes: List<JavaClassDto>, separateSize: Int, outPath: Path?)
{
    val outDir = resolveSeparateOutDir(outPath)
    Files.createDirectories(outDir)

    val chunks = if (classes.isEmpty()) listOf(emptyList()) else classes.chunked(separateSize)

    chunks.forEachIndexed { index, chunk ->
        val outFile = outDir.resolve("out${index}.json")
        val jsonText = prettyJson.encodeToString(ClassesEnvelope(chunk))
        Files.write(outFile, jsonText.toByteArray(StandardCharsets.UTF_8))
    }
}

private fun saveClasses(outDir: Path, classObjects: List<JSONObject>, jobs: Int)
{
    if (jobs <= 1 || classObjects.size <= 1)
    {
        classObjects.forEach { classJson ->
            JsonAdapter(classJson).getClassEmitter().save(outDir)
        }
        return
    }

    val pool = Executors.newFixedThreadPool(jobs)
    try
    {
        val futures = classObjects.map { classJson ->
            pool.submit(Callable {
                JsonAdapter(classJson).getClassEmitter().save(outDir)
                Unit
            })
        }

        futures.forEach { it.get() }
    }
    finally
    {
        pool.shutdown()
        pool.awaitTermination(365, TimeUnit.DAYS)
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

    if (opts.separateSize != null && !readMode)
    {
        System.err.println("--separate is only valid in read mode (--read).")
        printHelp()
        return
    }

    if (readMode)
    {
        val readPath: Path = opts.readPath!!
        val classes = readClassDtos(readPath, opts.debug)

        if (opts.separateSize != null)
        {
            writeSeparatedReadJson(classes, opts.separateSize, opts.outPath)
            return
        }

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
    saveClasses(outDir, classObjects, opts.jobs)
}
