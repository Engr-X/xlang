package com.wangdi.bctoolkit

import com.alibaba.fastjson2.JSONObject
import com.wangdi.bctoolkit.generator.JsonAdapter as GeneratorJsonAdapter
import com.wangdi.bctoolkit.reader.ByteCodeReader
import com.wangdi.bctoolkit.reader.JsonAdapter as ReaderJsonAdapter
import com.wangdi.bctoolkit.reader.JavaClassDto
import com.wangdi.bctoolkit.reader.database.DbMetadataStore

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.concurrent.Callable
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit


private data class ArgContext(
    var jsonText: String? = null,
    var jsonPath: Path? = null,
    var readPath: Path? = null,
    var outPath: Path? = null,
    var imports: List<String>? = null,
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
    val imports: List<String>?,
    val separateSize: Int?,
    val jobs: Int,
    val debug: Boolean,
    val showHelp: Boolean
)

private val readJsonAdapter: ReaderJsonAdapter = ReaderJsonAdapter()
private val dbMetadataStore: DbMetadataStore = DbMetadataStore()

/**
 * Auto-generated baseline docs for readFile.
 * Describes the intent and behavior of this function.
 *
 * @param path parameter from function signature.
 * @return return value of this function.
 */
private fun readFile(path: Path): String = File(path.toString()).readText()

/**
 * Auto-generated baseline docs for nextValue.
 * Describes the intent and behavior of this function.
 *
 * @param ctx parameter from function signature.
 * @param index parameter from function signature.
 * @return return value of this function.
 */
private fun nextValue(ctx: ArgContext, index: Int): String?
{
    if (index + 1 >= ctx.args.size)
    {
        ctx.showHelp = true
        return null
    }
    return ctx.args[index + 1]
}

/**
 * Auto-generated baseline docs for parsePositiveInt.
 * Describes the intent and behavior of this function.
 *
 * @param raw parameter from function signature.
 * @return return value of this function.
 */
private fun parsePositiveInt(raw: String): Int?
{
    val n = raw.toIntOrNull()
    return if (n == null || n <= 0) null else n
}

/**
 * Auto-generated baseline docs for parseJobsValue.
 * Describes the intent and behavior of this function.
 *
 * @param raw parameter from function signature.
 * @param ctx parameter from function signature.
 * @return return value of this function.
 */
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

/**
 * Parse imports option value and update parsing context.
 *
 * @param raw raw option value.
 * @param ctx argument context.
 * @return true when parsed successfully.
 */
private fun parseImportsValue(raw: String, ctx: ArgContext): Boolean
{
    val patterns = raw.split(',')
        .map { it.trim() }
        .filter { it.isNotEmpty() }

    return if (patterns.isEmpty())
    {
        System.err.println("Invalid value for --imports: '$raw' (use comma-separated patterns)")
        ctx.showHelp = true
        false
    }
    else
    {
        ctx.imports = patterns
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
    "--imports" to { ctx, i ->
        val raw = nextValue(ctx, i) ?: return@to ctx.args.size
        if (parseImportsValue(raw, ctx)) i + 2 else ctx.args.size
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

/**
 * Auto-generated baseline docs for parseArgs.
 * Describes the intent and behavior of this function.
 *
 * @param args parameter from function signature.
 * @return return value of this function.
 */
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

        if (arg.startsWith("--imports="))
        {
            val raw = arg.substringAfter("=", "")
            if (!parseImportsValue(raw, ctx))
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

    return Options(
        ctx.jsonText,
        ctx.jsonPath,
        ctx.readPath,
        ctx.outPath,
        ctx.imports,
        ctx.separateSize,
        ctx.jobs,
        ctx.debug,
        ctx.showHelp
    )
}

/**
 * Auto-generated baseline docs for printHelp.
 * Describes the intent and behavior of this function.
 */
private fun printHelp()
{
    val usage = """
        Usage:
            write mode (json -> class):
                --string (-s) <json>            JSON string input (wrap in quotes)
                --file (-f) <path>              JSON file path
                --out (-o) <dir>                Output directory (default: .)
                -j <n>, --jobs <n>              Parallel workers for class generation

            read mode (json/class/jar/jmod/db -> json or db):
                --read (-r) <file.json|file.class|file.jar|file.jmod|file.db|json_dir>
                                                Input json/class/jar/jmod/db file or json directory
                --out (-o) <file.json|file.db>  Write pretty JSON to file or save to sqlite db
                                                If omitted, print pretty JSON to stdout
                                                JSON shape: {"classes":[...]}
                --imports <patterns>            Filter by class full name pattern(s)
                                                Example: xlang.math.*,java.lang.String
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
            --read out/app.jar --out app.db
            --read out/app.jar --out ./meta --separate=2
            --read out/app.jar --imports java.lang.* --out java.db
            --read out/app.jar --debug
            --read D:/meta/jdk8-json --out D:/meta/jdk8.db
            --read C:/Java/jmods/java.base.jmod --out java.base.json
            --read C:/Java/jmods/java.base.jmod --out java.base.db
    """.trimIndent()
    println(usage)
}

/**
 * Auto-generated baseline docs for printReadProgress.
 * Describes the intent and behavior of this function.
 *
 * @param current parameter from function signature.
 * @param total parameter from function signature.
 * @param fileText parameter from function signature.
 */
private fun printReadProgress(current: Int, total: Int, fileText: String)
{
    println("[$current / $total]: read $fileText")
}

/**
 * Check whether a path points to sqlite db output/input.
 *
 * @param path file path.
 * @return true when suffix is .db or .sqlite.
 */
private fun isDbPath(path: Path): Boolean
{
    val fileName = path.fileName?.toString()?.lowercase() ?: ""
    return fileName.endsWith(".db") || fileName.endsWith(".sqlite")
}

/**
 * Build full class name from dto.
 *
 * @param clazz class metadata dto.
 * @return full class name.
 */
private fun classFullName(clazz: JavaClassDto): String =
    if (clazz.packagePath.isEmpty()) clazz.name else "${clazz.packagePath.joinToString(".")}.${clazz.name}"

/**
 * Convert an import pattern to regex.
 *
 * @param pattern import pattern text.
 * @return compiled regex.
 */
private fun importPatternRegex(pattern: String): Regex
{
    val normalized = pattern.trim().replace('/', '.')
    val escaped = Regex.escape(normalized).replace("\\*", ".*")
    return Regex("^$escaped$")
}

/**
 * Filter class metadata by import patterns.
 *
 * @param classes class metadata list.
 * @param imports import pattern list.
 * @return filtered class metadata.
 */
private fun filterByImports(classes: List<JavaClassDto>, imports: List<String>?): List<JavaClassDto>
{
    if (imports.isNullOrEmpty())
        return classes

    val patterns = imports.map(::importPatternRegex)
    return classes.filter { clazz ->
        val fullName = classFullName(clazz)
        patterns.any { it.matches(fullName) }
    }
}

/**
 * Auto-generated baseline docs for readOneClass.
 * Describes the intent and behavior of this function.
 *
 * @param path parameter from function signature.
 * @param debug parameter from function signature.
 * @return return value of this function.
 */
private fun readOneClass(path: Path, debug: Boolean = false): JavaClassDto
{
    if (debug)
        printReadProgress(1, 1, path.toString())

    return ByteCodeReader.readClass(path).toDto()
}

/**
 * Auto-generated baseline docs for readFromJar.
 * Describes the intent and behavior of this function.
 *
 * @param path parameter from function signature.
 * @param debug parameter from function signature.
 * @return return value of this function.
 */
private fun readFromJar(path: Path, debug: Boolean = false): List<JavaClassDto>
{
    val cb: ((Int, Int, String) -> Unit)? = if (debug)
        { current, total, entryText -> printReadProgress(current, total, entryText) }
    else
        null

    return ByteCodeReader.readJar(path, cb).map { it.toDto() }
}

/**
 * Auto-generated baseline docs for readFromJmod.
 * Describes the intent and behavior of this function.
 *
 * @param path parameter from function signature.
 * @param debug parameter from function signature.
 * @return return value of this function.
 */
private fun readFromJmod(path: Path, debug: Boolean = false): List<JavaClassDto>
{
    val cb: ((Int, Int, String) -> Unit)? = if (debug)
        { current, total, entryText -> printReadProgress(current, total, entryText) }
    else
        null

    return ByteCodeReader.readJmod(path, cb).map { it.toDto() }
}

/**
 * Load class metadata from db file.
 *
 * @param path sqlite db path.
 * @param debug debug switch.
 * @return decoded class metadata list.
 */
private fun readFromDb(path: Path, debug: Boolean = false): List<JavaClassDto>
{
    val classes = dbMetadataStore.loadClasses(path)
    if (debug)
        println("[1 / 1]: read ${path.toAbsolutePath().normalize()} (classes=${classes.size})")
    return classes
}

/**
 * Read class metadata list from one json file.
 *
 * @param path json file path.
 * @param debug debug switch.
 * @return decoded class metadata list.
 */
private fun readFromJsonFile(path: Path, debug: Boolean = false): List<JavaClassDto>
{
    val classes = readJsonAdapter.decodeClasses(readFile(path))
    if (debug)
        println("[1 / 1]: read ${path.toAbsolutePath().normalize()} (classes=${classes.size})")
    return classes
}

/**
 * Read and merge class metadata list from a json directory.
 *
 * @param path json directory path.
 * @param debug debug switch.
 * @return merged class metadata list.
 */
private fun readFromJsonDir(path: Path, debug: Boolean = false): List<JavaClassDto>
{
    val jsonFiles = mutableListOf<Path>()
    Files.walk(path).use { stream ->
        stream.filter { one ->
            Files.isRegularFile(one) && one.fileName.toString().lowercase().endsWith(".json")
        }.forEach { one ->
            jsonFiles.add(one)
        }
    }

    if (jsonFiles.isEmpty())
        throw IllegalArgumentException("No .json files found in directory: '$path'")

    jsonFiles.sortBy { it.toString() }

    val out = mutableListOf<JavaClassDto>()
    jsonFiles.forEachIndexed { index, one ->
        if (debug)
            printReadProgress(index + 1, jsonFiles.size, one.toString())
        out.addAll(readJsonAdapter.decodeClasses(readFile(one)))
    }

    return out
}

/**
 * Auto-generated baseline docs for readClassDtos.
 * Describes the intent and behavior of this function.
 *
 * @param path parameter from function signature.
 * @param debug parameter from function signature.
 * @return return value of this function.
 */
private fun readClassDtos(path: Path, debug: Boolean = false): List<JavaClassDto>
{
    if (Files.isDirectory(path))
        return readFromJsonDir(path, debug)

    val fileName = path.fileName?.toString()?.lowercase() ?: ""

    return when
    {
        fileName.endsWith(".json") -> readFromJsonFile(path, debug)
        fileName.endsWith(".class") -> listOf(readOneClass(path, debug))
        fileName.endsWith(".jar") -> readFromJar(path, debug)
        fileName.endsWith(".jmod") -> readFromJmod(path, debug)
        fileName.endsWith(".db") || fileName.endsWith(".sqlite") -> readFromDb(path, debug)
        else -> throw IllegalArgumentException("Unsupported --read input: '$path' (use .json/.class/.jar/.jmod/.db or a json directory)")
    }
}

/**
 * Auto-generated baseline docs for resolveSeparateOutDir.
 * Describes the intent and behavior of this function.
 *
 * @param outPath parameter from function signature.
 * @return return value of this function.
 */
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

/**
 * Auto-generated baseline docs for writeSeparatedReadJson.
 * Describes the intent and behavior of this function.
 *
 * @param classes parameter from function signature.
 * @param separateSize parameter from function signature.
 * @param outPath parameter from function signature.
 */
private fun writeSeparatedReadJson(classes: List<JavaClassDto>, separateSize: Int, outPath: Path?)
{
    val outDir = resolveSeparateOutDir(outPath)
    Files.createDirectories(outDir)

    val chunks = if (classes.isEmpty()) listOf(emptyList()) else classes.chunked(separateSize)

    chunks.forEachIndexed { index, chunk ->
        val outFile = outDir.resolve("out${index}.json")
        val jsonText = readJsonAdapter.encodeClasses(chunk)
        Files.write(outFile, jsonText.toByteArray(StandardCharsets.UTF_8))
    }
}

/**
 * Auto-generated baseline docs for saveClasses.
 * Describes the intent and behavior of this function.
 *
 * @param outDir parameter from function signature.
 * @param classObjects parameter from function signature.
 * @param jobs parameter from function signature.
 */
private fun saveClasses(outDir: Path, classObjects: List<JSONObject>, jobs: Int)
{
    if (jobs <= 1 || classObjects.size <= 1)
    {
        classObjects.forEach { classJson ->
            GeneratorJsonAdapter(classJson).getClassEmitter().save(outDir)
        }
        return
    }

    val pool = Executors.newFixedThreadPool(jobs)
    try
    {
        val futures = classObjects.map { classJson ->
            pool.submit(Callable {
                GeneratorJsonAdapter(classJson).getClassEmitter().save(outDir)
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

/**
 * Auto-generated baseline docs for main.
 * Describes the intent and behavior of this function.
 *
 * @param args parameter from function signature.
 */
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
        val classes = filterByImports(readClassDtos(readPath, opts.debug), opts.imports)

        if (opts.separateSize != null && opts.outPath != null && isDbPath(opts.outPath))
        {
            System.err.println("--separate cannot be used when --out points to a db file.")
            printHelp()
            return
        }

        if (opts.separateSize != null)
        {
            writeSeparatedReadJson(classes, opts.separateSize, opts.outPath)
            return
        }

        if (opts.outPath != null && isDbPath(opts.outPath))
        {
            dbMetadataStore.saveClasses(opts.outPath, classes)
            return
        }

        val jsonText = readJsonAdapter.encodeClasses(classes)

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

    val classObjects: List<JSONObject> = GeneratorJsonAdapter.parseClassObjects(jsonText)
    saveClasses(outDir, classObjects, opts.jobs)
}

