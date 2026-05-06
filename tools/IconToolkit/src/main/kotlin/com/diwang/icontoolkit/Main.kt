/*
 * Copyright (c) 2026 Di Wang
 * SPDX-License-Identifier: MIT
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 */

package com.diwang.icontoolkit

/**
 * This file provides the CLI entry of IconToolkit.
 * It supports SVG to PNG export, and Windows EXE icon patch workflow via `rcedit`.
 *
 * @author Di Wang
 * @since Alpha-1.1.0
 */

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import kotlin.system.exitProcess


private const val VERSION: String = "alpha-1.1.0"

private data class CliOptions(
    val source: Path? = null,
    val pngDestination: Path? = null,
    val exeTarget: Path? = null,
    val toolPath: Path? = null,
    val showHelp: Boolean = false,
    val showVersion: Boolean = false
)

private enum class ParseState
{
    EXPECT_FLAG,
    EXPECT_SOURCE_PATH,
    EXPECT_PNG_DEST_PATH,
    EXPECT_EXE_TARGET_PATH,
    EXPECT_TOOL_PATH
}

/**
 * Program entry. Runs either PNG export mode or EXE icon patch mode.
 *
 * @param args Raw command-line arguments.
 */
fun main(args: Array<String>)
{
    try
    {
        val options = parseArgs(args)

        if (options.showHelp)
        {
            printUsage()
            return
        }

        if (options.showVersion)
        {
            println("IconToolkit $VERSION")
            return
        }

        val source = options.source ?: throw IllegalArgumentException("Missing required flag: -s <source.svg>")
        if (!Files.isRegularFile(source))
            throw IllegalArgumentException("Source SVG not found: $source")

        val hasPngMode = options.pngDestination != null
        val hasExeMode = options.exeTarget != null
        if (!hasPngMode && !hasExeMode)
            throw IllegalArgumentException("You must provide one output target: -d <output.png> or -t <dest.exe>")
        if (hasPngMode && hasExeMode)
            throw IllegalArgumentException("Choose exactly one output mode: -d <output.png> or -t <dest.exe>")

        if (hasPngMode)
        {
            val (outputPath, renamed) = normalizePngOutputPath(options.pngDestination)
            if (renamed)
                System.err.println("Warning: only PNG output is supported. Output path changed to: $outputPath")

            SVGraphics(source.toString())
                .inline()
                .saveAsPng(outputPath)

            println("Saved PNG: $outputPath")
            return
        }

        if (!isWindows())
            throw IllegalArgumentException("EXE icon patch mode is only supported on Windows.")

        val exeTarget = options.exeTarget!!
        if (!Files.isRegularFile(exeTarget))
            throw IllegalArgumentException("Target EXE not found: $exeTarget")

        val tool = resolveRceditTool(options.toolPath, exeTarget)

        patchExeIconInPlace(
            toolPath = tool.toAbsolutePath().normalize(),
            sourceSvg = source.toAbsolutePath().normalize(),
            targetExe = exeTarget.toAbsolutePath().normalize()
        )

        println("Updated EXE icon in place: $exeTarget")
    }
    catch (e: IllegalArgumentException)
    {
        System.err.println("Error: ${e.message}")
        printUsage()
        exitProcess(1)
    }
    catch (e: Exception)
    {
        System.err.println("Error: ${e.message}")
        exitProcess(1)
    }
}

/**
 * Parse command-line arguments with a small state machine.
 *
 * @param args Raw command-line arguments.
 * @return Parsed [CliOptions].
 * @throws IllegalArgumentException When flags are missing, malformed, or unknown.
 */
private fun parseArgs(args: Array<String>): CliOptions
{
    if (args.isEmpty())
        throw IllegalArgumentException("Missing arguments.")

    var state = ParseState.EXPECT_FLAG
    var source: Path? = null
    var pngDestination: Path? = null
    var exeTarget: Path? = null
    var toolPath: Path? = null
    var showHelp = false
    var showVersion = false

    for (token in args)
    {
        when (state)
        {
            ParseState.EXPECT_FLAG ->
            {
                if (token.startsWith("--tool="))
                {
                    val rawPath = token.substringAfter("=", "")
                    if (rawPath.isBlank())
                        throw IllegalArgumentException("Empty --tool path.")
                    toolPath = Path.of(rawPath)
                    continue
                }

                when (token)
                {
                    "-s" -> state = ParseState.EXPECT_SOURCE_PATH
                    "-d" -> state = ParseState.EXPECT_PNG_DEST_PATH
                    "-t" -> state = ParseState.EXPECT_EXE_TARGET_PATH
                    "--tool" -> state = ParseState.EXPECT_TOOL_PATH
                    "-h", "--help" -> showHelp = true
                    "-v", "--version" -> showVersion = true
                    else -> throw IllegalArgumentException("Unknown argument: $token")
                }
            }

            ParseState.EXPECT_SOURCE_PATH ->
            {
                source = Path.of(token)
                state = ParseState.EXPECT_FLAG
            }

            ParseState.EXPECT_PNG_DEST_PATH ->
            {
                pngDestination = Path.of(token)
                state = ParseState.EXPECT_FLAG
            }

            ParseState.EXPECT_EXE_TARGET_PATH ->
            {
                exeTarget = Path.of(token)
                state = ParseState.EXPECT_FLAG
            }

            ParseState.EXPECT_TOOL_PATH ->
            {
                toolPath = Path.of(token)
                state = ParseState.EXPECT_FLAG
            }
        }
    }

    if (state != ParseState.EXPECT_FLAG)
        throw IllegalArgumentException("Missing value after flag.")

    return CliOptions(
        source = source,
        pngDestination = pngDestination,
        exeTarget = exeTarget,
        toolPath = toolPath,
        showHelp = showHelp,
        showVersion = showVersion
    )
}

/**
 * Generate a temporary `.ico` from SVG, then call `rcedit` to patch the target EXE in place.
 *
 * @param toolPath Absolute path to `rcedit.exe`.
 * @param sourceSvg Absolute path to source SVG.
 * @param targetExe Absolute path to destination EXE to patch.
 * @throws IllegalStateException When `rcedit` exits with non-zero code.
 */
private fun patchExeIconInPlace(toolPath: Path, sourceSvg: Path, targetExe: Path)
{
    val tempIco = Files.createTempFile("icontoolkit-", ".ico")

    try
    {
        SVGraphics(sourceSvg.toString())
            .inline()
            .saveAsIco(tempIco, 256, 256)

        val process = ProcessBuilder(
            toolPath.toString(),
            targetExe.toString(),
            "--set-icon",
            tempIco.toString()
        )
            .redirectErrorStream(true)
            .start()

        val output = process.inputStream
            .bufferedReader(StandardCharsets.UTF_8)
            .use { it.readText() }
            .trim()
        val exitCode = process.waitFor()

        if (exitCode != 0)
        {
            val details = if (output.isNotBlank()) " Output: $output" else ""
            throw IllegalStateException("rcedit failed (exit=$exitCode).$details")
        }
    }
    finally
    {
        Files.deleteIfExists(tempIco)
    }
}

/**
 * Resolve which `rcedit` executable to use.
 *
 * Priority:
 * 1) explicit `--tool` path
 * 2) auto-discovery (`rcedit-x64.exe` / `rcedit-x86.exe` / `rcedit.exe`)
 *
 * @param toolPath Optional explicit tool path from CLI.
 * @param targetExe Target EXE path; used as a search hint for auto-discovery.
 * @return Existing tool path.
 * @throws IllegalArgumentException When explicit path is invalid or auto-discovery fails.
 */
private fun resolveRceditTool(toolPath: Path?, targetExe: Path): Path
{
    if (toolPath != null)
    {
        val normalized = toolPath.toAbsolutePath().normalize()
        if (!Files.isRegularFile(normalized))
            throw IllegalArgumentException("rcedit not found: $normalized")
        return normalized
    }

    val found = autoFindRcedit(targetExe)
    if (found != null)
    {
        println("Auto-detected rcedit: $found")
        return found
    }

    val expected = preferredRceditNames().joinToString(", ")
    throw IllegalArgumentException(
        "Cannot find rcedit automatically. Expected one of: $expected. " +
            "Use --tool=<path to rcedit.exe>."
    )
}

/**
 * Try to find `rcedit` in common locations and in PATH.
 *
 * @param targetExe Target EXE path; its parent directory is searched first.
 * @return Matching tool path, or `null` when not found.
 */
private fun autoFindRcedit(targetExe: Path): Path?
{
    val names = preferredRceditNames()
    val searchDirs = LinkedHashSet<Path>()

    searchDirs.add(Path.of("").toAbsolutePath().normalize())
    targetExe.parent?.let { searchDirs.add(it.toAbsolutePath().normalize()) }

    val pathEnv = System.getenv("PATH").orEmpty()
    for (entry in pathEnv.split(File.pathSeparator))
    {
        if (entry.isBlank())
            continue

        runCatching {
            searchDirs.add(Path.of(entry).toAbsolutePath().normalize())
        }
    }

    for (dir in searchDirs)
    {
        for (name in names)
        {
            val candidate = dir.resolve(name)
            if (Files.isRegularFile(candidate))
                return candidate
        }
    }

    return null
}

/**
 * Preferred `rcedit` executable names ordered by current JVM bitness.
 *
 * @return Ordered file-name candidates for lookup.
 */
private fun preferredRceditNames(): List<String>
{
    return if (is64BitJvm())
        listOf("rcedit-x64.exe", "rcedit.exe", "rcedit-x86.exe")
    else
        listOf("rcedit-x86.exe", "rcedit.exe", "rcedit-x64.exe")
}

/**
 * Check whether current JVM architecture is 64-bit.
 *
 * @return `true` for 64-bit JVM.
 */
private fun is64BitJvm(): Boolean
{
    val arch = System.getProperty("os.arch", "").lowercase()
    return arch.contains("64")
}

/**
 * Ensure output path uses `.png` extension.
 *
 * @param path User-provided output path.
 * @return Pair of normalized PNG path and whether extension was rewritten.
 */
private fun normalizePngOutputPath(path: Path): Pair<Path, Boolean>
{
    val fileName = path.fileName?.toString().orEmpty()
    if (fileName.lowercase().endsWith(".png"))
        return path to false

    val baseName = if ("." in fileName) fileName.substringBeforeLast(".") else fileName.ifBlank { "output" }
    return path.resolveSibling("$baseName.png") to true
}

/**
 * Check whether the current runtime OS is Windows.
 *
 * @return `true` when running on Windows.
 */
private fun isWindows(): Boolean =
    System.getProperty("os.name", "").lowercase().contains("win")

/**
 * Print CLI usage and supported options.
 */
private fun printUsage()
{
    println("IconToolkit $VERSION")
    println("Usage:")
    println("  IconToolkit -s <source.svg> -d <output.png>")
    println("  IconToolkit [--tool=<path to rcedit.exe>] -s <source.svg> -t <dest.exe>")
    println("")
    println("Options:")
    println("  -s <path>         Source SVG file")
    println("  -d <path>         Export PNG (non-.png extension is rewritten to .png with warning)")
    println("  -t <path>         Target EXE file (patched in place, Windows only)")
    println("  --tool=<path>     Optional rcedit path; if missing, auto-detect x64/x86/default")
    println("  -h, --help        Show help")
    println("  -v, --version     Show version")
}
