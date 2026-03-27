import org.gradle.api.GradleException
import org.gradle.api.tasks.PathSensitivity
import org.gradle.api.tasks.SourceSetContainer
import org.gradle.api.tasks.Sync
import org.gradle.kotlin.dsl.the
import java.io.File

plugins {
    kotlin("jvm") version "2.2.21"
}

group = "com.wangdi"
version = "alpha"

repositories {
    mavenCentral()
}

dependencies {
    testImplementation(kotlin("test"))
}

val repoRoot = projectDir.resolve("../..").canonicalFile
val xlangSourceRoot = projectDir.resolve("src/main/xlang")
val xlangOutDir = layout.buildDirectory.dir("generated/xlang/classes")
val runtimeLibsDir = layout.buildDirectory.dir("runtime-libs")
val xlangExe = providers.gradleProperty("xlangExe")
    .orElse(repoRoot.resolve("build/xlang.exe").absolutePath)

val xlangJobs = providers.gradleProperty("xlangJobs")
    .orElse("1")

val xlangFiles = fileTree(xlangSourceRoot) {
    include("**/*.x")
    include("**/*.xl")
}

val compileXlang = tasks.register("compileXlang") {
    group = "build"
    description = "Compile .x sources under src/main/xlang into JVM .class files."

    inputs.files(xlangFiles).withPathSensitivity(PathSensitivity.RELATIVE)
    inputs.property("xlangExe", xlangExe)
    inputs.property("xlangJobs", xlangJobs)
    outputs.dir(xlangOutDir)

    doLast {
        val exe = file(xlangExe.get())
        if (!exe.exists()) {
            throw GradleException(
                "xlang executable not found: ${exe.absolutePath}\n" +
                    "Build it first from repo root, e.g. build.bat compile"
            )
        }

        val outDir = xlangOutDir.get().asFile
        if (outDir.exists()) outDir.deleteRecursively()
        outDir.mkdirs()

        val jobs = xlangJobs.get().toIntOrNull()?.takeIf { it > 0 }
            ?: throw GradleException("invalid xlangJobs: '${xlangJobs.get()}', expected positive integer")

        val relFiles = xlangFiles.files
            .sorted()
            .map { it.relativeTo(xlangSourceRoot).invariantSeparatorsPath }

        if (relFiles.isEmpty()) {
            logger.lifecycle("[xlang] no .x files under ${xlangSourceRoot.absolutePath}")
        } else {
            logger.lifecycle("[xlang] compiling ${relFiles.size} file(s) with -j$jobs")
            val args = mutableListOf(
                exe.absolutePath,
                "--target=jvm",
                "--root=${xlangSourceRoot.absolutePath}"
            )
            args.addAll(relFiles)
            args.addAll(listOf("-d", outDir.absolutePath, "-j$jobs"))

            project.exec {
                workingDir = repoRoot
                commandLine(args)
                val javaHome = System.getenv("JAVA_HOME") ?: System.getProperty("java.home")
                val javaBin = File(javaHome, "bin").absolutePath
                val oldPath = System.getenv("Path") ?: System.getenv("PATH") ?: ""
                val mergedPath = javaBin + File.pathSeparator + oldPath
                environment("JAVA_HOME", javaHome)
                environment("PATH", mergedPath)
                environment("Path", mergedPath)
            }
        }
    }
}

the<SourceSetContainer>().named("main") {
    output.dir(xlangOutDir, "builtBy" to compileXlang)
}

tasks.named("classes") {
    dependsOn(compileXlang)
}

val copyRuntimeLibs = tasks.register<Sync>("copyRuntimeLibs") {
    from(configurations.named("runtimeClasspath"))
    include("*.jar")
    into(runtimeLibsDir)
}

tasks.named("build") {
    dependsOn(copyRuntimeLibs)
}

kotlin {
    jvmToolchain(8)
}

tasks.test {
    useJUnitPlatform()
}
