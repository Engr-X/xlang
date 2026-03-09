import org.gradle.api.GradleException
import org.gradle.api.tasks.PathSensitivity
import org.gradle.api.tasks.SourceSetContainer
import org.gradle.kotlin.dsl.the

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
val xlangExe = providers.gradleProperty("xlangExe")
    .orElse(repoRoot.resolve("build/xlang.exe").absolutePath)

val xlangFiles = fileTree(xlangSourceRoot) {
    include("**/*.x")
}

val compileXlang = tasks.register("compileXlang") {
    group = "build"
    description = "Compile .x sources under src/main/xlang into JVM .class files."

    inputs.files(xlangFiles).withPathSensitivity(PathSensitivity.RELATIVE)
    inputs.property("xlangExe", xlangExe)
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

        if (xlangFiles.isEmpty) {
            logger.lifecycle("[xlang] no .x files under ${xlangSourceRoot.absolutePath}")
        }

        xlangFiles.files.sorted().forEach { src ->
            val rel = src.relativeTo(xlangSourceRoot).invariantSeparatorsPath
            logger.lifecycle("[xlang] compiling $rel")
            project.exec {
                workingDir = repoRoot
                commandLine(
                    exe.absolutePath,
                    "--target=jvm",
                    "--root=${xlangSourceRoot.absolutePath}",
                    rel,
                    "-d", outDir.absolutePath
                )
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

kotlin {
    jvmToolchain(8)
}

tasks.test {
    useJUnitPlatform()
}
