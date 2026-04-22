import org.gradle.api.JavaVersion
import org.gradle.api.tasks.compile.JavaCompile
import org.gradle.jvm.toolchain.JavaLanguageVersion
import org.jetbrains.kotlin.gradle.dsl.JvmTarget
import org.jetbrains.kotlin.gradle.tasks.KotlinJvmCompile

plugins {
    kotlin("jvm") version "2.2.21"
    kotlin("plugin.serialization") version "2.2.21"
    application
}

group = "com.wangdi"
version = "alpha"

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.ow2.asm:asm:9.9.1")
    implementation("com.alibaba.fastjson2:fastjson2:2.0.61")
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.10.0")
    implementation("org.xerial:sqlite-jdbc:3.51.2.0")
    testImplementation(kotlin("test"))
}

val requestedJvmToolchain = providers.gradleProperty("xlang.java.toolchain")
    .orNull
    ?.toIntOrNull()

val detectedJvmToolchain = JavaVersion.current().majorVersion.toIntOrNull()

val jvmToolchainVersion = (requestedJvmToolchain ?: detectedJvmToolchain ?: 8).coerceAtLeast(8)

logger.lifecycle("[BytecodeToolkit] Kotlin toolchain JDK=$jvmToolchainVersion (bytecode JVM 1.8)")

java {
    sourceCompatibility = JavaVersion.VERSION_1_8
    targetCompatibility = JavaVersion.VERSION_1_8
}

tasks.withType<JavaCompile>().configureEach {
    options.release.set(8)
}

kotlin {
    jvmToolchain {
        languageVersion.set(JavaLanguageVersion.of(jvmToolchainVersion))
    }
}

tasks.withType<KotlinJvmCompile>().configureEach {
    compilerOptions.jvmTarget.set(JvmTarget.JVM_1_8)
}

application {
    mainClass.set("com.wangdi.bctoolkit.MainKt")
}

tasks.jar {
    manifest {
        attributes["Main-Class"] = "com.wangdi.bctoolkit.MainKt"
    }
    duplicatesStrategy = DuplicatesStrategy.EXCLUDE
    from(configurations.runtimeClasspath.get().map { if (it.isDirectory) it else zipTree(it) })
}

tasks.test {
    useJUnitPlatform()
}
