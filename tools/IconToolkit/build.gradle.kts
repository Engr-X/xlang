plugins {
    kotlin("jvm") version "2.2.21"
    application
}

group = "com.diwang"
version = "alpha-1.1.0"

repositories {
    mavenCentral()
}

dependencies {
    implementation("com.github.weisj:jsvg:2.0.0")
    testImplementation(kotlin("test"))
}

tasks.test {
    useJUnitPlatform()
}

application {
    mainClass.set("com.diwang.icontoolkit.MainKt")
}

tasks.jar {
    archiveFileName.set("IconToolkit.jar")
    duplicatesStrategy = DuplicatesStrategy.EXCLUDE
    manifest {
        attributes["Main-Class"] = "com.diwang.icontoolkit.MainKt"
    }
    dependsOn(configurations.runtimeClasspath)
    from({
        configurations.runtimeClasspath.get()
            .filter { it.name.endsWith(".jar") }
            .map { zipTree(it) }
    })
}
