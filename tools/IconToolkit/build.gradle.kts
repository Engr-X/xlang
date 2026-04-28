plugins {
    kotlin("jvm") version "2.2.21"
    application
}

group = "com.wangdi"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    testImplementation(kotlin("test"))
}

tasks.test {
    useJUnitPlatform()
}

application {
    mainClass.set("com.diwang.IconToolkit.MainKt")
}

tasks.jar {
    archiveFileName.set("IconToolkit.jar")
    manifest {
        attributes["Main-Class"] = "com.diwang.IconToolkit.MainKt"
    }
}
