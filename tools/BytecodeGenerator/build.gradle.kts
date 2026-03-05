plugins {
    kotlin("jvm") version "2.2.21"
    application
}

group = "com.wangdi"
version = "1.0"

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.ow2.asm:asm:9.9.1")
    implementation("com.alibaba.fastjson2:fastjson2:2.0.61")
    testImplementation(kotlin("test"))
}

kotlin {
    jvmToolchain(8)
}

application {
    mainClass.set("com.wangdi.classgen.MainKt")
}

tasks.jar {
    manifest {
        attributes["Main-Class"] = "com.wangdi.classgen.MainKt"
    }
    duplicatesStrategy = DuplicatesStrategy.EXCLUDE
    from(configurations.runtimeClasspath.get().map { if (it.isDirectory) it else zipTree(it) })
}

tasks.test {
    useJUnitPlatform()
}
