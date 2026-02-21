plugins {
    kotlin("jvm") version "2.2.21"
}

group = "com.wangdi"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.ow2.asm:asm:9.9.1")
    testImplementation(kotlin("test"))
}

kotlin {
    jvmToolchain(17)
}

tasks.test {
    useJUnitPlatform()
}