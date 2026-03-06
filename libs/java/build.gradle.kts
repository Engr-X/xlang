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

kotlin {
    jvmToolchain(8)
}

tasks.test {
    useJUnitPlatform()
}