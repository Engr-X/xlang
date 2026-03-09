# Root directory (this Makefile is expected at repo root)
ROOT_DIR := $(abspath .)

# Build output directory (override: make BUILD_DIR=out)
BUILD_DIR ?= build
BUILD_DIR_ABS := $(ROOT_DIR)/$(BUILD_DIR)

# Parallel jobs (default: number of CPUs)
JOBS ?= $(shell (command -v nproc >/dev/null 2>&1 && nproc) || echo 8)

# Haskell compiler (Cabal)
COMPILER_DIR := $(ROOT_DIR)/compiler
EXE := xlang

# Java tool (Gradle)
BYTECODEGEN_DIR := $(ROOT_DIR)/tools/BytecodeToolkit
TOOLS_OUT_DIR := $(BUILD_DIR_ABS)/tools

# Java library project (Gradle)
JAVA_LIB_DIR := $(ROOT_DIR)/libs/java
JAVA_LIB_OUT_DIR := $(BUILD_DIR_ABS)/libs/java

# Keep Gradle caches local to avoid global cache issues
GRADLE_USER_HOME := $(BUILD_DIR_ABS)/.gradle-home
GRADLE_ARGS := --console=plain --no-daemon

.PHONY: all compile tools java_lib clean clean_ide rebuild

all: compile tools java_lib

compile:
	mkdir -p "$(BUILD_DIR_ABS)"
	cd "$(COMPILER_DIR)" && cabal build -j$(JOBS) exe:$(EXE)
	EXE_PATH=$$(cd "$(COMPILER_DIR)" && cabal list-bin exe:$(EXE)); \
	cp "$$EXE_PATH" "$(BUILD_DIR_ABS)/"

tools:
	mkdir -p "$(TOOLS_OUT_DIR)"
	cd "$(BYTECODEGEN_DIR)" && chmod +x ./gradlew
	cd "$(BYTECODEGEN_DIR)" && GRADLE_USER_HOME="$(GRADLE_USER_HOME)" ./gradlew build $(GRADLE_ARGS)
	cp -f "$(BYTECODEGEN_DIR)"/build/libs/*.jar "$(TOOLS_OUT_DIR)/" 2>/dev/null || true

java_lib:
	mkdir -p "$(JAVA_LIB_OUT_DIR)"
	cd "$(JAVA_LIB_DIR)" && chmod +x ./gradlew
	cd "$(JAVA_LIB_DIR)" && GRADLE_USER_HOME="$(GRADLE_USER_HOME)" ./gradlew build $(GRADLE_ARGS)
	cp -f "$(JAVA_LIB_DIR)"/build/libs/* "$(JAVA_LIB_OUT_DIR)/" 2>/dev/null || true

clean:
	cd "$(COMPILER_DIR)" && cabal clean
	cd "$(BYTECODEGEN_DIR)" && (GRADLE_USER_HOME="$(GRADLE_USER_HOME)" ./gradlew --stop $(GRADLE_ARGS) || true)
	cd "$(JAVA_LIB_DIR)" && (GRADLE_USER_HOME="$(GRADLE_USER_HOME)" ./gradlew --stop $(GRADLE_ARGS) || true)
	cd "$(BYTECODEGEN_DIR)" && (GRADLE_USER_HOME="$(GRADLE_USER_HOME)" ./gradlew clean $(GRADLE_ARGS) || true)
	cd "$(JAVA_LIB_DIR)" && (GRADLE_USER_HOME="$(GRADLE_USER_HOME)" ./gradlew clean $(GRADLE_ARGS) || true)
	rm -rf "$(GRADLE_USER_HOME)/caches" || true
	cd "$(BYTECODEGEN_DIR)" && (GRADLE_USER_HOME="$(GRADLE_USER_HOME)" ./gradlew clean $(GRADLE_ARGS) || true)
	cd "$(JAVA_LIB_DIR)" && (GRADLE_USER_HOME="$(GRADLE_USER_HOME)" ./gradlew clean $(GRADLE_ARGS) || true)
	rm -rf "$(BUILD_DIR_ABS)/tools" "$(BUILD_DIR_ABS)/libs" "$(BUILD_DIR_ABS)/$(EXE)" "$(BUILD_DIR_ABS)"/*.exe 2>/dev/null || true
	$(MAKE) clean_ide

clean_ide:
	rm -rf "$(ROOT_DIR)/.vscode" "$(ROOT_DIR)/.idea" 2>/dev/null || true
	rm -rf "$(COMPILER_DIR)/.vscode" "$(COMPILER_DIR)/.idea" 2>/dev/null || true
	rm -rf "$(BYTECODEGEN_DIR)/.vscode" "$(BYTECODEGEN_DIR)/.idea" 2>/dev/null || true
	rm -rf "$(JAVA_LIB_DIR)/.vscode" "$(JAVA_LIB_DIR)/.idea" 2>/dev/null || true

rebuild: clean all
