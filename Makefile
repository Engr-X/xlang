SHELL := /bin/sh

# Root directory (this Makefile is expected at repo root)
ROOT_DIR := $(abspath .)

# Build output directory (override: make BUILD_DIR=out)
BUILD_DIR ?= build
BUILD_DIR_ABS := $(ROOT_DIR)/$(BUILD_DIR)

# Parse make parallel flag, e.g. `make -j8` or `make --jobs=8`
MAKE_JOBS_LONG := $(patsubst --jobs=%,%,$(filter --jobs=%,$(MAKEFLAGS)))
MAKE_JOBS_SHORT := $(patsubst -j%,%,$(filter -j%,$(MAKEFLAGS)))
MAKE_JOBS := $(firstword $(filter-out ,$(MAKE_JOBS_LONG) $(MAKE_JOBS_SHORT)))

# Parallel jobs (default: from make -j, otherwise CPU count)
JOBS ?= $(if $(MAKE_JOBS),$(MAKE_JOBS),$(shell (command -v nproc >/dev/null 2>&1 && nproc) || (getconf _NPROCESSORS_ONLN 2>/dev/null) || echo 8))

# Optional pre-build metadata refresh.
# Use: make CABAL_UPDATE=1 all
CABAL_UPDATE ?= 0
CABAL ?= cabal

# xlang std compile jobs for libs/java
# override explicitly if needed: make java_lib XLANG_JOBS=9
XLANG_JOBS ?= $(JOBS)

# Haskell compiler (Cabal)
COMPILER_DIR := $(ROOT_DIR)/compiler
EXE := xlang
EXE_OUT := $(BUILD_DIR_ABS)/$(EXE)

# Java tool (Gradle)
BYTECODEGEN_DIR := $(ROOT_DIR)/tools/BytecodeToolkit
TOOLS_OUT_DIR := $(BUILD_DIR_ABS)/tools

# Java library project (Gradle)
JAVA_LIB_DIR := $(ROOT_DIR)/libs/java
JAVA_LIB_OUT_DIR := $(BUILD_DIR_ABS)/libs/java

# Keep Gradle caches local to avoid global cache issues
GRADLE_USER_HOME := $(BUILD_DIR_ABS)/.gradle-home
GRADLE_ARGS := --console=plain --no-daemon

.PHONY: help all build compile update maybe_update tools java_lib clean clean_ide rebuild

help:
	@echo "Usage: make [target] [JOBS=N] [CABAL_UPDATE=1]"
	@echo ""
	@echo "Targets:"
	@echo "  all/build   Build compiler + tools + java_lib"
	@echo "  compile     Build xlang executable"
	@echo "  update      Run cabal update in compiler/"
	@echo "  tools       Build BytecodeToolkit jars"
	@echo "  java_lib    Build libs/java artifacts"
	@echo "  clean       Clean build artifacts"
	@echo "  clean_ide   Remove .vscode/.idea folders"
	@echo "  rebuild     clean + all"
	@echo ""
	@echo "Examples:"
	@echo "  make -j8 all"
	@echo "  make CABAL_UPDATE=1 compile"

all: compile tools java_lib

# Alias for people who prefer `make build`
build: all

update:
	cd "$(COMPILER_DIR)" && $(CABAL) update

maybe_update:
ifeq ($(CABAL_UPDATE),1)
	$(MAKE) update
else
	@true
endif

compile: maybe_update
	mkdir -p "$(BUILD_DIR_ABS)"
	cd "$(COMPILER_DIR)" && $(CABAL) build -j$(JOBS) exe:$(EXE)
	EXE_PATH=$$(cd "$(COMPILER_DIR)" && $(CABAL) list-bin exe:$(EXE)); \
		cp -f "$$EXE_PATH" "$(BUILD_DIR_ABS)/"

tools:
	mkdir -p "$(TOOLS_OUT_DIR)"
	cd "$(BYTECODEGEN_DIR)" && chmod +x ./gradlew
	cd "$(BYTECODEGEN_DIR)" && GRADLE_USER_HOME="$(GRADLE_USER_HOME)" ./gradlew build -PxlangJobs=$(JOBS) $(GRADLE_ARGS)
	cp -f "$(BYTECODEGEN_DIR)"/build/libs/*.jar "$(TOOLS_OUT_DIR)/" 2>/dev/null || true

java_lib: compile
	mkdir -p "$(JAVA_LIB_OUT_DIR)"
	cd "$(JAVA_LIB_DIR)" && chmod +x ./gradlew
	cd "$(JAVA_LIB_DIR)" && GRADLE_USER_HOME="$(GRADLE_USER_HOME)" ./gradlew build -PxlangJobs=$(XLANG_JOBS) -PxlangExe="$(EXE_OUT)" $(GRADLE_ARGS)
	cp -f "$(JAVA_LIB_DIR)"/build/libs/* "$(JAVA_LIB_OUT_DIR)/" 2>/dev/null || true
	cp -f "$(JAVA_LIB_DIR)"/build/runtime-libs/*.jar "$(JAVA_LIB_OUT_DIR)/" 2>/dev/null || true

clean:
	cd "$(COMPILER_DIR)" && $(CABAL) clean
	cd "$(BYTECODEGEN_DIR)" && (GRADLE_USER_HOME="$(GRADLE_USER_HOME)" ./gradlew --stop $(GRADLE_ARGS) || true)
	cd "$(JAVA_LIB_DIR)" && (GRADLE_USER_HOME="$(GRADLE_USER_HOME)" ./gradlew --stop $(GRADLE_ARGS) || true)
	cd "$(BYTECODEGEN_DIR)" && (GRADLE_USER_HOME="$(GRADLE_USER_HOME)" ./gradlew clean $(GRADLE_ARGS) || true)
	cd "$(JAVA_LIB_DIR)" && (GRADLE_USER_HOME="$(GRADLE_USER_HOME)" ./gradlew clean $(GRADLE_ARGS) || true)
	rm -rf "$(GRADLE_USER_HOME)/caches" || true
	rm -rf "$(BUILD_DIR_ABS)/tools" "$(BUILD_DIR_ABS)/libs" "$(BUILD_DIR_ABS)/$(EXE)" "$(BUILD_DIR_ABS)"/*.exe 2>/dev/null || true
	$(MAKE) clean_ide

clean_ide:
	rm -rf "$(ROOT_DIR)/.vscode" "$(ROOT_DIR)/.idea" 2>/dev/null || true
	rm -rf "$(COMPILER_DIR)/.vscode" "$(COMPILER_DIR)/.idea" 2>/dev/null || true
	rm -rf "$(BYTECODEGEN_DIR)/.vscode" "$(BYTECODEGEN_DIR)/.idea" 2>/dev/null || true
	rm -rf "$(JAVA_LIB_DIR)/.vscode" "$(JAVA_LIB_DIR)/.idea" 2>/dev/null || true

rebuild: clean all
