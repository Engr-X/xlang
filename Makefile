# Build output directory (user can override: make BUILD_DIR=out)
BUILD_DIR ?= build

# Parallel jobs (default: number of CPUs)
JOBS ?= $(shell nproc 2>/dev/null || echo 8)

# Haskell compiler (Cabal)
COMPILER_DIR := compiler
EXE := xlang

# Java tool (Gradle)
BYTECODEGEN_DIR := tools/BytecodeGenerator
TOOLS_OUT_DIR := $(BUILD_DIR)/tools

# Java library project (Gradle)
JAVA_LIB_DIR := libs/java
JAVA_LIB_OUT_DIR := $(BUILD_DIR)/libs/java

# Keep Gradle caches local to avoid NFS ~/.gradle issues
GRADLE_USER_HOME := $(abspath $(BUILD_DIR)/.gradle-home)

.PHONY: all compile tools java_lib clean rebuild

all: compile tools java_lib

# ---------- Cabal ----------
compile: | $(BUILD_DIR)
	cd $(COMPILER_DIR) && cabal build -j$(JOBS) exe:$(EXE)
	# Copy the built executable into BUILD_DIR, keeping its original name
	EXE_PATH=$$(cd $(COMPILER_DIR) && cabal list-bin exe:$(EXE)); \
	cp "$$EXE_PATH" "$(BUILD_DIR)/"

# ---------- Tools (Gradle) ----------
tools: | $(TOOLS_OUT_DIR)
	cd $(BYTECODEGEN_DIR) && chmod +x ./gradlew
	cd $(BYTECODEGEN_DIR) && GRADLE_USER_HOME="$(GRADLE_USER_HOME)" ./gradlew build
	# Copy all jars to build/tools (best-effort if none)
	cp -f $(BYTECODEGEN_DIR)/build/libs/*.jar $(TOOLS_OUT_DIR)/ 2>/dev/null || true

# ---------- Java library (Gradle) ----------
java_lib: | $(JAVA_LIB_OUT_DIR)
	cd $(JAVA_LIB_DIR) && chmod +x ./gradlew
	cd $(JAVA_LIB_DIR) && GRADLE_USER_HOME="$(GRADLE_USER_HOME)" ./gradlew build
	# Copy everything from build/libs to build/libs/java (best-effort if empty)
	cp -f $(JAVA_LIB_DIR)/build/libs/* $(JAVA_LIB_OUT_DIR)/ 2>/dev/null || true

# ---------- Directories ----------
$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(TOOLS_OUT_DIR): | $(BUILD_DIR)
	mkdir -p $(TOOLS_OUT_DIR)

$(JAVA_LIB_OUT_DIR): | $(BUILD_DIR)
	mkdir -p $(JAVA_LIB_OUT_DIR)

# ---------- Clean (lab/NFS friendly) ----------
clean:
	cd $(COMPILER_DIR) && cabal clean

	# Ensure wrappers are executable (avoid Permission denied)
	cd $(BYTECODEGEN_DIR) && chmod +x ./gradlew || true
	cd $(JAVA_LIB_DIR) && chmod +x ./gradlew || true

	# Stop daemons first
	cd $(BYTECODEGEN_DIR) && (GRADLE_USER_HOME="$(GRADLE_USER_HOME)" ./gradlew --stop || true)
	cd $(JAVA_LIB_DIR) && (GRADLE_USER_HOME="$(GRADLE_USER_HOME)" ./gradlew --stop || true)

	# Run Gradle clean (force), prefer no-daemon to reduce NFS/lock issues
	cd $(BYTECODEGEN_DIR) && (GRADLE_USER_HOME="$(GRADLE_USER_HOME)" ./gradlew clean --no-daemon || true)
	cd $(JAVA_LIB_DIR) && (GRADLE_USER_HOME="$(GRADLE_USER_HOME)" ./gradlew clean --no-daemon || true)

	# If caches are corrupted, wipe local Gradle home and try clean once more (best-effort)
	rm -rf "$(GRADLE_USER_HOME)/caches" || true
	cd $(BYTECODEGEN_DIR) && (GRADLE_USER_HOME="$(GRADLE_USER_HOME)" ./gradlew clean --no-daemon || true)
	cd $(JAVA_LIB_DIR) && (GRADLE_USER_HOME="$(GRADLE_USER_HOME)" ./gradlew clean --no-daemon || true)

	# Remove build dir (best-effort)
	rm -rf $(BUILD_DIR)/tools $(BUILD_DIR)/libs $(BUILD_DIR)/$(EXE) $(BUILD_DIR)/*.exe 2>/dev/null || true