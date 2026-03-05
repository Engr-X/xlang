#!/usr/bin/env bash
set -euo pipefail

# ---------------- Config (override via env) ----------------
BUILD_DIR="${BUILD_DIR:-build}"

if command -v nproc >/dev/null 2>&1; then
    JOBS="${JOBS:-$(nproc)}"
else
    JOBS="${JOBS:-8}"
fi

COMPILER_DIR="compiler"
EXE="xlang"

BYTECODEGEN_DIR="tools/BytecodeGenerator"
TOOLS_OUT_DIR="$BUILD_DIR/tools"

JAVA_LIB_DIR="libs/java"
JAVA_LIB_OUT_DIR="$BUILD_DIR/libs/java"

# keep Gradle caches local (absolute path, no realpath dependency)
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GRADLE_USER_HOME="$ROOT_DIR/$BUILD_DIR/.gradle-home"

# Force Gradle to not emit ANSI cursor-control sequences
GRADLE_ARGS=(--console=plain --no-daemon)
export TERM="${TERM:-dumb}"

# Locale (optional but helps in some terminals)
export LANG="${LANG:-C.UTF-8}"
export LC_ALL="${LC_ALL:-C.UTF-8}"

# ---------------- Targets ----------------
compile() {
    mkdir -p "$BUILD_DIR"
    ( cd "$COMPILER_DIR" && cabal build -j"$JOBS" "exe:$EXE" )

    local exe_path
    exe_path="$(cd "$COMPILER_DIR" && cabal list-bin "exe:$EXE")"
    cp "$exe_path" "$BUILD_DIR/"
    echo "✅ copied: $exe_path -> $BUILD_DIR/"
}

tools() {
    mkdir -p "$TOOLS_OUT_DIR"

    ( cd "$BYTECODEGEN_DIR" && chmod +x ./gradlew )
    ( cd "$BYTECODEGEN_DIR" && GRADLE_USER_HOME="$GRADLE_USER_HOME" ./gradlew build "${GRADLE_ARGS[@]}" )

    cp -f "$BYTECODEGEN_DIR"/build/libs/*.jar "$TOOLS_OUT_DIR"/ 2>/dev/null || true
    echo "✅ tools jars copied to: $TOOLS_OUT_DIR/"
}

java_lib() {
    mkdir -p "$JAVA_LIB_OUT_DIR"

    ( cd "$JAVA_LIB_DIR" && chmod +x ./gradlew )
    ( cd "$JAVA_LIB_DIR" && GRADLE_USER_HOME="$GRADLE_USER_HOME" ./gradlew build "${GRADLE_ARGS[@]}" )

    cp -f "$JAVA_LIB_DIR"/build/libs/* "$JAVA_LIB_OUT_DIR"/ 2>/dev/null || true
    echo "✅ java lib outputs copied to: $JAVA_LIB_OUT_DIR/"
}

all() {
    compile
    tools
    java_lib
}

clean() {
    ( cd "$COMPILER_DIR" && cabal clean )

    ( cd "$BYTECODEGEN_DIR" && chmod +x ./gradlew ) || true
    ( cd "$JAVA_LIB_DIR" && chmod +x ./gradlew ) || true

    ( cd "$BYTECODEGEN_DIR" && GRADLE_USER_HOME="$GRADLE_USER_HOME" ./gradlew --stop "${GRADLE_ARGS[@]}" ) || true
    ( cd "$JAVA_LIB_DIR" && GRADLE_USER_HOME="$GRADLE_USER_HOME" ./gradlew --stop "${GRADLE_ARGS[@]}" ) || true

    ( cd "$BYTECODEGEN_DIR" && GRADLE_USER_HOME="$GRADLE_USER_HOME" ./gradlew clean "${GRADLE_ARGS[@]}" ) || true
    ( cd "$JAVA_LIB_DIR" && GRADLE_USER_HOME="$GRADLE_USER_HOME" ./gradlew clean "${GRADLE_ARGS[@]}" ) || true

    rm -rf "$GRADLE_USER_HOME/caches" || true

    ( cd "$BYTECODEGEN_DIR" && GRADLE_USER_HOME="$GRADLE_USER_HOME" ./gradlew clean "${GRADLE_ARGS[@]}" ) || true
    ( cd "$JAVA_LIB_DIR" && GRADLE_USER_HOME="$GRADLE_USER_HOME" ./gradlew clean "${GRADLE_ARGS[@]}" ) || true

    rm -rf "$BUILD_DIR/tools" "$BUILD_DIR/libs" "$BUILD_DIR/$EXE" "$BUILD_DIR"/*.exe 2>/dev/null || true
    echo "🧹 clean done"
}

rebuild() {
    clean
    all
}

usage() {
    cat <<'EOF'
Usage: ./build.sh [all|compile|tools|java_lib|clean|rebuild]

Env overrides:
    BUILD_DIR=out   (default: build)
    JOBS=24         (default: nproc or 8)
EOF
}

cmd="${1:-all}"
case "$cmd" in
    all) all ;;
    compile) compile ;;
    tools) tools ;;
    java_lib) java_lib ;;
    clean) clean ;;
    rebuild) rebuild ;;
    -h|--help|help) usage ;;
    *) echo "Unknown command: $cmd" >&2; usage; exit 2 ;;
esac