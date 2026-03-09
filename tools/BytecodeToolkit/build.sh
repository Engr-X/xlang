#!/usr/bin/env bash
set -euo pipefail

# ---------------- Config (override via env) ----------------
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_DIR="${BUILD_DIR:-build}"
BUILD_DIR_ABS="$ROOT_DIR/$BUILD_DIR"

if command -v nproc >/dev/null 2>&1; then
    JOBS="${JOBS:-$(nproc)}"
else
    JOBS="${JOBS:-8}"
fi

COMPILER_DIR="$ROOT_DIR/compiler"
EXE="xlang"

BYTECODEGEN_DIR="$ROOT_DIR/tools/BytecodeGenerator"
TOOLS_OUT_DIR="$BUILD_DIR_ABS/tools"

JAVA_LIB_DIR="$ROOT_DIR/libs/java"
JAVA_LIB_OUT_DIR="$BUILD_DIR_ABS/libs/java"

# keep Gradle caches local
GRADLE_USER_HOME="$BUILD_DIR_ABS/.gradle-home"

# Force Gradle to not emit ANSI cursor-control sequences
GRADLE_ARGS=(--console=plain --no-daemon)
export TERM="${TERM:-dumb}"

# Locale (optional but helps in some terminals)
export LANG="${LANG:-C.UTF-8}"
export LC_ALL="${LC_ALL:-C.UTF-8}"

remove_ide_dirs() {
    rm -rf \
        "$ROOT_DIR/.vscode" \
        "$ROOT_DIR/.idea" \
        "$COMPILER_DIR/.vscode" \
        "$COMPILER_DIR/.idea" \
        "$BYTECODEGEN_DIR/.vscode" \
        "$BYTECODEGEN_DIR/.idea" \
        "$JAVA_LIB_DIR/.vscode" \
        "$JAVA_LIB_DIR/.idea" 2>/dev/null || true
}

# ---------------- Targets ----------------
compile() {
    mkdir -p "$BUILD_DIR_ABS"
    ( cd "$COMPILER_DIR" && cabal build -j"$JOBS" "exe:$EXE" )

    local exe_path
    exe_path="$(cd "$COMPILER_DIR" && cabal list-bin "exe:$EXE")"
    cp "$exe_path" "$BUILD_DIR_ABS/"
    echo "[OK] copied: $exe_path -> $BUILD_DIR_ABS/"
}

tools() {
    mkdir -p "$TOOLS_OUT_DIR"

    ( cd "$BYTECODEGEN_DIR" && chmod +x ./gradlew )
    ( cd "$BYTECODEGEN_DIR" && GRADLE_USER_HOME="$GRADLE_USER_HOME" ./gradlew build "${GRADLE_ARGS[@]}" )

    cp -f "$BYTECODEGEN_DIR"/build/libs/*.jar "$TOOLS_OUT_DIR"/ 2>/dev/null || true
    echo "[OK] tools jars copied to: $TOOLS_OUT_DIR/"
}

java_lib() {
    mkdir -p "$JAVA_LIB_OUT_DIR"

    ( cd "$JAVA_LIB_DIR" && chmod +x ./gradlew )
    ( cd "$JAVA_LIB_DIR" && GRADLE_USER_HOME="$GRADLE_USER_HOME" ./gradlew build "${GRADLE_ARGS[@]}" )

    cp -f "$JAVA_LIB_DIR"/build/libs/* "$JAVA_LIB_OUT_DIR"/ 2>/dev/null || true
    echo "[OK] java lib outputs copied to: $JAVA_LIB_OUT_DIR/"
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

    rm -rf "$BUILD_DIR_ABS/tools" "$BUILD_DIR_ABS/libs" "$BUILD_DIR_ABS/$EXE" "$BUILD_DIR_ABS"/*.exe 2>/dev/null || true
    remove_ide_dirs
    echo "clean done"
}

clean_ide() {
    remove_ide_dirs
    echo "[OK] removed .vscode/.idea under root/compiler/tools/libs"
}

rebuild() {
    clean
    all
}

usage() {
    cat <<'EOF'
Usage: ./build.sh [all|compile|tools|java_lib|clean|clean_ide|rebuild]

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
    clean_ide) clean_ide ;;
    rebuild) rebuild ;;
    -h|--help|help) usage ;;
    *) echo "Unknown command: $cmd" >&2; usage; exit 2 ;;
esac
