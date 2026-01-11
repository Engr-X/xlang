set -e

ROOT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "🧹 Cleaning build directories..."

HASKELL_BUILD="$ROOT_DIR/haskell/build"
CABAL_BUILD="$ROOT_DIR/haskell/dist-newstyle"
C_HASKELL_BUILD="$ROOT_DIR/c/build"


if [ -d "$HASKELL_BUILD" ]; then
  echo " - Removing $HASKELL_BUILD"
  rm -rf "$HASKELL_BUILD"
else
  echo " - $HASKELL_BUILD does not exist"
fi

if [ -d "$CABAL_BUILD" ]; then
  echo " - Removing $CABAL_BUILD"
  rm -rf "$CABAL_BUILD"
else
  echo " - $CABAL_BUILD does not exist"
fi


if [ -d "$C_HASKELL_BUILD" ]; then
  echo " - Removing $C_HASKELL_BUILD"
  rm -rf "$C_HASKELL_BUILD"
else
  echo " - $C_HASKELL_BUILD does not exist"
fi

echo "Clean done."
