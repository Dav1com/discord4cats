#!/usr/bin/env bash
# Builds and installs Discord's libdave (DAVE E2EE for voice) as a shared library,
# together with its MLS dependency (cisco/mlspp), without vcpkg.
#
# Usage: scripts/install-libdave.sh [prefix]   (default prefix: /usr/local)
# Requires: cmake, a C++17 compiler, OpenSSL 3 headers, nlohmann-json headers.
set -euo pipefail

PREFIX="${1:-/usr/local}"
LIBDAVE_REF="${LIBDAVE_REF:-8de72b1f8a2ac3c5a5270755bb8091a62e3c6169}"
# The mlspp revision pinned by libdave's vcpkg overlay port (cpp/vcpkg-alts/openssl_3).
MLSPP_REF="${MLSPP_REF:-1cc50a124a3bc4e143a787ec934280dc70c1034d}"

SUDO=""
if [ ! -w "$PREFIX" ] && command -v sudo >/dev/null; then SUDO="sudo"; fi

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

fetch() { # repo ref dir
  git init -q "$3"
  git -C "$3" remote add origin "$1"
  git -C "$3" fetch -q --depth 1 origin "$2"
  git -C "$3" checkout -q FETCH_HEAD
}

fetch https://github.com/cisco/mlspp.git "$MLSPP_REF" "$WORK/mlspp"
cmake -S "$WORK/mlspp" -B "$WORK/mlspp/build" \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_POSITION_INDEPENDENT_CODE=ON \
  -DBUILD_SHARED_LIBS=OFF \
  -DDISABLE_GREASE=ON \
  -DMLS_CXX_NAMESPACE=mlspp \
  -DTESTING=OFF \
  -DCMAKE_INSTALL_PREFIX="$WORK/deps"
cmake --build "$WORK/mlspp/build" -j"$(nproc)"
cmake --install "$WORK/mlspp/build"

fetch https://github.com/discord/libdave.git "$LIBDAVE_REF" "$WORK/libdave"
cmake -S "$WORK/libdave/cpp" -B "$WORK/libdave/build" \
  -DCMAKE_BUILD_TYPE=Release \
  -DBUILD_SHARED_LIBS=ON \
  -DTESTING=OFF \
  -DCMAKE_PREFIX_PATH="$WORK/deps" \
  -DCMAKE_INSTALL_PREFIX="$PREFIX"
cmake --build "$WORK/libdave/build" --target libdave -j"$(nproc)"
$SUDO cmake --install "$WORK/libdave/build"
if command -v ldconfig >/dev/null; then $SUDO ldconfig || true; fi

echo "libdave installed to $PREFIX"
