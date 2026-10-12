#!/usr/bin/env bash
# Builds and installs s2n-tls, which fs2-io uses for TLS on Scala Native.
set -euo pipefail

PREFIX="${1:-/usr/local}"
S2N_VERSION="${S2N_VERSION:-v1.7.11}"

SUDO=""
if [ ! -w "$PREFIX" ] && command -v sudo >/dev/null; then SUDO="sudo"; fi

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

git clone -q --depth 1 --branch "$S2N_VERSION" https://github.com/aws/s2n-tls.git "$WORK/s2n-tls"
cmake -S "$WORK/s2n-tls" -B "$WORK/s2n-tls/build" \
  -DBUILD_TESTING=OFF -DBUILD_SHARED_LIBS=ON -DCMAKE_INSTALL_PREFIX="$PREFIX"
cmake --build "$WORK/s2n-tls/build" -j"$(nproc)"
$SUDO cmake --install "$WORK/s2n-tls/build"
if command -v ldconfig >/dev/null; then $SUDO ldconfig || true; fi
