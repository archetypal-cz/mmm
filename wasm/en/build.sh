#!/bin/bash
# Build the English original (Bill Wolpert's Mystery Mansion) to WebAssembly.
#   docker run --rm -v "$PWD":/w -w /w/wasm/en emscripten/emsdk:latest ./build.sh [web|node]
set -euo pipefail
cd "$(dirname "$0")"
TARGET=${1:-web}
LIB=../.cache/libf2c-$TARGET
[ -f $LIB/libf2c.a ] || (cd .. && ./build.sh $TARGET)     # builds the shared libf2c
command -v f2c >/dev/null || { apt-get -qq update >/dev/null && apt-get -qq install -y f2c >/dev/null; }

rm -rf gen && python3 prep_en.py
(cd gen && f2c -I2 -w -!bs -Nn802 -Nx400 -Nq300 -Ns801 -Nc99 *.f >/dev/null 2>f2c.log) || { cat gen/f2c.log; exit 1; }

CFLAGS="-O2 -fcommon -Ds_copy=s_copy_i -I$LIB -Wno-everything"
mkdir -p ../dist
if [ "$TARGET" = node ]; then
  emcc $CFLAGS gen/*.c en_runtime.c ../../native/compat.c $LIB/libf2c.a -lm \
    -sEXIT_RUNTIME=1 -sNODERAWFS=1 -sSTACK_SIZE=4194304 -o ../dist/mmm-en-node.js
else
  EXTRA="-include $PWD/../src/stdin_redirect.h"
  emcc $CFLAGS $EXTRA gen/*.c en_runtime.c ../../native/compat.c ../src/web_io.c $LIB/libf2c.a -lm \
    -sASYNCIFY -sASYNCIFY_STACK_SIZE=1048576 -sSTACK_SIZE=4194304 \
    -sMODULARIZE -sEXPORT_ES6 -sEXPORT_NAME=createMMMEN -sENVIRONMENT=web,node \
    -sEXIT_RUNTIME=1 -sALLOW_MEMORY_GROWTH \
    -sEXPORTED_RUNTIME_METHODS=FS,HEAPU8,HEAP16 -sEXPORTED_FUNCTIONS=_main,_mmm_common_mmbc \
    -o ../dist/mmm-en.js
fi
echo "built wasm/dist (english, $TARGET)"
