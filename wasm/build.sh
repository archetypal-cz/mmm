#!/bin/bash
# Build MMM to WebAssembly: patched Fortran (native/prep.py) -> f2c -> emcc.
#   docker run --rm -v "$PWD":/w -w /w/wasm emscripten/emsdk:latest ./build.sh [web|node] [puvodni|cz]
#   web:  dist/mmm.js + mmm.wasm (ES module, async console input via Asyncify)
#   node: dist/mmm-node.js (plain stdin/stdout, for differential tests)
#   cz:   the new Czech translation (cz/messages.json) -> dist/mmm-cz.js, mmm-cz-node.js
set -euo pipefail
cd "$(dirname "$0")"
TARGET=${1:-web}
VARIANT=${2:-puvodni}
HERE=$PWD
SRC=../native/build; GEN=gen; SUF=""
if [ "$VARIANT" = cz ]; then SRC=../native/build-cz; GEN=gen-cz; SUF="-cz"; fi
command -v f2c >/dev/null || { apt-get -qq update >/dev/null && apt-get -qq install -y f2c >/dev/null; }

# 1. patched Fortran sources (+ data for the web UI)
python3 ../native/prep.py
if [ "$VARIANT" = cz ]; then
  python3 ../native/prep.py --out ../native/build-cz --translation cz/messages.json
fi
python3 gen_data.py
python3 gen_layout.py

# 2. libf2c runtime (netlib), built once per target with emcc
EXTRA=""
[ "$TARGET" = web ] && EXTRA="-include $HERE/src/stdin_redirect.h"
LIB=.cache/libf2c-$TARGET
if [ ! -f $LIB/libf2c.a ]; then
  mkdir -p $LIB
  [ -f .cache/libf2c.zip ] || curl -sSL -o .cache/libf2c.zip https://www.netlib.org/f2c/libf2c.zip
  (cd $LIB && unzip -q -o ../libf2c.zip \
     && cp signal1.h0 signal1.h && cp sysdep1.h0 sysdep1.h && cp f2c.h0 f2c.h \
     && emcc -O2 -DNO_FPINIT arithchk.c -lm -DNO_LONG_LONG -o arithchk.js && node arithchk.js > arith.h \
     && for f in *.c; do case $f in arithchk.c|main.c|getarg_.c|iargc_.c|pow_qq.c|qbitbits.c|qbitshft.c|ftell64_.c) continue;; esac; \
          emcc -O2 -c -DWANT_LEAD_0 -DIEEE_drem -DSkip_f2c_Undefs -DNO_TRUNCATE $EXTRA -I. $f -o ${f%.c}.o || exit 1; done \
     && emcc -O2 -c -I. main.c -o main.o \
     && emar rcs libf2c.a *.o)
fi

# 3. Fortran -> C
rm -rf $GEN && mkdir $GEN
for f in $SRC/*.FOR; do b=$(basename "$f" .FOR); [ "$b" = MMCO ] && continue; cp "$f" $GEN/$b.f; done
cp $SRC/MMCO.FOR ../native/compat77.f $GEN/
(cd $GEN && f2c -I2 -w -!bs -Nn802 -Nx400 -Nq300 -Ns801 -Nc99 *.f >/dev/null 2>f2c.log) || { cat $GEN/f2c.log; exit 1; }

# 4. compile + link
CFLAGS="-O2 -fcommon -Ds_copy=s_copy_i -I$LIB -Wno-everything"
mkdir -p dist
if [ "$TARGET" = node ]; then
  emcc $CFLAGS $GEN/*.c ../native/compat.c $LIB/libf2c.a -lm \
    -sEXIT_RUNTIME=1 -sNODERAWFS=1 -o dist/mmm$SUF-node.js
else
  emcc $CFLAGS $EXTRA $GEN/*.c ../native/compat.c src/web_io.c $LIB/libf2c.a -lm \
    -sASYNCIFY -sASYNCIFY_STACK_SIZE=65536 -sSTACK_SIZE=1048576 \
    -sMODULARIZE -sEXPORT_ES6 -sEXPORT_NAME=createMMM -sENVIRONMENT=web,node \
    -sEXIT_RUNTIME=1 -sALLOW_MEMORY_GROWTH \
    -sEXPORTED_RUNTIME_METHODS=FS,HEAPU8,HEAP16 -sEXPORTED_FUNCTIONS=_main,_mmm_common_mmbc \
    -o dist/mmm$SUF.js
fi
echo "built wasm/dist ($TARGET, $VARIANT)"
