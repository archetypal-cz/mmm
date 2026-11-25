# WebAssembly build

The original Fortran, patched by `native/prep.py` (the same sources as the
native build), translated by **f2c** to C and compiled by **Emscripten**.
Console input is asynchronous (Asyncify): the f2c runtime's stdin reads are
redirected to `Module.mmmReadLine()`, so the unchanged game loop simply
"waits" for the next command from the page.

```sh
# build (from repo root); web = dist/mmm.js + mmm.wasm, node = test build
docker run --rm -v "$PWD":/w -w /w/wasm emscripten/emsdk:latest ./build.sh web
docker run --rm -v "$PWD":/w -w /w/wasm emscripten/emsdk:latest ./build.sh node

# play: serve wasm/ and open /web/index.html
cd wasm && python -m http.server 8765    # http://localhost:8765/web/

# differential test vs. the native gfortran build (byte-identical output)
docker run --rm -v "$PWD":/w -w /w/wasm emscripten/emsdk:latest python3 compare.py 200 250
docker run --rm -v "$PWD":/w -w /w/wasm emscripten/emsdk:latest python3 compare.py 30 200 --web
```

`MMM_FIXED_TIME=HHMMSS` (env var, or `fixedTime` in `startGame`) pins the
mystery number, so runs are reproducible.

| File | Role |
|---|---|
| `build.sh` | prep → f2c → emcc; builds netlib libf2c once into `.cache/` |
| `src/web_io.c`, `src/stdin_redirect.h` | async stdin for libf2c; `mmm_common_mmbc()` exposes game state |
| `web/game.js` | `startGame({onOutput, readLine, onExit})` wrapper |
| `web/cp852.js` | CP852 ⇄ Unicode (the engine works in CP852 bytes) |
| `web/index.html` | minimal terminal page |
| `compare.py`, `web/test-node.mjs`, `web/smoke-browser.mjs` | tests |

Build-specific fixes (also documented in `../OPRAVY.md`): lines wrapped to
72 columns, `'AB'` assignments to INTEGER*2 as numbers, `RND` in its own
COMMON (alignment), `REIO` with a text literal → `REIOS`, `s_copy` return
type shim, libf2c built with `WANT_LEAD_0` (prints `0.7` like MS Fortran).
